{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards, ScopedTypeVariables #-}

-- | Module for reading Cabal files.
module Input.Cabal(
    PkgName, Package(..),
    parseCabalTarball, readGhcPkg,
    packagePopularity, readCabal
    ) where

import qualified Data.Map.Strict                   as Map
import qualified Data.Set                          as Set
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Simple.Utils         as Cabal
import qualified Distribution.Types.PackageId      as Cabal
import qualified Distribution.Text                 as Cabal
import qualified Distribution.Compat.Exception     as Cabal
import qualified Distribution.Types.PackageName    as Cabal
import qualified Distribution.Utils.ShortText      as Cabal
import Control.Arrow                     (first)
import Control.DeepSeq                   (NFData, rnf, force)
import Control.Exception                 (evaluate)
import Control.Monad                     (mplus, foldM)
import Data.Maybe                        (listToMaybe)
import Data.List.NonEmpty                (NonEmpty((:|)))
import General.Conduit                   (runConduit, sourceList ,mapC, mapMC, groupOnLastC ,pipelineC ,foldMC, liftIO, (.|))
import General.Str                       (Str, strNull, lbstrUnpack, strPack, strUnpack)
import General.Util                      (PkgName, tarballReadFiles)
import Input.Settings                    (Settings(..))
import System.Directory                  (getDirectoryContents)
import System.Directory.Internal.Prelude (IOErrorType(..))
import System.Exit                       (die)
import System.FilePath                   (takeExtension, takeBaseName, (</>))
import System.IO.Error                   (ioeGetErrorType)
---------------------------------------------------------------------
-- DATA TYPE

-- | A representation of a Cabal package.
data Package = Package
    {packageTags :: ![(Str, Str)] -- ^ The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    ,packageLibrary :: !Bool -- ^ True if the package provides a library (False if it is only an executable with no API)
    ,packageSynopsis :: !Str -- ^ The synposis, grabbed from the top section.
    ,packageVersion :: !Str -- ^ The version, grabbed from the top section.
    ,packageDepends :: ![PkgName] -- ^ The list of packages that this package directly depends on.
    ,packageDocs :: !(Maybe FilePath) -- ^ Directory where the documentation is located
    } deriving Show

instance Semigroup Package where
    Package x1 x2 x3 x4 x5 x6 <> Package y1 y2 y3 y4 y5 y6 =
        Package (x1++y1) (x2||y2) (one x3 y3) (one x4 y4) (ordNub $ x5 ++ y5) (x6 `mplus` y6)
        where one a b = if strNull a then b else a

instance Monoid Package where
    mempty = Package [] True mempty mempty [] Nothing
    mappend = (<>)

instance NFData Package where
    rnf (Package a b c d e f) = rnf (a,b,c,d,e,f)


---------------------------------------------------------------------
-- POPULARITY

-- | Given a set of packages, return the popularity of each package, along with any warnings
--   about packages imported but not found.
packagePopularity :: Map.Map PkgName Package -> ([String], Map.Map PkgName Int)
packagePopularity cbl = mp `seq` (errs, mp)
    where
        mp = Map.map length good
        errs =  [ strUnpack user ++ ".cabal: Import of non-existant package " ++ strUnpack name ++
                          (if null rest then "" else ", also imported by " ++ show (length rest) ++ " others")
                | (name, user:rest) <- Map.toList bad]
        (good, bad)  = Map.partitionWithKey (\k _ -> k `Map.member` cbl) $
            Map.fromListWith (++) [(b,[a]) | (a,bs) <- Map.toList cbl, b <- packageDepends bs]


---------------------------------------------------------------------
-- READERS

-- | Run 'ghc-pkg' and get a list of packages which are installed.
readGhcPkg :: Settings -> [FilePath] -> IO (Map.Map PkgName Package)
readGhcPkg settings dirs = do
    confs <- foldM getConfs [] dirs
    pkgInfos <- mapM (parsePackageConfFile settings) confs
    pure $ Map.fromList pkgInfos


getConfs :: [FilePath] -> FilePath -> IO [FilePath]
getConfs confs pkgDbPath
  = do e <- Cabal.tryIO $ getDirectoryContents pkgDbPath
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType -> do
                  die $ "hoogle getConfs: ghc no longer supports single-file style package "
                     ++ "databases (" ++ pkgDbPath ++ ")"
           | otherwise -> do
                  die $ "hoogle getConfs: " ++ show err
         Right files -> pure $ foldr makeConfFullPath confs files
  where
    makeConfFullPath :: FilePath -> [FilePath] -> [FilePath]
    makeConfFullPath file acc =
      if ".conf" == takeExtension file then (pkgDbPath </> file):acc else acc


parsePackageConfFile :: Settings -> FilePath -> IO (PkgName, Package)
parsePackageConfFile settings file = do
  utf8File <- Cabal.readUTF8File file
  case readCabal settings utf8File of
    Left err -> die $ "hoogle parsePackageConfFile: " ++ show err
    Right ok -> pure ok

-- | Given a tarball of Cabal files, parse the latest version of each package.
parseCabalTarball :: Settings -> FilePath -> IO (Map.Map PkgName Package)
-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
-- rely on the fact the highest version is last (using lastValues)
parseCabalTarball settings tarfile = do
    res <- runConduit $
        (sourceList =<< liftIO (tarballReadFiles tarfile))
          .| mapC (first takeBaseName)
          .| groupOnLastC fst
          .| mapC snd
          .| pipelineC 10 (mapC (readCabal settings . lbstrUnpack)
          .| mapMC (evaluate . force)
          .| foldMC accM [])
          -- .| sinkList)
    return $ Map.fromList res
    where
      accM acc e = case e of
        Left err -> putStrLn err >> pure acc
        Right pkg -> pure (pkg:acc)
---------------------------------------------------------------------
-- PARSERS

-- | Cabal information, plus who I depend on
readCabal :: Settings -> String -> Either String (PkgName, Package)
readCabal Settings{..} src =
  case parsePackageInfo src of
    Left err      -> Left err
    Right pkgInfo -> Right (name, Package{..})
      where
          pkgId       = Cabal.sourcePackageId pkgInfo
          name        = strPack . Cabal.unPackageName . Cabal.pkgName $ pkgId
          authors     = lines . Cabal.fromShortText . Cabal.author $ pkgInfo
          maintainers = lines . Cabal.fromShortText . Cabal.maintainer $ pkgInfo
          licenses    = lines $ case Cabal.license pkgInfo of
                                  Left  l1 -> show l1 -- SPDX expression
                                  Right l2 -> show l2 -- OLD Licenses
          category  = Cabal.fromShortText . Cabal.category $ pkgInfo

          packageTags = [ (strPack "category", strPack category) ]
                     ++ [ (strPack "license", strPack license) | license <- licenses ]
                     ++ [ (strPack "author", strPack author) | author <- ordNub (authors <> maintainers) ]

          packageLibrary  = Cabal.libraryDirs pkgInfo /= [] || Cabal.libraryDynDirs pkgInfo /= []
          packageSynopsis = strPack . Cabal.fromShortText . Cabal.synopsis $ pkgInfo
          packageVersion  = strPack . Cabal.display . Cabal.pkgVersion $ pkgId
          packageDepends  = map (strPack . Cabal.display) (Cabal.depends pkgInfo)
          packageDocs     = listToMaybe $ Cabal.haddockHTMLs pkgInfo


parsePackageInfo :: String -> Either String Cabal.InstalledPackageInfo
parsePackageInfo src =
  case Cabal.parseInstalledPackageInfo (Cabal.toUTF8BS src) of
    Left (err :| _) -> Left err
    Right (_, ok)   -> Right ok


ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
