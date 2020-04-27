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
import Data.Maybe                        (listToMaybe, isJust)
import Data.List                         (sortBy)
import Data.List.NonEmpty                (NonEmpty((:|)))
import General.Conduit                   (runConduit, sourceList ,mapC, mapMC, groupOnLastC ,pipelineC ,foldMC, liftIO, (.|), filterC, filterMC)
import General.Str                       (Str, strNull, lbstrUnpack, strPack, strUnpack)
import General.Util                      (PkgName, tarballReadFiles)
import Input.Settings                    (Settings(..))
import System.Directory                  (getDirectoryContents)
import System.Directory.Internal.Prelude (IOErrorType(..))
import System.Exit                       (die)
import System.FilePath                   (takeExtension, takeBaseName, (</>))
import System.IO.Error                   (ioeGetErrorType)


import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as S
import Data.List (isSuffixOf, group, groupBy)
import System.FilePath (takeFileName, (<.>), (</>))
import System.Directory (doesFileExist)
import Control.Monad (forM_)
import Data.Traversable (forM)
import Control.Monad (when)
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

-- | TODO: Use package.cache
---------------------------------------------------------------------
-- READERS

-- | Run 'ghc-pkg' and get a list of packages which are installed.
-- | DONE: Get the latest versions.
-- | NOTE: There may exists multiple latest version conf with different hash.
readGhcPkg :: Settings -> [FilePath] -> IO (Map.Map PkgName Package)
readGhcPkg settings dirs = do
    confs <- foldM getConfs [] dirs
    pkgInfos <- mapM (parsePackageConfFile settings) confs
    -- pure $ Map.fromList $ sortBy compareTuple $ filter (not . isBlackListed) pkgInfos
    pure $ Map.fromList $ sortBy compareTuple pkgInfos
    where
      compareTuple (name1, pkg1) (name2, pkg2)
        | name1 == name2 = packageVersion pkg1 `compare` packageVersion pkg2
        | otherwise      = name1 `compare` name2
      isBlackListed (name, _) = name `elem` blackListedPackageNames

-- | NOTE: black list reason: No haddock documentations in the source code.
blackListedPackageNames :: [Str]
blackListedPackageNames = map strPack
  [ "rts"
  , "bytestring-builder" ]


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


-- | success case:  IO ByteString    -> (BS.unpack   :: ByteString   -> [Word8]) -> decodeStringUTF8 -> (ignoreBOM :: Skipping BOM head)
-- | now         :  IO LB.ByteString -> (LBS.unpack :: LB.ByteString -> [Word8]) -> decodeStringUTF8 -> (ignoreBOM :: Skipping BOM head)
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
          .| filterC isMalformedCabal
          .| mapC snd
          .| pipelineC 10 (mapC (readCabal settings . Cabal.ignoreBOM . Cabal.fromUTF8LBS)
          -- .| mapMC (evaluate . force)
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
    Left err      -> Left $ (unlines $ take 2 $ lines src) ++ "\n" ++ show err
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

          -- | TODO: this field is used only for determining package documentation existence.
          --         the field name is deceiving so, it needs improvement.
          packageLibrary  = (not $ isJust packageDocs) || Cabal.libraryDirs pkgInfo == []
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


-- | for testing

fakeSettings :: Settings
fakeSettings = Settings id (\_ _ -> 0)

test f = do
  case readCabal fakeSettings f of
    Left err -> print err
    Right (pkgName, pkgInfo) -> print pkgInfo

test2 f = do
  (pkgName, pkgInfo) <- parsePackageConfFile fakeSettings f
  print pkgInfo


test3 = do
  tars <- tarballReadFiles "/Volumes/Ramdisk/tmp/index.tar.gz"
  forM_ (groupByCabalFile tars) $ \tar -> do
    case readCabal fakeSettings (Cabal.ignoreBOM . Cabal.fromUTF8LBS . snd $ tar) of
      Left err -> die $ fst tar
      Right r' -> putChar '.'

test4 = do
  ms <- parseCabalTarball fakeSettings "/Volumes/Ramdisk/tmp/index.tar.gz"
  mapM_ print (Map.toList ms)

test5 = do
  let dirs = [ "/Users/taemu/compilers/ghc/ghc-8.10.1/lib/ghc-8.10.1/package.conf.d"
             , "/Users/taemu/.cabal/store/ghc-8.10.1/package.db/" ]
  ms <- readGhcPkg fakeSettings dirs
  let files = foo <$> Map.toList ms
  forM_ files $ \file -> do
    b <- doesFileExist file
    when (not b) $ putStrLn $ file ++ " doesn't exists."
  pure ()
  where
       foo (name, pkg) = case packageDocs pkg of
                        Nothing -> ""
                        Just doc -> doc </> strUnpack name <.> "txt"


{-
    Obviously these packages isn't maintained. So just ignore them.
    DSTM.cabal                        : `{}`brace character.                     line 12, 35
    control-monad-exception-mtl.cabal : ` default- extensions` space character   line 26
    ds-kanren.cabal                   : test-suite section spurious semicolon.   line 28, 40
    metric.cabal                      : test-suite section spurious semicolon.   line 28
    phasechange.cabal                 : impl(ghc >=.. ) spurious semicolon.      line 49, 54
    smartword.cabal                   : `build depends` missing hipen character. line 3438
-}
groupByCabalFile :: [(FilePath, LB.ByteString)] -> [(FilePath, LB.ByteString)]
groupByCabalFile xs = foldr go [] xs
  where
      go t acc@(a:as) = if isMalformedCabal t || extractFilename t == extractFilename a then acc else t:acc
      go t []         = [t]


extractFilename = takeFileName . fst
isMalformedCabal e   = extractFilename e `elem` malformedCabalFiles

malformedCabalFiles =
  [ "DSTM.cabal"
  , "control-monad-exception-mtl.cabal"
  , "ds-kanren.cabal"
  , "metric.cabal"
  , "phasechange.cabal"
  , "smartword.cabal" ]
