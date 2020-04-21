{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards, ScopedTypeVariables #-}

-- | Module for reading Cabal files.
module Input.Cabal(
    PkgName, Package(..),
    parseCabalTarball, readGhcPkg,
    packagePopularity, readCabal
    ) where

import Input.Settings

import Data.List.Extra
import System.FilePath
import Control.DeepSeq
import Control.Exception
import Control.Exception.Extra
import Control.Monad
import System.IO.Extra
import General.Str
import System.Exit
import qualified System.Process.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import System.Directory
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import General.Util
import General.Conduit
import Data.Semigroup
import Control.Applicative
import Prelude

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
        Package (x1++y1) (x2||y2) (one x3 y3) (one x4 y4) (nubOrd $ x5 ++ y5) (x6 `mplus` y6)
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
    topdir <- findExecutable "ghc-pkg"
    let ghcpkgArgs = ["dump"] ++ map (\dir -> "--package-db=" ++ dir) dirs
    -- important to use BS process reading so it's in Binary format, see #194
    (exit, stdout, stderr) <- BS.readProcessWithExitCode "ghc-pkg" ghcpkgArgs mempty
    when (exit /= ExitSuccess) $
        errorIO $ "Error when reading from ghc-pkg, " ++ show exit ++ "\n" ++ UTF8.toString stderr
    let g (stripPrefix "$topdir" -> Just x) | Just t <- topdir = takeDirectory t ++ x
        g x = x
    let fixer p = p{packageLibrary = True, packageDocs = g <$> packageDocs p}
    let f ((stripPrefix "name: " -> Just x):xs) = Just (strPack $ trim x, fixer $ readCabal settings $ unlines xs)
        f xs = Nothing
    return $ Map.fromList $ mapMaybe f $ splitOn ["---"] $ lines $ filter (/= '\r') $ UTF8.toString stdout


-- | Given a tarball of Cabal files, parse the latest version of each package.
parseCabalTarball :: Settings -> FilePath -> IO (Map.Map PkgName Package)
-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
-- rely on the fact the highest version is last (using lastValues)
parseCabalTarball settings tarfile = do
    res <- runConduit $
        (sourceList =<< liftIO (tarballReadFiles tarfile)) .|
        mapC (first takeBaseName) .| groupOnLastC fst .| mapMC (evaluate . force) .|
        pipelineC 10 (mapC (strPack *** readCabal settings . lbstrUnpack) .| mapMC (evaluate . force) .| sinkList)
    return $ Map.fromList res


---------------------------------------------------------------------
-- PARSERS

-- | Cabal information, plus who I depend on
readCabal :: Settings -> String -> Package
readCabal Settings{..} src = Package{..}
    where
        mp = Map.fromListWith (++) $ lexCabal src
        ask x = Map.findWithDefault [] x mp

        packageDepends =
            map strPack $ nubOrd $ filter (/= "") $
            map (intercalate "-" . takeWhile (all isAlpha . take 1) . splitOn "-" . fst . word1) $
            concatMap (split (== ',')) (ask "build-depends") ++ concatMap words (ask "depends")
        packageVersion = strPack $ head $ dropWhile null (ask "version") ++ ["0.0"]
        packageSynopsis = strPack $ unwords $ words $ unwords $ ask "synopsis"
        packageLibrary = "library" `elem` map (lower . trim) (lines src)
        packageDocs = listToMaybe $ ask "haddock-html"

        packageTags = map (both strPack) $ nubOrd $ concat
            [ map (head xs,) $ concatMap cleanup $ concatMap ask xs
            | xs <- [["license"],["category"],["author","maintainer"]]]

        -- split on things like "," "&" "and", then throw away email addresses, replace spaces with "-" and rename
        cleanup =
            filter (/= "") .
            map (renameTag . intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")) .
            concatMap (map unwords . split (== "and") . words) . split (`elem` ",&")


-- Ignores nesting beacuse it's not interesting for any of the fields I care about
lexCabal :: String -> [(String, [String])]
lexCabal = f . lines
    where
        isEmpty str
          | str == [] = True
          | otherwise = False
        f (x:xs) | (white,x) <- span isSpace x
                 , (name@(_:_),x) <- span (\c -> isAlpha c || c == '-') x
                 , ':':x <- trim x
                 , (xs1,xs2) <- span (\s -> length (takeWhile isSpace s) > length white) xs
                 , trx <- trim x
                 , xs3 <- replace ["."] [""] (map (trim . fst . breakOn "--") xs1)
                 = (lower name, if isEmpty trx then xs3 else trx:xs3) : f xs2
        f (x:xs) = f xs
        f [] = []
