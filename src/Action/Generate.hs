{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.IO.Extra
import Data.Tuple.Extra
import Control.Exception.Extra
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Extra
import Data.Monoid
import Data.Ord
import System.Console.CmdArgs.Verbosity
import Prelude

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Haddock
import Input.Download
import Input.Reorder
import Input.Set
import Input.Settings
import Input.Item
import General.Util
import General.Store
import General.Timing
import General.Str
import Action.CmdLine
import General.Conduit
import Control.DeepSeq

{-


data GenList
    = GenList_Package String -- a literally named package
    | GenList_GhcPkg String -- command to run, or "" for @ghc-pkg list@
    | GenList_Stackage String -- URL of stackage file, defaults to @http://www.stackage.org/lts/cabal.config@
    | GenList_Dependencies String -- dependencies in a named .cabal file
    | GenList_Sort String -- URL of file to sort by, defaults to @http://packdeps.haskellers.com/reverse@

data GenTags
    = GenTags_GhcPkg String -- command to run, or "" for @ghc-pkg dump@
    | GenTags_Diff FilePath -- a diff to apply to previous metadata
    | GenTags_Tarball String -- tarball of Cabal files, defaults to http://hackage.haskell.org/packages/index.tar.gz
    | GetTags_Cabal FilePath -- tarball to get tag information from

data GenData
    = GenData_File FilePath -- a file containing package data
    | GenData_Tarball String -- URL where a tarball of data files resides


* `hoogle generate` - generate for all things in Stackage based on Hackage information.
* `hoogle generate --source=file1.txt --source=local --source=stackage --source=hackage --source=tarball.tar.gz`

Which files you want to index. Currently the list on stackage, could be those locally installed, those in a .cabal file etc. A `--list` flag, defaults to `stackage=url`. Can also be `ghc-pkg`, `ghc-pkg=user` `ghc-pkg=global`. `name=p1`.

Extra metadata you want to apply. Could be a file. `+shake author:Neil-Mitchell`, `-shake author:Neil-Mitchel`. Can be sucked out of .cabal files. A `--tags` flag, defaults to `tarball=url` and `diff=renamings.txt`.

Where the haddock files are. Defaults to `tarball=hackage-url`. Can also be `file=p1.txt`. Use `--data` flag.

Defaults to: `hoogle generate --list=ghc-pkg --list=constrain=stackage-url`.

Three pieces of data:

* Which packages to index, in order.
* Metadata.


generate :: Maybe Int -> [GenList] -> [GenTags] -> [GenData] -> IO ()
-- how often to redownload, where to put the files



generate :: FilePath -> [(String, [(String, String)])] -> [(String, LBS.ByteString)] -> IO ()
generate output metadata  = undefined
-}


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

type Download = String -> URL -> IO FilePath

readHaskellOnline :: Timing -> Settings -> Download -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellOnline timing settings download = do
    stackage <- download "haskell-stackage.txt" "https://www.stackage.org/lts/cabal.config"
    platform <- download "haskell-platform.txt" "https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2015.hs"
    cabals   <- download "haskell-cabal.tar.gz" "https://hackage.haskell.org/packages/index.tar.gz"
    hoogles  <- download "haskell-hoogle.tar.gz" "https://hackage.haskell.org/packages/hoogle.tar.gz"

    -- peakMegabytesAllocated = 2
    setStackage <- Set.map strPack <$> setStackage stackage
    setPlatform <- Set.map strPack <$> setPlatform platform
    setGHC <- Set.map strPack <$> setGHC platform

    cbl <- timed timing "Reading Cabal" $ parseCabalTarball settings cabals
    let want = Set.insert (strPack "ghc") $ Set.unions [setStackage, setPlatform, setGHC]
    cbl <- return $ flip Map.mapWithKey cbl $ \name p ->
        p{packageTags =
            [(strPack "set",strPack "included-with-ghc") | name `Set.member` setGHC] ++
            [(strPack "set",strPack "haskell-platform") | name `Set.member` setPlatform] ++
            [(strPack "set",strPack "stackage") | name `Set.member` setStackage] ++
            packageTags p}

    let source = do
            tar <- liftIO $ tarballReadFiles hoogles
            forM_ tar $ \(strPack . takeBaseName -> name, src) ->
                yield (name, hackagePackageURL name, src)
    return (cbl, want, source)


readHaskellDirs :: Timing -> Settings -> [FilePath] -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellDirs timing settings dirs = do
    files <- concatMapM listFilesRecursive dirs
    -- We reverse/sort the list because of #206
    -- Two identical package names with different versions might be foo-2.0 and foo-1.0
    -- We never distinguish on versions, so they are considered equal when reordering
    -- So put 2.0 first in the list and rely on stable sorting. A bit of a hack.
    let order a = second Down $ parseTrailingVersion a
    let packages = map (strPack . takeBaseName &&& id) $ sortOn (map order . splitDirectories) $ filter ((==) ".txt" . takeExtension) files
    cabals <- mapM parseCabal $ filter ((==) ".cabal" . takeExtension) files
    let source = forM_ packages $ \(name, file) -> do
            src <- liftIO $ bstrReadFile file
            dir <- liftIO $ canonicalizePath $ takeDirectory file
            let url = "file://" ++ ['/' | not $ "/" `isPrefixOf` dir] ++ replace "\\" "/" dir ++ "/"
            yield (name, url, lbstrFromChunks [src])
    return (Map.union
                (Map.fromList cabals)
                (Map.fromList $ map ((,mempty{packageTags=[(strPack "set",strPack "all")]}) . fst) packages)
           ,Set.fromList $ map fst packages, source)
  where
    parseCabal fp = do
        src <- readFileUTF8' fp
        let pkg = readCabal settings src
        return (strPack $ takeBaseName fp, pkg)

readFregeOnline :: Timing -> Download -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readFregeOnline timing download = do
    frege <- download "frege-frege.txt" "http://try.frege-lang.org/hoogle-frege.txt"
    let source = do
            src <- liftIO $ bstrReadFile frege
            yield (strPack "frege", "http://google.com/", lbstrFromChunks [src])
    return (Map.empty, Set.singleton $ strPack "frege", source)


readHaskellGhcpkg :: Timing -> Settings -> [FilePath] -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellGhcpkg timing settings pkgdbs = do
    cbl <- timed timing "Reading ghc-pkg" $ readGhcPkg settings pkgdbs
    let source =
            forM_ (Map.toList cbl) $ \(name,Package{..}) -> whenJust packageDocs $ \docs -> do
                let file = docs </> strUnpack name <.> "txt"
                whenM (liftIO $ doesFileExist file) $ do
                    src <- liftIO $ bstrReadFile file
                    docs <- liftIO $ canonicalizePath docs
                    let url = "file://" ++ ['/' | not $ all isPathSeparator $ take 1 docs] ++
                              replace "\\" "/" (addTrailingPathSeparator docs)
                    yield (name, url, lbstrFromChunks [src])
    cbl <- return $ let ts = map (both strPack) [("set","stackage"),("set","installed")]
                    in Map.map (\p -> p{packageTags = ts ++ packageTags p}) cbl
    return (cbl, Map.keysSet cbl, source)

readHaskellHaddock :: Timing -> Settings -> FilePath -> [FilePath] -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellHaddock timing settings docBaseDir pkgdbs = do
    cbl <- timed timing "Reading ghc-pkg" $ readGhcPkg settings pkgdbs
    let source =
            forM_ (Map.toList cbl) $ \(name, p@Package{..}) -> do
                let docs = docDir (strUnpack name) p
                    file = docBaseDir </> docs </> (strUnpack name) <.> "txt"
                whenM (liftIO $ doesFileExist file) $ do
                    src <- liftIO $ bstrReadFile file
                    let url = ['/' | not $ all isPathSeparator $ take 1 docs] ++
                              replace "\\" "/" (addTrailingPathSeparator docs)
                    yield (name, url, lbstrFromChunks [src])
    cbl <- return $ let ts = map (both strPack) [("set","stackage"),("set","installed")]
                    in Map.map (\p -> p{packageTags = ts ++ packageTags p}) cbl
    return (cbl, Map.keysSet cbl, source)

    where docDir name Package{..} = name ++ "-" ++ strUnpack packageVersion

actionGenerate :: CmdLine -> IO ()
actionGenerate g@Generate{..} = withTiming (if debug then Just $ replaceExtension database "timing" else Nothing) $ \timing -> do
    putStrLn "Starting generate"
    createDirectoryIfMissing True $ takeDirectory database
    whenLoud $ putStrLn $ "Generating files to " ++ takeDirectory database

    download <- return $ downloadInput timing insecure download (takeDirectory database)
    settings <- loadSettings
    (cbl, want, source) <- case language of
        Haskell | Just dir <- haddock -> readHaskellHaddock timing settings dir pkgdbs
                | [""]     <- local_ -> readHaskellGhcpkg timing settings pkgdbs
                | []       <- local_ -> readHaskellOnline timing settings download
                | otherwise -> readHaskellDirs timing settings local_
        Frege | [] <- local_ -> readFregeOnline timing download
              | otherwise -> errorIO "No support for local Frege databases"
    (cblErrs, popularity) <- evaluate $ packagePopularity cbl
    cbl <- evaluate $ Map.map (\p -> p{packageDepends=[]}) cbl -- clear the memory, since the information is no longer used
    evaluate popularity

    -- mtl is more popular than transformers, despite having dodgy docs, which is a shame, so we hack it
    popularity <- evaluate $ Map.adjust (max $ 1 + Map.findWithDefault 0 (strPack "mtl") popularity) (strPack "transformers") popularity

    want <- return $ if include /= [] then Set.fromList $ map strPack include else want
    want <- return $ if count == 0 then want else Set.fromList $ take count $ Set.toList want

    (stats, _) <- storeWriteFile database $ \store -> do
        xs <- withBinaryFile (database `replaceExtension` "warn") WriteMode $ \warnings -> do
            hSetEncoding warnings utf8
            hPutStr warnings $ unlines cblErrs
            nCblErrs <- evaluate $ length cblErrs

            itemWarn <- newIORef 0
            let warning msg = do modifyIORef itemWarn succ; hPutStrLn warnings msg

            let consume :: ConduitM (Int, (PkgName, URL, LBStr)) (Maybe Target, [Item]) IO ()
                consume = awaitForever $ \(i, (strUnpack -> pkg, url, body)) -> do
                    timedOverwrite timing ("[" ++ show i ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                        parseHoogle (\msg -> warning $ pkg ++ ":" ++ msg) url body

            writeItems store $ \items -> do
                xs <- runConduit $
                    source .|
                    filterC (flip Set.member want . fst3) .|
                    void ((|$|)
                        (zipFromC 1 .| consume)
                        (do seen <- fmap Set.fromList $ mapMC (evaluate . force . strCopy . fst3) .| sinkList
                            let missing = [x | x <- Set.toList $ want `Set.difference` seen
                                             , fmap packageLibrary (Map.lookup x cbl) /= Just False]
                            liftIO $ putStrLn ""
                            liftIO $ whenNormal $ when (missing /= []) $ do
                                putStrLn $ "Packages missing documentation: " ++ unwords (sortOn lower $ map strUnpack missing)
                            liftIO $ when (Set.null seen) $
                                exitFail "No packages were found, aborting (use no arguments to index all of Stackage)"
                            -- synthesise things for Cabal packages that are not documented
                            forM_ (Map.toList cbl) $ \(name, Package{..}) -> when (name `Set.notMember` seen) $ do
                                let ret prefix = yield $ fakePackage name $ prefix ++ trim (strUnpack packageSynopsis)
                                if name `Set.member` want then
                                    (if packageLibrary
                                        then ret "Documentation not found, so not searched.\n"
                                        else ret "Executable only. ")
                                else if null include then
                                    ret "Not on Stackage, so not searched.\n"
                                else
                                    return ()
                            ))
                    .| pipelineC 10 (items .| sinkList)

                itemWarn <- readIORef itemWarn
                when (itemWarn > 0) $
                    putStrLn $ "Found " ++ show itemWarn ++ " warnings when processing items"
                return [(a,b) | (a,bs) <- xs, b <- bs]

        itemsMemory <- getStatsCurrentLiveBytes
        xs <- timed timing "Reordering items" $ return $! reorderItems settings (\s -> maybe 1 negate $ Map.lookup s popularity) xs
        timed timing "Writing tags" $ writeTags store (`Set.member` want) (\x -> maybe [] (map (both strUnpack) . packageTags) $ Map.lookup x cbl) xs
        timed timing "Writing names" $ writeNames store xs
        timed timing "Writing types" $ writeTypes store (if debug then Just $ dropExtension database else Nothing) xs

        x <- getVerbosity
        when (x >= Loud) $
            maybe (return ()) print =<< getStatsDebug
        when (x >= Normal) $ do
            whenJustM getStatsPeakAllocBytes $ \x ->
                putStrLn $ "Peak of " ++ x ++ ", " ++ fromMaybe "unknown" itemsMemory ++ " for items"

    when debug $
        writeFile (database `replaceExtension` "store") $ unlines stats
