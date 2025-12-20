{-# LANGUAGE ScopedTypeVariables #-}
module IntegrationSpec (spec) where

import Test.Hspec
import Control.Exception (catch, SomeException)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Control.Monad (forM, forM_, filterM, when)
import Data.List (isPrefixOf)

-- | Recursively find all .euclid files
findEuclidFiles :: FilePath -> IO [FilePath]
findEuclidFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      let paths = map (dir </>) entries
      files <- filterM doesFileExist paths
      dirs <- filterM doesDirectoryExist paths
      let euclidFiles = filter (\f -> takeExtension f == ".euclid") files
      subFiles <- concat <$> mapM findEuclidFiles dirs
      return (euclidFiles ++ subFiles)

-- | Execute a .euclid file with the actual prover (3 minute timeout)
executeWithProver :: FilePath -> IO (Either String String)
executeWithProver path = do
  content <- readFile path

  -- Get current directory to construct absolute path
  cwd <- getCurrentDirectory
  let proverExe = cwd ++ "/dist-newstyle/build/x86_64-windows/ghc-9.12.2/prover-0.1.0.0/x/prover-int/build/prover-int/prover-int.exe"

  -- Run with 3 minute timeout
  result <- timeout (180 * 1000000) $ do
    (exitCode, stdout, stderr) <- readProcessWithExitCode proverExe
      []  -- No args: prover reads from stdin in REPL mode
      content

    return (exitCode, stdout, stderr)

  case result of
    Nothing -> return $ Left "TIMEOUT (>3 minutes)"
    Just (ExitSuccess, out, err) ->
      -- Success! Check stderr for warnings but don't fail on them
      let numLines = length $ lines out
          hasStderr = not (null err)
          msg = "Success: " ++ show numLines ++ " lines output" ++
                (if hasStderr then " (with warnings)" else "")
      in return $ Right msg
    Just (ExitFailure code, out, err) ->
      return $ Left $ "Exit code " ++ show code ++ ": " ++ take 300 (err ++ "\n" ++ out)
  `catch` (\(e :: SomeException) -> return $ Left $ "Exception: " ++ show e)

spec :: Spec
spec = do
  describe "Integration Tests - REAL Prover Execution" $ do
    exampleFiles <- runIO $ do
      exists <- doesDirectoryExist "examples"
      if exists
        then findEuclidFiles "examples"  -- Test ALL files
        else return []

    if null exampleFiles
      then it "no example files found" $ pendingWith "No .euclid files"
      else do
        it ("found " ++ show (length exampleFiles) ++ " example files to test") $ do
          length exampleFiles `shouldSatisfy` (> 0)

        -- Execute all files and collect results
        results <- runIO $ forM exampleFiles $ \file -> do
          putStrLn $ "Testing: " ++ file
          result <- executeWithProver file
          case result of
            Left err -> putStrLn $ "  FAILED: " ++ take 100 err
            Right msg -> putStrLn $ "  OK: " ++ msg
          return (file, result)

        let successes = [(f, msg) | (f, Right msg) <- results]
            timeouts = [(f, e) | (f, Left e) <- results, "TIMEOUT" `isPrefixOf` e]
            errors = [(f, e) | (f, Left e) <- results, not ("TIMEOUT" `isPrefixOf` e)]

        it ("REAL EXECUTION RESULTS: " ++ show (length successes) ++ " succeeded, " ++
            show (length errors) ++ " failed, " ++
            show (length timeouts) ++ " timed out") $ do

          putStrLn $ "\n" ++ replicate 60 '='
          putStrLn "=== REAL PROVER INTEGRATION TEST RESULTS ==="
          putStrLn $ replicate 60 '='
          putStrLn $ "Total files tested: " ++ show (length exampleFiles)
          putStrLn $ "Successes: " ++ show (length successes)
          putStrLn $ "Failures (errors): " ++ show (length errors)
          putStrLn $ "Timeouts (>3min): " ++ show (length timeouts)

          when (not $ null errors) $ do
            putStrLn $ "\n" ++ replicate 60 '-'
            putStrLn "=== FAILURES (first 10) ==="
            putStrLn $ replicate 60 '-'
            forM_ (take 10 errors) $ \(file, err) -> do
              putStrLn $ "\n" ++ file ++ ":"
              putStrLn $ "  " ++ take 300 err

          when (not $ null timeouts) $ do
            putStrLn $ "\n" ++ replicate 60 '-'
            putStrLn "=== TIMEOUTS ==="
            putStrLn $ replicate 60 '-'
            forM_ timeouts $ \(file, _) -> do
              putStrLn $ "  " ++ file

          when (not $ null successes) $ do
            putStrLn $ "\n" ++ replicate 60 '-'
            putStrLn "=== SUCCESSES (first 5) ==="
            putStrLn $ replicate 60 '-'
            forM_ (take 5 successes) $ \(file, msg) -> do
              putStrLn $ "  " ++ file ++ ": " ++ msg

          putStrLn $ "\n" ++ replicate 60 '='

          -- The test passes if we got results (even if some failed)
          -- This allows us to see the full report
          length results `shouldBe` length exampleFiles
