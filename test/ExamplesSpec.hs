module ExamplesSpec (spec) where

import Test.Hspec
import Control.Exception (catch, SomeException)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Data.List (isSuffixOf, isInfixOf)
import Control.Monad (forM, forM_, filterM)

-- | Recursively find all .euclid files in a directory
findEuclidFiles :: FilePath -> IO [FilePath]
findEuclidFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      let paths = map (dir </>) entries

      -- Separate files and directories
      files <- filterM doesFileExist paths
      dirs <- filterM doesDirectoryExist paths

      -- Find .euclid files in current directory
      let euclidFiles = filter (\f -> takeExtension f == ".euclid") files

      -- Recursively search subdirectories
      subFiles <- concat <$> mapM findEuclidFiles dirs

      return (euclidFiles ++ subFiles)

-- | Read and parse a .euclid file (basic validation)
validateEuclidFile :: FilePath -> IO Bool
validateEuclidFile path = do
  content <- readFile path
  -- Basic validation: file should not be empty and should contain either:
  -- 1. REPL commands (:prove, :assume, etc.), OR
  -- 2. S-expressions (formulas in parentheses)
  let nonEmptyLines = filter (not . null) $ lines content
      hasCommands = any (":prove" `isInfixOf`) nonEmptyLines ||
                   any (":assume" `isInfixOf`) nonEmptyLines ||
                   any (":point" `isInfixOf`) nonEmptyLines ||
                   any (":exists" `isInfixOf`) nonEmptyLines ||
                   any (":forall" `isInfixOf`) nonEmptyLines ||
                   any (":define" `isInfixOf`) nonEmptyLines ||
                   any (":lagrange" `isInfixOf`) nonEmptyLines
      hasSExpressions = any ("(" `isInfixOf`) nonEmptyLines &&
                       any (")" `isInfixOf`) nonEmptyLines
  return (not (null nonEmptyLines) && (hasCommands || hasSExpressions))
  `catch` (\(_ :: SomeException) -> return False)

spec :: Spec
spec = do
  describe "Example Files" $ do
    it "examples directory exists" $ do
      exists <- doesDirectoryExist "examples"
      exists `shouldBe` True

    context "when loading example files" $ do
      exampleFiles <- runIO $ do
        exists <- doesDirectoryExist "examples"
        if exists
          then findEuclidFiles "examples"
          else return []

      if null exampleFiles
        then it "no example files found (skipping)" $ pendingWith "No .euclid files in examples/"
        else do
          it ("found " ++ show (length exampleFiles) ++ " example files") $ do
            length exampleFiles `shouldSatisfy` (> 0)

          forM_ exampleFiles $ \file -> do
            it ("successfully validates " ++ file) $ do
              result <- validateEuclidFile file
              result `shouldBe` True
