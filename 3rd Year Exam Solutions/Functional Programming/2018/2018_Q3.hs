import System.FilePath.Posix
import Data.Char
import System.IO
import System.Directory

main = do
  putStrLn "Please enter the filename:"
  line <- getLine
  if null line
    then return()
  else do
    putStrLn $ toDOS line
    lowify $ toDOS line
    main


toDOS :: String -> String
toDOS fileName =
  let baseName = take 8 $ takeBaseName fileName
      extension = take 4 $ takeExtension fileName
      updatedName = (map toUpper baseName) ++ (map toUpper extension)
  in updatedName

lowify :: String -> IO ()
lowify fileName = do
  inHandle <- openFile fileName ReadMode
  outHandle <- openFile "LOWER.OUT" WriteMode
  inputString <- hGetContents inHandle
  let result = processLine inputString
  hPutStr outHandle result
  hClose inHandle
  hClose outHandle

processLine :: String -> String
processLine = map toLower
