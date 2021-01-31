module Main where

import RegularExpression
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if args == [] then usage else do
    case (parseRegularExpression $ head args) of
      (Just regex) -> repl regex
      Nothing -> putStrLn "Failed to parse regular expression"
  where
    usage = do
      programName <- getProgName
      putStrLn $ "Usage: " ++ programName ++ " [REGEX]"

    repl regex = do
      putStr "=> "
      hFlush stdout
      string <- getLine
      putStrLn $ if (acceptsRegex regex string)
                    then "Accepted"
                    else "Rejected"
      repl regex
