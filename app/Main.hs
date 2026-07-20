module Main (main) where

import System.Environment (getArgs)
import Control.Monad (forM_)
import Lib

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = 
        foldl (\acc arg -> 
          case arg of
            "-d" -> acc { base = Just base10 }
            "-b" -> acc { base = Just base16BE }
            "-l" -> acc { base = Just base16LE }
            _    -> acc { input = input acc ++ " " ++ arg }
        ) (Args "" Nothing) args
  let inputStr = dropWhile (== ' ') $ input parsedArgs
      base' = case base parsedArgs of
                Just b -> Just b
                Nothing -> guessBase inputStr
  case base' of
    Nothing -> putStrLn "Could not guess base. Please specify with -d (dec), -b (hexBE), or -l (hexLE)."
    Just b -> case readBase b inputStr of
                Nothing -> putStrLn $ "Failed to parse input \'" ++ inputStr ++ "\' as " ++ baseName b
                Just n -> do
                  putStrLn $ "Parsed input \'" ++ inputStr ++ "\' as " ++ baseName b
                  forM_ [base10, base16BE, base16LE] $ \otherBase -> do
                      case showBase otherBase n of
                        Just s -> putStrLn $ "  " ++ baseName otherBase ++ ": " ++ s
                        Nothing -> putStrLn $ "  Failed to convert to " ++ baseName otherBase


data Args = Args
  { input :: String,
    base :: Maybe Base
  }


