module Main (main) where

import Data.Char
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import Control.Monad (forM_)

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
                Nothing -> putStrLn $ "Failed to parse input \'" ++ inputStr ++ "\' as " ++ name b
                Just n -> do
                  putStrLn $ "Parsed input \'" ++ inputStr ++ "\' as " ++ name b
                  forM_ [base10, base16BE, base16LE] $ \otherBase -> do
                      case showBase otherBase n of
                        Just s -> putStrLn $ "  " ++ name otherBase ++ ": " ++ s
                        Nothing -> putStrLn $ "  Failed to convert to " ++ name otherBase


data Args = Args
  { input :: String,
    base :: Maybe Base
  }


guessBase :: String -> Maybe Base
guessBase s
  | "0x" `isPrefixOf` nospaceLower s = Just base16BE
  | isdec s = Just base10
  | ishex s && any (== ' ') s = Just base16LE
  | ishex s = Just base16BE
  | otherwise = Nothing

data Base = Base
  { name :: String
  , readBase :: String -> Maybe Integer
  , showBase :: Integer -> Maybe String
  }

isdec :: String -> Bool
isdec s = all (`elem` ['0'..'9']) s

ishex :: String -> Bool
ishex s = all (`elem` "0123456789ABCDEFabcdef ") s


base10 :: Base
base10 = Base
  { name = "dec"
  , readBase = \s -> if isdec s then Just (read s) else Nothing
  , showBase = pure . show
  }

base16BE :: Base
base16BE = Base
  { name = "hexBE"
  , readBase = readHexBE
  , showBase = showHexBE
  }

base16LE :: Base
base16LE = Base
  { name = "hexLE"
  , readBase = readHexLE
  , showBase = showHexLE
  }
nospaceLower :: String -> String
nospaceLower = filter (not . (`elem` [' ', '\t', '\n'])) . map toLower

fromHexDigit :: Char -> Maybe Int
fromHexDigit c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
  | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
  | otherwise            = Nothing

toHexDigit :: Int -> Maybe Char
toHexDigit n
  | n >= 0 && n <= 9 = Just (toEnum (n + fromEnum '0'))
  | n >= 10 && n <= 15 = Just (toEnum (n - 10 + fromEnum 'A'))
  | otherwise = Nothing

toByteDigits :: Int -> Maybe [Char]
toByteDigits n
  | n >= 0 && n <= 255 = do
      let high = n `div` 16
          low = n `mod` 16
      highDigit <- toHexDigit high
      lowDigit <- toHexDigit low
      Just [highDigit, lowDigit]
  | otherwise = Nothing

-- 2桁ごとに逆順で処理する
readHexLE :: String -> Maybe Integer
readHexLE str = do
  let nospace = nospaceLower str
      str' = case nospace of
                ('0':'x':xs) -> xs
                _            -> nospace
  hexDigits <- reverse <$> traverse fromHexDigit str'
  pure (let go acc []          =     acc
            go acc [d1]        =     acc * 256 + fromIntegral d1
            go acc [d1,d16]    =     acc * 256 + fromIntegral d1 + fromIntegral d16 * 16
            go acc (d1:d16:ds) = go (acc * 256 + fromIntegral d1 + fromIntegral d16 * 16) ds
        in go 0 hexDigits)

showHexLE :: Integer -> Maybe String
showHexLE n
  | n < 0     = Nothing
  | otherwise =
      let go x =
            if x == 0 then Just []
            else let (q,r) = x `divMod` 256
                in case toByteDigits (fromIntegral r) of
                      Just digits -> ((" " ++ digits) ++) <$> go q
                      Nothing     -> Nothing
      in dropWhile (== ' ') <$> go n

readHexBE :: String -> Maybe Integer
readHexBE str = do
  let nospace = nospaceLower str
      str' = case nospace of
                ('0':'x':xs) -> xs
                _            -> nospace
  hexDigits <- traverse fromHexDigit str'
  pure (foldl (\acc d -> acc * 16 + fromIntegral d) 0 hexDigits)

showHexBE :: Integer -> Maybe String
showHexBE n
  | n < 0     = Nothing
  | otherwise =
      let go x =
            if x == 0 then Just []
            else let (q,r) = x `divMod` 16
                in case toHexDigit (fromIntegral r) of
                      Just digit -> (digit :) <$> go q
                      Nothing    -> Nothing
      in case reverse <$> go n of
           Just ds -> Just ('0':'x':ds) 
           Nothing -> Nothing