import Lib
import TestTools

main :: IO ()
main = 
  testSuite
    [ testGroup "Base Guessing" 
        [ assertEq "Guess base for decimal input"          (Just (baseName base10))   (baseName <$> guessBase "12345"),
          assertEq "Guess base for hexBE (explicit) input" (Just (baseName base16BE)) (baseName <$> guessBase "0x1A2B3C"),
          assertEq "Guess base for hexBE (implicit) input" (Just (baseName base16BE)) (baseName <$> guessBase "1A2B3C"),
          assertEq "Guess base for hexLE input"            (Just (baseName base16LE)) (baseName <$> guessBase "1A 2B 3C"),
          assertEq "Guess base for invalid input"          Nothing                    (baseName <$> guessBase "GHIJKL")
        ]
    , testGroup "Base Conversion" 
        [ assertEq "Convert decimal to hexBE" (Just "0x1A2B3C") (readBase base10   "1715004"  >>= showBase base16BE),
          assertEq "Convert decimal to hexLE" (Just "3C 2B 1A") (readBase base10   "1715004"  >>= showBase base16LE),
          assertEq "Convert hexBE to decimal" (Just "1715004")  (readBase base16BE "1A2B3C"   >>= showBase base10  ),
          assertEq "Convert hexLE to decimal" (Just "1715004")  (readBase base16LE "3C 2B 1A" >>= showBase base10  )
        ]
    ]