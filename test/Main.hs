{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.ByteString as BS
import           Control.Applicative
import           Control.Monad
import           Test.Hspec
import           Test.HUnit

import Graphics.Netpbm


checkSinglePPM :: PPMType -> (Int, Int) -> PpmParseResult -> Assertion
checkSinglePPM typ size parseResult = case parseResult of
  Right ([PPM { ppmType, ppmWidth, ppmHeight }], rest) -> (ppmType, (ppmWidth, ppmHeight), rest) `shouldBe` (typ, size, Nothing)
  Right (ppms, _)                                      -> assertFailure $ "expected only one image, but got " ++ show (length ppms)
  _                                                    -> assertFailure "image parse failed"


parse :: FilePath -> IO PpmParseResult
parse f = parsePPM <$> BS.readFile f


parseTestFile :: String -> String -> (PpmParseResult -> Assertion) -> Spec
parseTestFile name desc check = it ("parses " ++ desc ++ " (" ++ name ++ ")") $
  parse ("test/ppms/" ++ name) >>= check


main :: IO ()
main = hspec $ do
  describe "P6 PPM (color binary)" $ do

    parseTestFile "gimp.ppm" "a file produced by GIMP" $
      checkSinglePPM P6 (640,400)

    parseTestFile "gitlogo.ppm" "a file produced convert" $
      checkSinglePPM P6 (220,92)

    parseTestFile "image.ppm" "some random file from the internet" $
      checkSinglePPM P6 (1200,1200)

    parseTestFile "testimg.ppm" "the color file from the netpbm test suite" $
      checkSinglePPM P6 (227,149)

    parseTestFile "face.ppm" "a PPM with a trailing newline" $
      checkSinglePPM P6 (512,512)

    -- parseTestFile "gitlogo-double.ppm" "a multi-image file" $ do
    --   checkSinglePPM P6 (220,92)
