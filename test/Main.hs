{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.ByteString as BS
import           Control.Applicative
import           Control.Monad
import           Test.Hspec
import           Test.HUnit

import Graphics.Netpbm


checkSinglePPM :: PPMType -> (Int, Int) -> PpmParseResult -> Assertion
checkSinglePPM typ size parseResult = case parseResult of
  Right ([PPM { ppmHeader = PPMHeader { ppmType, ppmWidth, ppmHeight } }], rest) -> (ppmType, (ppmWidth, ppmHeight), rest) `shouldBe` (typ, size, Nothing)
  Right (ppms, _)                                      -> assertFailure $ "expected only one image, but got " ++ show (length ppms)
  _                                                    -> assertFailure "image parse failed"


shouldNotParse :: PpmParseResult -> Assertion
shouldNotParse res = case res of
  Left _ -> return ()
  Right r -> assertFailure $ "should not parse, but parses as: " ++ show r


parse :: FilePath -> IO PpmParseResult
parse f = parsePPM <$> BS.readFile f


parseTestFile :: String -> String -> (PpmParseResult -> Assertion) -> Spec
parseTestFile name desc check = it (desc ++ " (" ++ name ++ ")") $
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

    describe "more test files from the internet" $ do
      forM_
        [ ("boxes_1.ppm", (63,63))
        , ("boxes_2.ppm", (63,63))
        , ("house_1.ppm", (111,132))
        , ("house_2.ppm", (111,132))
        , ("moreboxes_1.ppm", (63,63))
        , ("moreboxes_2.ppm", (63,63))
        , ("sign_1.ppm", (99,99))
        , ("sign_2.ppm", (99,99))
        , ("stop_1.ppm", (99,99))
        , ("stop_2.ppm", (99,99))
        , ("synth_1.ppm", (100,100))
        , ("synth_2.ppm", (100,100))
        , ("tree_1.ppm", (133,133))
        , ("tree_2.ppm", (133,133))
        , ("west_1.ppm", (366,216))
        , ("west_2.ppm", (366,216))
        ] $ \(f, size) ->
          parseTestFile ("internet/set1/" ++ f) "from the internet" $
            checkSinglePPM P6 size


    parseTestFile "gitlogo-double.ppm" "a multi-image file" $ do
      \res -> case res of
        Right ([ PPM { ppmHeader = h1 }
               , PPM { ppmHeader = h2 }], Nothing) -> do h1 `shouldBe` PPMHeader P6 220 92
                                                         h2 `shouldBe` PPMHeader P6 220 92
        Right r                                    -> assertFailure $ "parsed unexpected: " ++ show r
        Left e                                     -> assertFailure $ "did not parse: " ++ e


    describe "comments" $ do

      parseTestFile "gitlogo-comment-after-magic-number.ppm" "a comment directly after the P6" $
        checkSinglePPM P6 (220,92)

      parseTestFile "gitlogo-only-spaces-in-header.ppm" "only spaces as header separators" $
        checkSinglePPM P6 (220,92)


    describe "weird files that are still OK with the spec" $ do

      parseTestFile "weird/gitlogo-width-0.ppm" "width '00' set in an image" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Just rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 0
                                                       assertBool "missing rest" (BS.length rest > 200)
          Right r                                -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                 -> assertFailure $ "did not parse: " ++ e

      parseTestFile "weird/gitlogo-comments-everywhere.ppm" "comments inside numbers" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Nothing) -> do ppmHeader `shouldBe` PPMHeader P6 220 92
          Right r                              -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                               -> assertFailure $ "did not parse: " ++ e


    describe "partially valid files of which we parse as much as we can" $ do

      parseTestFile "graceful/face.ppm" "a PPM with a trailing newline" $
        checkSinglePPM P6 (512,512)

      parseTestFile "graceful/gitlogo-one-and-a-half.ppm" "a multi-image file where the second image is chopped off" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Just rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 92
                                                       assertBool "missing rest" (BS.length rest > 200)
          Right r                                -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                 -> assertFailure $ "did not parse: " ++ e

      parseTestFile "graceful/gitlogo-double-with-whitespace-in-between.ppm" "a multi-image file with whitespace between the images" $
        \res -> case res of
          Right ([ PPM { ppmHeader = h1 }
                 , PPM { ppmHeader = h2 }], Nothing) -> do h1 `shouldBe` PPMHeader P6 220 92
                                                           h2 `shouldBe` PPMHeader P6 220 92
          Right r                                    -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                     -> assertFailure $ "did not parse: " ++ e


    describe "negative examples" $ do

      parseTestFile "bad/gitlogo-width--1.ppm" "width '-1' set in an image" shouldNotParse

      parseTestFile "bad/gitlogo-not-enough-data.ppm" "not containing (width * height) bytes" shouldNotParse

      parseTestFile "bad/gitlogo-comment-in-magic-number.ppm" "comment inside magic number" shouldNotParse

      parseTestFile "bad/gitlogo-comment-without-following-extra-newline-before-data-block.ppm" "no non-comment whitespace before data block" shouldNotParse
