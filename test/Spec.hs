{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map
import qualified Data.Set    as Set
import           Lib
import           Test.Hspec
import           Text.Parsec (parse, runParser)

main :: IO ()
main = hspec $ do
  describe "Lib.parseVal" $ do
    it "should parse Integers" $ do
      parseTest parseVal "1" `shouldBe` (Right (Integer 1))

    it "should parse Doubles" $ do
      parseTest parseVal "1.0" `shouldBe` (Right (Double 1.0))

    it "should parse Bools" $ do
      parseTest parseVal "True" `shouldBe` (Right (Bool True))

    it "should parse Chars" $ do
      parseTest parseVal "\'c\'" `shouldBe` (Right (Char 'c'))

    it "should parse Strings" $ do
      parseTest parseVal "\"test\"" `shouldBe` (Right (String "test"))

    it "should parse NTuples" $ do
      parseTest parseVal "(1, 2)" `shouldBe`
        (Right (Tuple (Integer 1, Integer 2)))

  describe "Lib.parseExpr" $ do
    it "should parse Unions" $ do
      parseTest parseExpr "a ∪ b" `shouldBe`
        (Right (Union (Identifier "a") (Identifier "b")))

    it "should parse Intersections" $ do
      parseTest parseExpr "a ∩ b" `shouldBe`
        (Right (Intersection (Identifier "a") (Identifier "b")))

    it "should parse Differences with backslash" $ do
      parseTest parseExpr "a \\ b" `shouldBe`
        (Right (Difference (Identifier "a") (Identifier "b")))

    it "should parse Differences with minus sign" $ do
      parseTest parseExpr "a - b" `shouldBe`
        (Right (Difference (Identifier "a") (Identifier "b")))

    it "should parse Cartesian Products" $ do
      parseTest parseExpr "a × b" `shouldBe`
        (Right (CartesianProduct (Identifier "a") (Identifier "b")))

parseTest parser str = parse parser "" str
