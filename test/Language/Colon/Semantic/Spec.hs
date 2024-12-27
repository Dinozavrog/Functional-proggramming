{-# LANGUAGE OverloadedRecordDot #-}

module Language.Colon.Semantic.Spec where

import Test.Hspec hiding (example)
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck (Arbitrary (..), suchThat, arbitraryPrintableChar, listOf)
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck (total)
import Test.QuickCheck.Property ((===))

import GHC.Generics (Generic)

import System.FilePath ((<.>), (</>))

import Data.Foldable (toList, for_)
import Data.Either (isRight, fromRight)

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Exception (displayException, try)

import Language.Colon.Semantic (EvalState (..), LoopFrame (..), ThrowException (..), builtInWords, denote, emptyEvalState, CaseInsensitiveString (..))
import Language.Colon.Syntax (Code, Command (..), StringSource)
import Language.Colon.Parsing (parse)

instance {-# overlaps #-} Arbitrary StringSource where
  arbitrary = elements (toList builtInWords)

instance Arbitrary LoopFrame where
  arbitrary = do
    limit <- arbitrary `suchThat` small
    index <- arbitrary `suchThat` \x -> small x && x <= limit
    pure MkLoopFrame{..}
    where
      small x = abs x < 10

instance Arbitrary EvalState where
  arbitrary = do
    stack <- arbitrary
    loops <- arbitrary
    input <- listOf arbitraryPrintableChar
    output <- listOf arbitraryPrintableChar
    let definitions = mempty
    let variables = mempty
    let constants = mempty
    let memory = mempty
    return MkEvalState{..}

instance Arbitrary Command where
  arbitrary = do
    intVal <- arbitrary @Int
    string <- arbitrary @StringSource
    str <- listOf arbitraryPrintableChar
    first <- arbitrary @Code
    second <- arbitrary @Code
    elements
      [ Int intVal
      , Name string
      , Str str
      , IfElse first second
      , DoILoop first ]

deriving stock instance Generic LoopFrame
deriving stock instance Generic CaseInsensitiveString
deriving stock instance Generic EvalState
deriving stock instance Generic ThrowException
deriving stock instance Generic Command

deriving anyclass instance NFData LoopFrame
deriving anyclass instance NFData CaseInsensitiveString
deriving anyclass instance NFData EvalState
deriving anyclass instance NFData ThrowException
deriving anyclass instance NFData Command

spec :: Spec
spec = do

  describe "Примеры" do

    describe "Арифметика" do

      example "2 2 3 +" \result -> do
        result.stack `shouldBe` [2, 5]

      example "1 10 3 + +" \result -> do
        result.stack `shouldBe` [14]

      example "10 254 31 + + +" \result -> do
        result.error `shouldBe` "Stack underflow"

      example "3 4 *" \result -> do
        result.stack `shouldBe` [12]

      example "4 5 /" \result -> do
        result.stack `shouldBe` [0]

      example "8 3 DIGITMOD" \result -> do
        result.stack `shouldBe` [2]

    describe "Работа со словами" do

      example "aboba" \result -> do
        result.error `shouldBe` "Unknown - aboba"

      example ": aboba 10 + ; 2 aboba" \result -> do
        result.stack `shouldBe` [12]

      exampleFile "factorial"


    describe "Комменты" do

      example "( + 2 3 ( - 3 2 ) )" \result -> do
        result.stack `shouldBe` []
        result.ok `shouldBe` True


    describe "Работа со стеком" do

      example "1 2 3 4 DROP" \result -> do
        result.stack `shouldBe` [1, 2, 3]

      example "1 2 3 4 SWAP" \result -> do
        result.stack `shouldBe` [1, 2, 4, 3]

      example "1 2 3 4 OVER" \result -> do
        result.stack `shouldBe` [1, 2, 3, 4, 3]

      example "1 2 3 4 ROT" \result -> do
        result.stack `shouldBe` [1, 3, 4, 2]


    describe "Операции ввода вывод" do

      example "1 2 PRINT PRINT 3 PRINT 4" \result -> do
        result.output `shouldBe` "2 1 3 "
        result.stack `shouldBe` [4]

      example "CR 100 PRINT CR 200 PRINT" \result -> do
        result.output `shouldBe` "\n100 \n200 "

      example "87 emit 111 emit 119 emit 33 emit" \result -> do
        result.output `shouldBe` "Wow!"

      example ": hi .\" Hello!\" ; hi" \result -> do
        result.output `shouldBe` "Hello!"

      example ".\"  Hello \"" \result -> do
        result.output `shouldBe` " Hello "

      example ".\" Hello\"" \result -> do
        result.output `shouldBe` "Hello"

      example ".\" \\\" \"" \result -> do
        result.output `shouldBe` "\" "

      exampleFile "uppercase"


    describe "Работа с булевыми операторами" do

      example "3 4 =" \result -> do
        result.stack `shouldBe` [0]

      example "5 5 =" \result -> do
        result.stack `shouldBe` [-1]

      example "1 10 <" \result -> do
        result.stack `shouldBe` [-1]

      example "1 0 >" \result -> do
        result.stack `shouldBe` [-1]

    describe "Цикл" do

      example "5 0 DO looper" \result -> do
        result.stack `shouldBe` [0, 1, 2, 3, 4]

      exampleFile "multiplication-table"


    describe "Работа с памятью" do

      example "var v1   v1 PRINT  (;)  123 v1 !  (;)  v1 @" \result -> do
        result.output `shouldBe` "1000 "
        result.stack `shouldBe` [123]

      exampleFile "variables"

      example "42 const_val answer (;) answer" \result -> do
        result.stack `shouldBe` [42]


  describe "denote" do

    prop "is concatenative"
      [ denote (a ++ b) s === (denote a >=> denote b) s
      | s <- arbitrary @EvalState
      , p <- validProgramForState s
      , (a, b) <- splitListArbitrary p
      ]

    prop "is total"
      [ total (denote p s)
      | p <- arbitrary @Code
      , s <- arbitrary @EvalState
      ]

  where
    validProgramForState s =
      arbitrary `suchThat` \p -> isRight (denote p s)

    splitListArbitrary xs = do
      ix <- elements [0..length xs]
      pure (splitAt ix xs)

data ExampleResult = MkExampleResult
  { stack :: [Int]
  , error :: String
  , output :: String
  , ok :: Bool
  }

example :: String -> (ExampleResult -> Expectation) -> Spec
example program k = example' program program "" k

exampleWithInput :: String -> String -> (ExampleResult -> Expectation) -> Spec
exampleWithInput program = example' program program

example' :: String -> String -> String -> (ExampleResult -> Expectation) -> Spec
example' name program input k = do
  it name do
    case parse program of
      Left err -> k (failure (displayException err))
      Right parsed -> do
        case denote parsed emptyEvalState{input} of
          Left err -> k (failure (displayException err))
          Right state -> k (success state)
  where
    failure exception = MkExampleResult
      { stack = error exception
      , output = error exception
      , error = exception
      , ok = False }
    success MkEvalState{stack, output} = MkExampleResult
      { stack = reverse stack
      , output = reverse output
      , error = ""
      , ok = True }

exampleFile :: FilePath -> Spec
exampleFile path = do
  input  <- runIO do try @IOError (readFile inputPath)
  output <- runIO do try @IOError (readFile outputPath)
  source <- runIO do readFile sourcePath
  example' sourcePath source (fromRight "" input) \result -> do
    result.ok `shouldBe` True
    for_ output \output -> do
      result.output `shouldBe` output
  where
    inputPath  = "examples" </> path <.> "input"
    outputPath = "examples" </> path <.> "output"
    sourcePath = "examples" </> path <.> "colon"