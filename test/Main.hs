module Main where

import Prelude hiding (assert)
import GHC.Exts (fromList)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified YamlUnscrambler as U
import qualified Test.QuickCheck as QuickCheck
import qualified Control.Foldl as Fold
import qualified NeatInterpolation as NeatInterpolation
import qualified Data.Text as Text


main =
  defaultMain $ 
  testGroup "All tests" [
    testCase "Should fail on sequence when no sequence is specified" $ let
      unscrambler =
        U.value [] (Just mapping) Nothing
        where
          mapping =
            U.foldMapping (,) Fold.list U.textString value
            where
              value =
                U.value [nullScalar, intScalar] Nothing Nothing
                where
                  nullScalar =
                    U.nullScalar Nothing
                  intScalar =
                    fmap Just $ U.boundedIntegerScalar @Int (U.Signed True) U.DecimalNumeralSystem
      input =
        [NeatInterpolation.text|
          a: 2
          b: 3
          c:
            - 1
            - 2
          |]
      in case U.parseText unscrambler input of
        Right res ->
          assertFailure (show res)
        Left failure ->
          assertEqual (Text.unpack failure) "/c: Unexpected sequence node" failure
    ,
    testCase "Domain sum-type regression" $ let
      unscrambler =
        doc
        where
          doc =
            U.mappingValue $
            U.byKeyMapping (U.CaseSensitive True) $
            asum [
              Just <$> U.atByKey "sums" sum,
              pure Nothing
              ]
          sum =
            U.mappingValue $
            U.foldMapping (,) Fold.list U.textString sumVariant
          sumVariant =
            U.mappingValue $
            U.foldMapping (,) Fold.list U.textString params
          params =
            U.value [nullScalar, intScalar] Nothing Nothing
            where
              nullScalar =
                U.nullScalar Nothing
              intScalar =
                fmap Just $ U.boundedIntegerScalar @Int (U.Signed True) U.DecimalNumeralSystem
      input =
        [NeatInterpolation.text|
          sums:
            A:
              a:
                - Int
                - Bool
              b: Char, Double
          |]
      in case U.parseText unscrambler input of
        Right res ->
          assertFailure (show res)
        Left failure ->
          assertEqual "" "/sums/A/a: Unexpected sequence node" failure
    ,
    testCase "Domain sum-type correct" $ let
      unscrambler =
        doc
        where
          doc =
            U.mappingValue $
            U.byKeyMapping (U.CaseSensitive True) $
            asum [
              Just <$> U.atByKey "sums" sum,
              pure Nothing
              ]
          sum =
            U.mappingValue $
            U.foldMapping (,) Fold.list U.textString sumVariant
          sumVariant =
            U.mappingValue $
            U.foldMapping (,) Fold.list U.textString params
          params =
            U.value [nullScalar, valueScalar] Nothing Nothing
            where
              nullScalar =
                U.nullScalar Nothing
              valueScalar =
                fmap Just $ U.stringScalar U.textString
      input =
        [NeatInterpolation.text|
          sums:
            A:
              a: Text
              b: Int
          |]
      in case U.parseText unscrambler input of
        Right res ->
          assertEqual "" (Just [("A", [("a", Just "Text"), ("b", Just "Int")])]) res
        Left failure ->
          assertFailure (Text.unpack failure)
    ]
