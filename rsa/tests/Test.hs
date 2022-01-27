module Main where

import WuRSA
import Test.HUnit
import qualified PrimeTests     as PT
import qualified RSATests       as RT
import qualified MathTests      as MT
import qualified EncodingTests  as ET

main :: IO Counts
main = runTestTT . TestList $ 
    PT.tests <> 
    RT.tests <> 
    MT.tests <>
    ET.tests
