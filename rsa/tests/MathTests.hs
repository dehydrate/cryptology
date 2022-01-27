module MathTests where

import Test.HUnit
import Math

canDoModExponents1 = TestCase $ assertEqual
    "42^42 (mod 50)"
    14
    (modularExponent 42 42 50)

canDoModExponents2 = TestCase $ assertEqual
    "10^200 (mod 13)"
    9
    (modularExponent 10 200 13)

canDoModExponents3 = TestCase $ assertEqual
    "1001^654321 (mod 7890)"
    4841
    (modularExponent 1001 654321 7890)

tests = 
    [ TestLabel "canDoModExponents1" canDoModExponents1
    , TestLabel "canDoModExponents2" canDoModExponents2
    , TestLabel "canDoModExponents3" canDoModExponents3
    ]
