module EncodingTests where

import Test.HUnit
import WuEncoding


canEncodeA = TestCase $ assertEqual
    "for encoding the word 'code' with a=1"
    70097
    (encodeA "code")

canEncodeB = TestCase $ assertEqual
    "for encoding the word 'code' with a=0"
    44698
    (encodeB "code")

canDecodeA = TestCase $ assertEqual
    "for decoding the message 70097 with a=1"
    "code"
    (decodeA 70097)

canDecodeB = TestCase $ assertEqual
    "for decoding the message 44698 with a=0"
    "code"
    (decodeB 44698)

determinesMessageSize1 = TestCase $ assertEqual
    "for finding message size"
    5
    (messageSizeB 11881375)

determinesMessageSize2 = TestCase $ assertEqual
    "for finding message size"
    5
    (messageSizeB 11881376)

determinesMessageSize3 = TestCase $ assertEqual
    "for finding message size"
    4
    (messageSizeB 11881374)

determinesMessageSize4 = TestCase $ assertEqual
    "for finding message size"
    5
    (messageSizeA 14348906)

determinesMesageSize5 = TestCase $ assertEqual
    "for finding message size"
    5
    (messageSizeA 14348907)

determinesMesageSize6 = TestCase $ assertEqual
    "for finding message size"
    4
    (messageSizeA 14348905)


tests = 
    [ TestLabel "canEncodeA" canEncodeA
    , TestLabel "canEncodeB" canEncodeB
    , TestLabel "canDecodeA" canDecodeA
    , TestLabel "canDecodeB" canDecodeB
    , TestLabel "determinesMessageSize1" determinesMessageSize1
    , TestLabel "determinesMessageSize2" determinesMessageSize2
    , TestLabel "determinesMessageSize3" determinesMessageSize3
    , TestLabel "determinesMessageSize4" determinesMessageSize1
    , TestLabel "determinesMessageSize5" determinesMessageSize2
    , TestLabel "determinesMessageSize6" determinesMessageSize3
    ]
