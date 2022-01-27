module Parse where

import qualified Text.Read          as R
import qualified Data.Maybe         as M
import qualified Data.Map.Strict    as Map
import qualified Data.Either        as E

type Argv = [String]

type FlaggedArg = (Flag, Arg)

type Argmap = Map.Map Flag Arg

data Flag = M | T | E | Pub | Priv | S deriving (Eq, Ord)

data Arg = Mode ModeChoice
         | Text String 
         | Encoding Int 
         | PrivateKey { p :: Integer, q :: Integer, e :: Integer }
         | PublicKey { m :: Integer, e :: Integer }
         | Size Int
         deriving Eq

data ModeChoice = Encrypt | Decrypt | Generate | Validate deriving Eq

parse :: Argv -> Either String Argmap
parse args = foldl cautiousInsert (Right Map.empty) (consumeAll args)

cautiousInsert :: Either String Argmap      -> 
                  Either String (Flag, Arg) -> 
                  Either String Argmap
cautiousInsert (Left s) _ = Left s
cautiousInsert _ (Left s) = Left s
cautiousInsert (Right m) (Right (f, a))
    | M.isJust (Map.lookup f m) = Left "Repeated argument"
    | otherwise                 = Right (Map.insert f a m)

consumeAll :: Argv -> [Either String FlaggedArg]
consumeAll [] = []
consumeAll args 
    | E.isLeft arg  = [arg]
    | otherwise     = arg : consumeAll rest
    where (arg, rest) = consumeArg args

-- this returns the offending values when it detects an error
-- but I'm not taking advantage of that yet
consumeArg :: Argv -> (Either String FlaggedArg, Argv)
consumeArg ("-m":m:args) 
    | m == "encrypt"    = (Right (M, Mode Encrypt), args)
    | m == "decrypt"    = (Right (M, Mode Decrypt), args)
    | m == "generate"   = (Right (M, Mode Generate), args)
    | m == "validate"   = (Right (M, Mode Validate), args)
    | otherwise         = (Left "Unexpected mode", [m])
consumeArg ("-t":t:args) = (Right (T, Text t), args)
consumeArg ("-e":e:args)
    | e == "0"          = (Right (E, Encoding 0), args)
    | e == "1"          = (Right (E, Encoding 1), args)
    | otherwise         = (Left "Unexpected encoding", [e])
consumeArg ("--private":p:q:e:args)
    | any M.isNothing vals  = (Left "Invalid key argument", [p,q,e])
    | otherwise             = (Right (Priv, PrivateKey p'' q'' e''), args)
    where
        p' = R.readMaybe p :: Maybe Integer
        q' = R.readMaybe q :: Maybe Integer
        e' = R.readMaybe e :: Maybe Integer
        vals = [p', q', e']
        p'' = M.fromJust p'
        q'' = M.fromJust q'
        e'' = M.fromJust e'
consumeArg ("--public":m:e:args)
    | any M.isNothing vals  = (Left "Invalid key argument", [m, e])
    | otherwise             = (Right (Pub, PublicKey m'' e''), args)
    where
        m' = R.readMaybe m :: Maybe Integer
        e' = R.readMaybe e :: Maybe Integer
        vals = [m', e']
        m'' = M.fromJust m'
        e'' = M.fromJust e'
consumeArg ("-s":s:args)
    | M.isNothing s'    = (Left "Invalid key size argument", [s])
    | otherwise         = (Right (S, Size s''), args)
    where
        s'  = R.readMaybe s :: Maybe Int
        s'' = M.fromJust s'
consumeArg args = (Left "Unexpected arguments", args)
