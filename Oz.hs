module Oz where

ozIntConst :: Int -> Integer -> [String]
ozIntConst register val = ["int_const r" <> show register <> ", " <> show val]

ozBoolConst :: Int -> Bool -> [String]
ozBoolConst register val = ["int_const r" <> show register <> ", " <> (if val then "1" else "0")]

ozWriteString :: String -> [String]
ozWriteString val =
    [ "load r0, \"" <> val <> "\""
    , "call_builtin print_string" ]
