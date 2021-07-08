module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

adder :: Int -> Int -> Int
adder a b = a + b
