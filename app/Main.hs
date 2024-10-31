module Main (main) where

import Lib

main :: IO ()
main = do
    print a
    print b
    print c
a :: OADict Int Int
a = Lib.fromList [(1, 12), (2, 44)]
b :: OADict Int Int
b = Lib.fromList [(1, 12), (2, 33), (4, 123)]
c = a <> b
