{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Naver.Fortune
import qualified Data.Text.Lazy.IO as TL


main :: IO ()
main = do
    fortune <- tellFortune 1993 3 30 Male Solar Today Study
    TL.putStrLn fortune
