{-# LANGUAGE TemplateHaskell #-}
module Control.Elim.Test where

import Control.Elim



data Test1 = Test11 | Test12 Int | Test13 String

makeElim ''Test1

newtype Test2 = Test1 String

makeElim ''Test2

data Test3 = Test31 { t311 :: Int, t312 :: String }
           | Test32 { t321 :: Bool }

makeElim ''Test3
