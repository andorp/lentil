{-# LANGUAGE TemplateHaskell #-}

module Lentil.Backend.SQLite.Test where

import Lentil.Backend.SQLite.TH



data Test = Test { row1 :: Int, row2 :: String }

makeEntity ''Test