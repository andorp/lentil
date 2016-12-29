{-# LANGUAGE TemplateHaskell #-}

module Lentil.SQL.Test where

import Lentil.SQL.TH



data Test = Test { row1 :: Int, row2 :: String }

sqlCommands ''Test
