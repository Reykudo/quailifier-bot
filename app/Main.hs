{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib(mainFn)
import System.Environment.Extra ( getEnv )

main :: IO ()
main = do
    token <- getEnv "TG_BOT_QUALIFIER_TOKEN"
    -- putStrLn token
    mainFn token
