{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad
import Options.Applicative

import Details


opts :: Parser (IO ())
opts = subparser
  ( command "details" (info (details <$> option str ( long "file" <> short 'f' )) idm)
  <> command "stop"  (info (pure stop) idm) )

main :: IO ()
main = join $ execParser (info opts idm)


stop :: IO ()
stop = print ("yo"::String)
