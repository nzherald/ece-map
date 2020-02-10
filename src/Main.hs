{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad
import Options.Applicative

import Details
import EroReports


opts :: Parser (IO ())
opts = subparser
  ( command "details" (info (details <$> option str ( long "file" <> short 'f' )) idm)
  <> command "ero"  (info (pure get) idm) )

main :: IO ()
main = join $ execParser (info opts idm)


