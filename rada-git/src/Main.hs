{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Semigroup
import Options.Applicative
import Text.InterpolatedString.Perl6 (q)

data Commands =
  DownloadProjects

commands :: Parser Commands
commands =
  subparser
    (command
       "download-projects"
       (info
          (pure DownloadProjects)
          (progDesc "Завантажити свіжі законопроекти")))

main :: IO ()
main = mainProg =<< execParser opts
  where
    opts =
      info
        (commands <**> helper)
        (fullDesc <> progDesc progDescText <> header headerText)

progDescText :: String
progDescText = [q|"Možlyvo varto Latynkoju?"|]

headerText :: String
headerText =
  [q|Структурний парсинг та представлення документів rada.gov.ua у вигляді git-репозиторію https://github.com/k-bx/rada-git|]

mainProg :: Commands -> IO ()
mainProg DownloadProjects = downloadProjects

downloadProjects :: IO ()
downloadProjects = return ()
