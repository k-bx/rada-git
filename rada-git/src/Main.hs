{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Semigroup
import Options.Applicative
import RadaGit.Config
import RadaGit.Projects (downloadProjects)
import RadaGit.Tests (runTests)
import Text.InterpolatedString.Perl6 (q)

data Commands
  = DownloadProjects
  | Test

commands :: Parser Commands
commands =
  subparser
    ((command
        "download-projects"
        (info
           (pure DownloadProjects)
           (progDesc "Завантажити свіжі законопроекти"))) <>
     (command "test" (info (pure Test) (progDesc "Виконати автоматичні тести"))))

main :: IO ()
main = do
  config <- readConfig "config.json"
  cmds <- execParser opts
  mainProg config cmds
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

mainProg :: Config -> Commands -> IO ()
mainProg cfg DownloadProjects = downloadProjects cfg
mainProg _cfg Test = runTests
