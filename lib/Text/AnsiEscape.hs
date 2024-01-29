module Text.AnsiEscape where

bold :: String -> String
bold str = "\x1b[1m" ++ str ++ "\x1b[0m"

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

orange :: String -> String
orange str = "\x1b[33m" ++ str ++ "\x1b[0m"

green :: String -> String
green str = "\x1b[32m" ++ str ++ "\x1b[0m"

gray :: String -> String
gray str = "\x1b[90m" ++ str ++ "\x1b[0m"
