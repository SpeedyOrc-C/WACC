module Text.AnsiEscape where

-- Adds ANSI escape codes for bold text.
bold :: String -> String
bold str = "\x1b[1m" ++ str ++ "\x1b[0m"

-- Adds ANSI escape codes for red text.
red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

-- Adds ANSI escape codes for orange text.
orange :: String -> String
orange str = "\x1b[33m" ++ str ++ "\x1b[0m"

-- Adds ANSI escape codes for green text.
green :: String -> String
green str = "\x1b[32m" ++ str ++ "\x1b[0m"

-- Adds ANSI escape codes for gray text.
gray :: String -> String
gray str = "\x1b[90m" ++ str ++ "\x1b[0m"
