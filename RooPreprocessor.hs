module RooPreprocessor where

import Text.Regex

includeMatcher :: Regex
includeMatcher = mkRegexWithOpts "^#include *\"(.+)\" *$" True True

extractIncludes :: String -> Maybe [String]
extractIncludes = matchRegex includeMatcher
