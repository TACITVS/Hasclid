module ReplSupport
  ( consumeBalancedScript
  , parenBalance
  , stripComment
  ) where

import Data.List (isPrefixOf)

-- Strip line comments
stripComment :: String -> String
stripComment = stripLine
  where
    stripLine [] = []
    stripLine ('-':'-':_) = []
    stripLine ('/':'/':_) = []
    stripLine ('#':_) = []
    stripLine (';':_) = []
    stripLine (c:cs) = c : stripLine cs

-- Compute parenthesis balance for multi-line inputs
parenBalance :: String -> Int
parenBalance = foldl step 0
  where
    step n '(' = n + 1
    step n ')' = n - 1
    step n _   = n

-- Consume lines until parentheses are balanced (ignoring comments). Used by script loader and tests.
consumeBalancedScript :: [String] -> (String, [String])
consumeBalancedScript [] = ("", [])
consumeBalancedScript (l:ls) =
  let initial = parenBalance (stripComment l)
  in go l initial ls
  where
    balance = parenBalance . stripComment
    go acc b rest =
      case rest of
        _ | b <= 0 -> (acc, rest)
        [] -> (acc, [])
        (ln:lns) ->
          let acc' = acc ++ "\n" ++ ln
              b' = b + balance ln
          in go acc' b' lns
