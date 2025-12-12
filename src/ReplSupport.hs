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
  let balance = parenBalance . stripComment
      initial = let b = balance l
                in if b == 0 && (isCmd l)
                     then 1 else b
  in go l initial ls
  where
    balance = parenBalance . stripComment
    isCmd ln =
      let t = dropWhile (== ' ') ln
      in (":prove" `isPrefixOf` t) || (":auto" `isPrefixOf` t)
    go acc b rest =
      case rest of
        _ | b <= 0 -> (acc, rest)
        [] -> (acc, [])
        (ln:lns) ->
          let acc' = acc ++ "\n" ++ ln
              b' = b + balance ln
          in go acc' b' lns
