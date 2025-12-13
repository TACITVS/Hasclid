import Parser
import Expr
import qualified Data.Map.Strict as M
main = do
  let macros = M.fromList [("inc",(["x"],List [Atom "+", Atom "x", Atom "1"]))]
  print (parseFormulaWithMacros macros "(= (inc 1) 2)")
