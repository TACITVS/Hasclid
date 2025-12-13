module Main where

import System.CPUTime
import Text.Printf
import qualified Data.Map.Strict as M

import Expr
import Parser (parseFormulaWithMacros)
import Geometry.WLOG (applyWLOG)
import Prover (buildSubMap, toPolySub)
import Wu (reduceWithWu)
import Positivity.SOS (checkSOS)
import TermOrder (compareMonomials, TermOrder(..))

lemmaInput :: [String]
lemmaInput = 
  [ "(= zA 0)", "(= zB 0)", "(= zC 0)", "(= zP 0)"
  , "(= a2 (dist2 B C))"
  , "(= b2 (dist2 A C))"
  , "(= c2 (dist2 A B))"
  , "(= RA2 (dist2 P A))"
  , "(= TwAreaPAC (+ (* xP (- yA yC)) (+ (* xA (- yC yP)) (* xC (- yP yA)))))"
  , "(= TwAreaPAB (+ (* xP (- yA yB)) (+ (* xA (- yB yP)) (* xB (- yP yA)))))"
  ]

lemmaGoal :: String
lemmaGoal = "(>= (* a2 (* b2 (* c2 RA2))) (^ (+ (* c2 TwAreaPAC) (* b2 TwAreaPAB)) 2))"

main :: IO ()
main = do
  putStrLn "=== Dumping Erdos-Mordell Polynomial ==="
  
  let Right goalF = parseFormulaWithMacros M.empty lemmaGoal
  let theoryF = map (\s -> let Right f = parseFormulaWithMacros M.empty s in f) lemmaInput
  
  let (thWLOG, _) = applyWLOG theoryF goalF
  let subMap = buildSubMap thWLOG
  let (Ge lhs rhs) = goalF
  let targetExpr = Sub lhs rhs
  let targetPoly = toPolySub subMap targetExpr
  
  -- Wu Reduction
  let isDefinition (Eq (Var _) _) = True
      isDefinition _ = False
  let constraints = [ eq | eq <- thWLOG, not (isDefinition eq) ]
  let reduced = if null constraints 
                then targetPoly 
                else head (reduceWithWu (map (toPolySub subMap . (\(Eq l r) -> Sub l r)) constraints) targetPoly)
  
  putStrLn "Reduced Polynomial:"
  putStrLn (prettyPolyNice reduced)
