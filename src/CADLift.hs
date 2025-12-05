module CADLift
  ( cadDecompose
  , cadDecomposeTree
  , CADCell(..)
  , CellType(..)
  , EvaluationTree(..)
  , SampleTree(..)
  , SignAssignment
  , Sign(..)
  , evaluateInequalityCAD
  , evaluateFormula
  , satisfyingCells
  , proveWithCAD
  , proveFormulaCAD
  , satisfiableFormulaCAD
  , solveQuantifiedFormulaCAD
  , formatCADCells
  ) where

import Expr
import CAD (completeProjection)
import Sturm (isolateRoots)
import qualified Data.Map.Strict as M
import Data.List (nub, sort, sortBy, partition)
import Data.Ord (comparing)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Set as S

-- =============================================
-- CAD Data Structures
-- =============================================

-- | A CAD cell represents a region in space
data CADCell = CADCell
  { cellDimension :: Int                    -- Dimension (1D, 2D, 3D...)
  , samplePoint :: M.Map String Rational    -- Representative sample point
  , cellType :: CellType                    -- Section or Sector ✅ NEW!
  , vanishingPolys :: [Poly]                -- Polynomials that are zero in this cell ✅ NEW!
  , cellDescription :: String                -- Human-readable description
  } deriving (Show, Eq)

-- | Tree structure for CAD decomposition (Hierarchical)
data EvaluationTree
  = EvalLeaf CADCell SignAssignment      -- Fully lifted cell (Leaf)
  | EvalNode CADCell [EvaluationTree]    -- Intermediate cell and its children
  deriving (Show, Eq)

-- | Cell type classification (CRITICAL for proper CAD)
data CellType
  = Sector                              -- Open region (full dimension n)
  | Section [Poly]                      -- Manifold where polynomials vanish (dimension n-1)
  deriving (Show, Eq)

-- | Sample tree for CAD lifting (Legacy)
data SampleTree
  = Leaf CADCell                             -- Base case: 1D cell
  | Node
      { baseValue :: Rational                -- Value in lower dimension
      , variable :: String                   -- Variable being lifted
      , children :: [SampleTree]             -- Cells above this value
      }
  deriving (Show, Eq)

-- | Sign assignment: polynomial -> sign at sample point
type SignAssignment = M.Map Poly Sign

data Sign = Negative | Zero | Positive deriving (Show, Eq, Ord)

-- =============================================
-- Main CAD Algorithm
-- =============================================

-- | Complete CAD decomposition for a set of polynomials
-- Returns: List of cells with sign assignments
cadDecompose :: [Poly] -> [String] -> [(CADCell, SignAssignment)]
cadDecompose polys vars =
  case vars of
    [] -> []  -- No variables
    [v] -> cad1D polys v  -- Base case: univariate
    (v:vs) ->  -- Recursive case: multivariate
      let
          -- Phase 1: Project to lower dimension
          projectedPolys = projectPolynomials polys v

          -- Phase 2: Recursively decompose lower dimension
          lowerCells = cadDecompose projectedPolys vs

          -- Phase 3: Lift to current dimension
          liftedCells = concatMap (liftCell polys v) lowerCells
      in
          liftedCells

-- | Hierarchical CAD decomposition (Tree structure)
--   Vars should be ordered [Top, ..., Bottom] (e.g. [y, x])
cadDecomposeTree :: [Poly] -> [String] -> [EvaluationTree]
cadDecomposeTree polys vars =
  case vars of
    [] -> []
    [v] -> -- Base case: Bottom variable
      let cells = cad1D polys v
      in [ EvalLeaf c s | (c,s) <- cells ]
    (v:vs) -> -- Recursive case
      let projected = projectPolynomials polys v
          lowerTrees = cadDecomposeTree projected vs
          
          -- Lift a tree node by extending its leaves
          liftTree :: EvaluationTree -> EvaluationTree
          liftTree (EvalLeaf lowerCell lowerSigns) =
              let lifted = liftCell polys v (lowerCell, lowerSigns)
                  children = [ EvalLeaf c s | (c,s) <- lifted ]
              in EvalNode lowerCell children
          liftTree (EvalNode lowerCell children) =
              let newChildren = map liftTree children
              in EvalNode lowerCell newChildren

      in map liftTree lowerTrees

-- =============================================
-- Phase 1: Projection (COMPLETE - Collins 1975)
-- =============================================

-- | Project polynomials to lower dimension using Collins' COMPLETE projection.
projectPolynomials :: [Poly] -> String -> [Poly]
projectPolynomials polys var = completeProjection polys var

polyDegreeIn :: Poly -> String -> Int
polyDegreeIn (Poly m) var =
  maximum (0 : [ fromIntegral (M.findWithDefault 0 var vars) | (Monomial vars, _) <- M.toList m ])

-- =============================================
-- Phase 2: Lifting (The Heart of CAD)
-- =============================================

-- | Lift a cell from (n-1)-D to n-D
liftCell :: [Poly] -> String -> (CADCell, SignAssignment) -> [(CADCell, SignAssignment)]
liftCell polys var (lowerCell, lowerSigns) =
  let
      -- Substitute lower cell's sample point into polynomials
      substituted = [ evaluatePolyRational(samplePoint lowerCell) p | p <- polys ]

      -- Find critical values (roots) in current variable.
      -- IMPORTANT: Track WHICH polynomial generated the root!
      -- This is crucial for Section cells when roots are irrational.
      rootsWithSource :: [(Rational, Poly)]
      rootsWithSource = concat
        [ [ (r, p) | r <- findRootsIn var subP ]
        | (p, subP) <- zip polys substituted ]

      -- Group by root value to combine multiple polynomials vanishing at same root
      groupedRoots :: [(Rational, [Poly])]
      groupedRoots =
         let sorted = sortBy (comparing fst) rootsWithSource
             grouped = groupRoots sorted
         in grouped

      -- Sort values
      sortedValues = map fst groupedRoots

      -- Generate samples: SECTIONS (exact roots) and SECTORS (between)
      -- (sections, sectors)
      (sectionVals, sectorVals) = generateSamplesClassified sortedValues

      -- Create SECTION cells (pass the generator polynomials!)
      sectionCells =
        [ createLiftedCell lowerCell var val polys (Just generators)
        | (val, generators) <- groupedRoots ]

      -- Create SECTOR cells (no generators)
      sectorCells =
        [ createLiftedCell lowerCell var val polys Nothing
        | val <- sectorVals ]
  in
      -- Return sections AND sectors, interleaved properly
      interleave sectorCells sectionCells

-- Helper to group roots by value (with small tolerance or exact equality)
groupRoots :: [(Rational, Poly)] -> [(Rational, [Poly])]
groupRoots [] = []
groupRoots ((r,p):rest) =
  let (matches, others) = span (\(r',_) -> r == r') rest -- Exact match for now
      polys = p : map snd matches
  in (r, polys) : groupRoots others

-- Helper: Interleave two lists (sector, section, sector, section, ...)
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Create a lifted cell with sign assignment
--   generators: Just [p] if this is a SECTION generated by roots of p
--              Nothing if this is a SECTOR
createLiftedCell :: CADCell -> String -> Rational -> [Poly] -> Maybe [Poly] -> (CADCell, SignAssignment)
createLiftedCell lowerCell var value polys generators =
  let
      -- Extend sample point with new variable
      newSample = M.insert var value (samplePoint lowerCell)

      -- Compute sign assignment
      -- If we have generators, FORCE them to be Zero
      forcedZero = case generators of
                     Nothing -> S.empty
                     Just gens -> S.fromList gens

      signs = M.fromList
        [ (p, if S.member p forcedZero then Zero else determineSign p newSample)
        | p <- polys ]

      -- Determine which polynomials vanish
      vanishing = [ p | (p, s) <- M.toList signs, s == Zero ]

      -- Classify cell type
      cType = case generators of
                Just _ -> Section vanishing
                Nothing -> Sector

      -- Create description
      typeStr = case cType of
                  Sector -> "SECTOR"
                  Section ps -> "SECTION(" ++ show (length ps) ++ " polys vanish)"

      -- Create new cell
      newCell = CADCell
        { cellDimension = cellDimension lowerCell + 1
        , samplePoint = newSample
        , cellType = cType
        , vanishingPolys = vanishing
        , cellDescription = cellDescription lowerCell ++ ", " ++ var ++ "=" ++ show value ++ " [" ++ typeStr ++ "]"
        }
  in
      (newCell, signs)

-- | Find roots of a polynomial in a specific variable
findRootsIn :: String -> Poly -> [Rational]
findRootsIn var p =
  case toUnivariate p of
    Just (v, coeffs) | v == var ->
      let intervals = isolateRoots coeffs
          -- Refine each interval to get exact or very close root
          refinedRoots = map (refineToExactRoot coeffs) intervals
      in refinedRoots
    _ -> []  -- Not univariate in this variable

-- | Refine interval to find exact rational root
refineToExactRoot :: [Rational] -> (Rational, Rational) -> Rational
refineToExactRoot coeffs (lo, hi) =
  -- Try integer values WITHIN the interval (not outside!)
  let loInt = ceiling lo  -- First integer >= lo
      hiInt = floor hi    -- Last integer <= hi
      integerCandidates = [loInt .. hiInt]
      exactRoot = filter (\x -> isRoot coeffs (fromIntegral x)) integerCandidates
  in case exactRoot of
       (r:_) -> fromIntegral r  -- Found exact integer root!
       [] -> refineRootNumerically coeffs (lo, hi) -- Fall back to numerical refinement

-- | Numerically refine a root interval
refineRootNumerically :: [Rational] -> (Rational, Rational) -> Rational
refineRootNumerically coeffs (lo, hi)
  | hi - lo < 1/100000 = (lo + hi) / 2  -- Close enough
  | otherwise =
      let mid = (lo + hi) / 2
          valMid = evalPolyAt coeffs mid
      in if abs valMid < 1/1000000000
         then mid  -- Found it!
         else
           let valLo = evalPolyAt coeffs lo
           in if valLo * valMid < 0
              then refineRootNumerically coeffs (lo, mid)
              else refineRootNumerically coeffs (mid, hi)

-- | Test if a value is a root of the polynomial
isRoot :: [Rational] -> Rational -> Bool
isRoot coeffs x = abs (evalPolyAt coeffs x) < 1/1000000000

-- | Evaluate polynomial at a point (for refinement)
evalPolyAt :: [Rational] -> Rational -> Rational
evalPolyAt coeffs x = sum [ c * (x ^ i) | (i, c) <- zip [0..] coeffs ]

-- | Generate samples classified as sections (on roots) and sectors (between roots)
generateSamplesClassified :: [Rational] -> ([Rational], [Rational])
generateSamplesClassified [] = ([], [0])  -- No roots: just one sector
generateSamplesClassified sorted@(first:_) =
  let
      sections = sorted
      beforeFirst = first - 1
      between = [ (a + b) / 2 | (a, b) <- zip sorted (drop 1 sorted) ]
      afterLast = case reverse sorted of (z:_) -> z + 1; _ -> first + 1
      sectors = beforeFirst : between ++ [afterLast]
  in (sections, sectors)



-- | Determine sign of polynomial at a point
determineSign :: Poly -> M.Map String Rational -> Sign
determineSign p assignment =
  let evaluated = evaluatePolyRational assignment p
  in case polyToRational evaluated of
       Just r | r > 0 -> Positive
              | r < 0 -> Negative
              | otherwise -> Zero
       Nothing -> Zero  -- Default if can't evaluate

-- =============================================
-- Base Case: 1D CAD (WITH CLASSIFICATION)
-- =============================================

cad1D :: [Poly] -> String -> [(CADCell, SignAssignment)]
cad1D polys var =
  let
      -- Find all roots with source polynomials
      rootsWithSource :: [(Rational, Poly)]
      rootsWithSource = concat
        [ [ (r, p) | r <- findRootsIn var p ]
        | p <- polys ]

      groupedRoots = groupRoots (sortBy (comparing fst) rootsWithSource)
      sortedRoots = map fst groupedRoots

      (sectionVals, sectorVals) = generateSamplesClassified sortedRoots

      sectionCells =
        [ create1DCell var val polys (Just gens)
        | (val, gens) <- groupedRoots ]

      sectorCells =
        [ create1DCell var val polys Nothing
        | val <- sectorVals ]
  in
      interleave sectorCells sectionCells

create1DCell :: String -> Rational -> [Poly] -> Maybe [Poly] -> (CADCell, SignAssignment)
create1DCell var value polys generators =
  let
      sample = M.singleton var value
      
      forcedZero = case generators of
                     Nothing -> S.empty
                     Just gens -> S.fromList gens

      signs = M.fromList
        [ (p, if S.member p forcedZero then Zero else determineSign p sample)
        | p <- polys ]

      vanishing = [ p | (p, s) <- M.toList signs, s == Zero ]

      cType = case generators of
                Just _ -> Section vanishing
                Nothing -> Sector

      typeStr = case cType of
                  Sector -> "SECTOR"
                  Section ps -> "SECTION(" ++ show (length ps) ++ " polys vanish)"

      cell = CADCell
        { cellDimension = 1
        , samplePoint = sample
        , cellType = cType
        , vanishingPolys = vanishing
        , cellDescription = var ++ "=" ++ show value ++ " [" ++ typeStr ++ "]"
        }
  in
      (cell, signs)

-- =============================================
-- Inequality Evaluation Using CAD
-- =============================================

-- | Evaluate an inequality using CAD
evaluateInequalityCAD :: [Poly] -> Poly -> [String] -> Bool
evaluateInequalityCAD constraints inequality vars =
  let
      -- Decompose with both constraints and inequality
      allPolys = inequality : constraints

      cells = cadDecompose allPolys vars

      -- Check if inequality holds in all cells satisfying constraints
      validCells = filter (cellSatisfiesConstraints constraints) cells

      inequalityHolds = all (cellSatisfiesInequality inequality) validCells
  in
      inequalityHolds

cellSatisfiesConstraints :: [Poly] -> (CADCell, SignAssignment) -> Bool
cellSatisfiesConstraints constraints (cell, signs) =
  all (\p -> M.lookup p signs == Just Zero) constraints

cellSatisfiesInequality :: Poly -> (CADCell, SignAssignment) -> Bool
cellSatisfiesInequality ineq (cell, signs) =
  M.lookup ineq signs == Just Positive || M.lookup ineq signs == Just Zero

-- =============================================
-- Formula Evaluation Over CAD (CRITICAL!)
-- =============================================

-- | Evaluate an arbitrary formula over a CAD cell.
evaluateFormula :: Formula -> (CADCell, SignAssignment) -> Bool
evaluateFormula formula (cell, signs) =
  case formula of
    -- Equality: f = g  ⟺  f - g = 0
    Eq lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in M.lookup diff signs == Just Zero

    -- Greater or equal: f >= g  ⟺  f - g >= 0
    Ge lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in case M.lookup diff signs of
           Just Positive -> True
           Just Zero -> True
           _ -> False

    -- Greater than: f > g  ⟺  f - g > 0
    Gt lhs rhs ->
      let diff = exprToPoly (Sub lhs rhs)
      in M.lookup diff signs == Just Positive

    And f1 f2 -> evaluateFormula f1 (cell, signs) && evaluateFormula f2 (cell, signs)
    Or f1 f2 -> evaluateFormula f1 (cell, signs) || evaluateFormula f2 (cell, signs)
    Not f -> not (evaluateFormula f (cell, signs))

    Forall _ _ -> error "Nested quantification evaluation not supported at cell level"
    Exists _ _ -> error "Nested quantification evaluation not supported at cell level"

-- | Find all cells that satisfy a given formula.
satisfyingCells :: Formula -> [(CADCell, SignAssignment)] -> [(CADCell, SignAssignment)]
satisfyingCells formula cells =
  filter (evaluateFormula formula) cells

-- | Prove or disprove a formula using CAD.
proveWithCAD :: Formula -> [String] -> Bool
proveWithCAD formula vars =
  let
      -- Extract polynomials from formula
      polys = formulaToPolys formula

      -- Decompose space
      cells = cadDecompose polys vars

      -- Check if formula holds in ALL cells
  in all (\cell -> evaluateFormula formula cell) cells

-- | CAD proof for a goal with supporting theory.
proveFormulaCAD :: Theory -> Formula -> Bool
proveFormulaCAD theory goal =
  let polys = concatMap formulaToPolys (goal : theory)
      vars = S.toList (extractPolyVarsList polys)
      cells = cadDecompose polys vars
      validCells = filter (\c -> all (`evaluateFormula` c) theory) cells
  in not (null validCells) && all (evaluateFormula goal) validCells

-- | Check satisfiability of a goal with supporting theory using CAD.
satisfiableFormulaCAD :: Theory -> Formula -> Bool
satisfiableFormulaCAD theory goal =
  let polys = concatMap formulaToPolys (goal : theory)
      vars = S.toList (extractPolyVarsList polys)
      cells = cadDecompose polys vars
      validCells = filter (\c -> all (`evaluateFormula` c) theory) cells
  in any (evaluateFormula goal) validCells

-- | Extract all polynomials from a formula
formulaToPolys :: Formula -> [Poly]
formulaToPolys formula =
  case formula of
    Eq lhs rhs -> [exprToPoly (Sub lhs rhs)]
    Ge lhs rhs -> [exprToPoly (Sub lhs rhs)]
    Gt lhs rhs -> [exprToPoly (Sub lhs rhs)]
    And f1 f2 -> formulaToPolys f1 ++ formulaToPolys f2
    Or f1 f2 -> formulaToPolys f1 ++ formulaToPolys f2
    Not f -> formulaToPolys f
    Forall _ f -> formulaToPolys f
    Exists _ f -> formulaToPolys f

-- | Convert an expression to a polynomial
exprToPoly :: Expr -> Poly
exprToPoly expr =
  case expr of
    Const r -> polyFromConst r
    Var v -> polyFromVar v
    Add e1 e2 -> polyAdd (exprToPoly e1) (exprToPoly e2)
    Sub e1 e2 -> polySub (exprToPoly e1) (exprToPoly e2)
    Mul e1 e2 -> polyMul (exprToPoly e1) (exprToPoly e2)
    Pow e n -> polyPow (exprToPoly e) (fromIntegral n)
    _ -> polyZero  -- Unsupported (geometric primitives, etc.)

-- Extract variables from a polynomial
extractPolyVars :: Poly -> S.Set String
extractPolyVars (Poly m) =
  S.fromList $ concatMap (\(Monomial vars) -> M.keys vars) (M.keys m)

-- Extract variables from a polynomial list
extractPolyVarsList :: [Poly] -> S.Set String
extractPolyVarsList = foldr (S.union . extractPolyVars) S.empty

-- =============================================
-- Helpers
-- =============================================

polyToRational :: Poly -> Maybe Rational
polyToRational (Poly m)
  | M.null m = Just 0
  | M.size m == 1 =
      case M.toList m of
        [(Monomial vm, c)] | M.null vm -> Just c
        _ -> Nothing
  | otherwise = Nothing

-- | Evaluate polynomial at a point (PARTIAL EVALUATION)
evaluatePolyRational :: M.Map String Rational -> Poly -> Poly
evaluatePolyRational assignment (Poly m) =
  let
      -- Partial evaluation: substitute only assigned variables
      evalMonomial (Monomial vars) coeff =
        let varList = M.toList vars
            (assignedVars, unassignedVars) =
              partition (\(v, _) -> M.member v assignment) varList
            assignedValue = product [ (assignment M.! v) ^ exp | (v, exp) <- assignedVars ]
            newCoeff = coeff * assignedValue
            newVars = M.fromList unassignedVars
        in
            (Monomial newVars, newCoeff)
      evaluated = [ evalMonomial mono coeff | (mono, coeff) <- M.toList m ]
  in
      Poly (M.fromListWith (+) evaluated)

-- =============================================
-- Pretty Printing
-- =============================================

formatCADCells :: [(CADCell, SignAssignment)] -> String
formatCADCells cells =
  unlines [ formatCell i cell signs | (i, (cell, signs)) <- zip [1..] cells ]
  where
    formatCell i cell signs =
      let typeInfo = case cellType cell of
                       Sector -> "SECTOR (full dimension)"
                       Section ps -> "SECTION (manifold, " ++ show (length ps) ++ " polys vanish)"
          sampleInfo = "Sample: " ++ show (samplePoint cell)
          signInfo = if null (M.toList signs)
                     then "  Signs: (none)"
                     else "  Signs: " ++ formatSigns signs
      in unlines
           [ "Cell " ++ show i ++ ": [" ++ typeInfo ++ "]"
           , "  " ++ sampleInfo
           , signInfo
           ]

    formatSigns signs =
      let signList = M.toList signs
          formatOne (p, s) = show s
      in unwords (map formatOne (take 3 signList)) ++
         if length signList > 3 then " ..." else ""

-- =============================================
-- Quantified Formula Solver (Mixed Quantifiers)
-- =============================================

-- | Solve a fully quantified formula using CAD
--   Handles alternation: Forall x Exists y ...
solveQuantifiedFormulaCAD :: Theory -> Formula -> Bool
solveQuantifiedFormulaCAD theory goal =
    let
        -- 1. Collect all polynomials from theory and goal
        polys = concatMap formulaToPolys (goal : theory)
        
        -- 2. Identify All Variables from polynomials
        allVars = extractPolyVarsList polys
        
        -- 3. Identify Quantified Variables (Ordered)
        quantifiedVarsList = getQuantifierOrder goal
        quantifiedVarsNames = map qvName quantifiedVarsList
        quantifiedVarsSet = S.fromList quantifiedVarsNames
        
        -- 4. Identify Free Variables (Implicit Forall)
        --    (vars in polynomials but not bound in goal)
        freeVarsSet = S.difference allVars quantifiedVarsSet
        freeVars = S.toList freeVarsSet
        
        -- 5. Total Variable Order for CAD: [Innermost, ..., Outermost, FreeN, ..., Free1]
        cadVars = reverse quantifiedVarsNames ++ reverse freeVars
        
        -- 6. Build Decomposition Tree
        forest = cadDecomposeTree polys cadVars
    in
        -- 7. Check Truth
        checkImplicitForall freeVars theory goal forest

-- | Traverse Implicit Forall (Free Variables)
checkImplicitForall :: [String] -> Theory -> Formula -> [EvaluationTree] -> Bool
checkImplicitForall [] theory goal forest =
    checkExplicitQuantifiers theory goal forest
checkImplicitForall (v:vs) theory goal forest =
    -- Forall v: All branches must satisfy
    all (checkImplicitForallNode vs theory goal) forest

checkImplicitForallNode :: [String] -> Theory -> Formula -> EvaluationTree -> Bool
checkImplicitForallNode vs theory goal (EvalNode _ children) =
    checkImplicitForall vs theory goal children
checkImplicitForallNode _ _ _ (EvalLeaf _ _) =
    error "CAD tree depth mismatch: Reached leaf while processing implicit free variables"

-- | Traverse Explicit Quantifiers
checkExplicitQuantifiers :: Theory -> Formula -> [EvaluationTree] -> Bool
checkExplicitQuantifiers theory (Forall qs inner) forest =
    checkQuantifierLayer qs True inner theory forest
checkExplicitQuantifiers theory (Exists qs inner) forest =
    checkQuantifierLayer qs False inner theory forest
checkExplicitQuantifiers theory goal forest =
    -- End of quantifiers: Evaluate at leaves
    all (evaluateAtNode theory goal) forest

-- | Handle a layer of quantifiers
checkQuantifierLayer :: [QuantVar] -> Bool -> Formula -> Theory -> [EvaluationTree] -> Bool
checkQuantifierLayer [] _ inner theory forest =
    checkExplicitQuantifiers theory inner forest
checkQuantifierLayer (q:qs) isUniversal inner theory forest =
    let checkNext = if isUniversal then all else any
    in checkNext (checkQuantifierNode (q:qs) isUniversal inner theory) forest

checkQuantifierNode :: [QuantVar] -> Bool -> Formula -> Theory -> EvaluationTree -> Bool
checkQuantifierNode (q:qs) isUniv inner theory node =
    if checkBounds q node
    then
        case node of
            EvalNode _ children ->
                checkQuantifierLayer qs isUniv inner theory children
            EvalLeaf cell signs ->
                if null qs
                then checkExplicitQuantifiers theory inner [EvalLeaf cell signs]
                else error ("CAD tree depth mismatch: Reached leaf but quantifiers remain: " ++ show (map qvName qs))
    else
        isUniv -- Vacuously true/false if bounds not met
checkQuantifierNode [] _ _ _ _ = error "Empty variable list in checkQuantifierNode"

-- | Check bounds for a quantifier variable against the cell
checkBounds :: QuantVar -> EvaluationTree -> Bool
checkBounds q tree =
    let pt = case tree of EvalNode c _ -> samplePoint c; EvalLeaf c _ -> samplePoint c
        valQ = M.lookup (qvName q) pt
        eval e = polyToRational (evaluatePolyRational pt (exprToPoly e))
    in case valQ of
         Nothing -> True -- Should not happen if tree is correct
         Just val ->
             let lowerOk = case qvLower q of Nothing -> True; Just e -> maybe True (<= val) (eval e)
                 upperOk = case qvUpper q of Nothing -> True; Just e -> maybe True (>= val) (eval e)
             in lowerOk && upperOk

-- | Evaluate formula at a specific node (Leaf or Node)
evaluateAtNode :: Theory -> Formula -> EvaluationTree -> Bool
evaluateAtNode theory goal (EvalLeaf cell signs) =
    -- Base case: Evaluate at sample point
    if all (`evaluateFormula` (cell, signs)) theory
    then evaluateFormula goal (cell, signs)
    else True -- Vacuously true if theory violated (for Universal assumption)

evaluateAtNode theory goal (EvalNode _ children) =
    -- Extra variables (Auxiliary) - treat as Universal
    all (evaluateAtNode theory goal) children

-- =============================================
-- Variable Extraction Helpers
-- =============================================

getQuantifierOrder :: Formula -> [QuantVar]
getQuantifierOrder (Forall qs f) = qs ++ getQuantifierOrder f
getQuantifierOrder (Exists qs f) = qs ++ getQuantifierOrder f
getQuantifierOrder (And f1 f2) = getQuantifierOrder f1 ++ getQuantifierOrder f2
getQuantifierOrder (Or f1 f2) = getQuantifierOrder f1 ++ getQuantifierOrder f2
getQuantifierOrder (Not f) = getQuantifierOrder f
getQuantifierOrder _ = []
