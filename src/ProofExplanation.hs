{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : ProofExplanation
-- Description : Human-readable step-by-step proof explanations
-- Copyright   : (c) 2024-2025
-- License     : MIT
-- Stability   : experimental
--
-- This module provides human-readable, mathematician-style proof explanations
-- with selectable output formats (LaTeX inline math or ASCII) and configurable
-- detail levels. Supports both REPL display and file export.
--
-- = Usage Example
--
-- @
-- -- Configure explanation settings
-- let config = ExplanationConfig LaTeX Normal
--
-- -- Generate explanation from a proof result
-- let explanation = explainGroebnerProof config proofResult
--
-- -- Format for display
-- putStrLn (formatExplanation config explanation)
--
-- -- Export to file
-- exportExplanation "proof.tex" config explanation
-- @

module ProofExplanation
  ( -- * Configuration Types
    ExplanationFormat(..)
  , DetailLevel(..)
  , ExplanationConfig(..)
  , defaultConfig

    -- * Explanation Types
  , ExplanationStep(..)
  , ProofExplanation(..)

    -- * Explanation Generation
  , explainGroebnerProof
  , explainWuProof
  , explainSOSProof
  , explainCADProof
  , explainAutoSolve

    -- * Formatting
  , formatExplanation
  , formatStep

    -- * File Export
  , exportExplanation
  , exportToLaTeX
  , exportToText
  ) where

import Core.Types
import qualified Prover as P
import qualified Wu as W
import qualified Positivity.SOSTypes as SOS
import qualified SolverRouter as SR
import Data.List (intercalate)
import Data.Ratio (numerator, denominator)

-- =============================================
-- Configuration Types
-- =============================================

-- | Output format for proof explanations.
data ExplanationFormat
  = ASCII   -- ^ Plain ASCII text with Unicode math symbols
  | LaTeX   -- ^ LaTeX inline math notation ($...$)
  deriving (Eq, Show, Read)

-- | Level of detail in proof explanations.
data DetailLevel
  = Minimal -- ^ Equations only, no prose
  | Brief   -- ^ Just the key steps and conclusion
  | Normal  -- ^ Standard level of detail
  | Verbose -- ^ Full detail with all intermediate computations
  deriving (Eq, Show, Read, Ord)

-- | Configuration for proof explanation generation.
data ExplanationConfig = ExplanationConfig
  { format :: ExplanationFormat  -- ^ Output format
  , detailLevel :: DetailLevel   -- ^ How much detail to show
  } deriving (Eq, Show)

-- | Default configuration: ASCII with Normal detail.
defaultConfig :: ExplanationConfig
defaultConfig = ExplanationConfig ASCII Normal

-- =============================================
-- Explanation Types
-- =============================================

-- | A single step in a proof explanation.
data ExplanationStep = ExplanationStep
  { stepNumber :: Int           -- ^ Step number (1-indexed)
  , stepTitle :: String         -- ^ Short title for the step
  , stepContent :: String       -- ^ Detailed explanation (format-specific)
  , stepMath :: Maybe String    -- ^ Optional mathematical expression
  } deriving (Eq, Show)

-- | Complete proof explanation structure.
data ProofExplanation = ProofExplanation
  { proofTitle :: String              -- ^ Title of what's being proved
  , proofMethod :: String             -- ^ Method used (Groebner, Wu, SOS, CAD)
  , proofGoal :: String               -- ^ The goal being proved
  , proofSteps :: [ExplanationStep]   -- ^ Individual proof steps
  , proofConclusion :: String         -- ^ Final conclusion
  , proofSuccess :: Bool              -- ^ Whether the proof succeeded
  } deriving (Eq, Show)

-- =============================================
-- Explanation Generation - Groebner Basis
-- =============================================

-- | Generate explanation for a Groebner basis proof.
explainGroebnerProof :: ExplanationConfig -> P.ProofResult -> ProofExplanation
explainGroebnerProof config result =
  let goalStr = "Goal from trace"
      steps = generateGroebnerSteps config (P.trace result)
      conclusion = if P.proved result
                   then conclusionText config "The theorem is proved."
                   else "The proof did not succeed: " ++ P.reason result
  in ProofExplanation
       { proofTitle = "Algebraic Proof via Groebner Basis"
       , proofMethod = "Groebner"
       , proofGoal = goalStr
       , proofSteps = steps
       , proofConclusion = conclusion
       , proofSuccess = P.proved result
       }

generateGroebnerSteps :: ExplanationConfig -> P.ProofTrace -> [ExplanationStep]
generateGroebnerSteps config trace =
  let level = detailLevel config
      assumptions = P.usedAssumptions trace
      bSize = P.basisSize trace
      stepList = P.steps trace

      -- Step 1: Setup
      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Setting up the proof"
        , stepContent = case level of
            Brief -> "Converting hypotheses to polynomials."
            _ -> "We convert the geometric hypotheses into polynomial equations. " ++
                 "This allows us to use algebraic techniques to verify the theorem."
        , stepMath = if level >= Normal && not (null assumptions)
                     then Just $ formatAssumptions config assumptions
                     else Nothing
        }

      -- Step 2: Groebner basis computation
      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Computing Groebner basis"
        , stepContent = case level of
            Brief -> "Computed basis with " ++ show bSize ++ " polynomials."
            Normal -> "From the hypothesis polynomials, we compute a Groebner basis using Buchberger's algorithm. " ++
                      "The resulting basis has " ++ show bSize ++ " polynomials."
            Verbose -> "Using Buchberger's algorithm with the lexicographic term ordering, " ++
                       "we compute a Groebner basis for the ideal generated by the hypothesis polynomials. " ++
                       "S-polynomial reduction eliminates redundant generators, yielding a basis of " ++
                       show bSize ++ " polynomials."
        , stepMath = Nothing
        }

      -- Step 3: Reduction
      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Reducing the goal"
        , stepContent = case level of
            Brief -> "Goal reduces to zero."
            Normal -> "We reduce the goal polynomial modulo the Groebner basis. " ++
                      "If it reduces to zero, the goal follows from the hypotheses."
            Verbose -> "Polynomial division with remainder is performed, dividing the goal polynomial " ++
                       "by each element of the Groebner basis in turn. By the properties of Groebner bases, " ++
                       "if the remainder is zero, then the goal polynomial lies in the ideal generated by the hypotheses."
        , stepMath = formatReductionSteps config stepList
        }

  in case level of
       Brief -> [step1, step3]
       _     -> [step1, step2, step3]

-- =============================================
-- Explanation Generation - Wu's Method
-- =============================================

-- | Generate explanation for Wu's method proof.
explainWuProof :: ExplanationConfig -> W.WuTrace -> ProofExplanation
explainWuProof config trace =
  let goalStr = formatFormula config (W.inputGoal trace)
      steps = generateWuSteps config trace
      conclusion = if W.isProved trace
                   then conclusionText config "The theorem is proved by Wu's method."
                   else "The proof did not succeed: " ++ W.proofReason trace
  in ProofExplanation
       { proofTitle = "Geometric Proof via Wu's Method"
       , proofMethod = "Wu"
       , proofGoal = goalStr
       , proofSteps = steps
       , proofConclusion = conclusion
       , proofSuccess = W.isProved trace
       }

generateWuSteps :: ExplanationConfig -> W.WuTrace -> [ExplanationStep]
generateWuSteps config trace =
  let level = detailLevel config
      hypPolys = W.hypothesisPolys trace
      concPoly = W.conclusionPoly trace
      branches = W.branches trace

      -- Step 1: Problem setup
      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Problem formulation"
        , stepContent = case level of
            Brief -> "Converting to polynomial form."
            _ -> "We express the geometric theorem as polynomial equations. " ++
                 "The hypotheses give us " ++ show (length hypPolys) ++ " polynomial constraints."
        , stepMath = if level == Verbose
                     then Just $ "Hypothesis polynomials: " ++ show (length hypPolys)
                     else Nothing
        }

      -- Step 2: Characteristic set
      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Computing characteristic set"
        , stepContent = case level of
            Brief -> "Built characteristic set."
            Normal -> "We compute a characteristic set of the hypothesis polynomials using " ++
                      "successive pseudo-division. This triangulates the polynomial system."
            Verbose -> "Wu's method triangulates the polynomial system by computing a characteristic set. " ++
                       "Starting with the highest-ranked polynomial, we perform successive pseudo-divisions " ++
                       "to eliminate variables, creating a triangular system where each polynomial " ++
                       "introduces exactly one new variable."
        , stepMath = Nothing
        }

      -- Step 3: Pseudo-division
      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Pseudo-dividing the conclusion"
        , stepContent = case level of
            Brief -> "Remainder is zero."
            Normal -> "We pseudo-divide the conclusion polynomial by the characteristic set. " ++
                      "A zero remainder proves the theorem under non-degeneracy conditions."
            Verbose -> "Pseudo-division avoids fractions by multiplying by powers of leading coefficients. " ++
                       "We successively pseudo-divide the conclusion by each element of the characteristic set, " ++
                       "from lowest to highest rank. The final remainder determines provability."
        , stepMath = if level >= Normal
                     then Just $ "Conclusion: " ++ formatPoly config concPoly
                     else Nothing
        }

      -- Step 4: Branch analysis (for verbose)
      branchSteps = if level == Verbose && length branches > 1
                    then [ExplanationStep
                           { stepNumber = 4
                           , stepTitle = "Branch analysis"
                           , stepContent = "The proof splits into " ++ show (length branches) ++
                                          " branches based on non-degeneracy conditions."
                           , stepMath = Nothing
                           }]
                    else []

  in case level of
       Brief -> [step1, step3]
       Normal -> [step1, step2, step3]
       Verbose -> [step1, step2, step3] ++ branchSteps

-- =============================================
-- Explanation Generation - Sum of Squares
-- =============================================

-- | Generate explanation for an SOS proof.
explainSOSProof :: ExplanationConfig -> SOS.SOSCertificate -> Poly -> ProofExplanation
explainSOSProof config cert goalPoly =
  let goalStr = formatPoly config goalPoly
      steps = generateSOSSteps config cert goalPoly
      conclusion = conclusionText config "The inequality is proved by sum-of-squares decomposition."
  in ProofExplanation
       { proofTitle = "Inequality Proof via Sum of Squares"
       , proofMethod = "SOS"
       , proofGoal = goalStr ++ " >= 0"
       , proofSteps = steps
       , proofConclusion = conclusion
       , proofSuccess = True
       }

generateSOSSteps :: ExplanationConfig -> SOS.SOSCertificate -> Poly -> [ExplanationStep]
generateSOSSteps config cert goalPoly =
  let level = detailLevel config
      terms = SOS.sosTerms cert
      lemmas = SOS.sosLemmas cert
      remainder = SOS.sosRemainder cert
      pattern = SOS.sosPattern cert

      -- Step 1: Problem setup
      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Setting up the inequality"
        , stepContent = case level of
            Brief -> "Proving polynomial is non-negative."
            _ -> "We prove the polynomial inequality by finding a sum-of-squares decomposition. " ++
                 "A polynomial that can be written as a sum of squares is always non-negative."
        , stepMath = Just $ formatPoly config goalPoly
        }

      -- Step 2: Pattern recognition
      step2 = if level >= Normal
              then [ExplanationStep
                     { stepNumber = 2
                     , stepTitle = "Pattern recognition"
                     , stepContent = case pattern of
                         Just SOS.TrivialSquare -> "The polynomial is a perfect square."
                         Just SOS.SumOfSquares -> "The polynomial is a sum of squared terms."
                         Just SOS.TriangleInequality -> "This matches the triangle inequality pattern."
                         Just SOS.AM_GM -> "This matches the AM-GM inequality pattern."
                         Nothing -> "Using general SOS decomposition."
                     , stepMath = Nothing
                     }]
              else []

      -- Step 3: Decomposition
      step3 = ExplanationStep
        { stepNumber = if level >= Normal then 3 else 2
        , stepTitle = "Sum of squares decomposition"
        , stepContent = case level of
            Brief -> "Found SOS decomposition with " ++ show (length terms) ++ " terms."
            _ -> "We decompose the polynomial as a sum of squared terms" ++
                 (if null lemmas then "." else " plus known non-negative quantities.")
        , stepMath = Just $ formatSOSDecomposition config terms lemmas remainder
        }

  in [step1] ++ step2 ++ [step3]

-- =============================================
-- Explanation Generation - CAD
-- =============================================

-- | Generate explanation for a CAD proof.
explainCADProof :: ExplanationConfig -> Bool -> String -> ProofExplanation
explainCADProof config success reason =
  let steps = generateCADSteps config success reason
      conclusion = if success
                   then conclusionText config "The formula holds over all of R^n."
                   else "The formula could not be verified: " ++ reason
  in ProofExplanation
       { proofTitle = "Proof via Cylindrical Algebraic Decomposition"
       , proofMethod = "CAD"
       , proofGoal = "Quantified formula"
       , proofSteps = steps
       , proofConclusion = conclusion
       , proofSuccess = success
       }

generateCADSteps :: ExplanationConfig -> Bool -> String -> [ExplanationStep]
generateCADSteps config success _reason =
  let level = detailLevel config

      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Projection phase"
        , stepContent = case level of
            Brief -> "Computing projection polynomials."
            _ -> "We compute projection polynomials that capture the algebraic structure " ++
                 "of the original formula across all variable dimensions."
        , stepMath = Nothing
        }

      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Cell decomposition"
        , stepContent = case level of
            Brief -> "Decomposing real space into cells."
            _ -> "The real algebraic set is decomposed into finitely many cells, " ++
                 "each with constant sign for all projection polynomials."
        , stepMath = Nothing
        }

      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Cell evaluation"
        , stepContent = if success
                        then case level of
                               Brief -> "All cells satisfy the formula."
                               _ -> "We evaluate the formula on a sample point from each cell. " ++
                                    "Every cell satisfies the formula, proving it holds universally."
                        else "Some cells do not satisfy the formula."
        , stepMath = Nothing
        }

  in case level of
       Brief -> [step1, step3]
       _     -> [step1, step2, step3]

-- =============================================
-- Explanation Generation - AutoSolve
-- =============================================

-- | Generate explanation from an AutoSolveResult.
explainAutoSolve :: ExplanationConfig -> SR.AutoSolveResult -> ProofExplanation
explainAutoSolve config result =
  let solver = SR.selectedSolver result
      goalFormula = SR.preprocessedGoalExpr result
      proved = SR.isProved result
      reason = SR.proofReason result
      trace = SR.detailedTrace result
      evidence = SR.proofEvidence result
  in case solver of
    SR.UseSOS -> explainSOSResult config goalFormula proved reason trace evidence
    SR.UseGroebner -> explainGroebnerResult config goalFormula proved reason trace
    SR.UseWu -> explainWuResult config goalFormula proved reason trace
    SR.UseCAD -> explainCADProof config proved reason
    _ -> explainGenericResult config solver goalFormula proved reason trace

-- | Explain SOS proof with actual decomposition
explainSOSResult :: ExplanationConfig -> Formula -> Bool -> String -> Maybe String -> Maybe SR.ProofEvidence -> ProofExplanation
explainSOSResult config goal proved reason trace evidence =
  let level = detailLevel config
      goalStr = formatFormula config goal

      -- Extract the actual SOS decomposition from evidence or trace
      sosDecomp = case evidence of
        Just (SR.EvidenceSOS cert _) -> formatSOSDecomposition config (SOS.sosTerms cert) (SOS.sosLemmas cert) (SOS.sosRemainder cert)
        Nothing -> extractSOSFromTrace trace

      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Inequality to prove"
        , stepContent = "We need to show the following expression is non-negative:"
        , stepMath = Just goalStr
        }

      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Sum of squares decomposition"
        , stepContent = if proved
                        then "The expression can be written as a sum of squares:"
                        else "Could not find a sum of squares decomposition."
        , stepMath = if proved then Just sosDecomp else Nothing
        }

      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Conclusion"
        , stepContent = if proved
                        then "Since squares are always non-negative, and a sum of non-negative terms is non-negative, the inequality holds."
                        else reason
        , stepMath = Nothing
        }

      steps = case level of
        Brief -> [step1, step2]
        _ -> [step1, step2, step3]

  in ProofExplanation
       { proofTitle = "Sum of Squares Proof"
       , proofMethod = "SOS"
       , proofGoal = goalStr
       , proofSteps = steps
       , proofConclusion = if proved then "The inequality holds." else "Not proved"
       , proofSuccess = proved
       }

-- | Extract SOS info from trace string
extractSOSFromTrace :: Maybe String -> String
extractSOSFromTrace Nothing = "decomposition not available"
extractSOSFromTrace (Just t) =
  -- Look for various SOS decomposition formats in the trace
  let content = t
      -- Try new format first: "Sum of squares decomposition:"
      newFormatStart = findSubstring "Sum of squares decomposition:" content
      -- Then try old format: "[5] SOS Decomposition:"
      oldFormatStart = findSubstring "[5] SOS Decomposition:" content
  in case newFormatStart of
       Just idx ->
         let afterSOS = drop (idx + 29) content  -- skip past "Sum of squares decomposition:"
             cleaned = dropWhile (`elem` " \n\t") afterSOS
             line = takeWhile (/= '\n') cleaned
         in if null line then "see detailed trace" else line
       Nothing -> case oldFormatStart of
         Just idx ->
           let afterSOS = drop (idx + 22) content  -- skip past "[5] SOS Decomposition:"
               decomp = takeWhileNotSection afterSOS
               cleaned = dropWhile (`elem` " \n\t") decomp
           in if "SDP-Based" `isInfixOf` cleaned || cleaned == "0" || null cleaned
              then extractFromCanonical t
              else takeFirstLine cleaned
         Nothing -> extractFromCanonical t

-- | Extract from [4] Canonical Reduction section as fallback
extractFromCanonical :: String -> String
extractFromCanonical t =
  let canonStart = findSubstring "[4] Canonical Reduction:" t
  in case canonStart of
       Just idx ->
         let afterCanon = drop (idx + 23) t
             identityStart = findSubstring "Identity:" afterCanon
         in case identityStart of
              Just idx2 ->
                let afterIdentity = drop (idx2 + 9) afterCanon
                    cleaned = dropWhile (`elem` " \n\t") afterIdentity
                in takeFirstLine cleaned
              Nothing -> "see detailed trace"
       Nothing -> "see detailed trace"

-- | Take content until next section marker
takeWhileNotSection :: String -> String
takeWhileNotSection s = go s
  where
    go [] = []
    go ('[':c:rest)
      | c >= '0' && c <= '9' = []  -- Found next section like [6]
      | otherwise = '[' : c : go rest
    go (c:rest) = c : go rest

-- | Take first meaningful line
takeFirstLine :: String -> String
takeFirstLine s =
  let line = takeWhile (/= '\n') s
      cleaned = dropWhile (`elem` " \t") line
  in if null cleaned then "see detailed trace"
     else if ">= 0" `isInfixOf` cleaned
          then takeWhile (/= '>') cleaned  -- Remove the ">= 0" suffix
          else cleaned

-- | Find substring index
findSubstring :: String -> String -> Maybe Int
findSubstring needle haystack = go 0 haystack
  where
    go _ [] = Nothing
    go n s@(_:rest)
      | needle `isPrefixOfLocal` s = Just n
      | otherwise = go (n + 1) rest
    isPrefixOfLocal [] _ = True
    isPrefixOfLocal _ [] = False
    isPrefixOfLocal (x:xs) (y:ys) = x == y && isPrefixOfLocal xs ys

-- | Explain Groebner basis proof
explainGroebnerResult :: ExplanationConfig -> Formula -> Bool -> String -> Maybe String -> ProofExplanation
explainGroebnerResult config goal proved reason trace =
  let level = detailLevel config
      goalStr = formatFormula config goal

      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Algebraic formulation"
        , stepContent = "We translate the geometric statement into polynomial equations."
        , stepMath = Just goalStr
        }

      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Groebner basis computation"
        , stepContent = "Computing a Groebner basis for the ideal generated by the hypotheses."
        , stepMath = extractBasisInfo trace
        }

      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Polynomial reduction"
        , stepContent = if proved
                        then "The goal polynomial reduces to zero modulo the basis, proving the theorem."
                        else "The goal does not reduce to zero: " ++ reason
        , stepMath = extractReductionInfo trace
        }

      steps = case level of
        Brief -> [step1, step3]
        _ -> [step1, step2, step3]

  in ProofExplanation
       { proofTitle = "Groebner Basis Proof"
       , proofMethod = "Groebner"
       , proofGoal = goalStr
       , proofSteps = steps
       , proofConclusion = if proved then "The theorem is proved." else "Not proved: " ++ reason
       , proofSuccess = proved
       }

-- | Explain Wu's method proof
explainWuResult :: ExplanationConfig -> Formula -> Bool -> String -> Maybe String -> ProofExplanation
explainWuResult config goal proved reason trace =
  let level = detailLevel config
      goalStr = formatFormula config goal

      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Polynomial formulation"
        , stepContent = "Converting the geometric theorem to polynomial form."
        , stepMath = Just goalStr
        }

      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Characteristic set"
        , stepContent = "Computing the characteristic set by successive pseudo-division."
        , stepMath = extractCharSetInfo trace
        }

      step3 = ExplanationStep
        { stepNumber = 3
        , stepTitle = "Remainder computation"
        , stepContent = if proved
                        then "Pseudo-dividing the conclusion by the characteristic set yields remainder zero."
                        else "Non-zero remainder: " ++ reason
        , stepMath = Nothing
        }

      steps = case level of
        Brief -> [step1, step3]
        _ -> [step1, step2, step3]

  in ProofExplanation
       { proofTitle = "Wu's Method Proof"
       , proofMethod = "Wu"
       , proofGoal = goalStr
       , proofSteps = steps
       , proofConclusion = if proved then "The theorem is proved." else "Not proved"
       , proofSuccess = proved
       }

-- | Generic fallback explanation
explainGenericResult :: ExplanationConfig -> SR.SolverChoice -> Formula -> Bool -> String -> Maybe String -> ProofExplanation
explainGenericResult config solver goal proved reason trace =
  let goalStr = formatFormula config goal
      traceContent = maybe "No trace available" id trace

      step1 = ExplanationStep
        { stepNumber = 1
        , stepTitle = "Problem"
        , stepContent = "Attempting proof using " ++ show solver
        , stepMath = Just goalStr
        }

      step2 = ExplanationStep
        { stepNumber = 2
        , stepTitle = "Result"
        , stepContent = if proved then "Proved: " ++ reason else "Not proved: " ++ reason
        , stepMath = if detailLevel config == Verbose then Just traceContent else Nothing
        }

  in ProofExplanation
       { proofTitle = "Automated Proof"
       , proofMethod = show solver
       , proofGoal = goalStr
       , proofSteps = [step1, step2]
       , proofConclusion = if proved then "The statement is proved." else "Not proved"
       , proofSuccess = proved
       }

-- | Extract basis size from trace
extractBasisInfo :: Maybe String -> Maybe String
extractBasisInfo Nothing = Nothing
extractBasisInfo (Just t) =
  let lines' = lines t
      basisLine = filter (\l -> "Basis" `isInfixOf` l || "basis" `isInfixOf` l) lines'
  in case basisLine of
       (x:_) -> Just x
       [] -> Nothing

-- | Extract reduction result from trace
extractReductionInfo :: Maybe String -> Maybe String
extractReductionInfo Nothing = Nothing
extractReductionInfo (Just t) =
  let lines' = lines t
      reductionLine = filter (\l -> "Reduced" `isInfixOf` l || "remainder" `isInfixOf` l || "Normal Form" `isInfixOf` l) lines'
  in case reductionLine of
       (x:_) -> Just x
       [] -> Nothing

-- | Extract characteristic set info from trace
extractCharSetInfo :: Maybe String -> Maybe String
extractCharSetInfo Nothing = Nothing
extractCharSetInfo (Just t) =
  let lines' = lines t
      charLine = filter (\l -> "Characteristic" `isInfixOf` l || "triangular" `isInfixOf` l) lines'
  in case charLine of
       (x:_) -> Just x
       [] -> Nothing

-- | Check if substring is in string
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- =============================================
-- Formatting Functions
-- =============================================

-- | Format a complete proof explanation.
formatExplanation :: ExplanationConfig -> ProofExplanation -> String
formatExplanation config expl =
  let fmt = format config
      level = detailLevel config
  in case level of
       Minimal -> formatMinimalExplanation fmt expl
       _ -> let title = formatTitle fmt (proofTitle expl)
                goal = formatGoalLine fmt (proofGoal expl)
                stepTexts = map (formatStep config) (proofSteps expl)
                conclusion = formatConclusion fmt (proofConclusion expl) (proofSuccess expl)
            in unlines $ [title, "", goal, ""] ++ stepTexts ++ ["", conclusion]

-- | Format minimal explanation (equations only)
formatMinimalExplanation :: ExplanationFormat -> ProofExplanation -> String
formatMinimalExplanation fmt expl =
  let mathOnly = [m | ExplanationStep _ _ _ (Just m) <- proofSteps expl]
      equations = filter (not . null) mathOnly
      qed = if proofSuccess expl
            then case fmt of
                   ASCII -> "QED"
                   LaTeX -> "$\\square$"
            else "NOT PROVED"
  in unlines $ equations ++ [qed]

-- | Format a single proof step.
formatStep :: ExplanationConfig -> ExplanationStep -> String
formatStep config step =
  let fmt = format config
      header = formatStepHeader fmt (stepNumber step) (stepTitle step)
      body = stepContent step
      math = maybe "" (\m -> "\n" ++ formatMathBlock fmt m) (stepMath step)
  in header ++ "\n" ++ body ++ math ++ "\n"

formatTitle :: ExplanationFormat -> String -> String
formatTitle ASCII title = title ++ "\n" ++ replicate (length title) '='
formatTitle LaTeX title = "\\textbf{" ++ title ++ "}"

formatGoalLine :: ExplanationFormat -> String -> String
formatGoalLine ASCII goal = "Goal: " ++ goal
formatGoalLine LaTeX goal = "\\textbf{Goal:} $" ++ goal ++ "$"

formatStepHeader :: ExplanationFormat -> Int -> String -> String
formatStepHeader ASCII n title = "Step " ++ show n ++ ": " ++ title
formatStepHeader LaTeX n title = "\\textbf{Step " ++ show n ++ ": " ++ title ++ "}"

formatMathBlock :: ExplanationFormat -> String -> String
formatMathBlock ASCII math = "  " ++ math
formatMathBlock LaTeX math = "  $" ++ math ++ "$"

formatConclusion :: ExplanationFormat -> String -> Bool -> String
formatConclusion ASCII concl True = "Conclusion: " ++ concl ++ " QED"
formatConclusion ASCII concl False = "Conclusion: " ++ concl
formatConclusion LaTeX concl True = "\\textbf{Conclusion:} " ++ concl ++ " $\\square$"
formatConclusion LaTeX concl False = "\\textbf{Conclusion:} " ++ concl

conclusionText :: ExplanationConfig -> String -> String
conclusionText _ text = text

-- =============================================
-- Helper Formatting Functions
-- =============================================

formatFormula :: ExplanationConfig -> Formula -> String
formatFormula config f = case format config of
  ASCII -> prettyFormula f
  LaTeX -> prettyFormulaLaTeX f

formatPoly :: ExplanationConfig -> Poly -> String
formatPoly config p = case format config of
  ASCII -> prettyPolyNice p
  LaTeX -> prettyPolyLaTeX p

formatAssumptions :: ExplanationConfig -> Theory -> String
formatAssumptions config theory =
  let formatted = map (formatFormula config) theory
  in case format config of
       ASCII -> intercalate ", " formatted
       LaTeX -> intercalate ", " formatted

formatReductionSteps :: ExplanationConfig -> [P.ProofStep] -> Maybe String
formatReductionSteps config steps =
  let reductions = [(from, to) | P.ReducedToNormalForm from to <- steps]
  in if null reductions
     then Nothing
     else Just $ intercalate " -> " $ map (formatPoly config . snd) reductions

formatSOSDecomposition :: ExplanationConfig -> [(Rational, Poly)] -> [Poly] -> Poly -> String
formatSOSDecomposition config terms lemmas remainder =
  let fmt = format config
      formatTerm (c, p) =
        let coeffStr = if c == 1 then "" else formatRational fmt c ++ " * "
            polyStr = formatPoly config p
        in coeffStr ++ "(" ++ polyStr ++ ")^2"
      termStrs = map formatTerm terms
      lemmaStrs = map (formatPoly config) lemmas
      allParts = termStrs ++ lemmaStrs
      remainderStr = if remainder == polyZero then ""
                     else " + [" ++ formatPoly config remainder ++ "]"
  in if null allParts
     then "0"
     else intercalate " + " allParts ++ remainderStr

formatRational :: ExplanationFormat -> Rational -> String
formatRational ASCII r
  | denominator r == 1 = show (numerator r)
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
formatRational LaTeX r
  | denominator r == 1 = show (numerator r)
  | otherwise = "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"

-- =============================================
-- File Export Functions
-- =============================================

-- | Export explanation to a file. Format is determined by extension.
exportExplanation :: FilePath -> ExplanationConfig -> ProofExplanation -> IO ()
exportExplanation path config expl
  | ".tex" `isSuffixOf` path = exportToLaTeX path expl
  | otherwise = exportToText path config expl
  where
    isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Export to a LaTeX file with proper document structure.
exportToLaTeX :: FilePath -> ProofExplanation -> IO ()
exportToLaTeX path expl = do
  let latexConfig = ExplanationConfig LaTeX Normal
      content = formatExplanation latexConfig expl
      document = unlines
        [ "\\documentclass{article}"
        , "\\usepackage{amsmath,amssymb}"
        , "\\begin{document}"
        , ""
        , content
        , ""
        , "\\end{document}"
        ]
  writeFile path document

-- | Export to a plain text file.
exportToText :: FilePath -> ExplanationConfig -> ProofExplanation -> IO ()
exportToText path config expl = do
  let content = formatExplanation config expl
  writeFile path content
