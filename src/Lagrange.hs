module Lagrange (solve4Squares, solve4SquaresE) where

import Data.List (find)
import Error

-- | Solve n = a^2 + b^2 + c^2 + d^2 (Either-based API)
solve4SquaresE :: Integer -> Either ProverError [Integer]
solve4SquaresE n
  | n < 0     = Left $ MathematicalError "Cannot represent negative numbers as sum of four squares"
  | n == 0    = Right [0,0,0,0]
  | otherwise =
      let factors = factorize n
          sols = map solvePrime factors
      in Right $ foldl combine4Squares [1,0,0,0] sols

-- | Solve n = a^2 + b^2 + c^2 + d^2 (Legacy API for backward compatibility)
solve4Squares :: Integer -> [Integer]
solve4Squares n = case solve4SquaresE n of
  Left err -> error $ formatError err
  Right result -> result

-- Standard Quaternion Multiplication
-- (a1 + a2i + a3j + a4k)(b1 + b2i + b3j + b4k)
combine4Squares :: [Integer] -> [Integer] -> [Integer]
combine4Squares [a1,a2,a3,a4] [b1,b2,b3,b4] =
    [ a1*b1 - a2*b2 - a3*b3 - a4*b4
    , a1*b2 + a2*b1 + a3*b4 - a4*b3
    , a1*b3 - a2*b4 + a3*b1 + a4*b2
    , a1*b4 + a2*b3 - a3*b2 + a4*b1
    ]

factorize :: Integer -> [Integer]
factorize n = go n 2
  where
    go 1 _ = []
    go x p
      | p*p > x = [x]
      | x `mod` p == 0 = p : go (x `div` p) p
      | otherwise = go x (if p==2 then 3 else p+2)

solvePrime :: Integer -> [Integer]
solvePrime 2 = [1,1,0,0]
solvePrime p = descent p

descent :: Integer -> [Integer]
descent p =
    -- 1. Find m, x such that sum x^2 = m*p, 0 < m < p
    let (m, x) = findInitialMultiple p
    in loop m x
  where
    loop 1 x = x
    loop m x =
        let
            -- 2. Find y_i = x_i mod m in (-m/2, m/2]
            y = map (\xi -> centeredMod xi m) x
            
            -- m' = sum y^2 / m
            sumY2 = sum (map (^2) y)
            m' = sumY2 `div` m
            
            -- 3. Compute z = x * conj(y) (Quaternion product)
            -- This ensures z_i are divisible by m.
            z = quatMulConjugate x y
            
            -- 4. w = z / m
            w = map (`div` m) z
        in loop m' w

    centeredMod n m =
        let r = n `mod` m
        in if r > m `div` 2 then r - m else r

    -- x * conj(y)
    -- y_conj = (y1, -y2, -y3, -y4)
    quatMulConjugate [a1,a2,a3,a4] [b1,b2,b3,b4] =
        -- Real part: a1b1 - a2(-b2) - a3(-b3) - a4(-b4) = sum ai*bi (Dot product)
        [ a1*b1 + a2*b2 + a3*b3 + a4*b4
        , a1*(-b2) + a2*b1 + a3*(-b4) - a4*(-b3)
        , a1*(-b3) - a2*(-b4) + a3*b1 + a4*(-b2)
        , a1*(-b4) + a2*(-b3) - a3*(-b2) + a4*b1
        ]

    findInitialMultiple p =
        -- Find x, y such that x^2 + y^2 + 1 = m*p
        let limit = p `div` 2
            pairs = [ (x, y) | x <- [0..limit], y <- [0..limit] ]
            -- Brute force search for Lemma 1
            res = [ (x,y) | (x,y) <- pairs, (x^2 + y^2 + 1) `mod` p == 0 ]
            (x, y) = case res of
                       (h:_) -> h
                       [] -> error "Lemma 1 failure: Should not happen for primes"
            val = x^2 + y^2 + 1
        in (val `div` p, [x, y, 1, 0])
