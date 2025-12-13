{-# LANGUAGE DeriveGeneric #-}

module Cache
  ( GroebnerCache
  , CacheStats(..)
  , emptyCache
  , emptyCacheStats
  , lookupBasis
  , insertBasis
  , clearCache
  , getCacheStats
  , formatCacheStats
  ) where

import Expr (Poly)
import qualified Data.Map.Strict as M
import Data.List (sort)
import GHC.Generics (Generic)

-- =============================================
-- Cache Data Structures
-- =============================================

-- | Cache key: sorted list of polynomials (ideal generators)
-- We sort to ensure (p1, p2) and (p2, p1) map to same key
type CacheKey = [Poly]

-- | Cache value: computed Gröbner basis
type CacheValue = [Poly]

-- | Gröbner basis cache
-- Maps ideal generators to their computed Gröbner basis
data GroebnerCache = GroebnerCache
  { cacheMap :: M.Map CacheKey CacheValue
  , stats :: CacheStats
  } deriving (Show, Generic)

-- | Cache statistics for performance tracking
data CacheStats = CacheStats
  { hits :: Int      -- Number of cache hits
  , misses :: Int    -- Number of cache misses
  , inserts :: Int   -- Number of insertions
  , clears :: Int    -- Number of cache clears
  } deriving (Show, Eq, Generic)

-- =============================================
-- Cache Operations
-- =============================================

-- | Create an empty cache
emptyCache :: GroebnerCache
emptyCache = GroebnerCache M.empty emptyCacheStats

-- | Create empty cache statistics
emptyCacheStats :: CacheStats
emptyCacheStats = CacheStats 0 0 0 0

-- | Look up a Gröbner basis in the cache
-- Returns (Maybe basis, updated cache with stats)
lookupBasis :: [Poly] -> GroebnerCache -> (Maybe [Poly], GroebnerCache)
lookupBasis generators cache =
  let key = normalizeKey generators
      result = M.lookup key (cacheMap cache)
      newStats = case result of
                   Just _  -> (stats cache) { hits = hits (stats cache) + 1 }
                   Nothing -> (stats cache) { misses = misses (stats cache) + 1 }
  in (result, cache { stats = newStats })

-- | Insert a computed Gröbner basis into the cache
insertBasis :: [Poly] -> [Poly] -> GroebnerCache -> GroebnerCache
insertBasis generators basis cache =
  let key = normalizeKey generators
      newMap = M.insert key basis (cacheMap cache)
      newStats = (stats cache) { inserts = inserts (stats cache) + 1 }
  in cache { cacheMap = newMap, stats = newStats }

-- | Clear the entire cache (e.g., when assumptions change)
clearCache :: GroebnerCache -> GroebnerCache
clearCache cache =
  let newStats = (stats cache) { clears = clears (stats cache) + 1 }
  in GroebnerCache M.empty newStats

-- | Get current cache statistics
getCacheStats :: GroebnerCache -> CacheStats
getCacheStats = stats

-- =============================================
-- Cache Key Normalization
-- =============================================

-- | Normalize a list of polynomials to create a canonical cache key
-- Sort polynomials to ensure order-independence
normalizeKey :: [Poly] -> CacheKey
normalizeKey = sort

-- =============================================
-- Statistics Formatting
-- =============================================

-- | Format cache statistics for display
formatCacheStats :: CacheStats -> String
formatCacheStats s =
  let total = hits s + misses s
      hitRate = if total == 0
                then 0.0
                else (fromIntegral (hits s) / fromIntegral total * 100.0) :: Double
  in unlines
       [ "=== Cache Statistics ==="
       , "Total lookups:  " ++ show total
       , "Cache hits:     " ++ show (hits s) ++ " (" ++ show (round hitRate :: Int) ++ "%)"
       , "Cache misses:   " ++ show (misses s)
       , "Insertions:     " ++ show (inserts s)
       , "Cache clears:   " ++ show (clears s)
       , "Current size:   " ++ "N/A (use :cache-info for size)"
       , "========================"
       ]
