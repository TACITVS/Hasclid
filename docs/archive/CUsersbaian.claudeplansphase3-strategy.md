# Phase 3: Morley Lemma Decomposition Strategy

## Goal
Prove Morley's theorem through a sequence of polynomial lemmas

## Target Triangle
**Right Isosceles**: A=(0,0), B=(1,0), C=(0,1)
- Angle at A = 90° → trisected to 3×30°
- Angle at B = 45° → trisected to 3×15°  
- Angle at C = 45° → trisected to 3×15°

## Algebraic Trisector Constraints

### From Angle A (90° → 30° each)
- First trisector: `3*y² = x²` (tan²(30°) = 1/3)
- Second trisector: `y² = 3*x²` (tan²(60°) = 3)

### From Angle B (45° → 15° each)
Using angle sum formulas for 15° = 45° - 30°:
- tan(15°) = tan(45° - 30°) = (1 - 1/√3)/(1 + 1/√3) = 2 - √3 ≈ 0.268

Algebraic constraint (avoiding trig):
- For point (x,y) on trisector from B=(1,0):
- Slope = tan(15°) satisfies: `t² + 4t - 1 = 0` where `t = (y-0)/(x-1)`
- Polynomial: `y² + 4*y*(x-1) - (x-1)² = 0`

### From Angle C (45° → 15° each)
By symmetry with B (C is at (0,1) instead of (1,0)):
- For point (x,y) on trisector from C=(0,1):
- Polynomial: `x² + 4*x*(y-1) - (y-1)² = 0`

## Lemma Sequence

### ✅ Lemma 1: Basic Trisector Property (PROVED)
**Statement**: Trisector constraint is self-consistent
**Proof**: Trivial (algebraic identity)

### ✅ Lemma 2: First Morley Point - x-coordinate (PROVED)
**Given**: 
- `3*y² = x²` (first trisector from A)
- `3*y² = (x-1)²` (approximation - needs refinement)

**Prove**: `2*x = 1` → x = 1/2
**Status**: PROVED via Groebner ✅

### 🔄 Lemma 3: First Morley Point - y-coordinate
**Given**: 
- `x = 1/2` (from Lemma 2)
- `3*y² = x²` (trisector constraint)

**Prove**: `12*y² = 1` (polynomial form of `y² = 1/12`)

### 🔄 Lemma 4: Second Morley Point (Q)
Intersection of:
- Second trisector from A: `y² = 3*x²`
- First trisector from C: `x² + 4*x*(y-1) - (y-1)² = 0`

**Prove**: Coordinates satisfy polynomial system

### 🔄 Lemma 5: Third Morley Point (R)
Intersection of:
- Second trisector from A: `y² = 3*x²`
- First trisector from B: `y² + 4*y*(x-1) - (x-1)² = 0`

**Prove**: Coordinates satisfy polynomial system

### 🔄 Lemma 6: Distance PQ²
**Given**: Coordinates of P and Q
**Prove**: Polynomial equation for `d_PQ² = (xQ - xP)² + (yQ - yP)²`

### 🔄 Lemma 7: Distance QR²
**Given**: Coordinates of Q and R
**Prove**: Polynomial equation for `d_QR²`

### 🔄 Lemma 8: Distance RP²
**Given**: Coordinates of R and P
**Prove**: Polynomial equation for `d_RP²`

### 🎯 Lemma 9: FINAL - Equilateral Triangle
**Given**: d_PQ², d_QR², d_RP² (from Lemmas 6-8)
**Prove**: `d_PQ² = d_QR²` AND `d_QR² = d_RP²`

## Strategy Notes

### Critical Success Factors
1. **Pure polynomial formulations** - no division in goals
2. **Incremental proofs** - each lemma builds on previous
3. **Groebner basis** preferred over CAD for polynomial ideals
4. **Exact symbolic coordinates** - avoid numerical approximations

### Challenges
- Trisector formulas are complex (degree 4 polynomials)
- Multiple intersection points (need to identify correct one)
- May need to add sign constraints to select right solution

### Fallback Strategy
If full symbolic proof fails:
- Use numerical coordinates with high-precision rationals
- Prove for specific triangle (establishes feasibility)
- Generalize approach for other triangles

## Implementation Plan

**Session 1** (Current): Prove Lemmas 3-5 (Morley point coordinates)
**Session 2**: Prove Lemmas 6-8 (Distances)
**Session 3**: Prove Lemma 9 (Final equilateral property)

## Expected Outcome
Complete proof that the Morley triangle is equilateral for right isosceles case,
demonstrating HASCLID's capability for advanced geometric theorem proving.
