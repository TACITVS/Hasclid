# Hasclid Improvement Roadmap
## Ranked by Difficulty (Easiest → Hardest)

### ✅ Already Completed
- [x] Expression simplification system
- [x] Enhanced pretty printing (prettyPolyNice)
- [x] Lemma library infrastructure
- [x] Verbose proof tracing
- [x] Formal language specification

---

## 🎯 Implementation Queue (Ascending Difficulty)

### **Tier 1: Quick Wins** (1-3 days each)

#### 1. ⭐ Better Error Handling
**Difficulty:** 1/10
**Time:** 1-2 days
**Impact:** High (user experience)
- Replace `error` calls with `Either` or custom error types
- Add proper error messages for parse failures
- Graceful handling of division by zero, invalid inputs

#### 2. ⭐ Non-degeneracy Checking
**Difficulty:** 2/10
**Time:** 2-3 days
**Impact:** Medium (correctness)
- Check for coincident points
- Warn about zero-length segments
- Validate non-collinearity when needed
- Add `:validate` command

#### 3. ⭐ Caching & Memoization
**Difficulty:** 3/10
**Time:** 2-3 days
**Impact:** High (performance)
- Cache Gröbner bases for common sub-problems
- Memoize polynomial operations
- Add `--cache` flag to REPL

---

### **Tier 2: Moderate Improvements** (3-7 days each)

#### 4. 🔧 Better Term Ordering
**Difficulty:** 4/10
**Time:** 3-5 days
**Impact:** High (performance)
- Add Lex, GrLex, GrevLex orderings
- Allow user to specify: `:set-order grevlex`
- Benchmark performance differences

#### 5. 🔧 Enhanced Gröbner Basis Algorithm
**Difficulty:** 5/10
**Time:** 5-7 days
**Impact:** High (performance)
- Implement Buchberger selection strategies (Normal, Sugar, Gebauer-Möller)
- Add criteria to skip useless S-polynomials
- Optimize pair selection

#### 6. 🔧 Counter-example Finding
**Difficulty:** 5/10
**Time:** 3-5 days
**Impact:** Medium (usability)
- When proof fails, generate counter-example
- Use CAD sampling to find witness
- Add `:find-counterexample` command

#### 7. 🔧 Improved Positivity Checking
**Difficulty:** 5/10
**Time:** 4-6 days
**Impact:** High (correctness)
- Add real root isolation
- Implement interval arithmetic method
- Add SOS decomposition with SDP
- Fallback to sampling with warnings

#### 8. 🔧 Infix Syntax Option
**Difficulty:** 6/10
**Time:** 5-7 days
**Impact:** High (usability)
- Add alternative infix parser: `dist2(A,B) = 9`
- Support both syntaxes simultaneously
- Add `:syntax prefix|infix` toggle

---

### **Tier 3: Substantial Features** (1-2 weeks each)

#### 9. 🏗️ Geometric Transformations
**Difficulty:** 6/10
**Time:** 7-10 days
**Impact:** Medium (expressiveness)
- Add: Translate, Rotate, Scale, Reflect
- Syntax: `:transform rotate(90, O) A -> A'`
- Prove invariants under transformations

#### 10. 🏗️ Incremental Proving
**Difficulty:** 7/10
**Time:** 7-14 days
**Impact:** High (performance)
- Extend existing Gröbner basis when adding assumptions
- Avoid full recomputation
- Maintain basis cache across commands

#### 11. 🏗️ Proof Certificates
**Difficulty:** 7/10
**Time:** 10-14 days
**Impact:** High (verification)
- Export to Coq/Lean/Isabelle
- Generate verifiable proof terms
- Add `:export-proof file.v` command

#### 12. 🏗️ Constraint Solving
**Difficulty:** 7/10
**Time:** 10-14 days
**Impact:** Medium (automation)
- Find point configurations satisfying constraints
- Syntax: `:construct "right triangle with legs 3,4"`
- Use Gröbner basis to solve for unknowns

---

### **Tier 4: Advanced Algorithms** (2-4 weeks each)

#### 13. 🚀 Complete CAD Implementation
**Difficulty:** 8/10
**Time:** 14-21 days
**Impact:** Very High (capabilities)
- Implement full CAD lifting phase
- Add sample tree data structure
- Complete 2D/3D inequality solver
- This unlocks multivariate inequality proving

#### 14. 🚀 Metric Geometry (Angles)
**Difficulty:** 8/10
**Time:** 14-21 days
**Impact:** High (expressiveness)
- Add `Angle A B C` primitive
- Requires trigonometric solving
- Add degree/radian conversions
- Careful handling of angle ranges

#### 15. 🚀 Parallel Computation
**Difficulty:** 8/10
**Time:** 14-21 days
**Impact:** High (performance)
- Parallelize S-polynomial computation
- Use Haskell's `par`/`pseq` primitives
- Benchmark speedup on multi-core systems

#### 16. 🚀 Wu's Method
**Difficulty:** 9/10
**Time:** 21-28 days
**Impact:** High (performance)
- Implement characteristic set algorithm
- Often faster than Gröbner for geometry
- Add `:method wu|groebner` option
- Requires careful dependency handling

#### 17. 🚀 Proof Search Strategies
**Difficulty:** 9/10
**Time:** 21-28 days
**Impact:** Medium (automation)
- Auto-apply lemmas intelligently
- Add `:auto-prove` with timeout
- Implement `:hint` suggestions
- Requires heuristic search

---

### **Tier 5: Research-Level Features** (1-3 months each)

#### 18. 🎓 Interactive Proof Assistant Mode
**Difficulty:** 9/10
**Time:** 28-42 days
**Impact:** High (education)
- Step-by-step proof construction
- Tactic language: `:tactic apply-lemma`, `:tactic reduce`
- Proof state tracking
- Undo/redo mechanism

#### 19. 🎓 Area Method (Chou-Gao-Zhang)
**Difficulty:** 9/10
**Time:** 28-42 days
**Impact:** High (performance)
- Express geometry using signed areas
- Alternative proof method
- Often simpler than Gröbner for certain theorems
- Requires complete reimplementation

#### 20. 🎓 Quantifier Elimination
**Difficulty:** 10/10
**Time:** 42-60 days
**Impact:** Very High (theoretical)
- Full Tarski-Seidenberg algorithm
- Add `Exists` and `Forall` quantifiers
- Complete decision procedure for real closed fields
- This is a PhD-level feature

#### 21. 🎓 Visualization (SVG/TikZ)
**Difficulty:** 8/10
**Time:** 21-35 days
**Impact:** High (education)
- Generate geometric diagrams
- Export to SVG, TikZ, or PNG
- Interactive geometry viewer
- Requires constraint solving + rendering

#### 22. 🎓 WebAssembly Frontend
**Difficulty:** 10/10
**Time:** 60+ days
**Impact:** Very High (reach)
- Compile Haskell to WASM via GHCJS
- Build interactive web interface
- Online geometry editor
- Major infrastructure change

---

## 📊 Recommended Implementation Order

**Phase 1 (Month 1-2):** Quick wins for stability
1. Better Error Handling
2. Non-degeneracy Checking
3. Caching & Memoization
4. Better Term Ordering

**Phase 2 (Month 2-4):** Performance improvements
5. Enhanced Gröbner Basis Algorithm
6. Counter-example Finding
7. Improved Positivity Checking
8. Incremental Proving

**Phase 3 (Month 4-6):** Expand capabilities
9. Complete CAD Implementation ⭐ (Critical)
10. Infix Syntax Option
11. Geometric Transformations
12. Constraint Solving

**Phase 4 (Month 6-9):** Advanced features
13. Wu's Method
14. Metric Geometry (Angles)
15. Proof Certificates
16. Parallel Computation

**Phase 5 (Month 9-12):** Research features
17. Proof Search Strategies
18. Interactive Proof Assistant
19. Area Method
20. Quantifier Elimination

**Phase 6 (Beyond):** Major infrastructure
21. Visualization
22. WebAssembly Frontend

---

## ✅ Success Criteria for Each Item

Each implementation must:
1. **Pass all existing tests** - No regressions
2. **Include new test suite** - Comprehensive coverage
3. **Update documentation** - LANGUAGE.md, TUTORIAL.md
4. **Add examples** - At least 2 `.euclid` scripts
5. **Git commit with message** - Following conventional commits
6. **Benchmark performance** - Compare before/after
7. **Code review checklist** - Style, correctness, efficiency

---

## 🔄 Git Workflow

For each feature:
```bash
git checkout -b feature/name-of-improvement
# ... implement ...
git add -A
git commit -m "feat: implement name-of-improvement"
# ... test thoroughly ...
cabal test
# ... if approved ...
git checkout main
git merge feature/name-of-improvement
git push origin main
```

---

**Current Status:** Ready to start Tier 1, Item 1 (Better Error Handling)
