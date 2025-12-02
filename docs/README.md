# Euclid Theorem Prover - Documentation

Welcome to the official documentation for the **Euclid Geometric Theorem Prover**!

---

## 📚 Documentation Index

### For Beginners

1. **[TUTORIAL.md](TUTORIAL.md)** - Start here!
   - Learn the language in 30 minutes
   - Step-by-step lessons with examples
   - Practice exercises
   - Quick reference guide

### For Users

2. **[LANGUAGE.md](LANGUAGE.md)** - Complete Language Reference
   - Formal specification
   - All commands and primitives
   - Mathematical semantics
   - File formats
   - Examples and use cases

### For Language Designers

3. **[GRAMMAR.bnf](GRAMMAR.bnf)** - Formal Grammar
   - Complete BNF/EBNF specification
   - Lexical structure
   - Syntax rules
   - Semantic constraints

---

## 🚀 Quick Start

### Installation

```bash
cabal build
cabal run prover
```

### Your First Proof

```lisp
:point A 0 0
:point B 3 4
(= (dist2 A B) 25)   -- Proves 3² + 4² = 25
```

See **[TUTORIAL.md](TUTORIAL.md)** for more!

---

## 📖 Documentation Structure

```
docs/
├── README.md         # This file
├── TUTORIAL.md       # Interactive learning guide
├── LANGUAGE.md       # Complete reference manual
└── GRAMMAR.bnf       # Formal grammar specification
```

---

## 🎯 What is Euclid?

**Euclid** is a domain-specific language for **automated geometric theorem proving** using:

- **Coordinate Geometry** - Points in 2D/3D space
- **Gröbner Bases** - Algebraic proof method for equalities
- **CAD (Cylindrical Algebraic Decomposition)** - For inequalities
- **Exact Arithmetic** - Rational numbers, no floating-point errors

### Key Features

✅ **Declarative** - State what's true, system proves consequences
✅ **Automated** - No manual proof steps required
✅ **Exact** - Uses symbolic computation, not approximations
✅ **Extensible** - Build reusable lemma libraries
✅ **Educational** - Detailed proof explanations available

---

## 📝 Language at a Glance

### Geometric Primitives

```lisp
(dist2 A B)               -- Squared distance
(perpendicular A B C D)   -- AB ⊥ CD
(parallel A B C D)        -- AB ∥ CD
(collinear A B C)         -- Collinear points
(midpoint A B M)          -- M is midpoint of AB
```

### Commands

```lisp
:point A x y              -- Define point
:assume (= expr 0)        -- Add assumption
(= expr1 expr2)           -- Prove equality
:lemma (= expr 0)         -- Prove and save
:verbose                  -- Show proof steps
:save-lemmas file.lemmas  -- Save theorems
:load-lemmas file.lemmas  -- Load theorems
```

---

## 🧮 Mathematical Foundation

Euclid translates geometric problems into **polynomial algebra**:

1. **Points** → Variables (xA, yA, zA)
2. **Geometric relations** → Polynomial equations
3. **Proving** → Gröbner basis computation
4. **Result** → Normal form = 0 ⇒ Proved ✓

### Soundness

Proofs are based on:
- **Hilbert's Nullstellensatz** (for equalities)
- **Sturm Sequences** (for univariate inequalities)
- **CAD** (for multivariate inequalities)

See **[LANGUAGE.md § 9](LANGUAGE.md#9-mathematical-semantics)** for details.

---

## 🎓 Learning Path

1. **Beginner** → Read [TUTORIAL.md](TUTORIAL.md)
2. **User** → Reference [LANGUAGE.md](LANGUAGE.md)
3. **Developer** → Study [GRAMMAR.bnf](GRAMMAR.bnf)
4. **Researcher** → Read implementation (src/*.hs)

---

## 🔬 Advanced Topics

### Proof Explanations

Enable detailed proof traces:

```lisp
:verbose
(= (dist2 A B) 9)
```

Shows:
- Which assumptions were used
- Variable substitutions applied
- Gröbner basis computation
- Reduction steps

### Lemma Libraries

Build reusable theorem collections:

```lisp
:lemma (= (dist2 A B) 9)
:save-lemmas geometry_basics.lemmas
```

Later:

```lisp
:load-lemmas geometry_basics.lemmas
```

### Inequalities

Solve 1D and 2D inequalities:

```lisp
:solve (> (+ (* x x) -4) 0) x
```

---

## 📐 Example Proofs

### Pythagorean Theorem

```lisp
:point A 0 0
:point B 3 0
:point C 0 4
(= (+ (dist2 A B) (dist2 A C)) (dist2 B C))
```

### Perpendicular Sides of Rectangle

```lisp
:point A 0 0
:point B 4 0
:point C 4 3
:point D 0 3
(= (perpendicular A B A D) 0)
```

### Midpoint Property

```lisp
:point A 0 0
:point B 6 0
:point M 3 0
:assume (= (midpoint A B M) 0)
(= (dist2 A M) (dist2 M B))
```

---

## 🛠️ Implementation Details

### Technology Stack

- **Language:** Haskell
- **Parser:** S-expression parser
- **Algebra:** Multivariate polynomial ring over ℚ
- **Proving:** Buchberger's algorithm (Gröbner bases)
- **Inequalities:** Sturm sequences + CAD

### Project Structure

```
src/
├── Main.hs       # REPL and command processor
├── Expr.hs       # AST and polynomial engine
├── Parser.hs     # S-expression parser
├── Prover.hs     # Gröbner basis + proof engine
├── Sturm.hs      # Sturm sequences for inequalities
└── CAD.hs        # Cylindrical Algebraic Decomposition
```

---

## 🤝 Contributing

We welcome contributions!

- **Prove classic theorems** and share .lemmas files
- **Report bugs** or suggest features
- **Improve documentation**
- **Add examples** to the tutorial

---

## 📜 License

See main repository for license information.

---

## 📞 Support

- **Questions?** Read the [TUTORIAL](TUTORIAL.md)
- **Reference needed?** Check [LANGUAGE.md](LANGUAGE.md)
- **Grammar question?** See [GRAMMAR.bnf](GRAMMAR.bnf)
- **Bug report?** Open an issue on GitHub

---

**Happy Theorem Proving! 📐✨**

*Euclid would be proud.* 😊
