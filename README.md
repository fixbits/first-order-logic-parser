# First-Order Logic Parser

A Scala 3 implementation of a first-order logic parser, translated from John Harrison's OCaml code in "Handbook of Practical Logic and Automated Reasoning".

## Features

✅ **Complete FOL parser** with:
- Terms: variables, constants, functions, 6 levels of infix operators
- Formulas: quantifiers (∀, ∃), logical connectives (∧, ∨, ⟹, ⟺), negation
- Predicates and infix relations (=, <, ≤, >, ≥)

✅ **Parser combinator infrastructure**:
- Generic `parseGinfix` for iterated infix operators
- Left and right associativity support
- Precedence handling through subparser composition

✅ **Parametric formula parser**:
- Generic over atom types
- Works with propositional logic, FOL, or custom logics

## Quick Start

```scala
import parser.FOLParser

// Parse simple predicates
val p1 = FOLParser.parse("P(x)")

// Parse quantified formulas
val p2 = FOLParser.parse("forall x . exists y . x < y")

// Parse complex formulas with arithmetic
val p3 = FOLParser.parse("forall x y z . x < y /\\ y < z ==> x < z")

// Parse arithmetic relations
val p4 = FOLParser.parse("2 * x + 3 = y")
```

## Project Structure

```
src/main/scala/
├── logic/
│   ├── Term.scala          # Term data type (Var, Fn)
│   ├── Formula.scala       # Polymorphic formula type
│   └── FOL.scala           # FOL atoms (predicates)
├── util/
│   └── StringUtil.scala    # String utilities (explode, implode)
├── lexer/
│   └── Lexer.scala         # Tokenization
└── parser/
    ├── Combinators.scala   # Parser combinators (parseGinfix, etc.)
    ├── SimpleExpr.scala    # Example: arithmetic expression parser
    ├── FormulaParser.scala # Generic formula parser
    ├── TermParser.scala    # FOL term parser
    ├── FOLAtomParser.scala # FOL atom parser
    └── FOLParser.scala     # Public API

src/test/scala/             # Comprehensive test suite (195+ tests)
```

## Educational Value

This project demonstrates:
- **OCaml to Scala translation** patterns
- **Parser combinator** design
- **Precedence and associativity** handling without parser generators
- **Parametric polymorphism** in parser design
- **Functional programming** techniques

## Implementation Phases

- ✅ Phase 1: Core data types (Term, Formula, FOL)
- ✅ Phase 2: String utilities
- ✅ Phase 3: Lexer
- ✅ Phase 4: Parser combinators
- ✅ Phase 5: Simple expression parser (learning exercise)
- ✅ Phase 6: Generic formula parser
- ✅ Phase 7: Term parser (6 operator levels)
- ✅ Phase 8: FOL atom parser
- ✅ Phase 9: Public API

## Running Tests

```bash
sbt test
```

All 195+ tests should pass.

## License

Educational project based on John Harrison's OCaml code.
