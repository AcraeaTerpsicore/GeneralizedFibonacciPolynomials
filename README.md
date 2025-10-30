# Generalized Fibonacci Polynomials Toolkit
- Implements the recurrence and Binet machinery for generalized Fibonacci polynomials (GFP) described in the paper [*Zeros and Orthogonality of generalized Fibonacci polynomials*](https://arxiv.org/abs/2510.00074).
- Ships a Wolfram Language package (`src/GeneralizedFibonacciPolynomials.wl`) that exposes helpers to:
  - construct Fibonacci- and Lucas-type GFP families,
  - generate polynomials via recurrence or Binet formulas,
  - recover generalized Hoggatt (binomial coefficient) expansions without recursion,
  - approximate polynomial zeros,
  - analyse orthogonality weight functions and numerically verify orthogonality via quadrature,
  - extract random-walk coefficients when the hypotheses from the reference paper are satisfied, and
  - build truncated stochastic / birth–death models with potential coefficients and basic ergodicity diagnostics.
- Includes a WolframScript test harness (`tests/runTests.wls`) that validates the main identities against textbook families (e.g. Chebyshev and classical Fibonacci sequences).

# Usage
- Ensure `wolframscript.exe` from Mathematica 14.0 is accessible (default path on this machine: `D:\Software\Wolfram Research\Mathematica\14.0\wolframscript.exe`).
- From the repository root, run `"/mnt/d/Software/Wolfram Research/Mathematica/14.0/wolframscript.exe" -file tests/runTests.wls` inside WSL/Arch to execute the automated checks.
- Load the package inside your own notebooks or scripts with `Get["src/GeneralizedFibonacciPolynomials.wl"]` (or append `src` to `$Path` and use `Needs["GeneralizedFibonacciPolynomials`"]`).
- Construct families through `CreateGFPFamily`, then call `GFPPolynomial`, `GFPBinet`, `GFPBinomialExpansion`, `GFPZeros`, `GFPOrthogonalityData`, `GFPOrthogonalityCheck`, `GFPRandomWalkData`, or `GFPRandomWalkModel` as needed.

# Repository Layout
- `src/GeneralizedFibonacciPolynomials.wl` — Wolfram Language package implementing the GFP toolkit.
- `tests/runTests.wls` — WolframScript test runner.
- `FORMULAS.md` — TeX-ready reference of the identities encoded in the implementation.
- `TEST_SUMMARY.md` — Execution report for the automated test suite.
- `reference_paper/` — Original arXiv source (ignored by version control per requirements).

# Future Extensions
- Provide Karlin–McGregor integral evaluators for discrete and continuous time, enabling explicit transition probability estimates via the orthogonal polynomial and weight data.
- Explore duality mappings between Markov processes and GFP families as highlighted in the concluding discussion, packaging reusable duality function builders.
