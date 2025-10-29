# Generalized Fibonacci Polynomials Toolkit
- Implements the recurrence and Binet machinery for generalized Fibonacci polynomials (GFP) described in `reference_paper/ArxivOrthogonalGFP.tex`.
- Ships a Wolfram Language package (`src/GeneralizedFibonacciPolynomials.wl`) that exposes helpers to:
  - construct Fibonacci- and Lucas-type GFP families,
  - generate polynomials via recurrence or Binet formulas,
  - approximate polynomial zeros,
  - analyse orthogonality weight functions, and
  - extract random-walk coefficients when the hypotheses from the reference paper are satisfied.
- Includes a WolframScript test harness (`tests/runTests.wls`) that validates the main identities against textbook families (e.g. Chebyshev and classical Fibonacci sequences).

# Usage
- Ensure `wolframscript.exe` from Mathematica 14.0 is accessible (default path on this machine: `D:\Software\Wolfram Research\Mathematica\14.0\wolframscript.exe`).
- From the repository root, run `"/mnt/d/Software/Wolfram Research/Mathematica/14.0/wolframscript.exe" -file tests/runTests.wls` inside WSL/Arch to execute the automated checks.
- Load the package inside your own notebooks or scripts with `Get["src/GeneralizedFibonacciPolynomials.wl"]` (or append `src` to `$Path` and use `Needs["GeneralizedFibonacciPolynomials`"]`).
- Construct families through `CreateGFPFamily`, then call `GFPPolynomial`, `GFPBinet`, `GFPZeros`, `GFPOrthogonalityData`, or `GFPRandomWalkData` as needed.

# Repository Layout
- `src/GeneralizedFibonacciPolynomials.wl` — Wolfram Language package implementing the GFP toolkit.
- `tests/runTests.wls` — WolframScript test runner.
- `FORMULAS.md` — TeX-ready reference of the identities encoded in the implementation.
- `TEST_SUMMARY.md` — Execution report for the automated test suite.
- `reference_paper/` — Original arXiv source (ignored by version control per requirements).
