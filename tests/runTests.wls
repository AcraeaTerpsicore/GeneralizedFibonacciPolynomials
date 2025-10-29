(* ::Package:: *)

Get["src/GeneralizedFibonacciPolynomials.wl"];

results = {};

record[label_, outcome_] := Module[{},
   AppendTo[results, <|"Label" -> label, "Pass" -> TrueQ[outcome]|>];
   Print[StringReplace[
      If[TrueQ[outcome], "PASS: `1`", "FAIL: `1`"],
      "`1`" -> label
      ]];
   outcome
   ];

check[label_, expr_] := record[label, Quiet@Check[expr, False]];

(* Fibonacci-type verification *)
Module[{var, family, value},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[var, 1, Variable -> var];
  value = Simplify[GFPPolynomial[family, 5] - GFPBinet[family, 5]] === 0;
  check["Fibonacci recurrence matches Binet for n=5", value];
  ];

(* Chebyshev-type zeros and parity *)
Module[{var, family, zeros, expected, diff, parity},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[2 var, -1,
    Type -> "Lucas", LucasP0 -> 1, LucasP1 -> var, Variable -> var];
  zeros = GFPZeros[family, 3, "Method" -> "Recurrence", WorkingPrecision -> 40];
  expected = N[Cos[(2 Range[3] - 1) Pi/(2*3)], 40];
  diff = Max[Abs[Sort[zeros] - Sort[expected]]];
  check["Chebyshev zeros agree with cosine formula", diff < 10^-25];
  parity = GFPOrthogonalityData[family]["ParitySymmetry", "Result"];
  check["Chebyshev parity symmetry detected", parity === True];
  ];

(* Random walk parameters from linear d(x) *)
Module[{var, family, params},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[3 var - 1, -1,
    Type -> "Lucas", LucasP0 -> 2, LucasP1 -> (3 var - 1), Variable -> var];
  params = GFPRandomWalkData[family];
  check["Discrete random-walk coefficients satisfy constraints",
   And @@ (Simplify[params["DiscreteTime", #] == 1/3] & /@ {"p", "q", "r"})];
  ];

(* Corollary weight extraction for d(x)=cx^t+h *)
Module[{var, family, data},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[3 var + 1, -2, Type -> "Fibonacci", Variable -> var];
  data = GFPOrthogonalityData[family]["CorollaryWeight"];
  check["Corollary weight detected for affine d(x)", AssociationQ[data] && KeyExistsQ[data, "WeightFunction"]];
  ];

passed = Count[results, _?(TrueQ[#["Pass"]] &)];
failed = Length[results] - passed;
summary = <|"Total" -> Length[results], "Passed" -> passed, "Failed" -> failed|>;

Print["RESULTS: ", summary];

If[summary["Failed"] > 0, Exit[1], Exit[0]];
