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

(* Binomial expansion equals recurrence (Fibonacci type) *)
Module[{var, family, ok},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[var, 1, Variable -> var];
  ok = And @@ Table[
      Simplify[GFPPolynomial[family, n] - GFPBinomialExpansion[family, n]] === 0,
      {n, 1, 8}
      ];
  check["Hoggatt expansion reproduces Fibonacci-type polynomials", ok];
  ];

(* Binomial expansion equals recurrence (Lucas type) *)
Module[{var, family, ok},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[2 var, -1,
    Type -> "Lucas", LucasP0 -> 1, LucasP1 -> var, Variable -> var];
  ok = And @@ Table[
      Simplify[GFPPolynomial[family, n] - GFPBinomialExpansion[family, n]] === 0,
      {n, 0, 8}
      ];
  check["Hoggatt expansion reproduces Lucas-type polynomials", ok];
  ];

(* Random walk matrix diagnostics (discrete) *)
Module[{var, family, model, rowsOk, boundaryOk, ergodicFlag},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[3 var - 1, -1,
    Type -> "Lucas", LucasP0 -> 2, LucasP1 -> (3 var - 1), Variable -> var];
  model = GFPRandomWalkModel[family, "Type" -> "Discrete", "Dimension" -> 5];
  rowsOk = Simplify[model["RowSums"][[2]] == 1];
  boundaryOk = Simplify[model["BoundaryOutflow"] == 1/3];
  ergodicFlag = model["Ergodicity", "Ergodic"] === False;
  check["Discrete random walk diagnostics", rowsOk && boundaryOk && ergodicFlag];
  ];

(* Random walk generator diagnostics (continuous) *)
Module[{var, family, model, rowsOk, ergodicFlag},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[-2 var + 1, -2,
    Type -> "Lucas", LucasP0 -> 2, LucasP1 -> (-2 var + 1), Variable -> var];
  model = GFPRandomWalkModel[family, "Type" -> "Continuous", "Dimension" -> 5];
  rowsOk = Simplify[model["RowSums"][[3]] == 0];
  ergodicFlag = model["Ergodicity", "Ergodic"] === True;
  check["Continuous-time random walk diagnostics", rowsOk && ergodicFlag];
  ];

(* Orthogonality verification for Chebyshev family *)
Module[{var, family, result},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[2 var, -1,
    Type -> "Lucas", LucasP0 -> 1, LucasP1 -> var, Variable -> var];
  result = GFPOrthogonalityCheck[family, 3,
    WorkingPrecision -> 40,
    "Tolerance" -> 10^-8,
    "IntegrationOptions" -> {MaxRecursion -> 6}];
  check["Chebyshev orthogonality integrals small",
   result =!= $Failed &&
   NumberQ[result["MaxAbsOffDiagonal"]] &&
   result["MaxAbsOffDiagonal"] < 10^-6 &&
   result["ParityDiagnostics", "Applies"] === True &&
   result["ParityDiagnostics", "Mismatches"] === {}];
  ];

(* Orthogonality verification for affine Fibonacci family *)
Module[{var, family, result},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[3 var + 1, -2,
    Type -> "Fibonacci", Variable -> var];
  result = GFPOrthogonalityCheck[family, 2,
    WorkingPrecision -> 40,
    "Tolerance" -> 10^-6,
    "IntegrationOptions" -> {MaxRecursion -> 6}];
  check["Corollary orthogonality integrals small",
   result =!= $Failed &&
   result["FailedPairs"] === {} &&
   NumberQ[result["MaxAbsOffDiagonal"]] &&
   result["MaxAbsOffDiagonal"] < 10^-5];
  ];

(* Karlin-McGregor discrete base cases *)
Module[{var, family, prob00, prob01},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[2 var, -1,
    Type -> "Lucas", LucasP0 -> 1, LucasP1 -> var, Variable -> var];
  prob00 = GFPKarlinMcGregor[family, {"Discrete", 0, 0, 0},
     WorkingPrecision -> 40,
     "IntegrationOptions" -> {MaxRecursion -> 6}];
  prob01 = GFPKarlinMcGregor[family, {"Discrete", 1, 0, 1},
     WorkingPrecision -> 40,
     "IntegrationOptions" -> {MaxRecursion -> 6}];
  check["Karlin discrete base cases",
   prob00 =!= $Failed && prob01 =!= $Failed &&
   Abs[prob00["Probability"] - 1] < 10^-8 &&
   prob01["Probability"] > 0 && prob01["Probability"] < 1];
  ];

(* Karlin-McGregor invalid specification handling *)
Module[{var, family, result},
  var = Symbol["\[FormalX]"];
  family = CreateGFPFamily[2 var, -1,
    Type -> "Lucas", LucasP0 -> 1, LucasP1 -> var, Variable -> var];
  result = GFPKarlinMcGregor[family, {"Unknown", 1, 0, 0}];
  check["Karlin invalid spec triggers failure", result === $Failed];
  ];

passed = Count[results, _?(TrueQ[#["Pass"]] &)];
failed = Length[results] - passed;
summary = <|"Total" -> Length[results], "Passed" -> passed, "Failed" -> failed|>;

Print["RESULTS: ", summary];

If[summary["Failed"] > 0, Exit[1], Exit[0]];
