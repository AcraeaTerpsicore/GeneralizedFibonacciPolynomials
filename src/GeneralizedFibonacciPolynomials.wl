(* ::Package:: *)

BeginPackage["GeneralizedFibonacciPolynomials`"];

CreateGFPFamily::usage =
  "CreateGFPFamily[d, g, opts] builds an association that represents a generalized Fibonacci polynomial family determined by the recurrence F_n(x)=d(x) F_{n-1}(x)+g(x) F_{n-2}(x). \
The options Type->\"Fibonacci\"|\"Lucas\" specify the species.";

GFPPolynomial::usage =
  "GFPPolynomial[family, n] returns the nth polynomial in x obtained from the recurrence definition stored in family.";

GFPBinet::usage =
  "GFPBinet[family, n] evaluates the Binet-type expression for the nth polynomial.";

GFPZeros::usage =
  "GFPZeros[family, n, opts] numerically approximates the zeros of the nth polynomial. \
Options include \"Method\" (\"Automatic\"|\"Recurrence\"|\"ChebyshevMapped\") and WorkingPrecision.";

GFPOrthogonalityData::usage =
  "GFPOrthogonalityData[family] analyses d(x) and g(x) to detect parity symmetries and weight functions described in the reference paper.";

GFPRandomWalkData::usage =
  "GFPRandomWalkData[family] tries to match the discrete- and continuous-time random walk parameters discussed in the reference paper when d(x)=c x + h.";

GFPBinomialExpansion::usage =
  "GFPBinomialExpansion[family, n] evaluates the generalized Hoggatt coefficient expansion (Lemma 2.6/2.7) for the nth polynomial without using recurrence.";

GFPOrthogonalityCheck::usage =
  "GFPOrthogonalityCheck[family, nmax, opts] numerically tests orthogonality under Proposition 3.4/Corollary 3.5 using the implied weights.";

GFPRandomWalkModel::usage =
  "GFPRandomWalkModel[family, opts] constructs truncated transition/ generator matrices together with potential coefficients and ergodicity diagnostics.";

Options[CreateGFPFamily] = {
   Type -> "Fibonacci",
   LucasP0 -> 2,
   LucasP1 -> Automatic,
   Alpha -> Automatic,
   Variable -> Symbol["\[FormalX]"]
   };

Options[GFPZeros] = {
   "Method" -> "Automatic",
   WorkingPrecision -> MachinePrecision
   };

Options[GFPOrthogonalityCheck] = {
   "Weight" -> Automatic,
   "Pairs" -> Automatic,
   "IntegrationOptions" -> {},
   WorkingPrecision -> Automatic,
   "Tolerance" -> 10^-8,
   Assumptions -> True
   };

Options[GFPRandomWalkModel] = {
   "Type" -> "Discrete",
   "Dimension" -> 6,
   Assumptions -> True
   };

Options[GFPKarlinMcGregor] = {
   "Weight" -> Automatic,
   "IntegrationOptions" -> {},
   WorkingPrecision -> Automatic,
   Assumptions -> True
   };

Begin["`Private`"];

clearContextSymbol[sym_Symbol] := (ClearAll[sym]; Clear[sym]);

toFunction[expr_, var_] := Module[{f},
   f = Function[{var}, Evaluate[expr]];
   f
   ];

normalizeFamilyOptions[type_, opts_] := Module[{p0, p1, alpha},
   p0 = OptionValue[CreateGFPFamily, opts, LucasP0];
   alpha = OptionValue[CreateGFPFamily, opts, Alpha];
   p1 = OptionValue[CreateGFPFamily, opts, LucasP1];
   {p0, p1, alpha}
   ];

CreateGFPFamily[d_, g_, opts : OptionsPattern[]] := Module[
   {
    type = OptionValue[Type],
    var = OptionValue[Variable],
    p0, p1, alpha, dFun, gFun, dExpr, gExpr, aExpr, bExpr,
    polynomialFunction, binetFunction, initial0, initial1, family,
    guardP0, alphaValue, p1Expr
    },
   
   If[! MemberQ[{"Fibonacci", "Lucas"}, type],
    Message[CreateGFPFamily::badtype, type];
    Return[$Failed];
    ];
   
   dFun = If[Head[d] === Function, d, toFunction[d, var]];
   gFun = If[Head[g] === Function, g, toFunction[g, var]];
   dExpr = Simplify[dFun[var]];
   gExpr = Simplify[gFun[var]];
   
   {p0, p1, alpha} = normalizeFamilyOptions[type, {opts}];
   
   If[type === "Lucas",
    guardP0 = MemberQ[{1, -1, 2, -2}, p0];
    If[! guardP0,
     Message[CreateGFPFamily::badp0, p0];
     Return[$Failed];
     ];
    alphaValue = If[alpha === Automatic, Rational[2, p0], alpha];
    p1Expr = If[p1 === Automatic, Simplify[dExpr/alphaValue], Simplify[toFunction[p1, var][var]]];
    If[type === "Lucas" && alphaValue === 0,
     Message[CreateGFPFamily::badalpha];
     Return[$Failed];
     ];
    initial0 = p0;
    initial1 = p1Expr;
    ,
    alphaValue = Null;
    initial0 = 0;
    initial1 = 1;
    p1Expr =.;
    ];
   
   aExpr = Simplify[(dExpr + Sqrt[dExpr^2 + 4 gExpr])/2];
   bExpr = Simplify[(dExpr - Sqrt[dExpr^2 + 4 gExpr])/2];
   
   polynomialFunction =
    With[{dLocal = dExpr, gLocal = gExpr},
     Module[{poly},
      clearContextSymbol[poly];
      poly[0] = initial0;
      poly[1] = initial1;
      poly[n_Integer?NonNegative] := poly[n] =
        Simplify@Expand[dLocal*poly[n - 1] + gLocal*poly[n - 2]];
      poly
      ]
     ];
   
   binetFunction =
    With[{a = aExpr, b = bExpr, alphaLoc = alphaValue, typeLocal = type,
      init0 = initial0, init1 = initial1},
     Module[{bin},
      clearContextSymbol[bin];
      Which[
       typeLocal === "Fibonacci",
       bin[0] = init0;
       bin[1] = init1;
       bin[n_Integer?NonNegative] := bin[n] =
         Simplify@Expand[(a^n - b^n)/(a - b)];
       ,
       typeLocal === "Lucas",
       bin[0] = init0;
       bin[1] = init1;
       bin[n_Integer?NonNegative] := bin[n] =
         Simplify@Expand[(a^n + b^n)/alphaLoc];
       ];
      bin
      ]
     ];
   
   family = <|
     "Type" -> type,
     "Variable" -> var,
     "dFunction" -> dFun,
     "gFunction" -> gFun,
     "dExpression" -> dExpr,
     "gExpression" -> gExpr,
     "Initial0" -> initial0,
     "Initial1" -> initial1,
     "Alpha" -> alphaValue,
     "P1Expression" -> If[type === "Lucas", p1Expr, None],
     "PolynomialFunction" -> polynomialFunction,
     "BinetFunction" -> binetFunction,
     "AExpression" -> aExpr,
     "BExpression" -> bExpr
     |>;
   family
   ];

CreateGFPFamily::badtype = "Unknown family type `1`. Use \"Fibonacci\" or \"Lucas\".";
CreateGFPFamily::badp0 = "Lucas-type families require LucasP0 in {±1, ±2}. Received `1`.";
CreateGFPFamily::badalpha = "Alpha must be non-zero for Lucas-type families.";

GFPPolynomial[family_Association, n_Integer?NonNegative] :=
  family["PolynomialFunction"][n];

GFPBinet[family_Association, n_Integer?NonNegative] :=
  family["BinetFunction"][n];

numericZerosFromPolynomial[poly_, var_, n_Integer, prec_] := Module[{sol, roots},
   sol = Quiet@NSolve[poly == 0, var, WorkingPrecision -> prec];
   roots = var /. sol;
   roots = Select[roots, FreeQ[#, Complex[___, _?Positive]] &];
   N[roots, prec]
   ];

chebyshevMappedZeros[family_, n_, prec_] := Module[
   {
    var = family["Variable"],
    gExpr = family["gExpression"],
    dExpr = family["dExpression"],
    constG, scale, zVals, eq, roots
    },
   constG = Simplify[gExpr];
   If[! FreeQ[constG, var] || constG == 0,
    Return[$Failed]
    ];
   scale = Sqrt[-4 constG];
   zVals = N[Cos[Range[n]*Pi/(n + 1)], prec];
   roots = Table[
     Quiet@Select[var /. NSolve[dExpr == scale*z, var, WorkingPrecision -> prec], Im[#] == 0 &]
     ,
     {z, zVals}
     ];
   If[Or @@ (roots === {}), Return[$Failed]];
   N[Flatten[roots], prec]
   ];

GFPZeros[family_Association, n_Integer?Positive, opts : OptionsPattern[]] := Module[
   {
    method = OptionValue["Method"],
    prec = OptionValue[WorkingPrecision],
    var = family["Variable"],
    poly, auto
    },
   poly = GFPPolynomial[family, n];
   auto = Which[
     method === "ChebyshevMapped", chebyshevMappedZeros[family, n, prec],
     method === "Recurrence", numericZerosFromPolynomial[poly, var, n, prec],
     method === "Automatic",
     Module[{attempt},
      attempt = chebyshevMappedZeros[family, n, prec];
      If[attempt === $Failed,
       numericZerosFromPolynomial[poly, var, n, prec],
       attempt
       ]
      ],
     True, Message[GFPZeros::badmethod, method]; Return[$Failed]
     ];
   Sort[N[auto, prec]]
   ];

GFPZeros::badmethod = "Unknown method `1`. Use \"Automatic\", \"Recurrence\", or \"ChebyshevMapped\".";

parityCheck[expr_, var_] := Module[{odd, even},
   odd = Simplify[(expr /. var -> -var) == -expr];
   even = Simplify[(expr /. var -> -var) == expr];
   <|"Odd" -> TrueQ[odd], "Even" -> TrueQ[even]|>
   ];

identifyCxth[expr_, var_] := Module[{poly, degree, coeffRules, t, c, h},
   If[! PolynomialQ[expr, var], Return[None]];
   coeffRules = Association[CoefficientRules[expr, var]];
   degree = Exponent[expr, var];
   Which[
    degree == 0,
    {0, 0, expr},
    KeyExistsQ[coeffRules, {degree}],
    c = Lookup[coeffRules, Key[{degree}], None];
    h = Lookup[coeffRules, Key[{0}], 0];
    If[c === None, Return[None]];
    t = degree;
    {c, t, h},
    True, None
    ]
   ];

orthogonalityCorollaryData[family_] := Module[
   {
    dExpr = family["dExpression"],
    gExpr = family["gExpression"],
    var = family["Variable"],
    parsedD, c, t, h, k, s1, s2, weight
    },
   parsedD = identifyCxth[dExpr, var];
   If[parsedD === None, Return[<||>]];
   {c, t, h} = parsedD;
   If[! IntegerQ[t] || OddQ[t] === False, Return[<||>]];
   If[! FreeQ[gExpr, var], Return[<||>]];
   k = Simplify[-4*gExpr];
   If[k <= 0, Return[<||>]];
   s1 = Simplify[((-Sqrt[k] - h)/c)^(1/t)];
   s2 = Simplify[((Sqrt[k] - h)/c)^(1/t)];
   weight = Simplify[Sqrt[k - dExpr^2]*var^(t - 1)];
   <|
    "c" -> c,
    "t" -> t,
    "h" -> h,
    "k" -> k,
    "Interval" -> {s1, s2},
    "WeightFunction" -> weight
    |>
   ];

GFPOrthogonalityData[family_Association] := Module[
   {
    dExpr = family["dExpression"],
    gExpr = family["gExpression"],
    var = family["Variable"],
    parityD, parityG, parityResult,
    corollaryData,
    constG, dPrime, weightCheb
    },
   parityD = parityCheck[dExpr, var];
   parityG = parityCheck[gExpr, var];
   parityResult = parityD["Odd"] && parityG["Even"];
   constG = Simplify[gExpr];
   dPrime = D[dExpr, var];
   weightCheb = If[FreeQ[constG, var] && constG =!= 0,
     Simplify[Sqrt[-4 constG - dExpr^2]*dPrime],
     None
     ];
   corollaryData = orthogonalityCorollaryData[family];
   <|
    "ParitySymmetry" -> <|
      "dOdd" -> parityD["Odd"],
      "gEven" -> parityG["Even"],
      "Result" -> parityResult
      |>,
    "ChebyshevMappingWeight" -> weightCheb,
    "CorollaryWeight" -> corollaryData
    |>
   ];

extractLinearParameters[expr_, var_] := Module[{coeff, intercept},
   If[! PolynomialQ[expr, var], Return[None]];
   coeff = Coefficient[expr, var];
   intercept = expr /. var -> 0;
   If[FreeQ[coeff, var] && FreeQ[intercept, var] && Simplify[Exponent[expr, var]] <= 1,
    {coeff, intercept},
    None
    ]
   ];

GFPRandomWalkData[family_Association] := Module[
   {
    dExpr = family["dExpression"],
    gExpr = family["gExpression"],
    var = family["Variable"],
    linParams, c, h, discrete, continuous, k
    },
   linParams = extractLinearParameters[dExpr, var];
   If[linParams === None || ! FreeQ[gExpr, var], Return[<||>]];
   {c, h} = linParams;
   discrete = <|
     "p" -> Simplify[1/c],
     "q" -> Simplify[(c - 1 + h)/c],
     "r" -> Simplify[-h/c]
     |>;
   k = Simplify[-4*gExpr];
   continuous = If[k =!= 0,
     <|
      "lambda" -> Simplify[-1/c],
      "mu" -> Simplify[-k/(4 c)],
      "Diagonal" -> Simplify[(4 + k)/(4 c)]
      |>,
     <||>
     ];
   <|
    "LinearParameters" -> <|"c" -> c, "h" -> h|>,
    "DiscreteTime" -> discrete,
    "ContinuousTime" -> continuous
    |>
   ];

GFPRandomWalkModel[family_Association, opts : OptionsPattern[]] := Module[
   {
    kind = OptionValue["Type"],
    dim = OptionValue["Dimension"],
    assumptions = OptionValue[Assumptions],
    params, discreteData, continuousData, p, q, r, lambda, mu, diag,
    matrix, ratio, piCoeffs, simp, rowSums, boundaryOutflow,
    absRatioLess1, absRatioGreaterEqual1, ergodic,
    resultAssoc
    },
   If[! IntegerQ[dim] || dim < 2,
    Message[GFPRandomWalkModel::baddim, dim];
    Return[$Failed];
    ];
   simp[expr_] := Simplify[expr, Assumptions -> assumptions];
   params = GFPRandomWalkData[family];
   discreteData = Lookup[params, "DiscreteTime", <||>];
   continuousData = Lookup[params, "ContinuousTime", <||>];
   resultAssoc = Switch[kind,
     "Discrete",
     If[discreteData === <||>,
      Message[GFPRandomWalkModel::nodata, kind];
      Return[$Failed];
      ];
     {p, q, r} = Lookup[discreteData, {"p", "q", "r"}];
     matrix = ConstantArray[0, {dim, dim}];
     matrix[[1, 1]] = simp[r];
     If[dim >= 2, matrix[[1, 2]] = simp[p]];
     Do[
      matrix[[i, i - 1]] = simp[q];
      matrix[[i, i]] = simp[r];
      If[i < dim, matrix[[i, i + 1]] = simp[p]];
      ,
      {i, 2, dim}
      ];
     ratio = If[q === 0,
       Indeterminate,
       simp[p/q]
       ];
     piCoeffs = Table[0, {dim}];
     piCoeffs[[1]] = 1;
     If[ratio =!= Indeterminate,
      Do[
       piCoeffs[[k]] = simp[piCoeffs[[k - 1]]*ratio],
       {k, 2, dim}
       ];
      ];
     rowSums = simp /@ Total[matrix, {2}];
     boundaryOutflow = simp[1 - matrix[[1, 1]] - If[dim >= 2, matrix[[1, 2]], 0]];
     absRatioLess1 = If[ratio === Indeterminate, Indeterminate,
       Simplify[Abs[ratio] < 1, Assumptions -> assumptions]
       ];
     absRatioGreaterEqual1 = If[ratio === Indeterminate, Indeterminate,
       Simplify[Abs[ratio] >= 1, Assumptions -> assumptions]
       ];
     ergodic = Which[
       TrueQ[absRatioLess1], True,
       TrueQ[absRatioGreaterEqual1], False,
       True, Indeterminate
       ];
     <|
      "Type" -> "Discrete",
      "Matrix" -> matrix,
      "RowSums" -> rowSums,
      "BoundaryOutflow" -> boundaryOutflow,
      "PotentialRatio" -> ratio,
      "PotentialCoefficients" -> piCoeffs,
      "Ergodicity" -> <|
        "AbsRatioLessThanOne" -> absRatioLess1,
        "AbsRatioGreaterEqualOne" -> absRatioGreaterEqual1,
        "Ergodic" -> ergodic
        |>
      |>,
     "Continuous",
     If[continuousData === <||>,
      Message[GFPRandomWalkModel::nodata, kind];
      Return[$Failed];
      ];
     {lambda, mu, diag} = Lookup[continuousData, {"lambda", "mu", "Diagonal"}];
     matrix = ConstantArray[0, {dim, dim}];
     matrix[[1, 1]] = simp[diag];
     If[dim >= 2, matrix[[1, 2]] = simp[lambda]];
     Do[
      matrix[[i, i - 1]] = simp[mu];
      matrix[[i, i]] = simp[diag];
      If[i < dim, matrix[[i, i + 1]] = simp[lambda]];
      ,
      {i, 2, dim}
      ];
     ratio = If[mu === 0,
       Indeterminate,
       simp[lambda/mu]
       ];
     piCoeffs = Table[0, {dim}];
     piCoeffs[[1]] = 1;
     If[ratio =!= Indeterminate,
      Do[
       piCoeffs[[k]] = simp[piCoeffs[[k - 1]]*ratio],
       {k, 2, dim}
       ];
      ];
     rowSums = simp /@ Total[matrix, {2}];
     absRatioLess1 = If[ratio === Indeterminate, Indeterminate,
       Simplify[Abs[ratio] < 1, Assumptions -> assumptions]
       ];
     absRatioGreaterEqual1 = If[ratio === Indeterminate, Indeterminate,
       Simplify[Abs[ratio] >= 1, Assumptions -> assumptions]
       ];
     ergodic = Which[
       TrueQ[absRatioLess1], True,
       TrueQ[absRatioGreaterEqual1], False,
       True, Indeterminate
       ];
     <|
      "Type" -> "Continuous",
      "Generator" -> matrix,
      "RowSums" -> rowSums,
      "PotentialRatio" -> ratio,
      "PotentialCoefficients" -> piCoeffs,
      "Ergodicity" -> <|
        "AbsRatioLessThanOne" -> absRatioLess1,
        "AbsRatioGreaterEqualOne" -> absRatioGreaterEqual1,
        "Ergodic" -> ergodic
        |>
      |>,
     _,
     Message[GFPRandomWalkModel::badtype, kind];
     Return[$Failed]
     ];
   resultAssoc
   ];

GFPRandomWalkModel::badtype = "Unknown model type `1`. Use \"Discrete\" or \"Continuous\".";
GFPRandomWalkModel::nodata = "No random walk data available for model type `1`.";
GFPRandomWalkModel::baddim = "Dimension must be an integer greater than 1. Received `1`.";

resolveWeightData[family_Association, weightOption_, var_, assumptions_, wpNumeric_] := Module[
   {
    orthData = GFPOrthogonalityData[family],
    corData, weightCheb,
    weightExprLocal, intervalLocal, source,
    dExpr = family["dExpression"],
    gExpr = family["gExpression"],
    constG, scale, sols, rootVals, wpSolver
    },
   Which[
    weightOption =!= Automatic,
    Which[
     AssociationQ[weightOption] && KeyExistsQ[weightOption, "Weight"] && KeyExistsQ[weightOption, "Interval"],
     weightExprLocal = weightOption["Weight"];
     intervalLocal = weightOption["Interval"];
     source = "Provided";
     ,
     MatchQ[weightOption, {_, {_, _}}],
     weightExprLocal = First[weightOption];
     intervalLocal = weightOption[[2]];
     source = "Provided";
     ,
     True,
     Return[Failure["WeightResolution", <|"Reason" -> "BadWeightOption", "Detail" -> weightOption|>]]
     ],
    AssociationQ[corData = orthData["CorollaryWeight"]] && corData =!= <||>,
    weightExprLocal = corData["WeightFunction"];
    intervalLocal = corData["Interval"];
    source = "Corollary";
    ,
    True,
    weightCheb = orthData["ChebyshevMappingWeight"];
    If[weightCheb === None,
     Return[Failure["WeightResolution", <|"Reason" -> "MissingData"|>]]
     ];
    constG = Simplify[gExpr, Assumptions -> assumptions];
    If[constG === 0 || ! FreeQ[constG, var],
     Return[Failure["WeightResolution", <|"Reason" -> "MissingData"|>]]
     ];
    scale = Simplify[Sqrt[-4 constG], Assumptions -> assumptions];
    wpSolver = Max[wpNumeric, 40];
    sols = Quiet@Join[
       NSolve[dExpr == scale, var, WorkingPrecision -> wpSolver],
       NSolve[dExpr == -scale, var, WorkingPrecision -> wpSolver]
       ];
    rootVals = Select[
      Chop[Re[var /. sols], 10^(-wpNumeric/2)],
      NumericQ[#] &
      ];
    If[Length[rootVals] < 2,
     Return[Failure["WeightResolution", <|"Reason" -> "MissingData"|>]]
     ];
    intervalLocal = {Min[rootVals], Max[rootVals]};
    weightExprLocal = weightCheb;
    source = "Chebyshev"
    ];
   If[! (ListQ[intervalLocal] && Length[intervalLocal] == 2),
    Return[Failure["WeightResolution", <|"Reason" -> "BadInterval", "Detail" -> intervalLocal|>]]
    ];
   <|
    "Weight" -> Simplify[weightExprLocal, Assumptions -> assumptions],
    "Interval" -> Simplify[intervalLocal, Assumptions -> assumptions],
    "Source" -> source
    |>
   ];

GFPOrthogonalityCheck[family_Association, nmax_Integer?NonNegative, opts : OptionsPattern[]] := Module[
   {
    type = family["Type"],
    var = family["Variable"],
    assumptions = OptionValue[Assumptions],
    wpOption = OptionValue[WorkingPrecision],
    wpNumeric,
    tol = OptionValue["Tolerance"],
    integrationOpts = OptionValue["IntegrationOptions"],
    weightOption = OptionValue["Weight"],
    pairsOption = OptionValue["Pairs"],
    orthData,
    weightInfo, weightExpr, intervalSym, aNum, bNum,
    pairs, results, matrix, offDiagVals, failedPairs, maxOffDiag,
    parityApplicable, parityPairs, parityConfirmed, parityMismatches,
    toNumeric, buildPairs, applyWeight, integratePair, optsList, weightSource
    },
   
   wp = If[wpOption === Automatic, MachinePrecision, wpOption];
   wpNumeric = If[NumberQ[wp], wp, 16];
   toNumeric[x_] := Module[{num = If[wp === MachinePrecision, N[x], N[x, Max[16, wpNumeric]]] , imag},
     If[NumericQ[num],
      imag = If[Head[num] === Complex, Im[num], 0];
      If[NumericQ[imag] && Abs[imag] <= 10^(-wp/2),
       Chop[Re[num], 10^(-wp/2)],
       $Failed
       ],
      $Failed
      ]
     ];
   
   buildPairs[n_Integer?NonNegative] := Flatten[
     Table[{i, j}, {i, 0, n}, {j, 0, i}],
     1];
   
   applyWeight[expr_] := Simplify[
     If[type === "Lucas",
      1/expr,
      expr
      ],
     Assumptions -> assumptions
     ];
   
   orthData = GFPOrthogonalityData[family];
   weightInfo = resolveWeightData[family, weightOption, var, assumptions, wpNumeric];
   If[FailureQ[weightInfo],
    Switch[Lookup[weightInfo, "Reason"],
     "BadWeightOption", Message[GFPOrthogonalityCheck::badweightopt, Lookup[weightInfo, "Detail"]],
     "MissingData", Message[GFPOrthogonalityCheck::noweight],
     "BadInterval", Message[GFPOrthogonalityCheck::badinterval, Lookup[weightInfo, "Detail"]],
     _, Message[GFPOrthogonalityCheck::noweight]
     ];
    Return[$Failed];
    ];
   weightSource = weightInfo["Source"];
   weightExpr = Simplify[weightInfo["Weight"], Assumptions -> assumptions];
   intervalSym = weightInfo["Interval"];
   If[!(ListQ[intervalSym] && Length[intervalSym] == 2),
    Message[GFPOrthogonalityCheck::badinterval, intervalSym];
    Return[$Failed];
    ];
   {aNum, bNum} = toNumeric /@ intervalSym;
   If[AnyTrue[{aNum, bNum}, # === $Failed &],
    Message[GFPOrthogonalityCheck::numericinterval, intervalSym];
    Return[$Failed];
    ];
   If[!(aNum < bNum),
    {aNum, bNum} = {bNum, aNum};
    ];
   
   weightExpr = applyWeight[weightExpr];
   
   pairs = If[pairsOption === Automatic,
     buildPairs[nmax],
     If[ListQ[pairsOption] && VectorQ[pairsOption, MatchQ[#, {_Integer, _Integer}] &],
      pairsOption,
      Message[GFPOrthogonalityCheck::badpairs, pairsOption];
      Return[$Failed];
      ]
     ];
   
   integrationOpts = If[ListQ[integrationOpts], integrationOpts, {integrationOpts}];
   integrationOpts = DeleteCases[integrationOpts, Null | Sequence[]];
   
   integratePair[n_, m_] := Module[
     {
      f = GFPPolynomial[family, n],
      g = GFPPolynomial[family, m],
      integrandExpr, integrandFun, optsList, xx, value
      },
     integrandExpr = Simplify[f*g*weightExpr, Assumptions -> assumptions];
     xx = Unique["orthVar"];
     integrandFun = If[wp === MachinePrecision,
       Function[{xx}, Evaluate[integrandExpr /. var -> xx]],
       Function[{xx}, Evaluate[N[integrandExpr /. var -> xx, wp]]]
       ];
     optsList = integrationOpts;
     If[wp =!= MachinePrecision && ! AnyTrue[optsList, MatchQ[#, WorkingPrecision -> _] &],
      AppendTo[optsList, WorkingPrecision -> wp]
      ];
     If[! AnyTrue[optsList, MatchQ[#, Exclusions -> _] &],
      AppendTo[optsList, Exclusions -> {xx == aNum, xx == bNum}]
      ];
     value = Quiet@NIntegrate[integrandFun[xx], {xx, aNum, bNum}, Sequence @@ optsList];
     If[NumericQ[value],
      Chop[value, 10^(-If[NumberQ[wpNumeric], wpNumeric, 16]/4)],
      Module[{symbolic, refinedAssumptions},
       refinedAssumptions = assumptions && Element[var, Reals] && aNum <= var <= bNum;
       symbolic = Quiet@Integrate[integrandExpr, {var, aNum, bNum}, Assumptions -> refinedAssumptions];
       If[NumericQ[symbolic],
        Chop[N[symbolic, If[NumberQ[wpNumeric], Max[16, wpNumeric], 16]], 10^(-If[NumberQ[wpNumeric], wpNumeric, 16]/4)],
        $Failed]
       ]
      ]
     ];
   
   results = Map[
     Function[{pair},
       With[{n = pair[[1]], m = pair[[2]], val = integratePair[pair[[1]], pair[[2]]]},
        <|"n" -> n, "m" -> m, "Integral" -> val|>
        ]
       ],
     pairs
     ];
   
   matrix = ConstantArray[Null, {nmax + 1, nmax + 1}];
   Do[
    With[{n = res["n"], m = res["m"], val = res["Integral"]},
    If[0 <= n <= nmax && 0 <= m <= nmax,
     matrix[[n + 1, m + 1]] = val;
      matrix[[m + 1, n + 1]] = val;
      ];
     ],
    {res, results}
    ];
   
   offDiagVals = Select[
     results,
     (#["n"] =!= #["m"] && NumberQ[#["Integral"]]) &
     ];
   maxOffDiag = If[offDiagVals === {},
     0,
     Max[Abs[#["Integral"]] & /@ offDiagVals]
     ];
   
   failedPairs = Select[
     results,
     With[{val = #["Integral"], n = #["n"], m = #["m"]},
       n =!= m && (val === $Failed || Abs[val] > tol)
       ] &
     ];
   
   parityApplicable = TrueQ[orthData["ParitySymmetry", "Result"]] &&
     Chop[aNum + bNum, tol] == 0;
   parityPairs = If[parityApplicable,
     Select[pairs, OddQ[Total[#]] &],
     {}
     ];
   parityConfirmed = Select[
     results,
     MemberQ[parityPairs, {#["n"], #["m"]}] &&
       NumberQ[#["Integral"]] && Abs[#["Integral"]] <= tol &
     ];
   parityMismatches = Select[
     results,
     MemberQ[parityPairs, {#["n"], #["m"]}] &&
       (#["Integral"] === $Failed || Abs[#["Integral"]] > tol) &
     ];
   
   <|
    "Type" -> type,
    "WeightSource" -> weightSource,
    "Weight" -> weightExpr,
    "Interval" -> {aNum, bNum},
    "Tolerance" -> tol,
    "PairsTested" -> results,
    "IntegralMatrix" -> matrix,
    "MaxAbsOffDiagonal" -> maxOffDiag,
    "FailedPairs" -> failedPairs,
    "ParityDiagnostics" -> <|
      "Applies" -> parityApplicable,
      "PredictedPairs" -> parityPairs,
      "Confirmed" -> parityConfirmed,
      "Mismatches" -> parityMismatches
      |>
    |>
   ];

GFPOrthogonalityCheck::badweightopt = "Cannot interpret weight specification `1`.";
GFPOrthogonalityCheck::noweight = "No automatic weight/interval available; supply one with the \"Weight\" option.";
GFPOrthogonalityCheck::nointerval = "Failed to determine suitable integration interval.";
GFPOrthogonalityCheck::badinterval = "Invalid integration interval specification `1`.";
GFPOrthogonalityCheck::numericinterval = "Integration interval `1` could not be converted to numeric endpoints.";
GFPOrthogonalityCheck::badpairs = "Pair specification `1` must be a list of integer index pairs.";

parseKarlinSpec[spec_] := Module[{kind, data, n, t, i, j},
   Which[
    AssociationQ[spec],
    kind = Lookup[spec, "Type", Missing["KeyAbsent", "Type"]];
    If[! StringQ[kind], Return[Failure["KarlinSpec", <|"Reason" -> "BadType", "Detail" -> spec|>]]];
    kind = ToLowerCase[kind];
    Switch[kind,
     "discrete",
     n = Lookup[spec, "Steps", Missing["KeyAbsent", "Steps"]];
     i = Lookup[spec, "From", Missing["KeyAbsent", "From"]];
     j = Lookup[spec, "To", Missing["KeyAbsent", "To"]];
     If[! (IntegerQ[n] && n >= 0 && IntegerQ[i] && i >= 0 && IntegerQ[j] && j >= 0),
      Return[Failure["KarlinSpec", <|"Reason" -> "BadDiscrete", "Detail" -> spec|>]]
      ];
     {"Discrete", n, i, j},
     "continuous",
     t = Lookup[spec, "Time", Missing["KeyAbsent", "Time"]];
     i = Lookup[spec, "From", Missing["KeyAbsent", "From"]];
     j = Lookup[spec, "To", Missing["KeyAbsent", "To"]];
     If[! (NumericQ[t] && t >= 0 && IntegerQ[i] && i >= 0 && IntegerQ[j] && j >= 0),
      Return[Failure["KarlinSpec", <|"Reason" -> "BadContinuous", "Detail" -> spec|>]]
      ];
     {"Continuous", N[t], i, j},
     _,
     Return[Failure["KarlinSpec", <|"Reason" -> "BadType", "Detail" -> kind|>]]
     ],
    ListQ[spec] && Length[spec] >= 4,
    kind = ToLowerCase[First[spec]];
    data = Rest[spec];
    Switch[kind,
     "discrete",
     {n, i, j} = Take[data, 3];
     If[! (IntegerQ[n] && n >= 0 && IntegerQ[i] && i >= 0 && IntegerQ[j] && j >= 0),
      Return[Failure["KarlinSpec", <|"Reason" -> "BadDiscrete", "Detail" -> spec|>]]
      ];
     {"Discrete", n, i, j},
     "continuous",
     {t, i, j} = Take[data, 3];
     If[! (NumericQ[t] && t >= 0 && IntegerQ[i] && i >= 0 && IntegerQ[j] && j >= 0),
      Return[Failure["KarlinSpec", <|"Reason" -> "BadContinuous", "Detail" -> spec|>]]
      ];
     {"Continuous", N[t], i, j},
     _, Return[Failure["KarlinSpec", <|"Reason" -> "BadType", "Detail" -> spec|>]]
     ],
    True,
    Failure["KarlinSpec", <|"Reason" -> "BadSpecification", "Detail" -> spec|>]
    ]
   ];

GFPKarlinMcGregor::badspec = "Invalid Karlin–McGregor specification `1`.";
GFPKarlinMcGregor::badtype = "Unknown Karlin–McGregor type `1`. Use \"Discrete\" or \"Continuous\".";
GFPKarlinMcGregor::baddiscrete = "Discrete specification `1` must provide nonnegative integer steps and indices.";
GFPKarlinMcGregor::badcontinuous = "Continuous specification `1` must provide nonnegative time and integer indices.";
GFPKarlinMcGregor::nodata = "No random walk data available for the `1` case.";
GFPKarlinMcGregor::badinterval = "Invalid integration interval `1`.";
GFPKarlinMcGregor::numericinterval = "Integration interval `1` could not be converted to numeric endpoints.";
GFPKarlinMcGregor::zeroratio = "Transition parameters lead to undefined potential coefficient ratio.";
GFPKarlinMcGregor::integral = "Unable to evaluate the Karlin–McGregor integral for the requested specification.";
GFPKarlinMcGregor::noweight = "No orthogonality weight available; provide one via the \"Weight\" option.";
GFPKarlinMcGregor::norm = "Unable to compute orthogonality norms for indices (`1`, `2`).";

GFPKarlinMcGregor[family_Association, spec_, opts : OptionsPattern[]] := Module[
   {
    parsed = parseKarlinSpec[spec], kind, nSteps = 0, time = 0., i, j,
    assumptions = OptionValue[Assumptions],
    weightOption = OptionValue["Weight"],
    integrationOpts = OptionValue["IntegrationOptions"],
    wpOption = OptionValue[WorkingPrecision],
    wp, wpNumeric, maxPrec, chopTol,
    toNumeric, weightInfo, intervalSym, aNum, bNum, weightExpr, weightSource,
    params = GFPRandomWalkData[family],
    var = family["Variable"],
    polyI, polyJ, weightAdjusted,
    discreteData, continuousData, ratio, maxIndex, piList, prefactorSymbolic, prefactorNumeric,
    integrandExpr, integrandFun, optsList, xx, integralValue, symbolic, specAssoc
    },
  If[FailureQ[parsed],
   Module[{data = parsed[[2]], reason, detail},
    reason = Lookup[data, "Reason", "Unknown"];
    detail = Lookup[data, "Detail", spec];
    Switch[reason,
     "BadType", Message[GFPKarlinMcGregor::badtype, detail],
     "BadDiscrete", Message[GFPKarlinMcGregor::baddiscrete, detail],
     "BadContinuous", Message[GFPKarlinMcGregor::badcontinuous, detail],
     _, Message[GFPKarlinMcGregor::badspec, spec]
     ];
    Return[$Failed]
    ]
   ];
   Switch[First[parsed],
    "Discrete",
    kind = "Discrete";
    nSteps = parsed[[2]];
    i = parsed[[3]];
    j = parsed[[4]];
    ,
    "Continuous",
    kind = "Continuous";
    time = parsed[[2]];
    i = parsed[[3]];
    j = parsed[[4]];
    ,
    _,
    Message[GFPKarlinMcGregor::badtype, First[parsed]];
    Return[$Failed]
    ];
   wp = If[wpOption === Automatic, MachinePrecision, wpOption];
   wpNumeric = If[NumberQ[wp], wp, 16];
   maxPrec = Max[16, wpNumeric /. MachinePrecision -> 16];
   chopTol = 10^(-maxPrec/2);
   toNumeric[x_] := Module[{num = If[wp === MachinePrecision, N[x], N[x, maxPrec]], imag},
     If[NumericQ[num],
      imag = If[Head[num] === Complex, Im[num], 0];
      If[NumericQ[imag] && Abs[imag] <= chopTol,
       Chop[Re[num], chopTol],
       $Failed
       ],
      $Failed
      ]
     ];
   weightInfo = resolveWeightData[family, weightOption, var, assumptions, wpNumeric];
   If[FailureQ[weightInfo],
    Switch[Lookup[weightInfo, "Reason"],
     "BadWeightOption", Message[GFPKarlinMcGregor::badspec, Lookup[weightInfo, "Detail"]],
     "MissingData", Message[GFPKarlinMcGregor::noweight],
     "BadInterval", Message[GFPKarlinMcGregor::badinterval, Lookup[weightInfo, "Detail"]],
     _, Message[GFPKarlinMcGregor::noweight]
     ];
    Return[$Failed]
    ];
   weightSource = weightInfo["Source"];
   weightExpr = Simplify[weightInfo["Weight"], Assumptions -> assumptions];
   intervalSym = weightInfo["Interval"];
   If[!(ListQ[intervalSym] && Length[intervalSym] == 2),
    Message[GFPKarlinMcGregor::badinterval, intervalSym];
    Return[$Failed]
    ];
   {aNum, bNum} = toNumeric /@ intervalSym;
   If[AnyTrue[{aNum, bNum}, # === $Failed &],
    Message[GFPKarlinMcGregor::numericinterval, intervalSym];
    Return[$Failed]
    ];
   If[!(aNum < bNum), {aNum, bNum} = {bNum, aNum}];
   polyI = GFPPolynomial[family, i];
   polyJ = GFPPolynomial[family, j];
  weightAdjusted = Simplify[If[family["Type"] === "Lucas", 1/weightExpr, weightExpr], Assumptions -> assumptions];
  optsList = Flatten[{integrationOpts}];
  optsList = Select[optsList, # =!= Null &];
  If[wp =!= MachinePrecision && ! AnyTrue[optsList, MatchQ[#, WorkingPrecision -> _] &],
   AppendTo[optsList, WorkingPrecision -> maxPrec]
   ];
  If[! AnyTrue[optsList, MatchQ[#, Exclusions -> _] &],
   AppendTo[optsList, Exclusions -> {xx == aNum, xx == bNum}]
   ];
  If[! AnyTrue[optsList, MatchQ[#, Method -> _] &],
   AppendTo[optsList, Method -> "DoubleExponential"]
   ];
  xx = Unique["kmVar"];
  optsList = DeleteDuplicates[optsList];
  measureValue = Quiet@Integrate[weightAdjusted, {var, aNum, bNum},
      Assumptions -> (assumptions && Element[var, Reals] && aNum <= var <= bNum)];
  If[! NumericQ[measureValue] || measureValue <= 0,
   measureValue = Quiet@NIntegrate[
      If[wp === MachinePrecision,
       Function[{xx}, Evaluate[weightAdjusted /. var -> xx]],
       Function[{xx}, Evaluate[N[weightAdjusted /. var -> xx, maxPrec]]]
       ][xx],
      {xx, aNum, bNum}, Sequence @@ optsList
      ];
   If[! NumericQ[measureValue] || measureValue <= 0,
    Message[GFPKarlinMcGregor::integral];
    Return[$Failed]
    ];
   ];
  weightNormalized = Simplify[weightAdjusted/measureValue, Assumptions -> assumptions];
  maxIndex = Max[i, j];
  integrandExpr = Simplify[
    Which[
      kind === "Discrete", var^nSteps,
      kind === "Continuous", Exp[-var*time]
      ]*polyI*polyJ*weightNormalized,
    Assumptions -> assumptions
    ];
  optsList = DeleteDuplicates[optsList];
  integrandFun = If[wp === MachinePrecision,
    Function[{xx}, Evaluate[integrandExpr /. var -> xx]],
    Function[{xx}, Evaluate[N[integrandExpr /. var -> xx, maxPrec]]]
    ];
   symbolic = Quiet@Integrate[integrandExpr, {var, aNum, bNum},
       Assumptions -> (assumptions && Element[var, Reals] && aNum <= var <= bNum)];
   integralValue = If[NumericQ[symbolic],
     Chop[N[symbolic, maxPrec], chopTol],
     Quiet@NIntegrate[integrandFun[xx], {xx, aNum, bNum}, Sequence @@ optsList]
     ];
   If[! NumericQ[integralValue],
    Message[GFPKarlinMcGregor::integral];
    Return[$Failed]
    ];
   integralValue = Chop[integralValue, chopTol];
  normValues = Table[
    With[{poly = GFPPolynomial[family, k]},
     normIntegrand = Simplify[poly^2*weightNormalized, Assumptions -> assumptions];
     Module[{symb = Quiet@Integrate[normIntegrand, {var, aNum, bNum},
           Assumptions -> (assumptions && Element[var, Reals] && aNum <= var <= bNum)],
       normFun, normValLocal},
      normFun = If[wp === MachinePrecision,
        Function[{xx}, Evaluate[normIntegrand /. var -> xx]],
        Function[{xx}, Evaluate[N[normIntegrand /. var -> xx, maxPrec]]]
        ];
      normValLocal = If[NumericQ[symb],
        Chop[N[symb, maxPrec], chopTol],
        Quiet@NIntegrate[normFun[xx], {xx, aNum, bNum}, Sequence @@ optsList]
        ];
      If[! NumericQ[normValLocal], $Failed, Chop[normValLocal, chopTol]]
      ]
     ],
    {k, 0, maxIndex}
    ];
  If[MemberQ[normValues, $Failed, Infinity],
   Message[GFPKarlinMcGregor::norm, i, j];
   Return[$Failed]
   ];
  normI = normValues[[i + 1]];
  normJ = normValues[[j + 1]];
   If[! (NumericQ[normI] && NumericQ[normJ] && normI > 0 && normJ > 0),
    Message[GFPKarlinMcGregor::norm, i, j];
    Return[$Failed]
    ];
   maxIndex = Max[i, j];
   Switch[kind,
    "Discrete",
    discreteData = Lookup[params, "DiscreteTime", <||>];
    If[discreteData === <||>, Message[GFPKarlinMcGregor::nodata, kind]; Return[$Failed]];
    ratio = Simplify[discreteData["p"]/discreteData["q"], Assumptions -> assumptions];
    If[! FreeQ[ratio, Indeterminate] && ratio === Indeterminate,
     Message[GFPKarlinMcGregor::zeroratio]; Return[$Failed]
     ];
    piList = Table[0, {maxIndex + 1}];
    piList[[1]] = 1;
    Do[piList[[k + 1]] = Simplify[piList[[k]]*ratio, Assumptions -> assumptions], {k, 1, maxIndex}];
    prefactorSymbolic = piList[[j + 1]];
    ,
    "Continuous",
    continuousData = Lookup[params, "ContinuousTime", <||>];
    If[continuousData === <||>, Message[GFPKarlinMcGregor::nodata, kind]; Return[$Failed]];
    ratio = Simplify[continuousData["lambda"]/continuousData["mu"], Assumptions -> assumptions];
    If[! FreeQ[ratio, Indeterminate] && ratio === Indeterminate,
     Message[GFPKarlinMcGregor::zeroratio]; Return[$Failed]
     ];
    piList = Table[0, {maxIndex + 1}];
    piList[[1]] = 1;
    Do[piList[[k + 1]] = Simplify[piList[[k]]*ratio, Assumptions -> assumptions], {k, 1, maxIndex}];
    prefactorSymbolic = piList[[j + 1]];
    ];
   prefactorNumeric = Chop[N[prefactorSymbolic, maxPrec], chopTol];
   If[! NumericQ[prefactorNumeric],
    Message[GFPKarlinMcGregor::zeroratio];
    Return[$Failed]
    ];
   specAssoc = <|
     "Type" -> kind,
     "From" -> i,
     "To" -> j
     |>;
   If[kind === "Discrete", specAssoc = Append[specAssoc, "Steps" -> nSteps], specAssoc = Append[specAssoc, "Time" -> time]];
   result = <|
     "Specification" -> specAssoc,
     "WeightSource" -> weightSource,
     "Interval" -> {aNum, bNum},
     "IntegralValue" -> integralValue,
     "Prefactor" -> prefactorSymbolic,
     "Norms" -> normValues,
     "Probability" -> Chop[prefactorNumeric*integralValue/Sqrt[normI*normJ], chopTol]
     |>;
   result
   ];

GFPBinomialExpansion[family_Association, n_Integer?NonNegative] := Module[
   {
    type = family["Type"],
    dExpr = family["dExpression"],
    gExpr = family["gExpression"],
    alpha = Lookup[family, "Alpha", None],
    init0 = family["Initial0"],
    result
    },
   result = Which[
     type === "Fibonacci",
     Which[
      n == 0, 0,
      n >= 1,
      Sum[
        Binomial[n - i - 1, i]*
         dExpr^(n - 2 i - 1)*
         gExpr^i,
        {i, 0, Floor[(n - 1)/2]}
        ]
      ],
     type === "Lucas",
     Which[
      n == 0, init0,
      n >= 1,
      Sum[
        (n/(n - i))*Binomial[n - i, i]*
         dExpr^(n - 2 i)*
         gExpr^i,
        {i, 0, Floor[n/2]}
        ]/alpha
      ],
     True,
     Message[GFPBinomialExpansion::badtype, type];
     Return[$Failed]
     ];
   Simplify[result]
   ];

GFPBinomialExpansion::badtype = "Unknown family type `1`. Expected \"Fibonacci\" or \"Lucas\".";

End[];

EndPackage[];
