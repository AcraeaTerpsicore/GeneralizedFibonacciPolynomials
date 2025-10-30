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

GFPOrthogonalityCheck[family_Association, nmax_Integer?NonNegative, opts : OptionsPattern[]] := Module[
   {
    type = family["Type"],
    var = family["Variable"],
    assumptions = OptionValue[Assumptions],
    wpOption = OptionValue[WorkingPrecision],
    wpNumeric,
    tol = OptionValue["Tolerance"],
    integrationOpts = OptionValue["IntegrationOptions"],
    orthData = GFPOrthogonalityData[family],
    weightOption = OptionValue["Weight"],
    pairsOption = OptionValue["Pairs"],
    weightInfo, weightExpr, interval, aNum, bNum,
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
   
   weightInfo = Module[
     {
      source = None,
      weightExprLocal = None,
      intervalLocal = None,
      corData, weightCheb, dExpr, gExpr, constG, scale,
      sols, rootVals
      },
     
     Which[
      weightOption =!= Automatic,
      Which[
       MatchQ[weightOption, {_, {_, _}}],
       {weightExprLocal, intervalLocal} = weightOption;
       source = "Provided",
       AssociationQ[weightOption] && KeyExistsQ[weightOption, "Weight"] && KeyExistsQ[weightOption, "Interval"],
       weightExprLocal = weightOption["Weight"];
       intervalLocal = weightOption["Interval"];
       source = "Provided",
       True,
       Message[GFPOrthogonalityCheck::badweightopt, weightOption];
       Return[$Failed]
       ],
      
      (corData = orthData["CorollaryWeight"]; AssociationQ[corData] && corData =!= <||>),
      weightExprLocal = corData["WeightFunction"];
      intervalLocal = corData["Interval"];
      source = "Corollary",
      
      True,
      weightCheb = orthData["ChebyshevMappingWeight"];
      If[weightCheb === None,
       Message[GFPOrthogonalityCheck::noweight];
       Return[$Failed];
       ];
      weightExprLocal = weightCheb;
      source = "Chebyshev";
      dExpr = family["dExpression"];
      gExpr = family["gExpression"];
      constG = Simplify[gExpr, Assumptions -> assumptions];
      scale = Simplify[Sqrt[-4 constG], Assumptions -> assumptions];
      sols = Join[
        Quiet@NSolve[dExpr == scale, var, WorkingPrecision -> wp],
        Quiet@NSolve[dExpr == -scale, var, WorkingPrecision -> wp]
        ];
      rootVals = DeleteDuplicates[
        Module[{vals = var /. sols},
         vals = N[vals, wp];
         vals = Select[vals, NumericQ[#] && Abs[Im[#]] <= 10^(-wp/2) &];
         Chop[Re[vals], 10^(-wp/2)]
         ]
        ];
      rootVals = Select[rootVals, NumberQ[#] &];
      If[Length[rootVals] < 2,
       Message[GFPOrthogonalityCheck::nointerval];
       Return[$Failed];
       ];
      intervalLocal = {Min[rootVals], Max[rootVals]}
      ];
     
     If[! ListQ[intervalLocal] || Length[intervalLocal] != 2,
      Message[GFPOrthogonalityCheck::badinterval, intervalLocal];
      Return[$Failed];
      ];
     
     <|
      "Weight" -> weightExprLocal,
      "Interval" -> intervalLocal,
      "Source" -> source
      |>
     ];
   
   If[weightInfo === $Failed, Return[$Failed]];
   weightSource = weightInfo["Source"];
   weightExpr = Simplify[weightInfo["Weight"], Assumptions -> assumptions];
   interval = weightInfo["Interval"];
   {aNum, bNum} = toNumeric /@ interval;
   If[AnyTrue[{aNum, bNum}, # === $Failed &],
    Message[GFPOrthogonalityCheck::numericinterval, interval];
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
