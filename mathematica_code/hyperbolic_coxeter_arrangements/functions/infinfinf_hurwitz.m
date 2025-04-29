LineReflectPoint[{a_, b_}, x_] := If[
   MemberQ[{a, b, x}, Infinity | -Infinity],
   If[MemberQ[{a}, Infinity | -Infinity],
    2 b - x,
    If[MemberQ[{b}, Infinity | -Infinity],
     2 a - x,
     (a + b)/2]],
   Module[
    {num, den},
    num = b x + a (-2 b + x);
    den = a + b - 2 x;
    Which[den === 0 && num === 0, Indeterminate, den === 0, 
     If[num > 0, Infinity, -Infinity], True, -num/den]]
   ];

LineReflectGeodesic[{a_, b_}, geodesic_] := 
  Sort[LineReflectPoint[{a, b}, #] & /@ geodesic];

HurwitzActionCustomConj[i_, R_, conj_] :=
  If[i > 0,
   ReplacePart[R,
    {
     i -> conj[R[[i]], R[[i + 1]]],
     i + 1 -> R[[i]]
     }],
   If[i < 0,
    ReplacePart[R,
     {
      -i -> R[[-i + 1]],
      -i + 1 -> conj[R[[-i + 1]], R[[-i]]]
      }],
    R
    ]
   ];

HurwitzActionInfInfInfDuples[i_,R_] := HurwitzActionCustomConj[i, R, LineReflectGeodesic];

MultipleHurwitzActionInfInfInfDuples[R_, listOfActions_] :=
  If[Length[listOfActions] == 0,
   R,
   MultipleHurwitzActionInfInfInfDuples[HurwitzActionInfInfInfDuples[Last[listOfActions], R], 
    Most[listOfActions]]
   ];

PlotGeodesicDuple[{a_, b_}, colour_: Red] :=
  Module[
   {center, radius},
   Graphics[Which[
     a === Infinity || a === -Infinity,
     Style[
      Line[{{b, 0}, {b, 2}}],
       colour],
     
     b === Infinity || b === -Infinity,
     Style[
      Line[{{a, 0}, {a, 2}}],
      colour],
     
     True,
     center = (a + b)/2;
     radius = Abs[b - a]/2;
     ParametricPlot[{center + radius Cos[\[Theta]], 
       radius Sin[\[Theta]]}, {\[Theta], 0, Pi}, PlotRange -> All, 
      PlotStyle -> colour]
     ]]
   ];

GenerateWords[i_Integer?Positive, n_Integer?Positive] :=
 Flatten[Table[Tuples[Range[-i, i], k], {k, 1, n}], 1]

GeodesicTupleEqual[a_, b_] := Sort[a] === Sort[b];

AllGeodesicDuplesFromHurwitzAction[R_, actions_] :=
 Module[
  {Rs},
  Rs = Flatten[
    Table[MultipleHurwitzAction[R, action], {action, actions}], 1];
  DeleteDuplicates[Rs, GeodesicTupleEqual]
  ]
