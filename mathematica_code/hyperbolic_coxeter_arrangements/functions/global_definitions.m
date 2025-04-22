tol = 0.01;

(* Useful to have a Sphere and axes *)
sphere = Graphics3D[{
    {Opacity[0.07], Sphere[{0, 0, 0}, 0.999]}
    },
   Boxed -> False, ImageSize -> 400, ViewPoint -> {100, 0, 0}, 
   Lighting -> "Neutral"];
axes = Graphics3D[
   {{Thick, Red, Arrow[{{-1, 0, 0}, {1, 0, 0}}]},
    {Thick, Green, Arrow[{{0, -1, 0}, {0, 1, 0}}]},
    {Thick, Blue, Arrow[{{0, 0, -1}, {0, 0, 1}}]}
    }
   ];
bigAxes = Graphics3D[
   {{Thick, Red, Arrow[{{-2, 0, 0}, {2, 0, 0}}]},
    {Thick, Green, Arrow[{{0, -2, 0}, {0, 2, 0}}]},
    {Thick, Blue, Arrow[{{0, 0, -2}, {0, 0, 2}}]}
    }
   ];
tetrahedronFaceColours = {Red, Green, Blue, Orange};
tetrahedronEdgeColours = {Black, Brown, Gray, Magenta, Cyan, Purple};
