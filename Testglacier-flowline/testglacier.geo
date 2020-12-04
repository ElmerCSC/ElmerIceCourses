backres = DefineNumber[ 10, Name "Parameters/backres" ];
frontres = DefineNumber[ 50, Name "Parameters/frontres" ];
Point(1) = {0, 0, 0, frontres};
Point(2) = {625, 0, 0, frontres};
Point(3) = {1250, 0, 0, backres};
Point(4) = {1600, 0, 0, backres};
Point(5) = {2500, 0, 0, backres};
//+
Line(1)={1,2};
Line(2)={2,3};
Line(3)={3,4};
Line(4)={4,5};
Extrude {0, 5, 0} {
  Line{1,2,3,4};Layers{10};Recombine;
}



//+
Physical Surface(1) = {8, 12, 16, 20};
//+
Physical Line(21) = {1, 2, 3, 4};
//+
Physical Line(22) = {5, 9, 13, 17};
//+
Physical Line(23) = {6};
//+
Physical Line(24) = {19};

