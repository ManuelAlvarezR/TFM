#  NLP written by GAMS Convert at 01/12/18 13:43:20
param u1>=0;
param u2>=0;
param u3>=0;
param l1>=0;
param l2>=0;
param l3>=0;

var x1 >= l1, <= u1;
var x2 >= l2, <= u2;
var x3 >= l3, <= u3;

minimize objfun: x2*x3 + x2 + x3 - x1*x3;

subject to

e1:  - 4*x1*x3 - x2 >= -12;

e2:    3*x1 - x2 >= -1;

