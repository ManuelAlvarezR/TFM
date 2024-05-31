param u1>=0;
param u2>=0;
param u3>=0;
param l1>=0;
param l2>=0;
param l3>=0;


var X1 >= l1, <= u1;
var X2 >= l2, <= u2;
var X3 >= l3, <= u3;
var X11 >=0;
var X12 >=0;
var X13 >=0;
var X22 >=0;
var X23 >=0;
var X33 >=0;


minimize objfun:
	-1*X13	+1*X2	+1*X23	+1*X3;

subject to cons1:
	-4*X13	-1*X2 >= -12;

subject to cons2:
	+3*X1	-1*X2 >= -1;
	
	
######-----------------------------------######
#subject to luij:
#	Xi*uj - Xij	-li*uj + li*Xj >= 0;
	
#subject to llij:
#	Xij -lj*Xi - li*Xj + li*lj >= 0;
		
#subject to uuij:
#	ui*uj - ui*Xj - uj*Xi + Xij >= 0;
######------------------------------------######
# n=2, delta=3; admisc::combnk(2*n+delta-1,delta)
# 					=21 restriccions bound factor 	
subject to lu11:
    X1*u1 - X11   - l1*u1 + l1*X1 >= 0;

subject to lu12:
    X1*u2 - X12   - l1*u2 + l1*X2 >= 0;

subject to lu13:
    X1*u3 - X13   - l1*u3 + l1*X3 >= 0;

subject to lu21:
    X2*u1 - X12   - l2*u1 + l2*X1 >= 0;

subject to lu22:
    X2*u2 - X22   - l2*u2 + l2*X2 >= 0;

subject to lu23:
    X2*u3 - X23   - l2*u3 + l2*X3 >= 0;

subject to lu31:
    X3*u1 - X13   - l3*u1 + l3*X1 >= 0;

subject to lu32:
    X3*u2 - X23   - l3*u2 + l3*X2 >= 0;

subject to lu33:
    X3*u3 - X33   - l3*u3 + l3*X3 >= 0;
######------------------------------------######
subject to ll11:
    X11 - l1*X1 - l1*X1 + l1*l1 >= 0;

subject to ll12:
    X12 - l2*X1 - l1*X2 + l1*l2 >= 0;

subject to ll13:
    X13 - l3*X1 - l1*X3 + l1*l3 >= 0;


subject to ll22:
    X22 - l2*X2 - l2*X2 + l2*l2 >= 0;

subject to ll23:
    X23 - l3*X2 - l2*X3 + l2*l3 >= 0;

subject to ll33:
    X33 - l3*X3 - l3*X3 + l3*l3 >= 0;
    
######------------------------------------######
subject to uu11:
    u1*u1 - u1*X1 - u1*X1 + X11 >= 0;

subject to uu12:
    u1*u2 - u1*X2 - u2*X1 + X12 >= 0;

subject to uu13:
    u1*u3 - u1*X3 - u3*X1 + X13 >= 0;

subject to uu22:
    u2*u2 - u2*X2 - u2*X2 + X22 >= 0;

subject to uu23:
    u2*u3 - u2*X3 - u3*X2 + X23 >= 0;

subject to uu33:
    u3*u3 - u3*X3 - u3*X3 + X33 >= 0;

