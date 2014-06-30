@rnd = float<ieee_64,ne>;

t rnd= (3*x1*x1 + 2*x2 - x1);
r rnd=  x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));

Mt = (3*x1*x1 + 2*x2 - x1);
Mr = x1 + ((2*x1*(Mt/(x1*x1 + 1))*
    (Mt/(x1*x1 + 1) - 3) + x1*x1*(4*(Mt/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(Mt/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));

{ x1 in [-5, 5] /\
  x2 in [-20, 5]
    -> |r - Mr| in ? }



