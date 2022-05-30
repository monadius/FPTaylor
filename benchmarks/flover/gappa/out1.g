y1 = float<ieee_64,ne>(My1);
y0 = float<ieee_64,ne>(My0);
s1 = float<ieee_64,ne>(Ms1);
s2 = float<ieee_64,ne>(Ms2);
s0 = float<ieee_64,ne>(Ms0);

Mex0 = ((((-3795323e-4 * Ms0) + (-5443608e-4 * Ms1)) + (92729e-3 * Ms2)) + 45165916241610748e-13);

ex0 float<ieee_64,ne>= ((((-float<ieee_64,ne>(3795323e-4) * s0) + (-float<ieee_64,ne>(5443608e-4) * s1)) + (float<ieee_64,ne>(92729e-3) * s2)) + float<ieee_64,ne>(45165916241610748e-13));

{ ((((((Ms2 >= 0) /\ (Ms2 <= 10)) /\ ((Ms0 >= 0) /\ (Ms0 <= 46e-1))) /\ ((My0 >= 0) /\ (My0 <= 10))) /\ ((Ms1 >= 0) /\ (Ms1 <= 10))) /\ ((My1 >= 0) /\ (My1 <= 10))) /\ (My1 in [0, 10] /\ My0 in [0, 10] /\ Ms1 in [0, 10] /\ Ms2 in [0, 10] /\ Ms0 in [0, 46e-1])
  -> |ex0 - Mex0| in ? }
