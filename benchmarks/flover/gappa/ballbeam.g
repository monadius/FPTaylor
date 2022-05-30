s4 = float<ieee_64,ne>(Ms4);
s1 = float<ieee_64,ne>(Ms1);
s2 = float<ieee_64,ne>(Ms2);
s3 = float<ieee_64,ne>(Ms3);

Mex0 = ((((-18286e-1 * Ms1) + (-10286e-1 * Ms2)) + (-2008 * Ms3)) + (-104 * Ms4));

ex0 float<ieee_64,ne>= ((((-float<ieee_64,ne>(18286e-1) * s1) + (-float<ieee_64,ne>(10286e-1) * s2)) + (-float<ieee_64,ne>(2008) * s3)) + (-float<ieee_64,ne>(104) * s4));

{ (((((Ms2 >= -5e-1) /\ (Ms2 <= 5e-1)) /\ ((Ms4 >= 0) /\ (Ms4 <= 5e-1))) /\ ((Ms3 >= 0) /\ (Ms3 <= 5e-1))) /\ ((Ms1 >= 0) /\ (Ms1 <= 1))) /\ (Ms4 in [0, 5e-1] /\ Ms1 in [0, 1] /\ Ms2 in [-5e-1, 5e-1] /\ Ms3 in [0, 5e-1])
  -> |ex0 - Mex0| in ? }
