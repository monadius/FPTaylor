s4 = float<ieee_64,ne>(Ms4);
s1 = float<ieee_64,ne>(Ms1);
s2 = float<ieee_64,ne>(Ms2);
s3 = float<ieee_64,ne>(Ms3);

Mex0 = ((((1 * Ms1) + (16567e-4 * Ms2)) + (-186854e-4 * Ms3)) + (-34594e-4 * Ms4));

ex0 float<ieee_64,ne>= ((((float<ieee_64,ne>(1) * s1) + (float<ieee_64,ne>(16567e-4) * s2)) + (-float<ieee_64,ne>(186854e-4) * s3)) + (-float<ieee_64,ne>(34594e-4) * s4));

{ (((((Ms2 >= -10) /\ (Ms2 <= 10)) /\ ((Ms4 >= -785e-3) /\ (Ms4 <= 785e-3))) /\ ((Ms3 >= -785e-3) /\ (Ms3 <= 785e-3))) /\ ((Ms1 >= -50) /\ (Ms1 <= 50))) /\ (Ms4 in [-785e-3, 785e-3] /\ Ms1 in [-50, 50] /\ Ms2 in [-10, 10] /\ Ms3 in [-785e-3, 785e-3])
  -> |ex0 - Mex0| in ? }
