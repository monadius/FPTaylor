x3 = float<ieee_64,ne>(Mx3);
x4 = float<ieee_64,ne>(Mx4);
x5 = float<ieee_64,ne>(Mx5);
x2 = float<ieee_64,ne>(Mx2);
x9 = float<ieee_64,ne>(Mx9);
x8 = float<ieee_64,ne>(Mx8);
x10 = float<ieee_64,ne>(Mx10);
x7 = float<ieee_64,ne>(Mx7);
x6 = float<ieee_64,ne>(Mx6);
x1 = float<ieee_64,ne>(Mx11);

Mex0 = (((((((((((48 * Mx11) + (42 * Mx2)) + (48 * Mx3)) + (45 * Mx4)) + (44 * Mx5)) + (41 * Mx6)) + (47 * Mx7)) + (42 * Mx8)) + (45 * Mx9)) + (46 * Mx10)) - (50 * ((((((((((Mx11 * Mx11) + (Mx2 * Mx2)) + (Mx3 * Mx3)) + (Mx4 * Mx4)) + (Mx5 * Mx5)) + (Mx6 * Mx6)) + (Mx7 * Mx7)) + (Mx8 * Mx8)) + (Mx9 * Mx9)) + (Mx10 * Mx10))));

ex0 float<ieee_64,ne>= (((((((((((float<ieee_64,ne>(48) * x1) + (float<ieee_64,ne>(42) * x2)) + (float<ieee_64,ne>(48) * x3)) + (float<ieee_64,ne>(45) * x4)) + (float<ieee_64,ne>(44) * x5)) + (float<ieee_64,ne>(41) * x6)) + (float<ieee_64,ne>(47) * x7)) + (float<ieee_64,ne>(42) * x8)) + (float<ieee_64,ne>(45) * x9)) + (float<ieee_64,ne>(46) * x10)) - (float<ieee_64,ne>(50) * ((((((((((x1 * x1) + (x2 * x2)) + (x3 * x3)) + (x4 * x4)) + (x5 * x5)) + (x6 * x6)) + (x7 * x7)) + (x8 * x8)) + (x9 * x9)) + (x10 * x10))));

{ (((((((((((Mx9 >= 0) /\ (Mx9 <= 1)) /\ ((Mx10 >= 0) /\ (Mx10 <= 1))) /\ ((Mx4 >= 0) /\ (Mx4 <= 1))) /\ ((Mx3 >= 0) /\ (Mx3 <= 1))) /\ ((Mx5 >= 0) /\ (Mx5 <= 1))) /\ ((Mx11 >= 0) /\ (Mx11 <= 1))) /\ ((Mx6 >= 0) /\ (Mx6 <= 1))) /\ ((Mx8 >= 0) /\ (Mx8 <= 1))) /\ ((Mx2 >= 0) /\ (Mx2 <= 1))) /\ ((Mx7 >= 0) /\ (Mx7 <= 1))) /\ (Mx3 in [0, 1] /\ Mx4 in [0, 1] /\ Mx5 in [0, 1] /\ Mx2 in [0, 1] /\ Mx9 in [0, 1] /\ Mx8 in [0, 1] /\ Mx10 in [0, 1] /\ Mx7 in [0, 1] /\ Mx6 in [0, 1] /\ Mx11 in [0, 1])
  -> |ex0 - Mex0| in ? }
