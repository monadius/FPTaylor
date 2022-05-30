u = float<ieee_64,ne>(Mu);

Mex1 = ((((((3 * Mu) * Mu) * Mu) - ((6 * Mu) * Mu)) + 4) / 6);

ex1 float<ieee_64,ne>= ((((((float<ieee_64,ne>(3) * u) * u) * u) - ((float<ieee_64,ne>(6) * u) * u)) + float<ieee_64,ne>(4)) / float<ieee_64,ne>(6));

{ ((Mu >= 0) /\ (Mu <= 1)) /\ (Mu in [0, 1])
  -> |ex1 - Mex1| in ? }
