u = float<ieee_64,ne>(Mu);

Mex2 = (((((((-3 * Mu) * Mu) * Mu) + ((3 * Mu) * Mu)) + (3 * Mu)) + 1) / 6);

ex2 float<ieee_64,ne>= (((((((-float<ieee_64,ne>(3) * u) * u) * u) + ((float<ieee_64,ne>(3) * u) * u)) + (float<ieee_64,ne>(3) * u)) + float<ieee_64,ne>(1)) / float<ieee_64,ne>(6));

{ ((Mu >= 0) /\ (Mu <= 1)) /\ (Mu in [0, 1])
  -> |ex2 - Mex2| in ? }
