u = float<ieee_64,ne>(Mu);

Mex0 = ((((1 - Mu) * (1 - Mu)) * (1 - Mu)) / 6);

ex0 float<ieee_64,ne>= ((((float<ieee_64,ne>(1) - u) * (float<ieee_64,ne>(1) - u)) * (float<ieee_64,ne>(1) - u)) / float<ieee_64,ne>(6));

{ ((Mu >= 0) /\ (Mu <= 1)) /\ (Mu in [0, 1])
  -> |ex0 - Mex0| in ? }
