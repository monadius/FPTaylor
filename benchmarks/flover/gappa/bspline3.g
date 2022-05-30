u = float<ieee_64,ne>(Mu);

Mex3 = (((-Mu * Mu) * Mu) / 6);

ex3 float<ieee_64,ne>= (((-u * u) * u) / float<ieee_64,ne>(6));

{ ((Mu >= 0) /\ (Mu <= 1)) /\ (Mu in [0, 1])
  -> |ex3 - Mex3| in ? }
