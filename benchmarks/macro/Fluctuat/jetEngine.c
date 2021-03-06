int main(void)
{
  double x1, x2;
  double t, res;

  x1 = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-5.0, 5.0);
  x2 = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-20.0, 5.0);

  t = (3*x1*x1 + 2*x2 - x1);

  res = x1 + ((2*x1*(t/(x1*x1 + 1)) *
	       (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
	      (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
	      3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));
  DSENSITIVITY(res);

  return 0;
}
