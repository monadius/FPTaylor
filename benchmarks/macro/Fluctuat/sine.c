int main(void)
{
  double x;
  double res;

  x = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-1.57079632679, 1.57079632679);

  res = x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
  DSENSITIVITY(res);

  return 0;
}
