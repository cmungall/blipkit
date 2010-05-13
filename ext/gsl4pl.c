#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>
#include <gsl/gsl_sf_bessel.h>

int
main (void)
{
  double x = 5.0;
  double y = gsl_sf_bessel_J0 (x);
  printf ("J0(%g) = %.18e\n", x, y);
  return 0;
}

static foreign_t
pl_gsl_sf_bessel_J0(term_t x_term, term_t out_term)
{ 
  double x;
  double y;
  int rval;

  if ( PL_get_float(x_term, &x) )
    { 
      y = gsl_sf_bessel_j0(x);
      rval = PL_unify_float(out_term, y);
      return rval;
    }

  PL_fail;
}

install_t
install()
{ 
  PL_register_foreign("gsl_sf_bessel_J0", 2, pl_gsl_sf_bessel_J0, 0);
}
