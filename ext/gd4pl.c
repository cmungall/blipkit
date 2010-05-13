#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>
#include <gd.h>

static foreign_t
pl_gdImageCreate(term_t im_term, term_t x_term, term_t y_term)
{ 
  int x;
  int y;
  gdImagePtr im;
  int rval;
  int black;
  int white;

  if ( PL_get_integer(x_term, &x) && PL_get_integer(y_term, &y) )
    { 
      im = gdImageCreate(x,y);
      black = gdImageColorAllocate(im, 0, 0, 0);
      white = gdImageColorAllocate(im, 255, 255, 255);
      rval = PL_unify_pointer(im_term, im);
      return rval;
    }

  PL_fail;
}

static foreign_t
pl_gdImagePng(term_t im_term, term_t filename_term)
{ 
  gdImagePtr im;
  void *im_ptr;
  char *filename;
  FILE *out;

  if ( PL_get_atom_chars(filename_term, &filename) && PL_get_pointer(im_term, &im_ptr) )
    { 
      im = (gdImagePtr)im_ptr;
      out = fopen(filename,"wb");
      if (out) {
        gdImagePng(im,out);
        PL_succeed;
      }
    }

  PL_fail;
}

install_t
install()
{ 
  PL_register_foreign("gdImageCreate", 3, pl_gdImageCreate, 0);
  PL_register_foreign("gdImagePng", 2, pl_gdImagePng, 0);
}
