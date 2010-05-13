#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>
#include <squid.h>

static foreign_t
pl_revcomp(term_t seq_term, term_t rev_term)
{ 
  char *seq;
  char *rev;
  int rval;

  if ( PL_get_atom_chars(seq_term, &seq) )
    { 
      if ((rev = (char *) PL_malloc ((strlen(seq) + 1) * sizeof(char))) == NULL)
        Die("malloc failed");
      revcomp(rev,seq);
      rval = PL_unify_atom_chars(rev_term, rev);
      PL_free(rev);
      return rval;
    }

  PL_fail;
}


static foreign_t
pl_seqh_new (term_t handle_term, term_t file_term)
{
  char *file;
  SQFILE *dbfp;

  if ( ! PL_get_atom_chars(file_term, &file) ) 
    {
      PL_fail;
    }

  if ((dbfp = SeqfileOpen(file, SQFILE_UNKNOWN, NULL)) == NULL)
    {
      term_t except = PL_new_term_ref();
      PL_unify_term(except,
                    PL_FUNCTOR_CHARS, "file_error", 2,
                    PL_CHARS, "cannot_open",
                    PL_TERM, file_term);
      
      return PL_raise_exception(except);
    }
  
  return PL_unify_integer(handle_term, (long)dbfp);
}

static foreign_t
pl_seqh_close (term_t handle_term) {
  long l;
  SQFILE *dbfp;

  PL_get_long(handle_term, &l);
  dbfp = (SQFILE*)l;
  SeqfileClose(dbfp);  
  PL_succeed;
}

static foreign_t
pl_seqh_read (term_t handle_term, term_t seq_term) {
  long l;
  SQFILE *dbfp;
  SQINFO sqinfo;
  char *seq;
  int rval;

  PL_get_long(handle_term, &l);
  dbfp = (SQFILE*)l;
  if (ReadSeq(dbfp, dbfp->format, &seq, &sqinfo)) {
    rval = 
      PL_unify_term(seq_term,
                    PL_FUNCTOR_CHARS, "seq", 4,
                    PL_CHARS, sqinfo.name,
                    PL_CHARS, sqinfo.desc,
                    PL_INT, sqinfo.len,
                    PL_CHARS, seq);
    //rval = PL_unify_atom_chars(seq_term, seq);
    FreeSequence(seq, &sqinfo);
    return rval;
  }
  else {
    PL_fail;
  }
}


install_t
install()
{ 
  PL_register_foreign("revcomp", 2, pl_revcomp, 0);
  PL_register_foreign("seqh_new", 2, pl_seqh_new, 0);
  PL_register_foreign("seqh_close", 1, pl_seqh_close, 0);
  PL_register_foreign("seqh_read", 2, pl_seqh_read, 0);
}
