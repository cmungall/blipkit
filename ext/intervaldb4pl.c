#include <stdio.h>

#define BUILD_C_LIBRARY
#include "intervaldb.h"
#include <SWI-Prolog.h>

#define MAX_MATCHES 1024  // the size of the match buffer used to build NCL
#define IDB -999 // this identifies the IntervalDB as an IDB in iDB->ntop


// type and struct definitions

// structs necessary to create a spandb
struct span{int start; int end; int id; int id_start; int id_end; struct span *next;};
typedef struct {struct span first; int n; struct span *last;}spandb;

// structs necessary to create a spandb2
struct span2{int start; int end; char* id; int id_start; int id_end; struct span2 *next;};
typedef struct {struct span2 first; int n; struct span2 *last;}spandb2;

// general context structure used in notdet intervadb_match
typedef struct                  // define a context structure 
{ 
  IntervalDB *iDB;
  IntervalMap *iMap;
  int pos;
} context;

//  context structure used in notdet intervadb_match with spandb2
typedef struct                  // define a context structure 
{ 
  IntervalDB *iDB;
  IntervalMap *iMap;
  int pos; 
  char **names;
} context2;


// A set of predicates to manipulate a static x
static int x=0;

foreign_t pl_get_x(term_t t)
{
  PL_unify_integer(t,x);

  PL_succeed;
}

foreign_t pl_set_x(term_t t)
{
  PL_get_integer(t,&x);

  PL_succeed;
}

foreign_t pl_inc_x()
{
  x++;

  PL_succeed;
}

foreign_t pl_dec_x()
{
  x--;

  PL_succeed;
}


// start of intervaldb set of predicates to make and manipulates IDBs

// makes an IDB of size num
foreign_t pl_intervaldb_make(term_t num, term_t idb)
{
  IntervalDB *iDB;
  int n,rval;

  Sprintf("intervaldb_make\n"); 

  PL_get_integer(num,&n);

  if ((iDB = malloc(sizeof(IntervalDB))))
    {
      // get a new IntervalMap
      if (n > 0)
	{
	if (!(iDB->im = interval_map_alloc(n)))
	  {
	    free(iDB);
	    PL_fail;
	  }
	}
      else
	iDB->im =  0;
    }
  else
    PL_fail;

  iDB->ntop = IDB; // to identify this as an IDB
  iDB->n = n;
  iDB->subheader = NULL;

  rval = PL_unify_pointer(idb,iDB);

  if (rval==FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}

// prints out an IDB
foreign_t pl_intervaldb_print(term_t idb)
{
  IntervalDB *iDB;
  int x;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_print/1: instantiation fault");

  for(x=0; x<iDB->n; x++)
    Sprintf("%d %d %d %d %d\n",iDB->im[x].start,iDB->im[x].end,iDB->im[x].target_id,iDB->im[x].target_start,iDB->im[x].target_end);

  Sprintf("n:%d ntop:%d nlists:%d\n",iDB->n,iDB->ntop,iDB->nlists);

  PL_succeed;
}

// one line of info on an IDB
foreign_t pl_intervaldb_info(term_t idb)
{
  IntervalDB *iDB;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_info/1: instantiation fault");

  Sprintf("num:%d ntop:%d nlists:%d\n",iDB->n,iDB->ntop,iDB->nlists);

  PL_succeed;
}

// adds a new interval to an IDB at position index
foreign_t pl_intervaldb_addone(term_t idb, term_t index, term_t s, term_t e)
{
  IntervalDB *iDB;
  IntervalMap *iMap;
  int n,x,y;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_addone/4: instantiation fault");
  if ( !PL_get_integer(index, &n) )
    return PL_warning("pl_intervaldb_addone/4: instantiation fault");
  if ( !PL_get_integer(s, &x) )
    return PL_warning("pl_intervaldb_addone/4: instantiation fault");
  if ( !PL_get_integer(e, &y) )
    return PL_warning("pl_intervaldb_addone/4: instantiation fault");

  // these are required to make sure we don't get a segmentation fault
  if (iDB->n == n)
    return PL_warning("pl_intervaldb_addone/4: index > IntervalDB size");
  if (iDB->n < n)
    PL_fail;

  iMap = &iDB->im[n];
 
  iMap->start = x;
  iMap->end = y;
  iMap->sublist = -1;

  PL_succeed;
}

// builds an NCL from an IDB
foreign_t pl_nclist_build(term_t idb, term_t ncl)
{
  IntervalDB *iDB;
  IntervalDB *iNCL;
  IntervalMap *pMap, *qMap;
  int n, rval = TRUE;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_nclist_build/2: instantiation fault");

  if (iDB->n<=0) // check we have a populated IDB
    PL_fail;

  if (!(iNCL = malloc(sizeof(IntervalDB))))
    PL_fail;
  // get a new IntervalMap
  if (!(iNCL->im = interval_map_alloc(iDB->n)))
    {
      free(iNCL);
      PL_fail;
    }
  iDB->subheader = NULL;

  iNCL->n = iDB->n;

  pMap = iNCL->im;
  qMap = iDB->im;
  for (n=0; n< iNCL->n; n++)
    *pMap++ = *qMap++;

  iNCL->subheader = build_nested_list(iNCL->im,iNCL->n,&iNCL->ntop,&iNCL->nlists);

  rval = PL_unify_pointer(ncl,iNCL);

  if (rval==FALSE) 
    {
      free(iDB->im);
      free(iDB);
    }
  
  return rval;
}

// C function to do an intersection on an NCL
IntervalDB *nclist_intersection(IntervalDB *iDB, int start, int end)
{
  IntervalDB *iMDB;
  IntervalIterator *int_it;
  IntervalIterator *int_it_alloc;
  IntervalMap im_buf[MAX_MATCHES];
  IntervalMap *pMap, *qMap;
  int n = 0;
  int nhit;
  int n_match = 0;

  Sprintf("NCL Intersection %d-%d\n", start, end);

  if (!(iMDB = malloc(sizeof(IntervalDB))))
    PL_fail;
  if (!(iMDB->im  = interval_map_alloc(iDB->n)))
    {
      free(iMDB);
      PL_fail;
    }
  iMDB->subheader = NULL;
  pMap = iMDB->im;
 
  // first we want to get an iterator
  int_it = interval_iterator_alloc();
  int_it_alloc = int_it;
  
  while(int_it)
    {
    find_intervals(int_it, start, end, iDB->im, iDB->ntop,
		   iDB->subheader, iDB->nlists, im_buf, MAX_MATCHES,
		   &nhit, &int_it);
    qMap = im_buf;
    for (n=0; n < nhit; n++)
      {
	//	Sprintf("Found: %d-%d\n",qMap->start,qMap->end);
	*pMap++ = *qMap++;
	n_match++;
      }
    }

  // think we need to free the interval iterator
  free_interval_iterator(int_it_alloc);

  Sprintf("Intersections:%d\n",n_match);

  iMDB->n = n_match;
  iMDB->ntop = IDB; // to identify this as an IDB

  return iMDB;
}

// intersection on an NCL unifying an MDB (which is an IDB)
foreign_t pl_nclist_intersection(term_t idb, term_t s, term_t e, term_t mdb)
{
  IntervalDB *iDB, *iMDB;
  IntervalIterator *int_it;
  IntervalIterator *int_it_alloc;
  IntervalMap im_buf[MAX_MATCHES];
  IntervalMap *pMap, *qMap;
  int n = 0;
  int nhit;
  int n_match = 0;
  int start = 0, end = 0;
  int rval;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_nclist_intersection/4: instantiation fault");
  if ( !PL_get_integer(s, &start) )
    return PL_warning("pl_nclist_intersection/4: instantiation fault");
  if ( !PL_get_integer(e, &end) )
    return PL_warning("pl_nclist_intersection/4: instantiation fault");
  Sprintf("NCL Intersection %d-%d\n", start, end);

  if (iDB->n<=0) // check we have a populated IDB
    PL_fail;

  if (!(iMDB = malloc(sizeof(IntervalDB))))
    PL_fail;
  if (!(iMDB->im  = interval_map_alloc(iDB->n)))
    {
      free(iMDB);
      PL_fail;
    }
  iMDB->subheader = NULL;
  pMap = iMDB->im;
 
  // first we want to get an iterator
  int_it = interval_iterator_alloc();
  int_it_alloc = int_it;
  
  while(int_it)
    {
    find_intervals(int_it, start, end, iDB->im, iDB->ntop,
		   iDB->subheader, iDB->nlists, im_buf, MAX_MATCHES,
		   &nhit, &int_it);
    qMap = im_buf;
    for (n=0; n < nhit; n++)
      {
	//	Sprintf("Found: %d-%d\n",qMap->start,qMap->end);
	*pMap++ = *qMap++;
	n_match++;
      }
    }

  // think we need to free the interval iterator
  free_interval_iterator(int_it_alloc);

  Sprintf("Intersections:%d\n",n_match);

  iMDB->n = n_match;
  iMDB->ntop = IDB; // to identify this as an IDB

  rval = PL_unify_pointer(mdb,iMDB);

  if (rval == FALSE)
    {
    free(iMDB->im);
    free(iMDB);
    }

  return rval;

  /* ORIGINAL PYTHON CODE I USED TO CREATE ABOVE
  def find_overlap_list(self,int start,int end):
    cdef int i,nhit
    cdef IntervalIterator *it,*it_alloc
    cdef IntervalMap im_buf[1024]
    self.check_nonempty() # RAISE EXCEPTION IF NO DATA
    it=interval_iterator_alloc()
    it_alloc=it
    l=[] # LIST OF RESULTS TO HAND BACK
    while it:
      find_intervals(it,start,end,self.im,self.ntop,
                     self.subheader,self.nlists,im_buf,1024,
                     &(nhit),&(it)) # GET NEXT BUFFER CHUNK
      for i from 0 <= i < nhit:
        l.append((im_buf[i].start,im_buf[i].end,im_buf[i].target_id,im_buf[i].target_start,im_buf[i].target_end))
    free_interval_iterator(it_alloc)
    return l
  */    
}

// does an intersection and returns a list of span(int, int, int)
foreign_t pl_nclist_intersection2(term_t idb, term_t s, term_t e, term_t lspan)
{
  IntervalDB *iDB; 
  IntervalIterator *int_it;
  IntervalIterator *int_it_alloc;
  IntervalMap im_buf[MAX_MATCHES];
  IntervalMap *qMap;
  int n = 0;
  int nhit;
  int n_match = 0;
  int start = 0, end = 0;
  term_t l = PL_copy_term_ref(lspan);
  term_t h = PL_new_term_ref();

  functor_t span;
  span = PL_new_functor(PL_new_atom("span"), 3);

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_nclist_intersection2/4: instantiation fault");
  if ( !PL_get_integer(s, &start) )
    return PL_warning("pl_nclist_intersection2/4: instantiation fault");
  if ( !PL_get_integer(e, &end) )
    return PL_warning("pl_nclist_intersection2/4: instantiation fault");
  Sprintf("Intersection %d-%d\n", start, end);

  if (iDB->n<=0) // check we have a populated IDB
    PL_fail;

  // first we want to get an iterator
  int_it = interval_iterator_alloc();
  int_it_alloc = int_it;
  
  while(int_it)
    {
    find_intervals(int_it, start, end, iDB->im, iDB->ntop,
		   iDB->subheader, iDB->nlists, im_buf, MAX_MATCHES,
		   &nhit, &int_it);
    qMap = im_buf;
    for (n=0; n < nhit; n++)
      {
	//	Sprintf("Found: %d-%d\n",qMap->start,qMap->end);
	//	*pMap++ = *qMap++;
	if (PL_unify_list(l, h, l))
	  {
	    PL_unify_term(h, PL_FUNCTOR, span,
			  PL_INT, qMap->target_id,
		 	  PL_INT, qMap->start,
                          PL_INT, qMap->end);
	  }
	else
	  PL_fail;
      qMap++;
      n_match++;
      }
    }

  // think we need to free the interval iterator
  free_interval_iterator(int_it_alloc);

  Sprintf("Intersections:%d\n",n_match);

  return PL_unify_nil(l);
}

// C function to do an intersection on an IDB
IntervalDB *intervaldb_intersection(IntervalDB *iDB, int start, int end)
{
  IntervalDB *iMDB;
  IntervalMap *pMap, *qMap;
  int n = 0;
  int n_match = 0;

  Sprintf("Intersection %d-%d\n", start, end);

  if (!(iMDB = malloc(sizeof(IntervalDB))))
    return 0;
  if (!(iMDB->im  = interval_map_alloc(iDB->n)))
    {
      free(iMDB);
      return 0;
    }
  iMDB->subheader = NULL;
  pMap = iMDB->im;
  qMap = iDB->im;
 
  for (n=0; n<iDB->n; n++)
    {
      if (
	  (qMap->end > start && qMap->end < end) ||
	  (qMap->start < end  && qMap->end >= end ) || 
	  (qMap->start >= start && qMap->end < end) ||
	  (qMap->start <= start && qMap->end >= end) 
	  )
	{   
	  *pMap++ = *qMap;
	  n_match++;
	}
      qMap++;
    }

  Sprintf("Intersections:%d\n",n_match);

  iMDB->n = n_match;

  return iMDB;
}

// intersection on an IDB unifying to an MDB
foreign_t pl_intervaldb_intersection(term_t idb, term_t s, term_t e, term_t mdb)
{
  IntervalDB *iDB, *iMDB;
  IntervalMap *pMap, *qMap;
  int n = 0;
  int n_match = 0;
  int start = 0, end = 0;
  int rval;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  if ( !PL_get_integer(s, &start) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  if ( !PL_get_integer(e, &end) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  Sprintf("Intersection %d-%d\n", start, end);

  if (iDB->n<=0) // check we have a populated IDB
    PL_fail;

  if (!(iMDB = malloc(sizeof(IntervalDB))))
    PL_fail;
  if (!(iMDB->im  = interval_map_alloc(iDB->n)))
    {
      free(iMDB);
      PL_fail;
    }
  iMDB->subheader = NULL;
  iMDB->ntop = IDB;
  pMap = iMDB->im;
  qMap = iDB->im;
 
  for (n=0; n<iDB->n; n++)
    {
      //if (start >= qMap->start && end <= qMap->end)
      if (
	  (qMap->end > start && qMap->end < end) ||
	  (qMap->start < end  && qMap->end >= end ) || 
	  (qMap->start >= start && qMap->end < end) ||
	  (qMap->start <= start && qMap->end >= end) 
	  )
	{   
	  *pMap++ = *qMap;
	  n_match++;
	}
      qMap++;
    }

  Sprintf("Intersections:%d\n",n_match);

  iMDB->n = n_match;

  rval = PL_unify_pointer(mdb,iMDB);

  if (rval == FALSE)
    {
    free(iMDB->im);
    free(iMDB);
    }

  return rval;
}

// intersection on an IDB or an NCL unifying to an MDB
foreign_t pl_intervaldb_intersection2(term_t idb, term_t s, term_t e, term_t mdb)
{
  IntervalDB *iDB, *iMDB;
  int start = 0, end = 0;
  int rval;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  if ( !PL_get_integer(s, &start) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  if ( !PL_get_integer(e, &end) )
    return PL_warning("pl_intervaldb_intersection/4: instantiation fault");
  //  Sprintf("Intersection %d-%d\n", start, end);

  if (iDB->n<=0) // check we have a populated IDB
    PL_fail;

  if (iDB->ntop == IDB)  // check whether IDB or NCL
    {
      if (!(iMDB = intervaldb_intersection(iDB,start,end)))
	{
	  PL_fail;
	}
    }
  else
    {
      if (!(iMDB = nclist_intersection(iDB,start,end)))
	{
	  PL_fail;
	}
    }

  Sprintf("Intersection2: ");
  iMDB->ntop = IDB;

  rval = PL_unify_pointer(mdb,iMDB);

  if (rval == FALSE)
    {
    free(iMDB->im);
    free(iMDB);
    }

  return rval;
}

/* LEFT HERE COMMENTED FOR CONVENIENCE
typedef struct                  // define a context structure 
{ 
  IntervalDB *iDB;
  IntervalMap *iMap;
  int pos;
} context;
*/

// non-deterministic function/predicate
// given an MDB (an IDB) unifies each succesive interval with start and end 
foreign_t pl_intervaldb_match(term_t matchdb, term_t start, term_t end, control_t handle)
{
  context *ctxt = NULL;
  IntervalDB *iMatch;

  switch( PL_foreign_control(handle) )
    { 
    case PL_FIRST_CALL:
      if ( !PL_get_pointer(matchdb, (void*)&iMatch) )
	return PL_warning("pl_intervaldb_match/3: instantiation fault");
      if (!(ctxt = malloc(sizeof(context))))
	PL_fail;
      ctxt->iDB = iMatch;
      ctxt->iDB->n = iMatch->n;
      ctxt->iMap = iMatch->im;
      //      Sprintf("FIRST_CALL: %d matches\n",ctxt->iDB->n);
      ctxt->pos = 0;
      if (ctxt->iDB->n <= 0)
	break;
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,*iMap[ctxt->pos].start,matches[ctxt->pos].end);  
      //      if(!PL_unify_integer(start, ctxt->iDB->im[ctxt->pos].start))
      //      	break;
      //if(!PL_unify_integer(end, ctxt->iDB->im[ctxt->pos].end))
      //      break;
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      if (ctxt->pos >= ctxt->iDB->n)
	break;
      //      Sprintf("REDO: %d matches\n",ctxt->iDB->n);
      //Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,matches[ctxt->pos].start,matches[ctxt->pos].end);  
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_CUTTED: 
      ctxt = PL_foreign_context_address(handle);
      // clear up the memory we've grabbed
      //      free(ctxt->iDB->im);
      //      free(ctxt->iDB);
      free(ctxt);
      PL_succeed;
    }

  // failed, so clear up the memory we've grabbed
  //  free(ctxt->iDB->im);
  //  free(ctxt->iDB);
  free(ctxt);
  PL_fail;
}

// non-deterministic function/predicate
// given an IDB or NCL does an intersection on FIRST_CALL
// and then unifies each succesive REDO with start and end of the MDB 
foreign_t pl_intervaldb_match2(term_t idb, term_t rx, term_t ry, term_t start, term_t end, control_t handle)
{
  context *ctxt = NULL;
  IntervalDB *iDB;
  int x,y;

  switch( PL_foreign_control(handle) )
    { 
    case PL_FIRST_CALL:
      if ( !PL_get_pointer(idb, (void*)&iDB) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");
      if ( !PL_get_integer(rx, &x) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");
      if ( !PL_get_integer(ry, &y) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");

      if (!(ctxt = malloc(sizeof(context))))
	break;
      //      if (!(ctxt->iDB = intervaldb_intersection(iDB,x,y)))
      Sprintf("FIRST_CALL: ntop=%d\n",iDB->ntop);
      if (iDB->ntop == IDB)  // check whether IDB or NCL
	{
	if (!(ctxt->iDB = intervaldb_intersection(iDB,x,y)))
	  {
	    free(ctxt);
	    PL_fail;
	  }
	}
      else
	{
	if (!(ctxt->iDB = nclist_intersection(iDB,x,y)))
	  {
	    free(ctxt);
	    PL_fail;
	  }
	}

      Sprintf("FIRST_CALL: %d matches\n",ctxt->iDB->n);

      //      ctxt->iDB->n = iMatch->n;
      ctxt->iMap = ctxt->iDB->im;
      ctxt->pos = 0;
      if (ctxt->iDB->n <= 0)
	break;
      Sprintf("FIRST_CALL: %d matches\n",ctxt->iDB->n);
      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      if (ctxt->pos >= ctxt->iDB->n)
	break;
      //      Sprintf("REDO: %d matches\n",ctxt->iDB->n);
      //Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,matches[ctxt->pos].start,matches[ctxt->pos].end);  
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_CUTTED: 
      ctxt = PL_foreign_context_address(handle);
      // clear up the memory we've grabbed
      free(ctxt->iDB->im);
      free(ctxt->iDB);
      free(ctxt);
      PL_succeed;
    }

  Sprintf("Interval Match: %d matches\n",ctxt->iDB->n);

  // failed, so clear up the memory we've grabbed
  free(ctxt->iDB->im);
  free(ctxt->iDB);
  free(ctxt);
  PL_fail;
}

// non-deterministic function/predicate
// given an MDB (an IDB) unifies each succesive interval with start, end and id 
foreign_t pl_intervaldb_match3(term_t matchdb, term_t start, term_t end, term_t id, control_t handle)
{
  context *ctxt = NULL;
  IntervalDB *iMatch;

  switch( PL_foreign_control(handle) )
    { 
    case PL_FIRST_CALL:
      if ( !PL_get_pointer(matchdb, (void*)&iMatch) )
	return PL_warning("pl_intervaldb_match/3: instantiation fault");
      if (!(ctxt = malloc(sizeof(context))))
	PL_fail;
      ctxt->iDB = iMatch;
      ctxt->iDB->n = iMatch->n;
      ctxt->iMap = iMatch->im;
      //      Sprintf("FIRST_CALL: %d matches\n",ctxt->iDB->n);
      ctxt->pos = 0;
      if (ctxt->iDB->n <= 0)
	break;
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id, ctxt->iMap->target_id))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      if (ctxt->pos >= ctxt->iDB->n)
	break;
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id, ctxt->iMap->target_id))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_CUTTED: 
      ctxt = PL_foreign_context_address(handle);
      // clear up the memory we've grabbed
      //      free(ctxt->iDB->im);
      //      free(ctxt->iDB);
      free(ctxt);
      PL_succeed;
    }

  // failed, so clear up the memory we've grabbed
  //  free(ctxt->iDB->im);
  //  free(ctxt->iDB);
  free(ctxt);
  PL_fail;
}

/* LEFT HERE COMMENTED FOR CONVENIENCE
typedef struct                  // define a context structure 
{ 
  IntervalDB *iDB;
  IntervalMap *iMap;
  int pos; 
  char **names;
} context2;
*/

// non-deterministic function/predicate
// given an MDB (an IDB) unifies each succesive interval with start, end and id
foreign_t pl_intervaldb_match4(term_t matchdb, term_t spandb, term_t start, term_t end, term_t id, term_t id_start, term_t id_end, control_t handle)
{
  context2 *ctxt=NULL;
  IntervalDB *iMatch;
  spandb2 *iSpandb;
  struct span2 *pSpan;
  char **cur_name;
  int x=0;

  switch( PL_foreign_control(handle) )
    { 
    case PL_FIRST_CALL:
      // get arguments
      if ( !PL_get_pointer(matchdb, (void*)&iMatch) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");
      if ( !PL_get_pointer(spandb, (void*)&iSpandb) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");

      // grab memory
      if (!(ctxt = malloc(sizeof(context2))))
	PL_fail;
      if (!(ctxt->names = malloc(sizeof(char*)*(iSpandb->n))))
	PL_fail;
      
      // populate ctxt
      pSpan = &(iSpandb->first);
      cur_name = ctxt->names;
      for (x=0; x<iSpandb->n; x++)
	{
	  //	  Sprintf("FIRST_CALL0: %s\n",pSpan->id);
	  *cur_name = pSpan->id;
	  cur_name++;
	  pSpan = pSpan->next;
	}
      ctxt->iDB = iMatch;
      ctxt->iDB->n = iMatch->n;
      ctxt->iMap = iMatch->im;
      ctxt->pos = 0;
      //      Sprintf("FIRST_CALL1: %d matches %s\n",ctxt->iDB->n, ctxt->cur_span->id);

      // unify reults
      if (ctxt->iDB->n <= 0)
	break;
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id_start, ctxt->iMap->target_start))
      	break;
      if(!PL_unify_integer(id_end, ctxt->iMap->target_end))
	break;
      if(!PL_unify_atom_chars(id, ctxt->names[ctxt->iMap->target_id]))
	break;

      // get ready for REDO
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      if (ctxt->pos >= ctxt->iDB->n)
	break;
      //      Sprintf("REDO: %d matches\n",ctxt->iDB->n);
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id_start, ctxt->iMap->target_start))
      	break;
      if(!PL_unify_integer(id_end, ctxt->iMap->target_end))
	break;
      if(!PL_unify_atom_chars(id, ctxt->names[ctxt->iMap->target_id]))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      //      Sprintf("REDO2: last_id %d\n",ctxt->last_id);
      PL_retry_address(ctxt);

    case PL_CUTTED: 
      ctxt = PL_foreign_context_address(handle);
      //      Sprintf("CUTTED: %d matches\n",ctxt->iDB->n);
      // clear up the memory we've grabbed
      //      free(ctxt->iDB->im);
      //      free(ctxt->iDB);
      free(ctxt);
      PL_succeed;
    }

  //  Sprintf("FAILED: %d matches\n",ctxt->pos);
  // failed, so clear up the memory we've grabbed
  //  free(ctxt->iDB->im);
  //  free(ctxt->iDB);
  free(ctxt);
  PL_fail;
}

// non-deterministic function/predicate
// given an MDB (an IDB) unifies each succesive interval with start, end and id
foreign_t pl_intervaldb_match5(term_t matchdb, term_t ndb, term_t start, term_t end, term_t id, term_t id_start, term_t id_end, control_t handle)
{
  context2 *ctxt=NULL;
  IntervalDB *iMatch;
  char **namedb;

  switch( PL_foreign_control(handle) )
    { 
    case PL_FIRST_CALL:
      // get arguments
      if ( !PL_get_pointer(matchdb, (void*)&iMatch) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");
      if ( !PL_get_pointer(ndb, (void*)&namedb) )
	return PL_warning("pl_intervaldb_match/5: instantiation fault");

      // grab memory
      if (!(ctxt = malloc(sizeof(context2))))
	PL_fail;
      
      // populate ctxt
      ctxt->names = namedb;
      ctxt->iDB = iMatch;
      ctxt->iDB->n = iMatch->n;
      ctxt->iMap = iMatch->im;
      ctxt->pos = 0;
      //      Sprintf("FIRST_CALL1: %d matches %s\n",ctxt->iDB->n, ctxt->cur_span->id);

      // unify reults
      if (ctxt->iDB->n <= 0)
	break;
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id_start, ctxt->iMap->target_start))
      	break;
      if(!PL_unify_integer(id_end, ctxt->iMap->target_end))
	break;
      if(!PL_unify_atom_chars(id, ctxt->names[ctxt->iMap->target_id]))
	break;

      // get ready for REDO
      ctxt->pos++;
      ctxt->iMap++;
      PL_retry_address(ctxt);

    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      if (ctxt->pos >= ctxt->iDB->n)
	break;
      //      Sprintf("REDO: %d matches\n",ctxt->iDB->n);
      //      Sprintf("Match:#%d/%d %d-%d\n",ctxt->pos,ctxt->iDB->n,ctxt->iMap->start,ctxt->iMap->end);  
      if(!PL_unify_integer(start, ctxt->iMap->start))
      	break;
      if(!PL_unify_integer(end, ctxt->iMap->end))
	break;
      if(!PL_unify_integer(id_start, ctxt->iMap->target_start))
      	break;
      if(!PL_unify_integer(id_end, ctxt->iMap->target_end))
	break;
      if(!PL_unify_atom_chars(id, ctxt->names[ctxt->iMap->target_id]))
	break;
      ctxt->pos++;
      ctxt->iMap++;
      //      Sprintf("REDO2: last_id %d\n",ctxt->last_id);
      PL_retry_address(ctxt);

    case PL_CUTTED: 
      ctxt = PL_foreign_context_address(handle);
      //      Sprintf("CUTTED: %d matches\n",ctxt->iDB->n);
      // clear up the memory we've grabbed
      //      free(ctxt->iDB->im);
      //      free(ctxt->iDB);
      free(ctxt);
      PL_succeed;
    }

  //  Sprintf("FAILED: %d matches\n",ctxt->pos);
  // failed, so clear up the memory we've grabbed
  //  free(ctxt->iDB->im);
  //  free(ctxt->iDB);
  free(ctxt);
  PL_fail;
}

// add a list of span(x,y) to an IDB
foreign_t pl_intervaldb_addall(term_t idb, term_t l)
{
  IntervalDB *iDB;
  int rval;
  int n = 0;
  int new_n;
  term_t head = PL_new_term_ref();      /* variable for the elements */
  term_t list = PL_copy_term_ref(l);    /* copy as we need to write */
  term_t t1 = PL_new_term_ref();
  term_t t2 = PL_new_term_ref();
  IntervalMap *iMap, *pMap, *qMap;

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_addall/2: instantiation fault");

  while( PL_get_list(list, head, list) )
    n++;
  //  Sprintf("Num recs=%d\n",n);
  
  new_n = iDB->n + n;

  // get a new IntervalMap
  if (!(iMap = interval_map_alloc(new_n)))
      PL_fail;

  // copy existing intervals to new IntervalMap array
  int x= 0 ,max = iDB->n; 
  pMap = iMap;
  qMap = iDB->im;
  //  for (x=0; x<iDB->n; pMap++) // done more efficiently with pointers    
    //    iMap[x] = iDB->im[x];
  for (x=0; x<max; x++) 
    *pMap++ = *qMap++;
  //  free(iDB->im); // not sure if we need this, so out
  
  head = PL_new_term_ref();      /* variable for the elements */
  list = PL_copy_term_ref(l);    /* copy as we need to write */

  // should really check for the name of the term cf print_term_list
  while( PL_get_list(list, head, list) )
    { int i1,i2;
      if ( PL_get_arg(1, head, t1) && PL_get_arg(2, head, t2) )
	{
	  if ( PL_get_integer(t1, &i1) && PL_get_integer(t2, &i2) )
	    {
	      //	      Sprintf("Adding %d:%d-%d\n", x,i1,i2);
	      pMap->start = i1;
	      pMap->end = i2;
	      pMap->sublist = -1;
	      x++; pMap++;
	    }
	  else
	    PL_fail;
	}
      else
	PL_fail;
    }

  // need to free up the old IntervalMap before reassigning
  free(iDB->im);
  iDB->im = iMap;
  iDB->n = x;

  rval = PL_unify_pointer(idb,iDB);

  if (rval == FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}

// free up any memory from an IDB
foreign_t pl_intervaldb_free(term_t idb)
{
  IntervalDB *iDB; 

  if ( !PL_get_pointer(idb, (void*)&iDB) )
    return PL_warning("pl_intervaldb_free/1: instantiation fault");

  //  Sprintf("Free SubHeader\n");
  if (iDB->subheader != NULL)
    free(iDB->subheader);
  //  Sprintf("Free iMap\n");
  free(iDB->im);
  //  Sprintf("Free iDB\n");
  free(iDB);

  PL_succeed;
}

// read an IDB from a file given the number of records
foreign_t pl_read_intervals(term_t filename, term_t recs, term_t idb)
{
  char *fname;
  IntervalDB *iDB;
  FILE *ifile;
  int rval;
  int n;

  if ( !PL_get_atom_chars(filename, &fname) )
    return PL_warning("pl_read_intervals/3: instantiation fault");
  if ( !PL_get_integer(recs, &n) )
    return PL_warning("pl_read_intervals/3: instantiation fault");

  Sprintf("Filename: %s  Records: %d\n", fname, n); 

  if (!(iDB = malloc(sizeof(IntervalDB))))
      PL_fail;
  iDB->subheader = NULL;
  iDB->n = n;
  iDB->ntop = IDB; // to identify this as an IDB

  ifile=fopen(fname,"r");
  if (n>0) // read_intervals doesn't work with n=0
    iDB->im = read_intervals(iDB->n, ifile);

  rval = PL_unify_pointer(idb,iDB);

  if (rval == FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}


// predicates to manipulate a spandb
  
/* LEFT HERE COMMENTED FOR CONVENIENCE
// structs necessary to create a spandb
struct span{int start; int end; int id; int id_start; int id_end; struct span *next;};
typedef struct {struct span first; int n; struct span *last;}spandb;
*/

// make an initial spandb to be added to
foreign_t pl_spandb_make(term_t sdb)
{
  spandb *iSDB;
  int rval;

  Sprintf("spandb_make\n"); 

  if (!(iSDB = malloc(sizeof(spandb))))
    PL_fail;
  iSDB->last = &(iSDB->first);
  iSDB->n = 0;

  rval = PL_unify_pointer(sdb,iSDB);

  if (rval==FALSE)
    {
      free(iSDB);
    }

  return rval;
}

// add a new span to a spandb
foreign_t pl_spandb_addone(term_t sdb, term_t s, term_t e, term_t id, term_t id_s, term_t id_e)
{
  spandb *iSDB;
  struct span *pSpan, *qSpan;
  int n;
  int s_s,s_e,s_id,s_id_s,s_id_e;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(s, &s_s) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(e, &s_e) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(id, &s_id) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(id_s, &s_id_s) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(id_e, &s_id_e) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");

  iSDB->last->start = s_s;
  iSDB->last->end = s_e;
  iSDB->last->id = s_id;
  iSDB->last->id_start = s_id_s;
  iSDB->last->id_end = s_id_e;

  if (!(iSDB->last->next = malloc(sizeof(struct span))))
    { // free up memory it we get a fail of malloc
      pSpan = &(iSDB->first);
      pSpan = pSpan->next;
      // start at 1 cos the first span is part of iSDB
      for (n=1; n<iSDB->n; n++)
	{
	  qSpan = pSpan->next;
	  free(pSpan);
	  pSpan = qSpan;      
	}
      free(iSDB);
   }
      
  (iSDB->n)++;

  iSDB->last = iSDB->last->next;

  //  Sprintf("spandb_addone: added %d %d\n", s_s,s_e); 

  PL_succeed;
}

// print out a spandb
foreign_t pl_spandb_print(term_t sdb)
{
  spandb *iSDB;
  struct span *pSpan;
  int n;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_print/1: instantiation fault");

  pSpan = &(iSDB->first);
  
  for (n=0; n<iSDB->n; n++)
    {
      Sprintf("spandb_print2 %d %d %d\n",pSpan->start,pSpan->end,pSpan->id);
      pSpan = pSpan->next;
    }

  PL_succeed;
}

// convert a spandb to an intervaldb
foreign_t pl_spandb_to_intervaldb(term_t sdb, term_t idb)
{
  IntervalDB *iDB;
  spandb *iSDB;
  struct span *pSpan;
  IntervalMap *pMap;
  int n,rval;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_to_intervaldb/2: instantiation fault");

  if ((iDB = malloc(sizeof(IntervalDB))))
    {
      // get a new IntervalMap
      if (iSDB->n > 0)
	{
	if (!(iDB->im = interval_map_alloc(iSDB->n)))
	  {
	    free(iDB);
	    PL_fail;
	  }
	}
      else
	iDB->im =  0;
    }
  else
    PL_fail;

  iDB->ntop = IDB; // to identify this as an IDB
  iDB->n = iSDB->n;
  iDB->subheader = NULL;

  pSpan = &(iSDB->first);
  pMap = iDB->im;
  
  for (n=0; n<iSDB->n; n++)
    {
      //      Sprintf("spandb_to_intervaldb %d %d\n",pSpan->start,pSpan->end);
      pMap->start = pSpan->start;
      pMap->end = pSpan->end;
      pMap->target_id = pSpan->id;
      pMap->target_start = pSpan->id_start;
      pMap->target_end = pSpan->id_end;
      pMap->sublist = -1;
      pMap++;
      pSpan = pSpan->next;
    }

  rval = PL_unify_pointer(idb,iDB);

  if (rval==FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}

// free up all memory used by a spandb
foreign_t pl_spandb_free(term_t sdb)
{
  spandb *iSDB;
  struct span *pSpan, *qSpan;
  int n;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_free/1: instantiation fault");

  pSpan = &(iSDB->first);
  pSpan = pSpan->next;
  // start at 1 cos the first span is part of iSDB
  for (n=1; n<iSDB->n; n++)
    {
      qSpan = pSpan->next;
      free(pSpan);
      pSpan = qSpan;      
    }

  free(iSDB);
      
  Sprintf("spandb_free complete\n");

  PL_succeed;
}

/* LEFT HERE COMMENTED FOR CONVENIENCE
// structs necessary to create a spandb
struct span2{int start; int end; char* id; int id_start; int id_end; struct span2 *next;};
typedef struct {struct span2 first; int n; struct span2 *last;}spandb2;
*/

// make an initial spandb to be added to
foreign_t pl_spandb2_make(term_t sdb)
{
  spandb2 *iSDB;
  int rval;

  Sprintf("spandb_make\n"); 

  if (!(iSDB = malloc(sizeof(spandb2))))
    PL_fail;
  iSDB->last = &(iSDB->first);
  iSDB->n = 0;

  rval = PL_unify_pointer(sdb,iSDB);

  if (rval==FALSE)
    {
      free(iSDB);
    }

  return rval;
}

// add a new span to a spandb
foreign_t pl_spandb2_addone(term_t sdb, term_t s, term_t e, term_t id, term_t id_s, term_t id_e)
{
  spandb2 *iSDB;
  struct span2 *pSpan, *qSpan;
  int n;
  int s_s,s_e,s_id_s,s_id_e;
  char *s_id;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(s, &s_s) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(e, &s_e) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_atom_chars(id, &s_id) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(id_s, &s_id_s) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");
  if ( !PL_get_integer(id_e, &s_id_e) )
    return PL_warning("pl_spandb_addone/6: instantiation fault");

  iSDB->last->start = s_s;
  iSDB->last->end = s_e;
  iSDB->last->id = s_id;
  iSDB->last->id_start = s_id_s;
  iSDB->last->id_end = s_id_e;

  if (!(iSDB->last->next = malloc(sizeof(struct span2))))
    { // free up memory it we get a fail of malloc
      pSpan = &(iSDB->first);
      pSpan = pSpan->next;
      // start at 1 cos the first span is part of iSDB
      for (n=1; n<iSDB->n; n++)
	{
	  qSpan = pSpan->next;
	  free(pSpan);
	  pSpan = qSpan;      
	}
      free(iSDB);
   }
      
  (iSDB->n)++;

  iSDB->last = iSDB->last->next;

  //  Sprintf("spandb_addone: added %d %d\n", s_s,s_e); 

  PL_succeed;
}

// print out a spandb
foreign_t pl_spandb2_print(term_t sdb)
{
  spandb2 *iSDB;
  struct span2 *pSpan;
  int n;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_print/1: instantiation fault");

  pSpan = &(iSDB->first);
  
  for (n=0; n<iSDB->n; n++)
    {
      Sprintf("spandb_print %d %d %s\n",pSpan->start,pSpan->end,pSpan->id);
      pSpan = pSpan->next;
    }

  PL_succeed;
}

// convert a spandb to an intervaldb
foreign_t pl_spandb2_to_intervaldb(term_t sdb, term_t idb)
{
  IntervalDB *iDB;
  spandb2 *iSDB;
  struct span2 *pSpan;
  IntervalMap *pMap;
  int n,rval;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_to_intervaldb/2: instantiation fault");

  if ((iDB = malloc(sizeof(IntervalDB))))
    {
      // get a new IntervalMap
      if (iSDB->n > 0)
	{
	if (!(iDB->im = interval_map_alloc(iSDB->n)))
	  {
	    free(iDB);
	    PL_fail;
	  }
	}
      else
	iDB->im =  0;
    }
  else
    PL_fail;

  iDB->ntop = IDB; // to identify this as an IDB
  iDB->n = iSDB->n;
  iDB->subheader = NULL;

  pSpan = &(iSDB->first);
  pMap = iDB->im;
  
  for (n=0; n<iSDB->n; n++)
    {
      //      Sprintf("spandb_to_intervaldb %d %d\n",pSpan->start,pSpan->end);
      pMap->start = pSpan->start;
      pMap->end = pSpan->end;
      pMap->target_id = n;
      pMap->target_start = pSpan->id_start;
      pMap->target_end = pSpan->id_end;
      pMap->sublist = -1;
      pMap++;
      pSpan = pSpan->next;
    }

  rval = PL_unify_pointer(idb,iDB);

  if (rval==FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}

// convert a spandb to an intervaldb and also returns an array of names
foreign_t pl_spandb2_to_intervaldb2(term_t sdb, term_t idb, term_t ndb)
{
  IntervalDB *iDB;
  spandb2 *iSDB;
  struct span2 *pSpan;
  IntervalMap *pMap;
  int n,rval;
  char **names;
  char **cur_name;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_to_intervaldb2/3: instantiation fault");

  if ((iDB = malloc(sizeof(IntervalDB))))
    {
      // get a new IntervalMap
      if (iSDB->n > 0)
	{
	if (!(iDB->im = interval_map_alloc(iSDB->n)))
	  {
	    free(iDB);
	    PL_fail;
	  }
	}
      else
	iDB->im =  0;
    }
  else
    PL_fail;

  if (!(names = malloc(sizeof(char*)*(iSDB->n))))
    {
      free(iDB->im);
      free(iDB);
      PL_fail;
    }

  iDB->ntop = IDB; // to identify this as an IDB
  iDB->n = iSDB->n;
  iDB->subheader = NULL;

  pSpan = &(iSDB->first);
  pMap = iDB->im;
  
  // set cur_name as a pointer to the current name
  cur_name = names;
  
  for (n=0; n<iSDB->n; n++)
    {
      //      Sprintf("spandb_to_intervaldb %d %d\n",pSpan->start,pSpan->end);
      pMap->start = pSpan->start;
      pMap->end = pSpan->end;
      pMap->target_id = n;
      pMap->target_start = pSpan->id_start;
      pMap->target_end = pSpan->id_end;
      pMap->sublist = -1;
      pMap++;
      *cur_name = pSpan->id;
      cur_name++;
      pSpan = pSpan->next;
    }

  rval = PL_unify_pointer(idb,iDB);

  if (rval==FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  rval = PL_unify_pointer(ndb,names);

  if (rval==FALSE)
    {
    free(iDB->im);
    free(iDB);
    }

  return rval;
}

// free up all memory used by a spandb
foreign_t pl_spandb2_free(term_t sdb)
{
  spandb2 *iSDB;
  struct span2 *pSpan, *qSpan;
  int n;

  if ( !PL_get_pointer(sdb, (void*)&iSDB) )
    return PL_warning("pl_spandb_free/1: instantiation fault");

  pSpan = &(iSDB->first);
  pSpan = pSpan->next;
  // start at 1 cos the first span is part of iSDB
  for (n=1; n<iSDB->n; n++)
    {
      qSpan = pSpan->next;
      free(pSpan);
      pSpan = qSpan;      
    }

  free(iSDB);
      
  Sprintf("spandb_free complete\n");

  PL_succeed;
}

install_t
install()
{ 
  PL_register_foreign("get_x", 1, (void*)pl_get_x, 0);
  PL_register_foreign("set_x", 1, (void*)pl_set_x, 0);
  PL_register_foreign("inc_x", 0, (void*)pl_inc_x, 0);
  PL_register_foreign("dec_x", 0, (void*)pl_dec_x, 0);
  PL_register_foreign("intervaldb_make", 2, (void*)pl_intervaldb_make, 0);
  PL_register_foreign("intervaldb_print", 1, (void*)pl_intervaldb_print, 0);
  PL_register_foreign("intervaldb_info", 1, (void*)pl_intervaldb_info, 0);
  PL_register_foreign("intervaldb_addone", 4, (void*)pl_intervaldb_addone, 0);
  PL_register_foreign("nclist_build", 2, (void*)pl_nclist_build, 0);
  PL_register_foreign("nclist_intersection", 4, (void*)pl_nclist_intersection, 0);
  PL_register_foreign("nclist_intersection2", 4, (void*)pl_nclist_intersection2, 0);
  PL_register_foreign("intervaldb_intersection", 4, (void*)pl_intervaldb_intersection, 0);
  PL_register_foreign("intervaldb_intersection2", 4, (void*)pl_intervaldb_intersection2, 0);
  PL_register_foreign("intervaldb_match", 3, (void*)pl_intervaldb_match, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("intervaldb_match", 5, (void*)pl_intervaldb_match2, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("intervaldb_match", 4, (void*)pl_intervaldb_match3, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("intervaldb_match", 7, (void*)pl_intervaldb_match4, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("intervaldb_match2", 7, (void*)pl_intervaldb_match5, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("intervaldb_addall", 2, (void*)pl_intervaldb_addall, 0);
  PL_register_foreign("intervaldb_free", 1, (void*)pl_intervaldb_free, 0);
  PL_register_foreign("read_intervals", 3, (void*)pl_read_intervals, 0);
  PL_register_foreign("spandb_make", 1, (void*)pl_spandb_make, 0);
  PL_register_foreign("spandb_addone", 6, (void*)pl_spandb_addone, 0);
  PL_register_foreign("spandb_print", 1, (void*)pl_spandb_print, 0);
  PL_register_foreign("spandb_to_intervaldb", 2, (void*)pl_spandb_to_intervaldb, 0);
  PL_register_foreign("spandb_free", 1, (void*)pl_spandb_free, 0);
  PL_register_foreign("spandb2_make", 1, (void*)pl_spandb2_make, 0);
  PL_register_foreign("spandb2_addone", 6, (void*)pl_spandb2_addone, 0);
  PL_register_foreign("spandb2_print", 1, (void*)pl_spandb2_print, 0);
  PL_register_foreign("spandb2_to_intervaldb", 2, (void*)pl_spandb2_to_intervaldb, 0);
  PL_register_foreign("spandb2_to_intervaldb", 3, (void*)pl_spandb2_to_intervaldb2, 0);
  PL_register_foreign("spandb2_free", 1, (void*)pl_spandb2_free, 0);
}

