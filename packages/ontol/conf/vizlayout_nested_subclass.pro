:- op(1100,xfy,::).
:- op(800,xfy,in).

%allow_overlap.
allow_overlap(subclass).

subclass :: lower(subject) > lower(object) in y. % contained
subclass :: upper(subject) < upper(object) in y. % contained
subclass :: lower(subject) > lower(object) in x.
subclass :: upper(subject) < upper(object) in x. % sub-part horizontally contained by super-part

/*

  simple case:
  
   blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_nested_subclass -n 'neuroblast NB2-5' -rel subclass -to svg > ~/tmp/foo.svg

  this works:
  
  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_nested_subclass -n 'neuroblast NB2-5' -rel subclass -sib -to svg
  
  this one takes a while:
  
  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_nested_subclass -n 'neuroblast' -rel subclass -down 2 -to svg > ~/tmp/foo.svg
*/
