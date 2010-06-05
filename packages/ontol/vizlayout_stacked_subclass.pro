:- op(1100,xfy,::).
:- op(800,xfy,in).

subclass :: lower(subject) < lower(object) in y. % arrange vertically, sub-parts lower
%subclass :: lower(subject) > lower(object) in y. % arrange vertically, sub-parts lower
subclass :: lower(subject) > lower(object) in x.
subclass :: upper(subject) < upper(object) in x. % sub-part horizontally contained by super-part

/*

  direction helps;

  this works, general at top:
  
  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset  -n 'neuroblast NB2-5' -rel subclass -constraint vizlayout_stacked_subclass -to svg > ~/tmp/foo2.svg

  this doesnt:
  
  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset  -n 'neuroblast' -rel subclass -constraint vizlayout_stacked_subclass -to svg -down 2
  
  but we need general at bottom for this:

  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_flydev -n 'neuroblast NB2-5' -rel subclass -rel all_begin_to_exist_during -constraint vizlayout_stacked_subclass -to svg

  
  
*/
