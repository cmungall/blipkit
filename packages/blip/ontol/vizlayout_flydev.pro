:- op(1100,xfy,::).
:- op(800,xfy,in).

%%% allow_nesting.

% this configuration is for visualizing the developmental aspects of fly anatomy, with time on the X-axis

all_begin_to_exist_during :: lower(subject) > lower(object) in x. % horizontal, R to L
all_begin_to_exist_during :: lower(subject) < upper(object) in x. % horizontal, R to L

begins_during :: lower(subject) > lower(object) in x. % horizontal, R to L
begins_during :: lower(subject) < upper(object) in x. % horizontal, R to L

all_cease_to_exist_during :: upper(subject) > lower(object) in x. % horizontal, R to L
all_cease_to_exist_during :: upper(subject) < upper(object) in x. % horizontal, R to L

/*

  empirical notes:
  
  too slow:
  
blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_flydev -n 'neuroblast NB2-5' -rel subclass -rel all_begin_to_exist_during -to svg > ~/tmp/foo2.svg

  adding more constraints helps:

  blip -debug viz -i test_data/NB_CARO_dev_pm.obo ontol-subset -constraint vizlayout_flydev -n 'neuroblast NB2-5' -rel subclass -rel all_begin_to_exist_during -constraint vizlayout_stacked_subclass -to svg
  
*/
