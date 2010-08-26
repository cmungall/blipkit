:- module(process_streams,
	  [process_file_lines/2,
	   process_file_lines/3,
	   process_stream_lines/1,
	   process_stream_lines/3,
	   process_stream_lines/4
	   ]).

process_file_lines(Rule,File) :-
        process_file_lines(Rule,File,[]).
process_file_lines(Rule,-,Opts) :-
	!,
	prompt(_,''),		% TODO - better way than this hack?
	process_stream_lines(Rule,user_input,Opts).
process_file_lines(Rule,File,Opts) :-
	!,
	open(File,read,S),
	process_stream_lines(Rule,S,Opts),
	close(S).

% as process_stream_lines/3, user_input to user_output
process_stream_lines(Rule) :-
	process_stream_lines(Rule,user_input,user_output,[]).

%% process_stream_lines(Rule,In:stream,Opts:list) is det
% as process_stream_lines/3, output on user_output
process_stream_lines(Rule,In,Opts) :-
	process_stream_lines(Rule,In,user_output,Opts).

%% process_stream_lines(+Rule,+In:stream,+Out:stream,+Opts:list) is det
%
% here Rule should be of the form
%  Head :- MatchGoal,MatchConditions
%
% (MatchConditions is optional)
%
% lines that match MatchGoal will be translated into
% Head terms.
%
% for example:
%  line(ID,FullName) :- line(ID,First,Last),concat_atom([First,Last],' ',FullName).
%
% will translate a 3 column file into a two column file with cols 2 and 3 concatenated.
process_stream_lines((Head :- (Body1,Body)),In,Out,Opts) :-
        !,
	repeat,
	(   at_end_of_stream(In)
	->  !
	;   read_column_values_from_stream(In,L,Opts),
	    debug(process_streams,'LINE: ~w [~w]',[L,Body1]),
	    Body1=..[line|L],
	    Body,
	    Head=..[line|L2],
	    concat_atom(L2,'\t',A2),
	    format(Out, '~w~n', [A2]),
	    flush_output(Out),
	    fail
	).
process_stream_lines((Head :- Body),In,Out,Opts) :-
        % default match conditions is true
        process_stream_lines((Head :- (Body,true)),In,Out,Opts).

read_column_values_from_stream(In,L,Opts) :-
        member(pl(true),Opts),
        !,
        read(In,Term),
        Term =.. L.
read_column_values_from_stream(In,L,_) :-
        read_line_to_codes(In, Codes),
        atom_codes(A,Codes),
        concat_atom(L,'\t',A).

