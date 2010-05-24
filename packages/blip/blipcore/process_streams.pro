:- module(process_streams,
	  [process_file_lines/2,
	   process_stream_lines/2,
	   process_stream_lines/3
	   ]).

process_file_lines(Rule,-) :-
	!,
	process_stream_lines(Rule,user_input).
process_file_lines(Rule,File) :-
	!,
	open(File,read,S),
	process_stream_lines(Rule,S),
	close(S).

%% process_stream_lines(Rule) is det
% as process_stream_lines/3, user_input to user_output
process_stream_lines(Rule) :-
	process_stream_lines(Rule,user_input,user_output).

%% process_stream_lines(Rule,In:stream) is det
% as process_stream_lines/3, output on user_output
process_stream_lines(Rule,In) :-
	process_stream_lines(Rule,In,user_output).

%% process_stream_lines(Rule,In:stream,Out:stream) is det
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
process_stream_lines((Head :- (Body1,Body)),In,Out) :-
        !,
	repeat,
	(   at_end_of_stream(In)
	->  !
	;   read_line_to_codes(In, Codes),
	    atom_codes(A,Codes),
	    concat_atom(L,'\t',A),
	    debug(sed,'LINE: ~w [~w]',[L,Body1]),
	    Body1=..[line|L],
	    Body,
	    Head=..[line|L2],
	    concat_atom(L2,'\t',A2),
	    format(Out, '~w~n', [A2]),
	    flush_output(Out),
	    fail
	).
process_stream_lines((Head :- Body),In,Out) :-
        % default match conditions is true
        process_stream_lines((Head :- (Body,true)),In,Out).

