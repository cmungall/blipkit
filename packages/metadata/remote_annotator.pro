:- module(remote_annotator,
          [
           annotate/2,
           annotate/3,
           tokens_path/2,
           class_annotate/4,
           annotate_pmid/3,
           annotate_pmid/4

           ]).

:- use_module(bio(bioprolog_util)).
:- use_module(metadata_db).
:- use_module(metadata_nlp).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

annotate(Text,Results) :-
        annotate(Text,Results,[]).
annotate(Text,Results,_Opts) :-
        webq('http://kato.crbs.ucsd.edu:9000/scigraph/annotations/entities.json?',[content=Text],Stream),
        json_read(Stream,Term),
        translate_json(Term,Results),
        close(Stream).

annotate_url(URL,Results,_Opts) :-
        webq('http://kato.crbs.ucsd.edu:9000/scigraph/annotations/url.json?',[url=URL],Stream),
        json_read(Stream,Term),
        translate_json(Term,Results),
        close(Stream).

annotate_pmid(ID,Results,Opts) :-
        annotate_pmid(ID,Results,_,Opts).
annotate_pmid(ID,Results,Text,Opts) :-
        atom_concat('ext_id:',ID,Q),
        debug(annotator,'Annotating: ~w',[ID]),
        webq('http://www.ebi.ac.uk/europepmc/webservices/rest/search/resulttype=core&query=ext_id:22363258&',[format=json,resulttype=core,query=Q],Stream),
        json_read(Stream,Term),
        translate_epmc_json(Term,[Text|_]),
        debug(annotator,'text="~w"',[Text]),
        close(Stream),
        annotate(Text,Results,Opts).


class_annotate(C,Text,Results,Term) :-
        class(C),
        class_text_term(C,Text,Term),
        annotate(Text,Results).

class_text_term(C,Text,Goal) :-
        class_goal_text(C,Goal,Text),
        Goal.
class_goal_text(C,entity_label(C,Text),Text).
class_goal_text(C,entity_synonym_scope(C,Text,_),Text).
class_goal_text(C,def(C,Text),Text).
class_goal_text(C,inst_sv(C,_,Text,_),Text).


% this tends to explode...
tokens_path(Tokens,Path) :-
        tokens_path(Tokens,Path,'').

tokens_path([],[],_).
tokens_path([Token|Tokens],Path,LastToken) :-
        same_start(Token,LastToken),
        tokens_path(Tokens,Path,LastToken).
tokens_path([Token|Tokens],[Token|Path],LastToken) :-
        same_start(Token,LastToken),
        tokens_path(Tokens,[_|Path],Token).
tokens_path([Token|Tokens],[Token|Path],LastToken) :-
        \+ same_start(Token,LastToken),
        tokens_path(Tokens,Path,Token).

same_start(token(_,_,_,S,_),token(_,_,_,S,_)).
       

webq(URL,Params,Stream) :-
        findall(A,(member(T=V,Params),
                   uri_encoded(query_value,V,V2),
                   atomic_list_concat([T,=,V2],A)),
                As),
        atomic_list_concat(As,'&',QS),
        %concat_atom([URL,'?',QS],QURL),
        concat_atom([URL,QS],QURL),
        debug(annotator,'URL=~w',[QURL]),
        http_open(QURL,Stream,[]).


translate_json([],[]).
translate_json([H|T],[H2|T2]) :-
        translate_json(H,H2),
        translate_json(T,T2).
translate_json(json(Args), token(ID,Cats,Terms,S,E)) :-
        member(start=S,Args),
        member(end=E,Args),
        member(token=json(TArgs),Args),
        member(id=URI,TArgs),
        member(categories=Cats,TArgs),
        member(terms=Terms,TArgs),
        uri_to_id(URI,ID).

translate_epmc_json([],[]).
translate_epmc_json([H|T],[H2|T2]) :-
        translate_epmc_json(H,H2),
        translate_epmc_json(T,T2).
translate_epmc_json(json(Args), X) :-
        member(abstractText=X,Args),
        !.
translate_epmc_json(json(Args), X) :-
        member(_=A,Args),
        translate_epmc_json(A, X).


uri_to_id(URI,ID) :-
        atom_concat('http://purl.obolibrary.org/obo/',Frag,URI),
        concat_atom([DB,Loc],'_',Frag),
        concat_atom([DB,Loc],':',ID),
        !.
uri_to_id(ID,ID).



        

        


        

