/************************************************************

  ontol_text_align_view.pro
  
************************************************************/

class_align(R,ID1,ID2):-
        ontol_db:class_align(R,ID1,ID2,_,_).

% override
sdefun(search_result_id_view(term,detail,ID),
       [
        term_detail_panel(ID),
        term_alignment_summary(ID),
        term_relationship_summary(ID)]).

sdefun(term_alignment_summary(ID),
       div(class=alignment_summary,
           show_alignments(ID2,class_align(exact,ID,ID2),'Exact string matches:'),
           show_alignments(ID2,class_align(subset,ID,ID2),'This term is a substring of:'),
           show_alignments(ID2,class_align(subset,ID2,ID),'This term is a superstring of:'))).

sdefun(show_alignments(X,Goal,Hdr),
       if(setof(X,Goal,Xs),
          then: div(class=alignments,
                    h3(Hdr),
                    html:table(findall(tdpair(href_id_as_label(term,X),
                                              Ont),
                                       (member(X,Xs),belongs(X,Ont))))))).
           

