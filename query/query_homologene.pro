:- use_module(bio(io)).
:- use_module(bio(homol_db)).
:- use_module(bio(ontol_db)).
:- use_module(bio(web_fetch_ncbi)).

loadhg:-
        findall(GeneID,(class(Class,_),
                        concat_atom([entrezgene,GeneID],:,Class)),
                GeneIDs),
        maplist(loadhg,GeneIDs),
        write_biofile('homol_db:pro','hgall.xml').

loadhg(GeneID):-
        format(user_error,'Fetching ~w',[GeneID]),
        web_search_ncbi(homologene,query(GeneID,'gene id'),HomologeneIDs),
        web_fetch_ncbi_db_by_ids(homologene,HomologeneIDs).

        
