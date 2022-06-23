/* William R. Murray

    Code to Load and Query First Version of YAGO

*/

% Turn off style check, but watch out for misspellings 
:- style_check(-singleton).

%  cd ~/Documents/Coding/Prolog/Semantic_Web/YAGO/YAGO1/
connect :- cd('/users/umurrwi/Documents/Coding/Prolog/Semantic_Web/YAGO/YAGO1/').

:- use_module(library('semweb/rdf11')).              % use new RDF 1.1 spec, incorporates older rdf_db
:- use_module(library('semweb/turtle')).              % to read in TTL triples
:- use_module(library('semweb/rdf_litindex')).    % provides better access for literals
:- use_module(library('semweb/rdf_portray')).     % supports use of prefixes

% Use concise prefixes when showing triples.
:- rdf_portray_as(prefix:id).

% Increase stack limit. Only needed for show_stats currently.
:-set_prolog_flag(stack_limit, 2_147_483_648). 

% set indexing options
:- rdf_set_literal_index_option([verbose(true),index_threads(4)]). 

% These are left out for now, as currently unused.
% [library(semweb/rdf_http_plugin)], % allows downloading by HTTP URL

init_prefixes:-
        rdf_register_prefix(yago,'http://yago-knowledge.org/resource/',[force(true)]),
        rdf_register_prefix(dbr,'http://dbpedia.org/page/',[force(true)]),
        rdf_register_prefix(dbp,'http://dbpedia.org/property/',[force(true)]),
        rdf_register_prefix(dbo,'http://dbpedia.org/ontology/',[force(true)]),
        rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').

:- init_prefixes,format("Prefixes initialized.~n").

load_yago :-
        rdf_load('/users/umurrwi/Documents/Projects/Ontologies/YAGO/YAGO1/yago-1.0.0-turtle/yago-1.0.0-turtle.ttl'),
        format('COMPLETE version of YAGO 1 loaded and ready to query!~n').                  

% Examples of Queries

% Just look at the predicates...
q1 :- rdf(X,Y,Z).

% Just look at the predicates concerning Elvis Presley.
q2 :-
        rdf(yago:'Elvis_Presley',Y,Z),
        format('Elvis Presley --~s--> ~s.~n',[Y,Z]).
q2 :-
        rdf(X,Y,yago:'Elvis_Presley'),
        format('~s --~s--> Evlis Presley.~n',[X,Y]).      

%----------------------------------
%  Goal: Show Statistics and Kinds of Facts
% ----------------------------------


% show what prefixes are currently defined.
show_prefixes :-
        rdf_current_prefix(Prefix, Expansion),
        format("~a = ~a.",[Prefix,Expansion]),
                   nl,
                   fail.

count_subject_resources(N) :- setof(X,rdf_subject(X),Xs),length(Xs,N).
count_all_resources(N) :- setof(X,rdf_resource(X),Xs),length(Xs,N).
count_predicates(N) :- setof(X,rdf_current_predicate(X),Xs),length(Xs,N).
count_literals(N) :- setof(X,rdf_current_literal(X),Xs),length(Xs,N).
count_graph(N) :- setof(X,rdf_graph(X),Xs),length(Xs,N).

show_all_stats :-
        count_subject_resources(N),
        format('Found ~d subject resources.~n',[N]),
        count_predicates(P),
        format('Found ~d subject resources.~n',[P]),
        count_literals(L),
        format('Found ~d literals.~n',[L]),
        count_all_resources(All),
        format('Altogether, found ~d resources.~n',[All]),
        show_triples_stats.

% Find the number of RDF triples present.
show_triples_stats :-
        rdf_statistics(triples(Count)),
        format("Loaded ~d triples in total.~n",[Count]).

%----------------------------------
%  Goal: Performance Improvements
% ----------------------------------

% Based on the counts above we may want to adjust the defaults below.
set_hash_parameters :-
      rdf_set(hash(s,   size, 1048576)),
      rdf_set(hash(p,   size, 1024)),
      rdf_set(hash(sp,  size, 2097152)),
      rdf_set(hash(o,   size, 1048576)),
      rdf_set(hash(po,  size, 2097152)),
      rdf_set(hash(spo, size, 2097152)),
      rdf_set(hash(g,   size, 1024)),
      rdf_set(hash(sg,  size, 1048576)),
      rdf_set(hash(pg,  size, 2048)).

optimize :- writeln('Warming indices...'),
            time(warm_indexes),
            writeln('Warming [s,p,o,sp,po] indices specifically...'),
            time(rdf_warm_indexes([s,p,o,sp,po])),
            writeln('GC and optimizing indices...'),
            time(rdf_gc),
            writeln('Ready to go now!').

/* Add prefixes for:

@base <http://yago-knowledge.org/resource/> .
@prefix dbp: <http://dbpedia.org/ontology/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

These are predefined, so are not included below:

rdf_register_prefix(owl,'http://www.w3.org/2002/07/owl#')
rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#')
rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#')
rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#')

*/

% to speed up performance, we call...
warm_indexes :- 
    ignore(rdf(s, _, _)),
    ignore(rdf(_, p, _)),
    ignore(rdf(_, _, o)),
    ignore(rdf(s, p, _)),
    ignore(rdf(_, p, o)),
    ignore(rdf(s, p, o)),
    ignore(rdf(_, _, _, g)),
    ignore(rdf(s, _, _, g)),
    ignore(rdf(_, p, _, g)).

% test warming up on specific indices, to see how much this helps (or not).

warm_specific :-
        once(rdf(X,rdfs:'label',Y)),
        once(rdf(X,skos:'prefLabel',Y)),
        once(rdf(X,rdf:'type',Y)),
        once(rdf(X,yago:'isLocatedIn',Y)),
        once(rdf(X,yago:'redirectedFrom',Y)),        
        once(rdf(X,yago:'hasLatitude',Y)),
        once(rdf(X,yago:'hasLongitude',Y)).
        
% Other code is separated out into code for loading YAGO subsets
% or code for querying YAGO once it is loaded.

% to be loaded next will be correct if they use yago:, rdf:, etc.
% :- [loading,queries,distance,time,tests,utils].

:- [utils,demo,queries].

:- write('Loaded YAGO code. To load the full YAGO 1 ontology, call load_yago.'),nl.
