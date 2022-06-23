/* Queries for part or all of YAGO, including test
queries to show what works.

----------------------------------
   Goal: Get an RDF fact from a Resource
----------------------------------

rdf(yago:'Colorado',Y,Z).   

----------------------------------
  Goal: Show Statistics and Kinds of Facts
----------------------------------

get_all_rdf_stats.

show_unique_relations.   

show_prefixes.
   
----------------------------------
     Goal: Get All Triples for a Resource
----------------------------------

get_next_triple_for_resource(+Resource,-Next_triple)

Get next triple in which the resource is either a subject or an object.
Example: get_next_triple_for_resource(yago:'Colorado',Triple).

get_all_triples_for_resource(+Resource,-Triples)
Example: get_all_triples_for_resource(yago:'Denver,_Colorado',Triples).

Find all triples in which a resource occurs, either as subject or object.

print_all_triples_for_resource(+Resource)
Example: print_all_triples_for_resource(yago:'Elvis_Presley').

Find all triples in which a resource occurs, either as subject or object then print them.

----------------------------------
     Goal: Get Facts Describing a Resource
----------------------------------

Facts are the same as triples except we screen for relevance, e.g., we suppress information about
geographic resources along with relations we deem uninteresting (label, family name, given name, and
redirection).

They are also printed in a more English-like manner below.   

print_all_relevant_facts_for_resource(+Resource)

Find all triples in which a resource occurs, then print out the triples in a more plain English
format, as if presenting facts.

Each triple part (resource) is printed in a more English and fact-like manner, if possible.

----------------------------------
    Goal: Map from a Resource to a Name
----------------------------------

best_name_for_resource(+Resource,-Name) is semidet

and returns the best name for a resource using heuristics:
 1. Use any name given as explicitly preferred.
 2. Otherwise use rdfs:label, first trying just the string, with no nationality.
 3. Otherwise use rdfs:label, allowing nationality, using 'us' first.
 4. Otherwise use rdfs:label, allowing nationality, using any nationality.

----------------------------------
  Goal: to Map from a Name to a Resource
----------------------------------

?- any_resource_for_name('Denver',X).
X = yago:'Denver,_Colorado' ;
X = yago:'Denver,_Pennsylvania' ;
X = yago:'Denver,_Indiana' .

?- all_resources_for_name("Denver",Rs).
Rs = [yago:'Denver,_Colorado', yago:'Denver,_Pennsylvania', yago:'Denver,_Indiana', yago:'Denver,_Iowa', yago:'Denver,_Missouri', yago:'Denver,_Norfolk', yago:'Denver,_Illinois', yago:'Denver,_North_Carolina', yago:'Denver,_Ohio'|...].

----------------------------------

These take a name, which is a string, such as "Lincoln" and then either find one or all resources that have that name as a tag. They can either
find these resources in ANY order or in a BEST-first order. 

When we do not consider preferred matches, we just consider matches in
X or Z from rdf(X,Y,Z). However, if the resource is a literal in the Z
position then there may not be an exact match and we need to use
rdf(X,Y,literal(substring(Name),Resource)).

To find the best match we can first see if there are any SKOS
preferred name matches. For YAGO1, there are none.  Next, we take all
resources that can match the name and order them by 'page rank', i.e.,
by the number of YAGO facts that were found by the resource.

The intuition is that the resource that has the most facts is the most
likely best match for the name.

In summary, there are TWO KEY WAYS of going from a name to the
resource that most likely represents it:

   any_resource_for_name(+Name,-Resource) is nondet

or

   best_resource_for_name(+Name,-Resource) is nondet

The corresponding predicates that collect all possible resources in a list
are:

   all_resources_for_name(+Name,-Resources) is det

and

   best_resources_for_name(+Name,-Resource) is nondet

Previously I also had predicates that only returned the very first
resource found, or for the best-first case, the single best resource
found. But, since we can use once/1 to do this, we do not need
separate functions.

   Implementation of Mapping from Name to Best Resource Match

best_resource_for_name(+Name,-Resource) calls best_resources_for_name(+Name,-Resource),
which in turn calls any_resource_for_name(+Name,-Resource) until all possible matches are found,
and then sorts those resources by "page rank" (the number of facts found in the YAGO kb).

----------------------------------
     Goal: Get all Facts for a Name
----------------------------------



----------------------------------

KEY NOTES:

1. If you want to find the facts from which an inference could be made,
use find and grep as in the example below to see all relevant facts.

  { Find "mesa.*verde" in *.ttl files in the current directory or below. }
  find . -name '*.ttl' -exec grep -iH "mesa.*verde" {} \;

2. DO NOT CONTROL-C AND THEN HIT 'A' FOR 'ABORT' as that may prevent future
queries from operating correctly! 

Instead, exit in another manner, such as:

a. 'r' for 'redo' and then '.' to stop further answers if desired.

b. 'f' for 'fail' which will take you back to the top level.

3. Execution is slow on the first query unless indices are 'warmed' but then
much faster after that. The indices are now 'warmed' first.

However, it still seems that they are slow on the first new query for a 
predicate, e.g.,

rdf(X,rdfs:'label',Y).
rdf(X,yago:'actedIn',Y).

are both very slow the first query and then very fast afterwards.

KEY NOTES ON SYNTAX TO USE IN QUERIES

1. To refer to a resource, use a form such as yago:'Chicago' and not 'yago:Chicago'
or yago:Chicago.

2. Only use prefixes such as 'yago' or 'rdf' INSIDE an rdf query. For example:

  rdf(yago:'Chicago',X,Y), X=yago:'wasCreatedOnDate', Y=literal(type(xsd:date,'1837-03-04')).

DOES NOT WORK (the yago prefix works ONLY INSIDE the rdf, but NOT OUTSIDE), but the following does (succeed):

  rdf(yago:'Chicago',X,Y), X='http://yago-knowledge.org/resource/wasCreatedOnDate', 
                           Y=literal(type('http://www.w3.org/2001/XMLSchema#date','1837-03-04')).

so do not try to use prefixes outside of rdf/3.

3. Do not use strings for names as Prolog treats "Denver" differently from 'Denver', unlike Python.
In general, use atoms everywhere when dealing with YAGO, even for literals.

4. For literals, from a note on http://www.swi-prolog.org/pldoc/man?predicate=rdf/3 :

Notice that queries for object term literal(lang(_,Lex)) also return RDF assertions whose object term is of the form literal(Lex).

For example:

?- [library(semweb/rdf_db)].
?- rdf_assert(rdf:s, rdf:p, literal(o)).
?- rdf(S, P, literal(lang(Lang,Lex))).
S = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#s',
P = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#p',
Lex = o.

5. Note that a query such as rdf(yago:'Mesa_Verde_Wilderness',Y,literal(like('Mesa*Verde*Wilderness',Label))) will fail.

The correct syntax is:

   rdf(yago:'Mesa_Verde_Wilderness',Y,literal(like('Mesa*Verde*Wilderness'),Label)).

In other words, do not call rdf(X,Y,literal(like(Pattern,Match))), instead call rdf(X,Y,literal(like(Pattern),Match))
as the proper syntax for a literal match is literal(?Query,-Match).

*/

% Turn off style check, but watch out for misspellings 
:- style_check(-singleton).

/*
==============================================================================
                      KEY PREDICATES
==============================================================================

*/


/*
==============================================================================
                   PREDICATES TO GET GENERAL INFO
==============================================================================
*/

get_all_rdf_stats :-
       rdf_statistics(triples(N)),
       format("YAGO 1 has ~d triples in it.~n",[N]),
       rdf_statistics(resources(R)),
       format("YAGO 1 has ~d resources in it.~n",[R]),
       rdf_statistics(properties(P)),
       format("YAGO 1 has ~d properties in it.~n",[P]),
       rdf_statistics(literals(L)),
       format("YAGO 1 has ~d literals in it.~n",[L]).

unique_relations(Ps) :- findall(P,rdf_db:rdf_current_predicate(P),Ps).

show_unique_relations :-
        unique_relations(Ps),
        print_all(Ps).

get_facts_for_name(Name) :- best_show_facts_best_first(Name).

get_triples_for_name(Name) :- best_show_triples_best_first(Name).

% find_relations(-X,-Y,+Name)
describe_name(X,Y,Name) :-
        best_resource_for_name(Name,Resource),

        rdf(X,Y,Resource),
        not(memberchk(Y,['http://www.w3.org/2000/01/rdf-schema#label',
                         'http://yago-knowledge.org/resource/redirectedFrom',
                         'http://www.w3.org/2000/01/rdf-schema#label',
                         'http://www.w3.org/2004/02/skos/core#prefLabel',
                         'http://yago-knowledge.org/resource/hasGivenName',
                         'http://yago-knowledge.org/resource/hasFamilyName'])).

% find_relations(+Name,-Y,-Z)
find_relations(Name,Y,Z) :-
        best_resource_for_name(Name,Resource),
        rdf(Resource,Y,Z),
        not(memberchk(Y,['http://www.w3.org/2000/01/rdf-schema#label',
                         'http://yago-knowledge.org/resource/redirectedFrom',
                         'http://www.w3.org/2000/01/rdf-schema#label',
                         'http://www.w3.org/2004/02/skos/core#prefLabel',
                         'http://yago-knowledge.org/resource/hasGivenName',
                         'http://yago-knowledge.org/resource/hasFamilyName'])).

/* 

describe returns all triples where the resource is either a subject or an object. 

describe(yago:'Denver). % yeields 937 facts

describe(yago:'Seattle'). % yeields 1389 facts

*/


% describe(+Resource) finds all triples where Resource is either the subject or object
% part of the triple and then prints them.
describe(Resource) :- setof(Triple,describe(Resource,Triple),Triples), print_triples(Triples).

:- rdf_meta print_all_triples_for_resource(r).
print_all_triples_for_resource(Resource) :-
        get_all_triples_for_resource(Resource,All_triples),
        print_triples(All_triples),
        !.

% describe(+Resource,-Triple_as_list) finds all triples where the Resource is the subject
% or Object part and binds a list representing the arguments of the triple to the output.

:- rdf_meta get_all_triples_for_resource(r,-).

% get all unique triples (as [S,P,O] lists) for Resource.
get_all_triples_for_resource(Resource,All_triples) :-
        setof(Triple,get_next_triple_for_resource(Resource,Triple),All_triples),
        !.

get_all_triples_for_resource(Resource,[]).

% get_next_triple_for_resource(+Resource,-Next_triple) is multi
% and returns the next triple where the Resource is either subject or object in an RDF triple.
% Each triple is returned as a [S, P, O] list.

:- rdf_meta get_next_triple_for_resource(r,-).

get_next_triple_for_resource(Resource,Next_Triple) :- 
        get_next_triple_w_resource_as_subject(Resource,Next_Triple).

get_next_triple_for_resource(Resource,Next_Triple) :-
        get_next_triple_w_resource_as_relation(Resource,Next_Triple).

get_next_triple_for_resource(Resource,Next_Triple) :-
        get_next_triple_w_resource_as_object(Resource,Next_Triple).

get_next_triple_w_resource_as_subject(Resource,[Resource,Y,Z]) :-
        rdf(Resource,Y,Z).

get_next_triple_w_resource_as_relation(Resource,[X,Resource,Z]) :-
        rdf(X,Resource,Z).

get_next_triple_w_resource_as_object(Resource,[X,Y,Resource]) :-
        rdf(X,Y,Resource).

/*
==============================================================================
         INTERFACE PREDICATES TO MAP FROM  YAGO RESOURCES TO TEXT STRING NAMES
==============================================================================
*/

:- rdf_meta best_name_for_resource(r,-).

% best_name_for_resource(+Resource,-Name) is semidet
best_name_for_resource(Resource,Name) :-
        rdf(Resource,skos:'prefLabel',Name), !.

best_name_for_resource(Resource,Name) :-
        rdf(Resource,rdfs:'label',Name^^xsd:string), !.

best_name_for_resource(Resource,Name) :-
        rdf(Resource,rdfs:'label',Name@en), !.

best_name_for_resource(Resource,Name) :-
        rdf(Resource,rdfs:'label',Name@Lang), !.

/*
==============================================================================
         INTERFACE PREDICATES TO MAP FROM TEXT STRING NAMES TO YAGO RESOURCES
==============================================================================
*/


% any_resource_labeling_name(+Name,-Resource) is nondet

% Find all resources with the given Name as their label.
%
% We may also pick up some metonymies, e.g., 'Colorado' can be referred
%  to by the label 'Denver', so we may want filter these out by, e.g.,
%  noting that the preferred label for 'Colorado' is not 'Denver', but
%  instead 'Colorado'.

% We use exact literal match below, but substring match would give us
% even more:
%         rdf(Concept,rdfs:'label',literal(substring(Name),Match)).
% Match binds with what was matched.

any_resource_labeling_name(Name,Resource) :-
        atom(Name),
        atom_string(Name,Name_as_string),
        rdf(Resource,rdfs:'label',Name_as_string).

any_resource_labeling_name(Name,Resource) :-
        string(Name),
        rdf(Resource,rdfs:'label',Name).

% all_resources_for_name(+Name,-Resources) is det
% finds a bag of all unique possible resources for a name (e.g., there are several "Bill Murray" resources).
all_resources_for_name(Name,Resources) :-
        bagof(Resource,any_resource_for_name(Name,Resource),Resources),
        uniquify(Resources,Unique_Resources).

% best_resource_for_name(+Name,-Resource) is det
% Use preferences and page rank heuristics to offer up the resource for a name.
% Can offer up the next best solution to a query if the first fails.
best_resource_for_name(Name,Resource) :- best_first_concepts_for_name(Name,Resource) .

% best_resources_for_name(+Name,-Resources) is multi
% Use preferences and page rank heuristics to offer up the resource for a name.
% Can offer up the next best solution to a query if the first fails.
best_resources_for_name(Name,Resources) :- name_of_resource_via_page_rank(Name,Resources).

% best_resource_for_name(+Name,-Resource) is semidet
% Use preferences and page rank heuristics to offer up the resource for a name.
% Can offer up the next best solution to a query if the first fails.
single_best_resource_for_name(Name,Resource) :- once(best_resource_for_name(Name,Resource)).

/*
==============================================================================
         LOWER-LEVEL PREDICATES TO MAP FROM TEXT STRING NAMES TO YAGO RESOURCES
==============================================================================

All assume that the Name or Label given is an atom, not a string. Use
ensure_atom/2 if necessary to ensure this.

If in doubt, use name_to_resource/2. It favors preferred labels or meanings
that have been explicitly defined and removes duplicates, but first has to
gather all possibilities.

For greater efficiency, if duplicates are OK, use best_first_concepts_for_name/2.

Note that a very good heuristic for the most likely meaning of a Name is similar
to PageRank: Find the meaning with the most facts attached to it. 

That is the slowest approach. Use name_of_resource_via_page_rank/2 for that.

*/

/*

NOTE: YAGO 1 has neither 

* skos:'prefLabel' 

or

*  yago:'isPreferredMeaning':

so this part is commented out. It is left in for the later YAGO
versions that do.

Notes on preferred_label_for_concept and preferred_concept_for_label,
skos:'prefLabel' and yago:'isPreferredMeaning':

preferred_label_for_concept finds the best name to use IF YOU KNOW THE
RESOURCE, since skos:'prefLabel' associates a resource with the name
most commonly used to describe it.

preferred_concept_for_label finds the best resource to use IF YOU KNOW
THE NAME, since yago:'isPreferredMeaning' associates a user name with
the resource it most commonly means, but the resource is still the
subject and the name the object in the triple.

Find the best name to use for a resource in text generation.
<London>	skos:prefLabel	"London"@eng .

preferred_label_for_concept(Name,Resource) :- rdf(Resource,skos:'prefLabel',Name).

% Find the most likely place or item being talked about, e.g., for "London", we want to match
% <London>	yago:'isPreferredMeaning'	"London"@eng .
% and not get London, Ontario or somewhere else. 

preferred_concept_for_label(Name,Resource) :- rdf(Resource,yago:'isPreferredMeaning',Name).

*/

% Try all possible resources, e.g., if we are in a query, but try the most likely matches first.
name_to_resource(Name,Resource) :-
        bagof(Resource,best_first_concepts_for_name(Name,Resource),Resources),        
        !,
        return_unique(Resources,Resource).

/* best_first_concepts_for_name/2 looks up the resources with a given name,
one at a time, starting with preferred translations. Unlike name_to_resource, it
may return the same Resource for Name more than once.
*/
% YAGO version 1 omits:  best_first_concepts_for_name(Name,Resource) :- preferred_concept_for_label(Name,Resource).
% YAGO version 1 omits: best_first_concepts_for_name(Name,Resource) :- preferred_label_for_concept(Name,Resource).
best_first_concepts_for_name(Name,Resource) :- best_first_concept_page_ranked_for_name(Name,Resource).

/* best_concept_only_for_name/2 looks up the resources with a given name,
one at a time, starting with preferred translations. If it succeeds, it only returns the
best candidate and then fails after that.
*/
% YAGO version 1 omits:  best_first_concepts_for_name(Name,Resource) :- preferred_concept_for_label(Name,Resource).
% YAGO version 1 omits: best_first_concepts_for_name(Name,Resource) :- preferred_label_for_concept(Name,Resource).
best_concept_only_for_name(Name,Resource) :- best_first_concept_page_ranked_for_name(Name,Resource),!.

/*
ordered_all_resources_for_name(+Name,-Resources)
finds a bag of all unique possible resources for a name (e.g., there are several "Bill Murray" resources),
ordered in best first manner considering preferred labels and meanings.
*/
ordered_all_resources_for_name(Name,Unique_Resources) :-
        bagof(Term,best_first_concepts_for_name(Name,Term),Resources),
        uniquify(Resources,Unique_Resources).


/*
best_match_for_name(Name,Resource)
returns the first match for the name given any preferred meanings and labels.
*/

best_match_for_name(Name,Resource) :- best_first_concepts_for_name(Name,Resource),!.

/*

page_ranked_resources prints the best resources considering page rank (number of facts) only.
*/

page_ranked_resources(Name) :-
        name_of_resource_via_page_rank(Name,Page_Ranked_Resources),
        print('The best matching resources for '),print(Name),print(' are:'),nl,
        print_all(Page_Ranked_Resources).

/* Given a Name return the resources that most likely match in order,
where the order is determined by the number of facts found for each
resource.

Thus, this translation interprets the resources with the most facts
as being the most likely interpretations of the name.
*/


name_of_resource_via_page_rank(Name,Unique_Page_Ranked_Resources) :-
        all_resources_for_name(Name,Matching_Resources),
        gather_and_sort_resources_via_number_of_facts(Matching_Resources,Page_Ranked_Resources),
        uniquify(Page_Ranked_Resources,Unique_Page_Ranked_Resources).

/* best_first_concept_page_ranked_for_name/2 looks up the resources with a given name,
one at a time, starting with preferred translations. Unlike name_to_resource, it
may return the same Resource for Name more than once.
*/
% YAGO version 1 omits:  best_first_concept_page_ranked_for_name(Name,Resource) :- preferred_concept_for_label(Name,Resource).
% YAGO version 1 omits:  best_first_concept_page_ranked_for_name(Name,Resource) :- preferred_label_for_concept(Name,Resource).
best_first_concept_page_ranked_for_name(Name,Resource) :- name_of_resource_via_page_rank(Name,Resources),
                                                               member(Resource,Resources).
/*
very_best_resources gets all resources, sorted using all heuristics:

1. The preferred concept for a label is the most likely referent.

2. A concept with the label as its preferred label is the next most likely.

3. Then concepts ordered by number of facts.

*/

very_best_resources(Name,Resources) :-
        bagof(Resource,
              (best_first_concept_page_ranked_for_name(Name,Resource),not(is_geo_resource(Resource))),
              Resources_w_Duplicates),
        uniquify(Resources_w_Duplicates,Resources).

print_very_best_resources(Name) :-
        very_best_resources(Name,Resources),
        print('The very best [preferences + page rank / # of facts] matching resources for '),
        print(Name),print(' are:'),nl,
        print_all(Resources).

/* gather_and_sort_resources_via_number_of_facts/2 takes a list of resources and
creates a list of pairs. For each pair,

1. The first element is the resource.

2. The second element is the number of facts found for the
resource.

The top-level set of triples is sorted on the second element, the
number of facts.

*/

gather_and_sort_resources_via_number_of_facts(Resources,Page_Ranked_Resources) :-
        generate_resource_number_facts_pairs(Resources,Resource_Number_Pairs),
        sort(2,@>=,Resource_Number_Pairs,Sorted_Resource_Number_Pairs),
        take_first_elements(Sorted_Resource_Number_Pairs,Page_Ranked_Resources).

% generate_resource_number_facts_pairs essentially creates [Resource,Number]
% pairs, one for each resource, gathered into a list.
generate_resource_number_facts_pairs([],[]).
generate_resource_number_facts_pairs([Resource|Other_Resources],[[Resource,N]|Facts_For_Others_Resources]) :-
        get_all_facts_for_resource(Resource,Facts_For_Resource),
        length(Facts_For_Resource,N),
        generate_resource_number_facts_pairs(Other_Resources,Facts_For_Others_Resources).

% take_first_elements essentially takes the first element of each list pair.
take_first_elements([],[]).
take_first_elements([[Resource,Number]|Rest],[Resource|Others]) :- take_first_elements(Rest,Others).

/*

==============================================================================
               PREDICATES TO GET FACTS FOR GIVEN NAMES
==============================================================================

         KEY RELATIONS: SHOW_FACTS_BEST_FIRST/1 and SHOW_TRIPLES_BEST_FIRST/1, AND
            BEST_SHOW_FACTS_BEST_FIRST/1 and BEST_SHOW_TRIPLES_BEST_FIRST/1.

These relations either print a list of facts (in English) or as triples.

The difference is how the relations operate:

1. show_facts_best_first/1 and show_triples_best_first/1 work by finding all possible
referents with the same label, counting the number of facts for each
possible referent, sorting the referents by number of facts
('page-ranked') and then printing out the results. The facts are kept
in triples along with the referents and the count of the facts so
facts are retrieved only once.


2.best_show_facts_best_first/1 and best_show_triples_best_first/1 work by first
ordering referents by heuristics, to get the best possible
interpretation. Explicit preferences go to the front of the
list, followed by facts ordered by the number of facts associated
with each. The latter causes the first retrieval of facts, but
the overall listing of referents may be better as explicit
preferences are taken into account.

Once the referents are ordered then the facts are retrieved for
each, one by one. This results in a second retrieval of referents.

In summary, the results should be better, but execution slower for
best_show_facts_best_first/1 and best_show_triples_best_first/1 than
show_facts_best_first/1 and show_triples_best_first/1.

*/

% If we are suppressing some kinds of facts, e.g., we do not want facts such as 'yago:geoentity_Abraham_Lincoln_3569752 has latitude 22.8.'
% to appear, then the setof in the first rule below may fail, so we need the second clause.
all_facts(Name,Facts) :- setof([Concept,Relation,Object],fact(Name,[Concept,Relation,Object]),Facts),!.
all_facts(Name,[]).

% Print out all the facts for a name or label in English.
% For example, show_all_facts("Alan Turing").
show_all_facts(Name) :- all_facts(Name,Facts),show_facts(Facts).

% Print out all the facts for a name or label as triples.
% For example, list_all_facts("Alan Turing").
list_all_facts(Name) :- all_facts(Name,Facts),print_triples(Facts).

/*

Print out all the facts for a name or label in English.  When the same
name can name multiple resources return the resources with the most
facts first.

This has the same result as taking the page ranked resources and
gathering their facts, but only gathers the facts once, not twice.
*/
show_facts_best_first(Name) :-
        all_resources_for_name(Name,Matching_Resources),
        gather_and_sort_facts(Matching_Resources,Facts),
        show_facts(Facts).

% Print out all the facts for a name or label as triples.
% When the same name can name multiple resources return
% the resources with the most triples first.
show_triples_best_first(Name) :-
        all_resources_for_name(Name,Matching_Resources),
        gather_and_sort_facts(Matching_Resources,Facts),
        print_triples(Facts).

/* gather_and_sort_facts/2 takes a list of resources and
creates a list of triples. For each triple,

1. The first element is the resource.

2. The second element is the number of facts found for the
resource.

3. The third element are the facts themselves, as S-P-O triples.

The top-level set of triples is sorted on the second element, the
number of facts, and then the resulting facts are extracted and
returned in order. 

*/

gather_and_sort_facts(Resources,Facts) :-
        generate_resource_number_facts_triples(Resources,Resource_Number_Facts_Triples),
        sort(2,@>=,Resource_Number_Facts_Triples,Sorted_Resource_Number_Facts_Triples),
        take_facts_and_flatten(Sorted_Resource_Number_Facts_Triples,Facts).

% generate_resource_number_facts_triples essentially creates [Resource,Number,Facts]
% triplets, one for each resource, gathered into a list.
generate_resource_number_facts_triples([],[]).
generate_resource_number_facts_triples([Resource|Other_Resources],[[Resource,N,Facts_For_Resource]|Facts_For_Others_Resources]) :-
        get_all_facts_for_resource(Resource,Facts_For_Resource),
        length(Facts_For_Resource,N),
        generate_resource_number_facts_triples(Other_Resources,Facts_For_Others_Resources).

% take_facts_and_flatten essentially takes the third element of each list triplet and appends them together.
take_facts_and_flatten(Sorted_Resource_Number_Facts_Triples,All_Facts) :-
        % write('Collecting just the facts, in order, from...'),nl,print_all(Sorted_Resource_Number_Facts_Triples), !,
        take_facts_and_flatten1(Sorted_Resource_Number_Facts_Triples, [],All_Facts).
take_facts_and_flatten1([],All_Facts,All_Facts).
take_facts_and_flatten1([[Resource,Number,Facts]|Rest],Facts_So_Far,All_Facts) :-
        % write(Resource),write(' has '),write(Number),write(' facts.'),nl,
        append(Facts_So_Far,Facts,Updated_Fact_List),
        % write('Facts so far...'),nl, show_triples(Updated_Fact_List),
        !,
        take_facts_and_flatten1(Rest,Updated_Fact_List,All_Facts).

/*
Rank order the referents by likelihood.

Then for each referent print out facts for it in English.
*/

best_show_facts_best_first(Name) :-
        very_best_resources(Name,Ordered_Resources),
        !,
        member(Resource,Ordered_Resources),
        print_all_facts_for_resource(Resource),
        fail.
best_show_facts_best_first(Name).


/*
Rank order the referents by likelihood.

Then for each referent print out triples for it.
*/

best_show_triples_best_first(Name) :-
        very_best_resources(Name,Ordered_Resources),
        !,
        member(Resource,Ordered_Resources),
        print_all_triples_for_resource(Resource),
        fail.
best_show_triples_best_first(Name).

/*

==============================================================================
                PREDICATES TO GET FACTS GIVEN RESOURCES
==============================================================================

Facts are the same as triples except we screen for relevance, e.g., we suppress information about
geographic resources along with relations we deem uninteresting (label, family name, given name, and
redirection).

*/


% TO DO 1: The first rdf/2 clause for finding concepts corresponding to Name may also
% pick up some metonymies, e.g., 'Colorado' can be referred to by the label 'Denver',
% so we may want filter these out by, e.g., noting that the preferred label for 'Colorado'
% is not 'Denver', but instead 'Colorado'.

get_relevant_fact_for_name(Name,[S,P,O]) :-
        any_resource_for_name(Name,Resource),
        get_relevant_fact_for_resource(Resource,[S,P,O]).

get_relevant_fact_for_resource(Resource,[S,P,O]) :-

        % generate
        get_next_triple_for_resource(Resource,[S,P,O]),

         % test: we don't want facts such as 'Lincoln has label Lincoln'.
        not(is_relationship_for_uninteresting_fact(P)),

         % test: we don't want facts such as 'yago:geoentity_Abraham_Lincoln_3569752 has latitude 22.8.'
        not(is_geo_resource(Resource)).

% If we are suppressing some kinds of facts, e.g., we do not want facts such as 'yago:geoentity_Abraham_Lincoln_3569752 has latitude 22.8.'
% to appear, then the setof in the first rule below may fail, so we need the second clause.
:- rdf_meta get_all_relevant_facts_for_resource(r,-).

get_all_relevant_facts_for_resource(Resource,Facts) :-
        setof([S,P,O],
              get_relevant_fact_for_resource(Resource,[S,P,O]),
              Facts),!.
get_all_relevant_facts_for_resource(Resource,[]).

/*
==============================================================================
              PREDICATES TO FILTER PREDICATES OR OTHER RESOURCES
==============================================================================
*/


% we don't want facts such as 'Lincoln has label Lincoln'.
is_relationship_for_uninteresting_fact(Relation) :-
        memberchk(Relation,
                  ['http://www.w3.org/2000/01/rdf-schema#label',
                   'http://yago-knowledge.org/resource/redirectedFrom',
                   'http://www.w3.org/2000/01/rdf-schema#label',
                   'http://yago-knowledge.org/resource/hasGivenName',
                   'http://yago-knowledge.org/resource/hasFamilyName']).

% we don't want facts such as 'yago:geoentity_Abraham_Lincoln_3569752 has latitude 22.8.'
is_geo_resource(Resource) :-
        atom_starts_with_prefix('http://yago-knowledge.org/resource/geoentity_',Resource).

/*
==============================================================================
                  PREDICATES TO PRETTY PRINT FACTS
==============================================================================
*/


/*
print_all_relevant_facts_for_resource/1 and print_all_triples_for_resource/1 are used to
print either facts in English or triples for a given resource.
*/

:- rdf_meta print_all_relevant_facts_for_resource(r).
print_all_relevant_facts_for_resource(Resource) :-
        get_all_relevant_facts_for_resource(Resource,Facts),
        show_facts(Facts),
        !.
                                    
% show_facts(+Facts) prints out a list of facts nicely. Each fact should be a grounded list
% corresponding to an RDF fact: [Subject,Predicate,Object].

show_facts([Fact|Rest]) :- show_fact(Fact),nl,show_facts(Rest).
show_facts([]) :- nl.

show_fact([Subject,Predicate,Object]) :-
        show_fact_part(Subject),
        write(' '),
        show_fact_part(Predicate),
        write(' '),
        show_fact_part(Object),
        write('.').

% Handle resources with generic naming methods (e.g., yago:'wikicat_20th-century_mathematicians',
% yago:wikicat_Academics_of_the_University_of_Cambridge, yago:wikicat_Logicians, etc.)

show_fact_part(Yago_Wiki_Category) :-
        atom(Yago_Wiki_Category),
        yago_wiki_category(Yago_Wiki_Category,Category_Name),
        format('member of the Wikipeda category ~a',[Category_Name]).

% Handle resources with generic naming methods (e.g., yago:'wordnetDomain_history',
% yago:'wordnetDomain_military, etc).

show_fact_part(Yago_Wordnet_Domain) :-
        atom(Yago_Wordnet_Domain),
        yago_wordnet_domain(Yago_Wordnet_Domain,Category_Name),
        format('~a',[Category_Name]).

% Handle most resources here.

% Note that skos:'prefLabel' also maps WordNet resources, such as yago:wordnet_conflict_100958896
% to their more succinct word lemmas, in this case, 'conflict'. Normally, it just looks up the
% best name for something, e.g., yago:'Denver' would have the best name 'Denver'.
show_fact_part(Resource) :-
        atom(Resource),
        best_name_for_resource(Resource,BestName),
        write(BestName),!.

show_fact_part(Compound):-
        Compound = String^^Type,
        write(String),!.

show_fact_part(Resource) :-
        atom(Resource),
        rdf_global_id(Short_Prefixed_Name,Resource),
        pretty_print_for(Short_Prefixed_Name,BestName),
        write(BestName),!.

% Handle literals
% show_fact_part(Name) :-
%pre        print(Name),!. % I use print rather than write as it uses registered RDF prefixes in the output.

show_fact_part(Resource) :- portray(Resource),!.
show_fact_part(Resource) :- write(Resource).

/* We would ideally like to have pretty_print translations for all of
the predicates that follow:

?- show_all_predicates().
1. http://www.w3.org/1999/02/22-rdf-syntax-ns#type
2. http://www.w3.org/2000/01/rdf-schema#comment
3. http://www.w3.org/2000/01/rdf-schema#label
4. http://www.w3.org/2000/01/rdf-schema#subClassOf
5. http://www.w3.org/2002/07/owl#disjointWith
6. http://www.w3.org/2002/07/owl#sameAs
7. http://www.w3.org/2004/02/skos/core#narrower
8. http://www.w3.org/2004/02/skos/core#prefLabel
9. http://yago-knowledge.org/resource/actedIn
10. http://yago-knowledge.org/resource/created
11. http://yago-knowledge.org/resource/dealsWith

14. http://yago-knowledge.org/resource/directed
15. http://yago-knowledge.org/resource/edited
16. http://yago-knowledge.org/resource/exports
17. http://yago-knowledge.org/resource/graduatedFrom
18. http://yago-knowledge.org/resource/happenedIn
19. http://yago-knowledge.org/resource/happenedOnDate
20. http://yago-knowledge.org/resource/hasAcademicAdvisor
21. http://yago-knowledge.org/resource/hasAirportCode

23. http://yago-knowledge.org/resource/hasBudget
24. http://yago-knowledge.org/resource/hasCapital
25. http://yago-knowledge.org/resource/hasChild
26. http://yago-knowledge.org/resource/hasCurrency
27. http://yago-knowledge.org/resource/hasDuration
28. http://yago-knowledge.org/resource/hasEconomicGrowth
29. http://yago-knowledge.org/resource/hasExpenses
30. http://yago-knowledge.org/resource/hasExport
31. http://yago-knowledge.org/resource/hasFamilyName
32. http://yago-knowledge.org/resource/hasGDP
33. http://yago-knowledge.org/resource/hasGender
34. http://yago-knowledge.org/resource/hasGeonamesEntityId
35. http://yago-knowledge.org/resource/hasGini
36. http://yago-knowledge.org/resource/hasGivenName
37. http://yago-knowledge.org/resource/hasGloss
38. http://yago-knowledge.org/resource/hasHeight
39. http://yago-knowledge.org/resource/hasISBN
40. http://yago-knowledge.org/resource/hasISIN
41. http://yago-knowledge.org/resource/hasImdb
42. http://yago-knowledge.org/resource/hasImport
43. http://yago-knowledge.org/resource/hasInflation
44. http://yago-knowledge.org/resource/hasLanguageCode
45. http://yago-knowledge.org/resource/hasLatitude
46. http://yago-knowledge.org/resource/hasLength
47. http://yago-knowledge.org/resource/hasLongitude
48. http://yago-knowledge.org/resource/hasMotto
49. http://yago-knowledge.org/resource/hasMusicalRole
50. http://yago-knowledge.org/resource/hasNeighbor
51. http://yago-knowledge.org/resource/hasNumberOfPeople
52. http://yago-knowledge.org/resource/hasOfficialLanguage
53. http://yago-knowledge.org/resource/hasPages
54. http://yago-knowledge.org/resource/hasPopulationDensity
55. http://yago-knowledge.org/resource/hasPoverty
56. http://yago-knowledge.org/resource/hasRevenue
57. http://yago-knowledge.org/resource/hasTLD
58. http://yago-knowledge.org/resource/hasThreeLetterLanguageCode
59. http://yago-knowledge.org/resource/hasUnemployment
60. http://yago-knowledge.org/resource/hasWebsite
61. http://yago-knowledge.org/resource/hasWeight
62. http://yago-knowledge.org/resource/hasWonPrize
63. http://yago-knowledge.org/resource/hasWordnetDomain
64. http://yago-knowledge.org/resource/imports
65. http://yago-knowledge.org/resource/influences
66. http://yago-knowledge.org/resource/isAffiliatedTo
67. http://yago-knowledge.org/resource/isCitizenOf
68. http://yago-knowledge.org/resource/isConnectedTo
69. http://yago-knowledge.org/resource/isInterestedIn
70. http://yago-knowledge.org/resource/isKnownFor
71. http://yago-knowledge.org/resource/isLeaderOf
72. http://yago-knowledge.org/resource/isLocatedIn
73. http://yago-knowledge.org/resource/isMarriedTo
74. http://yago-knowledge.org/resource/isPoliticianOf
75. http://yago-knowledge.org/resource/isPreferredMeaningOf
76. http://yago-knowledge.org/resource/livesIn
77. http://yago-knowledge.org/resource/owns
78. http://yago-knowledge.org/resource/participatedIn
79. http://yago-knowledge.org/resource/playsFor
80. http://yago-knowledge.org/resource/redirectedFrom
81. http://yago-knowledge.org/resource/startedOnDate
82. http://yago-knowledge.org/resource/wasBornIn
83. http://yago-knowledge.org/resource/wasBornOnDate
84. http://yago-knowledge.org/resource/wasCreatedOnDate
85. http://yago-knowledge.org/resource/wasDestroyedOnDate
86. http://yago-knowledge.org/resource/worksAt
87. http://yago-knowledge.org/resource/wroteMusicFor

Pretty print has not yet been defined for:

2. http://www.w3.org/2000/01/rdf-schema#comment
3. http://www.w3.org/2000/01/rdf-schema#label
4. http://www.w3.org/2000/01/rdf-schema#subClassOf
5. http://www.w3.org/2002/07/owl#disjointWith

7. http://www.w3.org/2004/02/skos/core#narrower
8. http://www.w3.org/2004/02/skos/core#prefLabel


11. http://yago-knowledge.org/resource/dealsWith
12. http://yago-knowledge.org/resource/diedIn
13. http://yago-knowledge.org/resource/diedOnDate
14. http://yago-knowledge.org/resource/directed
15. http://yago-knowledge.org/resource/edited
16. http://yago-knowledge.org/resource/exports
17. http://yago-knowledge.org/resource/graduatedFrom

19. http://yago-knowledge.org/resource/happenedOnDate
20. http://yago-knowledge.org/resource/hasAcademicAdvisor
21. http://yago-knowledge.org/resource/hasAirportCode

23. http://yago-knowledge.org/resource/hasBudget

25. http://yago-knowledge.org/resource/hasChild
26. http://yago-knowledge.org/resource/hasCurrency
27. http://yago-knowledge.org/resource/hasDuration
28. http://yago-knowledge.org/resource/hasEconomicGrowth
29. http://yago-knowledge.org/resource/hasExpenses
30. http://yago-knowledge.org/resource/hasExport
31. http://yago-knowledge.org/resource/hasFamilyName
32. http://yago-knowledge.org/resource/hasGDP
33. http://yago-knowledge.org/resource/hasGender
34. http://yago-knowledge.org/resource/hasGeonamesEntityId
35. http://yago-knowledge.org/resource/hasGini
36. http://yago-knowledge.org/resource/hasGivenName
37. http://yago-knowledge.org/resource/hasGloss
38. http://yago-knowledge.org/resource/hasHeight
39. http://yago-knowledge.org/resource/hasISBN
40. http://yago-knowledge.org/resource/hasISIN
41. http://yago-knowledge.org/resource/hasImdb
42. http://yago-knowledge.org/resource/hasImport
43. http://yago-knowledge.org/resource/hasInflation
44. http://yago-knowledge.org/resource/hasLanguageCode

46. http://yago-knowledge.org/resource/hasLength


49. http://yago-knowledge.org/resource/hasMusicalRole
50. http://yago-knowledge.org/resource/hasNeighbor
51. http://yago-knowledge.org/resource/hasNumberOfPeople
52. http://yago-knowledge.org/resource/hasOfficialLanguage
53. http://yago-knowledge.org/resource/hasPages
54. http://yago-knowledge.org/resource/hasPopulationDensity
55. http://yago-knowledge.org/resource/hasPoverty
56. http://yago-knowledge.org/resource/hasRevenue
57. http://yago-knowledge.org/resource/hasTLD
58. http://yago-knowledge.org/resource/hasThreeLetterLanguageCode

60. http://yago-knowledge.org/resource/hasWebsite
61. http://yago-knowledge.org/resource/hasWeight
62. http://yago-knowledge.org/resource/hasWonPrize
63. http://yago-knowledge.org/resource/hasWordnetDomain
64. http://yago-knowledge.org/resource/imports
65. http://yago-knowledge.org/resource/influences
66. http://yago-knowledge.org/resource/isAffiliatedTo
67. http://yago-knowledge.org/resource/isCitizenOf
68. http://yago-knowledge.org/resource/isConnectedTo
69. http://yago-knowledge.org/resource/isInterestedIn
70. http://yago-knowledge.org/resource/isKnownFor
71. http://yago-knowledge.org/resource/isLeaderOf
72. http://yago-knowledge.org/resource/isLocatedIn
73. http://yago-knowledge.org/resource/isMarriedTo
74. http://yago-knowledge.org/resource/isPoliticianOf
75. http://yago-knowledge.org/resource/isPreferredMeaningOf
76. http://yago-knowledge.org/resource/livesIn

78. http://yago-knowledge.org/resource/participatedIn
79. http://yago-knowledge.org/resource/playsFor
80. http://yago-knowledge.org/resource/redirectedFrom
81. http://yago-knowledge.org/resource/startedOnDate

84. http://yago-knowledge.org/resource/wasCreatedOnDate
85. http://yago-knowledge.org/resource/wasDestroyedOnDate
86. http://yago-knowledge.org/resource/worksAt
87. http://yago-knowledge.org/resource/wroteMusicFor

*/

/*
N.B. We DO NOT want :- rdf_meta pretty_print_for(r,-).  here, since that will cause expansion to the
much longer terms commented out above. Instead we are going to first convert the longer form into
the shorter form, and then call pretty_print_for, so we can enter the translations more clearly.
*/

pretty_print_for(rdf:'type','is a').
pretty_print_for(yago:'isDescribedBy','is described by the Wikipedia page at').
pretty_print_for(yago:'hasWonPrize','has won the prize').
pretty_print_for(yago:'hasChild','had child').
pretty_print_for(yago:'isMarriedTo','is or was married to').
pretty_print_for(yago:'bornIn','was born in').
pretty_print_for(yago:'bornOnDate','was born on').
pretty_print_for(yago:'livesIn','lives in or lived in').
pretty_print_for(yago:'influences','influenced').
pretty_print_for(yago:'wasBornIn','was born in').
pretty_print_for(yago:'wasBornOnDate','was born on').
pretty_print_for(yago:'diedIn','died in').
pretty_print_for(yago:'diedOnDate','died on').
pretty_print_for(yago:'hasGender','is a').
pretty_print_for(yago:'graduatedFrom','graduated from').
pretty_print_for(yago:'worksAt','worked at').
pretty_print_for(yago:'hasAcademicAdvisor','had as academic advisor').
pretty_print_for(yago:'isLocatedIn','is located in').
pretty_print_for(yago:'hasNumberOfPeople','has population').
pretty_print_for(yago:'hasLatitude','has latitude').
pretty_print_for(yago:'hasLongitude','has longitude').
pretty_print_for(yago:'hasWebsite','has web site').
pretty_print_for(yago:'wasCreatedOnDate','was created on').
pretty_print_for(yago:'wasDestroyedOnDate','was destroyed on').
pretty_print_for(yago:'hasArea','has an area of').
pretty_print_for(yago:'hasPopulationDensity','has a population density of').
pretty_print_for(yago:'hasMotto','has as a motto').
pretty_print_for(yago:'owns','owns').
pretty_print_for(yago:'hasCapital','has as capital').
pretty_print_for(yago:'hasPoverty','has a poverty level of').
pretty_print_for(yago:'hasUnemployment','has an unemployment level of').
pretty_print_for(yago:'hasGeonamesEntityId','is the same as the GeoNames entity').
pretty_print_for(yago:'happenedIn','happened in').
pretty_print_for(yago:'happenedOnDate','happened on').
pretty_print_for(yago:'actedIn','acted in').
pretty_print_for(yago:'created','created').
pretty_print_for(yago:'directed','directed').
pretty_print_for(yago:'produced','produced').
pretty_print_for(yago:'hasGloss','has a dictionary definition of').
pretty_print_for(yago:'hasWordnetDomain','is part of the topic').
pretty_print_for(owl:'sameAs','is the same as').
pretty_print_for(skos:'prefLabel','is typically called').
pretty_print_for(rdfs:'subClassOf','is a kind of').
 
/*
pretty_print_for('http://www.w3.org/1999/02/22-rdf-syntax-ns#type','belongs to class').
pretty_print_for('http://yago-knowledge.org/resource/hasWonPrize','has won the prize').
pretty_print_for('http://yago-knowledge.org/resource/influences','influenced').
pretty_print_for('http://yago-knowledge.org/resource/wasBornIn','was born in').
pretty_print_for('http://yago-knowledge.org/resource/wasBornOnDate','was born on').
pretty_print_for('http://yago-knowledge.org/resource/diedOnDate','died on').
pretty_print_for('http://yago-knowledge.org/resource/hasGender','is a').
pretty_print_for('http://yago-knowledge.org/resource/graduatedFrom','graduated from').
pretty_print_for('http://yago-knowledge.org/resource/worksAt','worked at').
pretty_print_for('http://yago-knowledge.org/resource/hasAcademicAdvisor','had as academic advisor').
*/

/*
==============================================================================
                PREDICATES FOR SPECIFIC KINDS OF QUERIES
==============================================================================
*/

% GeoNames experiments, if GeoNames is loaded.
find_label_Denver(Label) :- rdf('http://yago-knowledge.org/resource/geoentity_Denver_2169039',rdfs:'label',Label).

location(Name,Lat,Long) :- best_resource_for_name(Name,Place),
                           rdf(Place,'http://yago-knowledge.org/resource/hasLatitude',Lat),
                           rdf(Place,'http://yago-knowledge.org/resource/hasLongitude',Long).                           
what_is(Name,GeoType) :- best_resource_for_name(Name,Place),
                         rdf(Place,rdf:'type',GeoType).

/*

<geoentity_Europe_6255148>	<isLocatedIn>	<geoentity_Earth_6295630> .

*/

where_is(Name,Location) :- best_resource_for_name(Name,Place),
                           rdf(Place,yago:'isLocatedIn',Location).

locations_for(Name,Locations) :- best_resource_for_name(Name,Place),
                                 collect_locations(Place,Locations).

collect_locations(Place,[Location|Others]) :- rdf(Place,yago:'isLocatedIn',Location),
                                              collect_locations(Location,Others),
                                              !.

collect_locations(Place,[]).

/* Find when a person was born.

63 ?- born_on('Abraham Lincoln',X).
X = '1809-02-12'.

*/

born_on(Name,Date_String) :-
        best_resource_for_name(Name,Person),
        rdf(Person,yago:'wasBornOnDate',literal(type(xsd:'date',Date_String))).

/*

Find persons who were born between years X and Y.

Acts as a generator to generate all people in that range.

any_born_between(-Person, +X, +Y)

*/

any_person_born_between(Person, X,Y) :-
        rdf(Person,yago:'wasBornOnDate',date(Y,M,D)),
        split_string(Date_String,'-','',[Year,Month,Day]), % Date_String is, e.g., '1979-01-15'
        number_string(Year_Number,Year),
        Year_Number >= X,
        Year_Number =< Y,
        best_resource_for_name(Name,Person),
        write(Name),
        write(' was born: '),
        write(Date_String),
        nl.

/*

Find persons born in a particular location.

70 ?- any_born_in('London',X).
X = yago:'Simon_Milton_(politician)' 
X = yago:'Stuart_Rogers' 
X = yago:'Nic_Sadler' 
X = yago:'Joan_Orenstein' 
X = yago:'Pen_Tennyson' 
X = yago:'Mark_Ryan_(guitarist)' 
X = yago:'Nell_Gwyn' 
X = yago:'George_Sewell' 
X = yago:'Ronald_Leigh-Hunt' 
X = yago:'David_Farrar_(actor)' 
X = yago:'Francis_Searle' 
X = yago:'Rebecca_Lowe' 
X = yago:'Matthew_Vaughn' .
...

any_born_in(+Place,-Person)

*/

any_person_born_in(Place_Name,Person) :-
        best_resource_for_name(Place_Name,Place),
        find_person_born_in(Place,Person).

any_person_born_in(Place,Person) :-
        rdf(Person,yago:'wasBornIn',Place).

any_person_born_in(Place,Person) :-
        rdf(Place,yago:'isLocatedIn',Location),
        find_person_born_in(Location,Person).

/* Utilities used for questions. */

add_name(X,Name) :- rdf_assert(X,'rdfs:label',Name).

/*
   find_X_isa_Y(+X_Name,+Y_Name,-X_resource,-Y_resource)

is a convenience function to find the YAGO resources with names given.

So, if we want to find "Chicago", which is of a type with label "city",
this will find the right YAGO resource and right type.

Similarly, if we want to find "Bill Murray", which is of a type with
label "actor" this will find the right YAGO resource and right type,
and not all the other "Bill Murray" resources out there.

*/

% find_X_isa_Y(+X_Name,+Y_Name,-X_resource,-Y_resource) :-

% find_class_members_given_class_name(+Class_Name,-Member_Name,-Member_resource,-Class_resource)
% is multi. Given the name of a class, such as "singer", it returns names it finds that are instances of that class,
% such as "Elvis Presley".

find_class_members_given_class_name(Class_Name,Member_Name,Member_resource,Class_resource) :-
        name_to_resource(Class_Name,Class_resource),
        rdf(Member_resource,rdf:'type',Class_resource),
        resource_to_best_name(Member_resource,Member_name).

:- write('Loaded query code and sample queries for YAGO'),nl.

/*

isa(Resource,NameOfType) searches for resources that have
a WordNet type with the name given. The name of the resource
is not given here, unlike in find_X_isa_Y.

*/

isa(Resource,NameOfType) :-
        name_to_resource(NameOfType,WN_resource),
        rdf(Resource,rdf:'type',WN_resource).

/* whereIs looks up the location for a place given its name. It
may succeed more than once if there are multiple places with that
name.

<geoentity_Denver_2169039>	rdfs:label	"Denver"@eng .
<geoentity_Denver_2169039>	<hasLatitude>	"-37.26667"^^<degrees> .
<geoentity_Denver_2169039>	<hasLongitude>	"144.3"^^<degrees> .

*/

where_is(Name,Lat,Long) :-
        rdf(GeoEntity,rdfs:'label',literal(lang(eng,Name))),
        rdf(GeoEntity,yago:'hasLatitude',literal(type(yago:'degrees',Lat))),
        rdf(GeoEntity,yago:'hasLongitude',literal(type(yago:'degrees',Long))).

/* Saved for YAGO 2 and beyond, where we have lat / long info

near(Place1,Place2,Distance) :-
        where_is(Place1,Lat1,Long1),
        where_is(Place2,Lat2,Long2),
        nearby(Lat1,Long1,Lat2,Long2,Distance).

*/

% show_all_predicates shows all the predicates in the KB.
show_all_predicates() :-
        setof(Predicate, rdf_current_predicate(Predicate),Predicates),
        print_all(Predicates).

% sample queries to show what can be done with the loaded YAGO system.
% for all target triples, we use: @base <http://yago-knowledge.org/resource/> .
% assume we want to find out what movies Gregory Peck appeared in, who
% starred in Moby Dick, and what relationship Gregory Peck has to Moby Dick.

% NOTES ABOUT LITERALS:
%   example of a string literal, a label =  literal(lang(eng,'Moby Dick (1956 film)'))
%   example of a number literal, duration in sec =  literal(type(yago:s,'6960'))
%   examples of date literals:
%         literal(type(xsd:date, '1991-06-21'))
%         literal(type(xsd:date, '1998-02-##')) 
%         literal(type(xsd:date, '1962-##-##')) 

% NOTES ABOUT SYNTAX:
%   the correct syntax for the triple predicate rdf:type is rdf:'type',
% NOT the atom 'rdf:type' and not the atom rdf:type.
% Similarly, use rdfs:'label', not 'rdfs:label'. And here is the correct
% use of the yago prefix:
/*
Use yago:'Moby_Dick_(1956_film)' below and not 
'yago:Moby_Dick_(1956_film)' as I first had it.

51 ?- rdf(X,Y, yago:'Moby_Dick_(1956_film)').
X = yago:'Leo_Genn',
Y = yago:actedIn 
X = yago:'Richard_Basehart',
Y = yago:actedIn 
X = yago:'Orson_Welles',
Y = yago:actedIn 
X = yago:'John_Huston',
Y = yago:directed 
X = yago:'Philip_Sainton',
Y = yago:wroteMusicFor 
X = yago:'Gregory_Peck',
Y = yago:actedIn.
*/
% NOTES ABOUT STRINGS VS ATOMS
%   Use an atom, e.g., 'Bill Murray' for the rdf predicate, in a literal. Do NOT
% use a string, "Bill Murray". Python treats these the same, but in Prolog the first
% is an atom and the second is a string, wholly different.

% find dates connected to this name:

dates(Name,Predicate,Date) :- rdf(Concept,rdfs:'label',literal(exact(Name),_)),
                              rdf(Concept,Predicate,literal(type(xsd:'date', Date))).
% find WordNet types for a name or label
wordnet(Name,WN_type) :- rdf(Concept,rdfs:'label',literal(exact(Name),_)),
                         rdf(Concept,rdf:'type',Class),
                         sub_atom(Class,_,_,_,'wordnet'),
                         WN_type=Class.

% all_WN finds all wordnet categories for a label or name.
/*
103 ?- all_WN('Alan Turing',Senses).
Senses = ['http://yago-knowledge.org/resource/wordnet_scientist_110560637'].
*/
all_WN(Name,WN_types) :- setof(WN_type,wordnet(Name,WN_type),WN_types).

% find Wikipedia categories for a name or label.
wiki(Name,Wikicat) :- rdf(Concept,rdfs:'label',literal(exact(Name),_)),
                      rdf(Concept,rdf:'type',Class),
                      sub_atom(Class,_,_,_,'wikicat'),
                      Wikicat=Class.

% all_wiki finds all Wikipedia categories for a label or name.
/*
104 ?- all_wiki('Alan Turing',Categories).
Categories = ['http://yago-knowledge.org/resource/wikicat_20th-century_mathematicians', 
'http://yago-knowledge.org/resource/wikicat_20th-century_philosophers', 
'http://yago-knowledge.org/resource/wikicat_Academics_of_the_University_of_Cambridge', 
'http://yago-knowledge.org/resource/wikicat_Academics_of_the_University_of_Manchester', 
...
'http://yago-knowledge.org/resource/wikicat_Theoretical_computer_scientists'].
*/
all_wiki(Name,Wiki_types) :- setof(Type,wiki(Name,Type),Wiki_types).

/*

Inspired by the SPARQL query:

Select ?c Where { ?c hasType City .
?a1 hasType Airport . ?a2 hasType Airport .
?a1 locatedIn ?c . ?a2 locatedIn ?c .
?a1 namedAfter ?p . ?p hasType WarHero .
?a2 namedAfter ?b . ?b hasType BattleField . }

for the Jeopardy question Watson missed, as mentioned in the paper,
"YAGO-QA: Answering Questions by Structured Knowledge Queries".

*/

jeopardy_question :- isa(City,"city").

/*

Literals

*/

% find_words(+Word,-ListOfLiterals) requires semweb/rdf_litindex to be loaded.
find_words(Word,ListOfLiterals) :-
        rdf_find_literals(sounds(Word),ListOfLiterals).
