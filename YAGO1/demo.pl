% Stats about the KB first.

kb1 :- get_all_rdf_stats.

kb2 :- show_unique_relations.

kb3 :- show_prefixes.

demo1 :-
        format('Triples about Denver found in YAGO 1.~n'),
        print_all_triples_for_resource(yago:'Denver,_Colorado').

demo2 :-
        best_resource_for_name('Denver',X),
        format('The best resource found for Denver in YAGO 1 is ~a.~n',[X]).

demo3 :-
        format('Triples about Fort Collins found in YAGO 1.~n'),
        print_all_triples_for_name('Fort Collins').

demo4 :-
        format('Best candidate resources for name Lincoln found in YAGO 1.~n'),
        very_best_resources('Lincoln',Candidates),
        print_all(Candidates).

demo5 :-
        print_all_triples_for_resource(yago:'Bill_Murray').

demo6 :-
        print_all_relevant_facts_for_resource(yago:'Bill_Murray').

demo7 :-
        print_all_relevant_facts_for_name('Fort Collins').
        
demo8 :-
        format('Best candidate resources for name Lincoln found in YAGO 1, one at a time..~n'),
        very_best_resources('Abraham Lincoln',X),
        writeln(X),
        fail.

demo9 :-
        format('Show facts for each alternate interpretation of the word cat.'),
        show_facts_for_each_interpretation_of_name('cat').

demo10 :-
        best_show_facts_best_first('Lincoln').

demo11 :-
        find_all_parents(yago:'Bill_Murray',Ancestors).

demo12 :-
        find_all_parents(yago:'Abraham_Lincoln',Ancestors).

