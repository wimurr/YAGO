% Stats about the KB first.

kb1 :- get_all_rdf_stats.

kb2 :- show_unique_relations.

kb3 :- show_prefixes.

demo1 :-
        format('Triples about Denver found in YAGO 1.~n'),
        get_all_triples_for_resource(yago:'Denver,_Colorado').

demo2 :-
        format('Facts about Denver found in YAGO 1.~n'),
        get_facts_for_name('Denver').

demo2a :-
        format('Triples about Fort Collins found in YAGO 1.~n'),
        get_triples_for_name('Fort Collins').

demo3 :-
        format('All candidate resources for name Fort Collins found in YAGO 1, found by matching substrings..~n'),
        candidates_for('Fort Collins',Candidates),
        print_all(Candidates).

demo4 :-
        format('All candidate resources for name Lincoln found in YAGO 1, one at a time.~n'),
        any_resource_for_name('Lincoln',X),
        writeln(X),
        fail.

demo5 :-
        format('Best candidate resources for name Lincoln found in YAGO 1, one at a time..~n'),
        best_resource_for_name('Lincoln',X),
        writeln(X),
        fail.

demo6 :-
        format('Best candidate resources for name Abraham Lincoln found in YAGO 1, one at a time.~n'),
        best_resource_for_name('Abraham Lincoln',Resource),
        writeln(Resource),
        fail.

demo7 :-
        format('Best single candidate resources for name Obama found in YAGO 1, one at a time.~n'),
        best_resource_for_name('Obama',Resource),
        writeln(Resource),
        fail.

demo8 :-
        format('Best candidate resource for word cat found in YAGO 1, one at a time.~n'),
        best_resource_for_name('cat',Resource),
        writeln(Resource),
        fail.        

demo9 :-
        format('All best candidate resources for word cat found in YAGO 1, in order.~n'),
        best_resources_for_name('cat',Resources),
        writeln(Resources),
        fail.

demo10 :-
        format('Best facts for Obama found in YAGO 1, in order.~n'),        
        facts('Obama').

demo11 :-
        format('Best facts for Chicago found in YAGO 1, in order.~n'),        
        facts('Chicago').



