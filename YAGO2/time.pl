/*

Find events that happened in or on a particular day, month, or year in history.

*/

what_happened_on_day(Year,Month,Day) :- ensure_two_chars(Month,Month2),
                                        ensure_two_chars(Day,Day2),
                                        atomic_list_concat([Year,'-',Month2,'-',Day2],Date),
                                        format("Looking for events on ~w.~n",[Date]),
                                        setof([X,Y,Matched],rdf(X,Y,literal(exact(Date),Matched)),Facts),
                                        print_triples(Facts).

what_happened_in_month(Year,Month) :- ensure_two_chars(Month,Month2),
                                      atomic_list_concat([Year,'-',Month2,'-##'],Date),
                                      format("Looking for events in ~w.~n",[Date]),
                                      setof([X,Y,Matched],rdf(X,Y,literal(exact(Date),Matched)),Facts),
                                      print_triples(Facts).

what_happened_in_year(Year) :- atomic_concat(Year,'-',Year_Prefix),
                               format("Looking for events in ~w.~n",[Date]),
                               setof([X,Y,Matched],rdf(X,Y,literal(prefix(Year_Prefix),Matched)),Facts),
                               print_triples(Facts).

