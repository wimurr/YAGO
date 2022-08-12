/* Code to graph a set of nodes and links.
   
----------------------------------
   Goal: Create a subgraph of nodes and links
----------------------------------

Given a set of Nodes and Links, such as those from calling demo12, which are in this format:

Nodes, starting at 'http://yago-knowledge.org/resource/Abraham_Lincoln' and searching upwards:
1. owl:'Thing'
2. yago:'Abraham_Lincoln'
3. yago:wikicategory_American_people_of_English_descent
4. yago:wikicategory_American_people_of_Welsh_descent
5. yago:wikicategory_Assassinated_United_States_Presidents
6. yago:wikicategory_Illinois_lawyers
7. yago:'wikicategory_People_from_Coles_County,_Illinois'
8. yago:'wikicategory_People_from_LaRue_County,_Kentucky'
9. yago:'wikicategory_People_from_Macon_County,_Illinois'
10. yago:'wikicategory_People_from_Spencer_County,_Indiana'
11. yago:'wikicategory_People_from_Springfield,_Illinois'
12. yago:'wikicategory_People_murdered_in_Washington,_D.C.'
13. yago:wikicategory_People_of_Illinois_in_the_American_Civil_War
14. yago:wikicategory_People_of_the_Black_Hawk_War
15. yago:wikicategory_Postmasters
16. yago:wikicategory_Presidents_of_the_United_States
17. yago:wikicategory_Republican_Party_Presidents_of_the_United_States
18. yago:wikicategory_Union_political_leaders
19. yago:wordnet_adult_109605289
20. yago:wordnet_causal_agent_100007347
21. yago:wordnet_communicator_109610660
22. yago:wordnet_employer_110054657
23. yago:wordnet_head_of_state_110164747
24. yago:wordnet_holder_110180178
25. yago:wordnet_lawyer_110249950
26. yago:wordnet_leader_109623038
27. yago:wordnet_living_thing_100004258
28. yago:wordnet_master_110298647
29. yago:wordnet_negotiator_110351874
30. yago:wordnet_object_100002684
31. yago:wordnet_officeholder_110371450
32. yago:wordnet_official_110372373
33. yago:wordnet_organism_100004475
34. yago:wordnet_owner_110389398
35. yago:wordnet_person_100007846
36. yago:wordnet_physical_entity_100001930
37. yago:wordnet_politician_110450303
38. yago:wordnet_postmaster_110459575
39. yago:wordnet_president_110467179
40. yago:wordnet_professional_110480253
41. yago:wordnet_representative_110522035
42. yago:wordnet_skilled_worker_110605985
43. yago:wordnet_whole_100003553
44. yago:wordnet_worker_109632518
45. yago:yagoLegalActor
46. yago:yagoLegalActorGeo

Links, starting at 'http://yago-knowledge.org/resource/Abraham_Lincoln' and searching upwards:
1. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_American_people_of_English_descent]
2. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_American_people_of_Welsh_descent]
3. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Assassinated_United_States_Presidents]
4. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Illinois_lawyers]
5. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_from_Coles_County,_Illinois']
6. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_from_LaRue_County,_Kentucky']
7. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_from_Macon_County,_Illinois']
8. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_from_Spencer_County,_Indiana']
9. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_from_Springfield,_Illinois']
10. [yago:'Abraham_Lincoln',rdf:type,yago:'wikicategory_People_murdered_in_Washington,_D.C.']
11. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_People_of_Illinois_in_the_American_Civil_War]
12. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_People_of_the_Black_Hawk_War]
13. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Postmasters]
14. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Presidents_of_the_United_States]
15. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Republican_Party_Presidents_of_the_United_States]
16. [yago:'Abraham_Lincoln',rdf:type,yago:wikicategory_Union_political_leaders]
17. [yago:'Abraham_Lincoln',rdf:type,yago:wordnet_officeholder_110371450]
18. [yago:'Abraham_Lincoln',rdf:type,yago:wordnet_president_110467179]
19. [yago:wikicategory_American_people_of_English_descent,rdf:subClassOf,yago:wordnet_person_100007846]
20. [yago:wikicategory_American_people_of_Welsh_descent,rdf:subClassOf,yago:wordnet_person_100007846]
21. [yago:wikicategory_Assassinated_United_States_Presidents,rdf:subClassOf,yago:wordnet_president_110467179]
22. [yago:wikicategory_Illinois_lawyers,rdf:subClassOf,yago:wordnet_lawyer_110249950]
23. [yago:'wikicategory_People_from_Coles_County,_Illinois',rdf:subClassOf,yago:wordnet_person_100007846]
24. [yago:'wikicategory_People_from_LaRue_County,_Kentucky',rdf:subClassOf,yago:wordnet_person_100007846]
25. [yago:'wikicategory_People_from_Macon_County,_Illinois',rdf:subClassOf,yago:wordnet_person_100007846]
26. [yago:'wikicategory_People_from_Spencer_County,_Indiana',rdf:subClassOf,yago:wordnet_person_100007846]
27. [yago:'wikicategory_People_from_Springfield,_Illinois',rdf:subClassOf,yago:wordnet_person_100007846]
28. [yago:'wikicategory_People_murdered_in_Washington,_D.C.',rdf:subClassOf,yago:wordnet_person_100007846]
29. [yago:wikicategory_People_of_Illinois_in_the_American_Civil_War,rdf:subClassOf,yago:wordnet_person_100007846]
30. [yago:wikicategory_People_of_the_Black_Hawk_War,rdf:subClassOf,yago:wordnet_person_100007846]
31. [yago:wikicategory_Postmasters,rdf:subClassOf,yago:wordnet_postmaster_110459575]
32. [yago:wikicategory_Presidents_of_the_United_States,rdf:subClassOf,yago:wordnet_president_110467179]
33. [yago:wikicategory_Republican_Party_Presidents_of_the_United_States,rdf:subClassOf,yago:wordnet_president_110467179]
34. [yago:wikicategory_Union_political_leaders,rdf:subClassOf,yago:wordnet_politician_110450303]
35. [yago:wordnet_adult_109605289,rdf:subClassOf,yago:wordnet_person_100007846]
36. [yago:wordnet_causal_agent_100007347,rdf:subClassOf,yago:wordnet_physical_entity_100001930]
37. [yago:wordnet_communicator_109610660,rdf:subClassOf,yago:wordnet_person_100007846]
38. [yago:wordnet_employer_110054657,rdf:subClassOf,yago:wordnet_leader_109623038]
39. [yago:wordnet_head_of_state_110164747,rdf:subClassOf,yago:wordnet_representative_110522035]
40. [yago:wordnet_holder_110180178,rdf:subClassOf,yago:wordnet_owner_110389398]
41. [yago:wordnet_lawyer_110249950,rdf:subClassOf,yago:wordnet_professional_110480253]
42. [yago:wordnet_leader_109623038,rdf:subClassOf,yago:wordnet_person_100007846]
43. [yago:wordnet_living_thing_100004258,rdf:subClassOf,yago:wordnet_whole_100003553]
44. [yago:wordnet_master_110298647,rdf:subClassOf,yago:wordnet_employer_110054657]
45. [yago:wordnet_negotiator_110351874,rdf:subClassOf,yago:wordnet_communicator_109610660]
46. [yago:wordnet_object_100002684,rdf:subClassOf,yago:wordnet_physical_entity_100001930]
47. [yago:wordnet_officeholder_110371450,rdf:subClassOf,yago:wordnet_holder_110180178]
48. [yago:wordnet_officeholder_110371450,rdf:subClassOf,yago:wordnet_official_110372373]
49. [yago:wordnet_official_110372373,rdf:subClassOf,yago:wordnet_skilled_worker_110605985]
50. [yago:wordnet_organism_100004475,rdf:subClassOf,yago:wordnet_living_thing_100004258]
51. [yago:wordnet_owner_110389398,rdf:subClassOf,yago:wordnet_person_100007846]
52. [yago:wordnet_person_100007846,rdf:subClassOf,yago:wordnet_causal_agent_100007347]
53. [yago:wordnet_person_100007846,rdf:subClassOf,yago:wordnet_organism_100004475]
54. [yago:wordnet_person_100007846,rdf:subClassOf,yago:yagoLegalActor]
55. [yago:wordnet_physical_entity_100001930,rdf:subClassOf,owl:'Thing']
56. [yago:wordnet_politician_110450303,rdf:subClassOf,yago:wordnet_leader_109623038]
57. [yago:wordnet_postmaster_110459575,rdf:subClassOf,yago:wordnet_master_110298647]
58. [yago:wordnet_president_110467179,rdf:subClassOf,yago:wordnet_head_of_state_110164747]
59. [yago:wordnet_professional_110480253,rdf:subClassOf,yago:wordnet_adult_109605289]
60. [yago:wordnet_representative_110522035,rdf:subClassOf,yago:wordnet_negotiator_110351874]
61. [yago:wordnet_skilled_worker_110605985,rdf:subClassOf,yago:wordnet_worker_109632518]
62. [yago:wordnet_whole_100003553,rdf:subClassOf,yago:wordnet_object_100002684]
63. [yago:wordnet_worker_109632518,rdf:subClassOf,yago:wordnet_person_100007846]
64. [yago:yagoLegalActor,rdf:subClassOf,yago:yagoLegalActorGeo]
65. [yago:yagoLegalActorGeo,rdf:subClassOf,owl:'Thing']
true.

graph them by calling:

graph(Nodes, Links)

*/


% Assume we have loaded the prolog_graphviz pack, as in the next line:
% swipl -g 'pack_install(prolog_graphviz)' -t halt
% Now we can load it as a library package.
:- [library(gv)].

:- rdf_meta export_subject_graph(r).

% example: export_subject_graph(yago:'Bill_Murray').
export_subject_graph(Subject) :-
        split_off_prefix(Subject,_,Main_Part),
        atom_concat(Main_Part,'.svg',File_Name),
        gv_export(File_Name, {Subject}/[Out]>>export_subgraph(Out,Subject), options{directed:false}).

% example: export_subject_graph(yago:'Bill_Murray').
export_subject_graph_w_undirected_links(Subject) :-
        split_off_prefix(Subject,_,Main_Part),
        atom_concat(Main_Part,'.svg',File_Name),
        gv_export(File_Name, {Subject}/[Out]>>export_subgraph(Out,Subject)).

:- rdf_meta export_subgraph(-,r).

export_subgraph(Out,Subject) :-
        find_all_parents(Subject,Nodes,Links),
        length(Nodes,N1),
        length(Links,N2),
        format("Found ~d parents and ~d links for ancestors of node ~w.~n",[N1,N2,Subject]),        
        id_nodes(Out,0,Nodes,Pairs),
        add_edges(Out,Links,Pairs).

:- rdf_meta id_nodes(-,-,t,t).

id_nodes(Out,Index,[Node|Nodes_Rest],[Pair|Pairs_Rest]) :-
        New_Index is Index + 1,
        atom_number(Id,New_Index),
        % format("Node ~w has node ID ~w.~n",[Node,Id]),
        split_off_prefix(Node,_,Main_Part),
        dot_node_id(Out, Id, [label(Main_Part)]),
        Pair = Node-Id,
        id_nodes(Out,New_Index,Nodes_Rest,Pairs_Rest),
        !.

id_nodes(_,_,[],[]).

:- rdf_meta get_node_id(r,t,-).

get_node_id(Node,[Node_Id_Pair|_],Id) :-
        Node_Id_Pair = Node-Id,
        % format("Looked up node ~w to find it has node ID ~w.~n",[Node,Id]),
        !.

get_node_id(Node,[_|Rest],Id) :-
        get_node_id(Node,Rest,Id).

:- rdf_meta get_node_id(-,t,t).

add_edges(Out,[Link|Rest],Pairs) :-
        Link = [S,_,O],
        get_node_id(S,Pairs,S_id),
        get_node_id(O,Pairs,O_id),
        % format("Added edge from node ~w, id ~w to node ~w, id ~w.~n",[S,S_id,O,O_id]),
        dot_edge_id(Out, S_id, O_id),
        !,
        add_edges(Out,Rest,Pairs).

add_edges(_,[],_).


