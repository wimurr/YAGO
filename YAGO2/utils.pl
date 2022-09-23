/* My utility code for use in multiple SWI-Prolog applications. */


/*

                   PRINTING

*/  

% print_all/1 prints each element in a list on a separate line with a number. The list can contain any kinds of items.
print_all(List) :- print_all(List,1), !.
print_all([],_).
print_all([Item|Rest],Index) :-
        format("~d. ",[Index]),
        print(Item),    % When I use format here the prefixes are all expanded, so I use print instead.
        nl,
        Next is Index + 1,
        print_all(Rest,Next).

% print_resources/1 prints each resource in a list on a separate line with a number. The list only contains RDF resources.
% Each is printed with pretty_print_resource.

print_resources(List) :- print_resources(List,1), !.
print_resources([],_).
print_resources([Item|Rest],Index) :-
        pretty_print_resource(Item,Pretty_Item),    % Drop URI prefixes, change underscores to spaces, etc.
        format("~d. ",[Index]),
        print(Pretty_Item),    % When I use format here the prefixes are all expanded, so I use print instead.
        nl,
        Next is Index + 1,
        print_resources(Rest,Next).

% print_triples/1 prints each triple in a list on a separate line with a number
% Each triple should be given as a [X,Y,Z] list.

print_triples(List) :- print_triples(List,1).
print_triples([],_).
print_triples([[X,Y,Z]|Rest],Index) :-
        format("~d. ", [Index]),
        print(X),  % When I use format here the prefixes are all expanded, so I use print here and below.
        print(" "),
        print(Y),
        print(" "),
        print(Z),
        print("."),
        nl,
        Next is Index + 1,
        print_triples(Rest,Next).

% repeat_indent(+N) just prints 3 * N spaces.

repeat_indent(0) :- !.

repeat_indent(N) :-
        N > 0,
        Next is N - 1,
        write('   '),
        repeat_indent(Next).


/*

                   TYPE CONVERSION

*/  


% can_be_number(+N) succeeds if N can be
% turned into a number or is one already, otherwise
% it fails.

can_be_number(N) :- ensure_is_number(N,_).

% ensure_is_number(+In,-Out) is det
% converts In to a number if possible,
% if it is not one already.
ensure_is_number(N,N) :-
        number(N),
        !.

ensure_is_number(N,Out) :-
        string(N),
        number_string(Out,N),
        !.

ensure_is_number(N,Out) :-
        atom(N),
        atom_number(N,Out),
        !.

ensure_is_number(N,N) :- fail.

/*

                   LIST MANIPULATION

*/  

% N.B. that swi-prolog allows you to use both sort/2 or list_to_set/2
% as built in equivalents!

% return_unique returns unique elements from Bag, one a time.
return_unique(Bag,Next_Unique) :- uniquify(Bag,Set),!,member(Next_Unique,Set).

% uniquify(+Bag,-Set)
% removes duplicate elements, retaining initial order.
uniquify([],[]).
uniquify([First|RestOfBag],[First|RestOfSet]) :-
        memberchk(First,RestOfBag),
        delete(RestOfBag,First,Updated),
        uniquify(Updated,RestOfSet),!.
uniquify([First|RestOfBag],[First|RestOfSet]) :-
        uniquify(RestOfBag,RestOfSet).


/* zip

This Prolog zip is like Python's zip and can also work either forwards or backwards:

?- zip([1,2,3],[a,b,c],X).
X = [1-a, 2-b, 3-c].

?- zip(X,Y,[1-a, 2-b, 3-c]).
X = [1, 2, 3],
Y = [a, b, c].

*/


zip([],[],[]).
zip([X],[Y],[X-Y]).
zip([X|Rest1],[Y|Rest2],[X-Y|Rest]) :-
        zip(Rest1,Rest2,Rest),
        !.

% ensure_atom/2 converts the input to an atom, if necessary, when we wish to allow input to be
% either an atom, number, or string.

ensure_atom(Input,Input) :- atom(Input),!.
ensure_atom(Input,Output) :- integer(Input),!,atom_number(Output,Input).
ensure_atom(Input,Output) :- float(Input),!,atom_number(Output,Input).
ensure_atom(Input,Output) :- string(Input),!,atom_string(Output,Input).

/*

ensure_two_chars/2 takes an atom or numeric input and then adds any leading zeros
to ensure that exactly two characters are returned. If the input is more than two
characters then only the last two are returned.

?- ensure_two_chars(5,X).
X = '05' 

?- ensure_two_chars(11,X).
X = '11' 

*/

ensure_two_chars(Input,Padded) :- ensure_atom(Input,Output),
                                  atom_chars(Output,Chars),
                                  append(['0','0'],Chars,With_Leading_Zeros),
                                  append(_,[Digit1,Digit2],With_Leading_Zeros),
                                  atom_chars(Padded,[Digit1,Digit2]).

/*

Oddly, SWI-Prolog does not provide a primitive just to capitalize the first letter
in atom, so we do here.

capitalize_atom('foo',X).

*/

capitalize_atom(Given, Capitalized) :-
        atom_chars(Given, [FirstChar|Rest]),
        upcase_atom(FirstChar,CapitalizedChar),
        atom_chars(Capitalized, [CapitalizedChar|Rest]).

% Convenience function as atom_prefix/2 is now deprecated.
% e.g., atom_starts_with_prefix('preamble','preamble_to_the_constitution').
atom_starts_with_prefix(Prefix,Atom) :- sub_atom(Atom, 0, _, _, Prefix).

/*

                                  RDF

*/  


% Fully expand prefixes when printing resources and triples.
expand_registered_prefixes_when_writing :-
        rdf_portray_as(writeq).

% Use concise prefixes when printing resources and triples.
contract_registered_prefixes_when_writing :-
        rdf_portray_as(prefix:id).


/* get_yago_resource(+Name,-Yago_Resource)
takes a name, such as 'Abraham Lincoln' and converts it to
the standard format of a Yago resource, in this case:
'http://yago-knowledge.org/resource/Abraham_Lincoln',
which will be printed as 'yago:Abraham_Lincoln' if rdf_portray
is not explanding prefixes.   
*/

get_yago_resource(Name,Yago_Resource) :- 
        rdf_current_prefix('yago',Prefix),
        atom_spaces_to_underscores(Name,Name_With_Underscores),
        atom_concat(Prefix,Name_With_Underscores,Yago_Resource).

/* yago_wiki_category/2 takes a resource name, such as 
yago:wikicat_Mathematicians_who_committed_suicide and returns the
category name 'Mathematicians who committed suicide'.

e.g., yago_wiki_category('http://yago-knowledge.org/resource/wikicat_Mathematicians_who_committed_suicide',X).
X = 'Mathematicians who committed suicide'.

*/
yago_wiki_category(Resource,Category_Name) :-
        rdf_current_prefix('yago',Prefix),
        atom_concat(Prefix,Full_Wiki_Category,Resource),                                 % get Full_Wiki_Category from the resource name.
        atom_underscores_to_spaces(Full_Wiki_Category,Wikicat_and_Category_Name),
        atom_concat('wikicat ',Category_Name,Wikicat_and_Category_Name).   % Drop off the 'wikicat' part.

/* yago_wordnet_domain/2 takes a resource name, such as yago:'wordnetDomain_history',
and returns the category name 'History'.

e.g., yago_wordnet_domain('http://yago-knowledge.org/resource/wordnetDomain_military',X).
X = 'Military'

*/
yago_wordnet_domain(Resource,Final_Topic_Name) :-
        rdf_current_prefix('yago',Prefix),
        atom_concat(Prefix,Full_Wordnet_Domain,Resource),                                            % get Full_Wordnet_Domain from the resource name.
        atom_underscores_to_spaces(Full_Wordnet_Domain,WordNet_Topic_Name),
        atom_concat('wordnetDomain ',Topic_Name,WordNet_Topic_Name),                    %  Drop off the 'wordnetDomain' part.
        upcase_atom(Topic_Name,Final_Topic_Name).                                                      %  Capitalize the topic name

/* atom_underscores_to_spaces/2 changes the underscores to spaces within an atom, e.g.,
   atom_underscores_to_spaces('wikicat_Mathematicians_who_committed_suicide',X).
   X = 'wikicat Mathematicians who committed suicide'.
*/
atom_underscores_to_spaces(With_Underscores,With_Spaces) :-
        atomic_list_concat(Words,'_',With_Underscores),
        atomic_list_concat(Words,' ',With_Spaces).

/* atom_spaces_to_underscores/2 changes the spaces to underscores within an atom, e.g.,
   atom_spaces_to_underscores('wikicat Mathematicians who committed suicide',X).
   X = 'wikicat_Mathematicians_who_committed_suicide'.
*/
atom_spaces_to_underscores(With_Spaces,With_Underscores) :-
        atomic_list_concat(Words,' ',With_Spaces),
        atomic_list_concat(Words,'_',With_Underscores).

/* atom_underscores_to_spaces_dropping_numbers/2
   changes the underscores to spaces within an atom and ALSO
   drops any parts that are just numbers, so
   atom_underscores_to_spaces_dropping_numbers(''wordnet_humanitarian_ 110191613',X).
   X = 'wordnet humanitarian',X).
*/

atom_underscores_to_spaces_dropping_numbers(With_Underscores,With_Spaces) :-
        atomic_list_concat(Words,'_',With_Underscores),
        drop_numbers_from_list(Words,Words_Without_Numbers),
        atomic_list_concat(Words_Without_Numbers,' ',With_Spaces).

/* drop_numbers_from_list(+Words,-Without_Numbers) is det
It just strips out numbers, or strings or atoms that can be numbers,
from the input, and returns the rest.   
*/  

drop_numbers_from_list([Word|Words],Words_Without_Numbers) :-
        can_be_number(Word),
        drop_numbers_from_list(Words,Words_Without_Numbers),
        !.

drop_numbers_from_list([Word|Words],[Word|Words_Without_Numbers]) :-
        drop_numbers_from_list(Words,Words_Without_Numbers),
        !.

drop_numbers_from_list([],[]).

/* find_resource_with_string_in_label(+Substring,-Resource)

finds all relevant resources where Substring occurs in their label and
returns them one by one.

Useful when we don't know exactly what we're looking for.

Examples:   

find_resource_with_string_in_label("USA",Resource)

find_resource_with_string_in_label("cat",Resource)   

*/

find_resource_with_string_in_label(Substring,Resource,Label) :-
        rdf(Resource,rdfs:label,Label@eng),sub_string(Label,_Before,_Length,_After,Substring).

/* drop_rdf_prefix(+Resource,-Main_Part)

drops a URL prefix and just returns the main part.

Examples:   

?- split_off_prefix(yago:'Abraham_Lincoln',X,Y).
X = yago,
Y = 'Abraham_Lincoln'.

?- split_off_prefix(rdfs:'subClassOf',,X,Y).
X = rdfs,
Y = subClassOf.   

?- split_off_prefix('http://yago-knowledge.org/resource/Bill',X,Y).
X = yago,
Y = 'Bill'.

N.B. This is a case where we DO NOT WANT TO USE

   :- rdf_meta drop_rdf_prefix(r,_).

since then the code no longer works.

*/

% here URI is a compound such as yago:'Bill_Murray'
split_off_prefix(URI,Prefix,Main_Part) :-
        compound(URI),
        URI =.. [:, Prefix, Main_Part],
        !.

% here URI has been expanded to an atom such as: 'http://yago-knowledge.org/resource/Bill_Murray'
split_off_prefix(URI,Prefix,Main_Part) :-
        atom(URI),
        rdf_current_prefix(Prefix,Expansion),
        atom_concat(Expansion,Main_Part,URI),
        !.

% here we have failed to shear off any prefix:
split_off_prefix(URI,_,URI) :-
        !.

% pretty_print_resource converts a resource such as:
% 'http://yago-knowledge.org/resource/Red_Feather_Lakes,_Colorado'
% to a pretty display name, in this case: 'Red Feather Lakes, Colorado'
% Note: it does not actually print anything, it just does the conversion.

:- rdf_meta pretty_print_resource(r,_).

pretty_print_resource(Resource,Description_With_Spaces) :-
        split_off_prefix(Resource,_,Main_Part),
        atom_underscores_to_spaces_dropping_numbers(Main_Part,Description_With_Spaces),
        !.

pretty_print_resource(Resource,Main_Part) :-
        split_off_prefix(Resource,_,Main_Part),
        !.

pretty_print_resource(Resource,Resource) :-
        !.
