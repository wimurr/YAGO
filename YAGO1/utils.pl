/* My utility code for use in multiple SWI-Prolog applications. */

/*

Notes:

pwd.


*/


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


% print_all/1 prints each element in a list on a separate line with a number. The list can contain any kinds of items.
print_all(List) :- print_all(List,1).
print_all([],_).
print_all([Item|Rest],Index) :- format("~d. ~w~n",[Index,Item]),Next is Index + 1,print_all(Rest,Next).

% print_triples/1 prints each triple in a list on a separate line with a number
print_triples(List) :- print_triples(List,1).
print_triples([],_).
print_triples([[X,Y,Z]|Rest],Index) :- format("~d. ~p, ~p, ~p.~n",[Index,X,Y,Z]),Next is Index + 1,print_triples(Rest,Next).

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

Utilities relevant only to the use of the semweb package.

*/

/* yago_wiki_category/2 takes a resource name, such as 
yago:wikicat_Mathematicians_who_committed_suicide and returns the
category name 'Mathematicians who committed suicide'.

e.g., yago_wiki_category('http://yago-knowledge.org/resource/wikicat_Mathematicians_who_committed_suicide',X).
X = 'Mathematicians who committed suicide'.

*/
yago_wiki_category(Resource,Category_Name) :- Prefix = 'http://yago-knowledge.org/resource/',
                                              atom_concat(Prefix,Full_Wiki_Category,Resource),
                                              atom_hyphens_to_spaces(Full_Wiki_Category,Wikicat_and_Category_Name),
                                              atom_concat('wikicat ',Category_Name,Wikicat_and_Category_Name).


/* yago_wordnet_domain/2 takes a resource name, such as yago:'wordnetDomain_history',
and returns the category name 'History'.

e.g., yago_wordnet_domain('http://yago-knowledge.org/resource/wordnetDomain_military',X).
X = 'Military'

*/
yago_wordnet_domain(Resource,Final_Topic_Name) :- Prefix = 'http://yago-knowledge.org/resource/',
                                                  atom_concat(Prefix,Full_Wordnet_Domain,Resource),
                                                  atom_hyphens_to_spaces(Full_Wordnet_Domain,WordNet_Topic_Name),
                                                  atom_concat('wordnetDomain ',Topic_Name,WordNet_Topic_Name),
                                                  upcase_atom(Topic_Name,Final_Topic_Name).

/* atom_hyphens_to_spaces/2 changes the hyphens to spaces within an atom, e.g.,
   atom_hyphens_to_spaces('wikicat_Mathematicians_who_committed_suicide',X).
   X = 'wikicat Mathematicians who committed suicide'.
*/
atom_hyphens_to_spaces(With_Hyphens,With_Spaces) :- atomic_list_concat(Words,'_',With_Hyphens),
                                                    atomic_list_concat(Words,' ',With_Spaces).

/* atom_spaces_to_hyphens/2 changes the spaces to hyphens within an atom, e.g.,
   atom_spaces_to_hyphens('wikicat Mathematicians who committed suicide',X).
   X = 'wikicat_Mathematicians_who_committed_suicide'.
*/
atom_spaces_to_hyphens(With_Spaces,With_Hyphens) :- atomic_list_concat(Words,' ',With_Spaces),
                                                    atomic_list_concat(Words,'_',With_Hyphens).
