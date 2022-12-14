/* My utility code for use in multiple SWI-Prolog applications. */


/*

                   PRINTING

*/  

% print_all/1 prints each element in a list on a separate line with a number. The list can contain any kinds of items.
print_all(List) :- print_all(List,1).
print_all([],_).
print_all([Item|Rest],Index) :-
        format("~d. ",[Index]),
        print(Item),    % When I use format here the prefixes are all expanded, so I use print instead.
        nl,
        Next is Index + 1,
        print_all(Rest,Next).

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

                   LIST MANIPULATION

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

