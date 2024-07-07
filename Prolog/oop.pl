%%%% -*- Mode Prolog -*-
%%%% oop.pl

%%%% Cantaluppi Camilla 894557
%%%% Carano Antonio 902447
%%%% Ladisa Roberto 899699

%% Vengono dichiarati alcuni predicati dinamici, utilizzati per memorizzare 
%% informazioni sulle classi, istanze, campi e metodi definiti nel sistema.
:-
    dynamic(instance_of/2),
	   dynamic(method_in_instance/2),
		  dynamic(father_class_of/2),
			 dynamic(field_in_class/2),
				dynamic(method_in_class/2),
				    dynamic(c_creation/3),
					    dynamic(field_in_instance/2).
:-
    dynamic(is_class/1).

%% Definisce una nuova classe. Verifica che il nome della classe non sia gia' 
%% utilizzato, che i genitori (Parents) e le parti (Parts) siano insiemi 
%% validi, e registra la classe e i suoi componenti.
def_class(ClassName, _, _) :-
    is_class(ClassName), !,
    fail.

def_class(ClassName, Parents, Parts) :-
    is_a_set(Parents),
    is_a_set(Parts),
    are_parts(ClassName, Parts),
    are_parents(ClassName, Parents),
    assertz(is_class(ClassName)),
    assertz(c_creation(ClassName, Parents, Parts)), !.

%% Controlla se un elenco e'  un insieme, cioe' se non contiene elementi 
%% duplicati.
is_a_set([]).

is_a_set([(A, B) | Rest]) :-
    member_of_list((A, B), Rest), !, fail.

is_a_set([(_, _) | Rest]) :-
    is_a_set(Rest), !.

is_a_set([A | Rest]) :-
    member_of_list(A, Rest), !, fail.

is_a_set([_ | Rest]) :-
    is_a_set(Rest), !.

%% are_parents/2 e is_inheritor/2 gestiscono l'ereditarieta' tra classi, 
%% registrando le relazioni padre-figlio e ereditando campi e metodi dalla 
%% superclasse.
are_parents(_, []).

are_parents(ClassName, [Parent | _]) :-
    is_class(Parent),
    is_superclass(ClassName, Parent), !, fail.

are_parents(ClassName, [Parent | Ps]) :-
    is_class(Parent),
    assertz(father_class_of(Parent, ClassName)),
    is_inheritor(ClassName, Parent),
    are_parents(ClassName, Ps), !.

is_inheritor(ClassName, P) :-
    findall(field(FieldName, FieldValue, Type),
	    field_in_class(field(FieldName, FieldValue, Type), P), Fields),
    inherit_fields_from_supc(ClassName, Fields),
    findall(method(MethodName, Args, RawBody),
	    method_in_class(method(MethodName, Args, (RawBody)), P), ML),
    inherit_methods_from_supc(ClassName, ML).

%% eredita metodi e campi dalla superclasse, gestendo eventuali 
%% sovrascritture e controllando la compatibilita' dei tipi.
inherit_methods_from_supc(_, []).

inherit_methods_from_supc(ClassName, [method(MethodName, _, _) | Ms]) :-
    method_in_class(method(MethodName, _, _), ClassName),
    inherit_methods_from_supc(ClassName, Ms), !.

inherit_methods_from_supc(ClassName, [method(MN, Args, Body) | Ms]) :-
    assertz(method_in_class(method(MN, Args, (Body)), ClassName)),
    inherit_methods_from_supc(ClassName, Ms).

%% eredita metodi e campi dalla superclasse, gestendo eventuali 
%% sovrascritture e controllando la compatibilita' dei tipi.
inherit_fields_from_supc(_, []) :- !.

inherit_fields_from_supc(ClassName, [field(FN, _, TypeSp) | Fields]) :-
    field_in_class(field(FN, _, TypeCN), ClassName), !,			
    check_type(TypeSp, TypeCN),
    inherit_fields_from_supc(ClassName, Fields), !.

inherit_fields_from_supc(ClassName,
			 [field(FieldName,
				instance(IN, CN, Fields), Type) | Fields]) :-
    !,
    inst(IN, instance(IN, CN, Fields)),
    assertz(field_in_class(field(FieldName, instance(IN, CN, Fields), Type), 
			   ClassName)),
    inherit_fields_from_supc(ClassName, Fields), !.


inherit_fields_from_supc(ClassName, [field(FieldName, FieldValue, Type) | 
				     Fields]) :-
    assertz(field_in_class(field(FieldName, FieldValue, Type), ClassName)),
    inherit_fields_from_supc(ClassName, Fields), !.

% GESTIONE TYPE
%% Verifica la compatibilita' dei tipi, considerando anche i tipi definiti 
%% dall'utente attraverso le classi.
check_type(float, integer).

check_type(numeric, float).

check_type(numeric, integer).

check_type(X, X).

check_type([], _).

% check_type(FieldSuperClass, FieldCurrentClass).
check_type(TypeSp, TypeCN) :-
    is_class(TypeSp),
    is_class(TypeCN),
    is_superclass(TypeSp, TypeCN).

% INTEGER, REAL, STRING. LIST?

is_type(_, []).

is_type(X, integer) :-
    integer(X).

is_type(X, float) :-
    float(X).

is_type(X, number) :-
    number(X).

is_type(X, string) :-
    string(X).	

is_type(X, atom) :-
    atom(X).

is_type(X, compound) :-
    compound(X).

is_type(Class, Superclass) :-
    is_superclass(Superclass, Class).

%% Analizza e registra i componenti (campi e metodi) di una classe.
are_parts(_, []) :- !.

are_parts(ClassName, [method(MN, ArgL, Form) | Parts]) :-
    is_method_term(ClassName, method(MN, ArgL, Form)),
    are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, instance(IN, CN, Fields), Type) | Parts]) :-
    !,
    inst(IN, instance(IN, CN, Fields)),
    is_type(CN, Type),
    asserta(field_in_class(field(FN, instance(IN, CN, Fields), Type), 
			   ClassName)),	
    are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, FV, Type) | Parts]) :-
    is_type(FV, Type),
    asserta(field_in_class(field(FN, FV, Type), ClassName)),	
    are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, instance(IN, CN, Fields)) | Parts]) :-
    !,
    inst(IN, instance(IN, CN, Fields)),
    asserta(field_in_class(field(FN, instance(IN, CN, Fields), []), 
			   ClassName)),	
    are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, FV) | Parts]) :-
    asserta(field_in_class(field(FN, FV, []), ClassName)),	
    are_parts(ClassName, Parts), !.

%% risponde true se il valore passatogli e' l'istanza della classe passata 
%% come secondo valore, oppure e' l'istanza di una sottoclasse della 
%% supperclasse passata
is_instance(instance(Instance, ClassName, _)) :-
    instance_of(Instance, ClassName).

is_instance(instance(Instance, ClassName, Attrs), SCN) :-
    instance_of(Instance, ClassName),
    is_superclass(SCN, ClassName),
    findall(Term, field_in_instance(Term, Instance), Attrs).

%% Crea un'istanza di una classe, popolando i suoi campi.
inst(Instance, instance(Instance, ClassName, Attrs)) :-
    instance_of(Instance, ClassName),
    findall(Term, field_in_instance(Term, Instance), Attrs).

%% Verifica se una classe e' superclasse di un'altra.
%% is_superclass(SuperClasse, Sottoclasse).
is_superclass(SuperClass, SubClass) :-
    father_class_of(X, SubClass),
    is_superclass(SuperClass, X).

is_superclass(SuperClass, SubClass) :-
    father_class_of(SuperClass, SubClass), !.

%% Registra un metodo in una classe.
is_method_term(ClassName, method(MN, Args, RawBody)) :-
    is_list(Args),
    asserta(method_in_class(method(MN, Args, RawBody), ClassName)).

%% Esegue un metodo di un'istanza, sostituendo riferimenti all'istanza 
%% corrente (this) con l'istanza effettiva.
execute_method(MN, Args, Instance) :-
    inst(IN, Instance),
    execute_method(MN, Args, IN), !.

execute_method(MN, Args, IN) :-
    method_in_instance(method(MN, Args, Body), IN),
    call(Body), !.

%% replace/4 e replace_args/4 ostituiscono ricorsivamente un termine con un 
%% altro in una struttura di dati, usati per la gestione dei riferimenti a 
%% 'this' nei metodi.
replace((Term, OtherTerms), Keyword, Rep, (FirstTermReplaced, 
					   OtherTermsReplaced)) :-
    replace(Term, Keyword, Rep, FirstTermReplaced),
    replace(OtherTerms, Keyword, Rep, OtherTermsReplaced), !.

replace(Term, Keyword, Rep, TermReplaced) :-
    Term =.. [F | Args],
    replace_args(Args, Keyword, Rep, ReplacedArgs), !,
    TermReplaced =.. [F | ReplacedArgs].

replace_args([], _, _, []).

replace_args([Arg | Rest], Keyword, Rep, [Rep | ReplacedRest]) :-
    Arg == Keyword, !,
    replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Arg | Rest], Keyword, Rep, [Arg | ReplacedRest]) :-
    var(Arg), !,
    replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Arg | Rest], Keyword, Rep, [Arg | ReplacedRest]) :-
    Arg =.. [_ | []], !,
    replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Term | []], Keyword, Rep, [TermReplaced]) :- !,
    replace(Term, Keyword, Rep, TermReplaced).

replace_args([Term | OtherTerms], Keyword, Rep, [TermReplaced | 
						 OtherTermsReplaced]) :- !,
    replace(Term, Keyword, Rep, TermReplaced),
    replace_args(OtherTerms, Keyword, Rep, OtherTermsReplaced).

%% Crea un'istanza di una classe, eventualmente sovrascrivendo alcuni campi.
make(Instance, ClassName) :-
    make(Instance, ClassName, []).

make(X, ClassName, Parts) :-
    var(X), !,
    nonvar(ClassName),
    nonvar(Parts),
    make(oopinst, ClassName, Parts),
    inst(oopinst, X).

make(Instance, ClassName, Parts) :-
    instance_of(Instance, _), !,
    retractall(instance_of(Instance, _)),
    retractall(field_in_instance(_, Instance)),
    retractall(method_in_instance(_, Instance)),
    make(Instance, ClassName, Parts).

make(Instance, ClassName, Parts) :-
    is_class(ClassName),
    are_parts_in_instance(Instance, ClassName, Parts),
    assertz(instance_of(Instance, ClassName)), !.

%% Gestisce la creazione dei componenti di un'istanza, sovrascrivendo i campi 
%% se necessario.
are_parts_in_instance(Instance, CN, Parts) :-
    is_a_set(Parts),
    findall(field(FN, FV, Type),
	    field_in_class(field(FN, FV, Type), CN), FFC),
    findall(method(MN, Args, Body),
	    method_in_class(method(MN, Args, Body), CN), MFC),
    are_parts_in_instance(Instance, FFC, MFC, Parts).

are_parts_in_instance(_, [], [], []) :- !.

are_parts_in_instance(Instance, [field(FN, _, Type) | OtherPartsFC],
		      MethodsFC, PartsToOverride) :-
    member_of_list(FN=NFV, PartsToOverride),
    is_type(NFV, Type),
    asserta(field_in_instance(field(FN, NFV, Type), Instance)),
    rm_el_from_l(FN=NFV, PartsToOverride, OtherPartsToOverride),
    are_parts_in_instance(Instance, OtherPartsFC, MethodsFC,
			  OtherPartsToOverride), !.

are_parts_in_instance(Instance, [field(FN, FV, Type) | OtherPartsFC],
		      MethodsFC, PartsToOverride) :-
    asserta(field_in_instance(field(FN, FV, Type), Instance)),
    are_parts_in_instance(Instance, OtherPartsFC, MethodsFC,
			  PartsToOverride), !.

are_parts_in_instance(Instance, PartsFC,
		      [method(MN, Args, Body) | OtherParts],
		      PartsToOverride) :-
    asserta(field_in_instance(method(MN, Args, Body), Instance)),
    is_instance_method(Instance, method(MN, Args, Body)),
    are_parts_in_instance(Instance, PartsFC, OtherParts, PartsToOverride), !.

%% Gestisce i metodi a livello di istanza, sostituendo riferimenti a 'this'.
is_instance_method(IN, method(MN, Args, RawBody)) :-
    is_list(Args),
    method_in_instance(method(MN, _, _), IN), !,
    retractall(method_in_instance(method(MN, _, _), IN)),
    replace(RawBody, this, IN, Body),
    asserta(method_in_instance(method(MN, Args, Body), IN)).

is_instance_method(IN, method(MN, Args, RawBody)) :-
    is_list(Args),
    replace(RawBody, this, IN, Body),
    MSig =.. [MN, Instance | Args],
    asserta(method_in_instance(method(MN, Args, Body), IN)),
    asserta((MSig :- (MSig) =.. [MN, Instance | Args],
		     execute_method(MN, Args, Instance), !)), !.

%% controlla se il primo argomento appartiene alla lista passata come secondo 
%% parametro.
member_of_list(A, [A]) :- !.

member_of_list(A, [A | _]) :- !.

member_of_list(A, [_ | As]) :-
    member_of_list(A, As), !.

%% estrae il valore di un campo partendo dall'istanza passata.
field(Instance, FieldName, FieldValue) :-
    inst(IN, Instance), !,
    field_in_instance(field(FieldName, FieldValue, _), IN), !.

field(Instance, FieldName, FieldValue) :-
    field_in_instance(field(FieldName, FieldValue, _), Instance), !.

fieldx(Instance, [FieldName | FieldNames], FieldValue) :-
    field(Instance, FieldName, NInstance),
    fieldx(NInstance, FieldNames, FieldValue), !.

%% estrae il valore di un attributo da una classe percorrendo una catena di 
%% attributi.
fieldx(IN, [FieldName], FieldValue) :-
    field(IN, FieldName, FieldValue), !.

%% rimuove un elemento dalla lista passata e ritorna la lista senza elemento.
rm_el_from_l(_, [], []).

rm_el_from_l(X, [X | Z], E) :-
    rm_el_from_l(X, Z, E), !.

rm_el_from_l(X, [W | Z], [W | E]) :-
    rm_el_from_l(X, Z, E), !.

%% chiama il predicato fornito come primo argomento passando come parametri 
%% il primo elemento di ciascuna delle tre liste fornite. Ripete poi il 
%% procedimento per tutti gli altri elementi delle liste.
for_eachl(_, [], [], []).

for_eachl(P, [A | As], [B | Bs], [C | Cs]) :-
    call(P, A, B, C),
    for_eachl(P, As, Bs, Cs).

%% trasforma la lista di coppie passate in input in termini composti che 
%% hanno come funtore il primo elemento della coppia. Utilizato come 
%% predicato ausiliario per operazioni di creazione della classe.
explode_parts([], []).

explode_parts([Part | Parts], [(Field, Name) | Rest]):-
    Part =.. [Field, Name | _],
    explode_parts(Parts, Rest).

%%%% end of file -- oop.pl