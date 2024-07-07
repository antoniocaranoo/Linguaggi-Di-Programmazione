PROGETTO OOΠ in PROLOG

MEMBRI GRUPPO
Cantaluppi Camilla 894557
Carano Antonio 902447
Ladisa Roberto 899699


OOΠ implementa un sistema di programmazione orientata agli oggetti con
ereditarietà multipla. 

Al momento della creazione di una classe è possibile definire il tipo 
associato a ciascun attributo. Quest'ultimo, se non specificato, viene 
intepretato come 'generico' e rappresentato come lista vuota. Il tipo
 'generico' non vincola il dominio che dell'attributo, che può quindi 
assumere qualsiasi valore senza essere controllato.


I campi della classe, quindi fields e metodi, vengono salvati all'interno
della base di conoscenza attraverso i predicati field_in_class e 
method_in_class. Prima di essere salvati vengono opportunamente 
controllati, in modo tale che non sia possibile specificare per un 
attributo un valore di tipo differente da quello dichiarato.

Data la presenza dell’ereditarietà in questo progetto, è stato necessario
gestire le modalità in cui certi campi possano essere valutati.
Entrando più nel dettaglio: data una classe che ha un attributo di tipo 
'generico', la sottoclasse, la quale eredità il campo in questione, ha la
possibilità di sovrascrivere il campo con qualsiasi valore.
Inoltre, è fondamentale sottolineare che, se un campo ha un determinato
tipo, che sia diverso da il tipo ‘generico’, il valore della sottoclasse
può essere del tipo stesso oppure di un tipo di dato che rispetti la 
‘gerarchia’ tra i tipi.
Esempio: se in una superclasse definiamo un campo di tipo float, possiamo
assegnare, alla sottoclasse per lo stesso tipo di dato, un valore di tipo 
float o ad esempio integer; mentre non può essere fatto il contrario, 
questo perche il tipo di float ha una precisione maggiore rispetto al 
tipo integer.

Il processo di creazione della classe è strutturato come segue:
- i field e metodi specificati come parametro vengono salvati nella 
base di conoscienza
- vengono recuperati i field e metodi delle superclassi e ne viene 
fatto l'eventuale override con quelli "nuovi" specificati al momento 
della creazione della classe
- viene controllata la coerenza tra i tipi con quelli delle superclassi 
rispetto alla struttura gerarchica.


ALCUNI TEST EFFETTUATI

def_class(person, [], [field(name, 'Eve'), field(age, 21, integer)]).
true.

?- def_class(soccerplayer, [], [field(ruolo, 'Giocatore'), method(goal, [], 
(write('io sono un'), field(this, ruolo, N), writeln(N)))]).
true.

def_class(vipplayer, [person, soccerplayer], 
[method(news, [], (write('Io sono un vip')))]).
true.

?- make(eve, person).
true.

?- make(adam, person, [name = 'Adam']).
true.

?- make(s1, soccerplayer, [ruolo = 'Attaccante']).
true.

?- make(s2, vipplayer).
true.

?- field(s2, ruolo, A).
A = 'Giocatore'.

?- field(s1, ruolo, A).
A = 'Attaccante'.

?- field(s1, indirizzo, A).
false.

?- goal(eve).
false.

?- goal(soccerplayer).
false.

?- goal(s1).
io sono unAttaccante
true.

?- goal(s2).
io sono unGiocatore
true.

?- news(s2).
Io sono un vip
true.

inst(s2, E), news(E).
inst(eve, E), goal(E).
inst(eve, E), field(E, ruolo, N).
inst(s1, E), field(E, ruolo, N).