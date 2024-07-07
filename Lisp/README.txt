PROGETTO OOΛ in COMMON LISP

MEMBRI GRUPPO
Cantaluppi Camilla 894557
Carano Antonio 902447
Ladisa Roberto 899699



OOΛ è un linguaggio object-oriented con eredità multipla che mira a risolvere
aspetti dell'implementazione di linguaggi ad oggetti:

1)Problema di dove e come recuperare i valori ereditati;

2)come rappresentare i metodi e le loro chiamate;

3)come manipolare il codice nei metodi stessi.

Nell'implementazione effettuata si sono anche affrontate situazioni di coerenza 
di tipo dichiarato e situazioni di dichiarazione dei metodi prima dei field,
o viceversa. 

Si assume, nell'implementazione fornita, che al momento della chiamata della 
def-class nei <part>* vengano dichiarati una sola lista di fields e/o una sola 
lista di methods (dichiarati solo in def-class), come da esempi in traccia.

Per realizzare il progetto si è implementato ogni classe ed istanza 
come delle list con elementi di base e, per quanto riguarda i metodi,
con una association list. Ogni classe ha quindi una struttura ben precisa che
facilita il reperimento di fields e methods. 
A tal proposito, si è creata una struttura a macro-liste. Nella fattispecie si 
è creata una lista contenente i fields ed una contenente i methods racchiuse 
in una macro-lista.

Per recuperare il contenuto di un field in un’istanza, si è ragionato con
delle "gerarchie" dei valori. Nella fattispecie vengono recuperati, 
grazie alla hashmap, tutti gli elementi utili, non sovrascritti dalla 
sotto-classe, e inseriti, in fase di creazione della classe, 
nella lista della sotto-classe così da agevolare l'ereditarietà.
In questo modo basterà usare get-class-spec per ottenere la lista di 
parts della classe in cui andiamo a cercare il field e cercare direttamete
lì.

Le istanze sono rappresentate nella forma ( oolinst <class> <field-value>* 
<methods>*): molto simile alla rappresentazione scelta per le classi, così
da facilitare il reperimento dei fields e soprattutto la corretta chiamata 
dei metodi.

Nei test non si è analizzato il comportamento del codice in casi patologici.


ALCUNI TEST EFFETTUATI

TEST #1

(def-class 'personcina nil '(fields (age 21 real) (name "Eve") 
(hobby "pesca" string))) 

(def-class 'straniero nil '(methods (talk (&optional 
                                   (out *standard-output*))
                                  (format out "Ti amo Italia."))))

(def-class 'scolaro '(personcina straniero)
                        '(fields
                          (age 34 integer)
                          (university "Bicocca" string))
                          '(methods
                            (talk (&optional (out *standard-output*))
                                  (format out "My name is ~A~%My age is ~D~"
                                          (field this 'name)
                                          (field this 'age)))))

(def-class 'teacher '(scolaro) '(fields (name "John") (age 42)))

*****************************************************************************
TEST  #2

(def-class 'lista-spesa '() '(fields (spesa (carne) cons)) '(methods (talk ()
	(format t "Devo comprare ~A~%" (field this 'spesa)))))

(defparameter d (make 'lista-spesa))

(def-class 'spesa '(lista-spesa) '(fields (spesa (pesce) cons)))

(def-class 'casa '(lista-spesa) '(methods (talk ()
	(format t "Devo laurearmi ~%" ))))

(def-class 'due '(lista-spesa) '(methods (parla ()
	(format t "Devo laurearmi ~%" ))))

(def-class 'baby '(student) '(fields (piange "UEEEE")))

(def-class 'listina '(lista-spesa) '(fields (spesa (pesce) list)))
Error: value (PESCE) for field X is not of type LIST


*******************************************************************************
TEST per la FIELD*

(def-class 'persona nil '(fields (age 21 integer) (name "matteo" string)))

(defparameter p (make 'persona))

(def-class 'studente nil '(fields (straniero p)))

(defparameter s (make 'studente))

(def-class 'matricola nil '(fields (scolaro s)))

(defparameter m (make 'matricola))

(field* m 'scolaro 'straniero 'age)

*******************************************************************************
TEST #3

(def-class ’mensch nil ’(fields (name “eve”) (age 21 integer))  
‘(methods (speak (&optional (out *standard-output*))  
(format out "My name is ~A~%My age is ~D~%" (field this ’name) 
(field this ’age)))))

(def-class ‘soccerplayer nil ‘(methods (goals 
(&optional (out *standard-output*))
  (format out “ho fatto tanti goal~%”))))

(def-class ‘vipplayer ‘(mensch soccerplayer) 
‘(methods (news (&optional (out *standard-output*))  
(format out “ho news~%”))))

(defparameter eve (make 'mensch))

(defparameter s1 (make ’soccerplayer))

(speak eve)
My name is eve
My age is 21
NIL

(defparameter vip (make 'vipplayer))

(talk eve)
Error: no method for field TALK found.

(goals vip)
ho fatto tanti goal
NIL

(news vip)
ho news
NIL
