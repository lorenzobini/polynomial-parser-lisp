Tutte le funzioni, eccetto as-monomial e as-polynomial,
sono state implementate in modo tale da accettare in input sia
monomi e/o polinomi in forma tradizionale sia monomi e/o polinomi già 
sottoposti a parsing.


Monomio tradizionale:    (* 5 (expt y 3))
Monomio parsato:         (M 5 3 ((V 3 Y)))
Polinomio tradizionale:  (+ 3 (* 3 (expt x 2)))
Polinomio parsato:       (POLY ((M 3 0 NIL) (M 3 2 ((V 2 X)))))


Le funzioni as-monomial e as-polynomial richiedono in input rispettivamente
un monomio ed un polinomio in forma tradizionale, che verrà restituito 
parsato, normalizzato ed ordinato.

Le funzioni polyplus, polyminus e polytimes gestiscono qualsiasi tipo di 
input se e solo se entrambi i valori passati sono della medesima tipologia.
(i.e. Passare un polinomio tradizionale ed un monomio parsato o qualsiasi altra
combinazione produce risultato NIL)
L'unica eccezione risiede nel passare combinazioni tra qualsiasi tipologia e 
valori nulli. Questi ultimi verranno interpretati come 0 ("zero").

Tutte le funzioni richieste gestiscono il polinomio/monomio normalizzando  
e ordinando secondo i criteri previsti le varibili di ogni 
monomio ed eventualmente i monomi di un polinomio (se predisposte per polinomi),
in entrambi i tipi di input.

Si assume che i monomi ed i polinomi forniti in input in forma già parsata siano 
corretti, al più non normalizzati e non ordinati.

Nel progetto lo 0 ("zero") viene considerato a sé stante come:
(M 0 0 NIL)
Se lo zero è presente all'interno del polinomio esso viene ignorato.
Uguale condizione si verifica se durante la normalizzazione si ottengono
monomi con valore matematico uguale a zero.

Esempi di input:
0   --->  (POLY ((M 0 0 NIL)))
(+ x 0)  --->  (POLY ((M 1 1 ((V 1 X)))))
(+ (* 3 x) (* -3 x) (* 2 x))  --->  (POLY ((M 2 1 ((V 1 Y)))))
(+ (* 3 x 0) 2) --->  (POLY ((M 2 0 NIL)))


Nel progetto i simboli di variabile elevati a zero vengono interpretati come 1.
Uguale condizione si verifica se durante la normalizzazione si ottengono come
risultato simboli di variabile elevati a zero.

Esempi di input:
(expt x 0)  --->  (POLY ((M 1 0 NIL)))
(+ (* 3 (expt x 0)) 2) --->  (POLY ((M 5 0 NIL)))
(* (expt x 2) (expt x -2) a) --->  (POLY ((M 1 1 ((V 1 A)))))


****Informazioni sull'ordinamento****

Per l'ordinamento è stato scritto un algoritmo Merge Sort per venire incontro alle 
esigenze del programma.
L'algoritmo merge sort prevede esclusivamente codice dedicato all'ordinamento 
dei monomi di un polinomio.
L'ordinamento delle variabili di un monomio è stato eseguito con l'ausilio della
funzione predefinita sort.

I simboli di variabile di un monomio vengono ordinati in ordine lessicografico
crescente.
I monomi di un polinomio vengono inizialmente ordinati in ordine crescente 
in base al grado complessivo degli stessi con spareggi determinati dai  
simboli di variabile.
L'ordinamento di due monomi con stessi simboli di variabile viene effettuato
in maniera crescente rispetto alle combinazioni variabile/esponente.



****Informazioni su funzioni secondarie****

Sono state implementate delle funzioni secondarie per facilitare la stesura
di quelle primarie e garantire una maggiore facilità di lettura.
Le funzioni secondarie tendenzialmente richiedono in input una lista di monomi
già sottoposti a ordinamento e talvolta a normalizzazione (requisito fondamentale).
Sottoporre alle funzioni secondarie una lista di monomi non ordinata potrebbe
portare ad un output inatteso od errato.
Altre funzioni secondarie gestiscono esclusivamente liste di varpowers ordinate.

Tutte le funzioni che possono ricevere in input un polinomio in forma 
tradizionale si servono della funzione is-trad-poly che permette, appunto,
il loro riconoscimento.

Le funzioni che trattano sottrazioni tra polinomi si servono della funzione
invert-coefficient per invertire il segno del coefficiente e permettere una
corretta normalizzazione.

Alcune funzioni utilizzano per molteplici scopi la funzione accumula vista
durante le lezioni frontali.