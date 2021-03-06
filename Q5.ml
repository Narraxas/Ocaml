(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous. *)


type prop =
  | Symb of string
  | Top
  | Bot
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Equ of prop * prop;;

let f0 = Equ(And(Symb "a", Symb "c"),Or(Not(Symb "b"),Imp(Symb "c",And(Bot,Top))));;
let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;
let f2 = Or(Not(And(Symb "a", Not(Symb "b"))),  Not(Imp(Symb "a", Symb "b")));;
let f3 = And(Not(Imp(Symb "a", Or(Symb "a", Symb "b"))), Not(Not(And(Symb "a", Or(Symb "b", Not(Symb "c"))))));;
let f4 = And(And(And(And(And(Or(Or(Not(Symb "a"), Symb "b"), Symb "d"), Or(Not(Symb "d"), Symb "c")), Or(Symb "c", Symb "a")), Or(Not(Symb "c"), Symb "b")), Or(Not(Symb "c"), Not(Symb "b"))), Or(Not(Symb "b"), Symb "d"));; 

let rec affiche = fun p -> match p with
    Symb q -> q
  |Top -> "⊤"
  |Bot -> "⊥"
  |Not (q) -> "¬"^affiche(q)
  |And(q,r) ->  "("^affiche(q)^"∧"^affiche(r)^")"
  |Or(q,r) ->  "("^affiche(q)^"∨"^affiche(r)^")"
  |Imp(q,r) -> "("^affiche(q)^"⇒"^affiche(r)^")"
  |Equ(q,r) -> "("^affiche(q)^"⇔"^affiche(r)^")";;

affiche(f0);;
affiche(f1);;
affiche(f2);;
affiche(f3);;
affiche(f4);;