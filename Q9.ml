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

type valVerite = Zero | Un ;;
type interpretation = (string * valVerite) list;;

let f0 = Equ(And(Symb "a", Symb "c"),Or(Not(Symb "b"),Imp(Symb "c",And(Bot,Top))));;
let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;
let f2 = Or(Not(And(Symb "a", Not(Symb "b"))),  Not(Imp(Symb "a", Symb "b")));;
let f3 = And(Not(Imp(Symb "a", Or(Symb "a", Symb "b"))), Not(Not(And(Symb "a", Or(Symb "b", Not(Symb "c"))))));;
let f4 = And(And(And(And(And(Or(Or(Not(Symb "a"), Symb "b"), Symb "d"), Or(Not(Symb "d"), Symb "c")), Or(Symb "c", Symb "a")), Or(Not(Symb "c"), Symb "b")), Or(Not(Symb "c"), Not(Symb "b"))), Or(Not(Symb "b"), Symb "d"));; 

let i1 = [("a", Un); ("b", Zero); ("c", Un)];;
let i2 = [("a", Zero); ("b", Zero); ("c", Zero)];;
let i3 = [("a", Un); ("b", Un); ("c", Un)];;

let intSymb s i = List.assoc s i;;

let intTop = Un;;
intTop;;

let intBot = Zero;;
intBot;;

let intNeg vq = if vq == Un then Zero else Un;;
intNeg (intSymb "a" i1);;
intNeg (intSymb "b" i1);;

let intAnd vq vr = if vq == Un && vr == Un then Un else Zero;;
intAnd (intSymb "a" i2) (intSymb "b" i2);;
intAnd (intSymb "a" i2) (intSymb "b" i3);;
intAnd (intSymb "a" i1) (intSymb "c" i2);;
intAnd (intSymb "b" i3) (intSymb "c" i3);;

let intOr vq vr = if vq == Zero && vr == Zero then Zero else Un;;
intOr (intSymb "a" i2) (intSymb "b" i2);;
intOr (intSymb "a" i2) (intSymb "b" i3);;
intOr (intSymb "a" i1) (intSymb "c" i2);;
intOr (intSymb "b" i3) (intSymb "c" i3);;

let intImp vq vr = if vq == Un && vr == Zero then Zero else Un;;
intImp (intSymb "a" i2) (intSymb "b" i2);;
intImp (intSymb "a" i2) (intSymb "b" i3);;
intImp (intSymb "a" i1) (intSymb "c" i2);;
intImp (intSymb "b" i3) (intSymb "c" i3);;

let intEqu vq vr = if vq == vr then Un else Zero;;
intEqu (intSymb "a" i2) (intSymb "b" i2);;
intEqu (intSymb "a" i2) (intSymb "b" i3);;
intEqu (intSymb "a" i1) (intSymb "c" i2);;
intEqu (intSymb "b" i3) (intSymb "c" i3);;
