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
type ensembleInterpretation = interpretation list;;

type unaire = valVerite -> valVerite;;
type binaire = valVerite -> valVerite -> valVerite;;

let f0 = Equ(And(Symb "a", Symb "c"),Or(Not(Symb "b"),Imp(Symb "c",And(Bot,Top))));;
let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;
let f2 = Or(Not(And(Symb "a", Not(Symb "b"))),  Not(Imp(Symb "a", Symb "b")));;
let f3 = And(Not(Imp(Symb "a", Or(Symb "a", Symb "b"))), Not(Not(And(Symb "a", Or(Symb "b", Not(Symb "c"))))));;
let f4 = And(And(And(And(And(Or(Or(Not(Symb "a"), Symb "b"), Symb "d"), Or(Not(Symb "d"), Symb "c")), Or(Symb "c", Symb "a")), Or(Not(Symb "c"), Symb "b")), Or(Not(Symb "c"), Not(Symb "b"))), Or(Not(Symb "b"), Symb "d"));; 

let i1 = [("a", Un); ("b", Zero); ("c", Un)];;
let i2 = [("a", Zero); ("b", Zero); ("c", Zero)];;
let i3 = [("a", Un); ("b", Un); ("c", Un)];;
let i4 = [("a", Un); ("b", Un); ("c", Un); ("d", Un)];;

let dansListe x xs =
  let regardePour a = if a = x then true else false in
  List.exists regardePour xs;;

let ajouteSiAbsent x xs =
  if dansListe x xs = true then xs else x::xs;;

let union l1 l2 =
  List.fold_right ajouteSiAbsent (l1@l2) [];;

let rec sp = fun p -> match p with
    Symb q -> [q]
  |Top |Bot -> []
  |Not (q) -> sp(q)
  |And(q,r)| Or(q,r) | Imp(q,r) | Equ(q,r) -> union (sp(q)) (sp(r));;

let intSymb s i = List.assoc s i;;
let intTop = Un;;
let intBot = Zero;;
let intNeg vq = if vq == Un then Zero else Un;;
let intAnd vq vr = if vq == Un && vr == Un then Un else Zero;;
let intOr vq vr = if vq == Zero && vr == Zero then Zero else Un;;
let intImp vq vr = if vq == Un && vr == Zero then Zero else Un;;
let intEqu vq vr = if vq == vr then Un else Zero;;

let rec valV f i = match f with
    Symb q -> intSymb q i
  |Top -> intTop
  |Bot -> intBot
  |Not (q) -> intNeg(valV q i)
  |And(q,r) -> intAnd (valV q i) (valV r i)
  |Or(q,r) -> intOr (valV q i) (valV r i)
  |Imp(q,r) -> intImp (valV q i) (valV r i)
  |Equ(q,r) -> intEqu (valV q i) (valV r i);;

let consTous a l = List.map (List.cons a) l;;
let rec ensInt s = match s with
    [] -> [[]]
  |x::xs -> consTous (x, Zero) (ensInt xs) @ consTous (x, Un) (ensInt xs);;

let modele f i = valV f i == Un;;
let existeModele f eI = List.exists (fun x ->x = true) (List.map (modele f) eI);;
let satisfiable f = existeModele f (ensInt (sp f));;
let insatisfiable f = not (satisfiable f);;

insatisfiable (Symb "a");;
insatisfiable (Not(Symb "a"));;
insatisfiable (And(Symb "a", Symb "b"));;
insatisfiable (And(And(Symb "a", Symb "b"), Not(Symb "a")));;
insatisfiable f1;;
insatisfiable f2;;
insatisfiable f3;;
insatisfiable f4;;
