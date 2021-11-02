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

let addToList a l = a::l;;
let rec consTous a l = List.map (addToList a) l;;
let rec ensInt s = match s with
    [] -> [[]]
  |x::xs -> consTous (x, Zero) (ensInt xs) @ consTous (x, Un) (ensInt xs);;

let modele f i = valV f i == Un;;
let existeModele f eI = List.exists (fun x ->x = true) (List.map (modele f) eI);;
let satisfiable f = existeModele f (ensInt (sp f));;
let insatisfiable f = not (satisfiable f);;

let bool_to_string b = if b = true then "true" else "false";;
let rec print_bool_list l = match l with
  | [] -> print_string "\n"; ()
  | h::q -> print_string (bool_to_string h); print_string " "; print_bool_list q;;

let rec equivalent1 f1 f2 =
  let symb = union (ensInt (sp f1)) (ensInt (sp f2)) in
  let l1v = List.map (modele f1) symb in
  let l2v = List.map (modele f2) symb in
  if (l1v = l2v) then true else false ;;

equivalent1 (Symb "a") (Symb "a");;
equivalent1 (And(Symb "a", Symb "b")) (And(Symb "b", Symb "a"));;
equivalent1 (Or(Symb "a", Symb "b")) (And(Symb "a", Symb "b"));;
equivalent1 (Imp(Symb "a", Symb "b")) (Imp(Not(Symb "b"), Not(Symb "a")));;(* Ceci est un éditeur pour OCaml
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
let modele_inv i f = valV f i == Un;;
let tousModele f eI = List.for_all (fun x ->x = true) (List.map (modele f) eI);;
let valide f = tousModele f (ensInt (sp f));;
let existeModele f eI = List.exists (fun x ->x = true) (List.map (modele f) eI);;
let satisfiable f = existeModele f (ensInt (sp f));;
let insatisfiable f = not (satisfiable f);;

let bool_to_string b = if b = true then "true" else "false";;
let rec print_bool_list l = match l with
    [] -> print_string "\n"; ()
  | h::q -> print_string (bool_to_string h); print_string " "; print_bool_list q;;

let rec equivalent1 f1 f2 =
  let symb = ensInt (union (sp f1) (sp f2)) in
  let l1v = List.map (modele f1) symb in
  let l2v = List.map (modele f2) symb in
  if (l1v = l2v) then true else false ;;
let equivalent2 f1 f2 = valide (Equ(f1, f2));;
let consequence2 f1 f2 = valide (Imp(f1, f2));;

let tousSP fbfs =  union (List.concat (List.map sp fbfs)) [];;
let modeleCommun fbfs i = List.for_all (fun x ->x = true) (List.map (modele_inv i) fbfs);;
let contradictoire fbfs = List.for_all (fun x ->x = false) (List.map (modeleCommun fbfs) (ensInt (tousSP fbfs)));;

let nonConsequence fbfs f i = (modeleCommun fbfs i) && not(modele f i);;
let consequence fbfs f = if (List.for_all (fun x ->x = false) (List.map (nonConsequence fbfs f) (ensInt (union (tousSP fbfs) (sp f))))) then true else false;; 

let rec conjonction fbfs = match fbfs with
    [] -> failwith ("The list is empty")
  | h::[] -> h
  | h::q -> And(h, conjonction q);;
let consequenceV fbfs f = valide(Imp(conjonction fbfs, f));;

let consequenceI fbfs f = insatisfiable (conjonction (List.cons (Not(f)) fbfs));;

consequenceI [f0; f1; f2; f3; f4] Bot;;
consequenceI [f0; f1; f2; f3; f4] Top;;
consequenceI [f0; f1; f2] Top;;
consequenceI [f0; f1; f2] Bot;;
consequenceI [And(Symb "a", Symb "b"); Not(Symb "a"); Imp(Symb "b", Symb "d")] (Imp(Symb "c", Symb "d"));;