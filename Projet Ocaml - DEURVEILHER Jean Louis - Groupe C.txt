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

(*Question 1*)

let f0 = Equ(And(Symb "a", Symb "c"),Or(Not(Symb "b"),Imp(Symb "c",And(Bot,Top))));;
let f1 = Equ(And(Symb "a", Symb "b"), Or(Not(Symb "a"), Symb "b"));;
let f2 = Or(Not(And(Symb "a", Not(Symb "b"))),  Not(Imp(Symb "a", Symb "b")));;
let f3 = And(Not(Imp(Symb "a", Or(Symb "a", Symb "b"))), Not(Not(And(Symb "a", Or(Symb "b", Not(Symb "c"))))));;
let f4 = And(And(And(And(And(Or(Or(Not(Symb "a"), Symb "b"), Symb "d"), Or(Not(Symb "d"), Symb "c")), Or(Symb "c", Symb "a")), Or(Not(Symb "c"), Symb "b")), Or(Not(Symb "c"), Not(Symb "b"))), Or(Not(Symb "b"), Symb "d"));; 

(*Question 2*)

let rec nbc = fun p -> match p with
    Symb _ | Top | Bot -> 0
  |Not (q) -> nbc(q) + 1
  |And(q,r)| Or(q,r) | Imp(q,r) | Equ(q,r) -> nbc(q) + nbc(r) + 1;;

nbc(f0);;
nbc(f1);;
nbc(f2);;
nbc(f3);;
nbc(f4);;

(*Question 3*)

let rec prof = fun p -> match p with
    Symb _ | Top | Bot -> 1
  |Not (q) -> prof(q) + 1
  |And(q,r)| Or(q,r) | Imp(q,r) | Equ(q,r) -> max (prof q) (prof r) + 1;;

prof(f0);;
prof(f1);;
prof(f2);;
prof(f3);;
prof(f4);;

(*Question 4*)

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

sp(f0);;
sp(f1);;
sp(f2);;
sp(f3);;
sp(f4);;

(*Question 5*)

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

(*Question 7*)

let i1 = [("a", Un); ("b", Zero); ("c", Un)];;
let i2 = [("a", Zero); ("b", Zero); ("c", Zero)];;
let i3 = [("a", Un); ("b", Un); ("c", Un)];;

let i4 = [("a", Un); ("b", Un); ("c", Un); ("d", Un)];;

(*Question 8*)

let intSymb s i = List.assoc s i;;

intSymb "a" i1;;
intSymb "b" i1;;
intSymb "c" i1;;
intSymb "a" i2;;
intSymb "b" i2;;
intSymb "c" i2;;
intSymb "a" i3;;
intSymb "b" i3;;
intSymb "c" i3;;

(*Question 9*)

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

(*Question 10*)

let rec valV f i = match f with
    Symb q -> intSymb q i
  |Top -> intTop
  |Bot -> intBot
  |Not (q) -> intNeg(valV q i)
  |And(q,r) -> intAnd (valV q i) (valV r i)
  |Or(q,r) -> intOr (valV q i) (valV r i)
  |Imp(q,r) -> intImp (valV q i) (valV r i)
  |Equ(q,r) -> intEqu (valV q i) (valV r i);;

valV f0 i1;;
valV f0 i2;;
valV f0 i3;;

valV f1 i1;;
valV f1 i2;;
valV f1 i3;;

valV f2 i1;;
valV f2 i2;;
valV f2 i3;;

valV f3 i1;;
valV f3 i2;;
valV f3 i3;;

valV f4 i1;;
valV f4 i2;;
valV f4 i3;;
valV f4 i4;;

(*Question 11*)

let modele f i = valV f i == Un;;

modele f0 i1;;
modele f0 i2;;
modele f1 i1;;
modele f1 i2;;

(*Question 12*)

let ipq = [[("p", Zero); ("q", Zero)]; [("p", Zero); ("q", Un)]; [("p", Un); ("q", Zero)]; [("p", Un); ("q", Un)]];;

(*Question 13*)

let consTous a l = List.map (List.cons a) l;;

let rec ensInt s = match s with
    [] -> [[]]
  |x::xs -> consTous (x, Zero) (ensInt xs) @ consTous (x, Un) (ensInt xs);;

ensInt ["p"];;
ensInt ["p"; "q"];;
ensInt ["p"; "q"; "r"];;

(*Question 14*)

let existeModele f eI = List.exists (fun x ->x = true) (List.map (modele f) eI);;
let satisfiable f = existeModele f (ensInt (sp f));;

satisfiable (Symb "a");;
satisfiable (Not(Symb "a"));;
satisfiable (And(Symb "a", Symb "b"));;
satisfiable (And(And(Symb "a", Symb "b"), Not(Symb "a")));;
satisfiable f1;;
satisfiable f2;;
satisfiable f3;;
satisfiable f4;;

(*Question 15*)

let tousModele f eI = List.for_all (fun x ->x = true) (List.map (modele f) eI);;

let valide f = tousModele f (ensInt (sp f));;

valide (Symb "a");;
valide (Not(Symb "a"));;
valide (Or(Symb "a", Symb "b"));;
valide (Or(Or(Symb "a", Symb "b"), Not(Symb "a")));;
valide f1;;
valide f2;;
valide f3;;
valide f4;;

(*Question 16*)

let insatisfiable f = not (satisfiable f);;

insatisfiable (Symb "a");;
insatisfiable (Not(Symb "a"));;
insatisfiable (And(Symb "a", Symb "b"));;
insatisfiable (And(And(Symb "a", Symb "b"), Not(Symb "a")));;
insatisfiable f1;;
insatisfiable f2;;
insatisfiable f3;;
insatisfiable f4;;

(*Question 17*)

let rec equivalent1 f1 f2 =
  let symb = ensInt (union (sp f1) (sp f2)) in
  let l1v = List.map (modele f1) symb in
  let l2v = List.map (modele f2) symb in
  if (l1v = l2v) then true else false ;;

equivalent1 (Symb "a") (Symb "a");;
equivalent1 (And(Symb "a", Symb "b")) (And(Symb "b", Symb "a"));;
equivalent1 (Or(Symb "a", Symb "b")) (And(Symb "a", Symb "b"));;
equivalent1 (Imp(Symb "a", Symb "b")) (Imp(Not(Symb "b"), Not(Symb "a")));;
equivalent1 (Or(Or(Symb "a", Symb "b"), Not(Symb "a"))) (Not(And(And(Symb "c", Symb "d"), Not(Symb "c"))));;

let equivalent2 f1 f2 = valide (Equ(f1, f2));;

equivalent2 (Symb "a") (Symb "a");;
equivalent2 (And(Symb "a", Symb "b")) (And(Symb "b", Symb "a"));;
equivalent2 (Or(Symb "a", Symb "b")) (And(Symb "a", Symb "b"));;
equivalent2 (Imp(Symb "a", Symb "b")) (Imp(Not(Symb "b"), Not(Symb "a")));;
equivalent2 (Or(Or(Symb "a", Symb "b"), Not(Symb "a"))) (Not(And(And(Symb "c", Symb "d"), Not(Symb "c"))));;

(*Question 18*)

let consequence2 f1 f2 = valide (Imp(f1, f2));;

consequence2 (Symb "a") (Or(Symb "a", Symb "b"));;
consequence2 (Symb "a") (And(Symb "a", Symb "b"));;
consequence2 (Or(Or(Symb "a", Symb "b"), Not(Symb "a"))) (Not(And(And(Symb "c", Symb "d"), Not(Symb "c"))));;
consequence2 (And(And(Symb "a", Symb "b"), Not(Symb "a"))) (Or(Symb "c", Symb "d"));;

(*Question 19*)

let tousSP fbfs =  union (List.concat (List.map sp fbfs)) [];;

tousSP [f0];;
tousSP [f0;f1];;
tousSP [f0;f1;f2];;
tousSP [f0;f1;f2;f3];;
tousSP [f0;f1;f2;f3; f4];;

(*Question 20*)

let modele_inv i f = valV f i == Un;;
let modeleCommun fbfs i = List.for_all (fun x ->x = true) (List.map (modele_inv i) fbfs);;

modeleCommun [And(Symb "a", Symb "b"); Not (And(Symb "a", Symb "b"))] [("a", Un); ("b", Un)];;
modeleCommun [And(Symb "a", Symb "b"); And(Symb "a", Symb "b")] [("a", Un); ("b", Un)];;
modeleCommun [And(Symb "a", Symb "b"); Or(Symb "a", Symb "b")] [("a", Un); ("b", Un)];;
modeleCommun [And(Symb "a", Symb "b"); Or(Symb "a", Symb "b")] [("a", Un); ("b", Zero)];;

valV f1 i1;;
valV f2 i1;;
modeleCommun [f1; f2] i1;;
valV f1 i2;;
valV f2 i2;;
modeleCommun [f1; f2] i2;;

valV f1 i4;;
valV f2 i4;;
valV f3 i4;;
valV f4 i4;;
modeleCommun [f1] i4;;
modeleCommun [f1; f2] i4;;
modeleCommun [f1; f2; f3] i4;;
modeleCommun [f1; f2; f3; f4] i4;;

(*Question 21*)

let contradictoire fbfs = List.for_all (fun x ->x = false) (List.map (modeleCommun fbfs) (ensInt (tousSP fbfs)));;

contradictoire [And(Symb "a", Symb "b"); Not (And(Symb "a", Symb "b"))];;
contradictoire [And(Symb "a", Symb "b"); And(Symb "a", Symb "b")];;
contradictoire [And(Symb "a", Symb "b"); Or(Symb "a", Symb "b")];;
contradictoire [f0];;
contradictoire [f0; f1];;
contradictoire [f0; f1; f2];;
contradictoire [f0; f1; f2; f3];;
contradictoire [f0; f1; f2; f3; f4];;

(*Question 22*)

let nonConsequence fbfs f i = (modeleCommun fbfs i) && not(modele f i);;
let consequence fbfs f = if (List.for_all (fun x ->x = false) (List.map (nonConsequence fbfs f) (ensInt (union (tousSP fbfs) (sp f))))) then true else false;;

consequence [f0; f1; f2; f3; f4] Bot;;
consequence [f0; f1; f2; f3; f4] Top;;
consequence [f0; f1; f2] Top;;
consequence [f0; f1; f2] Bot;;
consequence [And(Symb "a", Symb "b"); Not(Symb "a"); Imp(Symb "b", Symb "d")] Imp(Symb "c", Symb "d");;

(*Question 23*)

let rec conjonction fbfs = match fbfs with
    [] -> failwith ("The list is empty")
  | h::[] -> h
  | h::q -> And(h, conjonction q);;
let consequenceV fbfs f = valide(Imp(conjonction fbfs, f));;

consequenceV [f0; f1; f2; f3; f4] Bot;;
consequenceV [f0; f1; f2; f3; f4] Top;;
consequenceV [f0; f1; f2] Top;;
consequenceV [f0; f1; f2] Bot;;
consequenceV [And(Symb "a", Symb "b"); Not(Symb "a"); Imp(Symb "b", Symb "d")] Imp(Symb "c", Symb "d");;

(*Question 24*)

let consequenceI fbfs f = insatisfiable (conjonction (List.cons (Not(f)) fbfs));;

consequenceI [f0; f1; f2; f3; f4] Bot;;
consequenceI [f0; f1; f2; f3; f4] Top;;
consequenceI [f0; f1; f2] Top;;
consequenceI [f0; f1; f2] Bot;;
consequenceI [And(Symb "a", Symb "b"); Not(Symb "a"); Imp(Symb "b", Symb "d")] (Imp(Symb "c", Symb "d"));;