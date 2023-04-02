(* /--------------------------------------------------------------------------------------------------------/ *)
(* /------------------Code source de la partie #2 - Programmation dynamique --------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *) 
type soi = 
  |S of string
  |I of int
  |C of char 
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* On initialise la matrice avec que des ' ' dans les cellules cplx: O(n²) en nombre d'acces a des cases du tableau *)
let initMatrix w1 w2 = 
  (*
  in : chaine de caractere
  in : chaine de caractere
  out: matrice vide avec |mot le plus long| colones et |mot le plus court| lignes.
  *)
  let min,max = (if (String.length w1) <(String.length w2) then w1,w2 else w2,w1) in 
  let mat = Array.make_matrix ((String.length min)+1) ((String.length max)+1) (C(' ')) in 
  for i =  0 to (String.length max-1) do
    Array.set mat.(0) (i+1) (C(String.get max i ));
  if i<(String.length min) then 
    Array.set (mat.(i+1)) 0 (C(String.get min i ));
  
    done ;
    mat
(* /--------------------------------------------------------------------------------------------------------/ *) 
let soitoint = function
|I(i) -> i 
|_-> raise  (Failure"not int")
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* initialise la matrice de programmation dynamique, la remplis et la renvoie completée de données utiles pour l'extraction complexité: O(n²) ou n² est la taille du mot le plus long entre w1 et w2 en nombre de comparaisons *)
let fillMatrix w1 w2 = 
(*
  in : chaine de caractere
  in : chaine de caractere
  out: matrice de programmation dynamique remplie selon la formule figurant dans le rapport
  out: longueur de la sous chaine commune
  out: coordonées de fin de la ss chaine commune
 *)
  let mat = initMatrix w1 w2 in 
  let lig = (ref 0) in 
  let col = (ref 0) in
  let lgmax = (ref 0) in 
  let lgsec = (ref 0) in 
 
  for i = 1 to (Array.length mat) -1 do 
    for j = 1 to (Array.length mat.(0)) -1 do 
      if mat.(i).(0) = mat.(0).(j) then(
      
        let com = (try (soitoint mat.(i-1).(j-1))
                    with 
                    |Failure(_) ->0 ) in 


         Array.set mat.(i) j  (I(com+1));
         lgsec := (com+1);
         (if (!lgsec) > (!lgmax) then  (lgmax := !lgsec ;col:= j ; lig := i ;))
          )
          else(           
          lgsec := 0;
           
            (*Array.set mat.(i) j (I(com)) ; *)
            Array.set mat.(i) j (I(0)))

      done 
    done;
    mat,!lgmax,!lig,!col;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* imprime le tableau passé en parametre complexité: O(|tableau|) *)
let printArray a = 
  (*
  in: tableau de type SOI
  *)
  for i = 0 to (Array.length a )-1 do 
    match a.(i) with 
    |S(s) -> print_string s
    |I(i) -> print_int i
    |C(c) -> (if c == ' ' then print_string "\\" else print_char c )
  done ;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* affiche la matrice passée en argument cplx : O(|matrice|) acces aux elements du tableau *)
let printMatrix m = 
  (*
  in : matrice de type SOI
  *)
  for i = 0 to (Array.length m) -1 do 
  printArray m.(i);
  print_string "\n";
  done;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
let getMatrix m = 
  let a,_,_,_ = m in a
(* /--------------------------------------------------------------------------------------------------------/ *) 
let getLgmax m = 
  let _,a,_,_ = m in a
(* /--------------------------------------------------------------------------------------------------------/ *) 
let getLig m = 
  let _,_,a,_ = m in a
(* /--------------------------------------------------------------------------------------------------------/ *) 
let getCol m = 
  let _,_,_,a = m in a
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Recupere la sous chaine commune une fois la matrice construite (necessité de construire la matrice avant l'appel) *)
let rec recupsub  col  i acc w1 w2=
  (*
  in: entier representant le numero de la colone d'ou il faut extraire la sous chaine commune
  in: entier: taille de la sous chaine commune
  in out: chaine de caractere: doit etre a "" lors d'un premier appel, sera retournée dans la cas terminal
  in: chaine de caractere depuis laquelle on souhaite recuperer la sous chaine commune
  in: chaine de caractere depuis laquelle on shouhaite recuperer la sous chaine commune 
  *)
  if String.length w1>=String.length w2 then 
    (if i = 0 then  acc^(Char.escaped w1.[col-i]) else recupsub  col (i-1) (acc^(Char.escaped w1.[col-i]))w1 w2)
  else if i = 0 then  acc^(Char.escaped w2.[col-i]) else recupsub  col (i-1) (acc^(Char.escaped w2.[col-i])) w1 w2 
  ;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* /-----------------------Code source de la partie #3 - Arbre des suffixes --------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Premiere structure de donnees utilise dans cette partie *)
type arbre = {
  mutable label: string; 
  mutable fils : arbre list; 
}
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Seconde structure de donnees utilise dans cette partie, qui est une amelioration de la precedente *)
type richArbre = {
  mutable labelR: string;
  mutable filsR: richArbre list;
  mutable pere: richArbre list;
  mutable cpt: int;
}
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Variable globale permettant de donner un identifiant unique a chaque noeud.
   Utile pour l'affichage des graphes sous Graphviz (evite les doublons de noeuds). *)
let idGen = ref 0;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui ecrit "chaine" a la suite du fichier "arbre_suffixes.dot" *)
let append_chaine chaine =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "arbre_suffixes.dot" in
  output_string oc chaine;
  close_out oc;;  
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliere de la fonction creer_fil  *)
let rec creer_fil_aux chaine i acc = 
  if i = -1 then acc 
  else( 
    incr idGen;
    creer_fil_aux chaine (i-1) ({label=(Char.escaped chaine.[i])^"_"^(string_of_int !idGen); fils=[acc]})   
  )
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de creer un arbre fil, i.e. un arbre ayant des noeuds d'artie 1 *)
let creer_fil chaine =
  incr idGen;
  creer_fil_aux chaine ((String.length chaine)-2) {label=(Char.escaped chaine.[((String.length chaine)-1)])^"_"^(string_of_int !idGen); fils=[]};;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliere de la fonction creer_Rfil_aux *)
let rec creer_Rfil_aux chaine i acc = 
  if i = -1 then acc 
  else (
    incr idGen;
    let node = {labelR = (Char.escaped chaine.[i])^"_"^(string_of_int !idGen);filsR = [];pere = [];  cpt = 0; } in 
    acc.pere <- [node];
    node.filsR <- [acc];
    creer_Rfil_aux chaine (i-1) node
  )
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a creer_fil mais utilisant le type richArbre *)
let creer_Rfil chaine = 
  incr idGen;
  creer_Rfil_aux chaine ((String.length chaine)-2) {labelR = (Char.escaped chaine.[((String.length chaine)-1)])^"_"^(string_of_int !idGen); filsR = []; pere = []; cpt=0};;  
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction permettant de transformer une liste de fils en une chaine de caracteres de ces fils *)
let rec getListLabel fils = 
  match fils with 
  | a::q -> [a.label^" "]@(getListLabel q)
  | [] -> []
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a getListLabel mais utilisant le type richArbre *)
let rec getListRlabel fils = 
  match fils with
  |[] -> []
  |a::q -> [a.labelR^" "]@(getListRlabel q) 
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui parcours des arbres de type arbre et les ecrits sous forme .dot dans "arbre_suffixes.dot" *)
let rec parcours_arbres_suffixes arbre = 
  append_chaine "\n";
  append_chaine arbre.label;
  append_chaine " -- {";
  List.iter append_chaine (getListLabel arbre.fils);
  append_chaine "};\n";
  List.iter parcours_arbres_suffixes arbre.fils;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a parcours_arbres_suffixes mais utilisant le type richArbre *)
let rec parcours_Rarbres_suffixes arbre = 
  append_chaine "\n";
  append_chaine arbre.labelR;
  append_chaine "--{";
  List.iter append_chaine(getListRlabel arbre.filsR);
  append_chaine "};\n";
  List.iter parcours_Rarbres_suffixes arbre.filsR;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet affecter les elements de la liste par rapport au pivot dans la fonction tri_rapide *)
let rec partition liste pivot=
    match liste with
    |[]->[],[]
    |tt::qq->
        let (l1,l2)=partition qq pivot in
        if(tt<pivot) then (tt::l1,l2) else (l1,tt::l2);;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de faire le tri rapide d'une liste (on va trier les labels des noeuds) *)
let rec tri_rapide liste =
  match liste with
  |[]->[];
  |a::ll->
      let (l1,l2)=partition ll a in
          (tri_rapide l1)@(a::(tri_rapide l2));;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction permettant de trouver le noeud qui a le label "eti" *)
let rec find_perso arbreList eti i = 
  if i = (List.length arbreList) then -1
  else let arbreIndice = (List.nth arbreList i) in (
    if (Char.escaped arbreIndice.label.[0]) = eti then i
    else find_perso arbreList eti (i+1)
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a find_perso mais utilisant le type richArbre *)
let rec find_Rperso arbreList eti i =
    if i = (List.length arbreList) then -1
    else let arbreIndice = (List.nth arbreList i) in (
      if(Char.escaped arbreIndice.labelR.[0]) = eti then i 
      else find_Rperso arbreList eti (i+1)
    )
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui ajoute le suffixe "chaine" a "arbre" au bon endroit (au dernier fils commun s'il y en a un) *)
let rec ajout_frere arbre chaine i =
  if i =  (String.length chaine) then () else 
  let indiceFind = (find_perso arbre.fils (Char.escaped chaine.[i]) 0) in (
    if indiceFind <> -1 then (ajout_frere (List.nth arbre.fils indiceFind) chaine (i+1))
    else let newNoeud = (creer_fil (String.sub chaine i ((String.length chaine)-i))) in arbre.fils <-newNoeud::arbre.fils; arbre.fils <- tri_rapide arbre.fils  
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a ajout_frere mais utilisant le type richArbre *)
let rec ajout_Rfrere arbre chaine i = 
  let indiceFind = (find_Rperso arbre.filsR(Char.escaped chaine.[i])0) in (
      if indiceFind <> -1 then ajout_Rfrere(List.nth arbre.filsR indiceFind) chaine (i+1)
      else let newNoeud = (creer_Rfil (String.sub chaine i ((String.length chaine)-i))) in newNoeud.pere <-[arbre];  arbre.filsR <- newNoeud::arbre.filsR; arbre.filsR <- tri_rapide arbre.filsR
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction creant l'arbre des suffixes de "chaine" *)
let arbre_suffixes chaine =
  let racine = {label = "§"; fils = []} in 
    for i=0 to ((String.length chaine)-1) do
      ajout_frere racine (String.sub chaine i ((String.length chaine)-i)) 0;
    done;
  racine;; 
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction similaire a arbre_suffixes mais utilisant le type richArbre *)
let rArbre_suffixes chaine = 
  let racine = {labelR = "§"; filsR = [];pere = []; cpt = 0} in 
  for i = 0 to ((String.length chaine)-1) do
      ajout_Rfrere racine (String.sub chaine i ((String.length chaine)-i))0;
  done;
  racine;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliere de sous_chaine *)
let rec aux_sous_chaine arbre chaine2 i =
  if (i = (String.length chaine2)) then true
  else (
    let indiceFind = (find_perso arbre.fils (Char.escaped chaine2.[i]) 0) in (
      if (indiceFind = -1) then false
      else aux_sous_chaine (List.nth arbre.fils indiceFind) chaine2 (i+1);
    )
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de verifier que "chaine2" est une sous-chaine de "chaine1" *)
let sous_chaine chaine1 chaine2 = 
  (aux_sous_chaine (arbre_suffixes chaine1) chaine2 0);;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de propager le compteur maximal *)
let rec propage node value = 
  node.cpt <- max node.cpt value;
  match node.pere with 
  |[a] -> propage a value;
  |[] -> ();
  |a::q -> failwith "tres grave erreur";;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet d'ajouter les sous-chaines de chaine dans "arbre" deja construit *)
let rec ajout_Rfrere2 arbre chaine i cpt = 
  try let indiceFind =  try (find_Rperso arbre.filsR( Char.escaped chaine.[i] )0) with _ ->( propage arbre cpt; failwith "erreur") in(
    if indiceFind <> -1 then (
      propage arbre cpt;
      ajout_Rfrere2( List.nth arbre.filsR indiceFind) chaine (i+1) (cpt+1)
    )
    else let newNoeud = (creer_Rfil(String.sub chaine i ((String.length chaine)-i))) in newNoeud.pere <- [arbre]; arbre.filsR <- newNoeud::arbre.filsR; arbre.filsR <- tri_rapide arbre.filsR;
  ) with _->()
;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction intermediaire qui permet de calculer le maximum entre une valeur "i" et le compteur d'un arbre "a2.cpt" *)
let maxL i a2=
  if a2.cpt > i then a2.cpt else i
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de trouver l'arbre fils ayant le plus grand compteur *)
let longueur arbre = 
  List.fold_left maxL 0 arbre.filsR
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de retourner l'arbre qui a l'indice le plus grand parmi la liste "l" d'arbres *)
let rec indiceMax l max acc= 
  match l with 
  |[] -> acc
  |a::q -> if a.cpt > max then indiceMax q a.cpt a else indiceMax q max acc 
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction qui permet de le label sans l'identifiant *)
let recupLetter label = 
  (**
  in: chaine de caractere de la forme a_b
  out: a
   *)
  if label.[0] = '_' then Char.escaped label.[0] else 
  (List.hd(String.split_on_char '_' label))
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de recuperer l'identifiant du label *)
let recupNumber label = 
  (*
  in: chaine de caractere de la forme a_b
  out: b
  *)
  List.nth(String.split_on_char '_' label) 1  
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliere de recuperation_lettre_debut *)
let rec recuperation_lettre_debut_aux label i = 
  (*
  in : label du noeud
  in : indice de pacours 
  *)
  if (label = "§") then "&"
  else if ((Char.escaped label.[i]) = "_") && ((Char.escaped label.[i+1]) = "_") then "_"
  else if ((Char.escaped label.[i]) = "_")  && ((Char.escaped label.[i+1]) <> "_") then ""
  else (Char.escaped label.[i])^(recuperation_lettre_debut_aux label (i+1));;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui recupere le label avant "_ID"  *)
let recuperation_lettre_debut label =
  (*
  in : label du noeud   
  *)
  (recuperation_lettre_debut_aux label 0);;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui retourne la plus longue sequence de l'arbre *)
let rec buildPlusGrandeSequence arbre acc =
  (*
  in : arbre
  in out: chaine de caractere : la plus grande séquence de l'arbre 
  *)
  let elu = indiceMax arbre.filsR (-1) arbre in 
  if elu.cpt = arbre.cpt && (elu.labelR.[0]<> '_') then (buildPlusGrandeSequence elu (acc^(recuperation_lettre_debut arbre.labelR)) ) else ( acc^(recuperation_lettre_debut arbre.labelR)^(recuperation_lettre_debut elu.labelR))  
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui retourne l'arbre des suffixes des deux chaines "ch1", "ch2", la longueur de la plus longue sous-chaine commune et cette sous-chaine *)
let buildArbre_suffixes ch1 ch2 =
  (*
  in : chaine de caractere
  in : chaine de caractere
  out: arbre des suffixes des 2 arguments
  out: entier: longueur de la plus grande sous chaine commune
  out: la plus grande séquence commune
  *)
  let arbre1 = rArbre_suffixes ch1 in 
  for i = 0 to ((String.length ch2)-1) do
    ajout_Rfrere2 arbre1 ( String.sub ch2 i ((String.length ch2)-i))0 1; 
  done;
  arbre1,( longueur arbre1 ),( buildPlusGrandeSequence arbre1 "" );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* /------------------Code source de la partie #4 - Compression de l'arbre des suffixes --------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Troisieme structure de donnees utilise *)
type idArbre = {
  mutable labelI: string; 
  mutable filsI : idArbre list; 
  mutable id : int;
  mutable pereI : idArbre list;
  mutable cptI : int;
}
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonctin qui fait la concatenation du noeud n1 avec le noeud n2 *)
let fusNode n1 n2 = 
  (*
  in: arbre 
  in : arbre
  out: arbre dont le label est le label de n1 ^ au label de n2 et le pere de out est le pere de n1 et les fils de out soont les fils de n2
  raise Invalid_argument si  le 1er argument a plusieurs fils
  *)
  if(List.length n1.filsR) <> 1 then raise (Invalid_argument (n1.labelR^"n'as pas qu'un seul fils")) else 
  let newNode = {labelR = (recupLetter n1.labelR)^(recupLetter n2.labelR)^"_"^(recupNumber n1.labelR); pere = n1.pere;filsR = n2.filsR; cpt = n1.cpt } in 
  (List.hd newNode.pere).filsR <- List.filter(fun x -> x.labelR <> n1.labelR) (List.hd newNode.pere).filsR;
  newNode;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui compresse l'arbre "arbre" selon la formule de l'enonce *)
let rec compression1 arbre = 
  (*
  in : arbre a compresser
  *)
  arbre.filsR <- (List.map (fun x -> try fusNode x (List.hd x.filsR)with Invalid_argument _ -> x|Failure _ -> x) arbre.filsR);
  if (List.exists (fun x ->List.length(x.filsR) = 1) arbre.filsR) then compression1 arbre else List.iter compression1 arbre.filsR;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction renvoie si le noeud est le plus profond qui appartienne aux 2*)
let predicat node = 
  List.for_all(fun x -> x.filsR = []) node.filsR;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui recupere la plus grande sequence de l'arbre dans acc *)
let rec buildPlusGrandeSequence2 node acc = 
(*
in : arbre 
in out: chaine de caractere, doit etre initialisée a "", contients la plus grande sous sequence commune de l'arbre
*)
let elu = indiceMax node.filsR (-1) node in
if elu.cpt = node.cpt && (longueur node)<> 0  then buildPlusGrandeSequence2 elu (acc^(recupLetter node.labelR)) else 
( if predicat node then acc^(recupLetter node.labelR) else (acc^(recupLetter node.labelR)^(recupLetter elu.labelR)) );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet affecter les elements de la liste par rapport au pivot dans la fonction tri_rapide_numeros en utilisant la table de hachage *)
let rec partition_numeros liste pivot tableHash =
  (*
  in : liste d'indices
  in : pivot choisit dant la tri_rapide_numeros 
  in : table de hachage des indices
  out : liste modifie 
  *)
  match liste with
  |[]->[],[]
  |tt::qq->
      let (l1,l2)=partition_numeros qq pivot tableHash in
      if((Hashtbl.find tableHash (int_of_string (Char.escaped tt.labelI.[0]))) < (Hashtbl.find tableHash (int_of_string (Char.escaped pivot.labelI.[0])))) then (tt::l1,l2) else (l1,tt::l2);;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de faire le tri rapide d'une liste d'indices en utilisant leurs correspondace dans la table de hachage *)
let rec tri_rapide_numeros liste tableHash =
  (*
  in : liste d'indices
  in : table de hachage des indices
  out : liste d'indices tries 
  *)
  match liste with
  |[]->[];
  |a::ll->
      let (l1,l2)=partition_numeros ll a tableHash in
          (tri_rapide_numeros l1 tableHash)@(a::(tri_rapide_numeros l2 tableHash));;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de retourner une table de hachage des lettres de "chaine" *)
let init_table chaine = 
  (*
  in : chaine de caracteres
  out: table de hachage offrant la correspondance entre la place des caracteres et les caracteres 
  *)
  let size = String.length chaine in 
  let table = Hashtbl.create size in 
  for i=0 to size-1 do 
    Hashtbl.add table i (Char.escaped chaine.[i]);
  done;
  table;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliere de creer_idFil *)
let rec creer_idFil_aux tableHash idTable i acc = 
  (*
  in : table de hachages des indices
  in : position courante dans la table de hachage du suffixe a ecrire 
  in : indice de parcours 
  in : accumulateur de la creation des noeuds fils  
  *)
  if i = idTable-1 then acc 
  else( 
    incr idGen;
    let n = {labelI = (string_of_int i); filsI =[acc]; id = !idGen; pereI = []; cptI = 0} in 
    acc.pereI <- [n];
    creer_idFil_aux tableHash idTable (i-1) (n)
  )
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de creer un arbre fil, i.e. un arbre ayant des noeuds d'artie 1 pour les arbres de type idArbre *)
let creer_idFil tableHash idTable = 
  (*
  in : table de hachage
  in : position courante dans la table de hachage du suffixe a ecrire  
  out: arbre fil
  *)
  incr idGen;
  creer_idFil_aux tableHash idTable ((Hashtbl.length tableHash)-2) {labelI=(string_of_int ((Hashtbl.length tableHash)-1)); filsI=[]; id = !idGen; pereI = []; cptI = 0};;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction permettant de trouver le noeud qui a le label "eti" pour les arbres de type idArbre *)
let rec find_idPerso tableHash idArbreList eti i = 
  (*
  in : table de hachage
  in : liste d'arbres
  in : indice/element a rechercher
  in : indice de parcours 
  in out : indice de la liste idArbreList du noeud qui a le label = eti
  *)
  if i = (List.length idArbreList) then -1
  else let arbreIndice = (List.nth idArbreList i) in (
    if Hashtbl.find tableHash (int_of_string arbreIndice.labelI) = Hashtbl.find tableHash eti then i
    else find_idPerso tableHash idArbreList eti (i+1)
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui ajoute le suffixe qui se trouve dans la table de hachage "tableHash" a partir de la position "idTable" a "idArbre" au bon endroit (au dernier fils commun s'il y en a un) pour les arbre de type idArbre*)
let rec ajout_idFrere idArbre tableHash idTable =
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : position courante dans la table de hachage du suffixe a ecrire  
  *)
  if idTable =  (Hashtbl.length tableHash) then ()
  else let indiceFind = (find_idPerso tableHash idArbre.filsI idTable 0) in (   
    if indiceFind <> -1 then (ajout_idFrere (List.nth idArbre.filsI indiceFind) tableHash (idTable+1))
    else let newNoeud = (creer_idFil tableHash idTable) in idArbre.filsI <-newNoeud::idArbre.filsI; idArbre.filsI <- tri_rapide_numeros idArbre.filsI tableHash 
  );;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui cree un arbre des suffixes non compresse avec des indices des lettres dans la table de hachage. Arbre de type idArbre *)
let idArbre_suffixes chaine =
  (*
  in : chaine de caracteres a ajouter dans l'arbre sous forme d'indices 
  *)
  idGen := 0;
  let racine = {labelI = "§"; filsI = []; id = !idGen; pereI = []; cptI = 0} and tableHash = init_table chaine in 
    for i=0 to ((Hashtbl.length tableHash)-1) do
      ajout_idFrere racine tableHash i;
    done;
  racine, tableHash;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction permettant de transformer une liste de fils en une chaine de caracteres de ces fils pour les arbres de type idArbre*)
let rec getListIdLabel fils = 
  (*
  in : liste de fils qui sont des indices   
  *)
  match fils with
  |[] -> []
  |a::q -> ["\"["^a.labelI^"]_"^(string_of_int a.id)^"\" "]@(getListIdLabel q) 
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de faire le parcours d'un arbre de type idArbre et d'ecrire sous forme dot dans "arbre_suffixes.dot" pour l'affichage *)
let rec parcours_idArbres_suffixes_numeros idArbre = 
  (*
  in : arbre de type idArbre   
  *)
  append_chaine "\n";
  append_chaine ("\"["^idArbre.labelI^"]_"^(string_of_int idArbre.id)^"\"");
  append_chaine "--{";
  List.iter append_chaine (getListIdLabel idArbre.filsI);
  append_chaine "};\n";
  List.iter parcours_idArbres_suffixes_numeros idArbre.filsI;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet retourner le premier noeud qui a plusieurs fils *)
let rec get_indice_profondeur idArbre =
  (* 
  in : arbre de type idArbre
  out: idArbre qui a plusieurs fils
  *)
  if ((List.length idArbre.filsI) = 1) then get_indice_profondeur (List.nth idArbre.filsI 0)
  else idArbre;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de compresser l'arbre "idArbre" en concatenant tous les noeuds d'artie 1 avec leurs peres *)
let rec compression_idArbre_numeros idArbre = 
  (*
  in : arbre de type idArbre   
  *)
  if ((List.length idArbre.filsI) = 1) then (
    let filsNonUnique = get_indice_profondeur idArbre in  
      idArbre.labelI <- idArbre.labelI^", "^filsNonUnique.labelI;
      idArbre.filsI <- filsNonUnique.filsI;
      if ((List.length idArbre.filsI) > 1) then List.iter compression_idArbre_numeros idArbre.filsI;   
    )
  else List.iter compression_idArbre_numeros idArbre.filsI;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* /--------------Code source de la partie #5 - Compression directe de l'arbre des suffixe------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet d'ajouter les fils restants du noeud et du suffixe a l'arbre "idArbre" *)
let changer_fils idArbre suffixe indicePb =
  (*
  in : arbre de type idArbre
  in : le suffixe a ajouter 
  in : indice a partir duquel les lettres ne sont plus egales   
  *)
  let labelRestant = (String.sub idArbre.labelI indicePb ((String.length idArbre.labelI)-indicePb)) in (
    idArbre.labelI <- String.sub idArbre.labelI 0 indicePb;
    incr idGen; incr idGen;
    let fils = idArbre.filsI in 
    let n = {labelI = labelRestant; filsI = fils; id = (!idGen-1);pereI = [idArbre]; cptI =0} in
    let n2 =  {labelI = (String.sub suffixe indicePb ((String.length suffixe)-indicePb)); filsI = []; id = !idGen;pereI = [idArbre]; cptI = 0}in 
    List.iter (fun x -> x.pereI   <- [n]) fils; 
    idArbre.filsI <- n::n2::[]; idArbre.filsI <- tri_rapide idArbre.filsI;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliere de indice_suffixe_egal *)
let rec indice_suffixe_egal_aux idArbre suffixe i = 
  (*
  in : arbre de type idArbre
  in : le suffixe a ajouter
  in : indice de parcours
  out : indice probleme    
  *)
  if i = (String.length suffixe) then -2
  else if i = (String.length idArbre.labelI) then -1 (* Pour dire qu'on doit passer au fils suivant *) 
  else if idArbre.labelI.[i] = suffixe.[i] then indice_suffixe_egal_aux idArbre suffixe (i+1)
  else i;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'indice a partir duquel les lettres ne sont plus egales *)
let indice_suffixe_egal idArbre suffixe = 
  (*
  in : arbre de type idArbre
  in : le suffixe a ajouter
  out : indice probleme    
  *) 
  indice_suffixe_egal_aux idArbre suffixe 0;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui ajoute le suffixe a l'endroit ou les lettres ne sont plus egales ou l'ajoute s'il n'existe pas *)
let rec ajout_fils idArbre suffixe i =
  (*
  in : arbre de type idArbre
  in : suffixe a ajouter
  in : indice de parcours     
  *)
  if i = (List.length idArbre.filsI) then (incr idGen; idArbre.filsI <- {labelI = suffixe; filsI=[]; id = !idGen;pereI = [idArbre]; cptI = 0}::idArbre.filsI; idArbre.filsI <- tri_rapide idArbre.filsI )
  else let indicePb = (indice_suffixe_egal (List.nth idArbre.filsI i) suffixe) in (
    if indicePb = 0 then ajout_fils idArbre suffixe (i+1) 
    else if indicePb = -1 then (let idArbreFils = (List.nth idArbre.filsI i) in let tailleLabel = String.length idArbreFils.labelI in 
      ajout_fils idArbreFils (String.sub suffixe tailleLabel ((String.length suffixe)-tailleLabel)) 0)
    else if indicePb = -2 then () (* Cas qd le suffixe est vide *)
    else changer_fils (List.nth idArbre.filsI i) suffixe indicePb;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'arbre compresse directement des suffixes avec lettres *)
let arbre_suffixes_compresse chaine = 
  (*
  in : chaine a ajouter
  out : arbre des suffixes compresse de chaine    
  *)
  idGen := 0;
  let racine = {labelI = "§"; filsI = []; id = !idGen; pereI = []; cptI = 0} in  
    for i=0 to  ((String.length chaine)-1) do 
      ajout_fils racine (String.sub chaine i ((String.length chaine)-i)) 0;
    done;
  racine ;;
  
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction permettant de transformer une liste de fils compresses en une chaine de caracteres de ces fils *)
let rec getListIdLabelCompresse fils = 
  match fils with
  |[] -> []
  |a::q -> [a.labelI^"_"^(string_of_int a.id)^" "]@(getListIdLabelCompresse q)
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui parcours les idArbes et qui stocke les labels+identifiant dans un "arbre_suffixes.dot" pour affichage sur Graphviz *)
let rec parcours_idArbres_suffixes idArbre = 
  (*
  in : arbre de type idArbre   
  *)
  append_chaine "\n";
  append_chaine (idArbre.labelI^"_"^(string_of_int idArbre.id));
  append_chaine "--{";
  List.iter append_chaine(getListIdLabelCompresse idArbre.filsI);
  append_chaine "};\n";
  List.iter parcours_idArbres_suffixes idArbre.filsI;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction auxiliaire de recuperation_chiffre_fin *)
let rec recuperation_chiffre_fin_aux label i =
  (*
  in : label du noeud de la forme (a, b) avec a et b des nombres   
  in : indice de parcours   
  *)
  if ((Char.escaped label.[i]) = ",") then int_of_string (String.sub label (i+1) ((String.length label)-(i+1)))
  else recuperation_chiffre_fin_aux label (i+1);;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet la recuperation du ciffre b dans l'intervalle (a, b) *)
let recuperation_chiffre_fin label =
  (*
  in : label du noeud de la forme (a, b) avec a et b des nombres   
  *)
  recuperation_chiffre_fin_aux label 0;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliaire de recuperation_chiffre_debut *)
let rec recuperation_chiffre_debut_aux label i = 
  (*
  in : label du noeud de la forme (a, b) avec a et b des nombres   
  in : indice de parcours    
  *)
  if ((Char.escaped label.[i]) = ",") then ""
  else (Char.escaped label.[i])^(recuperation_chiffre_debut_aux label (i+1));;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet la recuperation du ciffre a dans l'intervalle (a, b) *)
let recuperation_chiffre_debut label =
  (*
  in : label du noeud de la forme (a, b) avec a et b des nombres     
  *)
  int_of_string (recuperation_chiffre_debut_aux label 0);;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet d'ajouter les fils restants du noeud et du suffixe (a l'aide de l'indice courant "indiceTable" de la table de hachage) a l'arbre "idArbre" *)
let changer_fils_indices idArbre tableHash indicePb indiceTable =
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice a partir duquel les lettres sont differentes entre le suffixe et les noeuds du chemin
  in : indice courant de la table de hachage (suffixe restant)    
  *)
  let labelRestant = (string_of_int ((recuperation_chiffre_debut idArbre.labelI)+indicePb))^","^(string_of_int (recuperation_chiffre_fin idArbre.labelI)) in (
    idArbre.labelI <- (string_of_int (recuperation_chiffre_debut idArbre.labelI)^","^(string_of_int ((recuperation_chiffre_debut idArbre.labelI)+indicePb-1)));
    incr idGen; incr idGen;
    let fils = idArbre.filsI in 
    let n = {labelI = labelRestant; filsI = fils; id = (!idGen-1);pereI = [idArbre]; cptI =0} in
    let n2 =  {labelI = (string_of_int (indiceTable+indicePb))^","^(string_of_int ((Hashtbl.length tableHash)-1)); filsI = []; id = !idGen; pereI = [idArbre]; cptI = 0} in 
    List.iter (fun x -> x.pereI   <- [n]) fils; 
    idArbre.filsI <- n::n2::[]; idArbre.filsI <- tri_rapide_numeros idArbre.filsI tableHash;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliere de indice_suffixe_egal_indices *)
let rec indice_suffixe_egal_indices_aux idArbre tableHash indiceTable i =  
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant dans la table de hachage (suffixe restant) 
  in : indice de parcours  
  out : indice probleme a parti duquel il y a la difference    
  *)
  if indiceTable = (Hashtbl.length tableHash) then (-2, i)
  else if i = (recuperation_chiffre_fin idArbre.labelI) - (recuperation_chiffre_debut idArbre.labelI) + 1 then -1, i (* Pour dire qu'on doit passer au fils suivant *) (* ajout_fils idArbre (String.sub suffixe i ((String.length suffixe)-i)) 0 *)
  else if Hashtbl.find tableHash ((recuperation_chiffre_debut idArbre.labelI)+i) = Hashtbl.find tableHash indiceTable then indice_suffixe_egal_indices_aux idArbre tableHash (indiceTable+1) (i+1) 
  else i, 0;; 
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'indice a partir duquel les lettres ne sont plus egales *)
let indice_suffixe_egal_indices idArbre tableHash indiceTable = 
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant dans la table de hachage (suffixe restant)   
  out : indice probleme a parti duquel il y a la difference 
  *)
  indice_suffixe_egal_indices_aux idArbre tableHash indiceTable 0;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui ajoute le suffixe (indice courant "indiceTable" de la table de hachage jusqu'a la fin de celle-ci) a l'endroit ou les lettres ne sont plus egales ou l'ajoute s'il n'existe pas *)
let rec ajout_fils_indices idArbre tableHash indiceTable i = 
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant de la table (suffixe)
  in : indice de parcours   
  *)
  if i = (List.length idArbre.filsI) then (incr idGen; idArbre.filsI <- {labelI = (string_of_int indiceTable)^","^(string_of_int ((Hashtbl.length tableHash)-1)); filsI=[]; id = !idGen;pereI = [idArbre]; cptI = 0}::idArbre.filsI; idArbre.filsI <- tri_rapide_numeros idArbre.filsI tableHash)
  else let indicePb, indiceReserve = (indice_suffixe_egal_indices (List.nth idArbre.filsI i) tableHash indiceTable) in (
    if indicePb = 0 then ajout_fils_indices idArbre tableHash indiceTable (i+1) 
    else if indicePb = -1 then (let idArbreFils = (List.nth idArbre.filsI i) in ajout_fils_indices idArbreFils tableHash (indiceTable+indiceReserve) 0)
    else if indicePb = -2 then () (* Cas qd le suffixe est vide *)
    else changer_fils_indices (List.nth idArbre.filsI i) tableHash indicePb indiceTable;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de creer l'arbre des suffixes avec les indices directement compresse *)
let idArbre_suffixes_indices_compresse chaine =
  (*
  in : chaine a compresser   
  *) 
  idGen := 0;
  let racine = {labelI = "§"; filsI = []; id = !idGen; pereI = []; cptI = 0} and tableHash = init_table chaine in  
    for i=0 to ((Hashtbl.length tableHash)-1) do 
      ajout_fils_indices racine tableHash i 0;
    done;
  racine, tableHash;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de propager le compteur des suffixes communs et uniquement ceux-la *)
let rec propager_cpt arbre cpt = 
  (*
  in : arbre de type idArbre 
  in : compteur a propager   
  *)
  if arbre.labelI = "§" then  (())  
  else (
    if (cpt > arbre.cptI) && (arbre.cptI <> -1) then ( (* <> de - 1 pour prendre que ceux communs et pas deux fois ceux de la seconde chaine *)
      let pere = List.nth arbre.pereI 0 in
      arbre.cptI <- cpt; 
      propager_cpt pere cpt;
    );
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet d'ajouter les fils restants du noeud et du suffixe a l'arbre "idArbre" tout en progageant le compteur si les chaines sont communes *)
let changer_fils2 idArbre suffixe indicePb cpt =
  (*
  in : arbre de type idArbre
  in : suffixe a ajouter
  in : indice probleme a partir duquel les lettres sont diferentes
  in : compteur a propager   
  *)
  let labelRestant = (String.sub idArbre.labelI indicePb ((String.length idArbre.labelI)-indicePb)) in (
    idArbre.labelI <- String.sub idArbre.labelI 0 indicePb;
    incr idGen; incr idGen;
    let fils = idArbre.filsI in 
    let n = {labelI = labelRestant; filsI = fils; id = (!idGen-1);pereI = [idArbre]; cptI =idArbre.cptI} in 
    let n2 =  {labelI = (String.sub suffixe indicePb ((String.length suffixe)-indicePb)); filsI = []; id = !idGen;pereI = [idArbre]; cptI = -1}in 
    List.iter (fun x -> x.pereI   <- [n]) fils; 
    idArbre.filsI <- n::n2::[]; idArbre.filsI <- tri_rapide idArbre.filsI;
    cpt := !cpt+indicePb;
    propager_cpt idArbre !cpt;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliaire de indice_suffixe_egal2 *)
let rec indice_suffixe_egal_aux2 idArbre suffixe i = 
  (*
  in : arbre de type idArbre
  in : le suffixe a ajouter
  in : indice de parcours
  out : indice probleme    
  *)
  if i = (String.length suffixe) then -2, i
  else if i = (String.length idArbre.labelI) then -1, i (* Pour dire qu'on doit passer au fils suivant *) 
  else if idArbre.labelI.[i] = suffixe.[i] then indice_suffixe_egal_aux2 idArbre suffixe (i+1)
  else i, 0;; 
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'indice a partir duquel les lettres ne sont plus egales *)
let indice_suffixe_egal2 idArbre suffixe = 
  (*
  in : arbre de type idArbre
  in : le suffixe a ajouter
  out : indice probleme   
  *)
  indice_suffixe_egal_aux2 idArbre suffixe 0;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui ajoute le suffixe a l'endroit ou les lettres ne sont plus egales ou l'ajoute s'il n'existe pas avec un compteur a -1 pour identifier qu'il s'agit d'un suffixe de la seconde chaine *)
let rec ajout_fils2 idArbre suffixe i cpt =
  (*
  in : arbre de type idArbre
  in : suffixe a ajouter
  in : indice de parcours   
  in : compteur a propager   
  *)
  if i = (List.length idArbre.filsI) then (incr idGen; idArbre.filsI <- {labelI = suffixe; filsI=[]; id = !idGen;pereI = [idArbre]; cptI = -1}::idArbre.filsI; idArbre.filsI <- tri_rapide idArbre.filsI ) (* -1 pour savoir que il s'agit de suffixes de la seconde chaine *)
  else let indicePb, indiceReserve = (indice_suffixe_egal2 (List.nth idArbre.filsI i) suffixe) in (
    if indicePb = 0 then ajout_fils2 idArbre suffixe (i+1) cpt
    else if indicePb = -1 then (
      cpt := !cpt+indiceReserve; 
      let idArbreFils = (List.nth idArbre.filsI i) in let tailleLabel = String.length idArbreFils.labelI in 
      ajout_fils2 idArbreFils (String.sub suffixe tailleLabel ((String.length suffixe)-tailleLabel)) 0 cpt)
    else if indicePb = -2 then (cpt := !cpt+indiceReserve;  propager_cpt (List.nth idArbre.filsI i) !cpt; ()) (* Cas qd le suffixe est vide *)
    else changer_fils2 (List.nth idArbre.filsI i) suffixe indicePb cpt; 
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'arbre compresse de deux chaines de caracteres *)
let arbre_suffixes_compresse_2 arbre chaine = 
  (*
  in : arbre de type idArbre
  in : chaine a ajouter dans l'arbre compresse   
  out: arbre compresse avec 2 chaines  
  *)
  let racine = arbre in  
    for i=0 to ((String.length chaine)-1) do 
      ajout_fils2 racine (String.sub chaine i ((String.length chaine)-i)) 0 (ref 0);
    done;
  racine ;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliaire de sous_chaine_commune_compresse_lettres *)
let rec sous_chaine_commune_compresse_lettres_aux arbre cptMax i = 
  (*
  in : arbre de type idArbre
  in : compteur de la chaine commune la plus longue a parcourir
  in : indice de parcours   
  *)
  if i = (List.length arbre.filsI) then arbre.labelI
  else if (List.nth arbre.filsI i).cptI = cptMax then (arbre.labelI^(sous_chaine_commune_compresse_lettres_aux (List.nth arbre.filsI i) cptMax 0))
  else (sous_chaine_commune_compresse_lettres_aux arbre cptMax (i+1));;

(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de recuperer le compteur maximal parmi les fils de la racine  *)
let rec indice_max_id l max = 
  (*
  in : liste de fils d'arbre de type idArbre
  in out : compteur max    
  *)
  match l with 
  |[] -> max
  |a::q -> if a.cptI > max then indice_max_id q a.cptI else indice_max_id q max;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de recuperer la plus longue sous-chaine commune de l'arbre en suivant les noeuds d'indice maximal *)
let sous_chaine_commune_compresse_lettres arbre = 
  (*
  in : arbre de type idArbre   
  *)
  sous_chaine_commune_compresse_lettres_aux arbre (indice_max_id arbre.filsI (-1)) 0;;
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet d'ajouter les fils restants du noeud et du suffixe (a l'aide de l'indice courant "indiceTable" de la table de hachage) a l'arbre "idArbre" tout en propageant le compteur *)
let changer_fils_indices2 idArbre tableHash indicePb indiceTable cpt =
  (*
  in : arbre de type idArbre  
  in : table de hachage
  in : indice a partir duquel les lettres sont differentes entre le suffixe et les noeuds du chemin
  in : indice courant de la table de hachage (suffixe restant)  
  in : compteur a propager  
  *)
  let labelRestant = (string_of_int ((recuperation_chiffre_debut idArbre.labelI)+indicePb))^","^(string_of_int (recuperation_chiffre_fin idArbre.labelI)) in (
    idArbre.labelI <- (string_of_int (recuperation_chiffre_debut idArbre.labelI)^","^(string_of_int ((recuperation_chiffre_debut idArbre.labelI)+indicePb-1)));
    incr idGen; incr idGen;
    let fils = idArbre.filsI in 
    let n = {labelI = labelRestant; filsI = fils; id = (!idGen-1);pereI = [idArbre]; cptI =idArbre.cptI} in
    let n2 =  {labelI = (string_of_int (indiceTable+indicePb))^","^(string_of_int ((Hashtbl.length tableHash)-1)); filsI = []; id = !idGen; pereI = [idArbre]; cptI = -1} in 
    List.iter (fun x -> x.pereI   <- [n]) fils; 
    idArbre.filsI <- n::n2::[]; idArbre.filsI <- tri_rapide_numeros idArbre.filsI tableHash;
    cpt := !cpt+indicePb;
    propager_cpt idArbre !cpt;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliaire de indice_suffixe_egal_indices2*)
let rec indice_suffixe_egal_indices_aux2 idArbre tableHash indiceTable i =  
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant dans la table de hachage (suffixe restant) 
  in : indice de parcours  
  out : indice probleme a parti duquel il y a la difference  
  *)
  if indiceTable = (Hashtbl.length tableHash) then (-2, i)
  else if i = (recuperation_chiffre_fin idArbre.labelI) - (recuperation_chiffre_debut idArbre.labelI) + 1 then -1, i (* Pour dire qu'on doit passer au fils suivant *) 
  else if Hashtbl.find tableHash ((recuperation_chiffre_debut idArbre.labelI)+i) = Hashtbl.find tableHash indiceTable then indice_suffixe_egal_indices_aux2 idArbre tableHash (indiceTable+1) (i+1) 
  else i, 0;; 

(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui retourne l'indice a partir duquel les lettres ne sont plus egales *)
let indice_suffixe_egal_indices2 idArbre tableHash indiceTable = 
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant dans la table de hachage (suffixe restant)  
  out : indice probleme a parti duquel il y a la difference   
  *)
  indice_suffixe_egal_indices_aux2 idArbre tableHash indiceTable 0;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui ajoute le suffixe (indice courant "indiceTable" de la table de hachage jusqu'a la fin de celle-ci) a l'endroit ou les lettres ne sont plus egales ou l'ajoute s'il n'existe pas tout en propageant le compteur *)
let rec ajout_fils_indices2 idArbre tableHash indiceTable i cpt = 
  (*
  in : arbre de type idArbre
  in : table de hachage
  in : indice courant de la table (suffixe)
  in : indice de parcours  
  in : compteur a propager   
  *)
  if i = (List.length idArbre.filsI) then (incr idGen; idArbre.filsI <- {labelI = (string_of_int indiceTable)^","^(string_of_int ((Hashtbl.length tableHash)-1)); filsI=[]; id = !idGen;pereI = [idArbre]; cptI = -1}::idArbre.filsI; idArbre.filsI <- tri_rapide_numeros idArbre.filsI tableHash) (* -1 pour savoir que il s'agit de suffixes de la seconde chaine *)
  else let indicePb, indiceReserve = (indice_suffixe_egal_indices2 (List.nth idArbre.filsI i) tableHash indiceTable) in (
    if indicePb = 0 then ajout_fils_indices2 idArbre tableHash indiceTable (i+1) cpt
    else if indicePb = -1 then (
      cpt := !cpt+indiceReserve;
      let idArbreFils = (List.nth idArbre.filsI i) in ajout_fils_indices2 idArbreFils tableHash (indiceTable+indiceReserve) 0 cpt)
    else if indicePb = -2 then (cpt := !cpt+indiceReserve;  propager_cpt (List.nth idArbre.filsI i) !cpt; ())  (* Cas qd le suffixe est vide *)
    else changer_fils_indices2 (List.nth idArbre.filsI i) tableHash indicePb indiceTable cpt;
  );;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de remplir une table de hachage deja existante avec les elements de "chaine" a la suite *)
let fill idep chaine hashTable = 
  (*
  in : indice de depart de l'ancienne table
  in : chaine a ajouter 
  in : table de hachage ancienne   
  *)
  for i = 0 to (String.length chaine)-1 do
    Hashtbl.add hashTable (idep+i) (Char.escaped chaine.[i]);
  done;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui creer l'abre des suffixes avec indices compresses des deux chaines et met a jour le compteur des chaines communes *)
let idArbre_suffixes_indices_compresse2 chaine1 chaine2 = 
  (*
  in : chaine a ajouter
  in : chaine a ajouter
  out: arbre des suffixe compresse
  out: table de hachage correspondante    
  *)
  let racine, tableHash = idArbre_suffixes_indices_compresse chaine1 in
  let idep = (Hashtbl.length tableHash) in
  fill idep chaine2 tableHash;
    for i=idep to ((Hashtbl.length tableHash)-1) do 
      ajout_fils_indices2 racine tableHash i 0 (ref 0);
    done;
  racine, tableHash;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de passer des indices aux lettres a travers la table de hachage *)
let toHumain str hash = 
  (*
  in : chaine a traduire
  in : table de hachage
  out: chaine traduite    
  *)
  let start = recuperation_chiffre_debut str in 
  let stop = recuperation_chiffre_fin str in 
  let toret = ref "" in 
  for i = start to (stop) do 
      toret := !toret^(Hashtbl.find hash i);
  done;
  !toret;;
(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction auxiliaire de sous_chaine_commune_compresse_indices *)
let rec sous_chaine_commune_compresse_indices_aux arbre cptMax chaineRes tableHash i = 
  (*
  in : arbre de type idArbre
  in : compteur a suivre
  in out : chaine resultat
  in : table de hachage
  in : indice de parcours     
  *)
  if i = (List.length arbre.filsI) then string_of_int (recuperation_chiffre_fin arbre.labelI)
  else if (arbre.labelI = "§") && ((List.nth arbre.filsI i).cptI = cptMax) then (chaineRes := !chaineRes^(toHumain (List.nth arbre.filsI i).labelI tableHash); (string_of_int (recuperation_chiffre_debut (List.nth arbre.filsI i).labelI))^","^(sous_chaine_commune_compresse_indices_aux (List.nth arbre.filsI i) cptMax chaineRes tableHash 0))
  else if (arbre.labelI <> "§") && ((List.nth arbre.filsI i).cptI = cptMax) then (chaineRes := !chaineRes^(toHumain (List.nth arbre.filsI i).labelI tableHash); sous_chaine_commune_compresse_indices_aux (List.nth arbre.filsI i) cptMax chaineRes tableHash 0)
  else (sous_chaine_commune_compresse_indices_aux arbre cptMax chaineRes tableHash (i+1));;

(* /--------------------------------------------------------------------------------------------------------/ *) 
(* Fonction qui permet de retourner la plus longue sou-chaine commune en parcourant les noeuds avec le compteur maximal *)
let sous_chaine_commune_compresse_indices arbre tableHash = 
  (*
  in : arbre de type arbre
  in : table de hachage
  out : plus longue sous-chaine commune en indices + en lettres 
  *)
  let chaineRes = ref "§" in 
  let res = sous_chaine_commune_compresse_indices_aux arbre (indice_max_id arbre.filsI (-1)) chaineRes tableHash 0 in 
  res, !chaineRes;;
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* /--------------Code source de la partie #6 - Experimentations--------------------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction de recuperation file *)
let recupFile chemin  =  
  let chan = (open_in chemin) in 
  let str = ref "" in
   try while true do  
   str := !str ^(input_line chan);done; !str
  with _-> !str;;
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction abregee de print *)
let print s = 
  print_string s ;;
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction qui va exectuer le les 3 methodes que pour deux fichiers de donnes *)
let boucle d = 
  print "Quel fichier est l'original?\n";
  let original = recupFile (read_line ()) in 
  print "Quel fichier est le fichier a controler\n";
  let copie = recupFile(read_line ()) in 
  let ori = String.lowercase_ascii original in 
  let cop = String.lowercase_ascii copie in 
  let pt1 = Unix.times() in  
  let arbre = arbre_suffixes_compresse_2 (arbre_suffixes_compresse ori) cop in (
  print_string ((sous_chaine_commune_compresse_lettres arbre));
  let pt2 = Unix.times() in 
  print_string "\n===> Methode Arbre suffixes compresse avec lettres executee en: ";
  print_float (pt2.tms_utime-.pt1.tms_utime);
  print_string " secondes \n");
  print_string "\n-------------------------------------------------------------------------------------------\n";
  let pt3 = Unix.times() in  
  let a = fillMatrix ori cop in
  let str = recupsub((getCol a)-1) ((getLgmax a)-1) "" ori cop in 
  let pt4 = Unix.times() in 
  print_string str;
  print_string "\n===> Methode Programation Dynamique executee en: ";
  print_float (pt4.tms_utime -. pt3.tms_utime) ;
  print_string " secondes \n";
  print_string "\n-------------------------------------------------------------------------------------------\n";
  let pt6 = Unix.times() in 
  let arbre,hash = idArbre_suffixes_indices_compresse2 ori cop in 
  let resIndice,reschaine = sous_chaine_commune_compresse_indices arbre hash in 
  let pt7 = Unix.times() in 
  print_string reschaine;
  print_string "\n===> Methode Arbre suffixes compresses avec indices executee en: ";
  print_float (pt7.tms_utime -. pt6.tms_utime);  
  print_string " secondes \n\n";
  print_string "\n-------------------------------------------------------------------------------------------\n";
  ;;
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction qui va prendre tous les donnees.txt (du 0 au 6) et les executera *)
let rec all fname1 fname2 i = 
  print_string "\n";
  print_string "\n//-------------------------------------------------------------------------------------------//\n";
  print_string fname1;
  print_string " vs ";
  print_string fname2;
  print_string "\n\n";
    let original = recupFile fname1 in 
    let copie = recupFile fname2 in 
    let ori = String.lowercase_ascii original in 
    let cop = String.lowercase_ascii copie in 
    let pt1 = Unix.times() in  
    let arbre,hash = idArbre_suffixes_indices_compresse2 ori cop in (
    let resIndice,reschaine = sous_chaine_commune_compresse_indices arbre hash in 
    let pt2 = Unix.times() in 
    print_string reschaine;
    print_string "\n===> Methode Arbre suffixes compresse avec indices executee en: ";
    print_float (pt2.tms_utime-.pt1.tms_utime);
    print_string " secondes \n\n");
    let pt3 = Unix.times() in  
    let a = fillMatrix ori cop in
    let str = recupsub((getCol a)-1) ((getLgmax a)-1) "" ori cop in 
    let pt4 = Unix.times() in
    print_string str;
    print_string "\n===> Methode Programation Dynamique executee en: ";
    print_float (pt4.tms_utime -. pt3.tms_utime) ;
    print_string " secondes \n\n";
    let pt6 = Unix.times() in 
    let arbre = arbre_suffixes_compresse_2 (arbre_suffixes_compresse ori) cop in 
    let resCompress = sous_chaine_commune_compresse_lettres arbre in 
    let pt7 = Unix.times() in 
    print_string ((resCompress));
    print_string "\n===> Methode Arbre suffixes compresses avec lettres executee en: ";
    print_float (pt7.tms_utime -. pt6.tms_utime);
    print_string " secondes \n\n";
  
    if i <> 7 then all (fname1) ("donnee"^((string_of_int i)^".txt")) (i+1);;
(* /--------------------------------------------------------------------------------------------------------/ *)  
(* Fonction qui fera la moyenne de 10 executions sur all *)
let rec average fname1 fname2 i = 
  print_string "\n";
  print_string "\n//-------------------------------------------------------------------------------------------//\n";
  print_string fname1;
  print_string " vs ";
  print_string fname2;
  print_string "\n\n";
  let original = recupFile fname1 in 
  let copie = recupFile fname2 in 
  let ori = String.lowercase_ascii original in 
  let cop = String.lowercase_ascii copie in 
  let pt1 = Unix.times() in  
  let arbre,hash = (
  for i = 0 to 8 do 
    idArbre_suffixes_indices_compresse2 ori cop 
  done; idArbre_suffixes_indices_compresse2 ori cop)
  in (
  let resIndice,reschaine = sous_chaine_commune_compresse_indices arbre hash in 
  let pt2 = Unix.times() in 
  print_string reschaine;
  print_string "\n===> Methode Arbre suffixes compresse avec indices executee en: ";
  print_float ((pt2.tms_utime-.pt1.tms_utime)/.10.) ;
  print_string " secondes \n\n");
  let pt3 = Unix.times() in  
  let a = (
  for i = 0 to 8 do 
  
  fillMatrix ori cop 
  done; fillMatrix ori cop )
  in
  let str = recupsub((getCol a)-1) ((getLgmax a)-1) "" ori cop in 
  let pt4 = Unix.times() in
  print_string str;
  print_string "\n===> Methode Programation Dynamique executee en: ";
  print_float ((pt4.tms_utime -. pt3.tms_utime)/.10.) ;
  print_string " secondes \n\n";
  let pt6 = Unix.times() in 
  let arbre =(
  for i = 0 to 8 do 
    arbre_suffixes_compresse_2 (arbre_suffixes_compresse ori) cop
    done ;   
    arbre_suffixes_compresse_2 (arbre_suffixes_compresse ori) cop 
    ) in 
  let resCompress = sous_chaine_commune_compresse_lettres arbre in 
  let pt7 = Unix.times() in 
  print_string ((resCompress));
  print_string "\n===> Methode Arbre suffixes compresses avec lettres executee en: ";
  print_float ((pt7.tms_utime -. pt6.tms_utime)/.10.);
  print_string " secondes \n\n";

  if i <> 7 then all (fname1) ("donnee"^((string_of_int i)^".txt")) (i+1);; 
(* /--------------------------------------------------------------------------------------------------------/ *)
(* /-----------------------------------------TESTS----------------------------------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
let s = print_string"Ecrivez \"all\" -> pour executer tous les donnnees ensemble\nTapez <return> -> pour selectionner des fichiers de donnees\nEcrivez \"average\" -> pour faire la moyenne sur 10 essais de tous les donnnes\n"; read_line() in  if s = "all" then all "donnee0.txt" "donnee0.txt" 1 else if s = "average" then average "donnee0.txt" "donnee1.txt" 0 else boucle();
