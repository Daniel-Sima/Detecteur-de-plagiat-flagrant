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
let  maxL i a2=
if a2.cpt > i then a2.cpt else i
(* /--------------------------------------------------------------------------------------------------------/ *)
(* Fonction qui permet de trouver l'arbre fils ayant le plus grand compteur *)
let  longueur arbre = 
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
  (*
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
(* /-----------------------------------------TESTS----------------------------------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "=== Test #1 ===\nVoir dans le dossier Graphes/graphe_suffixes_1.png\n";;
let chaine = "ANANAS_" in (
  let arbreCompresse = rArbre_suffixes chaine in (
    compression1 arbreCompresse;
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_Rarbres_suffixes arbreCompresse;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_1.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #2 ===\nVoir dans le dossier Graphes/graphe_suffixes_2.png\n";;
let chaine1 = "ANANAS_" and chaine2 = "ANANASANANAS_" in (
  let arbreDeuxChainesCompresse, _, _ = buildArbre_suffixes chaine1 chaine2 in (
    compression1 arbreDeuxChainesCompresse;
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_Rarbres_suffixes arbreDeuxChainesCompresse;
    append_chaine "\n}";
    let sousChaineCommuneArbreCompresse = buildPlusGrandeSequence2 arbreDeuxChainesCompresse "" in 
    print_string ("La plus longue sous-chaine commune de l'arbre compresse de "^chaine1^" et de "^chaine2^" est: "^sousChaineCommuneArbreCompresse^"\n");
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_2.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #3 ===\nVoir dans le dossier Graphes/graphe_suffixes_3.png\n";;
let chaine = "ANANAS_" in (
  let arbreIndices, tableHash = idArbre_suffixes chaine in (
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_idArbres_suffixes_numeros arbreIndices;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_3.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #4 ===\nVoir dans le dossier Graphes/graphe_suffixes_4.png\n";;
let chaine = "ANANAS_" in (
  let arbreIndices, tableHash = idArbre_suffixes chaine in (
    compression_idArbre_numeros arbreIndices;
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_idArbres_suffixes_numeros arbreIndices;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_4.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)


