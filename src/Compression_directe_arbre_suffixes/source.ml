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
(* /-----------------------------------------TESTS----------------------------------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "=== Test #1 ===\nVoir dans le dossier Graphes/graphe_suffixes_1.png\n";;
let chaine = "ANANAS_" in (
  let arbreCompresseLettres = arbre_suffixes_compresse chaine in (
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_idArbres_suffixes arbreCompresseLettres;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_1.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #2 ===\nVoir dans le dossier Graphes/graphe_suffixes_2.png\n";;
let chaine = "ANANAS_" in (
  let arbreCompresseIndices, hash = idArbre_suffixes_indices_compresse chaine in (
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_idArbres_suffixes_numeros arbreCompresseIndices;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_2.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #3 ===\nVoir dans le dossier Graphes/graphe_suffixes_3.png\n";;
let chaine1 = "ANANAS_" and chaine2 = "ANANASANANAS_" in (
  let arbreCompressLettres2 = arbre_suffixes_compresse_2 (arbre_suffixes_compresse chaine1) chaine2 in (
    print_string ("La plus longue sous-chaine commune de "^chaine1^" et "^chaine2^" est: "^(sous_chaine_commune_compresse_lettres arbreCompressLettres2)^"\n");
    Sys.remove "arbre_suffixes.dot";
    append_chaine "graph {";
    parcours_idArbres_suffixes_numeros arbreCompressLettres2;
    append_chaine "\n}";
    Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_3.png";
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "\n=== Test #4 ===\nVoir dans le dossier Graphes/graphe_suffixes_4.png\n";;
let chaine1 = "ANANAS_" and chaine2 = "ANANASANANAS_" in (
  let arbreCompressIndices2, hash = idArbre_suffixes_indices_compresse2 chaine1 chaine2 in (
    let resIndices, resChaine = sous_chaine_commune_compresse_indices arbreCompressIndices2 hash in (
      print_string ("La plus longue sous-chaine commune de "^chaine1^" et "^chaine2^" est: "^(resChaine)^" | "^resIndices^"\n");
      Sys.remove "arbre_suffixes.dot";
      append_chaine "graph {";
      parcours_idArbres_suffixes_numeros arbreCompressIndices2;
      append_chaine "\n}";
      Sys.command "dot -Tpng arbre_suffixes.dot -o ./Graphes/graphe_suffixes_4.png";
    );
  )
);;
(* /--------------------------------------------------------------------------------------------------------/ *)
