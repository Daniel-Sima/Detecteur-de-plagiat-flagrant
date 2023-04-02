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
(* /--------------------------------------------TESTS-------------------------------------------------------/ *)
(* /--------------------------------------------------------------------------------------------------------/ *)
print_string "=== Test #1 ===\n";;
let a = fillMatrix "ANANAS" "ANANASANANAS" in
  let str = recupsub((getCol a)-1) ((getLgmax a)-1) "" "ANANAS" "ANANASANANAS" in 
    print_string (str^"\n");