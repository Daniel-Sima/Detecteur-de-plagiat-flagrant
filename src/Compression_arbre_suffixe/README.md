Le code de la partie #4 commence ligne 256, avant cette ligne se trouve le code de la partie #3 necessaire pour certaines fonctions de cette partie.

Executer le fichier source "souce.ml" pour tous vos tests comme ceci:
    ocamlc source.mal -o prog
    ./prog

A la fin du fichier "source.ml" se trouve la partie tests.
- Pour tester le bon fonctionnement de la fonction compression (compression1) vous changer le mot
  "ANANS_" qui fournit le graphe de l'arbre des suffixes compresses dans Graphes/graphe_suffixes_1.png par d'autres mots et voir le resultant dans ce même fichier.png.
- Pour tester le bon fonctionnement de l'adaptation de la fonction SousChaineCommune
  (buildPlusGrandeSequence2) pour l'arbre construit compressé vous pouvez changer les mots "ANANAS_" et "ANANASANANAS_" par d'autre mots voir le resultant dans le terminal ainsi que le graphe crée dans Graphes/graphe_suffixes_2.png.
- Pour tester le bon fonctionnement de la création de l'arbre des suffixes avec indices non compressés
  vous pouvez changer le mot "ANANS_" dans le test #3 par d'autres mots et voir son graphe associé dans Graphes/graphe_suffixes_3.png.
- Pour tester le bon fonctionnement de la création de l'arbre des suffixes avec indices compressés
  vous pouvez changer le mot "ANANS_" dans le test #4 par d'autres mots et voir son graphe associé dans Graphes/graphe_suffixes_4.png.
