Executer le fichier source "souce.ml" pour tous vos tests comme ceci:
    ocamlc source.mal -o prog
    ./prog

A la fin du fichier "source.ml" se trouve la partie tests.
- Pour tester le bon fonctionnement de la fonction ArbreSuffixes (arbre_suffixes) vous pouvez changer
  "ANANAS_" par d'autres mots et verifier dans le dossier Graphe le graphe graphe_suffixes_1.png pour voir le arbre des suffixes du mot choisit.
    - Si vous n'avez pas .dot alors il est possible que vous ne pouriez pas genere le graphe automatiquement. Soit vous l'installez soit vous passez par le site: https://graphs.grevian.org/graph et entrez ce qu'il y a dans le fichier arbre_suffixes.dot.
    - A noter aussi que ce fichier s'ecrase a chaque fois donc faire attention a ce que vous voulez garder dans le fichier arbre_suffixes.dot et commentez le code de tests qui ne vous interesse pas.
- Pour tester le bon fonctionnement de la fonction SousChaine (sous_chaine) vous pouvez changer
  "BANANE_" et "NAN" dans le second test et voir l'affichage du terminal pour le resultat.
- Pour tester le bon fonctionnement de la fonction SousChainesCommunes (buildArbre_suffixes) vous
  pouvez changer "ANANAS_" et "ANANASANANAS_" dans le troisieme test et voir l'affichage dans le terminal ainsi que l'arbre des suffixes des deux chaînes choisies dans le dossier Graphe/graphe_suffixes_2.png.
