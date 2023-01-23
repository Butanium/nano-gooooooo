# Point manquant 
- Copie récursive des structures pour passage par valeur. Pas facile à implémenter et requierant beaucoup de debugging, j'ai préféré le laisser de coté pour me concentrer sur la robustesse du reste du code.

# Méthodologie

## Structure du projet
J'ai adapté le projet en l'organisant en librairies afin d'utiliser pleinement le potentiel de `dune` et de le rendre plus lisible. J'ai ajouté les flags `--trace` et `--run`. `--trace` permet de voir la stack trace des erreurs qui sont levées. C'est très utile pour le debugging. `--run` permet de lancer le programme après la compilation, ce qui évite de devoir appeler `gcc` à la main. Le flag `--debug` désactive les erreurs de variables non utilisées afin d'avoir des exemples minimaux permettant d'identifier la cause des segfaults.


## Typing
### Environnement
Pour les fonctions et les structures, j'utilise une surcouche sur le module Hashtbl . La mutabilité de cette méthode rend son utilisation plus agréable qu'une Map.

Pour les variables j'utilise une StringMap afin de gérer la localité des fonctions. Pour cela je traite le cas PEVar à l'interieur du cas PEBlock avec la fonction `type_in_block`.
### Structures
Je détecte les structures récursives au moment de calculer la taille des structs. Pour cela, je "marque" les structures dont je calcule la taille en la mettant à `-2`. Si je rencontre une structure marquée à `-2` alors je suis dans une structure récursive et je renvoie une erreur. Si je rencontre une structure intialisée à `-1`, alors c'est que je n'ai pas encore calculé sa taille et je la calcule. Si je rencontre une structure intialisée à une valeur positive, alors c'est que je l'ai déjà calculée et je la renvoie.

### Difficultés rencontrées
Comprendre la structure de la fonction `type_function_body` (ancienne `expr`), notamment le booléen retourné m'a pris pas mal de temps. En effet celui ci était marqué de type `import_fmt` à cause de `open Ast` qui écrasait le type `bool`. J'ai fini par comprendre que l'on devait retourner la présence d'un `return` dans le corps de la fonction.

Le typing n'a pas grand chose d'intéressant algorithmiquement parlant, le plus dur fût de m'approprier le code existant dans un premier temps puis de ne pas y semer des bugs comme le petit poucet semait des cailloux. J'ai encore dû corriger des oublis la veille de rendre ce rapport. Une égalité de type qui utilisait `=` au lieu de `eq_type`, un oublie de tests de types pour `PEassign` et une inversion d'arguments pour `PEFor`.

## Compilation
### Environnement
Un simple type record suffit. Je n'ai besoin que du label de sortie, du nombre d'arguments et de la position relative de la dernière variable locale au pointeur de variable `%rbp`.

### Détails d'implémentation
Chaque type a sa fonction de `print`, seul les structures ont un print un peu élaboré qui print récursivement les différents fields de la structure. J'ai donné des noms aux labels et ai inclus des numéro de ligne correspondants aux lignes du fichier source pour faciliter le débogage.
