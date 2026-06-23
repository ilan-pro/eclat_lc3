# Processor LC-3

L'idée est simple, nous devons implementé le processeur pédagogique LC-3 dans le langage `Eclat` afin de tester des approches de deux styles de programmation différentes. D'un côté il y a le style impératif et de l'autre le style fonctionnel. 

## Choix d'implémentation de la mémoire 
J'ai choisi de faire une mémoire séparé du programme principal pour des choix de lisibilité au niveau des matchs. Faire trop de match rendrais le code illisible. La mémoire est simplement le type value. 

Pour ce qui est des opérations sur les registres, des directions restent à explorer. Pour le moment, je ne fais d'opération arithmétique uniquement sur les constante contenu dans les registres (je ne prends pas en compte l'adresse et les autres types de valeur). Dans le LC-3 classique, les opérations se ferait peu importe ce que le registre contient (aucune disinction faite). 

## Imperatif style vs fonctionnel
- `Impératif` : Accès sur le partage de variable en mémoire sans (jamais) avoir le besoin de copier. Cela permet, pour de gros programme, de ne pas prendre toute la place mémoire. 
- `Fonctionnel` : Accès sur la partage de référence. Consommation mémoire bien plus importante mais plus performant pour ce qui est du parallélisme et autre notion.

Globalement, le style impératif est apprecié dans le cas de programme lourd tandis que le style fonctionnel est appréciable quand nous cherchons le gain de performance étant donné qu'il sera bien plus naturel pour la programmation parallélisé.
Dans le cadre du langage `Eclat`, la programmation impérative impose certaines contraintes de cycle d'horloge qui amoindrit la performance global du programme. Les accès aux structures de donnée impératives (comme les arrays) sont garantit de prendre 1 cycle par accès (E/S) du à la présence de verrou. Aux antipodes des sutrctures de donnée fonctionnel, qui sont accessibles de manière immédiate. Il y a des pour et des contres pour les deux approches. Les critères se portent aussi sur la simplicité à écrire du code dans les deux cas.
