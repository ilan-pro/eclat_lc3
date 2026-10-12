# Processeur LC-3

L'objectif de ce projet est d'implémenter le processeur pédagogique LC-3 en langage `Éclat`, afin de comparer deux styles de programmation : **impératif et fonctionnel**.

## Choix d'implémentation de la mémoire

La mémoire a été séparée du programme principal afin de préserver la lisibilité du code, notamment dans la gestion des différents cas d'instructions (`match`). Une multiplication des expressions `match` rendrait en effet le code plus difficile à lire et à maintenir.

La mémoire est simplement représentée par une valeur de type `value`.

Concernant les opérations sur les registres, plusieurs pistes restent à explorer. Pour le moment, les opérations arithmétiques ne prennent en compte que les constantes stockées dans les registres. Les adresses et les autres types de valeurs ne sont donc pas encore traités. Dans le LC-3 classique, les opérations s'effectuent sur le contenu binaire des registres, sans distinction de type.

Contrairement à ce qui est indiqué dans le polycopié, les instructions de chargement et de modification de la mémoire n'utilisent plus `PC`. Cette variable est réservée au pointeur vers le programme `prog`, qui n'est pas stocké dans la mémoire du processeur. Il est donc important de distinguer ces deux éléments.

**Attention :** certaines instructions conservent néanmoins l'incrémentation du `PC`, conformément à leur fonctionnement.

## Style impératif vs style fonctionnel

- **Impératif :** repose sur la modification directe de l'état partagé en mémoire, sans nécessiter de copies systématiques des données. Cette approche peut limiter la consommation mémoire, notamment pour les programmes manipulant de grandes quantités de données.

- **Fonctionnel :** privilégie le partage de références et la manipulation de valeurs plutôt que la modification directe d'un état partagé. Cette approche peut entraîner une consommation mémoire plus importante, mais facilite certaines formes de parallélisation.

Globalement, le style impératif peut être intéressant pour limiter les copies et la consommation mémoire, tandis que le style fonctionnel peut favoriser la parallélisation et les performances dans certains contextes.

Dans le langage `Éclat`, la programmation impérative impose certaines contraintes liées aux cycles d'horloge, susceptibles de réduire les performances globales du programme. Les accès aux structures de données impératives, comme les tableaux (`arrays`), sont garantis de prendre un cycle par accès en lecture ou en écriture, en raison de la présence de mécanismes de verrouillage.

À l'inverse, les structures de données fonctionnelles sont accessibles de manière immédiate.

Les deux approches présentent donc des avantages et des inconvénients. La comparaison portera notamment sur les performances, la consommation mémoire, les possibilités de parallélisation et la simplicité d'écriture du code.

## Jeux de test dans le dossier `test`

Pour exécuter un jeu de test, il suffit d'intégrer le code correspondant dans la fonction d'initialisation de l'environnement de l'implémentation LC-3 choisie.

Les indices utilisés dans les programmes de test sont normalement corrects.

À terme, un parseur pourra être développé pour permettre l'écriture directe de programmes en assembleur LC-3, sans avoir à traduire manuellement les instructions en code `Éclat`.