# Émulateur de NES (Nintendo Entertainment System) en OCaml

## Introduction générale

La NES, ou Famicom au Japon, est une console de jeux sortie entre 1983 et 1987 selon les régions du monde. Elle est munie d'un processeur cadencé à 1,79 MHz et de 2 ko de RAM. Les cartouches de jeu incluent un circuit électronique nommé _mapper_, qui peut permettre de dépasser les capacités d'origine de la console.

L'émulation de cette console sur un ordinateur moderne est un défi très intéressant, nécessitant la gestion en parallèle et la synchronisation des divers composants de la console.

## Originalité du projet

La quasi-totalité des émulateurs étant programmés avec un langage impératif comme le C, en programmer un dans un langage fonctionnel constitue un défi technique très intéressant.

## Objectifs

L'objectif principal est de rendre jouables au clavier, sans audio, les jeux NES utilisant le _mapper_ 0, comme Donkey Kong ou Super Mario Bros., dans leur version américaine. Le langage de programmation choisi est OCaml.

Une fois cet objectif réalisé, les objectifs secondaires suivants pourraient être développés :
- Ajout de l'audio.
- On pourrait ajouter une interface graphique autour du jeu, permettant de mettre en pause l'émulation, charger un autre jeu, et sauvegarder/charger l'état complet de la console (_save states_).
- On pourrait également implémenter un support pour certains _mappers_ fréquemment utilisés, ajoutant un support pour d'autres jeux.
- En plus des contrôles au clavier, prendre en charge les manettes de jeu.

## Testabilité

Il existe des « jeux » conçus spécialement pour tester des comportements attendus de l'émulateur, disponibles sur https://wiki.nesdev.org/w/index.php/Emulator_tests. L'objectif est donc de réussir à faire marcher les principaux jeux de test.

## Calendrier

- Début février 2023 : Ouvrir une cartouche de jeu .nes et en parser le contenu pour pouvoir l'exécuter avec le CPU, etc.
- Fin février 2023 : Émulation du CPU : écrire une boucle qui lit des instructions pour le processeur de la NES et reproduit leur comportement sur une mémoire simulée.
- Fin février 2023 : Réussir à faire fonctionner les cartouches de jeu de test spécifiques au CPU (https://wiki.nesdev.org/w/index.php/Emulator_tests), et en faire des tests automatisés du code.
- Avril 2023 : Finir l'implémentation de la sortie vidéo des jeux.
- Avril 2023 : Implémenter les contrôles des jeux au clavier.

## Références

- Writing your own NES emulator Part 1 - overview : https://yizhang82.dev/nes-emu-overview
- Les jeux NES utilisant le _mapper_ 0 : https://nesdir.github.io/mapper0.html
