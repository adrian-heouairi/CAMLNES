# Émulateur de NES (Nintendo Entertainment System) en OCaml

## Introduction générale

La NES, ou Famicom au Japon, est une console de jeux sortie entre 1983 et 1987 selon les régions du monde. Elle est munie d'un processeur cadencé à 1,79 MHz et de 2 ko de RAM. Les cartouches de jeu incluent un circuit électronique nommé _mapper_, qui peut permettre de dépasser les capacités d'origine de la console.

L'émulation de cette console sur un ordinateur moderne est un défi très intéressant, nécessitant la gestion en parallèle et la synchronisation des divers composants de la console.

## Objectifs

L'objectif principal est de rendre jouables au clavier les jeux NES utilisant le _mapper_ 0, comme Donkey Kong ou Super Mario Bros., dans leur version américaine. Le langage de programmation choisi est OCaml.

Une fois cet objectif réalisé, les objectifs secondaires suivants pourraient être développés :
- On pourrait ajouter une interface graphique autour du jeu, permettant de mettre en pause l'émulation, charger un autre jeu, et sauvegarder/charger l'état complet de la console (_save states_).
- On pourrait également implémenter un support pour certains _mappers_ fréquemment utilisés, ajoutant un support pour d'autres jeux.
- L'ajout d'un support pour les manettes de jeu est également possible.

## Testabilité

Il existe des « jeux » fabriqués spécialement pour tester des comportements attendus de l'émulateur, disponibles sur https://wiki.nesdev.org/w/index.php/Emulator_tests. L'objectif est donc de réussir à faire marcher les principaux jeux de test.

## Calendrier

- Fin décembre 2022 : Émulation du CPU : écrire une boucle qui lit des instructions pour le processeur de la NES et reproduit leur comportement sur une mémoire simulée.
- Début janvier 2023 : Réussir à faire fonctionner les cartouches de jeu de test spécifiques au CPU (https://wiki.nesdev.org/w/index.php/Emulator_tests), et en faire des tests automatisés de notre code.
- Début février 2023 : Implémenter la sortie vidéo des jeux.
- Fin février 2023 : Implémenter les contrôles des jeux au clavier.
- Mars 2023 : Implémenter la sortie audio des jeux.

## Références

- Writing your own NES emulator Part 1 - overview : https://yizhang82.dev/nes-emu-overview
- Les jeux NES utilisant le _mapper_ 0 : https://nesdir.github.io/mapper0.html
