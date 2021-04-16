# Travail pratique 1 - Messagerie SmartMail

## Description

En utilisant Haskell, on a implementé des modules permettant la gestion d'une messagerie électronique appelé SmartMail. SmartMail permet à un utilisateur inscrit de disposer d’un service de messagerie électronique offrant trois boîtes (Envoi, Réception et Spams).

Les messages suspects sont automatiquement envoyés dans la boîte des Spams. Ils sont filtrés selon un ensemble de principes ou d’heuristiques spécifiques. L’utilisateur peut aussi entretenir un ensemble de contacts et éventuellement de préférences.

L’état d’un contact suspect passera de « blanc » à « noir » indiquant ainsi son inscription dans la liste noire et provoquant une redirection automatique des messages reçus de ce contact dans la boîte Spams.

Un service d’administration de SmartMail est offert pour la gestion des messages et la production de quelques statistiques d’intérêt.

## Auteur

CHAOUKI MOHAMED (CHAM27088802)

## Fonctionnement

Dans la ligne de commande il faut faire ghci afin de le lancer. Ensuite, il faut faire :l SmartMail.hs afin de compiler le logiciel pour pouvoir l'utiliser.


## Contenu du projet

- ``` CompteSmail.hs ``` : Module pour la gestion des comptes de la messagerie SmartMail
- ``` Personne.hs ``` : Module pour la gestion des personnes
- ``` SmartMail.hs ``` : Module pour la gestion de la messagerie SmartMail
- ``` Trame.hs ``` : Module pour la gestion des trames
- ``` README.md ``` : contient la documentation du projet

## Dépendances

Installer GHC :

- Depuis le terminal sudo apt-get install ghc ghc-prof ghc-doc
- Depuis le lien : https://www.haskell.org/platform/

## Références

- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
- https://tech.fpcomplete.com/haskell/library/containers


