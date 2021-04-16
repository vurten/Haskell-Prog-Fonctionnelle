{-|
Module      : Trame
Description : Module pour la gestion des trames
Copyright   : (c) Ange Tato et Roger Nkambou
License     : GPL-3
Maintainer  : nyamen_tato.ange_adrienne@uqam.ca
Stability   : experimental

Ce module offre les fonctionalités permettant de manipuler des trames de messages. 
 -}

module Trame where

import Personne
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe

data TypeMessage = Spam | NonSpam deriving (Show, Eq, Read)
type Annee = Integer
type Mois = Int
type Jour = Int

data Date = Date Annee Mois Jour deriving (Show, Eq, Read, Ord)
dateAuj = unsafeDupablePerformIO (getCurrentTime >>= return . toGregorian . utctDay)

type Contenu = String 
type Objet = [Char]

data Priorite = Important | Normal | Faible deriving (Show, Eq, Read)
data Entete = Entete Date Objet Personne Personne deriving  (Show,Eq, Read)
data Trame = Trame Entete Contenu deriving  (Show, Read, Eq)
type Message = (Courriel, Courriel, Objet, Contenu)

-- | Retourne la date d'envoi d'un message
date :: Trame -> Date
date (Trame (Entete d _ _ _) _ ) = d

-- | Retourne le courriel de l'metteur d'un message
emetteur (Trame (Entete _ _ e _) _ ) = courriel e

-- | Retourne le courriel du recepteur d'un message
receveur :: Trame -> String
receveur (Trame (Entete _ _ _ r) _ ) = courriel r 

-- | Retourne l'objet d'un message
objet :: Trame -> String
objet (Trame (Entete _ o _ _) _ ) = o

-- | Retourne le contenu d'un message
contenu :: Trame -> String
contenu (Trame _ c ) = c



