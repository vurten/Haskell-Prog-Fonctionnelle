{-|
Module      : CompteSmail
Description : Module pour la gestion des comptes de la messagerie SmartMail
Copyright   : (c) Ange Tato et Roger Nkambou et Chaouki Mohamed
License     : GPL-3
Stability   : experimental

Ce module offre les fonctionalités permettant de manipuler les comptes SmartMail. 
 -}
module CompteSmail where 

import Personne
import Trame
import Data.List
import Data.Char
type Contact = (Personne, Etat)
type Explications = String 
data Etat = Noir | Blanc deriving (Show, Eq, Read) -- Noir = contact dans liste noire (bloqué) , Blanc contact non bloqué
type Reception = [Trame] -- boîte de reception
type Envoi = [Trame] -- boîte d'envoi
type Spams = [(Trame, Explications)] -- boîte des spams
type Preferences = [Trame -> Bool]
data CompteSmail  = CompteSmail Personne Reception Envoi Spams Preferences [Contact] -- Compte smail 
-- Vous devez faire du type CompteSmail une instance de la classe Show. Vous devez donc redéfinir la fonction show pour ce type. Voir les exemples pour voir comment l'affichage est gérer

instance Show CompteSmail where
   show (CompteSmail (Personne co _) r e s _ c) = 
      (delete '\"' (delete '\"'(show "CompteSmail ")) ) ++ (show co) ++ (delete '\"' (delete '\"'(show " :")))++
      "\n" ++ (delete '\"' (delete '\"'(show "Recus = "))) ++ (show r) ++ (delete '\"' (delete '\"'(show ",")))++
      "\n" ++ (delete '\"' (delete '\"'(show "Envois = "))) ++ (show e) ++ (delete '\"' (delete '\"'(show ",")))++
      "\n" ++ (delete '\"' (delete '\"'(show "Spams = ")))  ++ (show s) ++ (delete '\"' (delete '\"'(show ",")))++
      "\n" ++ (delete '\"' (delete '\"'(show "Contacts = "))) ++ (show (zip (map (Personne.courriel) (map fst c )) (map snd c )))

listeContacts [] = []
listeContacts ((p,e):xs) = (courriel p, e):listeContacts xs

-- | Retourne la personne à qui appartient le compte Smail
personne :: CompteSmail -> Personne  -- boîte des spams
personne (CompteSmail p _ _ _ _ _) = p

-- | Retourne la liste des messages spams d'un compte Smail
spams :: CompteSmail -> Spams  -- boîte des spams
spams (CompteSmail _ _ _ s _ _) = s

-- | Retourne la liste des messages de la boîte d'envoi d'un compte Smail
envoi :: CompteSmail -> Envoi  -- boîte des messages envoyés
envoi  (CompteSmail _ _ e _ _ _)= e  

-- | Retourne la liste des messages de la boîte  de reception d'un compte Smail
reception :: CompteSmail -> Reception  -- boîte des messages reçus
reception (CompteSmail _ r _ _ _ _) = r

-- | Retourne la liste des préférences d'un compte Smail
--
-- filtres ou contraintes imposés par le titulaire d'un compte smail 
-- exemple: je ne veux aucun message dont le courriel de l'expéditeur se termine pas ".zz"
--          si la préférence n'est pas satisfaite, le message est automatiquement redirigé dans la boîte des spams
preferences :: CompteSmail  -> Preferences 
preferences (CompteSmail _ _ _ _ pr _) = pr

-- | Retourne la liste de tous les contacts d'un compte Smail
contacts :: CompteSmail  -> [Contact] 
contacts (CompteSmail _ _ _ _ _ c) = c 


-------------------------------------------------------------------
--------------------------NE PAS MODIFIER--------------------------
-------------------------------------------------------------------

-- Quelques données utilisées dans les tests
pers0 = Personne "equipesmartmail@smail.ca" ("equipe","smail")
pers1 = Personne "tato.ange@smail.ca" ("ange","tato")
pers2 = Personne "nkambou.roger@smail.ca" ("roger","nkambou")
pers3 = Personne "robert.julien@smail.ca" ("julien","robert")
pers4 = Personne "noel.alice@smail.ca" ("alice","noel")
pers5 = Personne "bourassa.alex@smail.ca" ("alex","bourassa")
pers6 = Personne "ariane.carotte@techno.co" ("arianne","carotte")
pers7 = Personne "pablo.adamo@blob.es" ("olivier","adam")
pers8 = Personne "michel.desrosiers@blob.ca" ("michel","desrosiers")
pers9 = Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")

-- Exemples de trame de message
trameBienvenue1 = (Trame (Entete (Date 2020 02 10) "Bienvenue" pers0 pers1) "Bienvenue dans votre boite smartMail !") 
trameBienvenue2 = (Trame (Entete (Date 2020 02 10) "Bienvenue" pers0 pers2) "Bienvenue dans votre boite smartMail !")
trame1 = (Trame (Entete (Date 2020 01 18) "AB CD EF" pers0 pers1) "Bienvenue dans votre boite smartMail !") 
trame2 = (Trame (Entete (Date 2019 12 21) "Bi!en! venue!" pers0 pers1) "Bienvenue dans votre boite smartMail !") 
trame3 = (Trame (Entete (Date 2020 01 01) "?Bien venue?" pers0 pers1) "Bienvenue dans votre boite smartMail !")  
trame4 = (Trame (Entete (Date 2018 10 05) "" pers0 pers1) "Bienvenue dans votre boite smartMail !") 
trame5 = (Trame (Entete (Date 2017 05 07) "Bienvenue $ " pers0 pers1) "Bienvenue dans votre boite smartMail !") 
trame6 = (Trame (Entete (Date 2018 05 09) "Bienvenue" pers1 pers3) "Allo Robert") 
trame7 = (Trame (Entete (Date 2019 18 10) "un message" pers0 pers1) "Bienvenue dans sexe du viagra chaud nu") 
trame8 = (Trame (Entete (Date 2020 01 15) "un message" pers0 pers1) "offre de Bienvenue dans  publicité gratuit pour voyage special") 
trame9 = (Trame (Entete (Date 2020 10 10) "un message" pers0 pers1) "0de 3Bienvenue dans 1 et 9 | pour tp1et2") 
trame10 = (Trame (Entete (Date 2020 02 01) "un message" pers0 pers1) "bien1venue") 
trame11 = (Trame (Entete (Date 2020 10 18) "un message" pers0 pers1) "allo|allo") 
trame12 = (Trame (Entete (Date 2019 11 17) "Salut ange" pers3 pers1) "Salut Ange tu vas bien ?") 
trame13 = (Trame (Entete (Date 2020 01 18) "Salut roger" pers3 pers2) "special voyage demain, viens vite" ) 
trame14 = (Trame (Entete (Date 2020 03 18) "Hola mimi" pers7 pers9) "como estas ?" ) 
trame15 = (Trame (Entete (Date 2020 02 09) "Bingo" pers6 pers9) "J'ai trouve ce que tu cherchais hier" ) 
trame16 = (Trame (Entete (Date 2020 01 07) "Par rapport a Ivan" pers8 pers9) "Ivan ne viendra pas demain ?" ) 
tramet = (Trame (Entete (Date 2020 01 13) "Cool" pers4 pers5) "Allo Alex") 
tramett = (Trame (Entete (Date 2020 01 25) "Nouvelles" pers2 pers5) "Tu vas bien ?") 

-- Exemples de compte smail
csmail0 = CompteSmail pers0 [] [trameBienvenue1, trameBienvenue2] [] [] []
csmail1 = CompteSmail pers1 [trameBienvenue1] [] [] [] [(pers2,Blanc),(pers4,Noir)]
csmail2 = CompteSmail pers2 [trameBienvenue2] [] [] [] []
csmail3 = CompteSmail pers3  [] [] [] [] []
csmail4 = CompteSmail pers4  [] [] [] [] [] 
csmail5 = CompteSmail pers5  [] [] [] [] [(pers1,Blanc)]
csmail6 = CompteSmail pers9  [] [] [] [\(Trame (Entete d ob p1 _) c) -> (tail $ dropWhile (/='.') (dropWhile (/='@') (courriel p1))) /= "es" ] []

-------------------------------------------------------------------
-----------------FIN DE LA ZONE À NE PAS MODIFIER------------------
-------------------------------------------------------------------


-- | Ajouter un contact
--
-- Les paramètres sont : les informations du contact à ajouter, et le compte à modifier
-- Note1: De base, un contact est ajouté avec l'état = Blanc et en entête de la liste de contacts. 
-- Note2: Pas besoin de vérifier si le courriel est bon ou pas car le courriel passé en paramètre sera vérifié (pas par vous) avant d'être envoyé à cette fonction.
-- Note3: Vous devez vous assurez que le contact n'existe pas déjà
--
-- >>> csmail4' = ajouterContact "robert.julien@smail.ca" "julien" "robert" csmail4
-- >>> csmail4'
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],
-- Contacts = [("robert.julien@smail.ca",Blanc)]
ajouterContact :: Courriel -> Prenom -> Nom -> CompteSmail -> CompteSmail 
ajouterContact  c p n cs = if c `elem` (map (Personne.courriel) (map fst (contacts cs) )) then cs
                           else (CompteSmail (personne cs) (reception cs) (envoi cs) (spams cs) (preferences cs) ((Personne c  (p, n), Blanc):(contacts cs)) )


-- | bloquer un contact
-- Les paramètres sont : le compte à modifier, le contact à bloquer, le compte modifié
--
-- >>> csmail4'' = ajouterContact "robert.julien@smail.ca" "julien" "robert" $ ajouterContact "bourassa.alex@smail.ca" "alex" "bourassa" csmail4
-- >>> bloquerContact csmail4'' pers5  
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],
-- Contacts = [("robert.julien@smail.ca",Blanc),("bourassa.alex@smail.ca",Noir)]
bloquerContact :: CompteSmail -> Personne -> CompteSmail 
bloquerContact cs p = (CompteSmail (personne cs) (reception cs) (envoi cs) (spams cs) (preferences cs) (listeNoire p  (contacts cs) ) )


-- | liste noire
-- Les paramètres sont : la personne à chercher, la liste de contact, la liste de contact modifié
-- On a comme retour le Contact de la personne avec son Etat qui est modifié à Noir quand on le bloque
listeNoire :: Personne -> [Contact] -> [Contact]
listeNoire _ [] = []
listeNoire per ((Personne c (p,n),e):xs) = if (per == Personne c (p,n)) then ((Personne c (p,n), Noir) : xs)
                                           else (Personne c (p,n),e):listeNoire per xs


-- | Vider la boîte de reception, d'envoi ou de spams d'un compte
-- Les paramètres : Le compte à vider, le type de la boîte : Spams, Envoi ou Reception, le comptre modifié
--
-- >>> csmail4_1 = CompteSmail (Personne "noel.alice@smail.ca" ("alice","noel")) [trame3,trame4] [trame5,trame6] [(trame1,"majuscules"),(trame2,"points d'exclamation")] [] [(Personne "bourassa.alex@smail.ca" ("alex","bourassa"),Blanc),(Personne "robert.julien@smail.ca" ("julien","robert"),Blanc)]
-- >>> spams $ viderBoite csmail4_1 "Spams" 
-- []
-- >>> (envoi $ viderBoite csmail4_1 "Envoi",spams $ viderBoite csmail4_1 "Envoi")
-- ([],[(Trame (Entete (Date 2020 1 18) "AB CD EF" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","majuscules"),(Trame (Entete (Date 2019 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","points d'exclamation")])
-- >>> (envoi $ viderBoite csmail4_1 "Reception",reception $ viderBoite csmail4_1 "Reception")
-- ([Trame (Entete (Date 2017 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !",Trame (Entete (Date 2018 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) (Personne "robert.julien@smail.ca" ("julien","robert"))) "Allo Robert"],[])
viderBoite :: CompteSmail  -> String -> CompteSmail 
viderBoite cs s = if (s == "Spams") then (CompteSmail (personne cs) (reception cs) (envoi cs) [] (preferences cs) (contacts cs) )
                  else if (s == "Envoi") then (CompteSmail (personne cs) (reception cs) [] (spams cs) (preferences cs) (contacts cs) )
                  else if (s == "Reception") then (CompteSmail (personne cs) [] (envoi cs) (spams cs) (preferences cs) (contacts cs) ) else cs





