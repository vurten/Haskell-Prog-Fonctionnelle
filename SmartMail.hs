module SmartMail where 

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Char
import Personne
import Trame
import CompteSmail
import System.Directory

type SmartMail = Map.Map Courriel CompteSmail -- Dictionnaire de comptes smail
first_third (f,_,t) = (f,t) -- Petite fonction utilisée dans les tests

-- | Retourne un SmartMail vide
--
-- >>> nombreCompteSmail emptySmartMail
-- 0
emptySmartMail :: SmartMail 
emptySmartMail = Map.empty


-- | Ajout d'un compteSmail 
--
-- Note: Si un compte existe déjà avec le même courriel alors ne rien faire
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> courrielsComptes s1
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
ajoutCompte :: CompteSmail -> SmartMail-> SmartMail
ajoutCompte cs sm = if ( Map.member (Personne.courriel (personne cs)) sm) then sm
                    else Map.insert (Personne.courriel (personne cs)) cs sm

-- | Ajout de plusieurs comptes Smail 
--
-- >>> courrielsComptes $ ajoutComptes [csmail1, csmail2] emptySmartMail
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
ajoutComptes::  [CompteSmail] -> SmartMail -> SmartMail 
ajoutComptes [] sm = sm
ajoutComptes (x:xs) sm = ajoutComptes xs (ajoutCompte x sm) 

-- | Affiche tous les courriels de comptes Smail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> courrielsComptes s1
-- ["nkambou.roger@smail.ca","tato.ange@smail.ca"]
-- >>> courrielsComptes emptySmartMail
-- []
courrielsComptes ::SmartMail-> [Courriel]
courrielsComptes sm = Map.keys sm

-- | Retourne nombre de CompteSmail contenu dans SmartMail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> nombreCompteSmail s1
-- 2
-- >>> nombreCompteSmail emptySmartMail
-- 0
nombreCompteSmail :: SmartMail -> Int
nombreCompteSmail sm = Map.size sm

-- | Retoune le comptre Smail associé à un courriel dans SmartMail
-- Note : On suppose que le courriel passé en argumemnt est associé à un compte existant du smartmail
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> obtenirCompte "tato.ange@smail.ca" s1
-- CompteSmail "tato.ange@smail.ca":
-- Recus = [Trame (Entete (Date 2020 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !"],
-- Envois = [],
-- Spams = [],
-- Contacts = [("nkambou.roger@smail.ca",Blanc),("noel.alice@smail.ca",Noir)]
obtenirCompte :: Courriel -> SmartMail -> CompteSmail
obtenirCompte c sm = sm Map.! c

-- | Déterminer la priorité d'un message non spam
-- Note: on suppose que la Trame en input est un message non spam
--    (1) Important= expediteur est un contact direct (qui n'est pas bloqué) ou est "equipesmartmail@smail.ca" 
--    (2) Normal = expediteur est de niveau 2 (est dans la liste de contacts d'au moins un des contacts du recepteur et n'est pas bloqué). Si il est bloqué par au moins un contact, alors il passe au niveau suivant.
--    (3) Faible = expediteur n'est ni un contact direct, n'est ni de niveau 2, n'est ni l'équipe smartMail.ca
--
-- >>> s1 = ajoutCompte csmail5 $ ajoutCompte csmail4 $ ajoutCompte csmail2 $ ajoutCompte csmail1 emptySmartMail
-- >>> prioriteMessage s1 trameBienvenue2
-- Important
-- >>> prioriteMessage s1 tramet
-- Faible
-- >>> prioriteMessage s1 tramett
-- Normal
prioriteMessage :: SmartMail -> Trame -> Priorite
prioriteMessage sm t =  let tabCourrielDirect = (map courriel (map fst (contacts (obtenirCompte  (receveur t) sm)))) in
                        if ( (emetteur t) == "equipesmartmail@smail.ca") then Important 
                        else if ( (emetteur t) `elem`  (tabCourrielDirect) ) then
                            if ((extraireEtat (personne (obtenirCompte (emetteur t) sm)) (contacts (obtenirCompte (receveur t) sm))) == Blanc ) then Important
                            else Faible
                        else if ( (emetteur t) `notElem` (tabCourrielDirect)  ) then 
                            if ( (extraireEtat (personne (obtenirCompte (emetteur t) sm)) (concat (contactDeContact sm (tabCourrielDirect) )) ) == Blanc ) then Normal 
                            else Faible
                        else Faible

-- methode qui prend l emetteur et cherche dans les contacts du receveur si son etat est blanc ou noir
extraireEtat:: Personne -> [(Personne,Etat)] -> Etat
extraireEtat _ [] = Blanc
extraireEtat p (x:xs) = if (p == (fst x)) then (snd x) else extraireEtat p xs

-- methode qui prend les contacts du receveur et affiche tout leurs contacts
contactDeContact :: SmartMail -> [Courriel] -> [[Contact]]
contactDeContact sm [] = []
contactDeContact sm (x:xs) =  (contacts (obtenirCompte x sm)) : contactDeContact sm xs

-------------------------------------------------------------------
---------- FILTRES ANTI SPAM ET ANTI HAMEÇONNAGE ------------------ 
-------------------------------------------------------------------
-- | Filtrage de l'enveloppe de la trame
-- Ce filtre s’exécute uniquement sur l'entête de la trame et non sur son contenu.
-- Une trame est détectée comme spam par le filtrageEnveloppe si au moins l'une des conditions suivantes est vraie: 
--      (1) tous les caractères de l'objet sont en majuscules (Explications = "classique_enveloppe")
--      (2) l'objet contient au moins 2 points d'exclamation "!" (Explications = "classique_enveloppe")
--      (3) l'objet contient au moins 2 points d'interrogation "?" (Explications = "classique_enveloppe")
--      (4) l'objet est vide (Explications = "objet vide")
--      (5) l'objet contient le caractère "$" (Explications = "classique_enveloppe")
--      (6) l'emetteur a été bloqué (Etat = Noir) par le destinataire   (Explications = "contact bloque")  
--
-- >>> ssm2 = Map.fromList [("nkambou.roger@smail.ca", CompteSmail (Personne "nkambou.roger@smail.ca" ("roger","nkambou")) [Trame (Entete (Date 2018 10 2) "Bienvenue" pers0 pers1) "Bienvenue dans votre boite smartMail !"] [] [] [] []),("robert.julien@smail.ca",CompteSmail (Personne "robert.julien@smail.ca" ("julien","robert")) [] [] [] [] [(Personne "tato.ange@smail.ca" ("",""),Noir)]),("tato.ange@smail.ca", CompteSmail (Personne "tato.ange@smail.ca" ("ange","tato")) [Trame (Entete (Date 2018 10 2) "Bienvenue" pers0 pers1) "Bienvenue dans votre boite smartMail !"] [] [] [] [])]
-- >>> filtrageEnveloppe trame6 ssm2 
-- (Spam,Trame (Entete (Date 2018 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) (Personne "robert.julien@smail.ca" ("julien","robert"))) "Allo Robert","contact bloque")
-- >>> filtrageEnveloppe trame1 ssm2 
-- (Spam,Trame (Entete (Date 2020 1 18) "AB CD EF" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame2 ssm2 
-- (Spam,Trame (Entete (Date 2019 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame3 ssm2 
-- (Spam,Trame (Entete (Date 2020 1 1) "?Bien venue?" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trame4 ssm2 
-- (Spam,Trame (Entete (Date 2018 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","objet vide")
-- >>> filtrageEnveloppe trame5 ssm2 
-- (Spam,Trame (Entete (Date 2017 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe")
-- >>> filtrageEnveloppe trameBienvenue1 ssm2 
-- (NonSpam,Trame (Entete (Date 2020 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","")
-- >>> filtrageEnveloppe trameBienvenue2 ssm2 
-- (NonSpam,Trame (Entete (Date 2020 2 10) "Bienvenue" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "nkambou.roger@smail.ca" ("roger","nkambou"))) "Bienvenue dans votre boite smartMail !","")
filtrageEnveloppe :: Trame -> SmartMail -> (TypeMessage, Trame, Explications) 
filtrageEnveloppe t sm = if ( (objet t) == [] ) then (Spam, t, "objet vide")
                         else if (length $ filter (/=' ') (objet t)) == (length $ filter(isUpper) (objet t)) then (Spam, t, "classique_enveloppe")
                         else if ( (length (filter (== '?') (objet t)))  >=2 ) then (Spam, t, "classique_enveloppe")
                         else if ( (length (filter (== '!') (objet t))) >=2 ) then (Spam, t, "classique_enveloppe")
                         else if ( (length (filter (== '$') (objet t))) >=1 ) then (Spam, t, "classique_enveloppe")
                         else if ( (emetteur t) `elem` (map courriel (map fst (contacts (obtenirCompte  (receveur t) sm)))) ) then
                                    if ((extraireEtat (personne (obtenirCompte (emetteur t) sm)) (contacts (obtenirCompte (receveur t) sm))) == Noir ) 
                                        then (Spam, t,"contact bloque") else (NonSpam, t, "")
                         else (NonSpam, t, "")


-- | Envoyer message
--
-- >>> s1 = ajoutCompte csmail2 $ ajoutCompte csmail1 $ ajoutCompte csmail0 emptySmartMail
-- >>> s2 = envoyerMessage  s1 ("equipesmartmail@smail.ca", "tato.ange@smail.ca", "Bi!en! venue!", "Bienvenue dans votre boite smartMail !")  
-- >>> length (envoi $ obtenirCompte "equipesmartmail@smail.ca" s2)
-- 3
-- >>> length (reception $ obtenirCompte "tato.ange@smail.ca" s2)
-- 1
-- >>> s3 = ajoutCompte csmail3 s2
-- >>> s4 = envoyerMessage s3 ("equipesmartmail@smail.ca", "tato.ange@smail.ca", "?Bien venue?", "Bienvenue dans votre boite smartMail !")
-- >>> length (reception $ obtenirCompte "tato.ange@smail.ca" s4)
-- 1
-- >>> length (envoi $ obtenirCompte "tato.ange@smail.ca" s4)
-- 0
envoyerMessage :: SmartMail -> Message -> SmartMail
envoyerMessage sm (c1, c2, o, c) =  let nouveauTrame = (Trame (Entete (Date 2020 02 18) o (personne (obtenirCompte c1 sm)) (personne (obtenirCompte c2 sm))) c)
                                        a = nouveauTrame : (envoi (obtenirCompte c1 sm)) in
                                    if ( c1 `elem` (Map.keys sm) ) then 
                                       ajoutCompte2 (CompteSmail ( personne (obtenirCompte c1 sm)) (reception (obtenirCompte c1 sm)) a (spams (obtenirCompte c1 sm)) (preferences (obtenirCompte c1 sm)) (contacts (obtenirCompte c1 sm)) ) sm
                                        else sm


-- methode qui ajoute un compteSmail dans SmarMail j'ai du le faire car dans la meme methode en haut elle ne faisait pas l'ajout car le cs est deja dans ls sm
ajoutCompte2 :: CompteSmail -> SmartMail-> SmartMail
ajoutCompte2 cs sm = Map.insert (Personne.courriel (personne cs)) cs sm 


-------------------------------------------------------------------
--------------------   STATISTIQUES ET AFFICHAGES   ---------------   
-------------------------------------------------------------------

-- | Donne le nombre de spams recus dans le systeme de messagerie au complet
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> nbTotalSpams s1
-- 0
-- >>> nbTotalSpams s
-- 8
nbTotalSpams :: SmartMail -> Int
nbTotalSpams sm = sum [length (spams csm) | csm <- Map.elems (sm)]

-- | Retourne l'ensemble de tous les spams du système dans une même liste
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> tousLesSpams s1
-- []
-- >>> tousLesSpams s
-- [(Trame (Entete (Date 2020 10 10) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "0de 3Bienvenue dans 1 et 9 | pour tp1et2","classique_contenu, 67% de mots comportant des caracteres etranges."),(Trame (Entete (Date 2020 1 15) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "offre de Bienvenue dans  publicit\233 gratuit pour voyage special","publicitaire, 56% de mots suspects."),(Trame (Entete (Date 2019 18 10) "un message" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans sexe du viagra chaud nu","hameconnage, 57% de mots suspects."),(Trame (Entete (Date 2017 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2018 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","objet vide"),(Trame (Entete (Date 2019 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2019 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe"),(Trame (Entete (Date 2020 1 18) "AB CD EF" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) (Personne "tato.ange@smail.ca" ("ange","tato"))) "Bienvenue dans votre boite smartMail !","classique_enveloppe")]
tousLesSpams:: SmartMail -> [(Trame, Explications)]
tousLesSpams sm = foldr (\x acc -> acc ++ spams x) [] (Map.elems (sm) )

-- | Retourne la liste qui associe à chaque inscrit (son courriel seulement), le nombre de spams recus. Résultat par ordre alphabétique de courriels
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> statSpamsRecus s1
-- [("bourassa.alex@smail.ca",0),("equipesmartmail@smail.ca",0),("nkambou.roger@smail.ca",0),("noel.alice@smail.ca",0),("robert.julien@smail.ca",0),("tato.ange@smail.ca",0)]
-- >>> statSpamsRecus s
-- [("bourassa.alex@smail.ca",0),("equipesmartmail@smail.ca",0),("nkambou.roger@smail.ca",0),("noel.alice@smail.ca",0),("robert.julien@smail.ca",0),("tato.ange@smail.ca",8)]
statSpamsRecus :: SmartMail -> [(String, Int)]
statSpamsRecus sm = [(courriel (personne csm), length (spams csm) ) | csm <- Map.elems(sm)]

-- | Produit une liste qui associe à chaque inscrit (son courriel seulement), le nombre de spams produit
--
-- >>> s1 = ajoutComptes [csmail0, csmail1, csmail2, csmail3, csmail4, csmail5] emptySmartMail
-- >>> s = envoyerMessage_Plusieurstrames s1 [trame1, trame2,trame2,trame4,trame5,trame6,trame7,trame8,trame9]
-- >>> statSpamsEnvoyes s1
-- []
-- >>> statSpamsEnvoyes s
-- [("equipesmartmail@smail.ca",8)]
statSpamsEnvoyes :: SmartMail -> [(Courriel, Int)]
statSpamsEnvoyes sm = [(courriel (personne csm), length (envoi csm) ) | csm <- Map.elems(sm)]


-- | Reformater boîte
-- Afin de faciliter la gestion des messages, l'administrateur voudrait pouvoir reorganiser toutes les boîtes (reception, spams, envoi) de la manière suivante:
--       chaque boîte doit devenir un dictionnaire avec le courriel de l'emetteur comme clé  et comme valeur, une liste des messages reçus de cette personne. 
--       Les messages de cette liste doivent comporter uniquement la date (au format "jj/mm/aaaa"),
--       l'objet et le contenu du message (sous forme d'un triplet). 
-- Voudevez donc écrire la fonction reformaterBoite qui reformate une boîte selon le principe décrit.
-- Donnée: Une boîte de messages (une liste de trames)
-- Sortie: Le dictionnaire correspondant. 
--
-- >>> reformaterBoite [trame7, trame8, trame9, trame10, trame11]
-- fromList [("equipesmartmail@smail.ca",[("18/10/2020","un message","allo|allo"),("1/2/2020","un message","bien1venue"),("10/10/2020","un message","0de 3Bienvenue dans 1 et 9 | pour tp1et2"),("15/1/2020","un message","offre de Bienvenue dans  publicit\233 gratuit pour voyage special"),("10/18/2019","un message","Bienvenue dans sexe du viagra chaud nu")])]
reformaterBoite :: [Trame] -> Map.Map String [(String, String, String)]
reformaterBoite xs = foldr(\x acc -> Map.insert (emetteur x) [(dateToString (date x), objet x, contenu x)] acc) Map.empty xs

dateToString (Date a m j) = show j ++ "/" ++ show m ++ "/" ++ show a




-- | Reformater compte
--  Utiliser la fonction reformaterBoite pour écrire la fonction reformaterCompte qui reconstruit  la structure d'un compte smail. Le résultat doit comporter l'ensemble des messages provenant des 
--  trois boîtes y compris celle des spams. Pour la boîte des spams, on ne s'interessera qu'aux Trames de message sans explications.
--  Donnée:  Un compte smail
--  Sortie:  Un triplet comportant les trois boîtes dans l'ordre et dans leur nouvelle format.
--
-- >>> ssm4 = Map.fromList [("nkambou.roger@smail.ca",CompteSmail (Personne "nkambou.roger@smail.ca" ("roger","nkambou")) [Trame (Entete (Date 2018 10 2) "Bienvenue" pers0 pers2) "Bienvenue dans votre boite smartMail !"] [Trame (Entete (Date 2018 10 26) "toto" pers2 pers1) "tato "] [] [] []),("tato.ange@smail.ca",CompteSmail (Personne "tato.ange@smail.ca" ("ange","tato")) [Trame (Entete (Date 2018 10 26) "toto" pers2 pers1) "tato ", Trame (Entete (Date 2018 10 25) "ggg" pers4 pers1) "jjjj",Trame (Entete (Date 2018 10 2) "Bienvenue" pers0 pers1) "Bienvenue dans votre boite smartMail !"] [] [(Trame (Entete (Date 2018 10 25) "ghgh jjhjh" pers3 pers1) "hgg6gg jhh7hh","Classique!! Contient 100% de mots comportant des caracteres etranges.")] [] []),("toto.tartampion@smail.ca",CompteSmail (Personne "toto.tartampion@smail.ca" ("tartampion","toto")) [] [Trame (Entete (Date 2018 10 25) "ghgh jjhjh" pers3 pers1) "hgg6gg jhh7hh",Trame (Entete (Date 2018 10 25) "ggg" pers3 pers1) "jjjj"] [] [] [])]
-- >>> reformaterCompte $ obtenirCompte "tato.ange@smail.ca" ssm4
-- (fromList [("equipesmartmail@smail.ca",[("2/10/2018","Bienvenue","Bienvenue dans votre boite smartMail !")]),("nkambou.roger@smail.ca",[("26/10/2018","toto","tato ")]),("noel.alice@smail.ca",[("25/10/2018","ggg","jjjj")])],fromList [],fromList [("robert.julien@smail.ca",[("25/10/2018","ghgh jjhjh","hgg6gg jhh7hh")])])
reformaterCompte :: CompteSmail
     -> (Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)])
reformaterCompte csm = (reformaterBoite $ reception csm, reformaterBoite  
                            (foldr (\x acc -> if snd x == "" then fst x : acc else acc) [] $ spams csm), reformaterBoite $ envoi csm)



