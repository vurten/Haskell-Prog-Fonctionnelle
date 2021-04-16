module Personne where 

type Nom = String
type Prenom = String
type Courriel = String
type Signature = (Prenom, Nom)
data Personne = Personne 
                Courriel 
                Signature 
                deriving (Show, Read) -- Courriel unique
instance Eq Personne where
    (==) (Personne cour1 _ ) (Personne cour2 _ ) = cour1 == cour2

-- | Retourne lle courriel d'une personne
courriel :: Personne -> String
courriel (Personne c _)= c

-- | Vérifie si le courriel passé en paramètre est conforme. 
-- Un courriel smail conforme est de la forme <xxxx@smail.ca>
--
-- >>> courrielValide "tato.ange@samail.ca"
-- False
-- >>> courrielValide "tato.ange@smail.cal"
-- False
-- >>> courrielValide "tatoooange@smail.ca"
-- True 
courrielValide :: [Char] -> Bool
courrielValide [] = False
courrielValide xs = (length xs > 9 ) && (snd ( span (/= '@') xs) ) == "@smail.ca"
