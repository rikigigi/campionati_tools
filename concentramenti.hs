import Data.List
import System.Random
import Data.Array.IO
import Control.Monad


data Gara = NessunaGara | Gara { squadraCasa :: Int, squadraFuori :: Int} deriving ( Eq)
instance Show Gara where
     show ( Gara a b ) = " "++(show a ) ++ "-" ++ ( show b )

gare 0 = []
gare nsquadre = let garep = gare (nsquadre - 1 ) 
                in garep ++ [ Gara a nsquadre | a <- [1..nsquadre-1] ] ++ [ Gara nsquadre a | a <- [1..nsquadre-1] ]

data Concentramento = NessunConcentramento | Concentramento {campoA :: [Gara], campoB :: [Gara], concentramentoPrecedente :: Concentramento }
instance Show Concentramento where
     show NessunConcentramento = "====\n"
     show ( Concentramento a b prec ) = (show a) ++ ( show b ) ++ "\n" ++ ( show prec )

sqUguale a ( Gara a2 b2) = a == a2 || a == b2

unaSqUguale ( Gara a1 b1 ) gara  = sqUguale a1 gara || sqUguale b1 gara

squadreDelleDueGare ( Gara a b ) gara1 gara2 = (sqUguale a gara1 || sqUguale a gara2 ) && (sqUguale b gara1 || sqUguale b gara2 )

garaPossibile gara ( Concentramento [] [] _ ) = True
garaPossibile gara ( Concentramento [] (garaB:[]) _ ) = not ( unaSqUguale gara garaB )
garaPossibile gara ( Concentramento (garaA:[]) [] _ ) = not ( unaSqUguale gara garaA )

garaPossibile gara ( Concentramento (garaA:gareA) (garaB:gareB) _ ) = squadreDelleDueGare gara garaA garaB


-- we need to remove equivalent labels in the choices: using capital symbols for used (not equivalent) labels and small symbol for unused (equivalent) labels, we must have the following:
-- - only 1 (one) (i,j) -- set of pairs where both are never used is equivalent to a single pair
-- - for each I, only one (I,j) or (j,I) -- 


aggiungiSimboloUsato simboliUsati simbolo = if simbolo `elem` simboliUsati then simboliUsati else simbolo:simboliUsati
aggiungiSimboliGara simboliUsati ( Gara a b ) = simboliUsati `aggiungiSimboloUsato` a `aggiungiSimboloUsato` b

filtraGara (validi, equivalentiab, equivalentia, equivalentib) simboliUsati gara@( Gara a b ) 
                       | a `elem` simboliUsati && b `elem` simboliUsati = ( gara:validi, equivalentiab, equivalentia, equivalentib )
		       | a `elem` simboliUsati = ( validi, equivalentiab, equivalentia, gara:equivalentib )
		       | b `elem` simboliUsati = ( validi, equivalentiab, gara:equivalentia, equivalentib )
		       | otherwise = ( validi, gara:equivalentiab, equivalentia, equivalentib )

filtraGare liste simboliUsati [] = liste
filtraGare liste simboliUsati (gara:gares) = filtraGare ( filtraGara liste simboliUsati gara ) simboliUsati gares

accumulatePartb g@(Gara a b ) [] = [g]
accumulatePartb g@(Gara a b )  g2@(((Gara a1 b1)):partlist) = if b == b1 then g2 else g:g2
accumulateParta g@(Gara a b ) [] = [g]
accumulateParta g@(Gara a b )  g2@(((Gara a1 b1)):partlist) = if a == a1 then g2 else g:g2

equivalenta listaa = let asort = sortBy (\(Gara a1 b1) (Gara a2 b2) -> compare b1 b2 ) listaa
                      in foldr accumulatePartb asort []
equivalentb listab = let bsort = sortBy (\(Gara a1 b1) (Gara a2 b2) -> compare a1 a2 ) listab
                      in foldr accumulateParta bsort []

firstIf [] = []
firstIf (a:as) = [a]

npartiteConcentramento NessunConcentramento = -1
npartiteConcentramento concentramento = length ( campoA concentramento ) + length ( campoB  concentramento  )

filtraGareEquivalenti simboliUsati listagare = let (validi, equivalentiab, equivalentia, equivalentib) = filtraGare ([], [], [], []) simboliUsati listagare
                                               in validi ++ ( firstIf equivalentiab ) ++ ( equivalenta equivalentia ) ++ ( equivalentb equivalentib )

concentramenti concentramento@( Concentramento gareA gareB prec ) listagare listasimboli
                              | npartiteConcentramento concentramento == 6 = []
                              | length gareA > length gareB = [ (Concentramento gareA (gareB++[gara]) prec , gara)  | gara <- gareNonEquivalenti, garaPossibile gara concentramento  ] 
			      | otherwise = [ ( Concentramento (gareA++[gara]) gareB prec , gara ) | gara <- gareNonEquivalenti, garaPossibile gara concentramento  ]
			      where gareNonEquivalenti = filtraGareEquivalenti listasimboli listagare

rimuoviGaraUsata (concentramento, gara ) listagare listasimboli = (concentramento, filter (\x -> x /= gara) listagare , aggiungiSimboliGara listasimboli gara )

newConcentramento concentramento = Concentramento [] [] concentramento


espandiConcentramenti [] = []
espandiConcentramenti ( (concentramento, listagare, listasimboli):altri ) = let newconc = concentramenti concentramento listagare listasimboli
                                                                                ngare = length listagare
                                                                            in case newconc of [] -> if ( ngare == 0 || npartiteConcentramento ( concentramentoPrecedente concentramento )  == 0 )
							                                                 then (concentramento, listagare, listasimboli):(espandiConcentramenti altri)
											                 else (newConcentramento concentramento, listagare, listasimboli ):(espandiConcentramenti altri)
							                                       newconc -> [ rimuoviGaraUsata conc_gara listagare listasimboli | conc_gara <- newconc ] ++ espandiConcentramenti altri 

showConcentramenti [] = ""
showConcentramenti  ((concentramento, listagare, listasimboli):altri) = show listagare ++ "\n" ++ show concentramento ++ ( showConcentramenti altri )

applyMany 0 what l = l
applyMany n what l = what $ applyMany (n-1) what l

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


main = do
        nc <- getLine
        nr <- getLine
        let nsq = read nc :: Int
        let nrep = read nr :: Int
	garetest <- shuffle $ gare nsq
        putStrLn $ showConcentramenti $ applyMany nrep espandiConcentramenti [( Concentramento [] [] NessunConcentramento, garetest, []) ] 

