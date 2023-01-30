data Fruct = Mar String Bool
           | Portocala String Int
ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia f = case f of (Portocala "Tarocco" _) -> True
                                  (Portocala "Moro" _) -> True
                                  (Portocala "Sanguinello" _) -> True
                                  otherwise -> False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum (map (\x -> if ePortocalaDeSicilia x then 1 else 0) l)

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length [(Mar ph bool) | (Mar ph bool) <- l, bool]

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa a = case a of (Caine _ r) -> Just r
                   (Pisica _) -> Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (&&) True (map (\(L l) -> (sum l) == n) linii)

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (&&) True (map (\(L l) -> all (>= 0) l) (filter (\(L l) -> length l == n) linii))

corect :: Matrice -> Bool
corect (M linii) = let list = map (\(L l) -> length l) linii in all (== head list) list