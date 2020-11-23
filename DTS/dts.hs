import Control.Monad

main :: IO ()
main = do
    let loop arbol = do
        putStrLn ("Wich " ++ getNodeString arbol ++ "?")
        line <- getLine
        let answer = navigate arbol line
        let answerS = snd answer
        let answerA = fst answer
        if (answerS == "edible" || answerS == "poisonus" || answerS == "no_entries") then do
            putStrLn ("Prediction: " ++ answerS)
            return()
        else if (answerS == "error") then do
            putStrLn "error"
            loop arbol
            return()
        else do
            loop answerA
            return()

    contents <- readFile "agaricus-lepiota.data"
    let matrix = map (filter (\x -> x /= ',')) (lines contents)
    let a = buildTree [] matrix
    mapM_ (putStrLn) (printPreorder "" a)
    loop (a)
    putStrLn "End of navigation"
    return()

data Arbre a = Arbre a [Arbre a]
    deriving (Show)

buildTree :: [Int] -> [String] -> Arbre String
buildTree banned matrix
    | (fst finish) = Arbre (snd finish) []
    | banned == [1..22] = Arbre (moda matrix) []
    | otherwise  = Arbre (decodeAtr(splitAtr)) (map (buildTree new_banned) fragmentMat)
    where finish = modaAbsoluta matrix
          splitAtr = bestScore matrix banned
          new_banned = insert banned splitAtr
          fragmentMat = fragment matrix splitAtr

printPreorder :: String -> Arbre String -> [String]
printPreorder tab (Arbre x l) = [(tab ++ x)] ++ recursive_preOrder
    where recursive_preOrder = concat mergeLabelList
          mergeLabelList = map (\(x,y) -> [newtab++x]++y) recLabelList
          recLabelList = zip (listLabel x) recList --[(String,[String])]
          recList = map (printPreorder (newtab ++ "  ")) l --[[String]]
          newtab = tab ++ "   "

navigate :: Arbre String -> String -> (Arbre String,String)
navigate (Arbre x l) choice
    | indexC == Nothing = ((Arbre x l),"error")
    | otherwise = (branch,branchLabel)
    where indexC = elemIndex choice (listLabel x)
          branch = selectBranch (Arbre x l) (indexC)
          branchLabel = getNodeString branch

getNodeString :: Arbre String -> String
getNodeString (Arbre x l) = x

insert :: Ord a => [a] -> a -> [a]
insert [] x = x:[]
insert (y:xs) x
    | x <= y = x:y:xs
    | otherwise = y:(insert xs x)

bestScore :: [String] -> [Int] -> Int
bestScore matrix banned = snd (maximum puntuacionesTuplas)
    where candidatos = filter (\x -> notElem x banned) [1..22]
          puntuacionesTuplas = zip (map (sum) tuplasMax) candidatos
          tuplasMax = map (map (\(x,y) -> max x y)) tuplas
          tuplas = map (scoreAtribute matrix) candidatos

scoreAtribute :: [String] -> Int -> [(Int,Int)]
scoreAtribute []  i = map (\c -> (0,0)) (llistaLabelsAbreviats!!i)
scoreAtribute (x:xs)  i = zipWith (mergeTuples) x_score rec_score
    where rec_score = scoreAtribute xs  i
          x_score = map (scoreLabel x i) (llistaLabelsAbreviats!!i)
          
scoreLabel :: String -> Int -> Char -> (Int,Int)
scoreLabel x i lb
    | i == 11 && lb == '?' = (0,0) --he pensat que si no saps quin era el seu valor, no ha de puntuar?
                                    --tot i aixi com que es informacio, si que te una branca al arbre
                                    --si es dones el cas de que la resta de la informacio puntua alt
    | x!!i /= lb = (0,0)
    | x!!0 == 'e' = (1,0)
    | x!!0 == 'p' = (0,1)

mergeTuples :: (Int,Int) -> (Int,Int) -> (Int,Int)
mergeTuples (a,b) (x,y) = (a+x,b+y)

fragment :: [String] -> Int -> [[String]]
fragment [] i = map (\x -> []) (llistaLabelsAbreviats!!i)
fragment matrix i = map (selectFilter matrix i) (llistaLabelsAbreviats!!i)

selectFilter :: [String] -> Int -> Char -> [String]
selectFilter matrix i label = filter (\x -> (x!!i) == label) matrix

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex y xs = getIndexI xs y 0
  where
      getIndexI :: (Eq a) => [a] -> a -> Int -> Maybe Int
      getIndexI [] _ _ = Nothing
      getIndexI (x:xs) y i
          | x == y = Just i
          | otherwise = getIndexI xs y (i + 1)

selectBranch :: Arbre String -> Maybe Int -> Arbre String
selectBranch a Nothing = a
selectBranch (Arbre x l) (Just n) = l!!n

modaAbsoluta :: [String] -> (Bool,String)
--pot ser es mes eficient fusionar amb la funcio moda?
--he decidit emprar la early finish lazy evaluation
modaAbsoluta [] = (True,"no_entries")
modaAbsoluta matrix = if (head (head matrix) == 'e') then (allE,"edible")
                                                     else (allP,"poisonus")
    where allE = foldl (\x y -> x && (y=='e')) True matrixHead --lazy evaluation??
          allP = foldl (\x y -> x && (y=='p')) True matrixHead
          matrixHead = map (head) matrix
          
moda :: [String] -> String
moda matrix = if (fromInteger numE / fromIntegral (length matrix) > 0.5) then "edible"
                                                                         else "poisonus"
    where numE = foldl (\x y -> x + boolToInt(y=='e') ) 0 matrixHead
          matrixHead = map (head) matrix
          boolToInt b = if (b) then 1
                               else 0
decodeAtr :: Int -> String
decodeAtr x 
    | x == 1 = "cap-shape"
    | x == 2 = "cap-surface"
    | x == 3 = "cap-color"
    | x == 4 = "bruises"
    | x == 5 = "odor"
    | x == 6 = "gill-attachment"
    | x == 7 = "gill-spacing"
    | x == 8 = "gill-size"
    | x == 9 = "gill-color"
    | x == 10 = "stalk-shape"
    | x == 11 = "stalk-root"
    | x == 12 = "stalk-surface-above-ring"
    | x == 13 = "stalk-surface-below-ring"
    | x == 14 = "stalk-color-above-ring"
    | x == 15 = "stalk-color-below-ring"
    | x == 16 = "veil-type"
    | x == 17 = "veil-color"
    | x == 18 = "ring-number"
    | x == 19 = "ring-type"
    | x == 20 = "spore-print-color"
    | x == 21 = "population"
    | x == 22 = "habitat"

{-
getHeaders :: [String] -> [String]
getHeaders x =  foldr (zipWith (addIfnotElem)) (repeat "") x

addIfnotElem :: Char -> [Char] -> [Char]
addIfnotElem x xs
    | notElem x xs = insert xs x
    | otherwise = xs
    -}

llistaLabelsAbreviats :: [String]
llistaLabelsAbreviats = [   "ep",
                            "bcxfks",
                            "fgys",
                            "nbcgrpuewy",
                            "tf",
                            "alcyfmnps",
                            "adfn",
                            "cwd",
                            "bn",
                            "knbhgropuewy",
                            "et",
                            "bcuezr",
                            "fyks",
                            "fyks",
                            "nbcgopewy",
                            "nbcgopewy",
                            "pu",
                            "nowy",
                            "not",
                            "ceflnpsz",
                            "knbhrouwy",
                            "acnsvy",
                            "glmpuwd"   ]

listLabel :: String -> [String]
listLabel x
    | x == "poisonus" = []
    | x == "edible" = []
    | x == "no_entries" = []
    | x == "cap-shape" = ["bell","conical","convex","flat", "knobbed", "sunken"]
    | x == "cap-surface" = ["fibrous", "grooves", "scaly", "smooth"]
    | x == "cap-color" = ["brown", "buff", "cinnamon", "gray", "green", "pink", "purple", "red", "white", "yellow"]
    | x == "bruises" = ["bruises","no"]
    | x == "odor" = ["almond", "anise", "creosote", "fishy", "foul", "musty", "none", "pungent", "spicy"]
    | x == "gill-attachment" = ["attached", "descending", "free", "notched"]
    | x == "gill-spacing" = ["close", "crowded", "distant"]
    | x == "gill-size" = ["broad", "narrow"]
    | x == "gill-color" = ["black", "brown", "buff", "chocolate", "gray", "green", "orange", "pink", "purple", "red"]
    | x == "stalk-shape" = ["enlarging", "tapering"]
    | x == "stalk-root" = ["bulbous", "club", "cup", "equal", "rhizomorphs", "rooted", "missing"]
    | x == "stalk-surface-above-ring" = ["fibrous", "scaly", "silky", "smooth"]
    | x == "stalk-surface-below-ring" = ["fibrous", "scaly", "silky", "smooth"]
    | x == "stalk-color-above-ring" = ["brown", "buff", "cinnamon", "gray", "orange", "pink", "red", "white", "yellow"]
    | x == "stalk-color-below-ring" = ["brown", "buff", "cinnamon", "gray", "orange", "pink", "red", "white", "yellow"]
    | x == "veil-type" = ["partial", "universal"]
    | x == "veil-color" = ["brown", "orange", "white", "yellow"]
    | x == "ring-number" = ["none", "one=o", "two"]
    | x == "ring-type" = ["cobwebby", "evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone"]
    | x == "spore-print-color" = ["black", "brown", "buff", "chocolate", "green", "orange", "purple", "white", "yellow"]
    | x == "population" = ["abundant", "clustered", "numerous", "scattered", "several", "solitary"]
    | x == "habitat" = ["grasses", "leaves", "meadows", "paths", "urban", "waste", "woods"]
