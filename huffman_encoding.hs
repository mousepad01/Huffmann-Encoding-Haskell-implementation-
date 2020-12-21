import Test.QuickCheck

-- coduri Huffman
-- ideea de implementare:
-- pe scurt, voi avea un heap in care fiecare nod este radacina unui arbore binar
-- initial formez heap ul cu noduri care sunt frecventele caracterelor,
-- fiecare frecventa fiind asociata unui caracter
-- la un anumit pas extrag de doua ori minimul
-- unesc cele doua noduri corespunzator, si adaug noul nod obtinut din nou in heap
-- fac asta pana raman cu un singur nod, 
-- moment in care acel nod va fi radacina arborelui Huffman corespunzator

data HuffmanTree = EmptyTree | TreeNode { freq :: Int, 
                                          char :: Maybe Char, 
                                          leftTree :: HuffmanTree, 
                                          rightTree :: HuffmanTree } deriving (Eq)

-- daca subarborele curent contine un nod(nodul) cu exact 1 fiu => Partial
-- daca subarborele curent contine noduri cu 0 sau 2 fii => Full
data State = Full | Partial deriving (Eq, Show)

instance Show HuffmanTree where
    show EmptyTree = "null"
    show node = "(" ++ show (freq node) ++ (if char node == Nothing then "" else "," ++ show (getVal(char node))) ++ ") " ++ show (leftTree node) ++ " " ++ show (rightTree node)

combine :: State -> State -> State
combine Full Partial = Partial
combine Partial Full = Partial
combine Full Full = Full
                                           
-- size imi indica numarul de noduri din (sub)arborele curent
-- ma ajuta sa identific pozitia de inserare
-- alaturi de state (cand si subarborele stang si cel drept sunt Full)

-- cazuri:
--
-- S - Full, R - Full:
--     size S == size R => S
--     size S > size R => R
--
-- S - Full, R - Partial => R
--
-- S - Partial, R - Full => S
--
-- restul cazurilor sunt imposibile datorita proprietatilor de Heap

data Heap = EmptyHeap | HeapNode { hTree :: HuffmanTree,
                                   leftHeap :: Heap,
                                   rightHeap :: Heap,
                                   size :: Int,
                                   state :: State } deriving(Eq)

getVal :: Maybe a -> a
getVal (Just val) = val

-- afisez ca un array
instance Show Heap where
    show EmptyHeap = "null "
    show HeapNode {hTree = ht,
                   leftHeap = lh,
                   rightHeap = rh,
                   size = s,
                   state = st} = show (freq ht) ++ "(" ++ (if char ht == Nothing then "" else [getVal (char ht)]) ++ ")" ++ show st ++ "," ++ show s ++ " " ++ show lh ++ show rh

-----------------------------------------------------------------
--                        INSERARE
-----------------------------------------------------------------

insertInHeap :: Heap -> Heap -> Heap

-- cand inserez nodul intr-o pozitie nula
insertInHeap EmptyHeap toInsert = toInsert

-- cand tatal pozitiei de inserat are ambii fii nuli
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = EmptyHeap, 
                       rightHeap = EmptyHeap, 
                       size = 1,
                       state = Full} toInsert

    | thisFreq <= fr = HeapNode {hTree = huftree,
                                 leftHeap = updatedHeap,
                                 rightHeap = EmptyHeap,
                                 size = 2,
                                 state = Partial}

    | thisFreq > fr  = updatedHeap {leftHeap = HeapNode {hTree = huftree,
                                                         leftHeap = EmptyHeap, 
                                                         rightHeap = EmptyHeap, 
                                                         size = 1,
                                                         state = Full},
                                    size = 2,
                                    state = Partial}
    where
        updatedHeap = insertInHeap EmptyHeap toInsert

        thisFreq = freq huftree
        fr = freq (hTree toInsert)

-- cand tatal pozitiei de inserat are doar fiul drept nenul
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = lheap, 
                       rightHeap = EmptyHeap, 
                       size = 2,
                       state = Partial} toInsert

    | thisFreq <= fr = HeapNode {hTree = huftree,
                                 leftHeap = lheap,
                                 rightHeap = updatedHeap,
                                 size = 3,
                                 state = Full}

    | thisFreq > fr  = updatedHeap {leftHeap = lheap,
                                    rightHeap = HeapNode {hTree = huftree,
                                                          leftHeap = EmptyHeap, 
                                                          rightHeap = EmptyHeap, 
                                                          size = 1,
                                                          state = Full},
                                    size = 3,
                                    state = Full}
    where
        updatedHeap = insertInHeap EmptyHeap toInsert

        thisFreq = freq huftree
        fr = freq (hTree toInsert)

-- cand ambii fii ai nodului curent sunt nenuli si trebuie sa cobor
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = lheap, 
                       rightHeap = rheap, 
                       size = s,
                       state = st} toInsert

    -- aici fac reordonarea corespunzator (daca e cazul), si returnez heap ul updatat
    | thisFreq <= newFreq && whichUpdated == "left" = HeapNode {hTree = huftree,
                                                                leftHeap = updatedHeap,
                                                                rightHeap = rheap,
                                                                size = 1 + size updatedHeap + size rheap,
                                                                state = state rheap `combine` state updatedHeap}

    | thisFreq <= newFreq && whichUpdated == "right" = HeapNode {hTree = huftree,
                                                                 leftHeap = lheap,
                                                                 rightHeap = updatedHeap,
                                                                 size = 1 + size updatedHeap + size lheap,
                                                                 state = state lheap `combine` state updatedHeap}

    | thisFreq > newFreq && whichUpdated == "left" = updatedHeap {leftHeap = HeapNode {hTree = huftree,
                                                                                       leftHeap = leftOfUpdated,
                                                                                       rightHeap = rightOfUpdated,
                                                                                       size = sizeOfUpdated,
                                                                                       state = stateOfUpdated},
                                                                  rightHeap = rheap,
                                                                  size = 1 + sizeOfUpdated + size rheap,
                                                                  state = stateOfUpdated `combine` state rheap}

    | thisFreq > newFreq && whichUpdated == "right" = updatedHeap {leftHeap = lheap,
                                                                   rightHeap = HeapNode {hTree = huftree,
                                                                                         leftHeap = leftOfUpdated,
                                                                                         rightHeap = rightOfUpdated,
                                                                                         size = sizeOfUpdated,
                                                                                         state = stateOfUpdated},
                                                                   size = 1 + sizeOfUpdated + size lheap,
                                                                   state = stateOfUpdated `combine` state lheap}
    where
        -- aici in where lansez recursiv insertia conrespunzator, in functie de state si size
        thisFreq = freq huftree

        whichUpdated 
            | state lheap == Full && state rheap == Full && size lheap == size rheap = "left"
            | state lheap == Full && state rheap == Full && size lheap > size rheap  = "right"
            | state lheap == Full && state rheap == Partial = "right"
            | state lheap == Partial && state rheap == Full = "left"
            | state lheap == Full && state rheap == Full && size lheap < size rheap = error "Imposibil"
            | state lheap == Partial && state rheap == Partial = "Imposibil2"

        updatedHeap 
            | whichUpdated == "left"  = insertInHeap lheap toInsert
            | whichUpdated == "right" = insertInHeap rheap toInsert
        
        sizeOfUpdated = size updatedHeap
        stateOfUpdated = state updatedHeap

        newFreq = freq (hTree updatedHeap)

        -- pentru unele cazuri ale rearanjarii
        leftOfUpdated = leftHeap updatedHeap
        rightOfUpdated = rightHeap updatedHeap

-----------------------------------------------------------------
--                 STERGERE ULTIMUL ELEMENT
-----------------------------------------------------------------

-- functia returneaza (fst) -> heap-ul modificat dupa extragere, 
--                    (snd) -> arborele huffman al elementului extras

lastFromHeap :: Heap -> (Heap, HuffmanTree)

-- cazul cand heap ul este gol (apelul nu prea are sens)
lastFromHeap EmptyHeap = (EmptyHeap, EmptyTree)

-- cazul cand am doar radacina
lastFromHeap HeapNode {hTree = huftree,
                       leftHeap = EmptyHeap,
                       rightHeap = EmptyHeap,
                       size = 1,
                       state = Full} = (EmptyHeap, huftree)

-- cazul cand nodul de extras este fiul stang al heap ului curent (partial)
lastFromHeap HeapNode {hTree = huftree,
                       leftHeap = lheap,
                       rightHeap = EmptyHeap,
                       size = 2,
                       state = Partial} = (HeapNode {hTree = huftree,
                                                     leftHeap = EmptyHeap,
                                                     rightHeap = EmptyHeap,
                                                     size = 1,
                                                     state = Full},
                                           hTree lheap)

-- cazul cand nodul de extras este fiul drept al nodului curent
lastFromHeap HeapNode {hTree = huftree,
                       leftHeap = lheap,
                       rightHeap = rheap,
                       size = 3,
                       state = Full} = (HeapNode {hTree = huftree,
                                                  leftHeap = lheap,
                                                  rightHeap = EmptyHeap,
                                                  size = 2,
                                                  state = Partial},
                                        hTree rheap)

-- cazul cand nodul curent are ambii fii nenuli, si cobor in continuare
lastFromHeap HeapNode {hTree = huftree,
                       leftHeap = lheap,
                       rightHeap = rheap,
                       size = s,
                       state = st} 

    | whichUpdated == "left" = (HeapNode {hTree = huftree,
                                          leftHeap = updatedHeap,
                                          rightHeap = rheap,
                                          size = s - 1,
                                          state = state rheap `combine` state updatedHeap},
                                extractedTree)

    | whichUpdated == "right" = (HeapNode {hTree = huftree,
                                           leftHeap = lheap,
                                           rightHeap = updatedHeap,
                                           size = s - 1,
                                           state = state lheap `combine` state updatedHeap},
                                 extractedTree)
    where
        -- cobor in functie de size si state al subarborilor stangi si drepti
        whichUpdated 
            | state lheap == Full && state rheap == Full && size lheap == size rheap = "right"
            | state lheap == Full && state rheap == Full && size lheap > size rheap  = "left"
            | state lheap == Full && state rheap == Partial = "right"
            | state lheap == Partial && state rheap == Full = "left"
            | state lheap == Full && state rheap == Full && size lheap < size rheap = error "Imposibil"
            | state lheap == Partial && state rheap == Partial = "Imposibil2"

        returnedPair
            | whichUpdated == "left"  = lastFromHeap lheap 
            | whichUpdated == "right" = lastFromHeap rheap 

        (updatedHeap, extractedTree) = returnedPair
        
        sizeOfUpdated = size updatedHeap
        stateOfUpdated = state updatedHeap

-----------------------------------------------------------------
--                   HEAPIFY PE RADACINA
-----------------------------------------------------------------

repair :: Heap -> Heap

-- cand nu am nici un nod
repair EmptyHeap = EmptyHeap

-- cand am doar radacina / cand am ajuns la un nod cu ambii fii nuli, nu fac practic nimic
repair HeapNode {hTree = huftree,
                leftHeap = EmptyHeap,
                rightHeap = EmptyHeap,
                size = 1,
                state = Full} = HeapNode {hTree = huftree,
                                          leftHeap = EmptyHeap,
                                          rightHeap = EmptyHeap,
                                          size = 1,
                                          state = Full}

-- cand nodul de recalibrat curent are doar fiul stang nenul
repair HeapNode {hTree = huftree,
                 leftHeap = lheap,
                 rightHeap = EmptyHeap,
                 size = 2,
                 state = Partial}
    
    | freq huftree > freq lhuftree = HeapNode {hTree = lhuftree,
                                               leftHeap = HeapNode {hTree = huftree,
                                                                    leftHeap = EmptyHeap,
                                                                    rightHeap = EmptyHeap,
                                                                    size = 1,
                                                                    state = Full},
                                               rightHeap = EmptyHeap,
                                               size = 2,
                                               state = Partial}

    | freq huftree <= freq lhuftree = HeapNode {hTree = huftree,
                                                leftHeap = lheap,
                                                rightHeap = EmptyHeap,
                                                size = 2,
                                                state = Partial}
    where
        lhuftree = hTree lheap

-- cand nodul de recalibrat curent are ambii fii nenuli
-- daca e cazul, propag recalibrarea mai departe
repair HeapNode {hTree = huftree,
                 leftHeap = lheap,
                 rightHeap = rheap,
                 size = s,
                 state = st}

    | freqCase == "isHeap" = HeapNode {hTree = huftree,
                                       leftHeap = lheap,
                                       rightHeap = rheap,
                                       size = s,
                                       state = st}
    
    | freqCase == "repairLeft" = HeapNode {hTree = lhuftree,
                                           leftHeap = repairedHeap,
                                           rightHeap = rheap,
                                           size = s,
                                           state = st}

    | freqCase == "repairRight" = HeapNode {hTree = rhuftree,
                                            leftHeap = lheap,
                                            rightHeap = repairedHeap,
                                            size = s,
                                            state = st}

    where

        lhuftree = hTree lheap
        rhuftree = hTree rheap

        freqCase
            | freq huftree <= freq lhuftree && freq huftree <= freq rhuftree = "isHeap"
            | freq huftree <= freq lhuftree && freq huftree > freq rhuftree  = "repairRight"
            | freq huftree > freq lhuftree && freq huftree <= freq rhuftree  = "repairLeft"
            | freq huftree > freq lhuftree && freq huftree > freq rhuftree && freq lhuftree <= freq rhuftree = "repairLeft"
            | freq huftree > freq lhuftree && freq huftree > freq rhuftree && freq lhuftree > freq rhuftree  = "repairRight" 

        repairedHeap
            | freqCase == "repairLeft"  = repair (lheap {hTree = huftree})
            | freqCase == "repairRight" = repair (rheap {hTree = huftree})

-----------------------------------------------------------------
--                     EXTRAGERE MINIM 
-----------------------------------------------------------------

extractMin :: Heap -> (Heap, HuffmanTree)

-- desi functiile lastFromHeap si repair trateaza toate cazurile
-- voi trata aici direct cazurile particulare de baza, pentru simplitate

-- cand nu am nici un nod in heap
extractMin EmptyHeap = (EmptyHeap, EmptyTree)

-- cand am doar radacina
extractMin HeapNode {hTree = huftree,
                     leftHeap = EmptyHeap,
                     rightHeap = EmptyHeap,
                     size = s,
                     state = st} = (EmptyHeap, huftree)

-- cand am cel putin doua noduri
extractMin heap = (repair (newHeap {hTree = extracted}), hTree heap)
    where
        (newHeap, extracted) = lastFromHeap heap

-----------------------------------------------------------------
--                     HEAP DIN LISTA
-----------------------------------------------------------------

listTransformation :: [(Char, Int)] -> [Heap]
listTransformation [] = []
listTransformation ((ch, fr):xs) = (HeapNode {hTree = TreeNode {freq = fr, 
                                                               char = Just ch,
                                                               leftTree = EmptyTree,
                                                               rightTree = EmptyTree},
                                              leftHeap = EmptyHeap,
                                              rightHeap = EmptyHeap,
                                              size = 1,
                                              state = Full}) : listTransformation xs

heapify :: [(Char, Int)] -> Heap
heapify frList = heapifyNodeList nodeList
    where
        nodeList = listTransformation frList

        heapifyNodeList [] = EmptyHeap
        heapifyNodeList (h:hs) = insertInHeap (heapifyNodeList hs) h

-----------------------------------------------------------------
--            CODURILE HUFFMAN SI FUNCTIILE ASOCIATE
-----------------------------------------------------------------

createHuffmanTree :: Heap -> HuffmanTree

-- cand nu am nici un nod (cazul are loc doar daca primeste un EmptyHeap ca argument la inceput)
createHuffmanTree EmptyHeap = EmptyTree

-- cand am un singur nod (ramas) in heap, care contine intregul arbore huffman cautat
createHuffmanTree HeapNode {hTree = huftree,
                            leftHeap = EmptyHeap,
                            rightHeap = EmptyHeap,
                            size = 1,
                            state = Full} = huftree

-- cand heap ul are >= 2 noduri in el
createHuffmanTree heap = createHuffmanTree newHeap
    where

        (fstNewHeap, fstExtracted) = extractMin heap
        (sndNewHeap, sndExtracted) = extractMin fstNewHeap

        mergedTree = TreeNode {freq = freq fstExtracted + freq sndExtracted,
                               char = Nothing,
                               leftTree = fstExtracted,
                               rightTree = sndExtracted}

        newHeap = insertInHeap sndNewHeap (HeapNode {hTree = mergedTree,
                                                     leftHeap = EmptyHeap,
                                                     rightHeap = EmptyHeap,
                                                     size = 1,
                                                     state = Full})

getHuffmanTree :: [(Char, Int)] -> HuffmanTree
getHuffmanTree freqList = createHuffmanTree (heapify freqList)


getHuffmanCodes :: [(Char, Int)] -> [(Char, String)]
getHuffmanCodes [] = error "Nu se pot calcula coduri pentru multimea vida!"
getHuffmanCodes [(onlyCh, fr)] = [(onlyCh, "0")]
getHuffmanCodes freqList = sort (extractCodes "" (getHuffmanTree freqList)) (\p1 p2 -> fst p1 < fst p2)
    where
        extractCodes :: String -> HuffmanTree -> [(Char, String)]
        extractCodes _ EmptyTree = []
        extractCodes codePreffix tree 
            | char tree == Nothing = extractedCodes
            | otherwise            = (getVal (char tree), codePreffix) : extractedCodes
            where
                extractedCodes = extractCodes (codePreffix ++ "0") (leftTree tree) ++ extractCodes (codePreffix ++ "1") (rightTree tree)
        
-----------------------------------------------------------------
--                     FUNCTIE DE SORTARE
-----------------------------------------------------------------

sort:: [a] -> (a -> a -> Bool) -> [a]
sort [] _ = []
sort [a] _ = [a]
sort l comp = intercalate (sort first_half comp) (sort second_half comp)
    where
        first_half  = slice l 0 (length l `div` 2)
        second_half = slice l (length l `div` 2) (length l)

        intercalate [] l2 = l2
        intercalate l1 [] = l1
        intercalate (x:xs) (y:ys) 
            | comp x y  = x : intercalate xs (y:ys)
            | otherwise = y : intercalate (x:xs) ys

        slice [] _ _ = []
        slice (x:xs) i j 
            | i > 0      = slice xs (i - 1) (j - 1)
            | i == j - 1 = [x]
            | i == j     = []
            | j > 0      = x : slice xs i (j - 1)










