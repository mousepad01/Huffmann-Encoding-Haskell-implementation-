import Test.QuickCheck

-- coduri huffmann
-- ideea de implementare:
-- pe scurt, voi avea un heap in care fiecare nod este radacina unui arbore binar
-- initial formez heap ul cu noduri care sunt frecventele caracterelor,
-- fiecare frecventa fiind asociata unui caracter
-- la un anumit pas extrag de doua ori minimul
-- unesc cele doua noduri corespunzator, si adaug noul nod obtinut din nou in heap
-- fac asta pana raman cu un singur nod, 
-- moment in care acel nod va fi radacina arborelui huffmann corespunzator

data HuffmannTree = EmptyTree | TreeNode { freq :: Int, 
                                           char :: Maybe Char, 
                                           leftTree :: HuffmannTree, 
                                           rightTree :: HuffmannTree } deriving (Eq, Show)

-- daca subarborele curent contine un nod(nodul) cu exact 1 fiu => Partial
-- daca subarborele curent contine noduri cu 0 sau 2 fii => Full
data State = Full | Partial deriving (Eq, Show)

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

data Heap = EmptyHeap | HeapNode { hTree :: HuffmannTree,
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

insertInHeap :: Heap -> (Char, Int) -> Heap

-- cand inserez nodul intr-o pozitie nula
insertInHeap EmptyHeap (ch, fr) = HeapNode {hTree = TreeNode {freq = fr, 
                                                              char = Just ch,
                                                              leftTree = EmptyTree,
                                                              rightTree = EmptyTree},
                                            leftHeap = EmptyHeap,
                                            rightHeap = EmptyHeap,
                                            size = 1,
                                            state = Full}

-- cand tatal pozitiei de inserat are ambii fii nuli
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = EmptyHeap, 
                       rightHeap = EmptyHeap, 
                       size = 1,
                       state = Full} (ch, fr) 

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
        updatedHeap = insertInHeap EmptyHeap (ch, fr)
        thisFreq = freq huftree

-- cand tatal pozitiei de inserat are doar fiul drept nenul
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = lheap, 
                       rightHeap = EmptyHeap, 
                       size = 2,
                       state = Partial} (ch, fr) 

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
        updatedHeap = insertInHeap EmptyHeap (ch, fr)
        thisFreq = freq huftree

-- cand ambii fii ai nodului curent sunt nenuli si trebuie sa cobor
insertInHeap HeapNode {hTree = huftree,
                       leftHeap = lheap, 
                       rightHeap = rheap, 
                       size = s,
                       state = st} (ch, fr) 

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
            | whichUpdated == "left"  = insertInHeap lheap (ch, fr)
            | whichUpdated == "right" = insertInHeap rheap (ch, fr)
        
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

lastFromHeap :: Heap -> (Heap, HuffmannTree)

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

extractMin :: Heap -> (Heap, HuffmannTree)

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

heapify :: Heap -> [(Char, Int)] -> Heap
heapify = foldl insertInHeap

makeHeap :: [(Char, Int)] -> Heap
makeHeap = heapify EmptyHeap

-----------------------------------------------------------------




