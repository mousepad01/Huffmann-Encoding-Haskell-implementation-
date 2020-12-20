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

freqTable = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]

data HuffmannTree = EmptyTree | TreeNode { freq :: Int, 
                                           char :: Char, 
                                           leftTree :: HuffmannTree, 
                                           rightTree :: HuffmannTree }

-- daca subarborele curent contine un nod(nodul) cu exact 1 fiu => Partial
-- daca subarborele curent contine noduri cu 0 sau 2 fii => Full
data State = Full | Partial deriving (Eq)

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
                                   state :: State }

insertInHeap :: Heap -> (Char, Int) -> Heap

-- cand inserez nodul intr-o pozitie nula
insertInHeap EmptyHeap (ch, fr) = HeapNode {hTree = TreeNode {freq = fr, 
                                                              char = ch,
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
                                                                 size = 1 + size updatedHeap + size rheap,
                                                                 state = state lheap `combine` state updatedHeap}

    | thisFreq > newFreq && whichUpdated == "left" = updatedHeap {leftHeap = HeapNode {hTree = huftree,
                                                                                       leftHeap = leftOfUpdated,
                                                                                       rightHeap = rightOfUpdated,
                                                                                       size = 1 + size leftOfUpdated + size rightOfUpdated,
                                                                                       state = state leftOfUpdated `combine` state rightOfUpdated},
                                                                  rightHeap = rheap,
                                                                  size = 1 + 1 + size leftOfUpdated + size rightOfUpdated + size rheap,
                                                                  state = state leftOfUpdated `combine` state rightOfUpdated `combine` state rheap}

    | thisFreq > newFreq && whichUpdated == "right" = updatedHeap {leftHeap = lheap,
                                                                   rightHeap = HeapNode {hTree = huftree,
                                                                                         leftHeap = leftOfUpdated,
                                                                                         rightHeap = rightOfUpdated,
                                                                                         size = 1 + size leftOfUpdated + size rightOfUpdated,
                                                                                         state = state leftOfUpdated `combine` state rightOfUpdated},
                                                                   size = 1 + 1 + size leftOfUpdated + size rightOfUpdated + size lheap,
                                                                   state = state leftOfUpdated `combine` state rightOfUpdated `combine` state lheap}
    where
        -- aici in where lansez recursiv insertia conrespunzator, in functie de state si size
        thisFreq = freq huftree

        whichUpdated 
            | state lheap == Full && state rheap == Full && size lheap == size rheap = "left"
            | state lheap == Full && state rheap == Full && size lheap > size rheap  = "right"
            | state lheap == Full && state rheap == Partial = "right"
            | state lheap == Partial && state rheap == Full = "left"

        updatedHeap 
            | whichUpdated == "left"  = insertInHeap lheap (ch, fr)
            | whichUpdated == "right" = insertInHeap rheap (ch, fr)

        newFreq = freq (hTree updatedHeap)

        -- pentru unele cazuri ale rearanjarii
        leftOfUpdated = leftHeap updatedHeap
        rightOfUpdated = rightHeap updatedHeap


heapify :: Heap -> [(Char, Int)] -> Heap
heapify alreadyHeap [] = alreadyHeap
heapify alreadyHeap (x:xs) = heapify (insertInHeap alreadyHeap x) xs




{-
data Dt = Dta {a :: Int, b :: Int} deriving (Eq, Show)

instance Arbitrary Dt where
    arbitrary = Dta <$> arbitrary <*> arbitrary

f (Dta 0 _) = 0
f (Dta _ 1) = (-1)
f (Dta x y) = x + y

f' (Dta {a = 0}) = 0
f' (Dta {b = 1}) = (-1)
f' (Dta {a = x, b = y}) = x + y

tes d = f d == f' d
-}



