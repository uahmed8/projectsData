

module MinBoundBox
(
    MBB,
    areaMBB,
    containsMBB,
    intersectMBB,
    unionMBB,
    unionLstMBBs,
 --   rangeSearch,
 --   searchK,
 --   searchN,
    insertrt
) where

import           Control.Concurrent     (forkIO)
import           Network
import           System.IO              (hClose)


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.List 
import Control.Monad
import Data.Char
--import System.IO  
import System.Directory  


{-========== Data Strucuture part ==============================-}

data MBB = MBB Double Double Double Double 
          deriving (Show, Ord, Eq)

areaMBB :: MBB -> Double
areaMBB (MBB c1 c2 c3 c4) = abs (c2 - c1) * abs (c4 - c3)

containsMBB :: MBB -> MBB -> Bool
containsMBB (MBB box1x1 box1x2 box1y1 box1y2) (MBB box2x1 box2x2 box2y1 box2y2) = box1x1 <= box2x1 && box1x2 >= box2x2 && box1y1 <= box2y1 && box1y2 >= box2y2

intersectMBB :: MBB -> MBB -> Bool
intersectMBB ( MBB box1x1 box1x2 box1y1 box1y2 ) (MBB box2x1 box2x2 box2y1 box2y2)
    | (max box1x1 box2x1) <= (min box1x2 box2x2) && (max box1y1 box2y1) <= (min box1y2 box2y2) = True
    | otherwise = False

unionMBB :: MBB -> MBB -> MBB
unionMBB (MBB box1x1 box1x2 box1y1 box1y2) (MBB box2x1 box2x2 box2y1 box2y2) = (MBB (min box1x1 box2x1) (max box1x2 box2x2) (min box1y1 box2y1) (max box1y2 box2y2))

unionLstMBBs :: MBB -> [MBB] -> MBB
unionLstMBBs l1 [] = l1
unionLstMBBs l1 [x] = l1 `unionMBB` x 
unionLstMBBs l1 (x:xs) = (l1 `unionMBB` x) `unionMBB` (unionLstMBBs l1 xs) 

minimumDistance :: MBB -> MBB -> Double 
minimumDistance (MBB box1x1 box1y1 box1x2 box1y2) (MBB box2x1 box2y1 box2x2 box2y2) = 
  minimum [sqrt' ((box2x1 - box1x1)^2 +(box2y1 - box1y1)^2), sqrt' ((box2x2 - box1x1)^2 + (box2y1 - box1y1)^2 ), sqrt' ((box2x1 - box1x2)^2 + (box2y1 - box1y2)^2) ,sqrt' ((box2x2 - box1x2)^2 + (box2y2 - box1y2)^2)]

sqrt' :: Double -> Double
sqrt' n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

data RTree = Leaf (Maybe (MBB, String)) (Maybe (MBB, String)) (Maybe (MBB, String)) |
            Node (Maybe (MBB, RTree)) (Maybe (MBB, RTree)) (Maybe (MBB, RTree))
            deriving (Show, Eq)

emptyNode = Node Nothing Nothing Nothing
emptyLeaf = Leaf Nothing Nothing Nothing

mayfilter :: Maybe a -> a
mayfilter (Just a) = a
mayfilter Nothing = error "error"

insertrt :: (MBB, String) -> RTree -> RTree
insertrt (rect, name) t@(Leaf v1 v2 v3) = 
	if not ((v1 /= Nothing) && (v2 /= Nothing) && (v3 /= Nothing)) then addToLeaf rect name t else splitNodeLeaf (rect, name) t 

insertrt (rect, name) t@(Node v1 v2 v3)   

    | v1 /= Nothing && (fst $ mayfilter v1) `containsMBB` rect = if nodeSize ( (snd $ mayfilter v1)) < nodeSize (snd $ mayfilter v1) then adjustTree t (rninsert (snd $ mayfilter v1)) v1 else Node ( Just ( fst (mayfilter v1), rninsert (snd $ mayfilter v1))) v2 v3
    | v2 /= Nothing && (fst $ mayfilter v2) `containsMBB` rect = if nodeSize (rninsert (snd $ mayfilter v2)) < nodeSize (snd $ mayfilter v2) then adjustTree t (rninsert (snd $ mayfilter v2)) v2 else Node v1 ( Just( fst (mayfilter v2), rninsert (snd $ mayfilter v2))) v3
    | v3 /= Nothing && (fst $ mayfilter v3) `containsMBB` rect = if nodeSize (rninsert (snd $ mayfilter v3)) < nodeSize (snd $ mayfilter v3) then adjustTree t (rninsert (snd $ mayfilter v3)) v3 else Node v1 v2 ( Just ( fst (mayfilter v3), rninsert (snd $ mayfilter v3)))

    | findmin (map (funcaddMBB rect) [v1,v2,v3]) == 0 = if nodeSize (rninsert (snd (mayfilter v1) ) ) < nodeSize (snd (mayfilter v1) ) then adjustTree t (rninsert (snd (mayfilter v1) ) ) v1
                      else Node (  Just ( rect `unionMBB` (fst (mayfilter v1) ), rninsert (snd (mayfilter v1)) )   ) v2 v3          
    | findmin (map (funcaddMBB rect) [v1,v2,v3]) == 1 = if nodeSize (rninsert (snd (mayfilter v2) ) ) < nodeSize (snd (mayfilter v2) ) then adjustTree t (rninsert (snd (mayfilter v2) ) ) v2
                      else Node v1 (   Just ( rect `unionMBB` (fst (mayfilter v2)), rninsert (snd (mayfilter v2)) )   ) v3          
    | findmin (map (funcaddMBB rect) [v1,v2,v3]) == 2 = if nodeSize (rninsert (snd (mayfilter v3) ) ) < nodeSize (snd (mayfilter v3) ) then adjustTree t (rninsert (snd (mayfilter v3) ) ) v3
                      else Node v1 v2 (   Just ( rect `unionMBB` (fst (mayfilter v3) ), rninsert (snd (mayfilter v3))))   

    where rninsert = insertrt (rect,name) 


adjustTree :: RTree -> RTree -> Maybe (MBB, RTree) -> RTree
adjustTree n1@(Node n11 n12 n13) n2@(Node n21 n22 n23) replacingNode@(Just (replace_rect, replace_RTree))
    | not (n11 /= Nothing && n12 /= Nothing && n13 /= Nothing) && n11 == replacingNode && n12 == Nothing && n13 == Nothing = Node n21 n22 n23
    | not (n11 /= Nothing && n12 /= Nothing && n13 /= Nothing) && n11 == replacingNode && n12 /= Nothing && n13 == Nothing = Node n21 n12 n22
    | not (n11 /= Nothing && n12 /= Nothing && n13 /= Nothing) && n12 == replacingNode && n11 /= Nothing && n13 == Nothing = Node n11 n21 n22    


funcaddMBB :: MBB -> Maybe (MBB, RTree) -> Double
funcaddMBB _ Nothing = -1
funcaddMBB querryMBB (Just (rect, _)) =  areaMBB ( unionMBB rect querryMBB ) - areaMBB querryMBB


findmin dlst = mayfilter (minEle `elemIndex` dlst)
    where minEle = minimum [x | x <- dlst, x >= 0]


nodeSize :: RTree -> Int
--nodeSize emptyLeaf = 0
--nodeSize emptyNode = 0
nodeSize (Node v1 v2 v3) = size [v1,v2,v3]
nodeSize (Leaf v1 v2 v3) = size [v1,v2,v3]

size :: (Eq a) => [Maybe a] -> Int
size [] = 0
size (x:xs) = if x == Nothing then size xs else 1 + size xs

addToLeaf :: MBB -> String -> RTree -> RTree
addToLeaf rect name (Leaf e1 e2 e3)
    | e1 == Nothing = Leaf (Just (rect, name)) e2 e3
    | e2 == Nothing = Leaf e1 (Just (rect, name)) e3
    | e3 == Nothing = Leaf e1 e2 (Just (rect,name))
    | otherwise = error "error" 

splitNodeLeaf :: (MBB,String) -> RTree -> RTree          
splitNodeLeaf (rect,name) t@(Leaf e1 e2 e3)  = Node (Just (mbb1, leaf1)) (Just (mbb2, leaf2)) Nothing
    where mbb1 = (head (makingListofMBBs t (rect,name))) `unionLstMBBs` l1_MBBs
          mbb2 = (last (makingListofMBBs t (rect,name))) `unionLstMBBs` l2_MBBs
          leaf1 = makeLeafLst emptyLeaf (matchMBBs ([head (makingListofMBBs t (rect,name))] ++ l1_MBBs) [mayfilter e1, mayfilter e2, mayfilter e3, (rect,name)])
          leaf2 = makeLeafLst emptyLeaf (matchMBBs ([last (makingListofMBBs t (rect,name))] ++ l2_MBBs) [mayfilter e1, mayfilter e2, mayfilter e3, (rect,name)])
          l1_MBBs = seperateMBBs (head (makingListofMBBs t (rect,name))) (matchingMBB t (rect,name))
          l2_MBBs = seperateMBBs (last (makingListofMBBs t (rect,name))) (matchingMBB t (rect,name))

matchingMBB :: RTree -> (MBB, String) -> [(MBB, MBB)]
matchingMBB t@(Leaf e1 e2 e3) (rect,name) = preferedMBBmatching ([head (makingListofMBBs t (rect,name)), last (makingListofMBBs t (rect,name))]) (getMBBfromlist [e1,e2,e3, Just (rect,name)])

makingListofMBBs :: RTree -> (MBB, String) -> [MBB]
makingListofMBBs t@(Leaf e1 e2 e3) (rect,name) = farMBBs (getMBBfromlist [e1,e2,e3, Just (rect,name)])    

makeLeafLst :: RTree -> [(MBB,String)] -> RTree
makeLeafLst t@(Leaf e1 e2 e3) [x] = addToLeaf (fst x) (snd x) t
makeLeafLst t@(Leaf e1 e2 e3) (x:xs) = addToLeaf fstEle sndEle (makeLeafLst t xs)
    where fstEle = fst x
          sndEle = snd x

getMBBfromlist :: (Eq a) => [Maybe (MBB,a)] -> [MBB]  
getMBBfromlist [] = []
getMBBfromlist (x:xs)
    | x == Nothing = getMBBfromlist xs
    | otherwise = [fst (mayfilter x)] ++ getMBBfromlist xs 

matchMBBs :: [MBB] -> [(MBB, a)] -> [(MBB, a)]
matchMBBs _ [] = []
matchMBBs [x] y = matchMBBs' x y
matchMBBs (x:xs) y = (matchMBBs' x y) ++ matchMBBs xs y 

matchMBBs' _ [] = []
matchMBBs' x ((y1, y2):ys)
    | x == y1 = [(y1,y2)]
    | otherwise = matchMBBs' x ys

seperateMBBs :: MBB -> [(MBB,MBB)] -> [MBB] 
seperateMBBs _ [] = []
seperateMBBs l1 (x:xs)
    | l1 == fstEle = [sndEle] ++ seperateMBBs l1 xs
    | otherwise = seperateMBBs l1 xs
    where fstEle = fst x
          sndEle = snd x
            
preferedMBBmatching :: [MBB] -> [MBB] -> [(MBB, MBB)]
preferedMBBmatching [] _ = error "error"
preferedMBBmatching _ [] = []
preferedMBBmatching [a,b] (x:xs)
    | a == x || b == x = preferedMBBmatching [a,b] xs
    | (areaMBB (x `unionMBB` a)) > (areaMBB (x `unionMBB` b)) = [(b,x)] ++ preferedMBBmatching [a,b] xs  
    | otherwise = [(a,x)] ++ preferedMBBmatching [a,b] xs

smallestDistance :: [[MBB]] -> [Double]
smallestDistance [] = []
smallestDistance [[x,y]] = [minimumDistance x y]
smallestDistance ([x,y]:xs) = minimumDistance x y:smallestDistance xs

farMBBs :: [MBB] -> [MBB]
farMBBs listMBBs = (pairMBBs listMBBs) !! furthestIndex
    where expectedDistances = smallestDistance (pairMBBs listMBBs)
          furthestDistance = maximum expectedDistances
          furthestIndex = mayfilter (furthestDistance `elemIndex` expectedDistances)

pairMBBs :: [MBB] -> [[MBB]] 
pairMBBs  listMBBs = [x | x <- subsequences listMBBs, length x == 2]

getLeaves :: RTree -> [(MBB, String)]
getLeaves t@(Leaf c1 c2 c3) = getLeafKey c1 ++ getLeafKey c2 ++ getLeafKey c3
getLeaves t@(Node c1 c2 c3) = traverseNode c1 ++ traverseNode c2 ++ traverseNode c3


getLeafKey :: Maybe (MBB, String) -> [(MBB, String)]
getLeafKey Nothing = []
getLeafKey ele = [(fst $ mayfilter ele, snd $ mayfilter ele)]

traverseNode :: Maybe (MBB, RTree) -> [(MBB,String)]
traverseNode Nothing = []
traverseNode ele = getLeaves $ snd $ mayfilter ele

reinsertall :: [(MBB,String)] -> RTree -> RTree  
reinsertall [x] t = insertrt x t
reinsertall (x:xs) t = reinsertall xs (insertrt x t) 

del :: String -> RTree -> RTree
del rectname t = reinsertall newLst emptyLeaf
    where elemLst = getLeaves t
          newLst = [ x | x <- elemLst, (snd x) /= rectname]

{-=========================================================================PROJECT Part=============================-}

{-==========================GUI Part==================================-}
toDiv :: String -> UI Element
toDiv str = UI.div #+ [ string str ]


main :: IO ()
main = do
    (uAccept, hAccept) <- newEvent
    startGUI defaultConfig $ \win -> do

        bAccept <- stepper "" uAccept
        entree1 <- UI.entry bAccept
        element entree1 # set (attr "size") "5" # set style [("width","100px")]
        getBody win #+  [string " MBB val x1: "] #+ [element entree1] 

        cAccept <- stepper "" uAccept
        entree2 <- UI.entry cAccept
        element entree2 # set (attr "size") "5" # set style [("width","100px")]
        getBody win #+ [string " MBB val y1: "] #+ [element entree2] 

        dAccept <- stepper "" uAccept
        entree3 <- UI.entry dAccept
        element entree3 # set (attr "size") "5" # set style [("width","100px")]
        getBody win #+ [string " MBB val x2: "] #+ [element entree3] 

        eAccept <- stepper "" uAccept
        entree4 <- UI.entry eAccept
        element entree4 # set (attr "size") "5" # set style [("width","100px")]
        getBody win  #+ [string " MBB val y2: "] #+ [element entree4]

        fAccept <- stepper "" uAccept
        entree5 <- UI.entry fAccept
        element entree5 # set (attr "size") "5" # set style [("width","150px")]
        getBody win #+ [string " MBB name "] #+ [element entree5] 

        button <- UI.button #+ [ string "   Generate! " ]
        getBody win #+ [ return button ]
        on UI.click button $ \_ ->
        	getBody win #+ [ toDiv " You clicked the generate button ! " ]



{-===================Console Based part========================-}

rt2 = Leaf (Just (MBB 1.0 1.0 1.0 1.0,"init")) Nothing Nothing

console = do
    putStrLn "Welcome to RTreeVisualization"
    putStrLn "Type -1 if this is your last MBB to insert then want to generate tree as output in latex form else keep continue insertion with typing first 0: " 
    initForm <- getLine

    putStrLn "Type True if this is your first MBB to insert else type False: "  
    inmbr <- getLine
    let nmbr = read inmbr :: Bool
    putStrLn "Please enter MBB x1 val: "
    input1 <- getLine
    let u = read input1 :: Double
    putStrLn "Please enter MBB y1 val: "
    input2 <- getLine
    let v = read input2 :: Double
    putStrLn "Please enter MBB x2 val: "
    input3 <- getLine
    let w = read input3 :: Double
    putStrLn "Please enter MBB y2 val: "
    input4 <- getLine
    let x = read input4 :: Double
    putStrLn "Please enter MBB name: "
    input5 <- getLine
    let y = read input5 :: String

    let rt1 = if (nmbr) then (insertrt ((MBB u v w x),y) emptyLeaf) else rt2

    let rt2 = if (nmbr==False) then (insertrt ((MBB u v w x),y) rt1) else rt1

    when (read initForm == -1) $
        writeFile "E:\\RTree.tex" (first++outputTex' (rt2)++end)

    when (read initForm == 0) $
        console
        

{-========================= Latex file part ======================================================-}

 --Manual function for wrting into latex first getting resultant Tree.}
funcOutput arg = writeFile "E:\\RTree.tex" (first++outputTex' (arg)++end)


first :: String
first = "\\documentclass{article}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes}\n\\begin{document}\n\\begin{center}\n\\begin{tikzpicture}\n\\tikzstyle{bplus}=[rectangle split, rectangle split horizontal,rectangle split ignore empty parts,draw]\n\\tikzstyle{every node}=[bplus]\n\\tikzstyle{level 1}=[sibling distance=60mm]\n\\tikzstyle{level 2}=[sibling distance=15mm]"


end :: String
end = ";\\end{tikzpicture}\n\\end{center}\n\\end{document}\n"



outputTex' :: RTree -> String
outputTex' t@(Leaf v1 v2 v3)
	| v1 /= Nothing && v2 /= Nothing && v3 /= Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++"{"++(show (snd (mayfilter v2)))++"}"++"{"++(show (snd (mayfilter v3)))++"}}\n" 
	| v1 /= Nothing && v2 /=Nothing && v3 == Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++"{"++(show (snd (mayfilter v2)))++"}"++"{}}\n" 
	| v1 /= Nothing && v2 ==Nothing && v3 == Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++"{}"++"{}}\n" 
	| otherwise = "Tree is empty"
--outputTex' t@(Node v1 v2 v3)
--	| v1 /= Nothing && v2 /= Nothing && v3 /= Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++leaves v1++"{"++(show (snd (mayfilter v2)))++"}"++leaves v2++"{"++(show (snd (mayfilter v3)))++leaves v3" 
--	| v1 /= Nothing && v2 /=Nothing && v3 == Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++"{"++(show (snd (mayfilter v2)))++"}"++"{}}\n" 
--	| v1 /= Nothing && v2 ==Nothing && v3 == Nothing = "\n \\node {\\threenode{"++(show (snd (mayfilter v1)))++"}"++"{}"++"{}}\n" 
--	| otherwise = "Tree is empty"

{-
leaves :: RTree -> String

leaves emptyNode = "\\node  {}\n"
leaves rt@(Leaf v1 v2 v3)
	| (hasChildren v3) && (hasChildren v2) = "child { node [arn_n] {"++(show val)++"} "++leaves v2 ++ " " ++ leaves v3 ++ "}\n"
	| hasChildren v2 = "child { node {"++(show val)++"}" ++ leaves v2 ++ writeTex v3 ++ "}\n"
	| hasChildren v3 = "child { node  {"++(show val)++"}" ++ writeTex v2 ++ leaves v3 ++ "}\n"
	| (hasRight' rt) && (hasLeft' rt) = "child { node [arn_n] {"++(show v1)++"}" ++ writeTex v2 ++ writeTex v3 ++ "}\n"
	| (hasLeft' rt) = "child { node {"++(show v1)++"}" ++ writeTex v2 ++ " child { node  {}} " ++ "}\n"
	| (hasRight' rt) = "child { node {"++(show v1)++"}" ++ " child { node  {}} " ++ writeTex v3 ++ "}\n"
	| otherwise =  "child {node  {"++(show v1)++"}}\n" 


hasLeft' emptyNode = False
hasLeft' rt@(Node v1 v2 v3) = lt /= emptyNode && (root v2 /= -100)

hasRight' emptyNode = False
hasRight' bt@(Node v1 v2 v3) = rt /= emptyNode && (root v3 /= -100)

hasChildren emptyNode = False
hasChildren bt@(Node v1 v2 v3) = (((hasRight' v3) || (hasLeft' v3)) || ((hasRight' v2) || (hasLeft' v3)))


writeTex :: RTree -> String
--helper function
writeTex emptyNode = "child {node  {}}"
writeTex rt@(Node v1 v2 v3) = leaves rt

-}
