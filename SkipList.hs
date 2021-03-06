{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module SkipList (insert, delete, get, contains, skiplist, Node(..), SkipList(..)) where

import Data.List (intercalate) 
import System.Random (randomIO, randoms, next, mkStdGen, StdGen)

data Node a = Node {
    height :: Int,
    value :: a,
    top :: Node a,
    bottom :: Node a
} | None deriving (Show)

-- might be useful later
instance Eq (Node a) where
    None == None = True
    _ == _ = False

--    The head Node will always be some arbitrary value,
--   and will not affect ordering at all


data SkipList a = SkipList {
    start :: Node a,
    size :: Int,
    gen :: StdGen
}

-- TODO: make a full visualization of a node tree
--instance (Show a) => Show (Node a) where
--    show None = "None"
--    show (Node{..}) = do
--        let x = "|\nV"

-- prints a skiplist's height and a list of nodes of each level
instance (Show a, Ord a, Eq a) => Show (SkipList a) where
    show (SkipList{..}) = do
        let entirety = harvests start (height start)
            display = (map show entirety)
        "SKIPLIST {\n\tsize = " ++ (show size) ++ "\n\t" ++ (intercalate "\n\t" display) ++ "\n}"

-- a list builder for level_harvest
-- returns each height + value of nodes of that height
harvests :: (Eq a, Ord a) => Node a -> Int -> [(Int, [a])]
harvests None _ = []
harvests n h
    | h == 0 = []
    | otherwise = let x = map value (level_harvest n h) in
        (h, x):(harvests n (h-1))

-- gets all nodes at a certain height
level_harvest :: (Eq a, Ord a) => Node a -> Int -> [Node a]
level_harvest None _ = []
level_harvest n@(Node{..}) h
    | height == h = n : level_harvest top h
    | otherwise = (level_harvest bottom h) ++ (level_harvest top h)

go_down n@(Node {..}) h = if height == h then n else go_down bottom h





-- gets logHeight at a certain size
getMaxHeight :: Int -> Int
getMaxHeight n
    | n <= 2 = 1
    | otherwise = max 1 (ceiling $ logBase 2 (fromIntegral (n)))

-- Will insert based on ordering. Duplicates always get put in front of existing values
insert :: (Eq a, Ord a) => a -> Int -> SkipList a -> SkipList a
-- inserts by partitioning the part of the skiplist that is to go behind
-- the node of the newly inserted value, connecting it to the new node,
-- then reconnecting it to the front part
insert val h skip@(SkipList{start = old_start, size = sz, gen = generator})
    | h < (-1) || h == 0 || h > maxH = error "invalid height"
    | otherwise = do
        let logH        = getMaxHeight (sz + 1)
            new_max     = max maxH logH
            -- h_len       = length $ level_harvest old_start maxH
            new_skip    = height_check maxH logH skip
            generated   = (randoms $ gen new_skip) :: [Bool]
            rand_h      = length $ takeWhile (== True) (True:generated)
            new_h       = case h of (-1)      -> min new_max (max 1 rand_h) 
                                    otherwise -> h
            new_node    = create_chain new_h val
            end_part    = build_insert new_node (search_key (>=) val (start new_skip))
            final_node  = rebuild (height end_part) end_part val (start new_skip)

        SkipList {start = final_node, size = sz + 1, gen = gen new_skip}
        where maxH = height old_start

-- checks if the skiplist is in need of a height expansion
height_check :: Int -> Int -> SkipList a -> SkipList a
height_check x y (SkipList{..})
    | x >= y =  SkipList {start, size, gen = gens}
    | otherwise = SkipList {start = expandTops start (True:generated), size, gen = gens}
        where gens = (snd $ next gen)
              generated = (randoms $ gen) :: [Bool]

-- expands the top of the highest nodes, whilst disconnecting the previous height connection
expandTops :: Node a -> [Bool] -> Node a
expandTops None _ = None
expandTops (Node{top = t, bottom = b, height = h, value = v}) (x:xs)
    | x =
        Node { 
            top     = expandTops t (xs),
            bottom  = Node {top = disconnect_bottom t xs, bottom=b, value=v, height=h},
            value   = v,
            height  = h + 1
        }     
    | otherwise = expandTops t (xs)

-- disconnects connections to newly expanded nodes, in order to prevent sprawling chains
disconnect_bottom :: Node a -> [Bool] -> Node a
disconnect_bottom None _ = None
disconnect_bottom (Node{..}) (x:xs)
    | not x = Node {top = disconnect_bottom top xs, bottom, value, height}
    | otherwise = None

-- gathers a list of nodes based on a provided criteria
search_key :: (Eq a, Ord a) => (a -> a -> Bool) -> a -> Node a -> [Node a]
search_key key val None = []
search_key key val (Node{top = None, ..}) = search_key key val bottom
search_key key val (Node{value = cur_val, ..})
    | (value top) `key` val = top : search_key key val bottom
    | otherwise             = search_key key val top

-- create a "single node" of height h
create_chain :: Int -> a -> Node a
create_chain 1 val = Node {top = None, bottom = None, height = 1, value = val}
create_chain h val = Node {top = None, bottom = create_chain (h-1) val, height = h, value = val}

-- connects the end partition of a skiplist to the new value's node
build_insert :: Node a -> [Node a] -> Node a
build_insert new_node [] = new_node
build_insert new_node@(Node{height = h, ..}) (x:xs)
    | h > height x = Node {top, bottom = build_insert bottom (x:xs), height = h, value}
    | h < height x = build_insert new_node xs
    | otherwise    = Node {top = x, bottom = build_insert bottom xs, height = h, value}

-- reconnects the first partition and the modified end partition
rebuild :: (Eq a, Ord a) => Int -> Node a -> a -> Node a -> Node a
rebuild h new val None = None

rebuild h new val (Node{top = None,..})
    | h == height   = Node {top = new, bottom = rebuild (h+1) None val bottom, height, value}
    | otherwise     = Node {top = None, bottom = rebuild h new val bottom, height, value}

rebuild h new val (Node{value = cur_val, ..})
    | (value top) < val = Node {top = (rebuild h new val top), bottom, height, value = cur_val}
    | h == height       = Node {top = new, bottom = (rebuild (h+1) None val bottom), height, value = cur_val}
    | h < height        = Node {top, bottom = (rebuild h new val bottom), height, value = cur_val}
    | otherwise         = Node {top = None, bottom = (rebuild h new val bottom), height, value = cur_val}








delete :: (Eq a, Ord a) => a -> SkipList a -> SkipList a
delete _ skip@(SkipList{size=0,..}) = skip
delete val skip@(SkipList{..}) = do
    -- search the skiplist first to 1). see if its there and 2). gather some duplicates
    let src_nodes = search_specific val start
        check_len = length src_nodes
    if check_len == 0 || size == 0 then skip
    else do
        let n = last src_nodes
            after_n = get_ends n -- this will get us everything we need
            new_node = first_part (height n) after_n val start
            -- time to do log height check
            logH = getMaxHeight (size - 1)
            maxH = height new_node
        if logH < maxH then do
            let levels = level_harvest new_node logH
                final_node = delete_til levels
            SkipList {start = final_node, size = size - 1, gen}
        else
            SkipList {start = new_node, size = size - 1, gen}


-- just get every successor node
get_ends :: Node a -> [Node a]
get_ends None = []
get_ends (Node{..}) = top : get_ends bottom


search_specific :: (Eq a, Ord a) => a -> Node a -> [Node a]
search_specific val None = []
search_specific val (Node{value = cur_val, ..})
    | top == None || val_chk = search_specific val bottom
    | (value top) == val     = (search_specific val top) ++ (top:(search_specific val bottom))
    | otherwise              = search_specific val top
        where val_chk = (value top) > val

-- we must guarantee that the list is never empty
-- this is horrific and i dont want to try explaining this
first_part :: (Eq a, Ord a) => Int -> [Node a] -> a -> Node a -> Node a
first_part _ _ _ None = None

first_part h (x:xs) val Node{top = None,..}
    | height < h = Node {top = x, bottom = first_part h (xs) val bottom, height, value}
    | otherwise = Node {top = None, bottom = first_part h (x:xs) val bottom, height, value}

first_part h (x:xs) val (Node{top=t,value = cur_val,..})
    | (value t) > val = Node {top=t, bottom = first_part h (x:xs) val bottom, height, value=cur_val}
    | (value t) < val = Node {top = first_part h (x:xs) val t, bottom, height, value=cur_val}
    | (value t) == val = do
        if height == h then
            Node {top = x, bottom = first_part h (xs) val bottom, height, value=cur_val}
        else
            Node {top=t, bottom = first_part h (x:xs) val bottom, height, value=cur_val}

delete_til :: [Node a] -> Node a
delete_til ([]) = None
delete_til ((Node{..}):xs) = Node {top = delete_til xs, bottom, value, height}

-- suite

get :: (Eq a, Ord a) => a -> SkipList a -> Node a
get val (SkipList{..}) = if length x == 0 then None else last x where x = search_specific val start

contains :: (Eq a, Ord a) => a -> SkipList a -> Bool
contains val sl = (get val sl) /= None


skiplist :: (Eq a, Ord a) => a -> Int -> IO (SkipList a)
skiplist val h
    | h < 1 = error "invalid height"
    | otherwise = do
        seed <- randomIO
        return $ SkipList {
            start = create_chain h val,
            size = 0,
            gen = mkStdGen seed
        }