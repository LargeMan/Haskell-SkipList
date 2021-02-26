{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


import SkipList
import System.Random
import System.IO.Unsafe -- I am sorry

{-      TESTING SUITE           -}
level_harvesting :: (Eq a, Ord a) => Node a -> Int -> [a]
level_harvesting None _ = []
level_harvesting (Node{..}) h
    | height == h = (value : level_harvesting top h)
    | otherwise = (level_harvesting bottom h) ++ (level_harvesting top h)

referenceCheck :: (Eq a, Ord a) => SkipList a -> Int -> [a] -> Bool
referenceCheck _ _ [] = True
referenceCheck EmptySL _ _ = False
referenceCheck (SkipList {..}) h xs = (tail $ level_harvesting start h) == xs



power_harvesting :: (Eq a, Ord a) => SkipList a -> [Int] -> [a] -> Int -> Bool
power_harvesting EmptySL _ _ _ = False
power_harvesting _ [] _ _ = True
power_harvesting skip (x:xs) (k:ks) h = do
    let current = insert k (-1) skip
        maxH = ceiling $ logBase 2 (fromIntegral x)
    if ((size current) /= x) || ((height $ start current) /= max h maxH) then
        False
    else 
        power_harvesting current xs ks h


test :: (Eq a, Ord a) => SkipList a -> [Int] -> [a] -> SkipList a
test EmptySL _ _ = EmptySL
test s [] _ = s
test skip (x:xs) (k:ks) = do
    let current = insert k (-1) skip
    --    maxH = ceiling $ logBase 2 (fromIntegral (x))
    --if ((size current) /= x) || ((height $ start current) /= max h maxH) then
    --    current
    --else 
    test current xs ks



{-     TEST CASES       -}
test_case_1 = referenceCheck (insert 5 1 (insert 15 1(insert 3 2 (insert 20 1 (skiplist 10 3))))) 1 [3, 5, 10, 15, 20]

test_case_2 = (referenceCheck temp 1 [3, 5, 10, 15, 20]) && ((size temp) == 5) && ((height $ start temp) == 3)
    where temp = insert 5 (-1) (insert 15 (-1) (insert 3 (-1) (insert 20 (-1) (skiplist 10 3))))

test_case_3 = do
    let seed = unsafePerformIO (randomIO) -- generate the random bool list; only gotta do this once per skip
        nums = (randoms $ mkStdGen seed) :: [Int]
    power_harvesting (insert 10 3 EmptySL) [2..1024] nums 3

test_case_4 :: Int -> Int -> Bool
test_case_4 i j
    | i >= j = False
    | otherwise = do
        let
            seed = unsafePerformIO (randomIO)
            nums = (randoms $ mkStdGen seed) :: [Int]
            meme = test (insert 10 1 EmptySL) [2..1024] nums
            rest = ((1024 `div` 2) - (length $ tail (level_harvesting (start meme) 2))) <= 10
        if rest then True
        else test_case_4 (i+1) j

test_case_5 = True

test_case_6 = do
    let case6 = test (insert 10 1 EmptySL) [2..10000] ((randoms $ mkStdGen (unsafePerformIO (randomIO))) :: [Int])
        lastone = last $ level_harvesting (start case6) 1
    (contains lastone case6) && not (contains (lastone + (value $ start case6)) case6)

test_case_7 :: Int -> Int -> Bool -> Bool -> Bool
test_case_7 i j isTall isShort
    | isTall && isShort = True
    | i >= j = False
    | otherwise = do
        let
            seed = unsafePerformIO (randomIO)
            nums = (randoms $ mkStdGen seed) :: [Int]
            four = take 4 nums
            meme = test (insert 10 1 EmptySL) [2..5] (four)
            s7 = get (last four) meme
        case s7 of
            None -> error "this is broken LOL" -- should never reach this
            otherwise -> if height s7 /= 3 then
                            test_case_7 (i+1) j True isShort
                         else
                            test_case_7 (i+1) j isTall True

-- test (insert 10 1 EmptySL) [2..5] take 4 ((randoms $ mkStdGen (unsafePerformIO (randomIO))) :: [Int])
--test_case_8 case8 = let case8_1 = delete (last case8_lst) case8 in
--    (((height $ start case8) == 3) && ((size case8) == 5) && ((height $ start case8_1) == 2))


test_case_14 = do
    let case1 = insert 30 5 (insert 25 1 (insert 25 4 (insert 20 2 (insert 15 3 (insert 10 2 (skiplist 0 5))))))
        s1 = start case1
        check1 = ((tail (level_harvesting s1 1) == [10,15,20,25,25,30])
            && (tail (level_harvesting s1 2) == [10,15,20,25,30])
            && (tail (level_harvesting s1 3) == [15,25,30])
            && (tail (level_harvesting s1 4) == [25,30])
            && (tail (level_harvesting s1 5) == [30])
            && ((height $ get 25 case1) == 1))

        case2 = delete 25 case1
        s2 = start case2
        check2 = ((tail (level_harvesting s2 1) == [10,15,20,25,30])
            && (tail (level_harvesting s2 2) == [10,15,20,25,30])
            && (tail (level_harvesting s2 3) == [15,25,30])
            && ((height s2) == 3))

    check1 && check2

test_case_15 = do
    let case1 = insert 30 5 (insert 25 4 (insert 25 1 (insert 20 2 (insert 15 3 (insert 10 2 (skiplist 0 5))))))
        s1 = start case1
        check1 = ((tail (level_harvesting s1 1) == [10,15,20,25,25,30])
            && (tail (level_harvesting s1 2) == [10,15,20,25,30])
            && (tail (level_harvesting s1 3) == [15,25,30])
            && (tail (level_harvesting s1 4) == [25,30])
            && (tail (level_harvesting s1 5) == [30])
            && ((height $ get 25 case1) == 4))

        case2 = delete 25 case1
        s2 = start case2
        check2 = ((tail (level_harvesting s2 1) == [10,15,20,25,30])
            && (tail (level_harvesting s2 2) == [10,15,20,30])
            && (tail (level_harvesting s2 3) == [15,30])
            && ((height s2) == 3))

    check1 && check2


assert_true [] = ""
assert_true ((h,x):xs) = let status = if x then "Passed" else "Failed" in
    "TestCase " ++ (show h) ++ ": " ++ status ++ "\n" ++ assert_true xs



main = do
    let s1 = referenceCheck (insert 5 1 (insert 15 1(insert 3 2 (insert 20 1 (skiplist 10 3))))) 1 [3, 5, 15, 20]
        temp = insert 5 (-1) (insert 15 (-1) (insert 3 (-1) (insert 20 (-1) (skiplist 10 3))))

        s2 = (referenceCheck temp 1 [3, 5, 15, 20]) && ((size temp) == 4) && ((height $ start temp) == 3)


        sizes = [2..1024]
        seed = unsafePerformIO (randomIO) -- generate the random bool list; only gotta do this once per skip
        nums = (randoms $ mkStdGen seed) :: [Int]

        s3 = power_harvesting (insert 10 3 EmptySL) sizes nums 3

        --meme = test (insert 10 1 EmptySL) sizes ((randoms $ mkStdGen seed) :: [Int]))

        s4 = test_case_4 0 10

        s5 = test_case_5

        case6 = test (insert 10 1 EmptySL) [2..10000] ((randoms $ mkStdGen (unsafePerformIO (randomIO))) :: [Int])
        lastone = last $ level_harvesting (start case6) 1
        s6 = (contains lastone case6) && not (contains (lastone + (value $ start case6)) case6)

        s7 = test_case_7 0 40 False False

        case8_lst = take 4 ((randoms $ mkStdGen (unsafePerformIO (randomIO))) :: [Int])
        case8 = test (insert 10 1 EmptySL) [2..5] case8_lst
        case8_1 = delete (last case8_lst) case8
        s8 = (((height $ start case8) == 3)
            && ((size case8) == 5)
            && ((height $ start case8_1) == 2))


        s9 = (height $ start (delete (1) case8)) == 3

        case10_1 = (insert 10 (-1) (insert 1 (-1) (insert 2 (-1) (skiplist 10 5))))
        case10 = delete (if (head nums) > 0 then 2 else 10) case10_1
        s10 = ((height $ start case10) == 1) && ((size case10) == 2)
        
        case11 = delete 11 case10_1
        s11 = ((height $ start case11) == 5) && ((size case11) == 3)

        case12 = insert 100000000 (-1) (skiplist (head nums) 1)
        s12 = ((height $ start case12) == 1) && ((size case12) == 1)

        case13 = delete 10 (delete 1 (delete 2 case10_1))
        s13 = ((height $ start case13) == 1) && ((size case13) == 0)


        s14 = test_case_14
        s15 = test_case_15


        cases = [s1,s2,s3,s4,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15]
        passed = foldr (&&) True cases

    putStr $ assert_true (zip [1..15] cases)
    if passed then
        print $ "it works yay"
    else do
        print $ "WHEN THE CODE IS SUS XD XD XD XD"
        --kk = insert 30 5 (insert 25 4 (insert 25 1 (insert 20 2 (insert 15 3 (insert 10 2 (skiplist 0 5))))))

 
