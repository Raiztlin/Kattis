-- Problem: https://open.kattis.com/problems/parking2


-- Solution Summary
-- Michale will need to walk the distance of the entire line and back. So the solution is given by calculating the length of the line and multiplying the result by two,
-- Expressed mathematicaly this would be

-- (max - min) * 2.

-- where max and min is the maximal and minimal values of the line.


import Data.List

main = interact (unlines . calculateAll . readInput) 

-- Takes the input string, stuffs everything into nested lists and cuts away the head. 
readInput :: String -> [[Int]]
readInput = (map (map read)) . tail . (map words) . lines

--Calculates the length of the line multiplied by two.
walkingDistance :: [Int] -> Int
walkingDistance (singleShop:[]) = 0
walkingDistance shops = ((maximum shops) - (minimum shops)) * 2

-- Calls walkingDistance on every second element of the list. Returns a new list with the calculated results.
calculateAll :: [[Int]] -> [String]
calculateAll (oddIndx:evenIndx:restOf) = (show $ walkingDistance evenIndx) : (calculateAll restOf)
calculateAll _ = []
