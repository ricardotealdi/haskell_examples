size_list [] = 0
size_list (x:xs) = 1 + size_list xs

invert :: [t] -> [t]
invert [] = []
invert (x:xs) = (invert xs) ++ [x]

includes :: [Int] -> Int -> Bool
includes [] _ = False
includes (x:xs) n | (x == n) = True
                  | otherwise = includes xs n

biggest :: [Int] -> Int
biggest [] = error "empty list"
biggest [x] = x
biggest (x:xs) | (x > biggest xs) = x
              | otherwise = biggest xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) | ((mod x 2) == 0) = [x] ++ evens xs
             | otherwise = evens xs

lowest :: [Int] -> Int
lowest [] = error "empty list"
lowest [x] = x
lowest (x:xs) | (x < (lowest xs)) = x
              | otherwise = lowest xs

remove_lowest :: [Int] -> [Int]
remove_lowest [] = []
remove_lowest [x] = []
remove_lowest (x:xs) | (x < lowest xs) = xs
                     | otherwise = x:remove_lowest xs

remove_biggest :: [Int] -> [Int]
remove_biggest [] = []
remove_biggest [x] = []
remove_biggest (x:xs) | (x > biggest xs) = xs
                     | otherwise = x:remove_biggest xs

sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort x = (lowest x):sort (remove_lowest x)

sort_descending :: [Int] -> [Int]
sort_descending [] = []
sort_descending [x] = [x]
sort_descending x = (biggest x):sort_descending (remove_biggest (x))
