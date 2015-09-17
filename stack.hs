push_to_stack :: [Int] -> Int -> [Int]
push_to_stack stack x = stack ++ [x]

top_element_from_stack :: [Int] -> Int
top_element_from_stack [] = error "empty stack"
top_element_from_stack [x] = x
top_element_from_stack (x:xs) = top_element_from_stack xs

pop_from_stack :: [Int] -> [Int]
pop_from_stack [] = error "empty stack"
pop_from_stack (x:xs) | (x == (top_element_from_stack (x:xs))) = xs
                      | otherwise = x:(pop_from_stack xs)

is_stack_empty :: [Int] -> Bool
is_stack_empty [] = True
is_stack_empty _ = False
