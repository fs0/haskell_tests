ins :: a -> [a] -> [[a]]
ins y [] = [[y]]
ins y (x:xs) = (y:x:xs) : (map (x:) (ins y xs))

remdup :: Eq a => [a] -> [a]
remdup [x] = [x]
remdup (x:xs) | elem x $ remdup xs = remdup xs 
              | otherwise          = x:(remdup xs)

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = remdup $ concat $ map (ins x) $ perms xs
