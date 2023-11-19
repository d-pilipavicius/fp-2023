module Functions.ListOperating (
  sameList,
  cropDuplicates,
  removeElementFromList,
  removeColumnFromList,
  getLastValue,
  listSubtraction  
) where

sameList :: Eq a => [a] -> [a] -> Bool
sameList a b = a_to_b a b && a_to_b b a 

cropDuplicates :: Eq a => [a] -> [a]
cropDuplicates l = cropDuplicates1 l []

removeElementFromList :: Integer -> [a] -> [a] -> [a]
removeElementFromList 0 f (x:xs) = f++xs
removeElementFromList i f [] = f
removeElementFromList i f (x:xs) = removeElementFromList (i-1) (f++[x]) xs

removeColumnFromList :: Integer -> [[a]] -> [[a]] -> [[a]]
removeColumnFromList i f (x:xs) = removeColumnFromList i (f++[removeElementFromList i [] x]) xs
removeColumnFromList i f [] = f

getLastValue :: [a] -> a
getLastValue [] = error "empty list does not have a last value"
getLastValue (x:xs) = getLastValue1 xs x

listSubtraction :: Eq a => [a] -> [a] -> [a]
listSubtraction a b = listSubtraction1 a b []

listSubtraction1 :: Eq a => [a] -> [a] -> [a] -> [a]
listSubtraction1 [] _ tList = tList
listSubtraction1 (x:xs) l tList = if elem x l 
  then listSubtraction1 xs l tList
  else listSubtraction1 xs l tList++[x]

cropDuplicates1 :: Eq a => [a] -> [a] -> [a]
cropDuplicates1 [] l = l
cropDuplicates1 (x:xs) l = if elem x l 
  then cropDuplicates1 xs l
  else cropDuplicates1 xs l++[x] 

a_to_b :: Eq a => [a] -> [a] -> Bool
a_to_b [] b = True
a_to_b (x:xs) b = if elem x b 
  then a_to_b xs b 
  else False

getLastValue1 :: [a] -> a -> a
getLastValue1 [] e = e
getLastValue1 (x:xs) _ = getLastValue1 xs x 