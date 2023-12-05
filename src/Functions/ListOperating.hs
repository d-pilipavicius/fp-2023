module Functions.ListOperating (
  sameList,
  cropDuplicates,
  hasDuplicates,
  removeElementFromList,
  removeColumnFromList,
  getLastValue,
  listSubtraction,
  changeElementN,
  Dictionary,
  mapLists,
  dictGetByKey,
  combineDict,
  removeLastElement
) where
import CustomDataTypes (ErrorMessage)
import Functions.ValueComparing (trueOrError)

sameList :: Eq a => [a] -> [a] -> Bool
sameList a b = a_to_b a b && a_to_b b a 

cropDuplicates :: Eq a => [a] -> [a]
cropDuplicates l = cropDuplicates1 l []

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = hasDuplicates1 x xs || hasDuplicates xs 

hasDuplicates1 :: Eq a => a -> [a] -> Bool
hasDuplicates1 _ [] = False
hasDuplicates1 el (x:xs) = (el == x) || hasDuplicates1 el xs 

removeElementFromList :: Integer -> [a] -> [a] -> [a]
removeElementFromList 0 f (_:xs) = f++xs
removeElementFromList _ f [] = f
removeElementFromList i f (x:xs) = removeElementFromList (i-1) (f++[x]) xs

removeColumnFromList :: Integer -> [[a]] -> [[a]] -> [[a]]
removeColumnFromList _ f [] = f
removeColumnFromList i f (x:xs) = removeColumnFromList i (f++[removeElementFromList i [] x]) xs

getLastValue :: [a] -> a
getLastValue [] = error "empty list does not have a last value"
getLastValue (x:xs) = getLastValue1 xs x

removeLastElement :: [a] -> [a]
removeLastElement l = removeElementFromList (toInteger $ (length l-1)) [] l

listSubtraction :: Eq a => [a] -> [a] -> [a]
listSubtraction a b = reverse $ listSubtraction1 a b []

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

changeElementN :: Integer -> a -> [a] -> [a]
changeElementN i el list = changeElementN1 i el list []

changeElementN1 :: Integer -> a -> [a] -> [a] -> [a]
changeElementN1 _ _ [] list = list
changeElementN1 0 el (_:xs) list = list++(el:xs)
changeElementN1 i el (x:xs) list = changeElementN1 (i-1) el xs (list++[x])

type Dictionary a b = [(a,b)]

mapLists :: Eq a => [a] -> [b] -> Dictionary a b
mapLists l1 l2 =
  if length l1 == length l2 
    then mapLists1 l1 l2 []
    else error "Unable to map lists of different length"

mapLists1 :: Eq a => [a] -> [b] -> Dictionary a b -> Dictionary a b
mapLists1 [] _ dict = dict
mapLists1 (x1:xs1) (x2:xs2) dict = mapLists1 xs1 xs2 dict++[(x1,x2)]

dictGetByKey :: Eq a => Dictionary a b -> a -> Maybe b
dictGetByKey [] _ = Nothing
dictGetByKey (x:xs) key = 
  if key == fst x
    then Just $ snd x
    else dictGetByKey xs key

combineDict :: Show a => Show b => Eq a => Dictionary a b -> Dictionary a b -> Either ErrorMessage (Dictionary a b)
combineDict dictA dictB = do
  let dictComb = dictA++dictB
  let dictKeys = map fst dictComb
  _ <- trueOrError (not $ hasDuplicates dictKeys) "Cannot combine dictionaries with same keys."
  return dictComb 