module Lib
    ( someFunc
    ) where
import Data.Ix()
import GHC.Arr
import Data.Maybe (isNothing)

someFunc :: IO ()
someFunc = print a1

class Hash a where
    hash :: a -> Int

instance Hash Int where
    hash a = a

instance (Show key, Show value) => Show (OADict key value) where
    show oad = "Max size:\t"  ++ show (maxSize oad) ++ "\nCurrent size:\t" ++ show (currentSize oad) ++ "\nLoad factor:\t" ++ show (loadFactor oad) ++ "\ndata :::\n" ++ show (arrayData oad)

data Cond a = a :? a
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

data OADict key value = OADictCons
    {
        currentSize :: Int,
        maxSize :: Int,
        loadFactor :: Float,
        arrayData :: Array Int (Maybe key, Maybe value)
    };

emptyDict :: OADict key value
emptyDict =
    OADictCons {maxSize = 8, currentSize = 0, loadFactor = 0.75, arrayData = array (0, 7) [(i, (Nothing, Nothing)) | i <- [0..7]]}

a1 :: OADict Int Char
a1 = emptyDict

findFreePlace :: Eq k => k -> Int -> Array Int (Maybe k, a) -> Int
findFreePlace key firstPosition arr =
    case arr ! rem firstPosition (length arr) of
        (Nothing, _) -> rem firstPosition (length arr)
        (Just k, _) ->
            if k == key then firstPosition else findFreePlace key (rem (firstPosition + 1) (length arr)) arr


getDataAsList :: OADict a b -> [(a, b)]
getDataAsList oad =
    removeMaybe (elems (arrayData oad)) where
        removeMaybe [] = []
        removeMaybe (x:xs) =
            case x of
                (Nothing, _) -> removeMaybe xs
                (Just _, Nothing) -> removeMaybe xs
                (Just a, Just b) -> (a, b) : removeMaybe xs
-- arr :: Array Int (Maybe Int, Maybe Char)
-- arr = listArray (0, 4) [
--     (Nothing, Nothing), 
--     (Just 43, Nothing), 
--     (Just 23, Nothing), 
--     (Nothing, Nothing), 
--     (Just 55, Nothing)]

insert :: (Hash k, Eq k) => k -> v -> OADict k v -> OADict k v
insert key value oad =
    if fromIntegral (currentSize oad + 1) >= (loadFactor oad * fromIntegral (maxSize oad)) then regeneratedDict else insertNormal
        where
            regeneratedDict =
                insert key value (addAll litsOfElem generatedDict)
                    where
                        generatedDict = OADictCons {maxSize = maxSize oad * 2, currentSize = 0, loadFactor = 0.75, arrayData = array (0, maxSize oad * 2 - 1) [(i, (Nothing, Nothing)) | i <- [0..maxSize oad * 2 - 1]]}
                        litsOfElem = getDataAsList oad
            insertNormal =
                OADictCons (currentSize oad + (if isNothing (fst (arrayData oad ! position)) then 1 else 0)) (maxSize oad) (loadFactor oad) newData
                    where 
                        newData = arrayData oad//[(position, (Just key, Just value))]
                        position = findFreePlace key (rem (hash key) (maxSize oad)) (arrayData oad)

get :: (Hash k, Eq k) => k -> OADict k v -> Maybe v
get key oad =
    let position = findFreePlace key (rem (hash key) (maxSize oad)) (arrayData oad) in
        snd (arrayData oad ! position)


fromList :: (Hash k, Eq k) => [(k, v)] -> OADict k v
fromList [] = emptyDict
fromList ((k1, v1):kvs) = insert k1 v1 (fromList kvs)
a2 :: OADict Int Char
a2 = fromList [(1, 'a'), (2, 'b'), (3, 'n'), (9, 'm'), (10, 'l')]

addAll :: (Hash k, Eq k) => [(k, v)] -> OADict k v -> OADict k v
addAll [] oad = oad
addAll ((k1, v1):xs) oad = insert k1 v1 (addAll xs oad)