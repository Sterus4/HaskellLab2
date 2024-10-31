{-# LANGUAGE InstanceSigs #-}
module Lib
    (
    OADict(..),
    emptyDict,

    getDataAsList,

    insert,
    delete,

    filterV,
    filterKV,

    fromList,
    addAll,

    get,
    size
    ) where
import Data.Ix()
import GHC.Arr ( (!), (//), array, elems, Array )
import Data.Maybe (isNothing, isJust)
import Data.List (sort, groupBy)
import Data.Hashable(Hashable, hash)

instance (Hashable key, Show key, Show value) => Show (OADict key value) where
    show oad = "Max size:\t"  ++ show (maxSize oad) ++ "\nCurrent size:\t" ++ show (currentSize oad) ++ "\nLoad factor:\t" ++ show (loadFactor oad) ++ "\ndata :::\n" ++ show (getDataAsList oad)

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

insert :: (Hashable k) => k -> v -> OADict k v -> OADict k v
insert key value oad =
    if fromIntegral (currentSize oad + 1) >= loadFactor oad * fromIntegral (maxSize oad) then regeneratedDict else insertNormal
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

get :: (Hashable k) => k -> OADict k v -> Maybe v
get key oad =
    let position = findFreePlace key (rem (hash key) (maxSize oad)) (arrayData oad) in
        snd (arrayData oad ! position)


fromList :: (Hashable k) => [(k, v)] -> OADict k v
--fromList ((k1, v1):kvs) = insert k1 v1 (fromList kvs)
fromList = foldr (uncurry insert) emptyDict

addAll :: (Hashable k) => [(k, v)] -> OADict k v -> OADict k v
addAll [] oad = oad
addAll ((k1, v1):xs) oad = insert k1 v1 (addAll xs oad)


delete :: (Hashable k) => k -> OADict k v -> OADict k v
delete key oad =
    OADictCons (currentSize oad - (if isNothing (fst (arrayData oad ! position)) then 0 else 1)) (maxSize oad) (loadFactor oad) newData
        where
            newData = arrayData oad//[(position, (Nothing, Nothing))]
            position = findFreePlace key (rem (hash key) (maxSize oad)) (arrayData oad)

-- Тут есть вариант делать прямо полную свертку вместе с ключами, но можно объяснить, почему и так нормально:
instance Foldable (OADict k) where
    foldr :: (a -> b -> b) -> b -> OADict k a -> b
    foldr f acc oad = foldr f acc $ map snd (getDataAsList oad)

instance Functor (OADict k) where
    fmap :: (a -> b) -> OADict k a -> OADict k b
    fmap f oad =
        OADictCons (currentSize oad) (maxSize oad) (loadFactor oad) newData
        where
            newData = fmap f' (arrayData oad)
            f' (a, b) = case (a, b) of
                (Nothing, _) -> (Nothing, Nothing)
                (_, Nothing) -> (Nothing, Nothing)
                (a1, Just b1) -> (a1, Just (f b1))

filterV :: (a -> Bool) -> OADict k a -> OADict k a
filterV f oad =
    OADictCons currentSize' (maxSize oad) (loadFactor oad) newData
    where
        newData = fmap f' (arrayData oad)
        f' (a1, b1) = case (a1, b1) of
            (Nothing, _) -> (Nothing, Nothing)
            (_, Nothing) -> (Nothing, Nothing)
            (Just a2, Just b2) -> if f b2 then (Just a2, Just b2) else (Nothing, Nothing)
        currentSize' = length $ filter (\(a, _) -> isJust a) (elems newData)

filterKV :: ((k, v) -> Bool) -> OADict k v -> OADict k v
filterKV f oad =
    OADictCons currentSize' (maxSize oad) (loadFactor oad) newData
    where
        newData = fmap f' (arrayData oad)
        f' (a1, b1) = case (a1, b1) of
            (Nothing, _) -> (Nothing, Nothing)
            (_, Nothing) -> (Nothing, Nothing)
            (Just a2, Just b2) -> if f (a2, b2) then (Just a2, Just b2) else (Nothing, Nothing)
        currentSize' = length $ filter (\(a, _) -> isJust a) (elems newData)



instance (Ord k, Ord v, Hashable k, Eq k) => Semigroup (OADict k v) where
    (<>) :: OADict k v -> OADict k v -> OADict k v
    oad1 <> oad2 = fromList $ map head (groupBy (\x y -> fst x == fst y) (sort (getDataAsList oad1 ++ getDataAsList oad2)))

instance (Ord k, Ord v, Hashable k, Eq k) => Monoid (OADict k v) where
    mempty = emptyDict

size :: OADict k v -> Int
size = currentSize

-- TODO
instance (Hashable k, Eq v) => Eq (OADict k v) where
    (==) :: OADict k v -> OADict k v -> Bool
    (==) oad1 oad2 = currentSize oad1 == currentSize oad2
        && maxSize oad1 == maxSize oad2
        && haveAll (getDataAsList oad1) oad2
        && haveAll (getDataAsList oad2) oad1 where
            haveAll :: (Hashable a, Eq b) => [(a, b)] -> OADict a b -> Bool
            haveAll [] _ = True
            haveAll (x:xs) oad = get (fst x) oad == Just (snd x) && haveAll xs oad
