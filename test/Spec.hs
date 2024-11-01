import Test.HUnit
import Test.QuickCheck
import Lib
    ( fromList, getValuesData, filterV, filterKV, size, get, delete )
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Maybe (isNothing)


main :: IO Counts
main = do
    quickCheck (withMaxSuccess 100 propertyMonoidEmpty)
    quickCheck (withMaxSuccess 100 propertyMonoidAssoc)
    quickCheck (withMaxSuccess 100 propertySizeCheck)
    quickCheck (withMaxSuccess 100 propertyFromList)
    quickCheck (withMaxSuccess 1000 propertyDelete)
    runTestTT allTests

propertyDelete :: Int -> [(Int, Int)] -> Bool
propertyDelete deleted list =
    isNothing $ get deleted oad where
        oad = delete deleted (fromList list)

propertyFromList :: [Int] -> Bool
propertyFromList l =
    sort l == sort (getValuesData oad) where
        oad = fromList (zip [1::Int,2..] l)

propertySizeCheck :: [Int] -> Bool
propertySizeCheck list =
    length list == size oad where
        oad = fromList (zip [1::Int,2..] list)

propertyMonoidEmpty :: [(Int, Char)] -> Bool
propertyMonoidEmpty list =
    all (== oad) [mappend mempty oad, mappend oad mempty] where
        oad = fromList list

propertyMonoidAssoc :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)] -> Bool
propertyMonoidAssoc listData1 listData2 listData3 =
    (oad1 <> oad2) <> oad3 == oad1 <> oad2 <> oad3
    where
        oad1 = fromList listData1
        oad2 = fromList listData2
        oad3 = fromList listData3

unitMapTest :: Eq b => (a -> b) -> [a] -> Bool
unitMapTest f l =
    oad1 == oad2 where
        oad1 = fmap f (fromList $ zip [1::Int,2..] l)
        oad2 = fromList $ zip [1,2..] (map f l)

unitFilterVTest :: (Eq a) => (a -> Bool) -> [a] -> Bool
unitFilterVTest f l =
    getValuesData oad1 == getValuesData oad2 where
        oad1 = filterV f (fromList $ zip [1::Int,2..] l)
        oad2 = fromList $ zip [1::Int,2..] (filter f l)

unitFilterKVTest :: (Hashable a, Eq b) => ((a, b) -> Bool) -> [(a, b)] -> Bool
unitFilterKVTest f l =
    oad1 == oad2 where
        oad1 = filterKV f (fromList l)
        oad2 = fromList (filter f l)


listData :: [(Int, Char)]
listData = take 100 $ iterate (\(x, y) -> (x * 2, succ y)) (1, 'a')

-- TODO there is only one test case
testMonoid1 :: Test
testMonoid1 = TestLabel "Check mempty law of monoid: " $ TestCase $ assertBool "Mempty law of monoid Failed" (propertyMonoidEmpty listData)

testMonoid2 :: Test
testMonoid2 = TestLabel "Check assoc law of monoid: " $ TestCase $ assertBool "Assoc law of monoid Failed" (propertyMonoidAssoc (take 10 listData) (take 15 $ drop 10 listData) (take 50 $ drop 25 listData))

testMap :: Test
testMap = TestLabel "Check map law: " $ TestCase $ assertBool "Map law failed" (unitMapTest (*(2::Int)) [1, 2, 3, 5])

testFilter :: Test
testFilter = TestLabel "Check filter (value) law: " $ TestCase $ assertBool "Filter (value) law failed" (unitFilterVTest (>4) [1::Int, 2, 3, 5])

testFilterKV :: Test
testFilterKV = TestLabel "Check filter (key-value) law: " $ TestCase $ assertBool "Filter (key-value) law failed" (unitFilterKVTest (\(x, y) -> x + y > 10) [(1::Int, 2::Int), (3, 4), (4, 5), (100, 100)])


unitTests :: Test
unitTests = TestList [testMap, testFilter, testFilter, testFilterKV]

propTests :: Test
propTests = TestList [testMonoid1, testMonoid2]

allTests :: Test
allTests = TestList [unitTests, propTests]