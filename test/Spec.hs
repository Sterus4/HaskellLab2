import Test.HUnit
import Lib (fromList)
import Data.Hashable (Hashable)


main :: IO Counts
main = do
    _ <- runTestTT propTests
    runTestTT unitTests



propertyMonoidEmpty :: (Hashable k, Ord v, Ord k) => [(k, v)] -> Bool
propertyMonoidEmpty list =
    all (== oad) [mappend mempty oad, mappend oad mempty] where
        oad = fromList list

propertyMonoidAssoc :: (Ord k, Ord v, Hashable k) => [(k, v)] -> [(k, v)] -> [(k, v)] -> Bool
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

-- unitFilterVTest :: (Eq a) => (a -> Bool) -> [a] -> Bool
-- unitFilterVTest f l =
--     getValuesData oad1 == getValuesData oad2 where
--         oad1 = filterV f (fromList $ zip [1::Int,2..] l)
--         oad2 = fromList $ zip [1::Int,2..] (filter f l)


listData :: [(Int, Char)]
listData = take 100 $ iterate (\(x, y) -> (x * 2, succ y)) (1, 'a')

-- TODO there is only one test case
testMonoid1 :: Test
testMonoid1 = TestLabel "Check mempty law of monoid: " $ TestCase $ assertBool "Mempty law of monoid Failed" (propertyMonoidEmpty listData)

testMonoid2 :: Test
testMonoid2 = TestLabel "Check assoc law of monoid: " $ TestCase $ assertBool "Assoc law of monoid Failed" (propertyMonoidAssoc (take 10 listData) (take 15 $ drop 10 listData) (take 50 $ drop 25 listData))

testMap :: Test
testMap = TestLabel "Check map law: " $ TestCase $ assertBool "Map law failed" (unitMapTest (*(2::Int)) [1, 2, 3, 5])

-- testFilter :: Test
-- testFilter = TestLabel "Check filter (value) law: " $ TestCase $ assertBool "Filter (value) law failed" (unitMapTest (*(2::Int)) [1, 2, 3, 5])


unitTests :: Test
unitTests = TestList [testMap]

propTests :: Test
propTests = TestList [testMonoid1, testMonoid2]

