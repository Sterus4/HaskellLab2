import Test.HUnit
import Lib
import Data.Hashable (Hashable)

main :: IO Counts
main = runTestTT propTests



propertyMonoidEmpty :: (Hashable k, Ord v, Ord k) => [(k, v)] -> Bool
propertyMonoidEmpty list =
    all (== oad) [mempty <> oad, oad <> mempty] where
        oad = fromList list

propertyMonoidAssoc :: (Ord k, Ord v, Hashable k) => [(k, v)] -> [(k, v)] -> [(k, v)] -> Bool
propertyMonoidAssoc listData1 listData2 listData3 =
    (oad1 <> oad2) <> oad3 == oad1 <> (oad2 <> oad3)
    where
        oad1 = fromList listData1
        oad2 = fromList listData2
        oad3 = fromList listData3


listData :: [(Int, Char)]
listData = take 100 $ iterate (\(x, y) -> (x * 2, succ y)) (1, 'a')

-- TODO there is only one test case
testMonoid1 :: Test
testMonoid1 = TestLabel "Check mempty law of monoid: " $ TestCase $ assertBool "Mempty law of monoid Failed" (propertyMonoidEmpty listData)

testMonoid2 :: Test
testMonoid2 = TestLabel "Check assoc law of monoid: " $ TestCase $ assertBool "Assoc law of monoid Failed" (propertyMonoidAssoc (take 10 listData) (take 15 $ drop 10 listData) (take 50 $ drop 25 listData))



propTests :: Test
propTests = TestList [testMonoid1, testMonoid2]

