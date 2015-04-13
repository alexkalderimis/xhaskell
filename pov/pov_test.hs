import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import POV(Graph(..), fromPOV, tracePathBetween)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList (tracePathTest : reparentingTests) ]

leaf :: a -> Graph a
leaf v = Graph v []

x :: String
x = "x"

singleton, flat, kids, nested, cousins :: Graph String
singleton = Graph x []
flat = Graph "root" (map leaf ["a", "b", x, "c"])
nested = Graph "level-0" [Graph "level-1" [Graph "level-2" [Graph "level-3" [Graph x []]]]]
kids = Graph "root" [Graph x [Graph "kid-0" [], Graph "kid-1" []]]
cousins = Graph "grandparent" [Graph "parent" [Graph x [leaf "kid-a", leaf "kid-b"],
                                               (leaf "sibling-0"),
                                               (leaf "sibling-1")],
                                Graph "uncle" [(leaf "cousin-0"),
                                               (leaf "cousin-1")]]

singleton', flat', nested', kids', cousins' :: Graph String
singleton' = singleton
flat' = Graph x [Graph "root" (map leaf ["a", "b", "c"])]
nested' = Graph x [Graph "level-3" [Graph "level-2" [Graph "level-1" [Graph "level-0" []]]]]
kids' = Graph x [Graph "kid-0" [], Graph "kid-1" [], Graph "root" []]
cousins' = Graph x [Graph "parent" [Graph "sibling-0" [],
                                   Graph "sibling-1" [],
                                   Graph "grandparent" [
                                                        Graph "uncle" [Graph "cousin-0" [],
                                                                       Graph "cousin-1" []]]]]

reparentTestCases :: [(String, Graph String, Maybe (Graph String))]
reparentTestCases = [
    ("reparenting singleton", singleton, Just singleton'),
    ("reparenting flat", flat, Just flat'),
    ("reparenting nested", nested, Just nested'),
    ("reparenting kids", kids, Just kids'),
    ("reparenting cousins", cousins, Just cousins'),
    ("from POV of non-existent node", (leaf "foo"), Nothing)]

reparentingTests :: [Test]
reparentingTests = do
    (name, input, output) <- reparentTestCases
    return $ testCase name $ output @=? (fromPOV x input)

tracePathTest :: Test
tracePathTest = testCase "Can trace a path from x -> cousin" $ expectedPath @=? (tracePathBetween x "cousin-1" cousins)
    where expectedPath = ["x", "parent", "grandparent", "uncle", "cousin-1"]
