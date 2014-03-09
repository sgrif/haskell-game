import Test.Framework as TF (defaultMain, Test)
import qualified Game.TestShape as TestShape

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = TestShape.tests
