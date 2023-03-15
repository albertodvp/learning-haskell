import           LookupIPSpec
import           ParseIPSpec
import           Test.Tasty
import           Test.Tasty.Hspec

main = do
  specs <- concat <$> mapM testSpecs
           [ parseIPSpecs,
             lookupIPSpecs
           ]
  defaultMain (testGroup "All Tests" [testGroup "Specs" specs])

