-- |

module GenIP where


import           Data.Bits
import           Data.List
import           Data.Word
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           IPTypes
import           ParseIP
import           Test.Tasty.Hedgehog

genIP :: Gen IP
genIP = IP <$> Gen.word32 Range.linearBounded


genIPComponents :: Gen [Word8]
genIPComponents = Gen.list (Range.singleton 4) (Gen.word8 Range.linearBounded)


genIPRange :: Gen IPRange
genIPRange = do
  (IP ip1) <- genIP
  ip2 <- Gen.word32 $ Range.linearFrom (ip1 + 1) ip1 maxBound
  pure $ IPRange (IP ip1) (IP ip2)


genIPRangeDBSized :: Int -> Int -> Gen IPRangeDB
genIPRangeDBSized minLen maxLen =
  IPRangeDB <$> Gen.list (Range.constant minLen maxLen) genIPRange


genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = do
  n1 <- Gen.integral (Range.constant 1 100)
  n2 <- Gen.integral (Range.constant n1 100)
  genIPRangeDBSized n1 n2

genIPString :: Gen String
genIPString = concat . intersperse "." . map show <$> genIPComponents


buildIP_foldr :: [Word8] -> IP
buildIP_foldr = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k*256)

buildIP_foldl :: [Word8] -> IP
buildIP_foldl = IP . foldl (\s b -> s*256 + fromIntegral b) 0

buildIP_foldl_shl :: [Word8] -> IP
buildIP_foldl_shl = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0

prop_buildIP :: Property
prop_buildIP = property $ do
  ipcs <- forAll genIPComponents
  let ip = buildIP ipcs
  buildIP_foldl ipcs === ip
  buildIP_foldr ipcs === ip
  buildIP_foldl_shl ipcs === ip


prop_parseIP :: Property
prop_parseIP = property $ do
  ip <- forAll genIP
  parseIP (show ip) === Just ip

prop_parseIP_show :: Property
prop_parseIP_show = property $ do
  ip <- forAll genIP
  tripping ip show parseIP


prop_parseIPRange_show :: Property
prop_parseIPRange_show = property $ do
  ipr <- forAll genIPRange
  tripping ipr show parseIPRange

prop_parseIPRanges_show :: Property
prop_parseIPRanges_show = property $ do
  iprdb <- forAll genIPRangeDB
  tripping iprdb show parseIPRanges

props = [
  testProperty "buildIP implementations agrees with each other" prop_buildIP
  , testProperty "parseIP works as expected" prop_parseIP
  , testProperty "parseIP agrees with show" prop_parseIP_show
  , testProperty "parseIPRange agrees with show" prop_parseIPRange_show
  , testProperty "parseIPRanges agrees with show" prop_parseIPRanges_show
  ]
