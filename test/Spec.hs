import Test.Hspec
import Test.QuickCheck


selfConvertor :: Convertor f a -> a -> a
selfConvertor f = f (Proxy :: Proxy f)

fromToSelf :: Convertor f a -> a -> a
fromToSelf f = fromTo f f

main :: IO ()
main = hspec $ do
  describe "angles" $ do
   it "self radians" $ do