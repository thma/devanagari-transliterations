import           Data.Char.Devanagari.DevanagariTokens
import           Data.Char.Devanagari.Generators
import           Data.Char.Devanagari.Tokenizer
import           Data.Sequence
import           Data.Text.Short                       (ShortText, pack)
import qualified Hedgehog.Gen                          as Gen
import           IoUtils                               (writeFileUtf8)
import           Test.Hspec
import           Test.Hspec.Hedgehog                   (Gen, forAll, hedgehog,
                                                        tripping)

toEither :: Tokenizer -> ShortText -> Either String (Seq DevanagariToken)
toEither f = Right . f

-- `main` is here so that this module can be run from GHCi on its own.  It is
--  not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

zwnj :: [Char]
zwnj = ['\8204'] -- Zero Width Non Joiner

zwj :: [Char]
zwj = ['\8205'] -- Zero Width Joiner

harvard :: ShortText
harvard = pack "smAramiM"

iast :: ShortText
iast = pack "smāramiṃ"

deva :: ShortText
deva = pack "स्मारमिं"

iso :: ShortText
iso = pack "smāramiṁ"

spec :: Spec
spec = do
  describe "roundtrip" $
    it "has output identical to input after complex roundtrip" $ do
      let roundtrip = (toIast . fromIso . toIso . fromDevanagari . toDevanagari . fromHarvard . toHarvard . fromIast) iast
      roundtrip `shouldBe` iast
  describe "token based roundtrip" $ do
    it "works for all vowels" $ hedgehog $ do
      x <- forAll (Gen.enumBounded :: Gen Vowel)
      tripping (fromList [Vow x]) toDevanagari (toEither fromDevanagari)
      tripping (fromList [Vow x]) toHarvard (toEither fromHarvard)
      tripping (fromList [Vow x]) toIast (toEither fromIast)
      tripping (fromList [Vow x]) toIso (toEither fromIso)
    it "works for all consonants" $ hedgehog $ do
      x <- forAll (Gen.enumBounded :: Gen Consonant)
      tripping (fromList [Cons x, Vow A]) toDevanagari (toEither fromDevanagari)
      tripping (fromList [Cons x, Vow A]) toHarvard (toEither fromHarvard)
      tripping (fromList [Cons x, Vow A]) toIast (toEither fromIast)
      tripping (fromList [Cons x, Vow A]) toIso (toEither fromIso)
    it "works for all digits" $ hedgehog $ do
      x <- forAll (Gen.enumBounded :: Gen Digit)
      tripping (fromList [Dig x]) toDevanagari (toEither fromDevanagari)
      tripping (fromList [Dig x]) toHarvard (toEither fromHarvard)
      tripping (fromList [Dig x]) toIast (toEither fromIast)
      tripping (fromList [Dig x]) toIso (toEither fromIso)
    it "works for special characters" $ hedgehog $ do
      let specials = [Anusvara, Anunasika, Visarga, Avagraha, Virama, OM, PurnaViram, DeerghViram]
      x <- forAll (Gen.element specials)
      tripping (fromList [x]) toDevanagari (toEither fromDevanagari)

  describe "selectParserByContent" $
    it "selects correct parse function based on input" $ do
      selectTokenizerByContent deva deva `shouldBe` fromDevanagari deva
      selectTokenizerByContent iast iast `shouldBe` fromIast iast
      selectTokenizerByContent harvard harvard `shouldBe` fromHarvard harvard
      selectTokenizerByContent iso iso `shouldBe` fromIso iso
  describe "tokenize" $
    it "produces correct tokens for any input" $ do
      let expected = fromList [Cons S, Cons M, Vow AA, Cons R, Vow A, Cons M, Vow I, Anusvara]
      tokenize harvard `shouldBe` expected
      tokenize iast `shouldBe` expected
      tokenize deva `shouldBe` expected
      tokenize iso `shouldBe` expected
  describe "ligature handling" $ do
    it "produces correct consonant ligatures " $ do
      let harvard' = pack "sadgamaye"
      (toDevanagari . fromHarvard) harvard' `shouldBe` pack "सद्गमये"
      let harvard'' = pack "vakSyAmi"
      (toDevanagari . fromHarvard) harvard'' `shouldBe` pack "वक्ष्यामि"
    it "can enforce usage of an explicit virama to suppress consonant ligatures " $ do
      let harvard' = pack "sad_gamaye"
          deva' = pack ("सद्" ++ zwnj ++ "गमये")
      (toDevanagari . fromHarvard) harvard' `shouldBe` deva'
    it "can enforce usage of regular consonant ligatures instead of complex ligatures" $ do
      let harvard' = pack "vak\\SyAmi"
          deva' = pack ("वक्" ++ zwj ++ "ष्यामि")
      (toDevanagari . fromHarvard) harvard' `shouldBe` deva'
  describe "reading of special chars for viramas" $ do
    it "reads | correctly as PurnaViram" $ do
      let harvard' = pack "OM zAntiH |"
          deva' = pack "ॐ शान्तिः ।"
      (toDevanagari . fromHarvard) harvard' `shouldBe` deva'
    it "reads || correctly as DeerghViram" $ do
      let harvard' = pack "OM zAntiH ||"
          deva' = pack "ॐ शान्तिः ॥"
      (toDevanagari . fromHarvard) harvard' `shouldBe` deva'
    it "handles unmapped characters gracefully" $ do
      let harvard' = pack "rüpel"
          deva' = pack "र्üपेल"
      print $ fromHarvard harvard'
      (toDevanagari . fromHarvard) harvard' `shouldBe` deva'

  describe "Auto-Documentation" $ do
    it "produces complete markdown Table" $ do
      let mdTokenMap = tokenMapToMd
      writeFileUtf8 "tokenMap.md" mdTokenMap
    it "produces complete html Table" $ do
      let htmlTokenMap = tokenMapToHtml
      writeFileUtf8 "tokenMap.html" htmlTokenMap