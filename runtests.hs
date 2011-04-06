{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8              ({- IsString -})
import           Data.Char                               (ord, chr)
import           Data.String
import           Data.Text                               (toLower)
import           Data.XML.Types
import           Test.HUnit                              hiding (Test)
import           Test.Hspec
import           Test.Hspec.HUnit
import           Text.XML.Enumerator.Combinators.General
import           Text.XML.Enumerator.Combinators.Tags
import           Text.XML.Enumerator.Parse               (decodeEntities)
import qualified Control.Exception                       as C
import qualified Data.ByteString.Lazy                    as L
import qualified Data.Map                                as Map
import qualified Text.XML.Enumerator.Parse               as P

main :: IO ()
main = hspec $ describe "XML combinators"
    [ it "has working chooseSplit" testChooseSplit
    , it "has working permute" testPermute
    , it "has working permuteFallback" testPermuteFallback
    , it "has working tags" testTags
    , it "has working tagsPermute" testTagsPermute
    , it "has working tagsPermuteRepetition" testTagsPermuteRepetition
    ]

testChooseSplit = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- chooseSplit (\t-> P.tagNoAttr t (return t)) ["a", "b", "c"]
        liftIO $ x @?= Just ("b",["a","c"])
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<b/>"
        , "</hello>"
        ]

testPermute 
    = do
        let frame input = P.parseLBS_ input decodeEntities $ do
                            P.force "need hello" $ P.tagNoAttr "hello" $ 
                              permute (\t -> P.tagNoAttr t (return t)) ["a", "b"]
        frame input1 >>= \result1 -> result1 @?= Just ["a", "b"]
        frame input2 >>= \result2 -> result2 @?= Just ["b", "a"]
        frame input3 >>= \result3 -> result3 @?= Nothing
        C.try (frame input4) >>= \result4 -> case result4 of
                                               Left (P.XmlException { 
                                                            P.xmlBadInput = Just (EventBeginElement 
                                                                                    Name { 
                                                                                      nameLocalName = "c"
                                                                                    , nameNamespace = Nothing
                                                                                    , namePrefix = Nothing 
                                                                                    }
                                                                                    _) 
                                                            }) -> return () -- right type of error
                                               Left  _ -> assertFailure "wrong error"
                                               Right _ -> assertFailure "erroneous document requires an error"
  where
    input1 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<b/>"
        , "</hello>"
        ]
    input2 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<b/>"
        , "<a/>"
        , "</hello>"
        ]
    input3 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "</hello>"
        ]
    input4 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<c/>"
        , "</hello>"
        ]

testPermuteFallback
    = do
        let frame input = P.parseLBS_ input decodeEntities $ do
                            P.force "need hello" $ P.tagNoAttr "hello" $ 
                              permuteFallback (fmap return `fmap` P.contentMaybe) 
                                               (\t -> P.tagNoAttr t (return $ nameLocalName t)) 
                                               ["a", "b"]
        frame input1 >>= \result1 -> result1 @?= Just ["a", "t", "b"]
        frame input2 >>= \result2 -> result2 @?= Just ["t", "b", "a"]
        frame input3 >>= \result3 -> result3 @?= Nothing
        C.try (frame input4) >>= \result4 -> case result4 of
                                               Left (P.XmlException { 
                                                            P.xmlBadInput = Just (EventBeginElement 
                                                                                    Name { 
                                                                                      nameLocalName = "c"
                                                                                    , nameNamespace = Nothing
                                                                                    , namePrefix = Nothing 
                                                                                    }
                                                                                    _) 
                                                            }) -> return () -- right type of error
                                               Left  _ -> assertFailure "wrong error"
                                               Right _ -> assertFailure "erroneous document requires an error"
  where
    input1 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "t"
        , "<b/>"
        , "</hello>"
        ]
    input2 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "t"
        , "<b/>"
        , "<a/>"
        , "</hello>"
        ]
    input3 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "</hello>"
        ]
    input4 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<c/>"
        , "</hello>"
        ]

testTags = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- tags (\state name -> do 
                       let n = nameLocalName name
                       guard (n == fromString [chr $ ord 'a' + state]) 
                       Just (return (), \_ -> return $ Just (state + 1, Just n)))
                    (const $ return Nothing)
                    0
        liftIO $ x @?= (5, ["a", "b", "c", "d", "e"])
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<b/>"
        , "<c/>"
        , "<d/>"
        , "<e/>"
        , "</hello>"
        ]

testTagsPermute = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        let p c = (return (), \_ -> return (Just c))
        x <- tagsPermute (toLower . nameLocalName) 
                           (Map.fromList $ map (\c -> (c, p c)) 
                                   ["a", "b", "c", "d", "e"])
                           (return Nothing)
        liftIO $ x @?= Just ["d", "b", "e", "a", "c"]
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<d/>"
        , "<b/>"
        , "<E/>"
        , "<a/>"
        , "<C/>"
        , "</hello>"
        ]

testTagsPermuteRepetition = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        let p r c = (r, return (), \_ -> return (Just ()))
        x <- tagsPermuteRepetition (toLower . nameLocalName) 
                                     (Map.fromList $ map (\c -> (c, p repeatOnce c)) ["a", "b", "c", "d", "e"] ++
                                                     map (\c -> (c, p repeatMany c)) ["r"])
                                     (return Nothing)
        liftIO $ fmap (map fst) x @?= Just ["d", "r", "b", "e", "r", "a", "c"]
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<d/>"
        , "<r/>"
        , "<b/>"
        , "<E/>"
        , "<r/>"
        , "<a/>"
        , "<C/>"
        , "</hello>"
        ]
