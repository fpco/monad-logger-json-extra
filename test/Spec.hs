{-# LANGUAGE OverloadedStrings, TemplateHaskell,
  ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L8
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Control.Concurrent.MVar (newMVar, modifyMVar_, takeMVar)
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec
       (SpecWith, expectationFailure, shouldBe, shouldStartWith, it,
        describe)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.Hspec (testSpec)

import Control.Monad.Logger.JSON.Extra

main :: IO ()
main = do
    tests <- testGroup "all" <$> sequence [jsonLoggingSpec]
    defaultMain tests

jsonLoggingSpec :: IO TestTree
jsonLoggingSpec = do
    testSpec "Control.Monad.Logger.JSON.Extra" $
        describe "Messages formatted correctly" $
        do itLog
               "String message with location"
               ($logInfoJ ("some information message" :: String))
               "info"
               Nothing
               (String "some information message")
               True
           itLog
               "String message with source and location"
               ($logErrorSJ "SOMESOURCE" ("some error message" :: String))
               "error"
               (Just "SOMESOURCE")
               (String "some error message")
               True
           itLog
               "String message with LevelOther and location"
               ($(logOtherJ "SOMELEVEL") ("some other message" :: String))
               "SOMELEVEL"
               Nothing
               (String "some other message")
               True
           itLog
               "String message without location"
               (logDebugNJ ("some debug message" :: String))
               "debug"
               Nothing
               (String "some debug message")
               False
           itLog
               "String message with source but without location"
               (logWarnNSJ "SOMESOURCE" ("some warning message" :: String))
               "warn"
               (Just "SOMESOURCE")
               (String "some warning message")
               False
           itLog
               "Structured message"
               (logInfoNJ
                    (object
                         [ "question" .=
                           ("life, the universe, and everything" :: String)
                         , "answer" .= (42 :: Int)]))
               "info"
               Nothing
               (Object
                    (Map.fromList
                         [ ("answer", Number 42.0)
                         , ( "question"
                           , String "life, the universe, and everything")]))
               False

itLog
    :: String
    -> LoggingT IO t
    -> T.Text
    -> Maybe T.Text
    -> Value
    -> Bool
    -> SpecWith ()
itLog desc logAction expectLevel expectSource expectMessage expectHasLocation =
    it desc $
    do ((),[j]) <-
           runListJSONLoggingT (return $ encodeUtf8 logDate) $
           do _ <- logAction
              return ()
       let mobj :: (Maybe Value) = decode' $ L8.fromChunks [fromLogStr j]
       case mobj of
           Just (Object obj0) -> do
               let obj' = Map.delete "location" obj0
                   mloc = Map.lookup "location" obj0
               if expectHasLocation
                   then case mloc of
                            Just (String loc) ->
                                T.unpack loc `shouldStartWith`
                                "main:Main test/Spec.hs:"
                            Just v ->
                                expectationFailure
                                    ("Location should be a String, but is: " ++
                                     show v)
                            Nothing ->
                                expectationFailure "Location should be set"
                   else mloc `shouldBe` Nothing
               obj' `shouldBe`
                   (Map.fromList $
                    concat
                        [ [ ("date", String logDate)
                          , ("message", expectMessage)
                          , ("level", String expectLevel)]
                        , case expectSource of
                              Nothing -> []
                              Just src -> [("source", String src)]])
           Just v ->
               expectationFailure
                   ("Parsed log output should be an Object, but is: " ++ show v)
           Nothing ->
               expectationFailure
                   ("Parsing log output as JSON failed: " ++ show (fromLogStr j))

logDate :: T.Text
logDate = "20160801T11:59:00Z"

runListJSONLoggingT :: (IO S8.ByteString) -> LoggingT IO a -> IO (a,[LogStr])
runListJSONLoggingT getdate inner = do
    mv <- newMVar []
    a <- runLoggingT inner (output mv)
    l <- takeMVar mv
    return (a,reverse l)
  where
    output mv loc src level msg = do
        j <- formatJSONLogMessage getdate loc src level msg
        modifyMVar_ mv (return . (j :))
