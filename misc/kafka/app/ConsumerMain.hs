{-# LANGUAGE OverloadedStrings #-}

module ConsumerMain where

import           Control.Exception (bracket)
import           Control.Monad
import qualified Data.Map          as M
import           Kafka.Consumer

config = [ ("security.protocol", "sasl_ssl")
         , ("sasl.mechanisms", "PLAIN")
         , ("sasl.username", "")
         , ("sasl.password", "")
         ]

consumerProps :: ConsumerProperties
consumerProps = brokersList [ "pkc-6ojv2.us-west4.gcp.confluent.cloud:9092"] <> groupId "poem_readers" <> noAutoCommit <> logLevel KafkaLogInfo <> extraProps (M.fromList config)

consumerSub :: Subscription
consumerSub = topics ["poems"] <> offsetReset Earliest

consumerMain :: IO ()
consumerMain = do
  res <- bracket mkConsumer clConsumer runHandler
  print res
  where
    mkConsumer = newConsumer consumerProps consumerSub
    clConsumer (Left err) = return (Left err)
    clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
    runHandler (Left err)  = return (Left err)
    runHandler (Right  kc) = processMessages kc


processMessages :: KafkaConsumer -> IO (Either KafkaError ())
processMessages kafka = do
  replicateM_ 10 $ do
    msg <- pollMessage kafka (Timeout 3000)
    putStrLn $ "Message " <> show msg
    err <- commitAllOffsets OffsetCommit kafka
    putStrLn $ "Offsets: " <> maybe "Committed." show err
  return $ Right ()
