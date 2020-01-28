
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Network.Mail.Assumpta.ByteStringSpec
  (main, spec)
  where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.IO (propertyIO)

import           Control.Concurrent
import           Control.Exception (bracket, evaluate)
import           Control.Monad

import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Char8 ( ByteString )
import           Data.Monoid -- for early base

import           Network.Socket as N hiding (recv)
import           Network.Socket.ByteString as N (sendAll, recv)


import Network.Mail.Assumpta.ByteString

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- oops
deriving instance Eq ReplyLine
deriving instance Eq SmtpError

-- .. probably shouldn't churn through *quite* so many sockets
-- so fast - but I haven't run out, so a pool doesn't seem
-- necessary yet.
mkServerSoc :: IO (N.Socket, N.PortNumber)
mkServerSoc = do
    -- wildcard port
    let addr = N.SockAddrInet N.aNY_PORT N.iNADDR_ANY
    soc <- N.socket N.AF_INET N.Stream N.defaultProtocol
    N.bind soc addr
    N.listen soc 5
    -- find out what port we got
    port <- N.socketPort soc
    return (soc,port)

-- | @runSmtpWithServer a replies@
--
-- spin up a TCP server, run 'runSmtp' to send
-- commands to it, return result, plus requests received
-- by server. @replies@ are the canned responses
-- the server is to make.
--
-- The client needs to 'expect' something to start
-- with (i.e. the server goes first), else
-- things are likely to get out of sync.
runSmtpWithServer ::
  Smtp a -> [ByteString] -> IO (Either SmtpError a, ByteString)
runSmtpWithServer commands replies = do
    (soc, port) <- mkServerSoc
    mRequestsRecvd <- newEmptyMVar
    -- server
    void $ forkIO $
        let acquire = fst <$> N.accept soc
            release = N.close
        in bracket acquire release $ \soc' -> do
              requests <- forM replies $ \reply -> do
                N.sendAll soc' $ reply <> crlf
                N.recv soc' 4096
              -- ??? needed??
              requests <- evaluate requests
              --
              putMVar mRequestsRecvd $ mconcat requests
    clientRes <- runSmtp "localhost" (fromIntegral port) commands
    -- return results
    requestsRcvd <- readMVar mRequestsRecvd
    return (clientRes, requestsRcvd)

-- make a server test case out of description, cmds to run, etc
mkSmtpTestCase ::
  (Show b, Eq b) =>
    String
    -> Smtp b
    -> [ByteString]
    -> (Either SmtpError b, ByteString)
    -> SpecWith ()
mkSmtpTestCase testCaseName cmds serverReplies expectedResult =
  context ("when run with test case " <> show testCaseName) $
    it "should send expected bytes & return expected result" $
      runSmtpWithServer cmds serverReplies `shouldReturn` expectedResult

-- | when the client disconnects, the server should get a
-- final empty bytestring
smtpTestCases :: [SpecWith ()]
smtpTestCases = [
    mkSmtpTestCase
            "helo then quit"
            -- cmds 
            (do expectGreeting
                helo "myhost"
                quit )
            -- responses
            [ "220 hi there"
            , "250 OK"
            , "221 Bye"
            ]
            -- expect
            (Right (), mconcat ["HELO myhost\r\n"
                                ,"QUIT\r\n"])

  , mkSmtpTestCase
            "improper greeting"
            -- cmds 
            expectGreeting
            -- responses
            [ "250 OK" -- bad greeting
            ]
            -- expect
            (Left (UnexpectedResponse "220" [ReplyLine 250 "OK"]), "")

  ]

data_Spec :: SpecWith ()
data_Spec = 
  describe "data_ function" $
    it "should pass on its content unchanged" $ property $
      \(NonEmpty someData) -> ioProperty $ do
          let someData' = BSC.pack someData 
              cmds = do expectGreeting
                        data_ $ someData' <> crlf
              serverReplies = [
                  "220 hi there"
                , "354 End data with <CR><LF>.<CR><LF>"
                , "250 OK"
                ]
          res <- runSmtpWithServer cmds serverReplies
          return $ propertyIO $ 
              res `shouldBe` (Right (), "DATA\r\n" <> someData' <> "\r\n")


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runSmtp" $ 
    sequence_ smtpTestCases
  data_Spec

