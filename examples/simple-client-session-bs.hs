
{-# LANGUAGE OverloadedStrings #-}

-- Program that runs a short SMTP session
-- with a server running on local port 2025.

module Main where

import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.BSD (getHostName)
import Network.Mail.Assumpta.ByteString as M

sampleMesg :: String
sampleMesg = unlines [
    "Date: Tue, 21 Jan 2020 02:28:37 +0800"
  , "To: neddie.seagoon@gmail.com"
  , "From: jim.moriarty@gmail.com"
  , "Subject: test Tue, 21 Jan 2020 02:28:37 +0800"
  , ""
  , "Sapristi nyuckoes!"
  ]

-- The sender and recipient supplied to the
-- SMTP server are what is used to route the
-- message; any 'To:' and 'From:' fields
-- in the message are completely ignored for this
-- purpose.
sender = "somesender@mycorp.com"
recipient = "somerecipient@mozilla.org"

main :: IO ()
main = do
  let toBinary = TE.encodeUtf8 . T.pack 
      port = 2025
      -- If we call the data_ function ourselves,
      -- we must end the messager with a <CRLF> '.' <CRLF>
      -- sequence.
      myMesg' = toBinary sampleMesg <> crlf <> "." <> crlf
  hostname <- getHostName
  print =<< runSmtp "localhost" port (do
    expectGreeting
    ehlo $ toBinary hostname
    mailFrom sender
    rcptTo recipient
    data_ myMesg'
    quit)


