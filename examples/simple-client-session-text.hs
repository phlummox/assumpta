
{-# LANGUAGE OverloadedStrings #-}

-- Program that runs a short SMTP session
-- with a server running on local port 2025.

module Main where

import           Data.Monoid
import qualified Data.Text as T
import Network.BSD (getHostName) -- requires 'network' package
import Network.Mail.Assumpta.Text

sampleMesg :: T.Text
sampleMesg = T.intercalate crlf [
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
sender, recipient :: T.Text
sender = "somesender@mycorp.com"
recipient = "somerecipient@mozilla.org"

main :: IO ()
main = do
  let  
      port = 2025
  hostname <- getHostName
  print =<< runSmtp "localhost" port (do
          expectGreeting
          ehlo $ T.pack hostname
          -- Properly, we should escape periods at the start of a
          -- line. But we know there aren't any
          sendRawMail sender [recipient] sampleMesg
          quit)


