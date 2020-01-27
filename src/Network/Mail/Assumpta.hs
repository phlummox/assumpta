
{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Assumpta
  where
import Network.BSD (getHostName)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Network.Mail.Assumpta.Text as AT
import qualified Network.Mail.Assumpta.String as AS
import qualified Network.Mail.Mime as MM

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString ( ByteString )
import Data.Monoid ( (<>) )

---- | A simple interface for generating an email with HTML and plain-text
---- alternatives and some file attachments.
----
---- Note that we use lazy IO for reading in the attachment contents.
simpleMail :: MM.Address -- ^ to
           -> MM.Address -- ^ from
           -> T.Text -- ^ subject
           -> TL.Text -- ^ plain body
           -> TL.Text -- ^ HTML body
           -> [(T.Text, FilePath)] -- ^ content type and path of attachments
           -> String  -- ^ SMTP server to connect to
           -> Int -- ^ Port of SMTP server
           -> IO (T.Text, Either AS.SmtpError ())
simpleMail to from subject plainBody htmlBody attachments server port =
  do
    mail <- escape . BSL.toStrict <$> (MM.renderMail' =<< 
              MM.simpleMail to from subject plainBody htmlBody attachments)
    putStrLn $ show $ "mail:\n=====\n" <> mail <> "======\n"
    let strToBinary = TE.encodeUtf8 . T.pack
        mail' = 
                if AS.crlf `T.isSuffixOf` mail
                then mail <> "." <> AS.crlf 
                else mail <> AS.crlf <> "." <> AS.crlf
        to' =  MM.addressEmail to
        from' =  MM.addressEmail from
    res <- AT.runSmtp server port $ do
      AT.expectGreeting
      AT.ehlo $ T.pack server
      AT.mailFrom from'
      AT.rcptTo to'
      AT.data_ mail'
      AT.quit
    return (mail, res)
  where
    --escape = AT.escapePeriods . AT.toCrLf . TE.decodeUtf8 
    escape = AT.toCrLf . TE.decodeUtf8 

--testX :: IO (Either AS.SmtpError ())
testX =
  let addr =  MM.Address (Just "joe") "joe@place"
      subj = "a test subj"
      body = "a test body"
  in  simpleMail addr addr subj body "" [] "localhost" 2025





