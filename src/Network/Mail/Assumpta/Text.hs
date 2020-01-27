{-# LANGUAGE OverloadedStrings #-}

{- |

'Text' versions of Smtp operations.

This module does not validate that your text
satisfies the requirements of the relevant RFCs -- see the note in
"Network.Mail.Assumpta.MonadSmtp#permissiblecharacters"
about "permissible characters". In general, unless you are using
an SMTP extension, your text must consist of 7-bit clean ASCII.

-}

module Network.Mail.Assumpta.Text
  (
    -- * Run Smtp actions
    -- | Run actions in the 'MonadSmtp' monad.
    ABS.runSmtp
  , ABS.runSecureSmtp
  , ABS.runSmtpWithParams
  , ABS.runSmtpHandle
    -- * MonadError variants
    -- | Instead of returning an Either, these run in MonadError --
    -- thus the caller can specialize them to a 'Maybe', 'Either',
    -- or some other 'MonadError' instance as desired.
  , ABS.runSmtp'
  , ABS.runSecureSmtp'
  , ABS.runSmtpWithParams'
  , ABS.runSmtpHandle'
    -- * SMTP commands
    -- | Functions for sending commands
    -- to an SMTP server. In general, these are wrappers around
    -- 'command' and 'expect' -- they send some command,
    -- and try to parse a response (throwing an 'SmtpError'
    -- on failure).
    -- See the "Network.Mail.Assumpta.ByteString" module for
    -- documentation of each function.
  , helo
  , ehlo
  , mailFrom
  , rcptTo
  , data_
  , ABS.noop
  , ABS.quit
  , ABS.rset
  , ABS.startTLS
  , expn
  , vrfy
  , help
  , sendRawMail
    -- * Server responses
  , ABS.expect
  , ABS.expectGreeting
    -- * Low-level operations and types
  , ABS.command
  , ABS.Reply
  , ABS.ReplyLine(..)
  , ABS.ReplyCode
  , ABS.SmtpError(..)
    -- * Monad transformer
    -- | A monad transformer, 'SmtpT', which provides the ability to
    -- send SMTP commands and parse replies, together with operations
    -- on the transformer.
  , ABS.SmtpT
  , ABS.Smtp
  , ABS.liftSmtpT
  , ABS.mapSmtpT
    -- * Network operations
  , ABS.open
  , ABS.openTls
  , ABS.openParams
  , ABS.close
  , ABS.withHandle
  , ABS.withSecureHandle
  , ABS.withHandleParams
  , ABS.HostName
  , ABS.Port
  , ABS.Handle(..)
  , ABS.defaultTLSSettings
  , send
  , sendLine
    -- * Utility functions
  , toCrLf
  , ABS.rethrow
  , ABS.mkParams
  , ABS.toIOError
  , ABS.crlf
  , escapePeriods
  )
  where

import           Data.Foldable (toList)
import           Data.Monoid -- needed for early versions of Base
import qualified Data.Text as T
import           Data.Text ( Text )
import qualified Data.Text.Encoding as TE

import qualified Network.Mail.Assumpta.ByteString as ABS

import           Network.Mail.Assumpta.MonadSmtp ( MonadSmtp )
import qualified Network.Mail.Assumpta.MonadSmtp as M

-- | See 'M.helo'
helo :: MonadSmtp m => Text -> m () 
helo =  M.helo . TE.encodeUtf8

-- | See 'M.ehlo'
ehlo :: MonadSmtp m => Text -> m () 
ehlo =  M.ehlo . TE.encodeUtf8

-- | See 'M.mailFrom'
mailFrom :: MonadSmtp m => Text -> m ()
mailFrom =  M.mailFrom . TE.encodeUtf8

-- | See 'M.rcptTo'
rcptTo :: MonadSmtp m => Text -> m ()
rcptTo =  M.rcptTo . TE.encodeUtf8

-- | See 'M.data_'
data_ :: MonadSmtp m => Text -> m ()
data_ =  M.data_ . TE.encodeUtf8

-- | See 'M.expn'
expn :: MonadSmtp m => Text -> m M.Reply
expn =  M.expn . TE.encodeUtf8

-- | See 'M.vrfy'
vrfy :: MonadSmtp m => Text -> m M.Reply
vrfy =  M.vrfy . TE.encodeUtf8

-- | See 'M.help'
help :: MonadSmtp m => Maybe Text -> m M.Reply
help = M.help . fmap TE.encodeUtf8


-- | See 'M.send'
send :: MonadSmtp m => Text -> m ()
send =  M.send . TE.encodeUtf8

-- | See 'M.sendLine'
sendLine :: MonadSmtp m => Text -> m ()
sendLine =  M.sendLine . TE.encodeUtf8

-- | replace newlines (@\'\\n'@) with crlf sequence (@\'\\r\\n'@).
toCrLf :: Text -> Text
toCrLf txt = T.intercalate M.crlf $ T.lines txt

-- | Where a period ('.') character starts a (crlf-delimited)
-- line, replace it with two periods.
escapePeriods :: Text -> Text
escapePeriods txt =
    T.concat $ map escapeLine $ T.splitOn M.crlf txt
  where
    escapeLine t =
      if "." `T.isPrefixOf` t
      then "." <> t
      else t

-- | see 'ABS.sendRawMail'. 
sendRawMail ::
  (MonadSmtp m, Foldable t) => Text -> t Text -> Text -> m ()
sendRawMail sender recipients message =
  let sender' = TE.encodeUtf8 sender
      recipients' = map TE.encodeUtf8 (toList recipients)
      message'    = TE.encodeUtf8 message
  in  ABS.sendRawMail sender' recipients' message'

