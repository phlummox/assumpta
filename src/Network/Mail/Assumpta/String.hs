
{- |

'String' versions of Smtp operations.

This module does not validate that your strings
satisfy the requirements of the relevant RFCs -- see the note in
"Network.Mail.Assumpta.MonadSmtp#permissiblecharacters"
about "permissible characters". In general, unless you are using
an SMTP extension, your strings must consist of 7-bit clean ASCII.

-}

module Network.Mail.Assumpta.String
  (
    -- * Run Smtp actions
    -- | Run actions in the 'MonadSmtp' monad.
    AT.runSmtp
  , AT.runSecureSmtp
  , AT.runSmtpWithParams
  , AT.runSmtpHandle
    -- * MonadError variants
    -- | Instead of returning an Either, these run in MonadError --
    -- thus the caller can specialize them to a 'Maybe', 'Either',
    -- or some other 'MonadError' instance as desired.
  , AT.runSmtp'
  , AT.runSecureSmtp'
  , AT.runSmtpWithParams'
  , AT.runSmtpHandle'
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
  , AT.noop
  , AT.quit
  , AT.rset
  , AT.startTLS
  , expn
  , vrfy
  , help
  , sendRawMail
    -- * Server responses
  , AT.expect
  , AT.expectGreeting
    -- * Low-level operations and types
  , AT.command
  , AT.Reply
  , AT.ReplyLine(..)
  , AT.ReplyCode
  , AT.SmtpError(..)
    -- * Monad transformer
    -- | A monad transformer, 'SmtpT', which provides the ability to
    -- send SMTP commands and parse replies, together with operations
    -- on the transformer.
  , AT.SmtpT
  , AT.Smtp
  , AT.liftSmtpT
  , AT.mapSmtpT
    -- * Network operations
  , AT.open
  , AT.openTls
  , AT.openParams
  , AT.close
  , AT.withHandle
  , AT.withSecureHandle
  , AT.withHandleParams
  , AT.HostName
  , AT.Port
  , AT.Handle(..)
  , AT.defaultTLSSettings
  , send
  , sendLine
    -- * Utility functions
  , toCrLf
  , AT.rethrow
  , AT.mkParams
  , AT.toIOError
  , AT.crlf
  , escapePeriods
  )
  where

import           Data.Foldable (toList)
import           Data.Monoid -- needed for early versions of Base
import qualified Data.Text as T

import qualified Network.Mail.Assumpta.ByteString as ABS
import qualified Network.Mail.Assumpta.Text as AT

import           Network.Mail.Assumpta.MonadSmtp ( MonadSmtp )
import qualified Network.Mail.Assumpta.MonadSmtp as M

-- | See 'M.helo'
helo :: MonadSmtp m => String -> m () 
helo =  AT.helo . T.pack

-- | See 'M.ehlo'
ehlo :: MonadSmtp m => String -> m () 
ehlo =  AT.ehlo . T.pack

-- | See 'M.mailFrom'
mailFrom :: MonadSmtp m => String -> m ()
mailFrom =  AT.mailFrom . T.pack

-- | See 'M.rcptTo'
rcptTo :: MonadSmtp m => String -> m ()
rcptTo =  AT.rcptTo . T.pack

-- | See 'M.data_'
data_ :: MonadSmtp m => String -> m ()
data_ =  AT.data_ . T.pack

-- | See 'M.expn'
expn :: MonadSmtp m => String -> m AT.Reply
expn =  AT.expn . T.pack

-- | See 'M.vrfy'
vrfy :: MonadSmtp m => String -> m AT.Reply
vrfy =  AT.vrfy . T.pack

help :: MonadSmtp m => Maybe String -> m AT.Reply
help = AT.help . fmap T.pack

-- | See 'M.send'
send :: MonadSmtp m => String -> m ()
send =  AT.send . T.pack

-- | See 'M.sendLine'
sendLine :: MonadSmtp m => String -> m ()
sendLine =  AT.sendLine . T.pack

-- | replace newlines (@\'\\n'@) with crlf sequence (@\'\\r\\n'@).
toCrLf :: String -> String
toCrLf = T.unpack . AT.toCrLf . T.pack

-- | Where a period ('.') character starts a (crlf-delimited)
-- line, replace it with two periods.
escapePeriods :: String -> String
escapePeriods = T.unpack . AT.escapePeriods . T.pack

-- | see 'ABS.sendRawMail'. 
sendRawMail ::
  (MonadSmtp m, Foldable t) => String -> t String -> String -> m ()
sendRawMail sender recipients message =
  let sender' = T.pack sender
      recipients' = map T.pack (toList recipients)
      message'    = T.pack message
  in  AT.sendRawMail sender' recipients' message'


