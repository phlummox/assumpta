
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Communicate with SMTP servers using raw 'ByteString's, using
the infrastructure of the
<https://hackage.haskell.org/package/connection connection>
package.

This module does not validate that your bytestrings
satisfy the requirements of the relevant RFCs -- see the note in
"Network.Mail.Assumpta.MonadSmtp#permissiblecharacters"
about "permissible characters". In general, unless you are using
an SMTP extension, your bytestrings must be 7-bit clean ASCII.

== Example code

Short example (which will work if you have an SMTP server
running on local port 2025):
(see the
<https://hackage.haskell.org/package/assumpta/src/examples/ examples>
directory for a copy of the code):

> {-# LANGUAGE OverloadedStrings #-}
> 
> -- Program that runs a short SMTP session
> -- with a server running on local port 2025.
> 
> module Main where
> 
> import           Data.Monoid
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import Network.BSD (getHostName) -- requires 'network' package
> import Network.Mail.Assumpta.ByteString as M
> 
> sampleMesg :: BS.ByteString
> sampleMesg = BS.intercalate crlf [
>     "Date: Tue, 21 Jan 2020 02:28:37 +0800"
>   , "To: neddie.seagoon@gmail.com"
>   , "From: jim.moriarty@gmail.com"
>   , "Subject: test Tue, 21 Jan 2020 02:28:37 +0800"
>   , ""
>   , "Sapristi nyuckoes!"
>   ]
> 
> -- The sender and recipient supplied to the
> -- SMTP server are what is used to route the
> -- message; any 'To:' and 'From:' fields
> -- in the message are completely ignored for this
> -- purpose.
> sender, recipient :: BS.ByteString
> sender = "somesender@mycorp.com"
> recipient = "somerecipient@mozilla.org"
> 
> main :: IO ()
> main = do
>   let toBinary = TE.encodeUtf8 . T.pack 
>       port = 2025
>   hostname <- getHostName
>   print =<< runSmtp "localhost" port (do
>           expectGreeting
>           ehlo $ toBinary hostname
>           -- Properly, we should escape periods at the start of a
>           -- line. But we know there aren't any
>           sendRawMail sender [recipient] sampleMesg
>           quit)

== Alternatives to "connection"

If you want to use other network libraries besides
<https://hackage.haskell.org/package/connection connection>,
it should be pretty straightforward to adapt the code here.

If you want to use stream-like IO, one possibility is
the <https://hackage.haskell.org/package/conduit-connection  conduit-connection> package,
which provides Conduit-based sources and sinks on top of "Network.Connection".


-}

module Network.Mail.Assumpta.ByteString
  (
    -- * Run Smtp actions
    -- | Run actions in the 'MonadSmtp' monad.
    runSmtp
  , runSecureSmtp
  , runSmtpWithParams
  , runSmtpHandle
    -- * MonadError variants
    -- | Instead of returning an Either, these run in MonadError --
    -- thus the caller can specialize them to a 'Maybe', 'Either',
    -- or some other 'MonadError' instance as desired.
  , runSmtp'
  , runSecureSmtp'
  , runSmtpWithParams'
  , runSmtpHandle'
    -- * SMTP commands
    -- | Functions for sending commands
    -- to an SMTP server. In general, these are wrappers around
    -- 'command' and 'expect' -- they send some command,
    -- and try to parse a response (throwing an 'SmtpError'
    -- on failure).
  , helo
  , ehlo
  , mailFrom
  , rcptTo
  , data_
  , noop
  , quit
  , rset
  , startTLS
  , expn
  , vrfy
  , help
  , Smtp.sendRawMail
    -- * Server responses
  , expect
  , expectGreeting
    -- * Low-level operations and types
  , sendLine
  , command
  , Reply
  , ReplyLine(..)
  , ReplyCode
  , SmtpError(..)
    -- * Monad transformer
    -- | A monad transformer, 'SmtpT', which provides the ability to
    -- send SMTP commands and parse replies, together with operations
    -- on the transformer.
  , SmtpT
  , Smtp
  , liftSmtpT
  , mapSmtpT
    -- * Network operations
    -- | Network operations based around 'Handle',
    -- using the
    -- <https://hackage.haskell.org/package/connection connection>
    -- package.
  , open
  , openTls
  , openParams
  , close
  , withHandle
  , withSecureHandle
  , withHandleParams
  , HostName
  , Port
  , Handle(..)
  , defaultTLSSettings
    -- * Utility functions
  , rethrow
  , mkParams
  , toIOError
  , crlf
  )
  where

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Except
import qualified Network.Connection as C

import           Network.Mail.Assumpta.Internal.Net
import           Network.Mail.Assumpta.MonadSmtp as Smtp
import qualified Network.Mail.Assumpta.Trans.Smtp as T
import           Network.Mail.Assumpta.Trans.Smtp hiding ( SmtpT(..)
                                                          , runSmtpEither
                                                          , rethrow
                                                          , runSmtp, open
                                                          , close )


-- --
-- Specialization of monad transformer to Handle

-- | Smtp monad transformer. A specialization to 'Handle'
-- of the more general SmtpT type in the
-- <http://hackage.haskell.org/package/assumpta-core assumpta-core>
-- package.
type SmtpT = T.SmtpT Handle

-- | Smtp actions in the IO monad.
type Smtp = SmtpT IO

-- base funcs

-- | @runSmtpHandle h a@
-- 
-- Run some 'Smtp' action @a@ on a 'Handle' @h@,
-- and return the result as an 'Either'.
runSmtpHandle ::
  Handle -> Smtp a -> IO (Either SmtpError a)
runSmtpHandle = T.runSmtpEither

-- | @runSmtpHandle h a@
-- 
-- Run some 'Smtp' action @a@ on a 'Handle' @h@,
-- and return the result as a 'MonadError'.
--
-- Like 'runSmtpHandle' but generalized to 'MonadError'.
runSmtpHandle' ::
  (MonadError SmtpError m, MonadIO m) =>
      Handle -> SmtpT m a -> m a
runSmtpHandle' = T.runSmtp




-- | @runSmtp hostname port a@
--
-- Open a connection to the specified @hostname@ and @port@,
-- run some 'Smtp' action @a@ with it, then
-- close, returning the result as an 'Either'.
runSmtp ::
  HostName -> Int -> Smtp a -> IO (Either SmtpError a)
runSmtp host port a =
  withHandle host port (`runSmtpHandle` a)

-- | @runSmtp' host port a@
--
-- Like 'runSmtp', but generalized to 'MonadError'.
runSmtp' ::
  (MonadMask m, MonadIO m, MonadError SmtpError m) =>
  HostName -> Port -> SmtpT m b -> m b
runSmtp' host port a =
  withHandle host port (`runSmtpHandle'` a)

-- | @runSecureSmtp hostname port a@
--
-- Open a secure TLS connection to the specified @hostname@ and @port@,
-- run some 'Smtp' action @a@ with it, then
-- close, returning the result as an 'Either'.
--
-- Uses the default TLS settings, 'defaultTLSSettings'. For
-- more control, use 'runSmtpWithParams'.
runSecureSmtp ::
  HostName -> Int -> Smtp a -> IO (Either SmtpError a)
runSecureSmtp host port a = 
  let params = (mkParams host port) {
                        C.connectionUseSecure = Just defaultTLSSettings
                      }
  in runSmtpWithParams params a

-- | @runSecureSmtp' host port a@
--
-- Like 'runSecureSmtp', but generalized to 'MonadError'.
runSecureSmtp' ::
  (MonadMask m, MonadIO m, MonadError SmtpError m) =>
  HostName -> Int -> SmtpT m b -> m b
runSecureSmtp' host port a = 
  let params = (mkParams host port) {
                        C.connectionUseSecure = Just defaultTLSSettings
                      }
  in runSmtpWithParams' params a


-- | @runSmtpWithParams params a@
--
-- Like 'runSmtp', but providing more control --
-- the actions are run using the specified connection 
-- parameters (hostname, port, TLS settings, etc.).
runSmtpWithParams ::
  C.ConnectionParams -> Smtp a -> IO (Either SmtpError a)
runSmtpWithParams p a =
  withHandleParams p (`runSmtpHandle` a)


-- | @runSmtpWithParams' params a@
--
-- Like 'runSmtpWithParams', but generalized to 'MonadError'.
runSmtpWithParams' ::
  (MonadMask m, MonadIO m, MonadError SmtpError m) =>
  C.ConnectionParams -> SmtpT m b -> m b
runSmtpWithParams' p a =
  withHandleParams p (`runSmtpHandle'` a)

-- -- | @runSmtpT h a@
-- 
-- Given some 'Handle' @h@ and an 'SmtpT' action @a@,
-- run the action
-- and return the result as an 'Either'.
--runSmtpT' ::
--  MonadIO m =>
--    Handle -> (forall n . Ass.MonadSmtp n => n a) -> m (Either SmtpError a)
--runSmtpT' = T.runSmtpEither


-- | Convenience function: convert an 'SmtpError' into 
-- an 'IOError'.
toIOError :: SmtpError -> IOError
toIOError ex = userError $ "smtp operation error: " ++ show ex

-- | convenience function: re-throw an 'SmtpError' as an
-- exception in 'IO'.
rethrow :: Either SmtpError a -> IO a
rethrow =
  either (ioError . toIOError) return


