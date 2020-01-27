
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Network operations using
the <https://hackage.haskell.org/package/connection connection>
package.

-}

module Network.Mail.Assumpta.Net

  where

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class
import           Data.Default (def)
import qualified Network.Connection as NC

import Network.Mail.Assumpta.Connection as C

-- --
-- Network types and ops

type Port = Int
type HostName = String

-- | Network handle, containing enough
-- information to both communicate a connection,
-- and upgrade to TLS.
data Handle = Handle {
    hConn     :: !NC.Connection
  , hContext  :: !NC.ConnectionContext
  }

-- Implementation of 'Connection' using
-- the <https://hackage.haskell.org/package/connection connection>
-- package.
instance Connection Handle where
  type Cstrt Handle = MonadIO
  type Params Handle = NC.ConnectionParams 

  open params = do ctx   <- liftIO NC.initConnectionContext
                   conn  <- liftIO $ NC.connectTo ctx params
                   return $ Handle conn ctx
  close       = liftIO . NC.connectionClose . hConn

  send h      = liftIO . NC.connectionPut (hConn h)
  recv        = liftIO . (`NC.connectionGet` 2048) . hConn

  upgrade h   = let conn = hConn h
                    ctx  = hContext h
                    tls  = defaultTLSSettings
                in liftIO $ NC.connectionSetSecure ctx conn tls

-- | default TLS settings
defaultTLSSettings :: NC.TLSSettings
defaultTLSSettings = def

mkParams ::
  HostName -> Int -> NC.ConnectionParams
mkParams hostname port =
  NC.ConnectionParams {
       NC.connectionHostname  = hostname
     , NC.connectionPort      = fromIntegral port
     , NC.connectionUseSecure = Nothing
     , NC.connectionUseSocks  = Nothing
     }

-- | Open a network 'Handle' with the specified 'NC.ConnectionParams'
openParams ::
  MonadIO m => NC.ConnectionParams -> m Handle
openParams =
  C.open

-- | Open a network 'Handle' to the specified hostname and port
open :: MonadIO m => HostName -> Port -> m Handle
open host port =
  let p = mkParams host port
  in openParams p


-- | Open a secure network 'Handle' to the specified hostname and port
-- using the default TLS settings ('defaultTLSSettings')
openTls :: MonadIO m => HostName -> Port -> m Handle
openTls host port =
  let p = (mkParams host port) { NC.connectionUseSecure = Just defaultTLSSettings }
  in openParams p

-- | Close a network 'Handle'
close :: MonadIO m => Handle -> m ()
close =
  C.close

-- | @withHandleParams p a@
--
-- Given some parameters @p@ (hostname, port etc) for opening a 'Handle': 
-- open a handle, run some action @a@ with it, then
-- close.
withHandleParams ::
  (MonadMask m, MonadIO m) => NC.ConnectionParams -> (Handle -> m b) -> m b
withHandleParams =
  C.withConnection

-- | @withHandle hostname port a@
--
-- Open a 'Handle' to the specified @hostname@ and @port@,
-- run some action @a@ with it, then
-- close.
withHandle ::
  (MonadMask m, MonadIO m) => HostName -> Port ->  (Handle -> m b) -> m b
withHandle host port =
  let p = mkParams host port
  in withHandleParams p

-- | @withSecureHandle hostname port a@
--
-- Open a secure 'Handle' to the specified @hostname@ and @port@,
-- run some action @a@ with it, then
-- close.
withSecureHandle ::
  (MonadMask m, MonadIO m) => HostName -> Port ->  (Handle -> m b) -> m b
withSecureHandle host port =
  let p = (mkParams host port) { 
              NC.connectionUseSecure = Just defaultTLSSettings
              }
  in withHandleParams p

