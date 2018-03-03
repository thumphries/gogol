{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.CloudDebugger.Controller.Debuggees.Register
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the debuggee with the controller service. All agents attached
-- to the same application must call this method with exactly the same
-- request content to get back the same stable \`debuggee_id\`. Agents
-- should call this method again whenever \`google.rpc.Code.NOT_FOUND\` is
-- returned from any controller method. This protocol allows the controller
-- service to disable debuggees, recover from data loss, or change the
-- \`debuggee_id\` format. Agents must handle \`debuggee_id\` value
-- changing upon re-registration.
--
-- /See:/ <http://cloud.google.com/debugger Stackdriver Debugger API Reference> for @clouddebugger.controller.debuggees.register@.
module Network.Google.Resource.CloudDebugger.Controller.Debuggees.Register
    (
    -- * REST Resource
      ControllerDebuggeesRegisterResource

    -- * Creating a Request
    , controllerDebuggeesRegister
    , ControllerDebuggeesRegister

    -- * Request Lenses
    , cdrXgafv
    , cdrUploadProtocol
    , cdrPp
    , cdrAccessToken
    , cdrUploadType
    , cdrPayload
    , cdrBearerToken
    , cdrCallback
    ) where

import Network.Google.Debugger.Types
import Network.Google.Prelude

-- | A resource alias for @clouddebugger.controller.debuggees.register@ method which the
-- 'ControllerDebuggeesRegister' request conforms to.
type ControllerDebuggeesRegisterResource =
     "v2" :>
       "controller" :>
         "debuggees" :>
           "register" :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] RegisterDebuggeeRequest :>
                               Post '[JSON] RegisterDebuggeeResponse

-- | Registers the debuggee with the controller service. All agents attached
-- to the same application must call this method with exactly the same
-- request content to get back the same stable \`debuggee_id\`. Agents
-- should call this method again whenever \`google.rpc.Code.NOT_FOUND\` is
-- returned from any controller method. This protocol allows the controller
-- service to disable debuggees, recover from data loss, or change the
-- \`debuggee_id\` format. Agents must handle \`debuggee_id\` value
-- changing upon re-registration.
--
-- /See:/ 'controllerDebuggeesRegister' smart constructor.
data ControllerDebuggeesRegister = ControllerDebuggeesRegister'
    { _cdrXgafv :: !(Maybe Xgafv)
    , _cdrUploadProtocol :: !(Maybe Text)
    , _cdrPp :: !Bool
    , _cdrAccessToken :: !(Maybe Text)
    , _cdrUploadType :: !(Maybe Text)
    , _cdrPayload :: !RegisterDebuggeeRequest
    , _cdrBearerToken :: !(Maybe Text)
    , _cdrCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ControllerDebuggeesRegister' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrXgafv'
--
-- * 'cdrUploadProtocol'
--
-- * 'cdrPp'
--
-- * 'cdrAccessToken'
--
-- * 'cdrUploadType'
--
-- * 'cdrPayload'
--
-- * 'cdrBearerToken'
--
-- * 'cdrCallback'
controllerDebuggeesRegister
    :: RegisterDebuggeeRequest -- ^ 'cdrPayload'
    -> ControllerDebuggeesRegister
controllerDebuggeesRegister pCdrPayload_ = 
    ControllerDebuggeesRegister'
    { _cdrXgafv = Nothing
    , _cdrUploadProtocol = Nothing
    , _cdrPp = True
    , _cdrAccessToken = Nothing
    , _cdrUploadType = Nothing
    , _cdrPayload = pCdrPayload_
    , _cdrBearerToken = Nothing
    , _cdrCallback = Nothing
    }

-- | V1 error format.
cdrXgafv :: Lens' ControllerDebuggeesRegister (Maybe Xgafv)
cdrXgafv = lens _cdrXgafv (\ s a -> s{_cdrXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
cdrUploadProtocol :: Lens' ControllerDebuggeesRegister (Maybe Text)
cdrUploadProtocol
  = lens _cdrUploadProtocol
      (\ s a -> s{_cdrUploadProtocol = a})

-- | Pretty-print response.
cdrPp :: Lens' ControllerDebuggeesRegister Bool
cdrPp = lens _cdrPp (\ s a -> s{_cdrPp = a})

-- | OAuth access token.
cdrAccessToken :: Lens' ControllerDebuggeesRegister (Maybe Text)
cdrAccessToken
  = lens _cdrAccessToken
      (\ s a -> s{_cdrAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
cdrUploadType :: Lens' ControllerDebuggeesRegister (Maybe Text)
cdrUploadType
  = lens _cdrUploadType
      (\ s a -> s{_cdrUploadType = a})

-- | Multipart request metadata.
cdrPayload :: Lens' ControllerDebuggeesRegister RegisterDebuggeeRequest
cdrPayload
  = lens _cdrPayload (\ s a -> s{_cdrPayload = a})

-- | OAuth bearer token.
cdrBearerToken :: Lens' ControllerDebuggeesRegister (Maybe Text)
cdrBearerToken
  = lens _cdrBearerToken
      (\ s a -> s{_cdrBearerToken = a})

-- | JSONP
cdrCallback :: Lens' ControllerDebuggeesRegister (Maybe Text)
cdrCallback
  = lens _cdrCallback (\ s a -> s{_cdrCallback = a})

instance GoogleRequest ControllerDebuggeesRegister
         where
        type Rs ControllerDebuggeesRegister =
             RegisterDebuggeeResponse
        type Scopes ControllerDebuggeesRegister =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud_debugger"]
        requestClient ControllerDebuggeesRegister'{..}
          = go _cdrXgafv _cdrUploadProtocol (Just _cdrPp)
              _cdrAccessToken
              _cdrUploadType
              _cdrBearerToken
              _cdrCallback
              (Just AltJSON)
              _cdrPayload
              debuggerService
          where go
                  = buildClient
                      (Proxy :: Proxy ControllerDebuggeesRegisterResource)
                      mempty
