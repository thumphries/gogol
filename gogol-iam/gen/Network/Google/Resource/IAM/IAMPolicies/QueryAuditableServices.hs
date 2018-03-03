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
-- Module      : Network.Google.Resource.IAM.IAMPolicies.QueryAuditableServices
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of services that support service level audit logging
-- configuration for the given resource.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.iamPolicies.queryAuditableServices@.
module Network.Google.Resource.IAM.IAMPolicies.QueryAuditableServices
    (
    -- * REST Resource
      IAMPoliciesQueryAuditableServicesResource

    -- * Creating a Request
    , iamPoliciesQueryAuditableServices
    , IAMPoliciesQueryAuditableServices

    -- * Request Lenses
    , ipqasXgafv
    , ipqasUploadProtocol
    , ipqasPp
    , ipqasAccessToken
    , ipqasUploadType
    , ipqasPayload
    , ipqasBearerToken
    , ipqasCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.iamPolicies.queryAuditableServices@ method which the
-- 'IAMPoliciesQueryAuditableServices' request conforms to.
type IAMPoliciesQueryAuditableServicesResource =
     "v1" :>
       "iamPolicies:queryAuditableServices" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] QueryAuditableServicesRequest :>
                           Post '[JSON] QueryAuditableServicesResponse

-- | Returns a list of services that support service level audit logging
-- configuration for the given resource.
--
-- /See:/ 'iamPoliciesQueryAuditableServices' smart constructor.
data IAMPoliciesQueryAuditableServices = IAMPoliciesQueryAuditableServices'
    { _ipqasXgafv :: !(Maybe Xgafv)
    , _ipqasUploadProtocol :: !(Maybe Text)
    , _ipqasPp :: !Bool
    , _ipqasAccessToken :: !(Maybe Text)
    , _ipqasUploadType :: !(Maybe Text)
    , _ipqasPayload :: !QueryAuditableServicesRequest
    , _ipqasBearerToken :: !(Maybe Text)
    , _ipqasCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'IAMPoliciesQueryAuditableServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipqasXgafv'
--
-- * 'ipqasUploadProtocol'
--
-- * 'ipqasPp'
--
-- * 'ipqasAccessToken'
--
-- * 'ipqasUploadType'
--
-- * 'ipqasPayload'
--
-- * 'ipqasBearerToken'
--
-- * 'ipqasCallback'
iamPoliciesQueryAuditableServices
    :: QueryAuditableServicesRequest -- ^ 'ipqasPayload'
    -> IAMPoliciesQueryAuditableServices
iamPoliciesQueryAuditableServices pIpqasPayload_ = 
    IAMPoliciesQueryAuditableServices'
    { _ipqasXgafv = Nothing
    , _ipqasUploadProtocol = Nothing
    , _ipqasPp = True
    , _ipqasAccessToken = Nothing
    , _ipqasUploadType = Nothing
    , _ipqasPayload = pIpqasPayload_
    , _ipqasBearerToken = Nothing
    , _ipqasCallback = Nothing
    }

-- | V1 error format.
ipqasXgafv :: Lens' IAMPoliciesQueryAuditableServices (Maybe Xgafv)
ipqasXgafv
  = lens _ipqasXgafv (\ s a -> s{_ipqasXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ipqasUploadProtocol :: Lens' IAMPoliciesQueryAuditableServices (Maybe Text)
ipqasUploadProtocol
  = lens _ipqasUploadProtocol
      (\ s a -> s{_ipqasUploadProtocol = a})

-- | Pretty-print response.
ipqasPp :: Lens' IAMPoliciesQueryAuditableServices Bool
ipqasPp = lens _ipqasPp (\ s a -> s{_ipqasPp = a})

-- | OAuth access token.
ipqasAccessToken :: Lens' IAMPoliciesQueryAuditableServices (Maybe Text)
ipqasAccessToken
  = lens _ipqasAccessToken
      (\ s a -> s{_ipqasAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ipqasUploadType :: Lens' IAMPoliciesQueryAuditableServices (Maybe Text)
ipqasUploadType
  = lens _ipqasUploadType
      (\ s a -> s{_ipqasUploadType = a})

-- | Multipart request metadata.
ipqasPayload :: Lens' IAMPoliciesQueryAuditableServices QueryAuditableServicesRequest
ipqasPayload
  = lens _ipqasPayload (\ s a -> s{_ipqasPayload = a})

-- | OAuth bearer token.
ipqasBearerToken :: Lens' IAMPoliciesQueryAuditableServices (Maybe Text)
ipqasBearerToken
  = lens _ipqasBearerToken
      (\ s a -> s{_ipqasBearerToken = a})

-- | JSONP
ipqasCallback :: Lens' IAMPoliciesQueryAuditableServices (Maybe Text)
ipqasCallback
  = lens _ipqasCallback
      (\ s a -> s{_ipqasCallback = a})

instance GoogleRequest
         IAMPoliciesQueryAuditableServices where
        type Rs IAMPoliciesQueryAuditableServices =
             QueryAuditableServicesResponse
        type Scopes IAMPoliciesQueryAuditableServices =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient IAMPoliciesQueryAuditableServices'{..}
          = go _ipqasXgafv _ipqasUploadProtocol (Just _ipqasPp)
              _ipqasAccessToken
              _ipqasUploadType
              _ipqasBearerToken
              _ipqasCallback
              (Just AltJSON)
              _ipqasPayload
              iAMService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy IAMPoliciesQueryAuditableServicesResource)
                      mempty
