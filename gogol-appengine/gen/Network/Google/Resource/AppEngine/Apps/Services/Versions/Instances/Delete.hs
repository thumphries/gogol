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
-- Module      : Network.Google.Resource.AppEngine.Apps.Services.Versions.Instances.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running instance.
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ App Engine Admin API Reference> for @appengine.apps.services.versions.instances.delete@.
module Network.Google.Resource.AppEngine.Apps.Services.Versions.Instances.Delete
    (
    -- * REST Resource
      AppsServicesVersionsInstancesDeleteResource

    -- * Creating a Request
    , appsServicesVersionsInstancesDelete
    , AppsServicesVersionsInstancesDelete

    -- * Request Lenses
    , aXgafv
    , aInstancesId
    , aUploadProtocol
    , aPp
    , aAccessToken
    , aUploadType
    , aVersionsId
    , aBearerToken
    , aAppsId
    , aServicesId
    , aCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.services.versions.instances.delete@ method which the
-- 'AppsServicesVersionsInstancesDelete' request conforms to.
type AppsServicesVersionsInstancesDeleteResource =
     "v1" :>
       "apps" :>
         Capture "appsId" Text :>
           "services" :>
             Capture "servicesId" Text :>
               "versions" :>
                 Capture "versionsId" Text :>
                   "instances" :>
                     Capture "instancesId" Text :>
                       QueryParam "$.xgafv" Xgafv :>
                         QueryParam "upload_protocol" Text :>
                           QueryParam "pp" Bool :>
                             QueryParam "access_token" Text :>
                               QueryParam "uploadType" Text :>
                                 QueryParam "bearer_token" Text :>
                                   QueryParam "callback" Text :>
                                     QueryParam "alt" AltJSON :>
                                       Delete '[JSON] Operation

-- | Stops a running instance.
--
-- /See:/ 'appsServicesVersionsInstancesDelete' smart constructor.
data AppsServicesVersionsInstancesDelete = AppsServicesVersionsInstancesDelete'
    { _aXgafv :: !(Maybe Xgafv)
    , _aInstancesId :: !Text
    , _aUploadProtocol :: !(Maybe Text)
    , _aPp :: !Bool
    , _aAccessToken :: !(Maybe Text)
    , _aUploadType :: !(Maybe Text)
    , _aVersionsId :: !Text
    , _aBearerToken :: !(Maybe Text)
    , _aAppsId :: !Text
    , _aServicesId :: !Text
    , _aCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsServicesVersionsInstancesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aXgafv'
--
-- * 'aInstancesId'
--
-- * 'aUploadProtocol'
--
-- * 'aPp'
--
-- * 'aAccessToken'
--
-- * 'aUploadType'
--
-- * 'aVersionsId'
--
-- * 'aBearerToken'
--
-- * 'aAppsId'
--
-- * 'aServicesId'
--
-- * 'aCallback'
appsServicesVersionsInstancesDelete
    :: Text -- ^ 'aInstancesId'
    -> Text -- ^ 'aVersionsId'
    -> Text -- ^ 'aAppsId'
    -> Text -- ^ 'aServicesId'
    -> AppsServicesVersionsInstancesDelete
appsServicesVersionsInstancesDelete pAInstancesId_ pAVersionsId_ pAAppsId_ pAServicesId_ = 
    AppsServicesVersionsInstancesDelete'
    { _aXgafv = Nothing
    , _aInstancesId = pAInstancesId_
    , _aUploadProtocol = Nothing
    , _aPp = True
    , _aAccessToken = Nothing
    , _aUploadType = Nothing
    , _aVersionsId = pAVersionsId_
    , _aBearerToken = Nothing
    , _aAppsId = pAAppsId_
    , _aServicesId = pAServicesId_
    , _aCallback = Nothing
    }

-- | V1 error format.
aXgafv :: Lens' AppsServicesVersionsInstancesDelete (Maybe Xgafv)
aXgafv = lens _aXgafv (\ s a -> s{_aXgafv = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
aInstancesId :: Lens' AppsServicesVersionsInstancesDelete Text
aInstancesId
  = lens _aInstancesId (\ s a -> s{_aInstancesId = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aUploadProtocol :: Lens' AppsServicesVersionsInstancesDelete (Maybe Text)
aUploadProtocol
  = lens _aUploadProtocol
      (\ s a -> s{_aUploadProtocol = a})

-- | Pretty-print response.
aPp :: Lens' AppsServicesVersionsInstancesDelete Bool
aPp = lens _aPp (\ s a -> s{_aPp = a})

-- | OAuth access token.
aAccessToken :: Lens' AppsServicesVersionsInstancesDelete (Maybe Text)
aAccessToken
  = lens _aAccessToken (\ s a -> s{_aAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aUploadType :: Lens' AppsServicesVersionsInstancesDelete (Maybe Text)
aUploadType
  = lens _aUploadType (\ s a -> s{_aUploadType = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
aVersionsId :: Lens' AppsServicesVersionsInstancesDelete Text
aVersionsId
  = lens _aVersionsId (\ s a -> s{_aVersionsId = a})

-- | OAuth bearer token.
aBearerToken :: Lens' AppsServicesVersionsInstancesDelete (Maybe Text)
aBearerToken
  = lens _aBearerToken (\ s a -> s{_aBearerToken = a})

-- | Part of \`name\`. Name of the resource requested. Example:
-- apps\/myapp\/services\/default\/versions\/v1\/instances\/instance-1.
aAppsId :: Lens' AppsServicesVersionsInstancesDelete Text
aAppsId = lens _aAppsId (\ s a -> s{_aAppsId = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
aServicesId :: Lens' AppsServicesVersionsInstancesDelete Text
aServicesId
  = lens _aServicesId (\ s a -> s{_aServicesId = a})

-- | JSONP
aCallback :: Lens' AppsServicesVersionsInstancesDelete (Maybe Text)
aCallback
  = lens _aCallback (\ s a -> s{_aCallback = a})

instance GoogleRequest
         AppsServicesVersionsInstancesDelete where
        type Rs AppsServicesVersionsInstancesDelete =
             Operation
        type Scopes AppsServicesVersionsInstancesDelete =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          AppsServicesVersionsInstancesDelete'{..}
          = go _aAppsId _aServicesId _aVersionsId _aInstancesId
              _aXgafv
              _aUploadProtocol
              (Just _aPp)
              _aAccessToken
              _aUploadType
              _aBearerToken
              _aCallback
              (Just AltJSON)
              appEngineService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AppsServicesVersionsInstancesDeleteResource)
                      mempty
