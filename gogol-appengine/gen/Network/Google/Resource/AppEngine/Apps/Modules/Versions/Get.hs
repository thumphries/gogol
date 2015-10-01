{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Resource.AppEngine.Apps.Modules.Versions.Get
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- | Gets application deployment information.
--
-- /See:/ <https://developers.google.com/appengine/ Google App Engine Admin API Reference> for @AppengineAppsModulesVersionsGet@.
module Network.Google.Resource.AppEngine.Apps.Modules.Versions.Get
    (
    -- * REST Resource
      AppsModulesVersionsGetResource

    -- * Creating a Request
    , appsModulesVersionsGet'
    , AppsModulesVersionsGet'

    -- * Request Lenses
    , amvgXgafv
    , amvgQuotaUser
    , amvgPrettyPrint
    , amvgUploadProtocol
    , amvgPp
    , amvgAccessToken
    , amvgUploadType
    , amvgVersionsId
    , amvgModulesId
    , amvgBearerToken
    , amvgKey
    , amvgAppsId
    , amvgView
    , amvgOauthToken
    , amvgFields
    , amvgCallback
    , amvgAlt
    ) where

import           Network.Google.AppEngine.Types
import           Network.Google.Prelude

-- | A resource alias for @AppengineAppsModulesVersionsGet@ which the
-- 'AppsModulesVersionsGet'' request conforms to.
type AppsModulesVersionsGetResource =
     "v1beta4" :>
       "apps" :>
         Capture "appsId" Text :>
           "modules" :>
             Capture "modulesId" Text :>
               "versions" :>
                 Capture "versionsId" Text :>
                   QueryParam "$.xgafv" Text :>
                     QueryParam "quotaUser" Text :>
                       QueryParam "prettyPrint" Bool :>
                         QueryParam "upload_protocol" Text :>
                           QueryParam "pp" Bool :>
                             QueryParam "access_token" Text :>
                               QueryParam "uploadType" Text :>
                                 QueryParam "bearer_token" Text :>
                                   QueryParam "key" Text :>
                                     QueryParam "view" Text :>
                                       QueryParam "oauth_token" Text :>
                                         QueryParam "fields" Text :>
                                           QueryParam "callback" Text :>
                                             QueryParam "alt" Text :>
                                               Get '[JSON] Version

-- | Gets application deployment information.
--
-- /See:/ 'appsModulesVersionsGet'' smart constructor.
data AppsModulesVersionsGet' = AppsModulesVersionsGet'
    { _amvgXgafv          :: !(Maybe Text)
    , _amvgQuotaUser      :: !(Maybe Text)
    , _amvgPrettyPrint    :: !Bool
    , _amvgUploadProtocol :: !(Maybe Text)
    , _amvgPp             :: !Bool
    , _amvgAccessToken    :: !(Maybe Text)
    , _amvgUploadType     :: !(Maybe Text)
    , _amvgVersionsId     :: !Text
    , _amvgModulesId      :: !Text
    , _amvgBearerToken    :: !(Maybe Text)
    , _amvgKey            :: !(Maybe Text)
    , _amvgAppsId         :: !Text
    , _amvgView           :: !(Maybe Text)
    , _amvgOauthToken     :: !(Maybe Text)
    , _amvgFields         :: !(Maybe Text)
    , _amvgCallback       :: !(Maybe Text)
    , _amvgAlt            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsModulesVersionsGet'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amvgXgafv'
--
-- * 'amvgQuotaUser'
--
-- * 'amvgPrettyPrint'
--
-- * 'amvgUploadProtocol'
--
-- * 'amvgPp'
--
-- * 'amvgAccessToken'
--
-- * 'amvgUploadType'
--
-- * 'amvgVersionsId'
--
-- * 'amvgModulesId'
--
-- * 'amvgBearerToken'
--
-- * 'amvgKey'
--
-- * 'amvgAppsId'
--
-- * 'amvgView'
--
-- * 'amvgOauthToken'
--
-- * 'amvgFields'
--
-- * 'amvgCallback'
--
-- * 'amvgAlt'
appsModulesVersionsGet'
    :: Text -- ^ 'versionsId'
    -> Text -- ^ 'modulesId'
    -> Text -- ^ 'appsId'
    -> AppsModulesVersionsGet'
appsModulesVersionsGet' pAmvgVersionsId_ pAmvgModulesId_ pAmvgAppsId_ =
    AppsModulesVersionsGet'
    { _amvgXgafv = Nothing
    , _amvgQuotaUser = Nothing
    , _amvgPrettyPrint = True
    , _amvgUploadProtocol = Nothing
    , _amvgPp = True
    , _amvgAccessToken = Nothing
    , _amvgUploadType = Nothing
    , _amvgVersionsId = pAmvgVersionsId_
    , _amvgModulesId = pAmvgModulesId_
    , _amvgBearerToken = Nothing
    , _amvgKey = Nothing
    , _amvgAppsId = pAmvgAppsId_
    , _amvgView = Nothing
    , _amvgOauthToken = Nothing
    , _amvgFields = Nothing
    , _amvgCallback = Nothing
    , _amvgAlt = "json"
    }

-- | V1 error format.
amvgXgafv :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgXgafv
  = lens _amvgXgafv (\ s a -> s{_amvgXgafv = a})

-- | Available to use for quota purposes for server-side applications. Can be
-- any arbitrary string assigned to a user, but should not exceed 40
-- characters.
amvgQuotaUser :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgQuotaUser
  = lens _amvgQuotaUser
      (\ s a -> s{_amvgQuotaUser = a})

-- | Returns response with indentations and line breaks.
amvgPrettyPrint :: Lens' AppsModulesVersionsGet' Bool
amvgPrettyPrint
  = lens _amvgPrettyPrint
      (\ s a -> s{_amvgPrettyPrint = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
amvgUploadProtocol :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgUploadProtocol
  = lens _amvgUploadProtocol
      (\ s a -> s{_amvgUploadProtocol = a})

-- | Pretty-print response.
amvgPp :: Lens' AppsModulesVersionsGet' Bool
amvgPp = lens _amvgPp (\ s a -> s{_amvgPp = a})

-- | OAuth access token.
amvgAccessToken :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgAccessToken
  = lens _amvgAccessToken
      (\ s a -> s{_amvgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
amvgUploadType :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgUploadType
  = lens _amvgUploadType
      (\ s a -> s{_amvgUploadType = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
amvgVersionsId :: Lens' AppsModulesVersionsGet' Text
amvgVersionsId
  = lens _amvgVersionsId
      (\ s a -> s{_amvgVersionsId = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
amvgModulesId :: Lens' AppsModulesVersionsGet' Text
amvgModulesId
  = lens _amvgModulesId
      (\ s a -> s{_amvgModulesId = a})

-- | OAuth bearer token.
amvgBearerToken :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgBearerToken
  = lens _amvgBearerToken
      (\ s a -> s{_amvgBearerToken = a})

-- | API key. Your API key identifies your project and provides you with API
-- access, quota, and reports. Required unless you provide an OAuth 2.0
-- token.
amvgKey :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgKey = lens _amvgKey (\ s a -> s{_amvgKey = a})

-- | Part of \`name\`. Name of the resource requested. For example:
-- \"apps\/myapp\/modules\/default\/versions\/v1\".
amvgAppsId :: Lens' AppsModulesVersionsGet' Text
amvgAppsId
  = lens _amvgAppsId (\ s a -> s{_amvgAppsId = a})

-- | Controls the set of fields returned in the \`Get\` response.
amvgView :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgView = lens _amvgView (\ s a -> s{_amvgView = a})

-- | OAuth 2.0 token for the current user.
amvgOauthToken :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgOauthToken
  = lens _amvgOauthToken
      (\ s a -> s{_amvgOauthToken = a})

-- | Selector specifying which fields to include in a partial response.
amvgFields :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgFields
  = lens _amvgFields (\ s a -> s{_amvgFields = a})

-- | JSONP
amvgCallback :: Lens' AppsModulesVersionsGet' (Maybe Text)
amvgCallback
  = lens _amvgCallback (\ s a -> s{_amvgCallback = a})

-- | Data format for response.
amvgAlt :: Lens' AppsModulesVersionsGet' Text
amvgAlt = lens _amvgAlt (\ s a -> s{_amvgAlt = a})

instance GoogleRequest AppsModulesVersionsGet' where
        type Rs AppsModulesVersionsGet' = Version
        request = requestWithRoute defReq appEngineURL
        requestWithRoute r u AppsModulesVersionsGet'{..}
          = go _amvgXgafv _amvgQuotaUser
              (Just _amvgPrettyPrint)
              _amvgUploadProtocol
              (Just _amvgPp)
              _amvgAccessToken
              _amvgUploadType
              _amvgVersionsId
              _amvgModulesId
              _amvgBearerToken
              _amvgKey
              _amvgAppsId
              _amvgView
              _amvgOauthToken
              _amvgFields
              _amvgCallback
              (Just _amvgAlt)
          where go
                  = clientWithRoute
                      (Proxy :: Proxy AppsModulesVersionsGetResource)
                      r
                      u