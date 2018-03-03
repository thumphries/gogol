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
-- Module      : Network.Google.Resource.Logging.Exclusions.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.exclusions.create@.
module Network.Google.Resource.Logging.Exclusions.Create
    (
    -- * REST Resource
      ExclusionsCreateResource

    -- * Creating a Request
    , exclusionsCreate
    , ExclusionsCreate

    -- * Request Lenses
    , ecParent
    , ecXgafv
    , ecUploadProtocol
    , ecPp
    , ecAccessToken
    , ecUploadType
    , ecPayload
    , ecBearerToken
    , ecCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.exclusions.create@ method which the
-- 'ExclusionsCreate' request conforms to.
type ExclusionsCreateResource =
     "v2" :>
       Capture "parent" Text :>
         "exclusions" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] LogExclusion :>
                             Post '[JSON] LogExclusion

-- | Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ 'exclusionsCreate' smart constructor.
data ExclusionsCreate = ExclusionsCreate'
    { _ecParent :: !Text
    , _ecXgafv :: !(Maybe Xgafv)
    , _ecUploadProtocol :: !(Maybe Text)
    , _ecPp :: !Bool
    , _ecAccessToken :: !(Maybe Text)
    , _ecUploadType :: !(Maybe Text)
    , _ecPayload :: !LogExclusion
    , _ecBearerToken :: !(Maybe Text)
    , _ecCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExclusionsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecParent'
--
-- * 'ecXgafv'
--
-- * 'ecUploadProtocol'
--
-- * 'ecPp'
--
-- * 'ecAccessToken'
--
-- * 'ecUploadType'
--
-- * 'ecPayload'
--
-- * 'ecBearerToken'
--
-- * 'ecCallback'
exclusionsCreate
    :: Text -- ^ 'ecParent'
    -> LogExclusion -- ^ 'ecPayload'
    -> ExclusionsCreate
exclusionsCreate pEcParent_ pEcPayload_ = 
    ExclusionsCreate'
    { _ecParent = pEcParent_
    , _ecXgafv = Nothing
    , _ecUploadProtocol = Nothing
    , _ecPp = True
    , _ecAccessToken = Nothing
    , _ecUploadType = Nothing
    , _ecPayload = pEcPayload_
    , _ecBearerToken = Nothing
    , _ecCallback = Nothing
    }

-- | Required. The parent resource in which to create the exclusion:
-- \"projects\/[PROJECT_ID]\" \"organizations\/[ORGANIZATION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\" \"folders\/[FOLDER_ID]\"
-- Examples: \"projects\/my-logging-project\",
-- \"organizations\/123456789\".
ecParent :: Lens' ExclusionsCreate Text
ecParent = lens _ecParent (\ s a -> s{_ecParent = a})

-- | V1 error format.
ecXgafv :: Lens' ExclusionsCreate (Maybe Xgafv)
ecXgafv = lens _ecXgafv (\ s a -> s{_ecXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ecUploadProtocol :: Lens' ExclusionsCreate (Maybe Text)
ecUploadProtocol
  = lens _ecUploadProtocol
      (\ s a -> s{_ecUploadProtocol = a})

-- | Pretty-print response.
ecPp :: Lens' ExclusionsCreate Bool
ecPp = lens _ecPp (\ s a -> s{_ecPp = a})

-- | OAuth access token.
ecAccessToken :: Lens' ExclusionsCreate (Maybe Text)
ecAccessToken
  = lens _ecAccessToken
      (\ s a -> s{_ecAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ecUploadType :: Lens' ExclusionsCreate (Maybe Text)
ecUploadType
  = lens _ecUploadType (\ s a -> s{_ecUploadType = a})

-- | Multipart request metadata.
ecPayload :: Lens' ExclusionsCreate LogExclusion
ecPayload
  = lens _ecPayload (\ s a -> s{_ecPayload = a})

-- | OAuth bearer token.
ecBearerToken :: Lens' ExclusionsCreate (Maybe Text)
ecBearerToken
  = lens _ecBearerToken
      (\ s a -> s{_ecBearerToken = a})

-- | JSONP
ecCallback :: Lens' ExclusionsCreate (Maybe Text)
ecCallback
  = lens _ecCallback (\ s a -> s{_ecCallback = a})

instance GoogleRequest ExclusionsCreate where
        type Rs ExclusionsCreate = LogExclusion
        type Scopes ExclusionsCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient ExclusionsCreate'{..}
          = go _ecParent _ecXgafv _ecUploadProtocol
              (Just _ecPp)
              _ecAccessToken
              _ecUploadType
              _ecBearerToken
              _ecCallback
              (Just AltJSON)
              _ecPayload
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy ExclusionsCreateResource)
                      mempty
