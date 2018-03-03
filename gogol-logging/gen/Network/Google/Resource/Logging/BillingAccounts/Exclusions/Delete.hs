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
-- Module      : Network.Google.Resource.Logging.BillingAccounts.Exclusions.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an exclusion.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.billingAccounts.exclusions.delete@.
module Network.Google.Resource.Logging.BillingAccounts.Exclusions.Delete
    (
    -- * REST Resource
      BillingAccountsExclusionsDeleteResource

    -- * Creating a Request
    , billingAccountsExclusionsDelete
    , BillingAccountsExclusionsDelete

    -- * Request Lenses
    , baedXgafv
    , baedUploadProtocol
    , baedPp
    , baedAccessToken
    , baedUploadType
    , baedBearerToken
    , baedName
    , baedCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.billingAccounts.exclusions.delete@ method which the
-- 'BillingAccountsExclusionsDelete' request conforms to.
type BillingAccountsExclusionsDeleteResource =
     "v2" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Deletes an exclusion.
--
-- /See:/ 'billingAccountsExclusionsDelete' smart constructor.
data BillingAccountsExclusionsDelete = BillingAccountsExclusionsDelete'
    { _baedXgafv :: !(Maybe Xgafv)
    , _baedUploadProtocol :: !(Maybe Text)
    , _baedPp :: !Bool
    , _baedAccessToken :: !(Maybe Text)
    , _baedUploadType :: !(Maybe Text)
    , _baedBearerToken :: !(Maybe Text)
    , _baedName :: !Text
    , _baedCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BillingAccountsExclusionsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baedXgafv'
--
-- * 'baedUploadProtocol'
--
-- * 'baedPp'
--
-- * 'baedAccessToken'
--
-- * 'baedUploadType'
--
-- * 'baedBearerToken'
--
-- * 'baedName'
--
-- * 'baedCallback'
billingAccountsExclusionsDelete
    :: Text -- ^ 'baedName'
    -> BillingAccountsExclusionsDelete
billingAccountsExclusionsDelete pBaedName_ = 
    BillingAccountsExclusionsDelete'
    { _baedXgafv = Nothing
    , _baedUploadProtocol = Nothing
    , _baedPp = True
    , _baedAccessToken = Nothing
    , _baedUploadType = Nothing
    , _baedBearerToken = Nothing
    , _baedName = pBaedName_
    , _baedCallback = Nothing
    }

-- | V1 error format.
baedXgafv :: Lens' BillingAccountsExclusionsDelete (Maybe Xgafv)
baedXgafv
  = lens _baedXgafv (\ s a -> s{_baedXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
baedUploadProtocol :: Lens' BillingAccountsExclusionsDelete (Maybe Text)
baedUploadProtocol
  = lens _baedUploadProtocol
      (\ s a -> s{_baedUploadProtocol = a})

-- | Pretty-print response.
baedPp :: Lens' BillingAccountsExclusionsDelete Bool
baedPp = lens _baedPp (\ s a -> s{_baedPp = a})

-- | OAuth access token.
baedAccessToken :: Lens' BillingAccountsExclusionsDelete (Maybe Text)
baedAccessToken
  = lens _baedAccessToken
      (\ s a -> s{_baedAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
baedUploadType :: Lens' BillingAccountsExclusionsDelete (Maybe Text)
baedUploadType
  = lens _baedUploadType
      (\ s a -> s{_baedUploadType = a})

-- | OAuth bearer token.
baedBearerToken :: Lens' BillingAccountsExclusionsDelete (Maybe Text)
baedBearerToken
  = lens _baedBearerToken
      (\ s a -> s{_baedBearerToken = a})

-- | Required. The resource name of an existing exclusion to delete:
-- \"projects\/[PROJECT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"folders\/[FOLDER_ID]\/exclusions\/[EXCLUSION_ID]\" Example:
-- \"projects\/my-project-id\/exclusions\/my-exclusion-id\".
baedName :: Lens' BillingAccountsExclusionsDelete Text
baedName = lens _baedName (\ s a -> s{_baedName = a})

-- | JSONP
baedCallback :: Lens' BillingAccountsExclusionsDelete (Maybe Text)
baedCallback
  = lens _baedCallback (\ s a -> s{_baedCallback = a})

instance GoogleRequest
         BillingAccountsExclusionsDelete where
        type Rs BillingAccountsExclusionsDelete = Empty
        type Scopes BillingAccountsExclusionsDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient BillingAccountsExclusionsDelete'{..}
          = go _baedName _baedXgafv _baedUploadProtocol
              (Just _baedPp)
              _baedAccessToken
              _baedUploadType
              _baedBearerToken
              _baedCallback
              (Just AltJSON)
              loggingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy BillingAccountsExclusionsDeleteResource)
                      mempty
