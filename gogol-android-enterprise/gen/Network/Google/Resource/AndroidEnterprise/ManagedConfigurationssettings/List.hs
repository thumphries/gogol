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
-- Module      : Network.Google.Resource.AndroidEnterprise.ManagedConfigurationssettings.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed configurations settings for the specified app.
-- Only the ID and the name is set.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.managedconfigurationssettings.list@.
module Network.Google.Resource.AndroidEnterprise.ManagedConfigurationssettings.List
    (
    -- * REST Resource
      ManagedConfigurationssettingsListResource

    -- * Creating a Request
    , managedConfigurationssettingsList
    , ManagedConfigurationssettingsList

    -- * Request Lenses
    , mclEnterpriseId
    , mclProductId
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.managedconfigurationssettings.list@ method which the
-- 'ManagedConfigurationssettingsList' request conforms to.
type ManagedConfigurationssettingsListResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "products" :>
               Capture "productId" Text :>
                 "managedConfigurationsSettings" :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] ManagedConfigurationsSettingsListResponse

-- | Lists all the managed configurations settings for the specified app.
-- Only the ID and the name is set.
--
-- /See:/ 'managedConfigurationssettingsList' smart constructor.
data ManagedConfigurationssettingsList = ManagedConfigurationssettingsList'
    { _mclEnterpriseId :: !Text
    , _mclProductId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ManagedConfigurationssettingsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mclEnterpriseId'
--
-- * 'mclProductId'
managedConfigurationssettingsList
    :: Text -- ^ 'mclEnterpriseId'
    -> Text -- ^ 'mclProductId'
    -> ManagedConfigurationssettingsList
managedConfigurationssettingsList pMclEnterpriseId_ pMclProductId_ = 
    ManagedConfigurationssettingsList'
    { _mclEnterpriseId = pMclEnterpriseId_
    , _mclProductId = pMclProductId_
    }

-- | The ID of the enterprise.
mclEnterpriseId :: Lens' ManagedConfigurationssettingsList Text
mclEnterpriseId
  = lens _mclEnterpriseId
      (\ s a -> s{_mclEnterpriseId = a})

-- | The ID of the product for which the managed configurations settings
-- applies to.
mclProductId :: Lens' ManagedConfigurationssettingsList Text
mclProductId
  = lens _mclProductId (\ s a -> s{_mclProductId = a})

instance GoogleRequest
         ManagedConfigurationssettingsList where
        type Rs ManagedConfigurationssettingsList =
             ManagedConfigurationsSettingsListResponse
        type Scopes ManagedConfigurationssettingsList =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient ManagedConfigurationssettingsList'{..}
          = go _mclEnterpriseId _mclProductId (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ManagedConfigurationssettingsListResource)
                      mempty
