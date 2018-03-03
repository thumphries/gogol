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
-- Module      : Network.Google.Resource.AndroidEnterprise.Enterprises.Unenroll
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unenrolls an enterprise from the calling EMM.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.enterprises.unenroll@.
module Network.Google.Resource.AndroidEnterprise.Enterprises.Unenroll
    (
    -- * REST Resource
      EnterprisesUnenrollResource

    -- * Creating a Request
    , enterprisesUnenroll
    , EnterprisesUnenroll

    -- * Request Lenses
    , euuEnterpriseId
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.enterprises.unenroll@ method which the
-- 'EnterprisesUnenroll' request conforms to.
type EnterprisesUnenrollResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "unenroll" :>
               QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Unenrolls an enterprise from the calling EMM.
--
-- /See:/ 'enterprisesUnenroll' smart constructor.
newtype EnterprisesUnenroll = EnterprisesUnenroll'
    { _euuEnterpriseId :: Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnterprisesUnenroll' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'euuEnterpriseId'
enterprisesUnenroll
    :: Text -- ^ 'euuEnterpriseId'
    -> EnterprisesUnenroll
enterprisesUnenroll pEuuEnterpriseId_ = 
    EnterprisesUnenroll'
    { _euuEnterpriseId = pEuuEnterpriseId_
    }

-- | The ID of the enterprise.
euuEnterpriseId :: Lens' EnterprisesUnenroll Text
euuEnterpriseId
  = lens _euuEnterpriseId
      (\ s a -> s{_euuEnterpriseId = a})

instance GoogleRequest EnterprisesUnenroll where
        type Rs EnterprisesUnenroll = ()
        type Scopes EnterprisesUnenroll =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient EnterprisesUnenroll'{..}
          = go _euuEnterpriseId (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy EnterprisesUnenrollResource)
                      mempty
