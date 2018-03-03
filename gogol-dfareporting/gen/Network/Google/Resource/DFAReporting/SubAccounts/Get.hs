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
-- Module      : Network.Google.Resource.DFAReporting.SubAccounts.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one subaccount by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.subaccounts.get@.
module Network.Google.Resource.DFAReporting.SubAccounts.Get
    (
    -- * REST Resource
      SubAccountsGetResource

    -- * Creating a Request
    , subAccountsGet
    , SubAccountsGet

    -- * Request Lenses
    , sagProFileId
    , sagId
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.subaccounts.get@ method which the
-- 'SubAccountsGet' request conforms to.
type SubAccountsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "subaccounts" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "alt" AltJSON :> Get '[JSON] SubAccount

-- | Gets one subaccount by ID.
--
-- /See:/ 'subAccountsGet' smart constructor.
data SubAccountsGet = SubAccountsGet'
    { _sagProFileId :: !(Textual Int64)
    , _sagId :: !(Textual Int64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubAccountsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sagProFileId'
--
-- * 'sagId'
subAccountsGet
    :: Int64 -- ^ 'sagProFileId'
    -> Int64 -- ^ 'sagId'
    -> SubAccountsGet
subAccountsGet pSagProFileId_ pSagId_ = 
    SubAccountsGet'
    { _sagProFileId = _Coerce # pSagProFileId_
    , _sagId = _Coerce # pSagId_
    }

-- | User profile ID associated with this request.
sagProFileId :: Lens' SubAccountsGet Int64
sagProFileId
  = lens _sagProFileId (\ s a -> s{_sagProFileId = a})
      . _Coerce

-- | Subaccount ID.
sagId :: Lens' SubAccountsGet Int64
sagId
  = lens _sagId (\ s a -> s{_sagId = a}) . _Coerce

instance GoogleRequest SubAccountsGet where
        type Rs SubAccountsGet = SubAccount
        type Scopes SubAccountsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient SubAccountsGet'{..}
          = go _sagProFileId _sagId (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy SubAccountsGetResource)
                      mempty
