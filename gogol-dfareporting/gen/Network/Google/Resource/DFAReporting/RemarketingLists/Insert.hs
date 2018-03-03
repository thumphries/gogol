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
-- Module      : Network.Google.Resource.DFAReporting.RemarketingLists.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts a new remarketing list.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.remarketingLists.insert@.
module Network.Google.Resource.DFAReporting.RemarketingLists.Insert
    (
    -- * REST Resource
      RemarketingListsInsertResource

    -- * Creating a Request
    , remarketingListsInsert
    , RemarketingListsInsert

    -- * Request Lenses
    , rliProFileId
    , rliPayload
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.remarketingLists.insert@ method which the
-- 'RemarketingListsInsert' request conforms to.
type RemarketingListsInsertResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "remarketingLists" :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] RemarketingList :>
                   Post '[JSON] RemarketingList

-- | Inserts a new remarketing list.
--
-- /See:/ 'remarketingListsInsert' smart constructor.
data RemarketingListsInsert = RemarketingListsInsert'
    { _rliProFileId :: !(Textual Int64)
    , _rliPayload :: !RemarketingList
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemarketingListsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rliProFileId'
--
-- * 'rliPayload'
remarketingListsInsert
    :: Int64 -- ^ 'rliProFileId'
    -> RemarketingList -- ^ 'rliPayload'
    -> RemarketingListsInsert
remarketingListsInsert pRliProFileId_ pRliPayload_ = 
    RemarketingListsInsert'
    { _rliProFileId = _Coerce # pRliProFileId_
    , _rliPayload = pRliPayload_
    }

-- | User profile ID associated with this request.
rliProFileId :: Lens' RemarketingListsInsert Int64
rliProFileId
  = lens _rliProFileId (\ s a -> s{_rliProFileId = a})
      . _Coerce

-- | Multipart request metadata.
rliPayload :: Lens' RemarketingListsInsert RemarketingList
rliPayload
  = lens _rliPayload (\ s a -> s{_rliPayload = a})

instance GoogleRequest RemarketingListsInsert where
        type Rs RemarketingListsInsert = RemarketingList
        type Scopes RemarketingListsInsert =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient RemarketingListsInsert'{..}
          = go _rliProFileId (Just AltJSON) _rliPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy RemarketingListsInsertResource)
                      mempty
