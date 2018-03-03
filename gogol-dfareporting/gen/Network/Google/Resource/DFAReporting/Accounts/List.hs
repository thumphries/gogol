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
-- Module      : Network.Google.Resource.DFAReporting.Accounts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of accounts, possibly filtered. This method supports
-- paging.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.accounts.list@.
module Network.Google.Resource.DFAReporting.Accounts.List
    (
    -- * REST Resource
      AccountsListResource

    -- * Creating a Request
    , accountsList
    , AccountsList

    -- * Request Lenses
    , accSearchString
    , accIds
    , accProFileId
    , accSortOrder
    , accActive
    , accPageToken
    , accSortField
    , accMaxResults
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.accounts.list@ method which the
-- 'AccountsList' request conforms to.
type AccountsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "accounts" :>
               QueryParam "searchString" Text :>
                 QueryParams "ids" (Textual Int64) :>
                   QueryParam "sortOrder" AccountsListSortOrder :>
                     QueryParam "active" Bool :>
                       QueryParam "pageToken" Text :>
                         QueryParam "sortField" AccountsListSortField :>
                           QueryParam "maxResults" (Textual Int32) :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] AccountsListResponse

-- | Retrieves the list of accounts, possibly filtered. This method supports
-- paging.
--
-- /See:/ 'accountsList' smart constructor.
data AccountsList = AccountsList'
    { _accSearchString :: !(Maybe Text)
    , _accIds :: !(Maybe [Textual Int64])
    , _accProFileId :: !(Textual Int64)
    , _accSortOrder :: !AccountsListSortOrder
    , _accActive :: !(Maybe Bool)
    , _accPageToken :: !(Maybe Text)
    , _accSortField :: !AccountsListSortField
    , _accMaxResults :: !(Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accSearchString'
--
-- * 'accIds'
--
-- * 'accProFileId'
--
-- * 'accSortOrder'
--
-- * 'accActive'
--
-- * 'accPageToken'
--
-- * 'accSortField'
--
-- * 'accMaxResults'
accountsList
    :: Int64 -- ^ 'accProFileId'
    -> AccountsList
accountsList pAccProFileId_ = 
    AccountsList'
    { _accSearchString = Nothing
    , _accIds = Nothing
    , _accProFileId = _Coerce # pAccProFileId_
    , _accSortOrder = AAscending
    , _accActive = Nothing
    , _accPageToken = Nothing
    , _accSortField = AID
    , _accMaxResults = 1000
    }

-- | Allows searching for objects by name or ID. Wildcards (*) are allowed.
-- For example, \"account*2015\" will return objects with names like
-- \"account June 2015\", \"account April 2015\", or simply \"account
-- 2015\". Most of the searches also add wildcards implicitly at the start
-- and the end of the search string. For example, a search string of
-- \"account\" will match objects with name \"my account\", \"account
-- 2015\", or simply \"account\".
accSearchString :: Lens' AccountsList (Maybe Text)
accSearchString
  = lens _accSearchString
      (\ s a -> s{_accSearchString = a})

-- | Select only accounts with these IDs.
accIds :: Lens' AccountsList [Int64]
accIds
  = lens _accIds (\ s a -> s{_accIds = a}) . _Default .
      _Coerce

-- | User profile ID associated with this request.
accProFileId :: Lens' AccountsList Int64
accProFileId
  = lens _accProFileId (\ s a -> s{_accProFileId = a})
      . _Coerce

-- | Order of sorted results.
accSortOrder :: Lens' AccountsList AccountsListSortOrder
accSortOrder
  = lens _accSortOrder (\ s a -> s{_accSortOrder = a})

-- | Select only active accounts. Don\'t set this field to select both active
-- and non-active accounts.
accActive :: Lens' AccountsList (Maybe Bool)
accActive
  = lens _accActive (\ s a -> s{_accActive = a})

-- | Value of the nextPageToken from the previous result page.
accPageToken :: Lens' AccountsList (Maybe Text)
accPageToken
  = lens _accPageToken (\ s a -> s{_accPageToken = a})

-- | Field by which to sort the list.
accSortField :: Lens' AccountsList AccountsListSortField
accSortField
  = lens _accSortField (\ s a -> s{_accSortField = a})

-- | Maximum number of results to return.
accMaxResults :: Lens' AccountsList Int32
accMaxResults
  = lens _accMaxResults
      (\ s a -> s{_accMaxResults = a})
      . _Coerce

instance GoogleRequest AccountsList where
        type Rs AccountsList = AccountsListResponse
        type Scopes AccountsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AccountsList'{..}
          = go _accProFileId _accSearchString
              (_accIds ^. _Default)
              (Just _accSortOrder)
              _accActive
              _accPageToken
              (Just _accSortField)
              (Just _accMaxResults)
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy AccountsListResource)
                      mempty
