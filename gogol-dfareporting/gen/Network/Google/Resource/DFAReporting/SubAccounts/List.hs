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
-- Module      : Network.Google.Resource.DFAReporting.SubAccounts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of subaccounts, possibly filtered. This method supports
-- paging.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.subaccounts.list@.
module Network.Google.Resource.DFAReporting.SubAccounts.List
    (
    -- * REST Resource
      SubAccountsListResource

    -- * Creating a Request
    , subAccountsList
    , SubAccountsList

    -- * Request Lenses
    , salSearchString
    , salIds
    , salProFileId
    , salSortOrder
    , salPageToken
    , salSortField
    , salMaxResults
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.subaccounts.list@ method which the
-- 'SubAccountsList' request conforms to.
type SubAccountsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "subaccounts" :>
               QueryParam "searchString" Text :>
                 QueryParams "ids" (Textual Int64) :>
                   QueryParam "sortOrder" SubAccountsListSortOrder :>
                     QueryParam "pageToken" Text :>
                       QueryParam "sortField" SubAccountsListSortField :>
                         QueryParam "maxResults" (Textual Int32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] SubAccountsListResponse

-- | Gets a list of subaccounts, possibly filtered. This method supports
-- paging.
--
-- /See:/ 'subAccountsList' smart constructor.
data SubAccountsList = SubAccountsList'
    { _salSearchString :: !(Maybe Text)
    , _salIds :: !(Maybe [Textual Int64])
    , _salProFileId :: !(Textual Int64)
    , _salSortOrder :: !SubAccountsListSortOrder
    , _salPageToken :: !(Maybe Text)
    , _salSortField :: !SubAccountsListSortField
    , _salMaxResults :: !(Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubAccountsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'salSearchString'
--
-- * 'salIds'
--
-- * 'salProFileId'
--
-- * 'salSortOrder'
--
-- * 'salPageToken'
--
-- * 'salSortField'
--
-- * 'salMaxResults'
subAccountsList
    :: Int64 -- ^ 'salProFileId'
    -> SubAccountsList
subAccountsList pSalProFileId_ = 
    SubAccountsList'
    { _salSearchString = Nothing
    , _salIds = Nothing
    , _salProFileId = _Coerce # pSalProFileId_
    , _salSortOrder = SALSOAscending
    , _salPageToken = Nothing
    , _salSortField = SALSFID
    , _salMaxResults = 1000
    }

-- | Allows searching for objects by name or ID. Wildcards (*) are allowed.
-- For example, \"subaccount*2015\" will return objects with names like
-- \"subaccount June 2015\", \"subaccount April 2015\", or simply
-- \"subaccount 2015\". Most of the searches also add wildcards implicitly
-- at the start and the end of the search string. For example, a search
-- string of \"subaccount\" will match objects with name \"my subaccount\",
-- \"subaccount 2015\", or simply \"subaccount\".
salSearchString :: Lens' SubAccountsList (Maybe Text)
salSearchString
  = lens _salSearchString
      (\ s a -> s{_salSearchString = a})

-- | Select only subaccounts with these IDs.
salIds :: Lens' SubAccountsList [Int64]
salIds
  = lens _salIds (\ s a -> s{_salIds = a}) . _Default .
      _Coerce

-- | User profile ID associated with this request.
salProFileId :: Lens' SubAccountsList Int64
salProFileId
  = lens _salProFileId (\ s a -> s{_salProFileId = a})
      . _Coerce

-- | Order of sorted results.
salSortOrder :: Lens' SubAccountsList SubAccountsListSortOrder
salSortOrder
  = lens _salSortOrder (\ s a -> s{_salSortOrder = a})

-- | Value of the nextPageToken from the previous result page.
salPageToken :: Lens' SubAccountsList (Maybe Text)
salPageToken
  = lens _salPageToken (\ s a -> s{_salPageToken = a})

-- | Field by which to sort the list.
salSortField :: Lens' SubAccountsList SubAccountsListSortField
salSortField
  = lens _salSortField (\ s a -> s{_salSortField = a})

-- | Maximum number of results to return.
salMaxResults :: Lens' SubAccountsList Int32
salMaxResults
  = lens _salMaxResults
      (\ s a -> s{_salMaxResults = a})
      . _Coerce

instance GoogleRequest SubAccountsList where
        type Rs SubAccountsList = SubAccountsListResponse
        type Scopes SubAccountsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient SubAccountsList'{..}
          = go _salProFileId _salSearchString
              (_salIds ^. _Default)
              (Just _salSortOrder)
              _salPageToken
              (Just _salSortField)
              (Just _salMaxResults)
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy SubAccountsListResource)
                      mempty
