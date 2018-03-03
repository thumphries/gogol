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
-- Module      : Network.Google.Resource.DFAReporting.DirectorySiteContacts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of directory site contacts, possibly filtered. This
-- method supports paging.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.directorySiteContacts.list@.
module Network.Google.Resource.DFAReporting.DirectorySiteContacts.List
    (
    -- * REST Resource
      DirectorySiteContactsListResource

    -- * Creating a Request
    , directorySiteContactsList
    , DirectorySiteContactsList

    -- * Request Lenses
    , dsclSearchString
    , dsclIds
    , dsclProFileId
    , dsclDirectorySiteIds
    , dsclSortOrder
    , dsclPageToken
    , dsclSortField
    , dsclMaxResults
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.directorySiteContacts.list@ method which the
-- 'DirectorySiteContactsList' request conforms to.
type DirectorySiteContactsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "directorySiteContacts" :>
               QueryParam "searchString" Text :>
                 QueryParams "ids" (Textual Int64) :>
                   QueryParams "directorySiteIds" (Textual Int64) :>
                     QueryParam "sortOrder"
                       DirectorySiteContactsListSortOrder
                       :>
                       QueryParam "pageToken" Text :>
                         QueryParam "sortField"
                           DirectorySiteContactsListSortField
                           :>
                           QueryParam "maxResults" (Textual Int32) :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] DirectorySiteContactsListResponse

-- | Retrieves a list of directory site contacts, possibly filtered. This
-- method supports paging.
--
-- /See:/ 'directorySiteContactsList' smart constructor.
data DirectorySiteContactsList = DirectorySiteContactsList'
    { _dsclSearchString :: !(Maybe Text)
    , _dsclIds :: !(Maybe [Textual Int64])
    , _dsclProFileId :: !(Textual Int64)
    , _dsclDirectorySiteIds :: !(Maybe [Textual Int64])
    , _dsclSortOrder :: !DirectorySiteContactsListSortOrder
    , _dsclPageToken :: !(Maybe Text)
    , _dsclSortField :: !DirectorySiteContactsListSortField
    , _dsclMaxResults :: !(Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectorySiteContactsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsclSearchString'
--
-- * 'dsclIds'
--
-- * 'dsclProFileId'
--
-- * 'dsclDirectorySiteIds'
--
-- * 'dsclSortOrder'
--
-- * 'dsclPageToken'
--
-- * 'dsclSortField'
--
-- * 'dsclMaxResults'
directorySiteContactsList
    :: Int64 -- ^ 'dsclProFileId'
    -> DirectorySiteContactsList
directorySiteContactsList pDsclProFileId_ = 
    DirectorySiteContactsList'
    { _dsclSearchString = Nothing
    , _dsclIds = Nothing
    , _dsclProFileId = _Coerce # pDsclProFileId_
    , _dsclDirectorySiteIds = Nothing
    , _dsclSortOrder = DSCLSOAscending
    , _dsclPageToken = Nothing
    , _dsclSortField = DSCLSFID
    , _dsclMaxResults = 1000
    }

-- | Allows searching for objects by name, ID or email. Wildcards (*) are
-- allowed. For example, \"directory site contact*2015\" will return
-- objects with names like \"directory site contact June 2015\",
-- \"directory site contact April 2015\", or simply \"directory site
-- contact 2015\". Most of the searches also add wildcards implicitly at
-- the start and the end of the search string. For example, a search string
-- of \"directory site contact\" will match objects with name \"my
-- directory site contact\", \"directory site contact 2015\", or simply
-- \"directory site contact\".
dsclSearchString :: Lens' DirectorySiteContactsList (Maybe Text)
dsclSearchString
  = lens _dsclSearchString
      (\ s a -> s{_dsclSearchString = a})

-- | Select only directory site contacts with these IDs.
dsclIds :: Lens' DirectorySiteContactsList [Int64]
dsclIds
  = lens _dsclIds (\ s a -> s{_dsclIds = a}) . _Default
      . _Coerce

-- | User profile ID associated with this request.
dsclProFileId :: Lens' DirectorySiteContactsList Int64
dsclProFileId
  = lens _dsclProFileId
      (\ s a -> s{_dsclProFileId = a})
      . _Coerce

-- | Select only directory site contacts with these directory site IDs. This
-- is a required field.
dsclDirectorySiteIds :: Lens' DirectorySiteContactsList [Int64]
dsclDirectorySiteIds
  = lens _dsclDirectorySiteIds
      (\ s a -> s{_dsclDirectorySiteIds = a})
      . _Default
      . _Coerce

-- | Order of sorted results.
dsclSortOrder :: Lens' DirectorySiteContactsList DirectorySiteContactsListSortOrder
dsclSortOrder
  = lens _dsclSortOrder
      (\ s a -> s{_dsclSortOrder = a})

-- | Value of the nextPageToken from the previous result page.
dsclPageToken :: Lens' DirectorySiteContactsList (Maybe Text)
dsclPageToken
  = lens _dsclPageToken
      (\ s a -> s{_dsclPageToken = a})

-- | Field by which to sort the list.
dsclSortField :: Lens' DirectorySiteContactsList DirectorySiteContactsListSortField
dsclSortField
  = lens _dsclSortField
      (\ s a -> s{_dsclSortField = a})

-- | Maximum number of results to return.
dsclMaxResults :: Lens' DirectorySiteContactsList Int32
dsclMaxResults
  = lens _dsclMaxResults
      (\ s a -> s{_dsclMaxResults = a})
      . _Coerce

instance GoogleRequest DirectorySiteContactsList
         where
        type Rs DirectorySiteContactsList =
             DirectorySiteContactsListResponse
        type Scopes DirectorySiteContactsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient DirectorySiteContactsList'{..}
          = go _dsclProFileId _dsclSearchString
              (_dsclIds ^. _Default)
              (_dsclDirectorySiteIds ^. _Default)
              (Just _dsclSortOrder)
              _dsclPageToken
              (Just _dsclSortField)
              (Just _dsclMaxResults)
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy DirectorySiteContactsListResource)
                      mempty
