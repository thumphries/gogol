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
-- Module      : Network.Google.Resource.Compute.Snapshots.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of Snapshot resources contained within the specified
-- project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.snapshots.list@.
module Network.Google.Resource.Compute.Snapshots.List
    (
    -- * REST Resource
      SnapshotsListResource

    -- * Creating a Request
    , snapshotsList
    , SnapshotsList

    -- * Request Lenses
    , sOrderBy
    , sProject
    , sFilter
    , sPageToken
    , sMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.snapshots.list@ method which the
-- 'SnapshotsList' request conforms to.
type SnapshotsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "snapshots" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :> Get '[JSON] SnapshotList

-- | Retrieves the list of Snapshot resources contained within the specified
-- project.
--
-- /See:/ 'snapshotsList' smart constructor.
data SnapshotsList = SnapshotsList'
    { _sOrderBy :: !(Maybe Text)
    , _sProject :: !Text
    , _sFilter :: !(Maybe Text)
    , _sPageToken :: !(Maybe Text)
    , _sMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SnapshotsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOrderBy'
--
-- * 'sProject'
--
-- * 'sFilter'
--
-- * 'sPageToken'
--
-- * 'sMaxResults'
snapshotsList
    :: Text -- ^ 'sProject'
    -> SnapshotsList
snapshotsList pSProject_ = 
    SnapshotsList'
    { _sOrderBy = Nothing
    , _sProject = pSProject_
    , _sFilter = Nothing
    , _sPageToken = Nothing
    , _sMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
sOrderBy :: Lens' SnapshotsList (Maybe Text)
sOrderBy = lens _sOrderBy (\ s a -> s{_sOrderBy = a})

-- | Project ID for this request.
sProject :: Lens' SnapshotsList Text
sProject = lens _sProject (\ s a -> s{_sProject = a})

-- | Sets a filter {expression} for filtering listed resources. Your
-- {expression} must be in the format: field_name comparison_string
-- literal_string. The field_name is the name of the field you want to
-- compare. Only atomic field types are supported (string, number,
-- boolean). The comparison_string must be either eq (equals) or ne (not
-- equals). The literal_string is the string value to filter to. The
-- literal value must be valid for the type of field you are filtering by
-- (string, number, boolean). For string fields, the literal value is
-- interpreted as a regular expression using RE2 syntax. The literal value
-- must match the entire field. For example, to filter for instances that
-- do not have a name of example-instance, you would use name ne
-- example-instance. You can filter on nested fields. For example, you
-- could filter on instances that have set the scheduling.automaticRestart
-- field to true. Use filtering on nested fields to take advantage of
-- labels to organize and search for results based on label values. To
-- filter on multiple expressions, provide each separate expression within
-- parentheses. For example, (scheduling.automaticRestart eq true) (zone eq
-- us-central1-f). Multiple expressions are treated as AND expressions,
-- meaning that resources must match all expressions to pass the filters.
sFilter :: Lens' SnapshotsList (Maybe Text)
sFilter = lens _sFilter (\ s a -> s{_sFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
sPageToken :: Lens' SnapshotsList (Maybe Text)
sPageToken
  = lens _sPageToken (\ s a -> s{_sPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
sMaxResults :: Lens' SnapshotsList Word32
sMaxResults
  = lens _sMaxResults (\ s a -> s{_sMaxResults = a}) .
      _Coerce

instance GoogleRequest SnapshotsList where
        type Rs SnapshotsList = SnapshotList
        type Scopes SnapshotsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient SnapshotsList'{..}
          = go _sProject _sOrderBy _sFilter _sPageToken
              (Just _sMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy SnapshotsListResource)
                      mempty
