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
-- Module      : Network.Google.Resource.Compute.Disks.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of persistent disks.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.disks.aggregatedList@.
module Network.Google.Resource.Compute.Disks.AggregatedList
    (
    -- * REST Resource
      DisksAggregatedListResource

    -- * Creating a Request
    , disksAggregatedList
    , DisksAggregatedList

    -- * Request Lenses
    , dalOrderBy
    , dalProject
    , dalFilter
    , dalPageToken
    , dalMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.disks.aggregatedList@ method which the
-- 'DisksAggregatedList' request conforms to.
type DisksAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "disks" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] DiskAggregatedList

-- | Retrieves an aggregated list of persistent disks.
--
-- /See:/ 'disksAggregatedList' smart constructor.
data DisksAggregatedList = DisksAggregatedList'
    { _dalOrderBy :: !(Maybe Text)
    , _dalProject :: !Text
    , _dalFilter :: !(Maybe Text)
    , _dalPageToken :: !(Maybe Text)
    , _dalMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisksAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalOrderBy'
--
-- * 'dalProject'
--
-- * 'dalFilter'
--
-- * 'dalPageToken'
--
-- * 'dalMaxResults'
disksAggregatedList
    :: Text -- ^ 'dalProject'
    -> DisksAggregatedList
disksAggregatedList pDalProject_ = 
    DisksAggregatedList'
    { _dalOrderBy = Nothing
    , _dalProject = pDalProject_
    , _dalFilter = Nothing
    , _dalPageToken = Nothing
    , _dalMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
dalOrderBy :: Lens' DisksAggregatedList (Maybe Text)
dalOrderBy
  = lens _dalOrderBy (\ s a -> s{_dalOrderBy = a})

-- | Project ID for this request.
dalProject :: Lens' DisksAggregatedList Text
dalProject
  = lens _dalProject (\ s a -> s{_dalProject = a})

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
dalFilter :: Lens' DisksAggregatedList (Maybe Text)
dalFilter
  = lens _dalFilter (\ s a -> s{_dalFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
dalPageToken :: Lens' DisksAggregatedList (Maybe Text)
dalPageToken
  = lens _dalPageToken (\ s a -> s{_dalPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
dalMaxResults :: Lens' DisksAggregatedList Word32
dalMaxResults
  = lens _dalMaxResults
      (\ s a -> s{_dalMaxResults = a})
      . _Coerce

instance GoogleRequest DisksAggregatedList where
        type Rs DisksAggregatedList = DiskAggregatedList
        type Scopes DisksAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient DisksAggregatedList'{..}
          = go _dalProject _dalOrderBy _dalFilter _dalPageToken
              (Just _dalMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy DisksAggregatedListResource)
                      mempty
