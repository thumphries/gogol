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
-- Module      : Network.Google.Resource.Compute.TargetPools.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of target pools.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.targetPools.aggregatedList@.
module Network.Google.Resource.Compute.TargetPools.AggregatedList
    (
    -- * REST Resource
      TargetPoolsAggregatedListResource

    -- * Creating a Request
    , targetPoolsAggregatedList
    , TargetPoolsAggregatedList

    -- * Request Lenses
    , tpalOrderBy
    , tpalProject
    , tpalFilter
    , tpalPageToken
    , tpalMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.targetPools.aggregatedList@ method which the
-- 'TargetPoolsAggregatedList' request conforms to.
type TargetPoolsAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "targetPools" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] TargetPoolAggregatedList

-- | Retrieves an aggregated list of target pools.
--
-- /See:/ 'targetPoolsAggregatedList' smart constructor.
data TargetPoolsAggregatedList = TargetPoolsAggregatedList'
    { _tpalOrderBy :: !(Maybe Text)
    , _tpalProject :: !Text
    , _tpalFilter :: !(Maybe Text)
    , _tpalPageToken :: !(Maybe Text)
    , _tpalMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TargetPoolsAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpalOrderBy'
--
-- * 'tpalProject'
--
-- * 'tpalFilter'
--
-- * 'tpalPageToken'
--
-- * 'tpalMaxResults'
targetPoolsAggregatedList
    :: Text -- ^ 'tpalProject'
    -> TargetPoolsAggregatedList
targetPoolsAggregatedList pTpalProject_ = 
    TargetPoolsAggregatedList'
    { _tpalOrderBy = Nothing
    , _tpalProject = pTpalProject_
    , _tpalFilter = Nothing
    , _tpalPageToken = Nothing
    , _tpalMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
tpalOrderBy :: Lens' TargetPoolsAggregatedList (Maybe Text)
tpalOrderBy
  = lens _tpalOrderBy (\ s a -> s{_tpalOrderBy = a})

-- | Project ID for this request.
tpalProject :: Lens' TargetPoolsAggregatedList Text
tpalProject
  = lens _tpalProject (\ s a -> s{_tpalProject = a})

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
tpalFilter :: Lens' TargetPoolsAggregatedList (Maybe Text)
tpalFilter
  = lens _tpalFilter (\ s a -> s{_tpalFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
tpalPageToken :: Lens' TargetPoolsAggregatedList (Maybe Text)
tpalPageToken
  = lens _tpalPageToken
      (\ s a -> s{_tpalPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
tpalMaxResults :: Lens' TargetPoolsAggregatedList Word32
tpalMaxResults
  = lens _tpalMaxResults
      (\ s a -> s{_tpalMaxResults = a})
      . _Coerce

instance GoogleRequest TargetPoolsAggregatedList
         where
        type Rs TargetPoolsAggregatedList =
             TargetPoolAggregatedList
        type Scopes TargetPoolsAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient TargetPoolsAggregatedList'{..}
          = go _tpalProject _tpalOrderBy _tpalFilter
              _tpalPageToken
              (Just _tpalMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy TargetPoolsAggregatedListResource)
                      mempty
