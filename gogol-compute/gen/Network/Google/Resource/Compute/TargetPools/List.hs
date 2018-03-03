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
-- Module      : Network.Google.Resource.Compute.TargetPools.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of target pools available to the specified project and
-- region.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.targetPools.list@.
module Network.Google.Resource.Compute.TargetPools.List
    (
    -- * REST Resource
      TargetPoolsListResource

    -- * Creating a Request
    , targetPoolsList
    , TargetPoolsList

    -- * Request Lenses
    , tplOrderBy
    , tplProject
    , tplFilter
    , tplRegion
    , tplPageToken
    , tplMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.targetPools.list@ method which the
-- 'TargetPoolsList' request conforms to.
type TargetPoolsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "targetPools" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] TargetPoolList

-- | Retrieves a list of target pools available to the specified project and
-- region.
--
-- /See:/ 'targetPoolsList' smart constructor.
data TargetPoolsList = TargetPoolsList'
    { _tplOrderBy :: !(Maybe Text)
    , _tplProject :: !Text
    , _tplFilter :: !(Maybe Text)
    , _tplRegion :: !Text
    , _tplPageToken :: !(Maybe Text)
    , _tplMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TargetPoolsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tplOrderBy'
--
-- * 'tplProject'
--
-- * 'tplFilter'
--
-- * 'tplRegion'
--
-- * 'tplPageToken'
--
-- * 'tplMaxResults'
targetPoolsList
    :: Text -- ^ 'tplProject'
    -> Text -- ^ 'tplRegion'
    -> TargetPoolsList
targetPoolsList pTplProject_ pTplRegion_ = 
    TargetPoolsList'
    { _tplOrderBy = Nothing
    , _tplProject = pTplProject_
    , _tplFilter = Nothing
    , _tplRegion = pTplRegion_
    , _tplPageToken = Nothing
    , _tplMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
tplOrderBy :: Lens' TargetPoolsList (Maybe Text)
tplOrderBy
  = lens _tplOrderBy (\ s a -> s{_tplOrderBy = a})

-- | Project ID for this request.
tplProject :: Lens' TargetPoolsList Text
tplProject
  = lens _tplProject (\ s a -> s{_tplProject = a})

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
tplFilter :: Lens' TargetPoolsList (Maybe Text)
tplFilter
  = lens _tplFilter (\ s a -> s{_tplFilter = a})

-- | Name of the region scoping this request.
tplRegion :: Lens' TargetPoolsList Text
tplRegion
  = lens _tplRegion (\ s a -> s{_tplRegion = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
tplPageToken :: Lens' TargetPoolsList (Maybe Text)
tplPageToken
  = lens _tplPageToken (\ s a -> s{_tplPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
tplMaxResults :: Lens' TargetPoolsList Word32
tplMaxResults
  = lens _tplMaxResults
      (\ s a -> s{_tplMaxResults = a})
      . _Coerce

instance GoogleRequest TargetPoolsList where
        type Rs TargetPoolsList = TargetPoolList
        type Scopes TargetPoolsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient TargetPoolsList'{..}
          = go _tplProject _tplRegion _tplOrderBy _tplFilter
              _tplPageToken
              (Just _tplMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy TargetPoolsListResource)
                      mempty
