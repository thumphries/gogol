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
-- Module      : Network.Google.Resource.Compute.Subnetworks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subnetworks available to the specified project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.subnetworks.list@.
module Network.Google.Resource.Compute.Subnetworks.List
    (
    -- * REST Resource
      SubnetworksListResource

    -- * Creating a Request
    , subnetworksList
    , SubnetworksList

    -- * Request Lenses
    , slOrderBy
    , slProject
    , slFilter
    , slRegion
    , slPageToken
    , slMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.subnetworks.list@ method which the
-- 'SubnetworksList' request conforms to.
type SubnetworksListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "subnetworks" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] SubnetworkList

-- | Retrieves a list of subnetworks available to the specified project.
--
-- /See:/ 'subnetworksList' smart constructor.
data SubnetworksList = SubnetworksList'
    { _slOrderBy :: !(Maybe Text)
    , _slProject :: !Text
    , _slFilter :: !(Maybe Text)
    , _slRegion :: !Text
    , _slPageToken :: !(Maybe Text)
    , _slMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubnetworksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slOrderBy'
--
-- * 'slProject'
--
-- * 'slFilter'
--
-- * 'slRegion'
--
-- * 'slPageToken'
--
-- * 'slMaxResults'
subnetworksList
    :: Text -- ^ 'slProject'
    -> Text -- ^ 'slRegion'
    -> SubnetworksList
subnetworksList pSlProject_ pSlRegion_ = 
    SubnetworksList'
    { _slOrderBy = Nothing
    , _slProject = pSlProject_
    , _slFilter = Nothing
    , _slRegion = pSlRegion_
    , _slPageToken = Nothing
    , _slMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
slOrderBy :: Lens' SubnetworksList (Maybe Text)
slOrderBy
  = lens _slOrderBy (\ s a -> s{_slOrderBy = a})

-- | Project ID for this request.
slProject :: Lens' SubnetworksList Text
slProject
  = lens _slProject (\ s a -> s{_slProject = a})

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
slFilter :: Lens' SubnetworksList (Maybe Text)
slFilter = lens _slFilter (\ s a -> s{_slFilter = a})

-- | Name of the region scoping this request.
slRegion :: Lens' SubnetworksList Text
slRegion = lens _slRegion (\ s a -> s{_slRegion = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
slPageToken :: Lens' SubnetworksList (Maybe Text)
slPageToken
  = lens _slPageToken (\ s a -> s{_slPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
slMaxResults :: Lens' SubnetworksList Word32
slMaxResults
  = lens _slMaxResults (\ s a -> s{_slMaxResults = a})
      . _Coerce

instance GoogleRequest SubnetworksList where
        type Rs SubnetworksList = SubnetworkList
        type Scopes SubnetworksList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient SubnetworksList'{..}
          = go _slProject _slRegion _slOrderBy _slFilter
              _slPageToken
              (Just _slMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy SubnetworksListResource)
                      mempty
