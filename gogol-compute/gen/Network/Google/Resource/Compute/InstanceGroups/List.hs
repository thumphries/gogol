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
-- Module      : Network.Google.Resource.Compute.InstanceGroups.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of instance groups that are located in the specified
-- project and zone.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.instanceGroups.list@.
module Network.Google.Resource.Compute.InstanceGroups.List
    (
    -- * REST Resource
      InstanceGroupsListResource

    -- * Creating a Request
    , instanceGroupsList
    , InstanceGroupsList

    -- * Request Lenses
    , iglOrderBy
    , iglProject
    , iglZone
    , iglFilter
    , iglPageToken
    , iglMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.instanceGroups.list@ method which the
-- 'InstanceGroupsList' request conforms to.
type InstanceGroupsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "instanceGroups" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] InstanceGroupList

-- | Retrieves the list of instance groups that are located in the specified
-- project and zone.
--
-- /See:/ 'instanceGroupsList' smart constructor.
data InstanceGroupsList = InstanceGroupsList'
    { _iglOrderBy :: !(Maybe Text)
    , _iglProject :: !Text
    , _iglZone :: !Text
    , _iglFilter :: !(Maybe Text)
    , _iglPageToken :: !(Maybe Text)
    , _iglMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iglOrderBy'
--
-- * 'iglProject'
--
-- * 'iglZone'
--
-- * 'iglFilter'
--
-- * 'iglPageToken'
--
-- * 'iglMaxResults'
instanceGroupsList
    :: Text -- ^ 'iglProject'
    -> Text -- ^ 'iglZone'
    -> InstanceGroupsList
instanceGroupsList pIglProject_ pIglZone_ = 
    InstanceGroupsList'
    { _iglOrderBy = Nothing
    , _iglProject = pIglProject_
    , _iglZone = pIglZone_
    , _iglFilter = Nothing
    , _iglPageToken = Nothing
    , _iglMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
iglOrderBy :: Lens' InstanceGroupsList (Maybe Text)
iglOrderBy
  = lens _iglOrderBy (\ s a -> s{_iglOrderBy = a})

-- | Project ID for this request.
iglProject :: Lens' InstanceGroupsList Text
iglProject
  = lens _iglProject (\ s a -> s{_iglProject = a})

-- | The name of the zone where the instance group is located.
iglZone :: Lens' InstanceGroupsList Text
iglZone = lens _iglZone (\ s a -> s{_iglZone = a})

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
iglFilter :: Lens' InstanceGroupsList (Maybe Text)
iglFilter
  = lens _iglFilter (\ s a -> s{_iglFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
iglPageToken :: Lens' InstanceGroupsList (Maybe Text)
iglPageToken
  = lens _iglPageToken (\ s a -> s{_iglPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
iglMaxResults :: Lens' InstanceGroupsList Word32
iglMaxResults
  = lens _iglMaxResults
      (\ s a -> s{_iglMaxResults = a})
      . _Coerce

instance GoogleRequest InstanceGroupsList where
        type Rs InstanceGroupsList = InstanceGroupList
        type Scopes InstanceGroupsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient InstanceGroupsList'{..}
          = go _iglProject _iglZone _iglOrderBy _iglFilter
              _iglPageToken
              (Just _iglMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy InstanceGroupsListResource)
                      mempty
