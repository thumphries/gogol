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
-- Module      : Network.Google.Resource.Compute.Instances.ListReferrers
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of referrers to instances contained within the
-- specified zone.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.instances.listReferrers@.
module Network.Google.Resource.Compute.Instances.ListReferrers
    (
    -- * REST Resource
      InstancesListReferrersResource

    -- * Creating a Request
    , instancesListReferrers
    , InstancesListReferrers

    -- * Request Lenses
    , ilrOrderBy
    , ilrProject
    , ilrZone
    , ilrFilter
    , ilrPageToken
    , ilrMaxResults
    , ilrInstance
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.instances.listReferrers@ method which the
-- 'InstancesListReferrers' request conforms to.
type InstancesListReferrersResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "instances" :>
                   Capture "instance" Text :>
                     "referrers" :>
                       QueryParam "orderBy" Text :>
                         QueryParam "filter" Text :>
                           QueryParam "pageToken" Text :>
                             QueryParam "maxResults" (Textual Word32) :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] InstanceListReferrers

-- | Retrieves the list of referrers to instances contained within the
-- specified zone.
--
-- /See:/ 'instancesListReferrers' smart constructor.
data InstancesListReferrers = InstancesListReferrers'
    { _ilrOrderBy :: !(Maybe Text)
    , _ilrProject :: !Text
    , _ilrZone :: !Text
    , _ilrFilter :: !(Maybe Text)
    , _ilrPageToken :: !(Maybe Text)
    , _ilrMaxResults :: !(Textual Word32)
    , _ilrInstance :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstancesListReferrers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilrOrderBy'
--
-- * 'ilrProject'
--
-- * 'ilrZone'
--
-- * 'ilrFilter'
--
-- * 'ilrPageToken'
--
-- * 'ilrMaxResults'
--
-- * 'ilrInstance'
instancesListReferrers
    :: Text -- ^ 'ilrProject'
    -> Text -- ^ 'ilrZone'
    -> Text -- ^ 'ilrInstance'
    -> InstancesListReferrers
instancesListReferrers pIlrProject_ pIlrZone_ pIlrInstance_ = 
    InstancesListReferrers'
    { _ilrOrderBy = Nothing
    , _ilrProject = pIlrProject_
    , _ilrZone = pIlrZone_
    , _ilrFilter = Nothing
    , _ilrPageToken = Nothing
    , _ilrMaxResults = 500
    , _ilrInstance = pIlrInstance_
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
ilrOrderBy :: Lens' InstancesListReferrers (Maybe Text)
ilrOrderBy
  = lens _ilrOrderBy (\ s a -> s{_ilrOrderBy = a})

-- | Project ID for this request.
ilrProject :: Lens' InstancesListReferrers Text
ilrProject
  = lens _ilrProject (\ s a -> s{_ilrProject = a})

-- | The name of the zone for this request.
ilrZone :: Lens' InstancesListReferrers Text
ilrZone = lens _ilrZone (\ s a -> s{_ilrZone = a})

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
ilrFilter :: Lens' InstancesListReferrers (Maybe Text)
ilrFilter
  = lens _ilrFilter (\ s a -> s{_ilrFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
ilrPageToken :: Lens' InstancesListReferrers (Maybe Text)
ilrPageToken
  = lens _ilrPageToken (\ s a -> s{_ilrPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
ilrMaxResults :: Lens' InstancesListReferrers Word32
ilrMaxResults
  = lens _ilrMaxResults
      (\ s a -> s{_ilrMaxResults = a})
      . _Coerce

-- | Name of the target instance scoping this request, or \'-\' if the
-- request should span over all instances in the container.
ilrInstance :: Lens' InstancesListReferrers Text
ilrInstance
  = lens _ilrInstance (\ s a -> s{_ilrInstance = a})

instance GoogleRequest InstancesListReferrers where
        type Rs InstancesListReferrers =
             InstanceListReferrers
        type Scopes InstancesListReferrers =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient InstancesListReferrers'{..}
          = go _ilrProject _ilrZone _ilrInstance _ilrOrderBy
              _ilrFilter
              _ilrPageToken
              (Just _ilrMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy InstancesListReferrersResource)
                      mempty