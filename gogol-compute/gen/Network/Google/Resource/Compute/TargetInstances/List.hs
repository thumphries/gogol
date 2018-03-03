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
-- Module      : Network.Google.Resource.Compute.TargetInstances.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of TargetInstance resources available to the specified
-- project and zone.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.targetInstances.list@.
module Network.Google.Resource.Compute.TargetInstances.List
    (
    -- * REST Resource
      TargetInstancesListResource

    -- * Creating a Request
    , targetInstancesList
    , TargetInstancesList

    -- * Request Lenses
    , tilOrderBy
    , tilProject
    , tilZone
    , tilFilter
    , tilPageToken
    , tilMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.targetInstances.list@ method which the
-- 'TargetInstancesList' request conforms to.
type TargetInstancesListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "targetInstances" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] TargetInstanceList

-- | Retrieves a list of TargetInstance resources available to the specified
-- project and zone.
--
-- /See:/ 'targetInstancesList' smart constructor.
data TargetInstancesList = TargetInstancesList'
    { _tilOrderBy :: !(Maybe Text)
    , _tilProject :: !Text
    , _tilZone :: !Text
    , _tilFilter :: !(Maybe Text)
    , _tilPageToken :: !(Maybe Text)
    , _tilMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TargetInstancesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tilOrderBy'
--
-- * 'tilProject'
--
-- * 'tilZone'
--
-- * 'tilFilter'
--
-- * 'tilPageToken'
--
-- * 'tilMaxResults'
targetInstancesList
    :: Text -- ^ 'tilProject'
    -> Text -- ^ 'tilZone'
    -> TargetInstancesList
targetInstancesList pTilProject_ pTilZone_ = 
    TargetInstancesList'
    { _tilOrderBy = Nothing
    , _tilProject = pTilProject_
    , _tilZone = pTilZone_
    , _tilFilter = Nothing
    , _tilPageToken = Nothing
    , _tilMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
tilOrderBy :: Lens' TargetInstancesList (Maybe Text)
tilOrderBy
  = lens _tilOrderBy (\ s a -> s{_tilOrderBy = a})

-- | Project ID for this request.
tilProject :: Lens' TargetInstancesList Text
tilProject
  = lens _tilProject (\ s a -> s{_tilProject = a})

-- | Name of the zone scoping this request.
tilZone :: Lens' TargetInstancesList Text
tilZone = lens _tilZone (\ s a -> s{_tilZone = a})

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
tilFilter :: Lens' TargetInstancesList (Maybe Text)
tilFilter
  = lens _tilFilter (\ s a -> s{_tilFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
tilPageToken :: Lens' TargetInstancesList (Maybe Text)
tilPageToken
  = lens _tilPageToken (\ s a -> s{_tilPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
tilMaxResults :: Lens' TargetInstancesList Word32
tilMaxResults
  = lens _tilMaxResults
      (\ s a -> s{_tilMaxResults = a})
      . _Coerce

instance GoogleRequest TargetInstancesList where
        type Rs TargetInstancesList = TargetInstanceList
        type Scopes TargetInstancesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient TargetInstancesList'{..}
          = go _tilProject _tilZone _tilOrderBy _tilFilter
              _tilPageToken
              (Just _tilMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy TargetInstancesListResource)
                      mempty
