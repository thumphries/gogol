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
-- Module      : Network.Google.Resource.Compute.ZoneOperations.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Operation resources contained within the specified
-- zone.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.zoneOperations.list@.
module Network.Google.Resource.Compute.ZoneOperations.List
    (
    -- * REST Resource
      ZoneOperationsListResource

    -- * Creating a Request
    , zoneOperationsList
    , ZoneOperationsList

    -- * Request Lenses
    , zolOrderBy
    , zolProject
    , zolZone
    , zolFilter
    , zolPageToken
    , zolMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.zoneOperations.list@ method which the
-- 'ZoneOperationsList' request conforms to.
type ZoneOperationsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "operations" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :> Get '[JSON] OperationList

-- | Retrieves a list of Operation resources contained within the specified
-- zone.
--
-- /See:/ 'zoneOperationsList' smart constructor.
data ZoneOperationsList = ZoneOperationsList'
    { _zolOrderBy :: !(Maybe Text)
    , _zolProject :: !Text
    , _zolZone :: !Text
    , _zolFilter :: !(Maybe Text)
    , _zolPageToken :: !(Maybe Text)
    , _zolMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ZoneOperationsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'zolOrderBy'
--
-- * 'zolProject'
--
-- * 'zolZone'
--
-- * 'zolFilter'
--
-- * 'zolPageToken'
--
-- * 'zolMaxResults'
zoneOperationsList
    :: Text -- ^ 'zolProject'
    -> Text -- ^ 'zolZone'
    -> ZoneOperationsList
zoneOperationsList pZolProject_ pZolZone_ = 
    ZoneOperationsList'
    { _zolOrderBy = Nothing
    , _zolProject = pZolProject_
    , _zolZone = pZolZone_
    , _zolFilter = Nothing
    , _zolPageToken = Nothing
    , _zolMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
zolOrderBy :: Lens' ZoneOperationsList (Maybe Text)
zolOrderBy
  = lens _zolOrderBy (\ s a -> s{_zolOrderBy = a})

-- | Project ID for this request.
zolProject :: Lens' ZoneOperationsList Text
zolProject
  = lens _zolProject (\ s a -> s{_zolProject = a})

-- | Name of the zone for request.
zolZone :: Lens' ZoneOperationsList Text
zolZone = lens _zolZone (\ s a -> s{_zolZone = a})

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
zolFilter :: Lens' ZoneOperationsList (Maybe Text)
zolFilter
  = lens _zolFilter (\ s a -> s{_zolFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
zolPageToken :: Lens' ZoneOperationsList (Maybe Text)
zolPageToken
  = lens _zolPageToken (\ s a -> s{_zolPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
zolMaxResults :: Lens' ZoneOperationsList Word32
zolMaxResults
  = lens _zolMaxResults
      (\ s a -> s{_zolMaxResults = a})
      . _Coerce

instance GoogleRequest ZoneOperationsList where
        type Rs ZoneOperationsList = OperationList
        type Scopes ZoneOperationsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient ZoneOperationsList'{..}
          = go _zolProject _zolZone _zolOrderBy _zolFilter
              _zolPageToken
              (Just _zolMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy ZoneOperationsListResource)
                      mempty
