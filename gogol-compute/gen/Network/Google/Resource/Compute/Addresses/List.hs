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
-- Module      : Network.Google.Resource.Compute.Addresses.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of addresses contained within the specified region.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.addresses.list@.
module Network.Google.Resource.Compute.Addresses.List
    (
    -- * REST Resource
      AddressesListResource

    -- * Creating a Request
    , addressesList
    , AddressesList

    -- * Request Lenses
    , alOrderBy
    , alProject
    , alFilter
    , alRegion
    , alPageToken
    , alMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.addresses.list@ method which the
-- 'AddressesList' request conforms to.
type AddressesListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "addresses" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :> Get '[JSON] AddressList

-- | Retrieves a list of addresses contained within the specified region.
--
-- /See:/ 'addressesList' smart constructor.
data AddressesList = AddressesList'
    { _alOrderBy :: !(Maybe Text)
    , _alProject :: !Text
    , _alFilter :: !(Maybe Text)
    , _alRegion :: !Text
    , _alPageToken :: !(Maybe Text)
    , _alMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddressesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alOrderBy'
--
-- * 'alProject'
--
-- * 'alFilter'
--
-- * 'alRegion'
--
-- * 'alPageToken'
--
-- * 'alMaxResults'
addressesList
    :: Text -- ^ 'alProject'
    -> Text -- ^ 'alRegion'
    -> AddressesList
addressesList pAlProject_ pAlRegion_ = 
    AddressesList'
    { _alOrderBy = Nothing
    , _alProject = pAlProject_
    , _alFilter = Nothing
    , _alRegion = pAlRegion_
    , _alPageToken = Nothing
    , _alMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
alOrderBy :: Lens' AddressesList (Maybe Text)
alOrderBy
  = lens _alOrderBy (\ s a -> s{_alOrderBy = a})

-- | Project ID for this request.
alProject :: Lens' AddressesList Text
alProject
  = lens _alProject (\ s a -> s{_alProject = a})

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
alFilter :: Lens' AddressesList (Maybe Text)
alFilter = lens _alFilter (\ s a -> s{_alFilter = a})

-- | Name of the region for this request.
alRegion :: Lens' AddressesList Text
alRegion = lens _alRegion (\ s a -> s{_alRegion = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
alPageToken :: Lens' AddressesList (Maybe Text)
alPageToken
  = lens _alPageToken (\ s a -> s{_alPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
alMaxResults :: Lens' AddressesList Word32
alMaxResults
  = lens _alMaxResults (\ s a -> s{_alMaxResults = a})
      . _Coerce

instance GoogleRequest AddressesList where
        type Rs AddressesList = AddressList
        type Scopes AddressesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient AddressesList'{..}
          = go _alProject _alRegion _alOrderBy _alFilter
              _alPageToken
              (Just _alMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy AddressesListResource)
                      mempty
