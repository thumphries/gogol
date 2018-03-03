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
-- Module      : Network.Google.Resource.Compute.MachineTypes.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of machine types available to the specified project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.machineTypes.list@.
module Network.Google.Resource.Compute.MachineTypes.List
    (
    -- * REST Resource
      MachineTypesListResource

    -- * Creating a Request
    , machineTypesList
    , MachineTypesList

    -- * Request Lenses
    , mtlOrderBy
    , mtlProject
    , mtlZone
    , mtlFilter
    , mtlPageToken
    , mtlMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.machineTypes.list@ method which the
-- 'MachineTypesList' request conforms to.
type MachineTypesListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "machineTypes" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] MachineTypeList

-- | Retrieves a list of machine types available to the specified project.
--
-- /See:/ 'machineTypesList' smart constructor.
data MachineTypesList = MachineTypesList'
    { _mtlOrderBy :: !(Maybe Text)
    , _mtlProject :: !Text
    , _mtlZone :: !Text
    , _mtlFilter :: !(Maybe Text)
    , _mtlPageToken :: !(Maybe Text)
    , _mtlMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MachineTypesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtlOrderBy'
--
-- * 'mtlProject'
--
-- * 'mtlZone'
--
-- * 'mtlFilter'
--
-- * 'mtlPageToken'
--
-- * 'mtlMaxResults'
machineTypesList
    :: Text -- ^ 'mtlProject'
    -> Text -- ^ 'mtlZone'
    -> MachineTypesList
machineTypesList pMtlProject_ pMtlZone_ = 
    MachineTypesList'
    { _mtlOrderBy = Nothing
    , _mtlProject = pMtlProject_
    , _mtlZone = pMtlZone_
    , _mtlFilter = Nothing
    , _mtlPageToken = Nothing
    , _mtlMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
mtlOrderBy :: Lens' MachineTypesList (Maybe Text)
mtlOrderBy
  = lens _mtlOrderBy (\ s a -> s{_mtlOrderBy = a})

-- | Project ID for this request.
mtlProject :: Lens' MachineTypesList Text
mtlProject
  = lens _mtlProject (\ s a -> s{_mtlProject = a})

-- | The name of the zone for this request.
mtlZone :: Lens' MachineTypesList Text
mtlZone = lens _mtlZone (\ s a -> s{_mtlZone = a})

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
mtlFilter :: Lens' MachineTypesList (Maybe Text)
mtlFilter
  = lens _mtlFilter (\ s a -> s{_mtlFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
mtlPageToken :: Lens' MachineTypesList (Maybe Text)
mtlPageToken
  = lens _mtlPageToken (\ s a -> s{_mtlPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
mtlMaxResults :: Lens' MachineTypesList Word32
mtlMaxResults
  = lens _mtlMaxResults
      (\ s a -> s{_mtlMaxResults = a})
      . _Coerce

instance GoogleRequest MachineTypesList where
        type Rs MachineTypesList = MachineTypeList
        type Scopes MachineTypesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient MachineTypesList'{..}
          = go _mtlProject _mtlZone _mtlOrderBy _mtlFilter
              _mtlPageToken
              (Just _mtlMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy MachineTypesListResource)
                      mempty
