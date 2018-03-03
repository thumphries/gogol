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
-- Module      : Network.Google.Resource.Compute.Firewalls.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of firewall rules available to the specified project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.firewalls.list@.
module Network.Google.Resource.Compute.Firewalls.List
    (
    -- * REST Resource
      FirewallsListResource

    -- * Creating a Request
    , firewallsList
    , FirewallsList

    -- * Request Lenses
    , flOrderBy
    , flProject
    , flFilter
    , flPageToken
    , flMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.firewalls.list@ method which the
-- 'FirewallsList' request conforms to.
type FirewallsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "firewalls" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :> Get '[JSON] FirewallList

-- | Retrieves the list of firewall rules available to the specified project.
--
-- /See:/ 'firewallsList' smart constructor.
data FirewallsList = FirewallsList'
    { _flOrderBy :: !(Maybe Text)
    , _flProject :: !Text
    , _flFilter :: !(Maybe Text)
    , _flPageToken :: !(Maybe Text)
    , _flMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FirewallsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flOrderBy'
--
-- * 'flProject'
--
-- * 'flFilter'
--
-- * 'flPageToken'
--
-- * 'flMaxResults'
firewallsList
    :: Text -- ^ 'flProject'
    -> FirewallsList
firewallsList pFlProject_ = 
    FirewallsList'
    { _flOrderBy = Nothing
    , _flProject = pFlProject_
    , _flFilter = Nothing
    , _flPageToken = Nothing
    , _flMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
flOrderBy :: Lens' FirewallsList (Maybe Text)
flOrderBy
  = lens _flOrderBy (\ s a -> s{_flOrderBy = a})

-- | Project ID for this request.
flProject :: Lens' FirewallsList Text
flProject
  = lens _flProject (\ s a -> s{_flProject = a})

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
flFilter :: Lens' FirewallsList (Maybe Text)
flFilter = lens _flFilter (\ s a -> s{_flFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
flPageToken :: Lens' FirewallsList (Maybe Text)
flPageToken
  = lens _flPageToken (\ s a -> s{_flPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
flMaxResults :: Lens' FirewallsList Word32
flMaxResults
  = lens _flMaxResults (\ s a -> s{_flMaxResults = a})
      . _Coerce

instance GoogleRequest FirewallsList where
        type Rs FirewallsList = FirewallList
        type Scopes FirewallsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient FirewallsList'{..}
          = go _flProject _flOrderBy _flFilter _flPageToken
              (Just _flMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy FirewallsListResource)
                      mempty
