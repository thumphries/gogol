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
-- Module      : Network.Google.Resource.Compute.HTTPHealthChecks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of HttpHealthCheck resources available to the
-- specified project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.httpHealthChecks.list@.
module Network.Google.Resource.Compute.HTTPHealthChecks.List
    (
    -- * REST Resource
      HTTPHealthChecksListResource

    -- * Creating a Request
    , hTTPHealthChecksList
    , HTTPHealthChecksList

    -- * Request Lenses
    , httphclOrderBy
    , httphclProject
    , httphclFilter
    , httphclPageToken
    , httphclMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.httpHealthChecks.list@ method which the
-- 'HTTPHealthChecksList' request conforms to.
type HTTPHealthChecksListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "httpHealthChecks" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] HTTPHealthCheckList

-- | Retrieves the list of HttpHealthCheck resources available to the
-- specified project.
--
-- /See:/ 'hTTPHealthChecksList' smart constructor.
data HTTPHealthChecksList = HTTPHealthChecksList'
    { _httphclOrderBy :: !(Maybe Text)
    , _httphclProject :: !Text
    , _httphclFilter :: !(Maybe Text)
    , _httphclPageToken :: !(Maybe Text)
    , _httphclMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'HTTPHealthChecksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httphclOrderBy'
--
-- * 'httphclProject'
--
-- * 'httphclFilter'
--
-- * 'httphclPageToken'
--
-- * 'httphclMaxResults'
hTTPHealthChecksList
    :: Text -- ^ 'httphclProject'
    -> HTTPHealthChecksList
hTTPHealthChecksList pHttphclProject_ = 
    HTTPHealthChecksList'
    { _httphclOrderBy = Nothing
    , _httphclProject = pHttphclProject_
    , _httphclFilter = Nothing
    , _httphclPageToken = Nothing
    , _httphclMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
httphclOrderBy :: Lens' HTTPHealthChecksList (Maybe Text)
httphclOrderBy
  = lens _httphclOrderBy
      (\ s a -> s{_httphclOrderBy = a})

-- | Project ID for this request.
httphclProject :: Lens' HTTPHealthChecksList Text
httphclProject
  = lens _httphclProject
      (\ s a -> s{_httphclProject = a})

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
httphclFilter :: Lens' HTTPHealthChecksList (Maybe Text)
httphclFilter
  = lens _httphclFilter
      (\ s a -> s{_httphclFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
httphclPageToken :: Lens' HTTPHealthChecksList (Maybe Text)
httphclPageToken
  = lens _httphclPageToken
      (\ s a -> s{_httphclPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
httphclMaxResults :: Lens' HTTPHealthChecksList Word32
httphclMaxResults
  = lens _httphclMaxResults
      (\ s a -> s{_httphclMaxResults = a})
      . _Coerce

instance GoogleRequest HTTPHealthChecksList where
        type Rs HTTPHealthChecksList = HTTPHealthCheckList
        type Scopes HTTPHealthChecksList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient HTTPHealthChecksList'{..}
          = go _httphclProject _httphclOrderBy _httphclFilter
              _httphclPageToken
              (Just _httphclMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy HTTPHealthChecksListResource)
                      mempty
