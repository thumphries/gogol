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
-- Module      : Network.Google.Resource.Compute.ForwardingRules.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of forwarding rules.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.forwardingRules.aggregatedList@.
module Network.Google.Resource.Compute.ForwardingRules.AggregatedList
    (
    -- * REST Resource
      ForwardingRulesAggregatedListResource

    -- * Creating a Request
    , forwardingRulesAggregatedList
    , ForwardingRulesAggregatedList

    -- * Request Lenses
    , fralOrderBy
    , fralProject
    , fralFilter
    , fralPageToken
    , fralMaxResults
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.forwardingRules.aggregatedList@ method which the
-- 'ForwardingRulesAggregatedList' request conforms to.
type ForwardingRulesAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "forwardingRules" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] ForwardingRuleAggregatedList

-- | Retrieves an aggregated list of forwarding rules.
--
-- /See:/ 'forwardingRulesAggregatedList' smart constructor.
data ForwardingRulesAggregatedList = ForwardingRulesAggregatedList'
    { _fralOrderBy :: !(Maybe Text)
    , _fralProject :: !Text
    , _fralFilter :: !(Maybe Text)
    , _fralPageToken :: !(Maybe Text)
    , _fralMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ForwardingRulesAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fralOrderBy'
--
-- * 'fralProject'
--
-- * 'fralFilter'
--
-- * 'fralPageToken'
--
-- * 'fralMaxResults'
forwardingRulesAggregatedList
    :: Text -- ^ 'fralProject'
    -> ForwardingRulesAggregatedList
forwardingRulesAggregatedList pFralProject_ = 
    ForwardingRulesAggregatedList'
    { _fralOrderBy = Nothing
    , _fralProject = pFralProject_
    , _fralFilter = Nothing
    , _fralPageToken = Nothing
    , _fralMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
fralOrderBy :: Lens' ForwardingRulesAggregatedList (Maybe Text)
fralOrderBy
  = lens _fralOrderBy (\ s a -> s{_fralOrderBy = a})

-- | Project ID for this request.
fralProject :: Lens' ForwardingRulesAggregatedList Text
fralProject
  = lens _fralProject (\ s a -> s{_fralProject = a})

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
fralFilter :: Lens' ForwardingRulesAggregatedList (Maybe Text)
fralFilter
  = lens _fralFilter (\ s a -> s{_fralFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
fralPageToken :: Lens' ForwardingRulesAggregatedList (Maybe Text)
fralPageToken
  = lens _fralPageToken
      (\ s a -> s{_fralPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
fralMaxResults :: Lens' ForwardingRulesAggregatedList Word32
fralMaxResults
  = lens _fralMaxResults
      (\ s a -> s{_fralMaxResults = a})
      . _Coerce

instance GoogleRequest ForwardingRulesAggregatedList
         where
        type Rs ForwardingRulesAggregatedList =
             ForwardingRuleAggregatedList
        type Scopes ForwardingRulesAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient ForwardingRulesAggregatedList'{..}
          = go _fralProject _fralOrderBy _fralFilter
              _fralPageToken
              (Just _fralMaxResults)
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ForwardingRulesAggregatedListResource)
                      mempty
