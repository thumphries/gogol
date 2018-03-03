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
-- Module      : Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of operation resources contained within the specified
-- project.
--
-- /See:/ <https://cloud.google.com/compute/docs/access/user-accounts/api/latest/ Cloud User Accounts API Reference> for @clouduseraccounts.globalAccountsOperations.list@.
module Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.List
    (
    -- * REST Resource
      GlobalAccountsOperationsListResource

    -- * Creating a Request
    , globalAccountsOperationsList
    , GlobalAccountsOperationsList

    -- * Request Lenses
    , gaolOrderBy
    , gaolProject
    , gaolFilter
    , gaolPageToken
    , gaolMaxResults
    ) where

import Network.Google.Prelude
import Network.Google.UserAccounts.Types

-- | A resource alias for @clouduseraccounts.globalAccountsOperations.list@ method which the
-- 'GlobalAccountsOperationsList' request conforms to.
type GlobalAccountsOperationsListResource =
     "clouduseraccounts" :>
       "beta" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "operations" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "alt" AltJSON :> Get '[JSON] OperationList

-- | Retrieves the list of operation resources contained within the specified
-- project.
--
-- /See:/ 'globalAccountsOperationsList' smart constructor.
data GlobalAccountsOperationsList = GlobalAccountsOperationsList'
    { _gaolOrderBy :: !(Maybe Text)
    , _gaolProject :: !Text
    , _gaolFilter :: !(Maybe Text)
    , _gaolPageToken :: !(Maybe Text)
    , _gaolMaxResults :: !(Textual Word32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GlobalAccountsOperationsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaolOrderBy'
--
-- * 'gaolProject'
--
-- * 'gaolFilter'
--
-- * 'gaolPageToken'
--
-- * 'gaolMaxResults'
globalAccountsOperationsList
    :: Text -- ^ 'gaolProject'
    -> GlobalAccountsOperationsList
globalAccountsOperationsList pGaolProject_ = 
    GlobalAccountsOperationsList'
    { _gaolOrderBy = Nothing
    , _gaolProject = pGaolProject_
    , _gaolFilter = Nothing
    , _gaolPageToken = Nothing
    , _gaolMaxResults = 500
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
gaolOrderBy :: Lens' GlobalAccountsOperationsList (Maybe Text)
gaolOrderBy
  = lens _gaolOrderBy (\ s a -> s{_gaolOrderBy = a})

-- | Project ID for this request.
gaolProject :: Lens' GlobalAccountsOperationsList Text
gaolProject
  = lens _gaolProject (\ s a -> s{_gaolProject = a})

-- | Sets a filter expression for filtering listed resources, in the form
-- filter={expression}. Your {expression} must be in the format: field_name
-- comparison_string literal_string. The field_name is the name of the
-- field you want to compare. Only atomic field types are supported
-- (string, number, boolean). The comparison_string must be either eq
-- (equals) or ne (not equals). The literal_string is the string value to
-- filter to. The literal value must be valid for the type of field you are
-- filtering by (string, number, boolean). For string fields, the literal
-- value is interpreted as a regular expression using RE2 syntax. The
-- literal value must match the entire field. For example, to filter for
-- instances that do not have a name of example-instance, you would use
-- filter=name ne example-instance. Compute Engine Beta API Only: If you
-- use filtering in the Beta API, you can also filter on nested fields. For
-- example, you could filter on instances that have set the
-- scheduling.automaticRestart field to true. In particular, use filtering
-- on nested fields to take advantage of instance labels to organize and
-- filter results based on label values. The Beta API also supports
-- filtering on multiple expressions by providing each separate expression
-- within parentheses. For example, (scheduling.automaticRestart eq true)
-- (zone eq us-central1-f). Multiple expressions are treated as AND
-- expressions, meaning that resources must match all expressions to pass
-- the filters.
gaolFilter :: Lens' GlobalAccountsOperationsList (Maybe Text)
gaolFilter
  = lens _gaolFilter (\ s a -> s{_gaolFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
gaolPageToken :: Lens' GlobalAccountsOperationsList (Maybe Text)
gaolPageToken
  = lens _gaolPageToken
      (\ s a -> s{_gaolPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests.
gaolMaxResults :: Lens' GlobalAccountsOperationsList Word32
gaolMaxResults
  = lens _gaolMaxResults
      (\ s a -> s{_gaolMaxResults = a})
      . _Coerce

instance GoogleRequest GlobalAccountsOperationsList
         where
        type Rs GlobalAccountsOperationsList = OperationList
        type Scopes GlobalAccountsOperationsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only",
               "https://www.googleapis.com/auth/cloud.useraccounts",
               "https://www.googleapis.com/auth/cloud.useraccounts.readonly"]
        requestClient GlobalAccountsOperationsList'{..}
          = go _gaolProject _gaolOrderBy _gaolFilter
              _gaolPageToken
              (Just _gaolMaxResults)
              (Just AltJSON)
              userAccountsService
          where go
                  = buildClient
                      (Proxy :: Proxy GlobalAccountsOperationsListResource)
                      mempty
