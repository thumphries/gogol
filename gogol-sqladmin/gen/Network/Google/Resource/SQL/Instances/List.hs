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
-- Module      : Network.Google.Resource.SQL.Instances.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists instances under a given project in the alphabetical order of the
-- instance name.
--
-- /See:/ <https://cloud.google.com/sql/docs/reference/latest Cloud SQL Administration API Reference> for @sql.instances.list@.
module Network.Google.Resource.SQL.Instances.List
    (
    -- * REST Resource
      InstancesListResource

    -- * Creating a Request
    , instancesList
    , InstancesList

    -- * Request Lenses
    , ilProject
    , ilFilter
    , ilPageToken
    , ilMaxResults
    ) where

import Network.Google.Prelude
import Network.Google.SQLAdmin.Types

-- | A resource alias for @sql.instances.list@ method which the
-- 'InstancesList' request conforms to.
type InstancesListResource =
     "sql" :>
       "v1beta4" :>
         "projects" :>
           Capture "project" Text :>
             "instances" :>
               QueryParam "filter" Text :>
                 QueryParam "pageToken" Text :>
                   QueryParam "maxResults" (Textual Word32) :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] InstancesListResponse

-- | Lists instances under a given project in the alphabetical order of the
-- instance name.
--
-- /See:/ 'instancesList' smart constructor.
data InstancesList = InstancesList'
    { _ilProject :: !Text
    , _ilFilter :: !(Maybe Text)
    , _ilPageToken :: !(Maybe Text)
    , _ilMaxResults :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstancesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilProject'
--
-- * 'ilFilter'
--
-- * 'ilPageToken'
--
-- * 'ilMaxResults'
instancesList
    :: Text -- ^ 'ilProject'
    -> InstancesList
instancesList pIlProject_ = 
    InstancesList'
    { _ilProject = pIlProject_
    , _ilFilter = Nothing
    , _ilPageToken = Nothing
    , _ilMaxResults = Nothing
    }

-- | Project ID of the project for which to list Cloud SQL instances.
ilProject :: Lens' InstancesList Text
ilProject
  = lens _ilProject (\ s a -> s{_ilProject = a})

-- | An expression for filtering the results of the request, such as by name
-- or label.
ilFilter :: Lens' InstancesList (Maybe Text)
ilFilter = lens _ilFilter (\ s a -> s{_ilFilter = a})

-- | A previously-returned page token representing part of the larger set of
-- results to view.
ilPageToken :: Lens' InstancesList (Maybe Text)
ilPageToken
  = lens _ilPageToken (\ s a -> s{_ilPageToken = a})

-- | The maximum number of results to return per response.
ilMaxResults :: Lens' InstancesList (Maybe Word32)
ilMaxResults
  = lens _ilMaxResults (\ s a -> s{_ilMaxResults = a})
      . mapping _Coerce

instance GoogleRequest InstancesList where
        type Rs InstancesList = InstancesListResponse
        type Scopes InstancesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/sqlservice.admin"]
        requestClient InstancesList'{..}
          = go _ilProject _ilFilter _ilPageToken _ilMaxResults
              (Just AltJSON)
              sQLAdminService
          where go
                  = buildClient (Proxy :: Proxy InstancesListResource)
                      mempty
