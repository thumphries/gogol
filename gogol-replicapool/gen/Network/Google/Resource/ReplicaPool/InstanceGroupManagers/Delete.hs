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
-- Module      : Network.Google.Resource.ReplicaPool.InstanceGroupManagers.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the instance group manager and all instances contained within.
-- If you\'d like to delete the manager without deleting the instances, you
-- must first abandon the instances to remove them from the group.
--
-- /See:/ <https://developers.google.com/compute/docs/instance-groups/manager/v1beta2 Google Compute Engine Instance Group Manager API Reference> for @replicapool.instanceGroupManagers.delete@.
module Network.Google.Resource.ReplicaPool.InstanceGroupManagers.Delete
    (
    -- * REST Resource
      InstanceGroupManagersDeleteResource

    -- * Creating a Request
    , instanceGroupManagersDelete
    , InstanceGroupManagersDelete

    -- * Request Lenses
    , igmdProject
    , igmdInstanceGroupManager
    , igmdZone
    ) where

import Network.Google.Prelude
import Network.Google.ReplicaPool.Types

-- | A resource alias for @replicapool.instanceGroupManagers.delete@ method which the
-- 'InstanceGroupManagersDelete' request conforms to.
type InstanceGroupManagersDeleteResource =
     "replicapool" :>
       "v1beta2" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "instanceGroupManagers" :>
                   Capture "instanceGroupManager" Text :>
                     QueryParam "alt" AltJSON :> Delete '[JSON] Operation

-- | Deletes the instance group manager and all instances contained within.
-- If you\'d like to delete the manager without deleting the instances, you
-- must first abandon the instances to remove them from the group.
--
-- /See:/ 'instanceGroupManagersDelete' smart constructor.
data InstanceGroupManagersDelete = InstanceGroupManagersDelete'
    { _igmdProject :: !Text
    , _igmdInstanceGroupManager :: !Text
    , _igmdZone :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceGroupManagersDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igmdProject'
--
-- * 'igmdInstanceGroupManager'
--
-- * 'igmdZone'
instanceGroupManagersDelete
    :: Text -- ^ 'igmdProject'
    -> Text -- ^ 'igmdInstanceGroupManager'
    -> Text -- ^ 'igmdZone'
    -> InstanceGroupManagersDelete
instanceGroupManagersDelete pIgmdProject_ pIgmdInstanceGroupManager_ pIgmdZone_ = 
    InstanceGroupManagersDelete'
    { _igmdProject = pIgmdProject_
    , _igmdInstanceGroupManager = pIgmdInstanceGroupManager_
    , _igmdZone = pIgmdZone_
    }

-- | The Google Developers Console project name.
igmdProject :: Lens' InstanceGroupManagersDelete Text
igmdProject
  = lens _igmdProject (\ s a -> s{_igmdProject = a})

-- | Name of the Instance Group Manager resource to delete.
igmdInstanceGroupManager :: Lens' InstanceGroupManagersDelete Text
igmdInstanceGroupManager
  = lens _igmdInstanceGroupManager
      (\ s a -> s{_igmdInstanceGroupManager = a})

-- | The name of the zone in which the instance group manager resides.
igmdZone :: Lens' InstanceGroupManagersDelete Text
igmdZone = lens _igmdZone (\ s a -> s{_igmdZone = a})

instance GoogleRequest InstanceGroupManagersDelete
         where
        type Rs InstanceGroupManagersDelete = Operation
        type Scopes InstanceGroupManagersDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute"]
        requestClient InstanceGroupManagersDelete'{..}
          = go _igmdProject _igmdZone _igmdInstanceGroupManager
              (Just AltJSON)
              replicaPoolService
          where go
                  = buildClient
                      (Proxy :: Proxy InstanceGroupManagersDeleteResource)
                      mempty
