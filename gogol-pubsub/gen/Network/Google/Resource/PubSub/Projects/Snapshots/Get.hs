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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration details of a snapshot.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.get@.
module Network.Google.Resource.PubSub.Projects.Snapshots.Get
    (
    -- * REST Resource
      ProjectsSnapshotsGetResource

    -- * Creating a Request
    , projectsSnapshotsGet
    , ProjectsSnapshotsGet

    -- * Request Lenses
    , psgsXgafv
    , psgsSnapshot
    , psgsUploadProtocol
    , psgsPp
    , psgsAccessToken
    , psgsUploadType
    , psgsBearerToken
    , psgsCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.get@ method which the
-- 'ProjectsSnapshotsGet' request conforms to.
type ProjectsSnapshotsGetResource =
     "v1" :>
       Capture "snapshot" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :> Get '[JSON] Snapshot

-- | Gets the configuration details of a snapshot.
--
-- /See:/ 'projectsSnapshotsGet' smart constructor.
data ProjectsSnapshotsGet = ProjectsSnapshotsGet'
    { _psgsXgafv :: !(Maybe Xgafv)
    , _psgsSnapshot :: !Text
    , _psgsUploadProtocol :: !(Maybe Text)
    , _psgsPp :: !Bool
    , _psgsAccessToken :: !(Maybe Text)
    , _psgsUploadType :: !(Maybe Text)
    , _psgsBearerToken :: !(Maybe Text)
    , _psgsCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psgsXgafv'
--
-- * 'psgsSnapshot'
--
-- * 'psgsUploadProtocol'
--
-- * 'psgsPp'
--
-- * 'psgsAccessToken'
--
-- * 'psgsUploadType'
--
-- * 'psgsBearerToken'
--
-- * 'psgsCallback'
projectsSnapshotsGet
    :: Text -- ^ 'psgsSnapshot'
    -> ProjectsSnapshotsGet
projectsSnapshotsGet pPsgsSnapshot_ = 
    ProjectsSnapshotsGet'
    { _psgsXgafv = Nothing
    , _psgsSnapshot = pPsgsSnapshot_
    , _psgsUploadProtocol = Nothing
    , _psgsPp = True
    , _psgsAccessToken = Nothing
    , _psgsUploadType = Nothing
    , _psgsBearerToken = Nothing
    , _psgsCallback = Nothing
    }

-- | V1 error format.
psgsXgafv :: Lens' ProjectsSnapshotsGet (Maybe Xgafv)
psgsXgafv
  = lens _psgsXgafv (\ s a -> s{_psgsXgafv = a})

-- | The name of the snapshot to get. Format is
-- \`projects\/{project}\/snapshots\/{snap}\`.
psgsSnapshot :: Lens' ProjectsSnapshotsGet Text
psgsSnapshot
  = lens _psgsSnapshot (\ s a -> s{_psgsSnapshot = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
psgsUploadProtocol :: Lens' ProjectsSnapshotsGet (Maybe Text)
psgsUploadProtocol
  = lens _psgsUploadProtocol
      (\ s a -> s{_psgsUploadProtocol = a})

-- | Pretty-print response.
psgsPp :: Lens' ProjectsSnapshotsGet Bool
psgsPp = lens _psgsPp (\ s a -> s{_psgsPp = a})

-- | OAuth access token.
psgsAccessToken :: Lens' ProjectsSnapshotsGet (Maybe Text)
psgsAccessToken
  = lens _psgsAccessToken
      (\ s a -> s{_psgsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
psgsUploadType :: Lens' ProjectsSnapshotsGet (Maybe Text)
psgsUploadType
  = lens _psgsUploadType
      (\ s a -> s{_psgsUploadType = a})

-- | OAuth bearer token.
psgsBearerToken :: Lens' ProjectsSnapshotsGet (Maybe Text)
psgsBearerToken
  = lens _psgsBearerToken
      (\ s a -> s{_psgsBearerToken = a})

-- | JSONP
psgsCallback :: Lens' ProjectsSnapshotsGet (Maybe Text)
psgsCallback
  = lens _psgsCallback (\ s a -> s{_psgsCallback = a})

instance GoogleRequest ProjectsSnapshotsGet where
        type Rs ProjectsSnapshotsGet = Snapshot
        type Scopes ProjectsSnapshotsGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSnapshotsGet'{..}
          = go _psgsSnapshot _psgsXgafv _psgsUploadProtocol
              (Just _psgsPp)
              _psgsAccessToken
              _psgsUploadType
              _psgsBearerToken
              _psgsCallback
              (Just AltJSON)
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSnapshotsGetResource)
                      mempty
