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
-- Module      : Network.Google.Resource.Dataflow.Projects.Jobs.GetMetrics
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request the job status.
--
-- /See:/ <https://cloud.google.com/dataflow Dataflow API Reference> for @dataflow.projects.jobs.getMetrics@.
module Network.Google.Resource.Dataflow.Projects.Jobs.GetMetrics
    (
    -- * REST Resource
      ProjectsJobsGetMetricsResource

    -- * Creating a Request
    , projectsJobsGetMetrics
    , ProjectsJobsGetMetrics

    -- * Request Lenses
    , pjgmXgafv
    , pjgmJobId
    , pjgmUploadProtocol
    , pjgmLocation
    , pjgmStartTime
    , pjgmPp
    , pjgmAccessToken
    , pjgmUploadType
    , pjgmBearerToken
    , pjgmProjectId
    , pjgmCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.jobs.getMetrics@ method which the
-- 'ProjectsJobsGetMetrics' request conforms to.
type ProjectsJobsGetMetricsResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "jobs" :>
             Capture "jobId" Text :>
               "metrics" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "location" Text :>
                       QueryParam "startTime" DateTime' :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "alt" AltJSON :>
                                     Get '[JSON] JobMetrics

-- | Request the job status.
--
-- /See:/ 'projectsJobsGetMetrics' smart constructor.
data ProjectsJobsGetMetrics = ProjectsJobsGetMetrics'
    { _pjgmXgafv :: !(Maybe Xgafv)
    , _pjgmJobId :: !Text
    , _pjgmUploadProtocol :: !(Maybe Text)
    , _pjgmLocation :: !(Maybe Text)
    , _pjgmStartTime :: !(Maybe DateTime')
    , _pjgmPp :: !Bool
    , _pjgmAccessToken :: !(Maybe Text)
    , _pjgmUploadType :: !(Maybe Text)
    , _pjgmBearerToken :: !(Maybe Text)
    , _pjgmProjectId :: !Text
    , _pjgmCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsJobsGetMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjgmXgafv'
--
-- * 'pjgmJobId'
--
-- * 'pjgmUploadProtocol'
--
-- * 'pjgmLocation'
--
-- * 'pjgmStartTime'
--
-- * 'pjgmPp'
--
-- * 'pjgmAccessToken'
--
-- * 'pjgmUploadType'
--
-- * 'pjgmBearerToken'
--
-- * 'pjgmProjectId'
--
-- * 'pjgmCallback'
projectsJobsGetMetrics
    :: Text -- ^ 'pjgmJobId'
    -> Text -- ^ 'pjgmProjectId'
    -> ProjectsJobsGetMetrics
projectsJobsGetMetrics pPjgmJobId_ pPjgmProjectId_ = 
    ProjectsJobsGetMetrics'
    { _pjgmXgafv = Nothing
    , _pjgmJobId = pPjgmJobId_
    , _pjgmUploadProtocol = Nothing
    , _pjgmLocation = Nothing
    , _pjgmStartTime = Nothing
    , _pjgmPp = True
    , _pjgmAccessToken = Nothing
    , _pjgmUploadType = Nothing
    , _pjgmBearerToken = Nothing
    , _pjgmProjectId = pPjgmProjectId_
    , _pjgmCallback = Nothing
    }

-- | V1 error format.
pjgmXgafv :: Lens' ProjectsJobsGetMetrics (Maybe Xgafv)
pjgmXgafv
  = lens _pjgmXgafv (\ s a -> s{_pjgmXgafv = a})

-- | The job to get messages for.
pjgmJobId :: Lens' ProjectsJobsGetMetrics Text
pjgmJobId
  = lens _pjgmJobId (\ s a -> s{_pjgmJobId = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pjgmUploadProtocol :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmUploadProtocol
  = lens _pjgmUploadProtocol
      (\ s a -> s{_pjgmUploadProtocol = a})

-- | The location which contains the job specified by job_id.
pjgmLocation :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmLocation
  = lens _pjgmLocation (\ s a -> s{_pjgmLocation = a})

-- | Return only metric data that has changed since this time. Default is to
-- return all information about all metrics for the job.
pjgmStartTime :: Lens' ProjectsJobsGetMetrics (Maybe UTCTime)
pjgmStartTime
  = lens _pjgmStartTime
      (\ s a -> s{_pjgmStartTime = a})
      . mapping _DateTime

-- | Pretty-print response.
pjgmPp :: Lens' ProjectsJobsGetMetrics Bool
pjgmPp = lens _pjgmPp (\ s a -> s{_pjgmPp = a})

-- | OAuth access token.
pjgmAccessToken :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmAccessToken
  = lens _pjgmAccessToken
      (\ s a -> s{_pjgmAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pjgmUploadType :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmUploadType
  = lens _pjgmUploadType
      (\ s a -> s{_pjgmUploadType = a})

-- | OAuth bearer token.
pjgmBearerToken :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmBearerToken
  = lens _pjgmBearerToken
      (\ s a -> s{_pjgmBearerToken = a})

-- | A project id.
pjgmProjectId :: Lens' ProjectsJobsGetMetrics Text
pjgmProjectId
  = lens _pjgmProjectId
      (\ s a -> s{_pjgmProjectId = a})

-- | JSONP
pjgmCallback :: Lens' ProjectsJobsGetMetrics (Maybe Text)
pjgmCallback
  = lens _pjgmCallback (\ s a -> s{_pjgmCallback = a})

instance GoogleRequest ProjectsJobsGetMetrics where
        type Rs ProjectsJobsGetMetrics = JobMetrics
        type Scopes ProjectsJobsGetMetrics =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient ProjectsJobsGetMetrics'{..}
          = go _pjgmProjectId _pjgmJobId _pjgmXgafv
              _pjgmUploadProtocol
              _pjgmLocation
              _pjgmStartTime
              (Just _pjgmPp)
              _pjgmAccessToken
              _pjgmUploadType
              _pjgmBearerToken
              _pjgmCallback
              (Just AltJSON)
              dataflowService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsJobsGetMetricsResource)
                      mempty
