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
-- Module      : Network.Google.Resource.Dataflow.Projects.Jobs.Messages.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request the job status.
--
-- /See:/ <https://cloud.google.com/dataflow Dataflow API Reference> for @dataflow.projects.jobs.messages.list@.
module Network.Google.Resource.Dataflow.Projects.Jobs.Messages.List
    (
    -- * REST Resource
      ProjectsJobsMessagesListResource

    -- * Creating a Request
    , projectsJobsMessagesList
    , ProjectsJobsMessagesList

    -- * Request Lenses
    , pjmlXgafv
    , pjmlJobId
    , pjmlUploadProtocol
    , pjmlLocation
    , pjmlStartTime
    , pjmlPp
    , pjmlAccessToken
    , pjmlUploadType
    , pjmlBearerToken
    , pjmlEndTime
    , pjmlMinimumImportance
    , pjmlPageToken
    , pjmlProjectId
    , pjmlPageSize
    , pjmlCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.jobs.messages.list@ method which the
-- 'ProjectsJobsMessagesList' request conforms to.
type ProjectsJobsMessagesListResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "jobs" :>
             Capture "jobId" Text :>
               "messages" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "location" Text :>
                       QueryParam "startTime" DateTime' :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "endTime" DateTime' :>
                                   QueryParam "minimumImportance" Text :>
                                     QueryParam "pageToken" Text :>
                                       QueryParam "pageSize" (Textual Int32) :>
                                         QueryParam "callback" Text :>
                                           QueryParam "alt" AltJSON :>
                                             Get '[JSON] ListJobMessagesResponse

-- | Request the job status.
--
-- /See:/ 'projectsJobsMessagesList' smart constructor.
data ProjectsJobsMessagesList = ProjectsJobsMessagesList'
    { _pjmlXgafv :: !(Maybe Xgafv)
    , _pjmlJobId :: !Text
    , _pjmlUploadProtocol :: !(Maybe Text)
    , _pjmlLocation :: !(Maybe Text)
    , _pjmlStartTime :: !(Maybe DateTime')
    , _pjmlPp :: !Bool
    , _pjmlAccessToken :: !(Maybe Text)
    , _pjmlUploadType :: !(Maybe Text)
    , _pjmlBearerToken :: !(Maybe Text)
    , _pjmlEndTime :: !(Maybe DateTime')
    , _pjmlMinimumImportance :: !(Maybe Text)
    , _pjmlPageToken :: !(Maybe Text)
    , _pjmlProjectId :: !Text
    , _pjmlPageSize :: !(Maybe (Textual Int32))
    , _pjmlCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsJobsMessagesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjmlXgafv'
--
-- * 'pjmlJobId'
--
-- * 'pjmlUploadProtocol'
--
-- * 'pjmlLocation'
--
-- * 'pjmlStartTime'
--
-- * 'pjmlPp'
--
-- * 'pjmlAccessToken'
--
-- * 'pjmlUploadType'
--
-- * 'pjmlBearerToken'
--
-- * 'pjmlEndTime'
--
-- * 'pjmlMinimumImportance'
--
-- * 'pjmlPageToken'
--
-- * 'pjmlProjectId'
--
-- * 'pjmlPageSize'
--
-- * 'pjmlCallback'
projectsJobsMessagesList
    :: Text -- ^ 'pjmlJobId'
    -> Text -- ^ 'pjmlProjectId'
    -> ProjectsJobsMessagesList
projectsJobsMessagesList pPjmlJobId_ pPjmlProjectId_ = 
    ProjectsJobsMessagesList'
    { _pjmlXgafv = Nothing
    , _pjmlJobId = pPjmlJobId_
    , _pjmlUploadProtocol = Nothing
    , _pjmlLocation = Nothing
    , _pjmlStartTime = Nothing
    , _pjmlPp = True
    , _pjmlAccessToken = Nothing
    , _pjmlUploadType = Nothing
    , _pjmlBearerToken = Nothing
    , _pjmlEndTime = Nothing
    , _pjmlMinimumImportance = Nothing
    , _pjmlPageToken = Nothing
    , _pjmlProjectId = pPjmlProjectId_
    , _pjmlPageSize = Nothing
    , _pjmlCallback = Nothing
    }

-- | V1 error format.
pjmlXgafv :: Lens' ProjectsJobsMessagesList (Maybe Xgafv)
pjmlXgafv
  = lens _pjmlXgafv (\ s a -> s{_pjmlXgafv = a})

-- | The job to get messages about.
pjmlJobId :: Lens' ProjectsJobsMessagesList Text
pjmlJobId
  = lens _pjmlJobId (\ s a -> s{_pjmlJobId = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pjmlUploadProtocol :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlUploadProtocol
  = lens _pjmlUploadProtocol
      (\ s a -> s{_pjmlUploadProtocol = a})

-- | The location which contains the job specified by job_id.
pjmlLocation :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlLocation
  = lens _pjmlLocation (\ s a -> s{_pjmlLocation = a})

-- | If specified, return only messages with timestamps >= start_time. The
-- default is the job creation time (i.e. beginning of messages).
pjmlStartTime :: Lens' ProjectsJobsMessagesList (Maybe UTCTime)
pjmlStartTime
  = lens _pjmlStartTime
      (\ s a -> s{_pjmlStartTime = a})
      . mapping _DateTime

-- | Pretty-print response.
pjmlPp :: Lens' ProjectsJobsMessagesList Bool
pjmlPp = lens _pjmlPp (\ s a -> s{_pjmlPp = a})

-- | OAuth access token.
pjmlAccessToken :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlAccessToken
  = lens _pjmlAccessToken
      (\ s a -> s{_pjmlAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pjmlUploadType :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlUploadType
  = lens _pjmlUploadType
      (\ s a -> s{_pjmlUploadType = a})

-- | OAuth bearer token.
pjmlBearerToken :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlBearerToken
  = lens _pjmlBearerToken
      (\ s a -> s{_pjmlBearerToken = a})

-- | Return only messages with timestamps \< end_time. The default is now
-- (i.e. return up to the latest messages available).
pjmlEndTime :: Lens' ProjectsJobsMessagesList (Maybe UTCTime)
pjmlEndTime
  = lens _pjmlEndTime (\ s a -> s{_pjmlEndTime = a}) .
      mapping _DateTime

-- | Filter to only get messages with importance >= level
pjmlMinimumImportance :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlMinimumImportance
  = lens _pjmlMinimumImportance
      (\ s a -> s{_pjmlMinimumImportance = a})

-- | If supplied, this should be the value of next_page_token returned by an
-- earlier call. This will cause the next page of results to be returned.
pjmlPageToken :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlPageToken
  = lens _pjmlPageToken
      (\ s a -> s{_pjmlPageToken = a})

-- | A project id.
pjmlProjectId :: Lens' ProjectsJobsMessagesList Text
pjmlProjectId
  = lens _pjmlProjectId
      (\ s a -> s{_pjmlProjectId = a})

-- | If specified, determines the maximum number of messages to return. If
-- unspecified, the service may choose an appropriate default, or may
-- return an arbitrarily large number of results.
pjmlPageSize :: Lens' ProjectsJobsMessagesList (Maybe Int32)
pjmlPageSize
  = lens _pjmlPageSize (\ s a -> s{_pjmlPageSize = a})
      . mapping _Coerce

-- | JSONP
pjmlCallback :: Lens' ProjectsJobsMessagesList (Maybe Text)
pjmlCallback
  = lens _pjmlCallback (\ s a -> s{_pjmlCallback = a})

instance GoogleRequest ProjectsJobsMessagesList where
        type Rs ProjectsJobsMessagesList =
             ListJobMessagesResponse
        type Scopes ProjectsJobsMessagesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient ProjectsJobsMessagesList'{..}
          = go _pjmlProjectId _pjmlJobId _pjmlXgafv
              _pjmlUploadProtocol
              _pjmlLocation
              _pjmlStartTime
              (Just _pjmlPp)
              _pjmlAccessToken
              _pjmlUploadType
              _pjmlBearerToken
              _pjmlEndTime
              _pjmlMinimumImportance
              _pjmlPageToken
              _pjmlPageSize
              _pjmlCallback
              (Just AltJSON)
              dataflowService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsJobsMessagesListResource)
                      mempty
