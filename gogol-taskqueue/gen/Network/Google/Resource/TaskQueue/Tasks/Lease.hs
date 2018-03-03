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
-- Module      : Network.Google.Resource.TaskQueue.Tasks.Lease
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lease 1 or more tasks from a TaskQueue.
--
-- /See:/ <https://developers.google.com/appengine/docs/python/taskqueue/rest TaskQueue API Reference> for @taskqueue.tasks.lease@.
module Network.Google.Resource.TaskQueue.Tasks.Lease
    (
    -- * REST Resource
      TasksLeaseResource

    -- * Creating a Request
    , tasksLease
    , TasksLease

    -- * Request Lenses
    , tlTaskQueue
    , tlTag
    , tlProject
    , tlNumTasks
    , tlLeaseSecs
    , tlGroupByTag
    ) where

import Network.Google.Prelude
import Network.Google.TaskQueue.Types

-- | A resource alias for @taskqueue.tasks.lease@ method which the
-- 'TasksLease' request conforms to.
type TasksLeaseResource =
     "taskqueue" :>
       "v1beta2" :>
         "projects" :>
           Capture "project" Text :>
             "taskqueues" :>
               Capture "taskqueue" Text :>
                 "tasks" :>
                   "lease" :>
                     QueryParam "numTasks" (Textual Int32) :>
                       QueryParam "leaseSecs" (Textual Int32) :>
                         QueryParam "tag" Text :>
                           QueryParam "groupByTag" Bool :>
                             QueryParam "alt" AltJSON :> Post '[JSON] Tasks

-- | Lease 1 or more tasks from a TaskQueue.
--
-- /See:/ 'tasksLease' smart constructor.
data TasksLease = TasksLease'
    { _tlTaskQueue :: !Text
    , _tlTag :: !(Maybe Text)
    , _tlProject :: !Text
    , _tlNumTasks :: !(Textual Int32)
    , _tlLeaseSecs :: !(Textual Int32)
    , _tlGroupByTag :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TasksLease' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlTaskQueue'
--
-- * 'tlTag'
--
-- * 'tlProject'
--
-- * 'tlNumTasks'
--
-- * 'tlLeaseSecs'
--
-- * 'tlGroupByTag'
tasksLease
    :: Text -- ^ 'tlTaskQueue'
    -> Text -- ^ 'tlProject'
    -> Int32 -- ^ 'tlNumTasks'
    -> Int32 -- ^ 'tlLeaseSecs'
    -> TasksLease
tasksLease pTlTaskQueue_ pTlProject_ pTlNumTasks_ pTlLeaseSecs_ = 
    TasksLease'
    { _tlTaskQueue = pTlTaskQueue_
    , _tlTag = Nothing
    , _tlProject = pTlProject_
    , _tlNumTasks = _Coerce # pTlNumTasks_
    , _tlLeaseSecs = _Coerce # pTlLeaseSecs_
    , _tlGroupByTag = Nothing
    }

-- | The taskqueue to lease a task from.
tlTaskQueue :: Lens' TasksLease Text
tlTaskQueue
  = lens _tlTaskQueue (\ s a -> s{_tlTaskQueue = a})

-- | The tag allowed for tasks in the response. Must only be specified if
-- group_by_tag is true. If group_by_tag is true and tag is not specified
-- the tag will be that of the oldest task by eta, i.e. the first available
-- tag
tlTag :: Lens' TasksLease (Maybe Text)
tlTag = lens _tlTag (\ s a -> s{_tlTag = a})

-- | The project under which the queue lies.
tlProject :: Lens' TasksLease Text
tlProject
  = lens _tlProject (\ s a -> s{_tlProject = a})

-- | The number of tasks to lease.
tlNumTasks :: Lens' TasksLease Int32
tlNumTasks
  = lens _tlNumTasks (\ s a -> s{_tlNumTasks = a}) .
      _Coerce

-- | The lease in seconds.
tlLeaseSecs :: Lens' TasksLease Int32
tlLeaseSecs
  = lens _tlLeaseSecs (\ s a -> s{_tlLeaseSecs = a}) .
      _Coerce

-- | When true, all returned tasks will have the same tag
tlGroupByTag :: Lens' TasksLease (Maybe Bool)
tlGroupByTag
  = lens _tlGroupByTag (\ s a -> s{_tlGroupByTag = a})

instance GoogleRequest TasksLease where
        type Rs TasksLease = Tasks
        type Scopes TasksLease =
             '["https://www.googleapis.com/auth/taskqueue",
               "https://www.googleapis.com/auth/taskqueue.consumer"]
        requestClient TasksLease'{..}
          = go _tlProject _tlTaskQueue (Just _tlNumTasks)
              (Just _tlLeaseSecs)
              _tlTag
              _tlGroupByTag
              (Just AltJSON)
              taskQueueService
          where go
                  = buildClient (Proxy :: Proxy TasksLeaseResource)
                      mempty
