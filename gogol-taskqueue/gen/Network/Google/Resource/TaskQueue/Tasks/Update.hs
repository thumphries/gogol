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
-- Module      : Network.Google.Resource.TaskQueue.Tasks.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update tasks that are leased out of a TaskQueue.
--
-- /See:/ <https://developers.google.com/appengine/docs/python/taskqueue/rest TaskQueue API Reference> for @taskqueue.tasks.update@.
module Network.Google.Resource.TaskQueue.Tasks.Update
    (
    -- * REST Resource
      TasksUpdateResource

    -- * Creating a Request
    , tasksUpdate
    , TasksUpdate

    -- * Request Lenses
    , tuTaskQueue
    , tuProject
    , tuPayload
    , tuTask
    , tuNewLeaseSeconds
    ) where

import Network.Google.Prelude
import Network.Google.TaskQueue.Types

-- | A resource alias for @taskqueue.tasks.update@ method which the
-- 'TasksUpdate' request conforms to.
type TasksUpdateResource =
     "taskqueue" :>
       "v1beta2" :>
         "projects" :>
           Capture "project" Text :>
             "taskqueues" :>
               Capture "taskqueue" Text :>
                 "tasks" :>
                   Capture "task" Text :>
                     QueryParam "newLeaseSeconds" (Textual Int32) :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] Task :> Post '[JSON] Task

-- | Update tasks that are leased out of a TaskQueue.
--
-- /See:/ 'tasksUpdate' smart constructor.
data TasksUpdate = TasksUpdate'
    { _tuTaskQueue :: !Text
    , _tuProject :: !Text
    , _tuPayload :: !Task
    , _tuTask :: !Text
    , _tuNewLeaseSeconds :: !(Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TasksUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuTaskQueue'
--
-- * 'tuProject'
--
-- * 'tuPayload'
--
-- * 'tuTask'
--
-- * 'tuNewLeaseSeconds'
tasksUpdate
    :: Text -- ^ 'tuTaskQueue'
    -> Text -- ^ 'tuProject'
    -> Task -- ^ 'tuPayload'
    -> Text -- ^ 'tuTask'
    -> Int32 -- ^ 'tuNewLeaseSeconds'
    -> TasksUpdate
tasksUpdate pTuTaskQueue_ pTuProject_ pTuPayload_ pTuTask_ pTuNewLeaseSeconds_ = 
    TasksUpdate'
    { _tuTaskQueue = pTuTaskQueue_
    , _tuProject = pTuProject_
    , _tuPayload = pTuPayload_
    , _tuTask = pTuTask_
    , _tuNewLeaseSeconds = _Coerce # pTuNewLeaseSeconds_
    }

tuTaskQueue :: Lens' TasksUpdate Text
tuTaskQueue
  = lens _tuTaskQueue (\ s a -> s{_tuTaskQueue = a})

-- | The project under which the queue lies.
tuProject :: Lens' TasksUpdate Text
tuProject
  = lens _tuProject (\ s a -> s{_tuProject = a})

-- | Multipart request metadata.
tuPayload :: Lens' TasksUpdate Task
tuPayload
  = lens _tuPayload (\ s a -> s{_tuPayload = a})

tuTask :: Lens' TasksUpdate Text
tuTask = lens _tuTask (\ s a -> s{_tuTask = a})

-- | The new lease in seconds.
tuNewLeaseSeconds :: Lens' TasksUpdate Int32
tuNewLeaseSeconds
  = lens _tuNewLeaseSeconds
      (\ s a -> s{_tuNewLeaseSeconds = a})
      . _Coerce

instance GoogleRequest TasksUpdate where
        type Rs TasksUpdate = Task
        type Scopes TasksUpdate =
             '["https://www.googleapis.com/auth/taskqueue",
               "https://www.googleapis.com/auth/taskqueue.consumer"]
        requestClient TasksUpdate'{..}
          = go _tuProject _tuTaskQueue _tuTask
              (Just _tuNewLeaseSeconds)
              (Just AltJSON)
              _tuPayload
              taskQueueService
          where go
                  = buildClient (Proxy :: Proxy TasksUpdateResource)
                      mempty
