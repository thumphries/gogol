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
-- Module      : Network.Google.Resource.TaskQueue.Tasks.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Insert a new task in a TaskQueue
--
-- /See:/ <https://developers.google.com/appengine/docs/python/taskqueue/rest TaskQueue API Reference> for @taskqueue.tasks.insert@.
module Network.Google.Resource.TaskQueue.Tasks.Insert
    (
    -- * REST Resource
      TasksInsertResource

    -- * Creating a Request
    , tasksInsert
    , TasksInsert

    -- * Request Lenses
    , tiTaskQueue
    , tiProject
    , tiPayload
    ) where

import Network.Google.Prelude
import Network.Google.TaskQueue.Types

-- | A resource alias for @taskqueue.tasks.insert@ method which the
-- 'TasksInsert' request conforms to.
type TasksInsertResource =
     "taskqueue" :>
       "v1beta2" :>
         "projects" :>
           Capture "project" Text :>
             "taskqueues" :>
               Capture "taskqueue" Text :>
                 "tasks" :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] Task :> Post '[JSON] Task

-- | Insert a new task in a TaskQueue
--
-- /See:/ 'tasksInsert' smart constructor.
data TasksInsert = TasksInsert'
    { _tiTaskQueue :: !Text
    , _tiProject :: !Text
    , _tiPayload :: !Task
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TasksInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiTaskQueue'
--
-- * 'tiProject'
--
-- * 'tiPayload'
tasksInsert
    :: Text -- ^ 'tiTaskQueue'
    -> Text -- ^ 'tiProject'
    -> Task -- ^ 'tiPayload'
    -> TasksInsert
tasksInsert pTiTaskQueue_ pTiProject_ pTiPayload_ = 
    TasksInsert'
    { _tiTaskQueue = pTiTaskQueue_
    , _tiProject = pTiProject_
    , _tiPayload = pTiPayload_
    }

-- | The taskqueue to insert the task into
tiTaskQueue :: Lens' TasksInsert Text
tiTaskQueue
  = lens _tiTaskQueue (\ s a -> s{_tiTaskQueue = a})

-- | The project under which the queue lies
tiProject :: Lens' TasksInsert Text
tiProject
  = lens _tiProject (\ s a -> s{_tiProject = a})

-- | Multipart request metadata.
tiPayload :: Lens' TasksInsert Task
tiPayload
  = lens _tiPayload (\ s a -> s{_tiPayload = a})

instance GoogleRequest TasksInsert where
        type Rs TasksInsert = Task
        type Scopes TasksInsert =
             '["https://www.googleapis.com/auth/taskqueue",
               "https://www.googleapis.com/auth/taskqueue.consumer"]
        requestClient TasksInsert'{..}
          = go _tiProject _tiTaskQueue (Just AltJSON)
              _tiPayload
              taskQueueService
          where go
                  = buildClient (Proxy :: Proxy TasksInsertResource)
                      mempty
