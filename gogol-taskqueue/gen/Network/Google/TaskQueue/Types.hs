{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.TaskQueue.Types
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.TaskQueue.Types
    (
    -- * Service Configuration
      taskQueueService

    -- * OAuth Scopes
    , taskqueueConsumerScope
    , taskqueueScope

    -- * Tasks2
    , Tasks2
    , tasks2
    , tKind
    , tItems

    -- * TaskQueue
    , TaskQueue
    , taskQueue
    , tqKind
    , tqStats
    , tqMaxLeases
    , tqId
    , tqACL

    -- * TaskQueueACL
    , TaskQueueACL
    , taskQueueACL
    , tqaProducerEmails
    , tqaAdminEmails
    , tqaConsumerEmails

    -- * TaskQueueStats
    , TaskQueueStats
    , taskQueueStats
    , tqsTotalTasks
    , tqsOldestTask
    , tqsLeasedLastHour
    , tqsLeasedLastMinute

    -- * Tasks
    , Tasks
    , tasks
    , tasKind
    , tasItems

    -- * Task
    , Task
    , task
    , ttRetryCount
    , ttEnqueueTimestamp
    , ttTag
    , ttKind
    , ttQueueName
    , ttPayloadBase64
    , ttId
    , ttLeaseTimestamp
    ) where

import           Network.Google.Prelude
import           Network.Google.TaskQueue.Types.Product
import           Network.Google.TaskQueue.Types.Sum

-- | Default request referring to version 'v1beta2' of the TaskQueue API. This contains the host and root path used as a starting point for constructing service requests.
taskQueueService :: Service
taskQueueService
  = defaultService (ServiceId "taskqueue:v1beta2")
      "www.googleapis.com"
      "taskqueue/v1beta2/projects/"

-- | Consume Tasks from your Taskqueues
taskqueueConsumerScope :: OAuthScope
taskqueueConsumerScope = OAuthScope "https://www.googleapis.com/auth/taskqueue.consumer";

-- | Manage your Tasks and Taskqueues
taskqueueScope :: OAuthScope
taskqueueScope = OAuthScope "https://www.googleapis.com/auth/taskqueue";