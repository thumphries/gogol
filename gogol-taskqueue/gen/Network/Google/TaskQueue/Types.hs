{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.TaskQueue.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
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
    , taskQueueConsumerScope
    , taskQueueScope

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
    , ttEnQueueTimestamp
    , ttTag
    , ttKind
    , ttQueueName
    , ttPayloadBase64
    , ttId
    , ttLeaseTimestamp
    ) where

import Network.Google.Prelude
import Network.Google.TaskQueue.Types.Product
import Network.Google.TaskQueue.Types.Sum

-- | Default request referring to version 'v1beta2' of the TaskQueue API. This contains the host and root path used as a starting point for constructing service requests.
taskQueueService :: ServiceConfig
taskQueueService
  = defaultService (ServiceId "taskqueue:v1beta2")
      "www.googleapis.com"

-- | Consume Tasks from your Taskqueues
taskQueueConsumerScope :: Proxy '["https://www.googleapis.com/auth/taskqueue.consumer"]
taskQueueConsumerScope = Proxy;

-- | Manage your Tasks and Taskqueues
taskQueueScope :: Proxy '["https://www.googleapis.com/auth/taskqueue"]
taskQueueScope = Proxy;
