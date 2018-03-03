{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Genomics.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Genomics.Types
    (
    -- * Service Configuration
      genomicsService

    -- * OAuth Scopes
    , genomicsScope
    , cloudPlatformScope

    -- * ContainerStartedEventPortMAppings
    , ContainerStartedEventPortMAppings
    , containerStartedEventPortMAppings
    , csepmaAddtional

    -- * Event
    , Event
    , event
    , eDetails
    , eTimestamp
    , eDescription

    -- * CheckInRequestEvent
    , CheckInRequestEvent
    , checkInRequestEvent
    , cireAddtional

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * OperationSchema
    , OperationSchema
    , operationSchema
    , osAddtional

    -- * RunPipelineRequestLabels
    , RunPipelineRequestLabels
    , runPipelineRequestLabels
    , rprlAddtional

    -- * ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorNextPageToken
    , lorOperations

    -- * FailedEvent
    , FailedEvent
    , failedEvent
    , feCause
    , feCode

    -- * CancelOperationRequest
    , CancelOperationRequest
    , cancelOperationRequest

    -- * ContainerStoppedEvent
    , ContainerStoppedEvent
    , containerStoppedEvent
    , cseExitStatus
    , cseActionId

    -- * CheckInResponse
    , CheckInResponse
    , checkInResponse
    , cirDeadline
    , cirMetadata

    -- * WorkerAssignedEvent
    , WorkerAssignedEvent
    , workerAssignedEvent
    , waeZone
    , waeInstance

    -- * OperationMetadataLabels
    , OperationMetadataLabels
    , operationMetadataLabels
    , omlAddtional

    -- * Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- * Empty
    , Empty
    , empty

    -- * Disk
    , Disk
    , disk
    , dSourceImage
    , dSizeGb
    , dName
    , dType

    -- * UnexpectedExitStatusEvent
    , UnexpectedExitStatusEvent
    , unexpectedExitStatusEvent
    , ueseExitStatus
    , ueseActionId

    -- * DelayedEvent
    , DelayedEvent
    , delayedEvent
    , deMetrics
    , deCause

    -- * ActionPortMAppings
    , ActionPortMAppings
    , actionPortMAppings
    , apmaAddtional

    -- * MetadataLabels
    , MetadataLabels
    , metadataLabels
    , mlAddtional

    -- * CheckInRequest
    , CheckInRequest
    , checkInRequest
    , cirEvent
    , cirResult
    , cirDeadlineExpired

    -- * PullStoppedEvent
    , PullStoppedEvent
    , pullStoppedEvent
    , pseImageURI

    -- * ActionLabels
    , ActionLabels
    , actionLabels
    , alAddtional

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * Network
    , Network
    , network
    , nUsePrivateAddress
    , nName

    -- * EventDetails
    , EventDetails
    , eventDetails
    , edAddtional

    -- * Action
    , Action
    , action
    , aCommands
    , aFlags
    , aEnvironment
    , aEntrypoint
    , aPortMAppings
    , aMounts
    , aImageURI
    , aName
    , aLabels
    , aPidNamespace

    -- * OperationMetadataRequest
    , OperationMetadataRequest
    , operationMetadataRequest
    , omrAddtional

    -- * Resources
    , Resources
    , resources
    , rZones
    , rRegions
    , rVirtualMachine
    , rProjectId

    -- * VirtualMachineLabels
    , VirtualMachineLabels
    , virtualMachineLabels
    , vmlAddtional

    -- * OperationMetadataRuntimeMetadata
    , OperationMetadataRuntimeMetadata
    , operationMetadataRuntimeMetadata
    , omrmAddtional

    -- * VirtualMachine
    , VirtualMachine
    , virtualMachine
    , vmNetwork
    , vmCPUPlatform
    , vmServiceAccount
    , vmMachineType
    , vmLabels
    , vmBootDiskSizeGb
    , vmDisks
    , vmBootImage
    , vmPreemptible

    -- * ServiceAccount
    , ServiceAccount
    , serviceAccount
    , saEmail
    , saScopes

    -- * CheckInResponseMetadata
    , CheckInResponseMetadata
    , checkInResponseMetadata
    , cirmAddtional

    -- * PipelineEnvironment
    , PipelineEnvironment
    , pipelineEnvironment
    , peAddtional

    -- * WorkerReleasedEvent
    , WorkerReleasedEvent
    , workerReleasedEvent
    , wreZone
    , wreInstance

    -- * ContainerStartedEvent
    , ContainerStartedEvent
    , containerStartedEvent
    , cIPAddress
    , cActionId
    , cPortMAppings

    -- * FailedEventCode
    , FailedEventCode (..)

    -- * Xgafv
    , Xgafv (..)

    -- * RunPipelineRequest
    , RunPipelineRequest
    , runPipelineRequest
    , rprPipeline
    , rprLabels

    -- * Pipeline
    , Pipeline
    , pipeline
    , pActions
    , pEnvironment
    , pResources

    -- * ImportVariantsResponse
    , ImportVariantsResponse
    , importVariantsResponse
    , ivrCallSetIds

    -- * ImportReadGroupSetsResponse
    , ImportReadGroupSetsResponse
    , importReadGroupSetsResponse
    , irgsrReadGroupSetIds

    -- * Metadata
    , Metadata
    , metadata
    , mEvents
    , mPipeline
    , mLabels
    , mCreateTime

    -- * Mount
    , Mount
    , mount
    , mPath
    , mDisk
    , mReadOnly

    -- * PullStartedEvent
    , PullStartedEvent
    , pullStartedEvent
    , pImageURI

    -- * OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omClientId
    , omStartTime
    , omEvents
    , omEndTime
    , omLabels
    , omProjectId
    , omCreateTime
    , omRuntimeMetadata
    , omRequest

    -- * ActionEnvironment
    , ActionEnvironment
    , actionEnvironment
    , aeAddtional

    -- * ComputeEngine
    , ComputeEngine
    , computeEngine
    , ceZone
    , ceDiskNames
    , ceMachineType
    , ceInstanceName

    -- * OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional

    -- * OperationEvent
    , OperationEvent
    , operationEvent
    , oeStartTime
    , oeEndTime
    , oeDescription

    -- * RuntimeMetadata
    , RuntimeMetadata
    , runtimeMetadata
    , rmComputeEngine
    ) where

import Network.Google.Genomics.Types.Product
import Network.Google.Genomics.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v2alpha1' of the Genomics API. This contains the host and root path used as a starting point for constructing service requests.
genomicsService :: ServiceConfig
genomicsService
  = defaultService (ServiceId "genomics:v2alpha1")
      "genomics.googleapis.com"

-- | View and manage Genomics data
genomicsScope :: Proxy '["https://www.googleapis.com/auth/genomics"]
genomicsScope = Proxy;

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
