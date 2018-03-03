{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.StorageTransfer
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers data from external data sources to a Google Cloud Storage
-- bucket or between Google Cloud Storage buckets.
--
-- /See:/ <https://cloud.google.com/storage/transfer Storage Transfer API Reference>
module Network.Google.StorageTransfer
    (
    -- * Service Configuration
      storageTransferService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , StorageTransferAPI

    -- * Resources

    -- ** storagetransfer.googleServiceAccounts.get
    , module Network.Google.Resource.StorageTransfer.GoogleServiceAccounts.Get

    -- ** storagetransfer.transferJobs.create
    , module Network.Google.Resource.StorageTransfer.TransferJobs.Create

    -- ** storagetransfer.transferJobs.get
    , module Network.Google.Resource.StorageTransfer.TransferJobs.Get

    -- ** storagetransfer.transferJobs.list
    , module Network.Google.Resource.StorageTransfer.TransferJobs.List

    -- ** storagetransfer.transferJobs.patch
    , module Network.Google.Resource.StorageTransfer.TransferJobs.Patch

    -- ** storagetransfer.transferOperations.cancel
    , module Network.Google.Resource.StorageTransfer.TransferOperations.Cancel

    -- ** storagetransfer.transferOperations.delete
    , module Network.Google.Resource.StorageTransfer.TransferOperations.Delete

    -- ** storagetransfer.transferOperations.get
    , module Network.Google.Resource.StorageTransfer.TransferOperations.Get

    -- ** storagetransfer.transferOperations.list
    , module Network.Google.Resource.StorageTransfer.TransferOperations.List

    -- ** storagetransfer.transferOperations.pause
    , module Network.Google.Resource.StorageTransfer.TransferOperations.Pause

    -- ** storagetransfer.transferOperations.resume
    , module Network.Google.Resource.StorageTransfer.TransferOperations.Resume

    -- * Types

    -- ** ErrorSummary
    , ErrorSummary
    , errorSummary
    , esErrorCount
    , esErrorCode
    , esErrorLogEntries

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorNextPageToken
    , lorOperations

    -- ** TransferOperationStatus
    , TransferOperationStatus (..)

    -- ** Schedule
    , Schedule
    , schedule
    , sScheduleEndDate
    , sScheduleStartDate
    , sStartTimeOfDay

    -- ** ObjectConditions
    , ObjectConditions
    , objectConditions
    , ocMinTimeElapsedSinceLastModification
    , ocIncludePrefixes
    , ocMaxTimeElapsedSinceLastModification
    , ocExcludePrefixes

    -- ** Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- ** Empty
    , Empty
    , empty

    -- ** PauseTransferOperationRequest
    , PauseTransferOperationRequest
    , pauseTransferOperationRequest

    -- ** GoogleServiceAccount
    , GoogleServiceAccount
    , googleServiceAccount
    , gsaAccountEmail

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** Date
    , Date
    , date
    , dDay
    , dYear
    , dMonth

    -- ** UpdateTransferJobRequest
    , UpdateTransferJobRequest
    , updateTransferJobRequest
    , utjrTransferJob
    , utjrProjectId
    , utjrUpdateTransferJobFieldMask

    -- ** TransferCounters
    , TransferCounters
    , transferCounters
    , tcBytesFoundOnlyFromSink
    , tcBytesDeletedFromSink
    , tcObjectsDeletedFromSource
    , tcObjectsFoundFromSource
    , tcBytesFailedToDeleteFromSink
    , tcBytesFromSourceFailed
    , tcBytesCopiedToSink
    , tcBytesFoundFromSource
    , tcBytesDeletedFromSource
    , tcObjectsDeletedFromSink
    , tcObjectsFoundOnlyFromSink
    , tcBytesFromSourceSkippedBySync
    , tcObjectsCopiedToSink
    , tcObjectsFromSourceFailed
    , tcObjectsFailedToDeleteFromSink
    , tcObjectsFromSourceSkippedBySync

    -- ** TransferJob
    , TransferJob
    , transferJob
    , tjCreationTime
    , tjStatus
    , tjSchedule
    , tjDeletionTime
    , tjName
    , tjProjectId
    , tjTransferSpec
    , tjDescription
    , tjLastModificationTime

    -- ** GcsData
    , GcsData
    , gcsData
    , gdBucketName

    -- ** Xgafv
    , Xgafv (..)

    -- ** AwsS3Data
    , AwsS3Data
    , awsS3Data
    , asdBucketName
    , asdAwsAccessKey

    -- ** TransferJobStatus
    , TransferJobStatus (..)

    -- ** HTTPData
    , HTTPData
    , hTTPData
    , httpdListURL

    -- ** TimeOfDay'
    , TimeOfDay'
    , timeOfDay
    , todNanos
    , todHours
    , todMinutes
    , todSeconds

    -- ** ErrorLogEntry
    , ErrorLogEntry
    , errorLogEntry
    , eleURL
    , eleErrorDetails

    -- ** OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- ** TransferOptions
    , TransferOptions
    , transferOptions
    , toDeleteObjectsUniqueInSink
    , toDeleteObjectsFromSourceAfterTransfer
    , toOverwriteObjectsAlreadyExistingInSink

    -- ** TransferOperation
    , TransferOperation
    , transferOperation
    , toStatus
    , toCounters
    , toStartTime
    , toTransferJobName
    , toName
    , toEndTime
    , toProjectId
    , toTransferSpec
    , toErrorBreakdowns

    -- ** TransferSpec
    , TransferSpec
    , transferSpec
    , tsGcsDataSource
    , tsObjectConditions
    , tsHTTPDataSource
    , tsAwsS3DataSource
    , tsGcsDataSink
    , tsTransferOptions

    -- ** ListTransferJobsResponse
    , ListTransferJobsResponse
    , listTransferJobsResponse
    , ltjrNextPageToken
    , ltjrTransferJobs

    -- ** OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional

    -- ** ResumeTransferOperationRequest
    , ResumeTransferOperationRequest
    , resumeTransferOperationRequest

    -- ** AwsAccessKey
    , AwsAccessKey
    , awsAccessKey
    , aakSecretAccessKey
    , aakAccessKeyId

    -- ** ErrorSummaryErrorCode
    , ErrorSummaryErrorCode (..)
    ) where

import Network.Google.Prelude
import Network.Google.Resource.StorageTransfer.GoogleServiceAccounts.Get
import Network.Google.Resource.StorageTransfer.TransferJobs.Create
import Network.Google.Resource.StorageTransfer.TransferJobs.Get
import Network.Google.Resource.StorageTransfer.TransferJobs.List
import Network.Google.Resource.StorageTransfer.TransferJobs.Patch
import Network.Google.Resource.StorageTransfer.TransferOperations.Cancel
import Network.Google.Resource.StorageTransfer.TransferOperations.Delete
import Network.Google.Resource.StorageTransfer.TransferOperations.Get
import Network.Google.Resource.StorageTransfer.TransferOperations.List
import Network.Google.Resource.StorageTransfer.TransferOperations.Pause
import Network.Google.Resource.StorageTransfer.TransferOperations.Resume
import Network.Google.StorageTransfer.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Storage Transfer API service.
type StorageTransferAPI =
     TransferJobsListResource :<|>
       TransferJobsPatchResource
       :<|> TransferJobsGetResource
       :<|> TransferJobsCreateResource
       :<|> TransferOperationsListResource
       :<|> TransferOperationsGetResource
       :<|> TransferOperationsPauseResource
       :<|> TransferOperationsCancelResource
       :<|> TransferOperationsDeleteResource
       :<|> TransferOperationsResumeResource
       :<|> GoogleServiceAccountsGetResource
