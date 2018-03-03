{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Dataproc
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manages Hadoop-based clusters and jobs on Google Cloud Platform.
--
-- /See:/ <https://cloud.google.com/dataproc/ Google Cloud Dataproc API Reference>
module Network.Google.Dataproc
    (
    -- * Service Configuration
      dataprocService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , DataprocAPI

    -- * Resources

    -- ** dataproc.projects.regions.clusters.create
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Create

    -- ** dataproc.projects.regions.clusters.delete
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Delete

    -- ** dataproc.projects.regions.clusters.diagnose
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Diagnose

    -- ** dataproc.projects.regions.clusters.get
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Get

    -- ** dataproc.projects.regions.clusters.list
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.List

    -- ** dataproc.projects.regions.clusters.patch
    , module Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Patch

    -- ** dataproc.projects.regions.jobs.cancel
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Cancel

    -- ** dataproc.projects.regions.jobs.delete
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Delete

    -- ** dataproc.projects.regions.jobs.get
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Get

    -- ** dataproc.projects.regions.jobs.list
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.List

    -- ** dataproc.projects.regions.jobs.patch
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Patch

    -- ** dataproc.projects.regions.jobs.submit
    , module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Submit

    -- ** dataproc.projects.regions.operations.cancel
    , module Network.Google.Resource.Dataproc.Projects.Regions.Operations.Cancel

    -- ** dataproc.projects.regions.operations.delete
    , module Network.Google.Resource.Dataproc.Projects.Regions.Operations.Delete

    -- ** dataproc.projects.regions.operations.get
    , module Network.Google.Resource.Dataproc.Projects.Regions.Operations.Get

    -- ** dataproc.projects.regions.operations.list
    , module Network.Google.Resource.Dataproc.Projects.Regions.Operations.List

    -- * Types

    -- ** JobReference
    , JobReference
    , jobReference
    , jrJobId
    , jrProjectId

    -- ** JobStatusState
    , JobStatusState (..)

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** PySparkJobProperties
    , PySparkJobProperties
    , pySparkJobProperties
    , psjpAddtional

    -- ** DiagnoseClusterResults
    , DiagnoseClusterResults
    , diagnoseClusterResults
    , dcrOutputURI

    -- ** InstanceGroupConfig
    , InstanceGroupConfig
    , instanceGroupConfig
    , igcNumInstances
    , igcDiskConfig
    , igcIsPreemptible
    , igcImageURI
    , igcAccelerators
    , igcInstanceNames
    , igcManagedGroupConfig
    , igcMachineTypeURI

    -- ** SparkJob
    , SparkJob
    , sparkJob
    , sjArgs
    , sjMainJarFileURI
    , sjJarFileURIs
    , sjFileURIs
    , sjArchiveURIs
    , sjMainClass
    , sjLoggingConfig
    , sjProperties

    -- ** SoftwareConfigProperties
    , SoftwareConfigProperties
    , softwareConfigProperties
    , scpAddtional

    -- ** JobScheduling
    , JobScheduling
    , jobScheduling
    , jsMaxFailuresPerHour

    -- ** DiskConfig
    , DiskConfig
    , diskConfig
    , dcNumLocalSsds
    , dcBootDiskSizeGb

    -- ** ClusterOperationMetadataLabels
    , ClusterOperationMetadataLabels
    , clusterOperationMetadataLabels
    , comlAddtional

    -- ** ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorNextPageToken
    , lorOperations

    -- ** HiveJobScriptVariables
    , HiveJobScriptVariables
    , hiveJobScriptVariables
    , hjsvAddtional

    -- ** Cluster
    , Cluster
    , cluster
    , cStatus
    , cMetrics
    , cClusterUuid
    , cConfig
    , cClusterName
    , cLabels
    , cProjectId
    , cStatusHistory

    -- ** JobLabels
    , JobLabels
    , jobLabels
    , jlAddtional

    -- ** SubmitJobRequest
    , SubmitJobRequest
    , submitJobRequest
    , sjrRequestId
    , sjrJob

    -- ** ClusterStatusSubState
    , ClusterStatusSubState (..)

    -- ** ClusterMetrics
    , ClusterMetrics
    , clusterMetrics
    , cmYarnMetrics
    , cmHdfsMetrics

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

    -- ** HiveJob
    , HiveJob
    , hiveJob
    , hjQueryFileURI
    , hjJarFileURIs
    , hjScriptVariables
    , hjQueryList
    , hjContinueOnFailure
    , hjProperties

    -- ** SparkSQLJobScriptVariables
    , SparkSQLJobScriptVariables
    , sparkSQLJobScriptVariables
    , ssqljsvAddtional

    -- ** PigJobProperties
    , PigJobProperties
    , pigJobProperties
    , pjpAddtional

    -- ** ClusterConfig
    , ClusterConfig
    , clusterConfig
    , ccWorkerConfig
    , ccInitializationActions
    , ccMasterConfig
    , ccGceClusterConfig
    , ccConfigBucket
    , ccSoftwareConfig
    , ccSecondaryWorkerConfig

    -- ** HadoopJobProperties
    , HadoopJobProperties
    , hadoopJobProperties
    , hjpAddtional

    -- ** ClusterOperationStatus
    , ClusterOperationStatus
    , clusterOperationStatus
    , cosState
    , cosInnerState
    , cosStateStartTime
    , cosDetails

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** SparkSQLJobProperties
    , SparkSQLJobProperties
    , sparkSQLJobProperties
    , ssqljpAddtional

    -- ** NodeInitializationAction
    , NodeInitializationAction
    , nodeInitializationAction
    , niaExecutionTimeout
    , niaExecutableFile

    -- ** ClusterMetricsYarnMetrics
    , ClusterMetricsYarnMetrics
    , clusterMetricsYarnMetrics
    , cmymAddtional

    -- ** JobPlacement
    , JobPlacement
    , jobPlacement
    , jpClusterUuid
    , jpClusterName

    -- ** GceClusterConfig
    , GceClusterConfig
    , gceClusterConfig
    , gccSubnetworkURI
    , gccInternalIPOnly
    , gccNetworkURI
    , gccZoneURI
    , gccServiceAccount
    , gccMetadata
    , gccServiceAccountScopes
    , gccTags

    -- ** YarnApplicationState
    , YarnApplicationState (..)

    -- ** ClusterStatusState
    , ClusterStatusState (..)

    -- ** GceClusterConfigMetadata
    , GceClusterConfigMetadata
    , gceClusterConfigMetadata
    , gccmAddtional

    -- ** ClusterOperationStatusState
    , ClusterOperationStatusState (..)

    -- ** HiveJobProperties
    , HiveJobProperties
    , hiveJobProperties
    , hAddtional

    -- ** ClusterLabels
    , ClusterLabels
    , clusterLabels
    , clAddtional

    -- ** Job
    , Job
    , job
    , jSparkJob
    , jStatus
    , jDriverControlFilesURI
    , jHiveJob
    , jReference
    , jSparkSQLJob
    , jHadoopJob
    , jYarnApplications
    , jLabels
    , jPysparkJob
    , jDriverOutputResourceURI
    , jScheduling
    , jStatusHistory
    , jPlacement
    , jPigJob

    -- ** DiagnoseClusterRequest
    , DiagnoseClusterRequest
    , diagnoseClusterRequest

    -- ** HadoopJob
    , HadoopJob
    , hadoopJob
    , hArgs
    , hMainJarFileURI
    , hJarFileURIs
    , hFileURIs
    , hArchiveURIs
    , hMainClass
    , hLoggingConfig
    , hProperties

    -- ** Xgafv
    , Xgafv (..)

    -- ** ClusterOperationMetadata
    , ClusterOperationMetadata
    , clusterOperationMetadata
    , comStatus
    , comClusterUuid
    , comWarnings
    , comClusterName
    , comLabels
    , comOperationType
    , comStatusHistory
    , comDescription

    -- ** SparkSQLJob
    , SparkSQLJob
    , sparkSQLJob
    , ssqljQueryFileURI
    , ssqljJarFileURIs
    , ssqljScriptVariables
    , ssqljQueryList
    , ssqljLoggingConfig
    , ssqljProperties

    -- ** SoftwareConfig
    , SoftwareConfig
    , softwareConfig
    , scImageVersion
    , scProperties

    -- ** ListJobsResponse
    , ListJobsResponse
    , listJobsResponse
    , ljrNextPageToken
    , ljrJobs

    -- ** AcceleratorConfig
    , AcceleratorConfig
    , acceleratorConfig
    , acAcceleratorCount
    , acAcceleratorTypeURI

    -- ** SparkJobProperties
    , SparkJobProperties
    , sparkJobProperties
    , sjpAddtional

    -- ** PySparkJob
    , PySparkJob
    , pySparkJob
    , psjPythonFileURIs
    , psjMainPythonFileURI
    , psjArgs
    , psjJarFileURIs
    , psjFileURIs
    , psjArchiveURIs
    , psjLoggingConfig
    , psjProperties

    -- ** ManagedGroupConfig
    , ManagedGroupConfig
    , managedGroupConfig
    , mgcInstanceTemplateName
    , mgcInstanceGroupManagerName

    -- ** ListClustersResponse
    , ListClustersResponse
    , listClustersResponse
    , lcrNextPageToken
    , lcrClusters

    -- ** JobStatusSubState
    , JobStatusSubState (..)

    -- ** CancelJobRequest
    , CancelJobRequest
    , cancelJobRequest

    -- ** QueryList
    , QueryList
    , queryList
    , qlQueries

    -- ** OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- ** JobStatus
    , JobStatus
    , jobStatus
    , jsState
    , jsSubState
    , jsStateStartTime
    , jsDetails

    -- ** PigJobScriptVariables
    , PigJobScriptVariables
    , pigJobScriptVariables
    , pjsvAddtional

    -- ** ClusterMetricsHdfsMetrics
    , ClusterMetricsHdfsMetrics
    , clusterMetricsHdfsMetrics
    , cmhmAddtional

    -- ** OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional

    -- ** LoggingConfigDriverLogLevels
    , LoggingConfigDriverLogLevels
    , loggingConfigDriverLogLevels
    , lcdllAddtional

    -- ** ClusterStatus
    , ClusterStatus
    , clusterStatus
    , csState
    , csSubState
    , csStateStartTime
    , csDetail

    -- ** YarnApplication
    , YarnApplication
    , yarnApplication
    , yaTrackingURL
    , yaState
    , yaProgress
    , yaName

    -- ** PigJob
    , PigJob
    , pigJob
    , pjQueryFileURI
    , pjJarFileURIs
    , pjScriptVariables
    , pjQueryList
    , pjContinueOnFailure
    , pjLoggingConfig
    , pjProperties

    -- ** LoggingConfig
    , LoggingConfig
    , loggingConfig
    , lcDriverLogLevels
    ) where

import Network.Google.Prelude
import Network.Google.Dataproc.Types
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Create
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Delete
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Diagnose
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Get
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.List
import Network.Google.Resource.Dataproc.Projects.Regions.Clusters.Patch
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Cancel
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Delete
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Get
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.List
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Patch
import Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Submit
import Network.Google.Resource.Dataproc.Projects.Regions.Operations.Cancel
import Network.Google.Resource.Dataproc.Projects.Regions.Operations.Delete
import Network.Google.Resource.Dataproc.Projects.Regions.Operations.Get
import Network.Google.Resource.Dataproc.Projects.Regions.Operations.List

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Dataproc API service.
type DataprocAPI =
     ProjectsRegionsJobsListResource :<|>
       ProjectsRegionsJobsPatchResource
       :<|> ProjectsRegionsJobsGetResource
       :<|> ProjectsRegionsJobsSubmitResource
       :<|> ProjectsRegionsJobsCancelResource
       :<|> ProjectsRegionsJobsDeleteResource
       :<|> ProjectsRegionsOperationsListResource
       :<|> ProjectsRegionsOperationsGetResource
       :<|> ProjectsRegionsOperationsCancelResource
       :<|> ProjectsRegionsOperationsDeleteResource
       :<|> ProjectsRegionsClustersDiagnoseResource
       :<|> ProjectsRegionsClustersListResource
       :<|> ProjectsRegionsClustersPatchResource
       :<|> ProjectsRegionsClustersGetResource
       :<|> ProjectsRegionsClustersCreateResource
       :<|> ProjectsRegionsClustersDeleteResource
