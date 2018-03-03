{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Script.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Script.Types
    (
    -- * Service Configuration
      scriptService

    -- * OAuth Scopes
    , mailGoogleComScope
    , m8FeedsScope
    , adminDirectoryUserScope
    , userInfoEmailScope
    , formsCurrentOnlyScope
    , driveScope
    , adminDirectoryGroupScope
    , calendarFeedsScope
    , formsScope
    , spreadsheetsScope
    , groupsScope

    -- * GoogleAppsScriptTypeFunctionSet
    , GoogleAppsScriptTypeFunctionSet
    , googleAppsScriptTypeFunctionSet
    , gastfsValues

    -- * GoogleAppsScriptTypeExecutionAPIConfig
    , GoogleAppsScriptTypeExecutionAPIConfig
    , googleAppsScriptTypeExecutionAPIConfig
    , gasteacAccess

    -- * GoogleAppsScriptTypeProcessProcessType
    , GoogleAppsScriptTypeProcessProcessType (..)

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * GoogleAppsScriptTypeProcessUserAccessLevel
    , GoogleAppsScriptTypeProcessUserAccessLevel (..)

    -- * Metrics
    , Metrics
    , metrics
    , mActiveUsers
    , mFailedExecutions
    , mTotalExecutions

    -- * GoogleAppsScriptTypeProcess
    , GoogleAppsScriptTypeProcess
    , googleAppsScriptTypeProcess
    , gastpProcessStatus
    , gastpStartTime
    , gastpProjectName
    , gastpFunctionName
    , gastpUserAccessLevel
    , gastpProcessType
    , gastpExecutingUser
    , gastpDuration

    -- * GoogleAppsScriptTypeWebAppConfigExecuteAs
    , GoogleAppsScriptTypeWebAppConfigExecuteAs (..)

    -- * Project
    , Project
    , project
    , pCreator
    , pLastModifyUser
    , pUpdateTime
    , pScriptId
    , pTitle
    , pParentId
    , pCreateTime

    -- * Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse

    -- * ListUserProcessesResponse
    , ListUserProcessesResponse
    , listUserProcessesResponse
    , luprNextPageToken
    , luprProcesses

    -- * Empty
    , Empty
    , empty

    -- * GoogleAppsScriptTypeUser
    , GoogleAppsScriptTypeUser
    , googleAppsScriptTypeUser
    , gastuEmail
    , gastuPhotoURL
    , gastuDomain
    , gastuName

    -- * EntryPoint
    , EntryPoint
    , entryPoint
    , epExecutionAPI
    , epAddOn
    , epEntryPointType
    , epWebApp

    -- * ListVersionsResponse
    , ListVersionsResponse
    , listVersionsResponse
    , lvrNextPageToken
    , lvrVersions

    -- * GoogleAppsScriptTypeExecutionAPIConfigAccess
    , GoogleAppsScriptTypeExecutionAPIConfigAccess (..)

    -- * ExecutionRequest
    , ExecutionRequest
    , executionRequest
    , erFunction
    , erSessionState
    , erDevMode
    , erParameters

    -- * ListScriptProcessesResponse
    , ListScriptProcessesResponse
    , listScriptProcessesResponse
    , lsprNextPageToken
    , lsprProcesses

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * GoogleAppsScriptTypeScope
    , GoogleAppsScriptTypeScope
    , googleAppsScriptTypeScope
    , gastsAuthorizer
    , gastsName

    -- * FileType
    , FileType (..)

    -- * ScriptStackTraceElement
    , ScriptStackTraceElement
    , scriptStackTraceElement
    , ssteFunction
    , ssteLineNumber

    -- * Content
    , Content
    , content
    , cScriptId
    , cFiles

    -- * GoogleAppsScriptTypeScopeSet
    , GoogleAppsScriptTypeScopeSet
    , googleAppsScriptTypeScopeSet
    , gastssValues

    -- * GoogleAppsScriptTypeAddOnEntryPointAddOnType
    , GoogleAppsScriptTypeAddOnEntryPointAddOnType (..)

    -- * MetricsValue
    , MetricsValue
    , metricsValue
    , mvStartTime
    , mvValue
    , mvEndTime

    -- * GoogleAppsScriptTypeAddOnEntryPoint
    , GoogleAppsScriptTypeAddOnEntryPoint
    , googleAppsScriptTypeAddOnEntryPoint
    , gastaoepPostInstallTipURL
    , gastaoepAddOnType
    , gastaoepReportIssueURL
    , gastaoepHelpURL
    , gastaoepTitle
    , gastaoepDescription

    -- * GoogleAppsScriptTypeScopeAuthorizer
    , GoogleAppsScriptTypeScopeAuthorizer (..)

    -- * GoogleAppsScriptTypeWebAppConfig
    , GoogleAppsScriptTypeWebAppConfig
    , googleAppsScriptTypeWebAppConfig
    , gastwacAccess
    , gastwacExecuteAs

    -- * Version
    , Version
    , version
    , vVersionNumber
    , vScriptId
    , vDescription
    , vCreateTime

    -- * Xgafv
    , Xgafv (..)

    -- * ExecutionError
    , ExecutionError
    , executionError
    , eeScriptStackTraceElements
    , eeErrorType
    , eeErrorMessage

    -- * GoogleAppsScriptTypeWebAppEntryPoint
    , GoogleAppsScriptTypeWebAppEntryPoint
    , googleAppsScriptTypeWebAppEntryPoint
    , gastwaepEntryPointConfig
    , gastwaepURL

    -- * EntryPointEntryPointType
    , EntryPointEntryPointType (..)

    -- * CreateProjectRequest
    , CreateProjectRequest
    , createProjectRequest
    , cprTitle
    , cprParentId

    -- * GoogleAppsScriptTypeProcessProcessStatus
    , GoogleAppsScriptTypeProcessProcessStatus (..)

    -- * DeploymentConfig
    , DeploymentConfig
    , deploymentConfig
    , dcVersionNumber
    , dcScriptId
    , dcManifestFileName
    , dcDescription

    -- * GoogleAppsScriptTypeWebAppConfigAccess
    , GoogleAppsScriptTypeWebAppConfigAccess (..)

    -- * ListDeploymentsResponse
    , ListDeploymentsResponse
    , listDeploymentsResponse
    , ldrNextPageToken
    , ldrDeployments

    -- * GoogleAppsScriptTypeFunction
    , GoogleAppsScriptTypeFunction
    , googleAppsScriptTypeFunction
    , gastfName

    -- * File
    , File
    , file
    , fFunctionSet
    , fLastModifyUser
    , fUpdateTime
    , fName
    , fSource
    , fType
    , fCreateTime

    -- * GoogleAppsScriptTypeExecutionAPIEntryPoint
    , GoogleAppsScriptTypeExecutionAPIEntryPoint
    , googleAppsScriptTypeExecutionAPIEntryPoint
    , gasteaepEntryPointConfig

    -- * OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional

    -- * ExecutionResponse
    , ExecutionResponse
    , executionResponse
    , erResult

    -- * Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dFunctionSet
    , dUpdateTime
    , dScopeSet
    , dEntryPoints
    , dDeploymentConfig

    -- * UpdateDeploymentRequest
    , UpdateDeploymentRequest
    , updateDeploymentRequest
    , udrDeploymentConfig
    ) where

import Network.Google.Prelude
import Network.Google.Script.Types.Product
import Network.Google.Script.Types.Sum

-- | Default request referring to version 'v1' of the Google Apps Script API. This contains the host and root path used as a starting point for constructing service requests.
scriptService :: ServiceConfig
scriptService
  = defaultService (ServiceId "script:v1")
      "script.googleapis.com"

-- | Read, send, delete, and manage your email
mailGoogleComScope :: Proxy '["https://mail.google.com/"]
mailGoogleComScope = Proxy;

-- | Manage your contacts
m8FeedsScope :: Proxy '["https://www.google.com/m8/feeds"]
m8FeedsScope = Proxy;

-- | View and manage the provisioning of users on your domain
adminDirectoryUserScope :: Proxy '["https://www.googleapis.com/auth/admin.directory.user"]
adminDirectoryUserScope = Proxy;

-- | View your email address
userInfoEmailScope :: Proxy '["https://www.googleapis.com/auth/userinfo.email"]
userInfoEmailScope = Proxy;

-- | View and manage forms that this application has been installed in
formsCurrentOnlyScope :: Proxy '["https://www.googleapis.com/auth/forms.currentonly"]
formsCurrentOnlyScope = Proxy;

-- | View and manage the files in your Google Drive
driveScope :: Proxy '["https://www.googleapis.com/auth/drive"]
driveScope = Proxy;

-- | View and manage the provisioning of groups on your domain
adminDirectoryGroupScope :: Proxy '["https://www.googleapis.com/auth/admin.directory.group"]
adminDirectoryGroupScope = Proxy;

-- | Manage your calendars
calendarFeedsScope :: Proxy '["https://www.google.com/calendar/feeds"]
calendarFeedsScope = Proxy;

-- | View and manage your forms in Google Drive
formsScope :: Proxy '["https://www.googleapis.com/auth/forms"]
formsScope = Proxy;

-- | View and manage your spreadsheets in Google Drive
spreadsheetsScope :: Proxy '["https://www.googleapis.com/auth/spreadsheets"]
spreadsheetsScope = Proxy;

-- | View and manage your Google Groups
groupsScope :: Proxy '["https://www.googleapis.com/auth/groups"]
groupsScope = Proxy;
