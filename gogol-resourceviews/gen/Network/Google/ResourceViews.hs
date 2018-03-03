{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.ResourceViews
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Resource View API allows users to create and manage logical sets of
-- Google Compute Engine instances.
--
-- /See:/ <https://developers.google.com/compute/ Google Compute Engine Instance Groups API Reference>
module Network.Google.ResourceViews
    (
    -- * Service Configuration
      resourceViewsService

    -- * OAuth Scopes
    , computeScope
    , cloudPlatformReadOnlyScope
    , cloudPlatformScope
    , ndevCloudmanScope
    , ndevCloudmanReadOnlyScope
    , computeReadOnlyScope

    -- * API Declaration
    , ResourceViewsAPI

    -- * Resources

    -- ** resourceviews.zoneOperations.get
    , module Network.Google.Resource.ResourceViews.ZoneOperations.Get

    -- ** resourceviews.zoneOperations.list
    , module Network.Google.Resource.ResourceViews.ZoneOperations.List

    -- ** resourceviews.zoneViews.addResources
    , module Network.Google.Resource.ResourceViews.ZoneViews.AddResources

    -- ** resourceviews.zoneViews.delete
    , module Network.Google.Resource.ResourceViews.ZoneViews.Delete

    -- ** resourceviews.zoneViews.get
    , module Network.Google.Resource.ResourceViews.ZoneViews.Get

    -- ** resourceviews.zoneViews.getService
    , module Network.Google.Resource.ResourceViews.ZoneViews.GetService

    -- ** resourceviews.zoneViews.insert
    , module Network.Google.Resource.ResourceViews.ZoneViews.Insert

    -- ** resourceviews.zoneViews.list
    , module Network.Google.Resource.ResourceViews.ZoneViews.List

    -- ** resourceviews.zoneViews.listResources
    , module Network.Google.Resource.ResourceViews.ZoneViews.ListResources

    -- ** resourceviews.zoneViews.removeResources
    , module Network.Google.Resource.ResourceViews.ZoneViews.RemoveResources

    -- ** resourceviews.zoneViews.setService
    , module Network.Google.Resource.ResourceViews.ZoneViews.SetService

    -- * Types

    -- ** OperationWarningsItemDataItem
    , OperationWarningsItemDataItem
    , operationWarningsItemDataItem
    , owidiValue
    , owidiKey

    -- ** OperationList
    , OperationList
    , operationList
    , olNextPageToken
    , olKind
    , olItems
    , olSelfLink
    , olId

    -- ** ResourceView
    , ResourceView
    , resourceView
    , rvSize
    , rvKind
    , rvFingerprint
    , rvNetwork
    , rvResources
    , rvSelfLink
    , rvName
    , rvCreationTimestamp
    , rvId
    , rvLabels
    , rvEndpoints
    , rvDescription

    -- ** ZoneViewsList
    , ZoneViewsList
    , zoneViewsList
    , zvlNextPageToken
    , zvlKind
    , zvlItems
    , zvlSelfLink

    -- ** Operation
    , Operation
    , operation
    , oTargetId
    , oStatus
    , oInsertTime
    , oProgress
    , oStartTime
    , oKind
    , oError
    , oHTTPErrorMessage
    , oZone
    , oWarnings
    , oHTTPErrorStatusCode
    , oUser
    , oSelfLink
    , oName
    , oStatusMessage
    , oCreationTimestamp
    , oEndTime
    , oId
    , oOperationType
    , oRegion
    , oTargetLink
    , oClientOperationId

    -- ** ZoneViewsListResourcesFormat
    , ZoneViewsListResourcesFormat (..)

    -- ** ZoneViewsListResourcesResponse
    , ZoneViewsListResourcesResponse
    , zoneViewsListResourcesResponse
    , zvlrrNextPageToken
    , zvlrrItems
    , zvlrrNetwork

    -- ** ServiceEndpoint
    , ServiceEndpoint
    , serviceEndpoint
    , seName
    , sePort

    -- ** ZoneViewsListResourcesListState
    , ZoneViewsListResourcesListState (..)

    -- ** ZoneViewsRemoveResourcesRequest
    , ZoneViewsRemoveResourcesRequest
    , zoneViewsRemoveResourcesRequest
    , zvrrrResources

    -- ** ListResourceResponseItemEndpoints
    , ListResourceResponseItemEndpoints
    , listResourceResponseItemEndpoints
    , lrrieAddtional

    -- ** ZoneViewsAddResourcesRequest
    , ZoneViewsAddResourcesRequest
    , zoneViewsAddResourcesRequest
    , zvarrResources

    -- ** OperationError
    , OperationError
    , operationError
    , oeErrors

    -- ** ZoneViewsGetServiceResponse
    , ZoneViewsGetServiceResponse
    , zoneViewsGetServiceResponse
    , zvgsrFingerprint
    , zvgsrEndpoints

    -- ** OperationErrorErrorsItem
    , OperationErrorErrorsItem
    , operationErrorErrorsItem
    , oeeiLocation
    , oeeiCode
    , oeeiMessage

    -- ** ZoneViewsSetServiceRequest
    , ZoneViewsSetServiceRequest
    , zoneViewsSetServiceRequest
    , zvssrResourceName
    , zvssrFingerprint
    , zvssrEndpoints

    -- ** ListResourceResponseItem
    , ListResourceResponseItem
    , listResourceResponseItem
    , lrriResource
    , lrriEndpoints

    -- ** Label
    , Label
    , label
    , lValue
    , lKey

    -- ** OperationWarningsItem
    , OperationWarningsItem
    , operationWarningsItem
    , owiData
    , owiCode
    , owiMessage
    ) where

import Network.Google.Prelude
import Network.Google.Resource.ResourceViews.ZoneOperations.Get
import Network.Google.Resource.ResourceViews.ZoneOperations.List
import Network.Google.Resource.ResourceViews.ZoneViews.AddResources
import Network.Google.Resource.ResourceViews.ZoneViews.Delete
import Network.Google.Resource.ResourceViews.ZoneViews.Get
import Network.Google.Resource.ResourceViews.ZoneViews.GetService
import Network.Google.Resource.ResourceViews.ZoneViews.Insert
import Network.Google.Resource.ResourceViews.ZoneViews.List
import Network.Google.Resource.ResourceViews.ZoneViews.ListResources
import Network.Google.Resource.ResourceViews.ZoneViews.RemoveResources
import Network.Google.Resource.ResourceViews.ZoneViews.SetService
import Network.Google.ResourceViews.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Compute Engine Instance Groups API service.
type ResourceViewsAPI =
     ZoneViewsSetServiceResource :<|>
       ZoneViewsInsertResource
       :<|> ZoneViewsListResource
       :<|> ZoneViewsListResourcesResource
       :<|> ZoneViewsGetResource
       :<|> ZoneViewsRemoveResourcesResource
       :<|> ZoneViewsAddResourcesResource
       :<|> ZoneViewsGetServiceResource
       :<|> ZoneViewsDeleteResource
       :<|> ZoneOperationsListResource
       :<|> ZoneOperationsGetResource
