{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.YouTubeAnalytics.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.YouTubeAnalytics.Types
    (
    -- * Service Configuration
      youTubeAnalyticsService

    -- * OAuth Scopes
    , youTubeScope
    , youTubeAnalyticsReadOnlyScope
    , youTubePartnerScope
    , youTubeAnalyticsMonetaryReadOnlyScope
    , youTubeReadOnlyScope

    -- * GroupContentDetails
    , GroupContentDetails
    , groupContentDetails
    , gcdItemType
    , gcdItemCount

    -- * Group
    , Group
    , group'
    , gEtag
    , gSnippet
    , gKind
    , gContentDetails
    , gId

    -- * GroupItemResource
    , GroupItemResource
    , groupItemResource
    , girKind
    , girId

    -- * ResultTableColumnHeadersItem
    , ResultTableColumnHeadersItem
    , resultTableColumnHeadersItem
    , rtchiColumnType
    , rtchiName
    , rtchiDataType

    -- * ResultTable
    , ResultTable
    , resultTable
    , rtKind
    , rtRows
    , rtColumnHeaders

    -- * GroupSnippet
    , GroupSnippet
    , groupSnippet
    , gsPublishedAt
    , gsTitle

    -- * GroupItem
    , GroupItem
    , groupItem
    , giEtag
    , giKind
    , giResource
    , giGroupId
    , giId

    -- * GroupItemListResponse
    , GroupItemListResponse
    , groupItemListResponse
    , gilrEtag
    , gilrKind
    , gilrItems

    -- * GroupListResponse
    , GroupListResponse
    , groupListResponse
    , glrEtag
    , glrNextPageToken
    , glrKind
    , glrItems
    ) where

import Network.Google.Prelude
import Network.Google.YouTubeAnalytics.Types.Product
import Network.Google.YouTubeAnalytics.Types.Sum

-- | Default request referring to version 'v1' of the YouTube Analytics API. This contains the host and root path used as a starting point for constructing service requests.
youTubeAnalyticsService :: ServiceConfig
youTubeAnalyticsService
  = defaultService (ServiceId "youtubeAnalytics:v1")
      "www.googleapis.com"

-- | Manage your YouTube account
youTubeScope :: Proxy '["https://www.googleapis.com/auth/youtube"]
youTubeScope = Proxy;

-- | View YouTube Analytics reports for your YouTube content
youTubeAnalyticsReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/yt-analytics.readonly"]
youTubeAnalyticsReadOnlyScope = Proxy;

-- | View and manage your assets and associated content on YouTube
youTubePartnerScope :: Proxy '["https://www.googleapis.com/auth/youtubepartner"]
youTubePartnerScope = Proxy;

-- | View monetary and non-monetary YouTube Analytics reports for your
-- YouTube content
youTubeAnalyticsMonetaryReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/yt-analytics-monetary.readonly"]
youTubeAnalyticsMonetaryReadOnlyScope = Proxy;

-- | View your YouTube account
youTubeReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/youtube.readonly"]
youTubeReadOnlyScope = Proxy;
