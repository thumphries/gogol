{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.GroupsSettings.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.GroupsSettings.Types
    (
    -- * Service Configuration
      groupsSettingsService

    -- * OAuth Scopes
    , appsGroupsSettingsScope

    -- * Groups
    , Groups
    , groups
    , gEmail
    , gSendMessageDenyNotification
    , gWhoCanPostMessage
    , gAllowExternalMembers
    , gShowInGroupDirectory
    , gSpamModerationLevel
    , gKind
    , gWhoCanViewMembership
    , gMembersCanPostAsTheGroup
    , gWhoCanViewGroup
    , gWhoCanJoin
    , gArchiveOnly
    , gMaxMessageBytes
    , gReplyTo
    , gMessageModerationLevel
    , gIncludeInGlobalAddressList
    , gIsArchived
    , gName
    , gMessageDisplayFont
    , gIncludeCustomFooter
    , gCustomReplyTo
    , gWhoCanContactOwner
    , gWhoCanAdd
    , gWhoCanInvite
    , gWhoCanLeaveGroup
    , gAllowGoogleCommunication
    , gCustomFooterText
    , gPrimaryLanguage
    , gDescription
    , gDefaultMessageDenyNotificationText
    , gAllowWebPosting
    ) where

import Network.Google.GroupsSettings.Types.Product
import Network.Google.GroupsSettings.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Groups Settings API. This contains the host and root path used as a starting point for constructing service requests.
groupsSettingsService :: ServiceConfig
groupsSettingsService
  = defaultService (ServiceId "groupssettings:v1")
      "www.googleapis.com"

-- | View and manage the settings of a G Suite group
appsGroupsSettingsScope :: Proxy '["https://www.googleapis.com/auth/apps.groups.settings"]
appsGroupsSettingsScope = Proxy;
