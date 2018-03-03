{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.UserAccounts
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and manages users and groups for accessing Google Compute Engine
-- virtual machines.
--
-- /See:/ <https://cloud.google.com/compute/docs/access/user-accounts/api/latest/ Cloud User Accounts API Reference>
module Network.Google.UserAccounts
    (
    -- * Service Configuration
      userAccountsService

    -- * OAuth Scopes
    , cloudPlatformReadOnlyScope
    , cloudPlatformScope
    , cloudUserAccountsScope
    , cloudUserAccountsReadOnlyScope

    -- * API Declaration
    , UserAccountsAPI

    -- * Resources

    -- ** clouduseraccounts.globalAccountsOperations.delete
    , module Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.Delete

    -- ** clouduseraccounts.globalAccountsOperations.get
    , module Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.Get

    -- ** clouduseraccounts.globalAccountsOperations.list
    , module Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.List

    -- ** clouduseraccounts.groups.addMember
    , module Network.Google.Resource.CloudUserAccounts.Groups.AddMember

    -- ** clouduseraccounts.groups.delete
    , module Network.Google.Resource.CloudUserAccounts.Groups.Delete

    -- ** clouduseraccounts.groups.get
    , module Network.Google.Resource.CloudUserAccounts.Groups.Get

    -- ** clouduseraccounts.groups.insert
    , module Network.Google.Resource.CloudUserAccounts.Groups.Insert

    -- ** clouduseraccounts.groups.list
    , module Network.Google.Resource.CloudUserAccounts.Groups.List

    -- ** clouduseraccounts.groups.removeMember
    , module Network.Google.Resource.CloudUserAccounts.Groups.RemoveMember

    -- ** clouduseraccounts.linux.getAuthorizedKeysView
    , module Network.Google.Resource.CloudUserAccounts.Linux.GetAuthorizedKeysView

    -- ** clouduseraccounts.linux.getLinuxAccountViews
    , module Network.Google.Resource.CloudUserAccounts.Linux.GetLinuxAccountViews

    -- ** clouduseraccounts.users.addPublicKey
    , module Network.Google.Resource.CloudUserAccounts.Users.AddPublicKey

    -- ** clouduseraccounts.users.delete
    , module Network.Google.Resource.CloudUserAccounts.Users.Delete

    -- ** clouduseraccounts.users.get
    , module Network.Google.Resource.CloudUserAccounts.Users.Get

    -- ** clouduseraccounts.users.insert
    , module Network.Google.Resource.CloudUserAccounts.Users.Insert

    -- ** clouduseraccounts.users.list
    , module Network.Google.Resource.CloudUserAccounts.Users.List

    -- ** clouduseraccounts.users.removePublicKey
    , module Network.Google.Resource.CloudUserAccounts.Users.RemovePublicKey

    -- * Types

    -- ** OperationWarningsItemDataItem
    , OperationWarningsItemDataItem
    , operationWarningsItemDataItem
    , owidiValue
    , owidiKey

    -- ** OperationWarningsItemCode
    , OperationWarningsItemCode (..)

    -- ** OperationList
    , OperationList
    , operationList
    , olNextPageToken
    , olKind
    , olItems
    , olSelfLink
    , olId

    -- ** Group
    , Group
    , group'
    , gKind
    , gMembers
    , gSelfLink
    , gName
    , gCreationTimestamp
    , gId
    , gDescription

    -- ** GroupList
    , GroupList
    , groupList
    , glNextPageToken
    , glKind
    , glItems
    , glSelfLink
    , glId

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
    , oDescription
    , oTargetLink
    , oClientOperationId

    -- ** UserList
    , UserList
    , userList
    , ulNextPageToken
    , ulKind
    , ulItems
    , ulSelfLink
    , ulId

    -- ** PublicKey
    , PublicKey
    , publicKey
    , pkFingerprint
    , pkKey
    , pkCreationTimestamp
    , pkExpirationTimestamp
    , pkDescription

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** LinuxAccountViews
    , LinuxAccountViews
    , linuxAccountViews
    , lavUserViews
    , lavKind
    , lavGroupViews

    -- ** User
    , User
    , user
    , uGroups
    , uPublicKeys
    , uKind
    , uOwner
    , uSelfLink
    , uName
    , uCreationTimestamp
    , uId
    , uDescription

    -- ** GroupsAddMemberRequest
    , GroupsAddMemberRequest
    , groupsAddMemberRequest
    , gamrUsers

    -- ** LinuxGroupView
    , LinuxGroupView
    , linuxGroupView
    , lgvMembers
    , lgvGid
    , lgvGroupName

    -- ** LinuxUserView
    , LinuxUserView
    , linuxUserView
    , luvGecos
    , luvUid
    , luvUsername
    , luvShell
    , luvGid
    , luvHomeDirectory

    -- ** LinuxGetLinuxAccountViewsResponse
    , LinuxGetLinuxAccountViewsResponse
    , linuxGetLinuxAccountViewsResponse
    , lglavrResource

    -- ** OperationError
    , OperationError
    , operationError
    , oeErrors

    -- ** LinuxGetAuthorizedKeysViewResponse
    , LinuxGetAuthorizedKeysViewResponse
    , linuxGetAuthorizedKeysViewResponse
    , lgakvrResource

    -- ** OperationErrorErrorsItem
    , OperationErrorErrorsItem
    , operationErrorErrorsItem
    , oeeiLocation
    , oeeiCode
    , oeeiMessage

    -- ** GroupsRemoveMemberRequest
    , GroupsRemoveMemberRequest
    , groupsRemoveMemberRequest
    , grmrUsers

    -- ** AuthorizedKeysView
    , AuthorizedKeysView
    , authorizedKeysView
    , akvSudoer
    , akvKeys

    -- ** OperationWarningsItem
    , OperationWarningsItem
    , operationWarningsItem
    , owiData
    , owiCode
    , owiMessage
    ) where

import Network.Google.Prelude
import Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.Delete
import Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.Get
import Network.Google.Resource.CloudUserAccounts.GlobalAccountsOperations.List
import Network.Google.Resource.CloudUserAccounts.Groups.AddMember
import Network.Google.Resource.CloudUserAccounts.Groups.Delete
import Network.Google.Resource.CloudUserAccounts.Groups.Get
import Network.Google.Resource.CloudUserAccounts.Groups.Insert
import Network.Google.Resource.CloudUserAccounts.Groups.List
import Network.Google.Resource.CloudUserAccounts.Groups.RemoveMember
import Network.Google.Resource.CloudUserAccounts.Linux.GetAuthorizedKeysView
import Network.Google.Resource.CloudUserAccounts.Linux.GetLinuxAccountViews
import Network.Google.Resource.CloudUserAccounts.Users.AddPublicKey
import Network.Google.Resource.CloudUserAccounts.Users.Delete
import Network.Google.Resource.CloudUserAccounts.Users.Get
import Network.Google.Resource.CloudUserAccounts.Users.Insert
import Network.Google.Resource.CloudUserAccounts.Users.List
import Network.Google.Resource.CloudUserAccounts.Users.RemovePublicKey
import Network.Google.UserAccounts.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Cloud User Accounts API service.
type UserAccountsAPI =
     GroupsInsertResource :<|> GroupsListResource :<|>
       GroupsGetResource
       :<|> GroupsRemoveMemberResource
       :<|> GroupsDeleteResource
       :<|> GroupsAddMemberResource
       :<|> UsersAddPublicKeyResource
       :<|> UsersInsertResource
       :<|> UsersListResource
       :<|> UsersRemovePublicKeyResource
       :<|> UsersGetResource
       :<|> UsersDeleteResource
       :<|> GlobalAccountsOperationsListResource
       :<|> GlobalAccountsOperationsGetResource
       :<|> GlobalAccountsOperationsDeleteResource
       :<|> LinuxGetLinuxAccountViewsResource
       :<|> LinuxGetAuthorizedKeysViewResource
