{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Drive
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manages files in Drive including uploading, downloading, searching,
-- detecting changes, and updating sharing permissions.
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference>
module Network.Google.Drive
    (
    -- * Service Configuration
      driveService

    -- * OAuth Scopes
    , driveMetadataReadOnlyScope
    , drivePhotosReadOnlyScope
    , driveAppDataScope
    , driveReadOnlyScope
    , driveScope
    , driveFileScope
    , driveMetadataScope
    , driveScriptsScope

    -- * API Declaration
    , DriveAPI

    -- * Resources

    -- ** drive.about.get
    , module Network.Google.Resource.Drive.About.Get

    -- ** drive.changes.getStartPageToken
    , module Network.Google.Resource.Drive.Changes.GetStartPageToken

    -- ** drive.changes.list
    , module Network.Google.Resource.Drive.Changes.List

    -- ** drive.changes.watch
    , module Network.Google.Resource.Drive.Changes.Watch

    -- ** drive.channels.stop
    , module Network.Google.Resource.Drive.Channels.Stop

    -- ** drive.comments.create
    , module Network.Google.Resource.Drive.Comments.Create

    -- ** drive.comments.delete
    , module Network.Google.Resource.Drive.Comments.Delete

    -- ** drive.comments.get
    , module Network.Google.Resource.Drive.Comments.Get

    -- ** drive.comments.list
    , module Network.Google.Resource.Drive.Comments.List

    -- ** drive.comments.update
    , module Network.Google.Resource.Drive.Comments.Update

    -- ** drive.files.copy
    , module Network.Google.Resource.Drive.Files.Copy

    -- ** drive.files.create
    , module Network.Google.Resource.Drive.Files.Create

    -- ** drive.files.delete
    , module Network.Google.Resource.Drive.Files.Delete

    -- ** drive.files.emptyTrash
    , module Network.Google.Resource.Drive.Files.EmptyTrash

    -- ** drive.files.export
    , module Network.Google.Resource.Drive.Files.Export

    -- ** drive.files.generateIds
    , module Network.Google.Resource.Drive.Files.GenerateIds

    -- ** drive.files.get
    , module Network.Google.Resource.Drive.Files.Get

    -- ** drive.files.list
    , module Network.Google.Resource.Drive.Files.List

    -- ** drive.files.update
    , module Network.Google.Resource.Drive.Files.Update

    -- ** drive.files.watch
    , module Network.Google.Resource.Drive.Files.Watch

    -- ** drive.permissions.create
    , module Network.Google.Resource.Drive.Permissions.Create

    -- ** drive.permissions.delete
    , module Network.Google.Resource.Drive.Permissions.Delete

    -- ** drive.permissions.get
    , module Network.Google.Resource.Drive.Permissions.Get

    -- ** drive.permissions.list
    , module Network.Google.Resource.Drive.Permissions.List

    -- ** drive.permissions.update
    , module Network.Google.Resource.Drive.Permissions.Update

    -- ** drive.replies.create
    , module Network.Google.Resource.Drive.Replies.Create

    -- ** drive.replies.delete
    , module Network.Google.Resource.Drive.Replies.Delete

    -- ** drive.replies.get
    , module Network.Google.Resource.Drive.Replies.Get

    -- ** drive.replies.list
    , module Network.Google.Resource.Drive.Replies.List

    -- ** drive.replies.update
    , module Network.Google.Resource.Drive.Replies.Update

    -- ** drive.revisions.delete
    , module Network.Google.Resource.Drive.Revisions.Delete

    -- ** drive.revisions.get
    , module Network.Google.Resource.Drive.Revisions.Get

    -- ** drive.revisions.list
    , module Network.Google.Resource.Drive.Revisions.List

    -- ** drive.revisions.update
    , module Network.Google.Resource.Drive.Revisions.Update

    -- ** drive.teamdrives.create
    , module Network.Google.Resource.Drive.Teamdrives.Create

    -- ** drive.teamdrives.delete
    , module Network.Google.Resource.Drive.Teamdrives.Delete

    -- ** drive.teamdrives.get
    , module Network.Google.Resource.Drive.Teamdrives.Get

    -- ** drive.teamdrives.list
    , module Network.Google.Resource.Drive.Teamdrives.List

    -- ** drive.teamdrives.update
    , module Network.Google.Resource.Drive.Teamdrives.Update

    -- * Types

    -- ** FileList
    , FileList
    , fileList
    , flNextPageToken
    , flIncompleteSearch
    , flKind
    , flFiles

    -- ** TeamDriveCapabilities
    , TeamDriveCapabilities
    , teamDriveCapabilities
    , tdcCanRename
    , tdcCanComment
    , tdcCanRenameTeamDrive
    , tdcCanChangeTeamDriveBackgRound
    , tdcCanDownload
    , tdcCanAddChildren
    , tdcCanRemoveChildren
    , tdcCanDeleteTeamDrive
    , tdcCanListChildren
    , tdcCanEdit
    , tdcCanManageMembers
    , tdcCanReadRevisions
    , tdcCanCopy
    , tdcCanShare

    -- ** FilesListCorpus
    , FilesListCorpus (..)

    -- ** CommentQuotedFileContent
    , CommentQuotedFileContent
    , commentQuotedFileContent
    , cqfcValue
    , cqfcMimeType

    -- ** AboutStorageQuota
    , AboutStorageQuota
    , aboutStorageQuota
    , asqUsageInDriveTrash
    , asqLimit
    , asqUsage
    , asqUsageInDrive

    -- ** Reply
    , Reply
    , reply
    , rHTMLContent
    , rModifiedTime
    , rCreatedTime
    , rKind
    , rAction
    , rContent
    , rAuthor
    , rId
    , rDeleted

    -- ** AboutImportFormats
    , AboutImportFormats
    , aboutImportFormats
    , aifAddtional

    -- ** FileCapabilities
    , FileCapabilities
    , fileCapabilities
    , fcCanRename
    , fcCanComment
    , fcCanDelete
    , fcCanMoveItemIntoTeamDrive
    , fcCanDownload
    , fcCanTrash
    , fcCanUntrash
    , fcCanAddChildren
    , fcCanRemoveChildren
    , fcCanMoveTeamDriveItem
    , fcCanReadTeamDrive
    , fcCanListChildren
    , fcCanEdit
    , fcCanChangeViewersCanCopyContent
    , fcCanReadRevisions
    , fcCanCopy
    , fcCanShare

    -- ** ReplyList
    , ReplyList
    , replyList
    , rlNextPageToken
    , rlKind
    , rlReplies

    -- ** FileContentHintsThumbnail
    , FileContentHintsThumbnail
    , fileContentHintsThumbnail
    , fchtImage
    , fchtMimeType

    -- ** TeamDriveList
    , TeamDriveList
    , teamDriveList
    , tdlNextPageToken
    , tdlTeamDrives
    , tdlKind

    -- ** Channel
    , Channel
    , channel
    , cResourceURI
    , cResourceId
    , cKind
    , cExpiration
    , cToken
    , cAddress
    , cPayload
    , cParams
    , cId
    , cType

    -- ** AboutTeamDriveThemesItem
    , AboutTeamDriveThemesItem
    , aboutTeamDriveThemesItem
    , atdtiColorRgb
    , atdtiBackgRoundImageLink
    , atdtiId

    -- ** TeamDriveBackgRoundImageFile
    , TeamDriveBackgRoundImageFile
    , teamDriveBackgRoundImageFile
    , tdbrifXCoordinate
    , tdbrifYCoordinate
    , tdbrifWidth
    , tdbrifId

    -- ** FileVideoMediaMetadata
    , FileVideoMediaMetadata
    , fileVideoMediaMetadata
    , fvmmHeight
    , fvmmWidth
    , fvmmDurationMillis

    -- ** FileAppProperties
    , FileAppProperties
    , fileAppProperties
    , fapAddtional

    -- ** Change
    , Change
    , change
    , chaRemoved
    , chaTime
    , chaKind
    , chaTeamDrive
    , chaTeamDriveId
    , chaType
    , chaFileId
    , chaFile

    -- ** TeamDrive
    , TeamDrive
    , teamDrive
    , tdThemeId
    , tdBackgRoundImageFile
    , tdColorRgb
    , tdCreatedTime
    , tdKind
    , tdBackgRoundImageLink
    , tdName
    , tdId
    , tdCapabilities

    -- ** AboutExportFormats
    , AboutExportFormats
    , aboutExportFormats
    , aefAddtional

    -- ** User
    , User
    , user
    , uPhotoLink
    , uMe
    , uKind
    , uEmailAddress
    , uDisplayName
    , uPermissionId

    -- ** ChangeList
    , ChangeList
    , changeList
    , clNewStartPageToken
    , clNextPageToken
    , clChanges
    , clKind

    -- ** FileContentHints
    , FileContentHints
    , fileContentHints
    , fchThumbnail
    , fchIndexableText

    -- ** ChannelParams
    , ChannelParams
    , channelParams
    , cpAddtional

    -- ** FileProperties
    , FileProperties
    , fileProperties
    , fpAddtional

    -- ** AboutMaxImportSizes
    , AboutMaxImportSizes
    , aboutMaxImportSizes
    , amisAddtional

    -- ** About
    , About
    , about
    , aExportFormats
    , aMaxImportSizes
    , aCanCreateTeamDrives
    , aImportFormats
    , aKind
    , aAppInstalled
    , aUser
    , aStorageQuota
    , aMaxUploadSize
    , aTeamDriveThemes
    , aFolderColorPalette

    -- ** FileImageMediaMetadataLocation
    , FileImageMediaMetadataLocation
    , fileImageMediaMetadataLocation
    , fimmlLatitude
    , fimmlAltitude
    , fimmlLongitude

    -- ** StartPageToken
    , StartPageToken
    , startPageToken
    , sptKind
    , sptStartPageToken

    -- ** FileImageMediaMetadata
    , FileImageMediaMetadata
    , fileImageMediaMetadata
    , fimmRotation
    , fimmHeight
    , fimmSubjectDistance
    , fimmMaxApertureValue
    , fimmIsoSpeed
    , fimmTime
    , fimmLocation
    , fimmAperture
    , fimmFocalLength
    , fimmCameraMake
    , fimmWidth
    , fimmExposureTime
    , fimmCameraModel
    , fimmWhiteBalance
    , fimmLens
    , fimmFlashUsed
    , fimmExposureBias
    , fimmMeteringMode
    , fimmExposureMode
    , fimmSensor
    , fimmColorSpace

    -- ** Comment
    , Comment
    , comment
    , comHTMLContent
    , comModifiedTime
    , comCreatedTime
    , comKind
    , comResolved
    , comQuotedFileContent
    , comAnchor
    , comContent
    , comReplies
    , comAuthor
    , comId
    , comDeleted

    -- ** Revision
    , Revision
    , revision
    , revModifiedTime
    , revSize
    , revOriginalFilename
    , revKind
    , revPublished
    , revLastModifyingUser
    , revPublishAuto
    , revMD5Checksum
    , revKeepForever
    , revMimeType
    , revPublishedOutsideDomain
    , revId

    -- ** Permission
    , Permission
    , permission
    , pPhotoLink
    , pTeamDrivePermissionDetails
    , pKind
    , pDomain
    , pRole
    , pEmailAddress
    , pAllowFileDiscovery
    , pDisplayName
    , pId
    , pDeleted
    , pType
    , pExpirationTime

    -- ** File
    , File
    , file
    , fOwnedByMe
    , fThumbnailLink
    , fFullFileExtension
    , fModifiedTime
    , fModifiedByMeTime
    , fFileExtension
    , fViewedByMe
    , fOwners
    , fViewedByMeTime
    , fModifiedByMe
    , fSize
    , fTrashed
    , fWebViewLink
    , fCreatedTime
    , fTrashedTime
    , fOriginalFilename
    , fKind
    , fLastModifyingUser
    , fIconLink
    , fHasThumbnail
    , fThumbnailVersion
    , fImageMediaMetadata
    , fExplicitlyTrashed
    , fShared
    , fMD5Checksum
    , fTeamDriveId
    , fFolderColorRgb
    , fMimeType
    , fIsAppAuthorized
    , fName
    , fParents
    , fStarred
    , fSpaces
    , fVersion
    , fHasAugmentedPermissions
    , fWritersCanShare
    , fTrashingUser
    , fId
    , fPermissionIds
    , fPermissions
    , fQuotaBytesUsed
    , fAppProperties
    , fVideoMediaMetadata
    , fSharedWithMeTime
    , fHeadRevisionId
    , fCapabilities
    , fDescription
    , fViewersCanCopyContent
    , fSharingUser
    , fWebContentLink
    , fContentHints
    , fProperties

    -- ** PermissionTeamDrivePermissionDetailsItem
    , PermissionTeamDrivePermissionDetailsItem
    , permissionTeamDrivePermissionDetailsItem
    , ptdpdiInherited
    , ptdpdiTeamDrivePermissionType
    , ptdpdiRole
    , ptdpdiInheritedFrom

    -- ** GeneratedIds
    , GeneratedIds
    , generatedIds
    , giSpace
    , giKind
    , giIds

    -- ** CommentList
    , CommentList
    , commentList
    , cllNextPageToken
    , cllKind
    , cllComments

    -- ** RevisionList
    , RevisionList
    , revisionList
    , rllNextPageToken
    , rllKind
    , rllRevisions

    -- ** PermissionList
    , PermissionList
    , permissionList
    , plNextPageToken
    , plKind
    , plPermissions
    ) where

import Network.Google.Prelude
import Network.Google.Drive.Types
import Network.Google.Resource.Drive.About.Get
import Network.Google.Resource.Drive.Changes.GetStartPageToken
import Network.Google.Resource.Drive.Changes.List
import Network.Google.Resource.Drive.Changes.Watch
import Network.Google.Resource.Drive.Channels.Stop
import Network.Google.Resource.Drive.Comments.Create
import Network.Google.Resource.Drive.Comments.Delete
import Network.Google.Resource.Drive.Comments.Get
import Network.Google.Resource.Drive.Comments.List
import Network.Google.Resource.Drive.Comments.Update
import Network.Google.Resource.Drive.Files.Copy
import Network.Google.Resource.Drive.Files.Create
import Network.Google.Resource.Drive.Files.Delete
import Network.Google.Resource.Drive.Files.EmptyTrash
import Network.Google.Resource.Drive.Files.Export
import Network.Google.Resource.Drive.Files.GenerateIds
import Network.Google.Resource.Drive.Files.Get
import Network.Google.Resource.Drive.Files.List
import Network.Google.Resource.Drive.Files.Update
import Network.Google.Resource.Drive.Files.Watch
import Network.Google.Resource.Drive.Permissions.Create
import Network.Google.Resource.Drive.Permissions.Delete
import Network.Google.Resource.Drive.Permissions.Get
import Network.Google.Resource.Drive.Permissions.List
import Network.Google.Resource.Drive.Permissions.Update
import Network.Google.Resource.Drive.Replies.Create
import Network.Google.Resource.Drive.Replies.Delete
import Network.Google.Resource.Drive.Replies.Get
import Network.Google.Resource.Drive.Replies.List
import Network.Google.Resource.Drive.Replies.Update
import Network.Google.Resource.Drive.Revisions.Delete
import Network.Google.Resource.Drive.Revisions.Get
import Network.Google.Resource.Drive.Revisions.List
import Network.Google.Resource.Drive.Revisions.Update
import Network.Google.Resource.Drive.Teamdrives.Create
import Network.Google.Resource.Drive.Teamdrives.Delete
import Network.Google.Resource.Drive.Teamdrives.Get
import Network.Google.Resource.Drive.Teamdrives.List
import Network.Google.Resource.Drive.Teamdrives.Update

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Drive API service.
type DriveAPI =
     TeamdrivesListResource :<|> TeamdrivesGetResource
       :<|> TeamdrivesCreateResource
       :<|> TeamdrivesDeleteResource
       :<|> TeamdrivesUpdateResource
       :<|> ChangesListResource
       :<|> ChangesGetStartPageTokenResource
       :<|> ChangesWatchResource
       :<|> ChannelsStopResource
       :<|> RepliesListResource
       :<|> RepliesGetResource
       :<|> RepliesCreateResource
       :<|> RepliesDeleteResource
       :<|> RepliesUpdateResource
       :<|> AboutGetResource
       :<|> FilesExportResource
       :<|> FilesListResource
       :<|> FilesCopyResource
       :<|> FilesGetResource
       :<|> FilesEmptyTrashResource
       :<|> FilesCreateResource
       :<|> FilesGenerateIdsResource
       :<|> FilesDeleteResource
       :<|> FilesUpdateResource
       :<|> FilesWatchResource
       :<|> PermissionsListResource
       :<|> PermissionsGetResource
       :<|> PermissionsCreateResource
       :<|> PermissionsDeleteResource
       :<|> PermissionsUpdateResource
       :<|> CommentsListResource
       :<|> CommentsGetResource
       :<|> CommentsCreateResource
       :<|> CommentsDeleteResource
       :<|> CommentsUpdateResource
       :<|> RevisionsListResource
       :<|> RevisionsGetResource
       :<|> RevisionsDeleteResource
       :<|> RevisionsUpdateResource
