{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Drive.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Drive.Types.Product where

import Network.Google.Drive.Types.Sum
import Network.Google.Prelude

-- | A list of files.
--
-- /See:/ 'fileList' smart constructor.
data FileList = FileList'
    { _flNextPageToken :: !(Maybe Text)
    , _flIncompleteSearch :: !(Maybe Bool)
    , _flKind :: !Text
    , _flFiles :: !(Maybe [File])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flNextPageToken'
--
-- * 'flIncompleteSearch'
--
-- * 'flKind'
--
-- * 'flFiles'
fileList
    :: FileList
fileList = 
    FileList'
    { _flNextPageToken = Nothing
    , _flIncompleteSearch = Nothing
    , _flKind = "drive#fileList"
    , _flFiles = Nothing
    }

-- | The page token for the next page of files. This will be absent if the
-- end of the files list has been reached. If the token is rejected for any
-- reason, it should be discarded, and pagination should be restarted from
-- the first page of results.
flNextPageToken :: Lens' FileList (Maybe Text)
flNextPageToken
  = lens _flNextPageToken
      (\ s a -> s{_flNextPageToken = a})

-- | Whether the search process was incomplete. If true, then some search
-- results may be missing, since all documents were not searched. This may
-- occur when searching multiple Team Drives with the
-- \"user,allTeamDrives\" corpora, but all corpora could not be searched.
-- When this happens, it is suggested that clients narrow their query by
-- choosing a different corpus such as \"user\" or \"teamDrive\".
flIncompleteSearch :: Lens' FileList (Maybe Bool)
flIncompleteSearch
  = lens _flIncompleteSearch
      (\ s a -> s{_flIncompleteSearch = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#fileList\".
flKind :: Lens' FileList Text
flKind = lens _flKind (\ s a -> s{_flKind = a})

-- | The list of files. If nextPageToken is populated, then this list may be
-- incomplete and an additional page of results should be fetched.
flFiles :: Lens' FileList [File]
flFiles
  = lens _flFiles (\ s a -> s{_flFiles = a}) . _Default
      . _Coerce

instance FromJSON FileList where
        parseJSON
          = withObject "FileList"
              (\ o ->
                 FileList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "incompleteSearch")
                     <*> (o .:? "kind" .!= "drive#fileList")
                     <*> (o .:? "files" .!= mempty))

instance ToJSON FileList where
        toJSON FileList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _flNextPageToken,
                  ("incompleteSearch" .=) <$> _flIncompleteSearch,
                  Just ("kind" .= _flKind), ("files" .=) <$> _flFiles])

-- | Capabilities the current user has on this Team Drive.
--
-- /See:/ 'teamDriveCapabilities' smart constructor.
data TeamDriveCapabilities = TeamDriveCapabilities'
    { _tdcCanRename :: !(Maybe Bool)
    , _tdcCanComment :: !(Maybe Bool)
    , _tdcCanRenameTeamDrive :: !(Maybe Bool)
    , _tdcCanChangeTeamDriveBackgRound :: !(Maybe Bool)
    , _tdcCanDownload :: !(Maybe Bool)
    , _tdcCanAddChildren :: !(Maybe Bool)
    , _tdcCanRemoveChildren :: !(Maybe Bool)
    , _tdcCanDeleteTeamDrive :: !(Maybe Bool)
    , _tdcCanListChildren :: !(Maybe Bool)
    , _tdcCanEdit :: !(Maybe Bool)
    , _tdcCanManageMembers :: !(Maybe Bool)
    , _tdcCanReadRevisions :: !(Maybe Bool)
    , _tdcCanCopy :: !(Maybe Bool)
    , _tdcCanShare :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamDriveCapabilities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdcCanRename'
--
-- * 'tdcCanComment'
--
-- * 'tdcCanRenameTeamDrive'
--
-- * 'tdcCanChangeTeamDriveBackgRound'
--
-- * 'tdcCanDownload'
--
-- * 'tdcCanAddChildren'
--
-- * 'tdcCanRemoveChildren'
--
-- * 'tdcCanDeleteTeamDrive'
--
-- * 'tdcCanListChildren'
--
-- * 'tdcCanEdit'
--
-- * 'tdcCanManageMembers'
--
-- * 'tdcCanReadRevisions'
--
-- * 'tdcCanCopy'
--
-- * 'tdcCanShare'
teamDriveCapabilities
    :: TeamDriveCapabilities
teamDriveCapabilities = 
    TeamDriveCapabilities'
    { _tdcCanRename = Nothing
    , _tdcCanComment = Nothing
    , _tdcCanRenameTeamDrive = Nothing
    , _tdcCanChangeTeamDriveBackgRound = Nothing
    , _tdcCanDownload = Nothing
    , _tdcCanAddChildren = Nothing
    , _tdcCanRemoveChildren = Nothing
    , _tdcCanDeleteTeamDrive = Nothing
    , _tdcCanListChildren = Nothing
    , _tdcCanEdit = Nothing
    , _tdcCanManageMembers = Nothing
    , _tdcCanReadRevisions = Nothing
    , _tdcCanCopy = Nothing
    , _tdcCanShare = Nothing
    }

-- | Whether the current user can rename files or folders in this Team Drive.
tdcCanRename :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanRename
  = lens _tdcCanRename (\ s a -> s{_tdcCanRename = a})

-- | Whether the current user can comment on files in this Team Drive.
tdcCanComment :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanComment
  = lens _tdcCanComment
      (\ s a -> s{_tdcCanComment = a})

-- | Whether the current user can rename this Team Drive.
tdcCanRenameTeamDrive :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanRenameTeamDrive
  = lens _tdcCanRenameTeamDrive
      (\ s a -> s{_tdcCanRenameTeamDrive = a})

-- | Whether the current user can change the background of this Team Drive.
tdcCanChangeTeamDriveBackgRound :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanChangeTeamDriveBackgRound
  = lens _tdcCanChangeTeamDriveBackgRound
      (\ s a -> s{_tdcCanChangeTeamDriveBackgRound = a})

-- | Whether the current user can download files in this Team Drive.
tdcCanDownload :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanDownload
  = lens _tdcCanDownload
      (\ s a -> s{_tdcCanDownload = a})

-- | Whether the current user can add children to folders in this Team Drive.
tdcCanAddChildren :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanAddChildren
  = lens _tdcCanAddChildren
      (\ s a -> s{_tdcCanAddChildren = a})

-- | Whether the current user can remove children from folders in this Team
-- Drive.
tdcCanRemoveChildren :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanRemoveChildren
  = lens _tdcCanRemoveChildren
      (\ s a -> s{_tdcCanRemoveChildren = a})

-- | Whether the current user can delete this Team Drive. Attempting to
-- delete the Team Drive may still fail if there are untrashed items inside
-- the Team Drive.
tdcCanDeleteTeamDrive :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanDeleteTeamDrive
  = lens _tdcCanDeleteTeamDrive
      (\ s a -> s{_tdcCanDeleteTeamDrive = a})

-- | Whether the current user can list the children of folders in this Team
-- Drive.
tdcCanListChildren :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanListChildren
  = lens _tdcCanListChildren
      (\ s a -> s{_tdcCanListChildren = a})

-- | Whether the current user can edit files in this Team Drive
tdcCanEdit :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanEdit
  = lens _tdcCanEdit (\ s a -> s{_tdcCanEdit = a})

-- | Whether the current user can add members to this Team Drive or remove
-- them or change their role.
tdcCanManageMembers :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanManageMembers
  = lens _tdcCanManageMembers
      (\ s a -> s{_tdcCanManageMembers = a})

-- | Whether the current user can read the revisions resource of files in
-- this Team Drive.
tdcCanReadRevisions :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanReadRevisions
  = lens _tdcCanReadRevisions
      (\ s a -> s{_tdcCanReadRevisions = a})

-- | Whether the current user can copy files in this Team Drive.
tdcCanCopy :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanCopy
  = lens _tdcCanCopy (\ s a -> s{_tdcCanCopy = a})

-- | Whether the current user can share files or folders in this Team Drive.
tdcCanShare :: Lens' TeamDriveCapabilities (Maybe Bool)
tdcCanShare
  = lens _tdcCanShare (\ s a -> s{_tdcCanShare = a})

instance FromJSON TeamDriveCapabilities where
        parseJSON
          = withObject "TeamDriveCapabilities"
              (\ o ->
                 TeamDriveCapabilities' <$>
                   (o .:? "canRename") <*> (o .:? "canComment") <*>
                     (o .:? "canRenameTeamDrive")
                     <*> (o .:? "canChangeTeamDriveBackground")
                     <*> (o .:? "canDownload")
                     <*> (o .:? "canAddChildren")
                     <*> (o .:? "canRemoveChildren")
                     <*> (o .:? "canDeleteTeamDrive")
                     <*> (o .:? "canListChildren")
                     <*> (o .:? "canEdit")
                     <*> (o .:? "canManageMembers")
                     <*> (o .:? "canReadRevisions")
                     <*> (o .:? "canCopy")
                     <*> (o .:? "canShare"))

instance ToJSON TeamDriveCapabilities where
        toJSON TeamDriveCapabilities'{..}
          = object
              (catMaybes
                 [("canRename" .=) <$> _tdcCanRename,
                  ("canComment" .=) <$> _tdcCanComment,
                  ("canRenameTeamDrive" .=) <$> _tdcCanRenameTeamDrive,
                  ("canChangeTeamDriveBackground" .=) <$>
                    _tdcCanChangeTeamDriveBackgRound,
                  ("canDownload" .=) <$> _tdcCanDownload,
                  ("canAddChildren" .=) <$> _tdcCanAddChildren,
                  ("canRemoveChildren" .=) <$> _tdcCanRemoveChildren,
                  ("canDeleteTeamDrive" .=) <$> _tdcCanDeleteTeamDrive,
                  ("canListChildren" .=) <$> _tdcCanListChildren,
                  ("canEdit" .=) <$> _tdcCanEdit,
                  ("canManageMembers" .=) <$> _tdcCanManageMembers,
                  ("canReadRevisions" .=) <$> _tdcCanReadRevisions,
                  ("canCopy" .=) <$> _tdcCanCopy,
                  ("canShare" .=) <$> _tdcCanShare])

-- | The file content to which the comment refers, typically within the
-- anchor region. For a text file, for example, this would be the text at
-- the location of the comment.
--
-- /See:/ 'commentQuotedFileContent' smart constructor.
data CommentQuotedFileContent = CommentQuotedFileContent'
    { _cqfcValue :: !(Maybe Text)
    , _cqfcMimeType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommentQuotedFileContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqfcValue'
--
-- * 'cqfcMimeType'
commentQuotedFileContent
    :: CommentQuotedFileContent
commentQuotedFileContent = 
    CommentQuotedFileContent'
    { _cqfcValue = Nothing
    , _cqfcMimeType = Nothing
    }

-- | The quoted content itself. This is interpreted as plain text if set
-- through the API.
cqfcValue :: Lens' CommentQuotedFileContent (Maybe Text)
cqfcValue
  = lens _cqfcValue (\ s a -> s{_cqfcValue = a})

-- | The MIME type of the quoted content.
cqfcMimeType :: Lens' CommentQuotedFileContent (Maybe Text)
cqfcMimeType
  = lens _cqfcMimeType (\ s a -> s{_cqfcMimeType = a})

instance FromJSON CommentQuotedFileContent where
        parseJSON
          = withObject "CommentQuotedFileContent"
              (\ o ->
                 CommentQuotedFileContent' <$>
                   (o .:? "value") <*> (o .:? "mimeType"))

instance ToJSON CommentQuotedFileContent where
        toJSON CommentQuotedFileContent'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _cqfcValue,
                  ("mimeType" .=) <$> _cqfcMimeType])

-- | The user\'s storage quota limits and usage. All fields are measured in
-- bytes.
--
-- /See:/ 'aboutStorageQuota' smart constructor.
data AboutStorageQuota = AboutStorageQuota'
    { _asqUsageInDriveTrash :: !(Maybe (Textual Int64))
    , _asqLimit :: !(Maybe (Textual Int64))
    , _asqUsage :: !(Maybe (Textual Int64))
    , _asqUsageInDrive :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AboutStorageQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asqUsageInDriveTrash'
--
-- * 'asqLimit'
--
-- * 'asqUsage'
--
-- * 'asqUsageInDrive'
aboutStorageQuota
    :: AboutStorageQuota
aboutStorageQuota = 
    AboutStorageQuota'
    { _asqUsageInDriveTrash = Nothing
    , _asqLimit = Nothing
    , _asqUsage = Nothing
    , _asqUsageInDrive = Nothing
    }

-- | The usage by trashed files in Google Drive.
asqUsageInDriveTrash :: Lens' AboutStorageQuota (Maybe Int64)
asqUsageInDriveTrash
  = lens _asqUsageInDriveTrash
      (\ s a -> s{_asqUsageInDriveTrash = a})
      . mapping _Coerce

-- | The usage limit, if applicable. This will not be present if the user has
-- unlimited storage.
asqLimit :: Lens' AboutStorageQuota (Maybe Int64)
asqLimit
  = lens _asqLimit (\ s a -> s{_asqLimit = a}) .
      mapping _Coerce

-- | The total usage across all services.
asqUsage :: Lens' AboutStorageQuota (Maybe Int64)
asqUsage
  = lens _asqUsage (\ s a -> s{_asqUsage = a}) .
      mapping _Coerce

-- | The usage by all files in Google Drive.
asqUsageInDrive :: Lens' AboutStorageQuota (Maybe Int64)
asqUsageInDrive
  = lens _asqUsageInDrive
      (\ s a -> s{_asqUsageInDrive = a})
      . mapping _Coerce

instance FromJSON AboutStorageQuota where
        parseJSON
          = withObject "AboutStorageQuota"
              (\ o ->
                 AboutStorageQuota' <$>
                   (o .:? "usageInDriveTrash") <*> (o .:? "limit") <*>
                     (o .:? "usage")
                     <*> (o .:? "usageInDrive"))

instance ToJSON AboutStorageQuota where
        toJSON AboutStorageQuota'{..}
          = object
              (catMaybes
                 [("usageInDriveTrash" .=) <$> _asqUsageInDriveTrash,
                  ("limit" .=) <$> _asqLimit,
                  ("usage" .=) <$> _asqUsage,
                  ("usageInDrive" .=) <$> _asqUsageInDrive])

-- | A reply to a comment on a file.
--
-- /See:/ 'reply' smart constructor.
data Reply = Reply'
    { _rHTMLContent :: !(Maybe Text)
    , _rModifiedTime :: !(Maybe DateTime')
    , _rCreatedTime :: !(Maybe DateTime')
    , _rKind :: !Text
    , _rAction :: !(Maybe Text)
    , _rContent :: !(Maybe Text)
    , _rAuthor :: !(Maybe User)
    , _rId :: !(Maybe Text)
    , _rDeleted :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Reply' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHTMLContent'
--
-- * 'rModifiedTime'
--
-- * 'rCreatedTime'
--
-- * 'rKind'
--
-- * 'rAction'
--
-- * 'rContent'
--
-- * 'rAuthor'
--
-- * 'rId'
--
-- * 'rDeleted'
reply
    :: Reply
reply = 
    Reply'
    { _rHTMLContent = Nothing
    , _rModifiedTime = Nothing
    , _rCreatedTime = Nothing
    , _rKind = "drive#reply"
    , _rAction = Nothing
    , _rContent = Nothing
    , _rAuthor = Nothing
    , _rId = Nothing
    , _rDeleted = Nothing
    }

-- | The content of the reply with HTML formatting.
rHTMLContent :: Lens' Reply (Maybe Text)
rHTMLContent
  = lens _rHTMLContent (\ s a -> s{_rHTMLContent = a})

-- | The last time the reply was modified (RFC 3339 date-time).
rModifiedTime :: Lens' Reply (Maybe UTCTime)
rModifiedTime
  = lens _rModifiedTime
      (\ s a -> s{_rModifiedTime = a})
      . mapping _DateTime

-- | The time at which the reply was created (RFC 3339 date-time).
rCreatedTime :: Lens' Reply (Maybe UTCTime)
rCreatedTime
  = lens _rCreatedTime (\ s a -> s{_rCreatedTime = a})
      . mapping _DateTime

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#reply\".
rKind :: Lens' Reply Text
rKind = lens _rKind (\ s a -> s{_rKind = a})

-- | The action the reply performed to the parent comment. Valid values are:
-- - resolve - reopen
rAction :: Lens' Reply (Maybe Text)
rAction = lens _rAction (\ s a -> s{_rAction = a})

-- | The plain text content of the reply. This field is used for setting the
-- content, while htmlContent should be displayed. This is required on
-- creates if no action is specified.
rContent :: Lens' Reply (Maybe Text)
rContent = lens _rContent (\ s a -> s{_rContent = a})

-- | The user who created the reply.
rAuthor :: Lens' Reply (Maybe User)
rAuthor = lens _rAuthor (\ s a -> s{_rAuthor = a})

-- | The ID of the reply.
rId :: Lens' Reply (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a})

-- | Whether the reply has been deleted. A deleted reply has no content.
rDeleted :: Lens' Reply (Maybe Bool)
rDeleted = lens _rDeleted (\ s a -> s{_rDeleted = a})

instance FromJSON Reply where
        parseJSON
          = withObject "Reply"
              (\ o ->
                 Reply' <$>
                   (o .:? "htmlContent") <*> (o .:? "modifiedTime") <*>
                     (o .:? "createdTime")
                     <*> (o .:? "kind" .!= "drive#reply")
                     <*> (o .:? "action")
                     <*> (o .:? "content")
                     <*> (o .:? "author")
                     <*> (o .:? "id")
                     <*> (o .:? "deleted"))

instance ToJSON Reply where
        toJSON Reply'{..}
          = object
              (catMaybes
                 [("htmlContent" .=) <$> _rHTMLContent,
                  ("modifiedTime" .=) <$> _rModifiedTime,
                  ("createdTime" .=) <$> _rCreatedTime,
                  Just ("kind" .= _rKind), ("action" .=) <$> _rAction,
                  ("content" .=) <$> _rContent,
                  ("author" .=) <$> _rAuthor, ("id" .=) <$> _rId,
                  ("deleted" .=) <$> _rDeleted])

-- | A map of source MIME type to possible targets for all supported imports.
--
-- /See:/ 'aboutImportFormats' smart constructor.
newtype AboutImportFormats = AboutImportFormats'
    { _aifAddtional :: HashMap Text [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AboutImportFormats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifAddtional'
aboutImportFormats
    :: HashMap Text [Text] -- ^ 'aifAddtional'
    -> AboutImportFormats
aboutImportFormats pAifAddtional_ = 
    AboutImportFormats'
    { _aifAddtional = _Coerce # pAifAddtional_
    }

aifAddtional :: Lens' AboutImportFormats (HashMap Text [Text])
aifAddtional
  = lens _aifAddtional (\ s a -> s{_aifAddtional = a})
      . _Coerce

instance FromJSON AboutImportFormats where
        parseJSON
          = withObject "AboutImportFormats"
              (\ o -> AboutImportFormats' <$> (parseJSONObject o))

instance ToJSON AboutImportFormats where
        toJSON = toJSON . _aifAddtional

-- | Capabilities the current user has on this file. Each capability
-- corresponds to a fine-grained action that a user may take.
--
-- /See:/ 'fileCapabilities' smart constructor.
data FileCapabilities = FileCapabilities'
    { _fcCanRename :: !(Maybe Bool)
    , _fcCanComment :: !(Maybe Bool)
    , _fcCanDelete :: !(Maybe Bool)
    , _fcCanMoveItemIntoTeamDrive :: !(Maybe Bool)
    , _fcCanDownload :: !(Maybe Bool)
    , _fcCanTrash :: !(Maybe Bool)
    , _fcCanUntrash :: !(Maybe Bool)
    , _fcCanAddChildren :: !(Maybe Bool)
    , _fcCanRemoveChildren :: !(Maybe Bool)
    , _fcCanMoveTeamDriveItem :: !(Maybe Bool)
    , _fcCanReadTeamDrive :: !(Maybe Bool)
    , _fcCanListChildren :: !(Maybe Bool)
    , _fcCanEdit :: !(Maybe Bool)
    , _fcCanChangeViewersCanCopyContent :: !(Maybe Bool)
    , _fcCanReadRevisions :: !(Maybe Bool)
    , _fcCanCopy :: !(Maybe Bool)
    , _fcCanShare :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileCapabilities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcCanRename'
--
-- * 'fcCanComment'
--
-- * 'fcCanDelete'
--
-- * 'fcCanMoveItemIntoTeamDrive'
--
-- * 'fcCanDownload'
--
-- * 'fcCanTrash'
--
-- * 'fcCanUntrash'
--
-- * 'fcCanAddChildren'
--
-- * 'fcCanRemoveChildren'
--
-- * 'fcCanMoveTeamDriveItem'
--
-- * 'fcCanReadTeamDrive'
--
-- * 'fcCanListChildren'
--
-- * 'fcCanEdit'
--
-- * 'fcCanChangeViewersCanCopyContent'
--
-- * 'fcCanReadRevisions'
--
-- * 'fcCanCopy'
--
-- * 'fcCanShare'
fileCapabilities
    :: FileCapabilities
fileCapabilities = 
    FileCapabilities'
    { _fcCanRename = Nothing
    , _fcCanComment = Nothing
    , _fcCanDelete = Nothing
    , _fcCanMoveItemIntoTeamDrive = Nothing
    , _fcCanDownload = Nothing
    , _fcCanTrash = Nothing
    , _fcCanUntrash = Nothing
    , _fcCanAddChildren = Nothing
    , _fcCanRemoveChildren = Nothing
    , _fcCanMoveTeamDriveItem = Nothing
    , _fcCanReadTeamDrive = Nothing
    , _fcCanListChildren = Nothing
    , _fcCanEdit = Nothing
    , _fcCanChangeViewersCanCopyContent = Nothing
    , _fcCanReadRevisions = Nothing
    , _fcCanCopy = Nothing
    , _fcCanShare = Nothing
    }

-- | Whether the current user can rename this file.
fcCanRename :: Lens' FileCapabilities (Maybe Bool)
fcCanRename
  = lens _fcCanRename (\ s a -> s{_fcCanRename = a})

-- | Whether the current user can comment on this file.
fcCanComment :: Lens' FileCapabilities (Maybe Bool)
fcCanComment
  = lens _fcCanComment (\ s a -> s{_fcCanComment = a})

-- | Whether the current user can delete this file.
fcCanDelete :: Lens' FileCapabilities (Maybe Bool)
fcCanDelete
  = lens _fcCanDelete (\ s a -> s{_fcCanDelete = a})

-- | Whether the current user can move this item into a Team Drive. If the
-- item is in a Team Drive, this field is equivalent to
-- canMoveTeamDriveItem.
fcCanMoveItemIntoTeamDrive :: Lens' FileCapabilities (Maybe Bool)
fcCanMoveItemIntoTeamDrive
  = lens _fcCanMoveItemIntoTeamDrive
      (\ s a -> s{_fcCanMoveItemIntoTeamDrive = a})

-- | Whether the current user can download this file.
fcCanDownload :: Lens' FileCapabilities (Maybe Bool)
fcCanDownload
  = lens _fcCanDownload
      (\ s a -> s{_fcCanDownload = a})

-- | Whether the current user can move this file to trash.
fcCanTrash :: Lens' FileCapabilities (Maybe Bool)
fcCanTrash
  = lens _fcCanTrash (\ s a -> s{_fcCanTrash = a})

-- | Whether the current user can restore this file from trash.
fcCanUntrash :: Lens' FileCapabilities (Maybe Bool)
fcCanUntrash
  = lens _fcCanUntrash (\ s a -> s{_fcCanUntrash = a})

-- | Whether the current user can add children to this folder. This is always
-- false when the item is not a folder.
fcCanAddChildren :: Lens' FileCapabilities (Maybe Bool)
fcCanAddChildren
  = lens _fcCanAddChildren
      (\ s a -> s{_fcCanAddChildren = a})

-- | Whether the current user can remove children from this folder. This is
-- always false when the item is not a folder.
fcCanRemoveChildren :: Lens' FileCapabilities (Maybe Bool)
fcCanRemoveChildren
  = lens _fcCanRemoveChildren
      (\ s a -> s{_fcCanRemoveChildren = a})

-- | Whether the current user can move this Team Drive item by changing its
-- parent. Note that a request to change the parent for this item may still
-- fail depending on the new parent that is being added. Only populated for
-- Team Drive files.
fcCanMoveTeamDriveItem :: Lens' FileCapabilities (Maybe Bool)
fcCanMoveTeamDriveItem
  = lens _fcCanMoveTeamDriveItem
      (\ s a -> s{_fcCanMoveTeamDriveItem = a})

-- | Whether the current user can read the Team Drive to which this file
-- belongs. Only populated for Team Drive files.
fcCanReadTeamDrive :: Lens' FileCapabilities (Maybe Bool)
fcCanReadTeamDrive
  = lens _fcCanReadTeamDrive
      (\ s a -> s{_fcCanReadTeamDrive = a})

-- | Whether the current user can list the children of this folder. This is
-- always false when the item is not a folder.
fcCanListChildren :: Lens' FileCapabilities (Maybe Bool)
fcCanListChildren
  = lens _fcCanListChildren
      (\ s a -> s{_fcCanListChildren = a})

-- | Whether the current user can edit this file.
fcCanEdit :: Lens' FileCapabilities (Maybe Bool)
fcCanEdit
  = lens _fcCanEdit (\ s a -> s{_fcCanEdit = a})

-- | Whether the current user can change whether viewers can copy the
-- contents of this file.
fcCanChangeViewersCanCopyContent :: Lens' FileCapabilities (Maybe Bool)
fcCanChangeViewersCanCopyContent
  = lens _fcCanChangeViewersCanCopyContent
      (\ s a -> s{_fcCanChangeViewersCanCopyContent = a})

-- | Whether the current user can read the revisions resource of this file.
-- For a Team Drive item, whether revisions of non-folder descendants of
-- this item, or this item itself if it is not a folder, can be read.
fcCanReadRevisions :: Lens' FileCapabilities (Maybe Bool)
fcCanReadRevisions
  = lens _fcCanReadRevisions
      (\ s a -> s{_fcCanReadRevisions = a})

-- | Whether the current user can copy this file. For a Team Drive item,
-- whether the current user can copy non-folder descendants of this item,
-- or this item itself if it is not a folder.
fcCanCopy :: Lens' FileCapabilities (Maybe Bool)
fcCanCopy
  = lens _fcCanCopy (\ s a -> s{_fcCanCopy = a})

-- | Whether the current user can modify the sharing settings for this file.
fcCanShare :: Lens' FileCapabilities (Maybe Bool)
fcCanShare
  = lens _fcCanShare (\ s a -> s{_fcCanShare = a})

instance FromJSON FileCapabilities where
        parseJSON
          = withObject "FileCapabilities"
              (\ o ->
                 FileCapabilities' <$>
                   (o .:? "canRename") <*> (o .:? "canComment") <*>
                     (o .:? "canDelete")
                     <*> (o .:? "canMoveItemIntoTeamDrive")
                     <*> (o .:? "canDownload")
                     <*> (o .:? "canTrash")
                     <*> (o .:? "canUntrash")
                     <*> (o .:? "canAddChildren")
                     <*> (o .:? "canRemoveChildren")
                     <*> (o .:? "canMoveTeamDriveItem")
                     <*> (o .:? "canReadTeamDrive")
                     <*> (o .:? "canListChildren")
                     <*> (o .:? "canEdit")
                     <*> (o .:? "canChangeViewersCanCopyContent")
                     <*> (o .:? "canReadRevisions")
                     <*> (o .:? "canCopy")
                     <*> (o .:? "canShare"))

instance ToJSON FileCapabilities where
        toJSON FileCapabilities'{..}
          = object
              (catMaybes
                 [("canRename" .=) <$> _fcCanRename,
                  ("canComment" .=) <$> _fcCanComment,
                  ("canDelete" .=) <$> _fcCanDelete,
                  ("canMoveItemIntoTeamDrive" .=) <$>
                    _fcCanMoveItemIntoTeamDrive,
                  ("canDownload" .=) <$> _fcCanDownload,
                  ("canTrash" .=) <$> _fcCanTrash,
                  ("canUntrash" .=) <$> _fcCanUntrash,
                  ("canAddChildren" .=) <$> _fcCanAddChildren,
                  ("canRemoveChildren" .=) <$> _fcCanRemoveChildren,
                  ("canMoveTeamDriveItem" .=) <$>
                    _fcCanMoveTeamDriveItem,
                  ("canReadTeamDrive" .=) <$> _fcCanReadTeamDrive,
                  ("canListChildren" .=) <$> _fcCanListChildren,
                  ("canEdit" .=) <$> _fcCanEdit,
                  ("canChangeViewersCanCopyContent" .=) <$>
                    _fcCanChangeViewersCanCopyContent,
                  ("canReadRevisions" .=) <$> _fcCanReadRevisions,
                  ("canCopy" .=) <$> _fcCanCopy,
                  ("canShare" .=) <$> _fcCanShare])

-- | A list of replies to a comment on a file.
--
-- /See:/ 'replyList' smart constructor.
data ReplyList = ReplyList'
    { _rlNextPageToken :: !(Maybe Text)
    , _rlKind :: !Text
    , _rlReplies :: !(Maybe [Reply])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplyList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlNextPageToken'
--
-- * 'rlKind'
--
-- * 'rlReplies'
replyList
    :: ReplyList
replyList = 
    ReplyList'
    { _rlNextPageToken = Nothing
    , _rlKind = "drive#replyList"
    , _rlReplies = Nothing
    }

-- | The page token for the next page of replies. This will be absent if the
-- end of the replies list has been reached. If the token is rejected for
-- any reason, it should be discarded, and pagination should be restarted
-- from the first page of results.
rlNextPageToken :: Lens' ReplyList (Maybe Text)
rlNextPageToken
  = lens _rlNextPageToken
      (\ s a -> s{_rlNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#replyList\".
rlKind :: Lens' ReplyList Text
rlKind = lens _rlKind (\ s a -> s{_rlKind = a})

-- | The list of replies. If nextPageToken is populated, then this list may
-- be incomplete and an additional page of results should be fetched.
rlReplies :: Lens' ReplyList [Reply]
rlReplies
  = lens _rlReplies (\ s a -> s{_rlReplies = a}) .
      _Default
      . _Coerce

instance FromJSON ReplyList where
        parseJSON
          = withObject "ReplyList"
              (\ o ->
                 ReplyList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "drive#replyList")
                     <*> (o .:? "replies" .!= mempty))

instance ToJSON ReplyList where
        toJSON ReplyList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _rlNextPageToken,
                  Just ("kind" .= _rlKind),
                  ("replies" .=) <$> _rlReplies])

-- | A thumbnail for the file. This will only be used if Drive cannot
-- generate a standard thumbnail.
--
-- /See:/ 'fileContentHintsThumbnail' smart constructor.
data FileContentHintsThumbnail = FileContentHintsThumbnail'
    { _fchtImage :: !(Maybe Bytes)
    , _fchtMimeType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileContentHintsThumbnail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fchtImage'
--
-- * 'fchtMimeType'
fileContentHintsThumbnail
    :: FileContentHintsThumbnail
fileContentHintsThumbnail = 
    FileContentHintsThumbnail'
    { _fchtImage = Nothing
    , _fchtMimeType = Nothing
    }

-- | The thumbnail data encoded with URL-safe Base64 (RFC 4648 section 5).
fchtImage :: Lens' FileContentHintsThumbnail (Maybe ByteString)
fchtImage
  = lens _fchtImage (\ s a -> s{_fchtImage = a}) .
      mapping _Bytes

-- | The MIME type of the thumbnail.
fchtMimeType :: Lens' FileContentHintsThumbnail (Maybe Text)
fchtMimeType
  = lens _fchtMimeType (\ s a -> s{_fchtMimeType = a})

instance FromJSON FileContentHintsThumbnail where
        parseJSON
          = withObject "FileContentHintsThumbnail"
              (\ o ->
                 FileContentHintsThumbnail' <$>
                   (o .:? "image") <*> (o .:? "mimeType"))

instance ToJSON FileContentHintsThumbnail where
        toJSON FileContentHintsThumbnail'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _fchtImage,
                  ("mimeType" .=) <$> _fchtMimeType])

-- | A list of Team Drives.
--
-- /See:/ 'teamDriveList' smart constructor.
data TeamDriveList = TeamDriveList'
    { _tdlNextPageToken :: !(Maybe Text)
    , _tdlTeamDrives :: !(Maybe [TeamDrive])
    , _tdlKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamDriveList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdlNextPageToken'
--
-- * 'tdlTeamDrives'
--
-- * 'tdlKind'
teamDriveList
    :: TeamDriveList
teamDriveList = 
    TeamDriveList'
    { _tdlNextPageToken = Nothing
    , _tdlTeamDrives = Nothing
    , _tdlKind = "drive#teamDriveList"
    }

-- | The page token for the next page of Team Drives. This will be absent if
-- the end of the Team Drives list has been reached. If the token is
-- rejected for any reason, it should be discarded, and pagination should
-- be restarted from the first page of results.
tdlNextPageToken :: Lens' TeamDriveList (Maybe Text)
tdlNextPageToken
  = lens _tdlNextPageToken
      (\ s a -> s{_tdlNextPageToken = a})

-- | The list of Team Drives. If nextPageToken is populated, then this list
-- may be incomplete and an additional page of results should be fetched.
tdlTeamDrives :: Lens' TeamDriveList [TeamDrive]
tdlTeamDrives
  = lens _tdlTeamDrives
      (\ s a -> s{_tdlTeamDrives = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#teamDriveList\".
tdlKind :: Lens' TeamDriveList Text
tdlKind = lens _tdlKind (\ s a -> s{_tdlKind = a})

instance FromJSON TeamDriveList where
        parseJSON
          = withObject "TeamDriveList"
              (\ o ->
                 TeamDriveList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "teamDrives" .!= mempty)
                     <*> (o .:? "kind" .!= "drive#teamDriveList"))

instance ToJSON TeamDriveList where
        toJSON TeamDriveList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _tdlNextPageToken,
                  ("teamDrives" .=) <$> _tdlTeamDrives,
                  Just ("kind" .= _tdlKind)])

-- | An notification channel used to watch for resource changes.
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
    { _cResourceURI :: !(Maybe Text)
    , _cResourceId :: !(Maybe Text)
    , _cKind :: !Text
    , _cExpiration :: !(Maybe (Textual Int64))
    , _cToken :: !(Maybe Text)
    , _cAddress :: !(Maybe Text)
    , _cPayload :: !(Maybe Bool)
    , _cParams :: !(Maybe ChannelParams)
    , _cId :: !(Maybe Text)
    , _cType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResourceURI'
--
-- * 'cResourceId'
--
-- * 'cKind'
--
-- * 'cExpiration'
--
-- * 'cToken'
--
-- * 'cAddress'
--
-- * 'cPayload'
--
-- * 'cParams'
--
-- * 'cId'
--
-- * 'cType'
channel
    :: Channel
channel = 
    Channel'
    { _cResourceURI = Nothing
    , _cResourceId = Nothing
    , _cKind = "api#channel"
    , _cExpiration = Nothing
    , _cToken = Nothing
    , _cAddress = Nothing
    , _cPayload = Nothing
    , _cParams = Nothing
    , _cId = Nothing
    , _cType = Nothing
    }

-- | A version-specific identifier for the watched resource.
cResourceURI :: Lens' Channel (Maybe Text)
cResourceURI
  = lens _cResourceURI (\ s a -> s{_cResourceURI = a})

-- | An opaque ID that identifies the resource being watched on this channel.
-- Stable across different API versions.
cResourceId :: Lens' Channel (Maybe Text)
cResourceId
  = lens _cResourceId (\ s a -> s{_cResourceId = a})

-- | Identifies this as a notification channel used to watch for changes to a
-- resource. Value: the fixed string \"api#channel\".
cKind :: Lens' Channel Text
cKind = lens _cKind (\ s a -> s{_cKind = a})

-- | Date and time of notification channel expiration, expressed as a Unix
-- timestamp, in milliseconds. Optional.
cExpiration :: Lens' Channel (Maybe Int64)
cExpiration
  = lens _cExpiration (\ s a -> s{_cExpiration = a}) .
      mapping _Coerce

-- | An arbitrary string delivered to the target address with each
-- notification delivered over this channel. Optional.
cToken :: Lens' Channel (Maybe Text)
cToken = lens _cToken (\ s a -> s{_cToken = a})

-- | The address where notifications are delivered for this channel.
cAddress :: Lens' Channel (Maybe Text)
cAddress = lens _cAddress (\ s a -> s{_cAddress = a})

-- | A Boolean value to indicate whether payload is wanted. Optional.
cPayload :: Lens' Channel (Maybe Bool)
cPayload = lens _cPayload (\ s a -> s{_cPayload = a})

-- | Additional parameters controlling delivery channel behavior. Optional.
cParams :: Lens' Channel (Maybe ChannelParams)
cParams = lens _cParams (\ s a -> s{_cParams = a})

-- | A UUID or similar unique string that identifies this channel.
cId :: Lens' Channel (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | The type of delivery mechanism used for this channel.
cType :: Lens' Channel (Maybe Text)
cType = lens _cType (\ s a -> s{_cType = a})

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ o ->
                 Channel' <$>
                   (o .:? "resourceUri") <*> (o .:? "resourceId") <*>
                     (o .:? "kind" .!= "api#channel")
                     <*> (o .:? "expiration")
                     <*> (o .:? "token")
                     <*> (o .:? "address")
                     <*> (o .:? "payload")
                     <*> (o .:? "params")
                     <*> (o .:? "id")
                     <*> (o .:? "type"))

instance ToJSON Channel where
        toJSON Channel'{..}
          = object
              (catMaybes
                 [("resourceUri" .=) <$> _cResourceURI,
                  ("resourceId" .=) <$> _cResourceId,
                  Just ("kind" .= _cKind),
                  ("expiration" .=) <$> _cExpiration,
                  ("token" .=) <$> _cToken,
                  ("address" .=) <$> _cAddress,
                  ("payload" .=) <$> _cPayload,
                  ("params" .=) <$> _cParams, ("id" .=) <$> _cId,
                  ("type" .=) <$> _cType])

--
-- /See:/ 'aboutTeamDriveThemesItem' smart constructor.
data AboutTeamDriveThemesItem = AboutTeamDriveThemesItem'
    { _atdtiColorRgb :: !(Maybe Text)
    , _atdtiBackgRoundImageLink :: !(Maybe Text)
    , _atdtiId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AboutTeamDriveThemesItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atdtiColorRgb'
--
-- * 'atdtiBackgRoundImageLink'
--
-- * 'atdtiId'
aboutTeamDriveThemesItem
    :: AboutTeamDriveThemesItem
aboutTeamDriveThemesItem = 
    AboutTeamDriveThemesItem'
    { _atdtiColorRgb = Nothing
    , _atdtiBackgRoundImageLink = Nothing
    , _atdtiId = Nothing
    }

-- | The color of this Team Drive theme as an RGB hex string.
atdtiColorRgb :: Lens' AboutTeamDriveThemesItem (Maybe Text)
atdtiColorRgb
  = lens _atdtiColorRgb
      (\ s a -> s{_atdtiColorRgb = a})

-- | A link to this Team Drive theme\'s background image.
atdtiBackgRoundImageLink :: Lens' AboutTeamDriveThemesItem (Maybe Text)
atdtiBackgRoundImageLink
  = lens _atdtiBackgRoundImageLink
      (\ s a -> s{_atdtiBackgRoundImageLink = a})

-- | The ID of the theme.
atdtiId :: Lens' AboutTeamDriveThemesItem (Maybe Text)
atdtiId = lens _atdtiId (\ s a -> s{_atdtiId = a})

instance FromJSON AboutTeamDriveThemesItem where
        parseJSON
          = withObject "AboutTeamDriveThemesItem"
              (\ o ->
                 AboutTeamDriveThemesItem' <$>
                   (o .:? "colorRgb") <*> (o .:? "backgroundImageLink")
                     <*> (o .:? "id"))

instance ToJSON AboutTeamDriveThemesItem where
        toJSON AboutTeamDriveThemesItem'{..}
          = object
              (catMaybes
                 [("colorRgb" .=) <$> _atdtiColorRgb,
                  ("backgroundImageLink" .=) <$>
                    _atdtiBackgRoundImageLink,
                  ("id" .=) <$> _atdtiId])

-- | An image file and cropping parameters from which a background image for
-- this Team Drive is set. This is a write only field; it can only be set
-- on drive.teamdrives.update requests that don\'t set themeId. When
-- specified, all fields of the backgroundImageFile must be set.
--
-- /See:/ 'teamDriveBackgRoundImageFile' smart constructor.
data TeamDriveBackgRoundImageFile = TeamDriveBackgRoundImageFile'
    { _tdbrifXCoordinate :: !(Maybe (Textual Double))
    , _tdbrifYCoordinate :: !(Maybe (Textual Double))
    , _tdbrifWidth :: !(Maybe (Textual Double))
    , _tdbrifId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamDriveBackgRoundImageFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdbrifXCoordinate'
--
-- * 'tdbrifYCoordinate'
--
-- * 'tdbrifWidth'
--
-- * 'tdbrifId'
teamDriveBackgRoundImageFile
    :: TeamDriveBackgRoundImageFile
teamDriveBackgRoundImageFile = 
    TeamDriveBackgRoundImageFile'
    { _tdbrifXCoordinate = Nothing
    , _tdbrifYCoordinate = Nothing
    , _tdbrifWidth = Nothing
    , _tdbrifId = Nothing
    }

-- | The X coordinate of the upper left corner of the cropping area in the
-- background image. This is a value in the closed range of 0 to 1. This
-- value represents the horizontal distance from the left side of the
-- entire image to the left side of the cropping area divided by the width
-- of the entire image.
tdbrifXCoordinate :: Lens' TeamDriveBackgRoundImageFile (Maybe Double)
tdbrifXCoordinate
  = lens _tdbrifXCoordinate
      (\ s a -> s{_tdbrifXCoordinate = a})
      . mapping _Coerce

-- | The Y coordinate of the upper left corner of the cropping area in the
-- background image. This is a value in the closed range of 0 to 1. This
-- value represents the vertical distance from the top side of the entire
-- image to the top side of the cropping area divided by the height of the
-- entire image.
tdbrifYCoordinate :: Lens' TeamDriveBackgRoundImageFile (Maybe Double)
tdbrifYCoordinate
  = lens _tdbrifYCoordinate
      (\ s a -> s{_tdbrifYCoordinate = a})
      . mapping _Coerce

-- | The width of the cropped image in the closed range of 0 to 1. This value
-- represents the width of the cropped image divided by the width of the
-- entire image. The height is computed by applying a width to height
-- aspect ratio of 80 to 9. The resulting image must be at least 1280
-- pixels wide and 144 pixels high.
tdbrifWidth :: Lens' TeamDriveBackgRoundImageFile (Maybe Double)
tdbrifWidth
  = lens _tdbrifWidth (\ s a -> s{_tdbrifWidth = a}) .
      mapping _Coerce

-- | The ID of an image file in Drive to use for the background image.
tdbrifId :: Lens' TeamDriveBackgRoundImageFile (Maybe Text)
tdbrifId = lens _tdbrifId (\ s a -> s{_tdbrifId = a})

instance FromJSON TeamDriveBackgRoundImageFile where
        parseJSON
          = withObject "TeamDriveBackgRoundImageFile"
              (\ o ->
                 TeamDriveBackgRoundImageFile' <$>
                   (o .:? "xCoordinate") <*> (o .:? "yCoordinate") <*>
                     (o .:? "width")
                     <*> (o .:? "id"))

instance ToJSON TeamDriveBackgRoundImageFile where
        toJSON TeamDriveBackgRoundImageFile'{..}
          = object
              (catMaybes
                 [("xCoordinate" .=) <$> _tdbrifXCoordinate,
                  ("yCoordinate" .=) <$> _tdbrifYCoordinate,
                  ("width" .=) <$> _tdbrifWidth,
                  ("id" .=) <$> _tdbrifId])

-- | Additional metadata about video media. This may not be available
-- immediately upon upload.
--
-- /See:/ 'fileVideoMediaMetadata' smart constructor.
data FileVideoMediaMetadata = FileVideoMediaMetadata'
    { _fvmmHeight :: !(Maybe (Textual Int32))
    , _fvmmWidth :: !(Maybe (Textual Int32))
    , _fvmmDurationMillis :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileVideoMediaMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fvmmHeight'
--
-- * 'fvmmWidth'
--
-- * 'fvmmDurationMillis'
fileVideoMediaMetadata
    :: FileVideoMediaMetadata
fileVideoMediaMetadata = 
    FileVideoMediaMetadata'
    { _fvmmHeight = Nothing
    , _fvmmWidth = Nothing
    , _fvmmDurationMillis = Nothing
    }

-- | The height of the video in pixels.
fvmmHeight :: Lens' FileVideoMediaMetadata (Maybe Int32)
fvmmHeight
  = lens _fvmmHeight (\ s a -> s{_fvmmHeight = a}) .
      mapping _Coerce

-- | The width of the video in pixels.
fvmmWidth :: Lens' FileVideoMediaMetadata (Maybe Int32)
fvmmWidth
  = lens _fvmmWidth (\ s a -> s{_fvmmWidth = a}) .
      mapping _Coerce

-- | The duration of the video in milliseconds.
fvmmDurationMillis :: Lens' FileVideoMediaMetadata (Maybe Int64)
fvmmDurationMillis
  = lens _fvmmDurationMillis
      (\ s a -> s{_fvmmDurationMillis = a})
      . mapping _Coerce

instance FromJSON FileVideoMediaMetadata where
        parseJSON
          = withObject "FileVideoMediaMetadata"
              (\ o ->
                 FileVideoMediaMetadata' <$>
                   (o .:? "height") <*> (o .:? "width") <*>
                     (o .:? "durationMillis"))

instance ToJSON FileVideoMediaMetadata where
        toJSON FileVideoMediaMetadata'{..}
          = object
              (catMaybes
                 [("height" .=) <$> _fvmmHeight,
                  ("width" .=) <$> _fvmmWidth,
                  ("durationMillis" .=) <$> _fvmmDurationMillis])

-- | A collection of arbitrary key-value pairs which are private to the
-- requesting app. Entries with null values are cleared in update and copy
-- requests.
--
-- /See:/ 'fileAppProperties' smart constructor.
newtype FileAppProperties = FileAppProperties'
    { _fapAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileAppProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fapAddtional'
fileAppProperties
    :: HashMap Text Text -- ^ 'fapAddtional'
    -> FileAppProperties
fileAppProperties pFapAddtional_ = 
    FileAppProperties'
    { _fapAddtional = _Coerce # pFapAddtional_
    }

fapAddtional :: Lens' FileAppProperties (HashMap Text Text)
fapAddtional
  = lens _fapAddtional (\ s a -> s{_fapAddtional = a})
      . _Coerce

instance FromJSON FileAppProperties where
        parseJSON
          = withObject "FileAppProperties"
              (\ o -> FileAppProperties' <$> (parseJSONObject o))

instance ToJSON FileAppProperties where
        toJSON = toJSON . _fapAddtional

-- | A change to a file or Team Drive.
--
-- /See:/ 'change' smart constructor.
data Change = Change'
    { _chaRemoved :: !(Maybe Bool)
    , _chaTime :: !(Maybe DateTime')
    , _chaKind :: !Text
    , _chaTeamDrive :: !(Maybe TeamDrive)
    , _chaTeamDriveId :: !(Maybe Text)
    , _chaType :: !(Maybe Text)
    , _chaFileId :: !(Maybe Text)
    , _chaFile :: !(Maybe File)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaRemoved'
--
-- * 'chaTime'
--
-- * 'chaKind'
--
-- * 'chaTeamDrive'
--
-- * 'chaTeamDriveId'
--
-- * 'chaType'
--
-- * 'chaFileId'
--
-- * 'chaFile'
change
    :: Change
change = 
    Change'
    { _chaRemoved = Nothing
    , _chaTime = Nothing
    , _chaKind = "drive#change"
    , _chaTeamDrive = Nothing
    , _chaTeamDriveId = Nothing
    , _chaType = Nothing
    , _chaFileId = Nothing
    , _chaFile = Nothing
    }

-- | Whether the file or Team Drive has been removed from this list of
-- changes, for example by deletion or loss of access.
chaRemoved :: Lens' Change (Maybe Bool)
chaRemoved
  = lens _chaRemoved (\ s a -> s{_chaRemoved = a})

-- | The time of this change (RFC 3339 date-time).
chaTime :: Lens' Change (Maybe UTCTime)
chaTime
  = lens _chaTime (\ s a -> s{_chaTime = a}) .
      mapping _DateTime

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#change\".
chaKind :: Lens' Change Text
chaKind = lens _chaKind (\ s a -> s{_chaKind = a})

-- | The updated state of the Team Drive. Present if the type is teamDrive,
-- the user is still a member of the Team Drive, and the Team Drive has not
-- been removed.
chaTeamDrive :: Lens' Change (Maybe TeamDrive)
chaTeamDrive
  = lens _chaTeamDrive (\ s a -> s{_chaTeamDrive = a})

-- | The ID of the Team Drive associated with this change.
chaTeamDriveId :: Lens' Change (Maybe Text)
chaTeamDriveId
  = lens _chaTeamDriveId
      (\ s a -> s{_chaTeamDriveId = a})

-- | The type of the change. Possible values are file and teamDrive.
chaType :: Lens' Change (Maybe Text)
chaType = lens _chaType (\ s a -> s{_chaType = a})

-- | The ID of the file which has changed.
chaFileId :: Lens' Change (Maybe Text)
chaFileId
  = lens _chaFileId (\ s a -> s{_chaFileId = a})

-- | The updated state of the file. Present if the type is file and the file
-- has not been removed from this list of changes.
chaFile :: Lens' Change (Maybe File)
chaFile = lens _chaFile (\ s a -> s{_chaFile = a})

instance FromJSON Change where
        parseJSON
          = withObject "Change"
              (\ o ->
                 Change' <$>
                   (o .:? "removed") <*> (o .:? "time") <*>
                     (o .:? "kind" .!= "drive#change")
                     <*> (o .:? "teamDrive")
                     <*> (o .:? "teamDriveId")
                     <*> (o .:? "type")
                     <*> (o .:? "fileId")
                     <*> (o .:? "file"))

instance ToJSON Change where
        toJSON Change'{..}
          = object
              (catMaybes
                 [("removed" .=) <$> _chaRemoved,
                  ("time" .=) <$> _chaTime, Just ("kind" .= _chaKind),
                  ("teamDrive" .=) <$> _chaTeamDrive,
                  ("teamDriveId" .=) <$> _chaTeamDriveId,
                  ("type" .=) <$> _chaType,
                  ("fileId" .=) <$> _chaFileId,
                  ("file" .=) <$> _chaFile])

-- | Representation of a Team Drive.
--
-- /See:/ 'teamDrive' smart constructor.
data TeamDrive = TeamDrive'
    { _tdThemeId :: !(Maybe Text)
    , _tdBackgRoundImageFile :: !(Maybe TeamDriveBackgRoundImageFile)
    , _tdColorRgb :: !(Maybe Text)
    , _tdCreatedTime :: !(Maybe DateTime')
    , _tdKind :: !Text
    , _tdBackgRoundImageLink :: !(Maybe Text)
    , _tdName :: !(Maybe Text)
    , _tdId :: !(Maybe Text)
    , _tdCapabilities :: !(Maybe TeamDriveCapabilities)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamDrive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdThemeId'
--
-- * 'tdBackgRoundImageFile'
--
-- * 'tdColorRgb'
--
-- * 'tdCreatedTime'
--
-- * 'tdKind'
--
-- * 'tdBackgRoundImageLink'
--
-- * 'tdName'
--
-- * 'tdId'
--
-- * 'tdCapabilities'
teamDrive
    :: TeamDrive
teamDrive = 
    TeamDrive'
    { _tdThemeId = Nothing
    , _tdBackgRoundImageFile = Nothing
    , _tdColorRgb = Nothing
    , _tdCreatedTime = Nothing
    , _tdKind = "drive#teamDrive"
    , _tdBackgRoundImageLink = Nothing
    , _tdName = Nothing
    , _tdId = Nothing
    , _tdCapabilities = Nothing
    }

-- | The ID of the theme from which the background image and color will be
-- set. The set of possible teamDriveThemes can be retrieved from a
-- drive.about.get response. When not specified on a
-- drive.teamdrives.create request, a random theme is chosen from which the
-- background image and color are set. This is a write-only field; it can
-- only be set on requests that don\'t set colorRgb or backgroundImageFile.
tdThemeId :: Lens' TeamDrive (Maybe Text)
tdThemeId
  = lens _tdThemeId (\ s a -> s{_tdThemeId = a})

-- | An image file and cropping parameters from which a background image for
-- this Team Drive is set. This is a write only field; it can only be set
-- on drive.teamdrives.update requests that don\'t set themeId. When
-- specified, all fields of the backgroundImageFile must be set.
tdBackgRoundImageFile :: Lens' TeamDrive (Maybe TeamDriveBackgRoundImageFile)
tdBackgRoundImageFile
  = lens _tdBackgRoundImageFile
      (\ s a -> s{_tdBackgRoundImageFile = a})

-- | The color of this Team Drive as an RGB hex string. It can only be set on
-- a drive.teamdrives.update request that does not set themeId.
tdColorRgb :: Lens' TeamDrive (Maybe Text)
tdColorRgb
  = lens _tdColorRgb (\ s a -> s{_tdColorRgb = a})

-- | The time at which the Team Drive was created (RFC 3339 date-time).
tdCreatedTime :: Lens' TeamDrive (Maybe UTCTime)
tdCreatedTime
  = lens _tdCreatedTime
      (\ s a -> s{_tdCreatedTime = a})
      . mapping _DateTime

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#teamDrive\".
tdKind :: Lens' TeamDrive Text
tdKind = lens _tdKind (\ s a -> s{_tdKind = a})

-- | A short-lived link to this Team Drive\'s background image.
tdBackgRoundImageLink :: Lens' TeamDrive (Maybe Text)
tdBackgRoundImageLink
  = lens _tdBackgRoundImageLink
      (\ s a -> s{_tdBackgRoundImageLink = a})

-- | The name of this Team Drive.
tdName :: Lens' TeamDrive (Maybe Text)
tdName = lens _tdName (\ s a -> s{_tdName = a})

-- | The ID of this Team Drive which is also the ID of the top level folder
-- for this Team Drive.
tdId :: Lens' TeamDrive (Maybe Text)
tdId = lens _tdId (\ s a -> s{_tdId = a})

-- | Capabilities the current user has on this Team Drive.
tdCapabilities :: Lens' TeamDrive (Maybe TeamDriveCapabilities)
tdCapabilities
  = lens _tdCapabilities
      (\ s a -> s{_tdCapabilities = a})

instance FromJSON TeamDrive where
        parseJSON
          = withObject "TeamDrive"
              (\ o ->
                 TeamDrive' <$>
                   (o .:? "themeId") <*> (o .:? "backgroundImageFile")
                     <*> (o .:? "colorRgb")
                     <*> (o .:? "createdTime")
                     <*> (o .:? "kind" .!= "drive#teamDrive")
                     <*> (o .:? "backgroundImageLink")
                     <*> (o .:? "name")
                     <*> (o .:? "id")
                     <*> (o .:? "capabilities"))

instance ToJSON TeamDrive where
        toJSON TeamDrive'{..}
          = object
              (catMaybes
                 [("themeId" .=) <$> _tdThemeId,
                  ("backgroundImageFile" .=) <$>
                    _tdBackgRoundImageFile,
                  ("colorRgb" .=) <$> _tdColorRgb,
                  ("createdTime" .=) <$> _tdCreatedTime,
                  Just ("kind" .= _tdKind),
                  ("backgroundImageLink" .=) <$>
                    _tdBackgRoundImageLink,
                  ("name" .=) <$> _tdName, ("id" .=) <$> _tdId,
                  ("capabilities" .=) <$> _tdCapabilities])

-- | A map of source MIME type to possible targets for all supported exports.
--
-- /See:/ 'aboutExportFormats' smart constructor.
newtype AboutExportFormats = AboutExportFormats'
    { _aefAddtional :: HashMap Text [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AboutExportFormats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aefAddtional'
aboutExportFormats
    :: HashMap Text [Text] -- ^ 'aefAddtional'
    -> AboutExportFormats
aboutExportFormats pAefAddtional_ = 
    AboutExportFormats'
    { _aefAddtional = _Coerce # pAefAddtional_
    }

aefAddtional :: Lens' AboutExportFormats (HashMap Text [Text])
aefAddtional
  = lens _aefAddtional (\ s a -> s{_aefAddtional = a})
      . _Coerce

instance FromJSON AboutExportFormats where
        parseJSON
          = withObject "AboutExportFormats"
              (\ o -> AboutExportFormats' <$> (parseJSONObject o))

instance ToJSON AboutExportFormats where
        toJSON = toJSON . _aefAddtional

-- | Information about a Drive user.
--
-- /See:/ 'user' smart constructor.
data User = User'
    { _uPhotoLink :: !(Maybe Text)
    , _uMe :: !(Maybe Bool)
    , _uKind :: !Text
    , _uEmailAddress :: !(Maybe Text)
    , _uDisplayName :: !(Maybe Text)
    , _uPermissionId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uPhotoLink'
--
-- * 'uMe'
--
-- * 'uKind'
--
-- * 'uEmailAddress'
--
-- * 'uDisplayName'
--
-- * 'uPermissionId'
user
    :: User
user = 
    User'
    { _uPhotoLink = Nothing
    , _uMe = Nothing
    , _uKind = "drive#user"
    , _uEmailAddress = Nothing
    , _uDisplayName = Nothing
    , _uPermissionId = Nothing
    }

-- | A link to the user\'s profile photo, if available.
uPhotoLink :: Lens' User (Maybe Text)
uPhotoLink
  = lens _uPhotoLink (\ s a -> s{_uPhotoLink = a})

-- | Whether this user is the requesting user.
uMe :: Lens' User (Maybe Bool)
uMe = lens _uMe (\ s a -> s{_uMe = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#user\".
uKind :: Lens' User Text
uKind = lens _uKind (\ s a -> s{_uKind = a})

-- | The email address of the user. This may not be present in certain
-- contexts if the user has not made their email address visible to the
-- requester.
uEmailAddress :: Lens' User (Maybe Text)
uEmailAddress
  = lens _uEmailAddress
      (\ s a -> s{_uEmailAddress = a})

-- | A plain text displayable name for this user.
uDisplayName :: Lens' User (Maybe Text)
uDisplayName
  = lens _uDisplayName (\ s a -> s{_uDisplayName = a})

-- | The user\'s ID as visible in Permission resources.
uPermissionId :: Lens' User (Maybe Text)
uPermissionId
  = lens _uPermissionId
      (\ s a -> s{_uPermissionId = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ o ->
                 User' <$>
                   (o .:? "photoLink") <*> (o .:? "me") <*>
                     (o .:? "kind" .!= "drive#user")
                     <*> (o .:? "emailAddress")
                     <*> (o .:? "displayName")
                     <*> (o .:? "permissionId"))

instance ToJSON User where
        toJSON User'{..}
          = object
              (catMaybes
                 [("photoLink" .=) <$> _uPhotoLink,
                  ("me" .=) <$> _uMe, Just ("kind" .= _uKind),
                  ("emailAddress" .=) <$> _uEmailAddress,
                  ("displayName" .=) <$> _uDisplayName,
                  ("permissionId" .=) <$> _uPermissionId])

-- | A list of changes for a user.
--
-- /See:/ 'changeList' smart constructor.
data ChangeList = ChangeList'
    { _clNewStartPageToken :: !(Maybe Text)
    , _clNextPageToken :: !(Maybe Text)
    , _clChanges :: !(Maybe [Change])
    , _clKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clNewStartPageToken'
--
-- * 'clNextPageToken'
--
-- * 'clChanges'
--
-- * 'clKind'
changeList
    :: ChangeList
changeList = 
    ChangeList'
    { _clNewStartPageToken = Nothing
    , _clNextPageToken = Nothing
    , _clChanges = Nothing
    , _clKind = "drive#changeList"
    }

-- | The starting page token for future changes. This will be present only if
-- the end of the current changes list has been reached.
clNewStartPageToken :: Lens' ChangeList (Maybe Text)
clNewStartPageToken
  = lens _clNewStartPageToken
      (\ s a -> s{_clNewStartPageToken = a})

-- | The page token for the next page of changes. This will be absent if the
-- end of the changes list has been reached. If the token is rejected for
-- any reason, it should be discarded, and pagination should be restarted
-- from the first page of results.
clNextPageToken :: Lens' ChangeList (Maybe Text)
clNextPageToken
  = lens _clNextPageToken
      (\ s a -> s{_clNextPageToken = a})

-- | The list of changes. If nextPageToken is populated, then this list may
-- be incomplete and an additional page of results should be fetched.
clChanges :: Lens' ChangeList [Change]
clChanges
  = lens _clChanges (\ s a -> s{_clChanges = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#changeList\".
clKind :: Lens' ChangeList Text
clKind = lens _clKind (\ s a -> s{_clKind = a})

instance FromJSON ChangeList where
        parseJSON
          = withObject "ChangeList"
              (\ o ->
                 ChangeList' <$>
                   (o .:? "newStartPageToken") <*>
                     (o .:? "nextPageToken")
                     <*> (o .:? "changes" .!= mempty)
                     <*> (o .:? "kind" .!= "drive#changeList"))

instance ToJSON ChangeList where
        toJSON ChangeList'{..}
          = object
              (catMaybes
                 [("newStartPageToken" .=) <$> _clNewStartPageToken,
                  ("nextPageToken" .=) <$> _clNextPageToken,
                  ("changes" .=) <$> _clChanges,
                  Just ("kind" .= _clKind)])

-- | Additional information about the content of the file. These fields are
-- never populated in responses.
--
-- /See:/ 'fileContentHints' smart constructor.
data FileContentHints = FileContentHints'
    { _fchThumbnail :: !(Maybe FileContentHintsThumbnail)
    , _fchIndexableText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileContentHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fchThumbnail'
--
-- * 'fchIndexableText'
fileContentHints
    :: FileContentHints
fileContentHints = 
    FileContentHints'
    { _fchThumbnail = Nothing
    , _fchIndexableText = Nothing
    }

-- | A thumbnail for the file. This will only be used if Drive cannot
-- generate a standard thumbnail.
fchThumbnail :: Lens' FileContentHints (Maybe FileContentHintsThumbnail)
fchThumbnail
  = lens _fchThumbnail (\ s a -> s{_fchThumbnail = a})

-- | Text to be indexed for the file to improve fullText queries. This is
-- limited to 128KB in length and may contain HTML elements.
fchIndexableText :: Lens' FileContentHints (Maybe Text)
fchIndexableText
  = lens _fchIndexableText
      (\ s a -> s{_fchIndexableText = a})

instance FromJSON FileContentHints where
        parseJSON
          = withObject "FileContentHints"
              (\ o ->
                 FileContentHints' <$>
                   (o .:? "thumbnail") <*> (o .:? "indexableText"))

instance ToJSON FileContentHints where
        toJSON FileContentHints'{..}
          = object
              (catMaybes
                 [("thumbnail" .=) <$> _fchThumbnail,
                  ("indexableText" .=) <$> _fchIndexableText])

-- | Additional parameters controlling delivery channel behavior. Optional.
--
-- /See:/ 'channelParams' smart constructor.
newtype ChannelParams = ChannelParams'
    { _cpAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChannelParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpAddtional'
channelParams
    :: HashMap Text Text -- ^ 'cpAddtional'
    -> ChannelParams
channelParams pCpAddtional_ = 
    ChannelParams'
    { _cpAddtional = _Coerce # pCpAddtional_
    }

-- | Declares a new parameter by name.
cpAddtional :: Lens' ChannelParams (HashMap Text Text)
cpAddtional
  = lens _cpAddtional (\ s a -> s{_cpAddtional = a}) .
      _Coerce

instance FromJSON ChannelParams where
        parseJSON
          = withObject "ChannelParams"
              (\ o -> ChannelParams' <$> (parseJSONObject o))

instance ToJSON ChannelParams where
        toJSON = toJSON . _cpAddtional

-- | A collection of arbitrary key-value pairs which are visible to all apps.
-- Entries with null values are cleared in update and copy requests.
--
-- /See:/ 'fileProperties' smart constructor.
newtype FileProperties = FileProperties'
    { _fpAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fpAddtional'
fileProperties
    :: HashMap Text Text -- ^ 'fpAddtional'
    -> FileProperties
fileProperties pFpAddtional_ = 
    FileProperties'
    { _fpAddtional = _Coerce # pFpAddtional_
    }

fpAddtional :: Lens' FileProperties (HashMap Text Text)
fpAddtional
  = lens _fpAddtional (\ s a -> s{_fpAddtional = a}) .
      _Coerce

instance FromJSON FileProperties where
        parseJSON
          = withObject "FileProperties"
              (\ o -> FileProperties' <$> (parseJSONObject o))

instance ToJSON FileProperties where
        toJSON = toJSON . _fpAddtional

-- | A map of maximum import sizes by MIME type, in bytes.
--
-- /See:/ 'aboutMaxImportSizes' smart constructor.
newtype AboutMaxImportSizes = AboutMaxImportSizes'
    { _amisAddtional :: HashMap Text (Textual Int64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AboutMaxImportSizes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amisAddtional'
aboutMaxImportSizes
    :: HashMap Text Int64 -- ^ 'amisAddtional'
    -> AboutMaxImportSizes
aboutMaxImportSizes pAmisAddtional_ = 
    AboutMaxImportSizes'
    { _amisAddtional = _Coerce # pAmisAddtional_
    }

amisAddtional :: Lens' AboutMaxImportSizes (HashMap Text Int64)
amisAddtional
  = lens _amisAddtional
      (\ s a -> s{_amisAddtional = a})
      . _Coerce

instance FromJSON AboutMaxImportSizes where
        parseJSON
          = withObject "AboutMaxImportSizes"
              (\ o -> AboutMaxImportSizes' <$> (parseJSONObject o))

instance ToJSON AboutMaxImportSizes where
        toJSON = toJSON . _amisAddtional

-- | Information about the user, the user\'s Drive, and system capabilities.
--
-- /See:/ 'about' smart constructor.
data About = About'
    { _aExportFormats :: !(Maybe AboutExportFormats)
    , _aMaxImportSizes :: !(Maybe AboutMaxImportSizes)
    , _aCanCreateTeamDrives :: !(Maybe Bool)
    , _aImportFormats :: !(Maybe AboutImportFormats)
    , _aKind :: !Text
    , _aAppInstalled :: !(Maybe Bool)
    , _aUser :: !(Maybe User)
    , _aStorageQuota :: !(Maybe AboutStorageQuota)
    , _aMaxUploadSize :: !(Maybe (Textual Int64))
    , _aTeamDriveThemes :: !(Maybe [AboutTeamDriveThemesItem])
    , _aFolderColorPalette :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'About' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aExportFormats'
--
-- * 'aMaxImportSizes'
--
-- * 'aCanCreateTeamDrives'
--
-- * 'aImportFormats'
--
-- * 'aKind'
--
-- * 'aAppInstalled'
--
-- * 'aUser'
--
-- * 'aStorageQuota'
--
-- * 'aMaxUploadSize'
--
-- * 'aTeamDriveThemes'
--
-- * 'aFolderColorPalette'
about
    :: About
about = 
    About'
    { _aExportFormats = Nothing
    , _aMaxImportSizes = Nothing
    , _aCanCreateTeamDrives = Nothing
    , _aImportFormats = Nothing
    , _aKind = "drive#about"
    , _aAppInstalled = Nothing
    , _aUser = Nothing
    , _aStorageQuota = Nothing
    , _aMaxUploadSize = Nothing
    , _aTeamDriveThemes = Nothing
    , _aFolderColorPalette = Nothing
    }

-- | A map of source MIME type to possible targets for all supported exports.
aExportFormats :: Lens' About (Maybe AboutExportFormats)
aExportFormats
  = lens _aExportFormats
      (\ s a -> s{_aExportFormats = a})

-- | A map of maximum import sizes by MIME type, in bytes.
aMaxImportSizes :: Lens' About (Maybe AboutMaxImportSizes)
aMaxImportSizes
  = lens _aMaxImportSizes
      (\ s a -> s{_aMaxImportSizes = a})

-- | Whether the user can create Team Drives.
aCanCreateTeamDrives :: Lens' About (Maybe Bool)
aCanCreateTeamDrives
  = lens _aCanCreateTeamDrives
      (\ s a -> s{_aCanCreateTeamDrives = a})

-- | A map of source MIME type to possible targets for all supported imports.
aImportFormats :: Lens' About (Maybe AboutImportFormats)
aImportFormats
  = lens _aImportFormats
      (\ s a -> s{_aImportFormats = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#about\".
aKind :: Lens' About Text
aKind = lens _aKind (\ s a -> s{_aKind = a})

-- | Whether the user has installed the requesting app.
aAppInstalled :: Lens' About (Maybe Bool)
aAppInstalled
  = lens _aAppInstalled
      (\ s a -> s{_aAppInstalled = a})

-- | The authenticated user.
aUser :: Lens' About (Maybe User)
aUser = lens _aUser (\ s a -> s{_aUser = a})

-- | The user\'s storage quota limits and usage. All fields are measured in
-- bytes.
aStorageQuota :: Lens' About (Maybe AboutStorageQuota)
aStorageQuota
  = lens _aStorageQuota
      (\ s a -> s{_aStorageQuota = a})

-- | The maximum upload size in bytes.
aMaxUploadSize :: Lens' About (Maybe Int64)
aMaxUploadSize
  = lens _aMaxUploadSize
      (\ s a -> s{_aMaxUploadSize = a})
      . mapping _Coerce

-- | A list of themes that are supported for Team Drives.
aTeamDriveThemes :: Lens' About [AboutTeamDriveThemesItem]
aTeamDriveThemes
  = lens _aTeamDriveThemes
      (\ s a -> s{_aTeamDriveThemes = a})
      . _Default
      . _Coerce

-- | The currently supported folder colors as RGB hex strings.
aFolderColorPalette :: Lens' About [Text]
aFolderColorPalette
  = lens _aFolderColorPalette
      (\ s a -> s{_aFolderColorPalette = a})
      . _Default
      . _Coerce

instance FromJSON About where
        parseJSON
          = withObject "About"
              (\ o ->
                 About' <$>
                   (o .:? "exportFormats") <*> (o .:? "maxImportSizes")
                     <*> (o .:? "canCreateTeamDrives")
                     <*> (o .:? "importFormats")
                     <*> (o .:? "kind" .!= "drive#about")
                     <*> (o .:? "appInstalled")
                     <*> (o .:? "user")
                     <*> (o .:? "storageQuota")
                     <*> (o .:? "maxUploadSize")
                     <*> (o .:? "teamDriveThemes" .!= mempty)
                     <*> (o .:? "folderColorPalette" .!= mempty))

instance ToJSON About where
        toJSON About'{..}
          = object
              (catMaybes
                 [("exportFormats" .=) <$> _aExportFormats,
                  ("maxImportSizes" .=) <$> _aMaxImportSizes,
                  ("canCreateTeamDrives" .=) <$> _aCanCreateTeamDrives,
                  ("importFormats" .=) <$> _aImportFormats,
                  Just ("kind" .= _aKind),
                  ("appInstalled" .=) <$> _aAppInstalled,
                  ("user" .=) <$> _aUser,
                  ("storageQuota" .=) <$> _aStorageQuota,
                  ("maxUploadSize" .=) <$> _aMaxUploadSize,
                  ("teamDriveThemes" .=) <$> _aTeamDriveThemes,
                  ("folderColorPalette" .=) <$> _aFolderColorPalette])

-- | Geographic location information stored in the image.
--
-- /See:/ 'fileImageMediaMetadataLocation' smart constructor.
data FileImageMediaMetadataLocation = FileImageMediaMetadataLocation'
    { _fimmlLatitude :: !(Maybe (Textual Double))
    , _fimmlAltitude :: !(Maybe (Textual Double))
    , _fimmlLongitude :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileImageMediaMetadataLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fimmlLatitude'
--
-- * 'fimmlAltitude'
--
-- * 'fimmlLongitude'
fileImageMediaMetadataLocation
    :: FileImageMediaMetadataLocation
fileImageMediaMetadataLocation = 
    FileImageMediaMetadataLocation'
    { _fimmlLatitude = Nothing
    , _fimmlAltitude = Nothing
    , _fimmlLongitude = Nothing
    }

-- | The latitude stored in the image.
fimmlLatitude :: Lens' FileImageMediaMetadataLocation (Maybe Double)
fimmlLatitude
  = lens _fimmlLatitude
      (\ s a -> s{_fimmlLatitude = a})
      . mapping _Coerce

-- | The altitude stored in the image.
fimmlAltitude :: Lens' FileImageMediaMetadataLocation (Maybe Double)
fimmlAltitude
  = lens _fimmlAltitude
      (\ s a -> s{_fimmlAltitude = a})
      . mapping _Coerce

-- | The longitude stored in the image.
fimmlLongitude :: Lens' FileImageMediaMetadataLocation (Maybe Double)
fimmlLongitude
  = lens _fimmlLongitude
      (\ s a -> s{_fimmlLongitude = a})
      . mapping _Coerce

instance FromJSON FileImageMediaMetadataLocation
         where
        parseJSON
          = withObject "FileImageMediaMetadataLocation"
              (\ o ->
                 FileImageMediaMetadataLocation' <$>
                   (o .:? "latitude") <*> (o .:? "altitude") <*>
                     (o .:? "longitude"))

instance ToJSON FileImageMediaMetadataLocation where
        toJSON FileImageMediaMetadataLocation'{..}
          = object
              (catMaybes
                 [("latitude" .=) <$> _fimmlLatitude,
                  ("altitude" .=) <$> _fimmlAltitude,
                  ("longitude" .=) <$> _fimmlLongitude])

--
-- /See:/ 'startPageToken' smart constructor.
data StartPageToken = StartPageToken'
    { _sptKind :: !Text
    , _sptStartPageToken :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartPageToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sptKind'
--
-- * 'sptStartPageToken'
startPageToken
    :: StartPageToken
startPageToken = 
    StartPageToken'
    { _sptKind = "drive#startPageToken"
    , _sptStartPageToken = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#startPageToken\".
sptKind :: Lens' StartPageToken Text
sptKind = lens _sptKind (\ s a -> s{_sptKind = a})

-- | The starting page token for listing changes.
sptStartPageToken :: Lens' StartPageToken (Maybe Text)
sptStartPageToken
  = lens _sptStartPageToken
      (\ s a -> s{_sptStartPageToken = a})

instance FromJSON StartPageToken where
        parseJSON
          = withObject "StartPageToken"
              (\ o ->
                 StartPageToken' <$>
                   (o .:? "kind" .!= "drive#startPageToken") <*>
                     (o .:? "startPageToken"))

instance ToJSON StartPageToken where
        toJSON StartPageToken'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _sptKind),
                  ("startPageToken" .=) <$> _sptStartPageToken])

-- | Additional metadata about image media, if available.
--
-- /See:/ 'fileImageMediaMetadata' smart constructor.
data FileImageMediaMetadata = FileImageMediaMetadata'
    { _fimmRotation :: !(Maybe (Textual Int32))
    , _fimmHeight :: !(Maybe (Textual Int32))
    , _fimmSubjectDistance :: !(Maybe (Textual Int32))
    , _fimmMaxApertureValue :: !(Maybe (Textual Double))
    , _fimmIsoSpeed :: !(Maybe (Textual Int32))
    , _fimmTime :: !(Maybe Text)
    , _fimmLocation :: !(Maybe FileImageMediaMetadataLocation)
    , _fimmAperture :: !(Maybe (Textual Double))
    , _fimmFocalLength :: !(Maybe (Textual Double))
    , _fimmCameraMake :: !(Maybe Text)
    , _fimmWidth :: !(Maybe (Textual Int32))
    , _fimmExposureTime :: !(Maybe (Textual Double))
    , _fimmCameraModel :: !(Maybe Text)
    , _fimmWhiteBalance :: !(Maybe Text)
    , _fimmLens :: !(Maybe Text)
    , _fimmFlashUsed :: !(Maybe Bool)
    , _fimmExposureBias :: !(Maybe (Textual Double))
    , _fimmMeteringMode :: !(Maybe Text)
    , _fimmExposureMode :: !(Maybe Text)
    , _fimmSensor :: !(Maybe Text)
    , _fimmColorSpace :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileImageMediaMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fimmRotation'
--
-- * 'fimmHeight'
--
-- * 'fimmSubjectDistance'
--
-- * 'fimmMaxApertureValue'
--
-- * 'fimmIsoSpeed'
--
-- * 'fimmTime'
--
-- * 'fimmLocation'
--
-- * 'fimmAperture'
--
-- * 'fimmFocalLength'
--
-- * 'fimmCameraMake'
--
-- * 'fimmWidth'
--
-- * 'fimmExposureTime'
--
-- * 'fimmCameraModel'
--
-- * 'fimmWhiteBalance'
--
-- * 'fimmLens'
--
-- * 'fimmFlashUsed'
--
-- * 'fimmExposureBias'
--
-- * 'fimmMeteringMode'
--
-- * 'fimmExposureMode'
--
-- * 'fimmSensor'
--
-- * 'fimmColorSpace'
fileImageMediaMetadata
    :: FileImageMediaMetadata
fileImageMediaMetadata = 
    FileImageMediaMetadata'
    { _fimmRotation = Nothing
    , _fimmHeight = Nothing
    , _fimmSubjectDistance = Nothing
    , _fimmMaxApertureValue = Nothing
    , _fimmIsoSpeed = Nothing
    , _fimmTime = Nothing
    , _fimmLocation = Nothing
    , _fimmAperture = Nothing
    , _fimmFocalLength = Nothing
    , _fimmCameraMake = Nothing
    , _fimmWidth = Nothing
    , _fimmExposureTime = Nothing
    , _fimmCameraModel = Nothing
    , _fimmWhiteBalance = Nothing
    , _fimmLens = Nothing
    , _fimmFlashUsed = Nothing
    , _fimmExposureBias = Nothing
    , _fimmMeteringMode = Nothing
    , _fimmExposureMode = Nothing
    , _fimmSensor = Nothing
    , _fimmColorSpace = Nothing
    }

-- | The rotation in clockwise degrees from the image\'s original
-- orientation.
fimmRotation :: Lens' FileImageMediaMetadata (Maybe Int32)
fimmRotation
  = lens _fimmRotation (\ s a -> s{_fimmRotation = a})
      . mapping _Coerce

-- | The height of the image in pixels.
fimmHeight :: Lens' FileImageMediaMetadata (Maybe Int32)
fimmHeight
  = lens _fimmHeight (\ s a -> s{_fimmHeight = a}) .
      mapping _Coerce

-- | The distance to the subject of the photo, in meters.
fimmSubjectDistance :: Lens' FileImageMediaMetadata (Maybe Int32)
fimmSubjectDistance
  = lens _fimmSubjectDistance
      (\ s a -> s{_fimmSubjectDistance = a})
      . mapping _Coerce

-- | The smallest f-number of the lens at the focal length used to create the
-- photo (APEX value).
fimmMaxApertureValue :: Lens' FileImageMediaMetadata (Maybe Double)
fimmMaxApertureValue
  = lens _fimmMaxApertureValue
      (\ s a -> s{_fimmMaxApertureValue = a})
      . mapping _Coerce

-- | The ISO speed used to create the photo.
fimmIsoSpeed :: Lens' FileImageMediaMetadata (Maybe Int32)
fimmIsoSpeed
  = lens _fimmIsoSpeed (\ s a -> s{_fimmIsoSpeed = a})
      . mapping _Coerce

-- | The date and time the photo was taken (EXIF DateTime).
fimmTime :: Lens' FileImageMediaMetadata (Maybe Text)
fimmTime = lens _fimmTime (\ s a -> s{_fimmTime = a})

-- | Geographic location information stored in the image.
fimmLocation :: Lens' FileImageMediaMetadata (Maybe FileImageMediaMetadataLocation)
fimmLocation
  = lens _fimmLocation (\ s a -> s{_fimmLocation = a})

-- | The aperture used to create the photo (f-number).
fimmAperture :: Lens' FileImageMediaMetadata (Maybe Double)
fimmAperture
  = lens _fimmAperture (\ s a -> s{_fimmAperture = a})
      . mapping _Coerce

-- | The focal length used to create the photo, in millimeters.
fimmFocalLength :: Lens' FileImageMediaMetadata (Maybe Double)
fimmFocalLength
  = lens _fimmFocalLength
      (\ s a -> s{_fimmFocalLength = a})
      . mapping _Coerce

-- | The make of the camera used to create the photo.
fimmCameraMake :: Lens' FileImageMediaMetadata (Maybe Text)
fimmCameraMake
  = lens _fimmCameraMake
      (\ s a -> s{_fimmCameraMake = a})

-- | The width of the image in pixels.
fimmWidth :: Lens' FileImageMediaMetadata (Maybe Int32)
fimmWidth
  = lens _fimmWidth (\ s a -> s{_fimmWidth = a}) .
      mapping _Coerce

-- | The length of the exposure, in seconds.
fimmExposureTime :: Lens' FileImageMediaMetadata (Maybe Double)
fimmExposureTime
  = lens _fimmExposureTime
      (\ s a -> s{_fimmExposureTime = a})
      . mapping _Coerce

-- | The model of the camera used to create the photo.
fimmCameraModel :: Lens' FileImageMediaMetadata (Maybe Text)
fimmCameraModel
  = lens _fimmCameraModel
      (\ s a -> s{_fimmCameraModel = a})

-- | The white balance mode used to create the photo.
fimmWhiteBalance :: Lens' FileImageMediaMetadata (Maybe Text)
fimmWhiteBalance
  = lens _fimmWhiteBalance
      (\ s a -> s{_fimmWhiteBalance = a})

-- | The lens used to create the photo.
fimmLens :: Lens' FileImageMediaMetadata (Maybe Text)
fimmLens = lens _fimmLens (\ s a -> s{_fimmLens = a})

-- | Whether a flash was used to create the photo.
fimmFlashUsed :: Lens' FileImageMediaMetadata (Maybe Bool)
fimmFlashUsed
  = lens _fimmFlashUsed
      (\ s a -> s{_fimmFlashUsed = a})

-- | The exposure bias of the photo (APEX value).
fimmExposureBias :: Lens' FileImageMediaMetadata (Maybe Double)
fimmExposureBias
  = lens _fimmExposureBias
      (\ s a -> s{_fimmExposureBias = a})
      . mapping _Coerce

-- | The metering mode used to create the photo.
fimmMeteringMode :: Lens' FileImageMediaMetadata (Maybe Text)
fimmMeteringMode
  = lens _fimmMeteringMode
      (\ s a -> s{_fimmMeteringMode = a})

-- | The exposure mode used to create the photo.
fimmExposureMode :: Lens' FileImageMediaMetadata (Maybe Text)
fimmExposureMode
  = lens _fimmExposureMode
      (\ s a -> s{_fimmExposureMode = a})

-- | The type of sensor used to create the photo.
fimmSensor :: Lens' FileImageMediaMetadata (Maybe Text)
fimmSensor
  = lens _fimmSensor (\ s a -> s{_fimmSensor = a})

-- | The color space of the photo.
fimmColorSpace :: Lens' FileImageMediaMetadata (Maybe Text)
fimmColorSpace
  = lens _fimmColorSpace
      (\ s a -> s{_fimmColorSpace = a})

instance FromJSON FileImageMediaMetadata where
        parseJSON
          = withObject "FileImageMediaMetadata"
              (\ o ->
                 FileImageMediaMetadata' <$>
                   (o .:? "rotation") <*> (o .:? "height") <*>
                     (o .:? "subjectDistance")
                     <*> (o .:? "maxApertureValue")
                     <*> (o .:? "isoSpeed")
                     <*> (o .:? "time")
                     <*> (o .:? "location")
                     <*> (o .:? "aperture")
                     <*> (o .:? "focalLength")
                     <*> (o .:? "cameraMake")
                     <*> (o .:? "width")
                     <*> (o .:? "exposureTime")
                     <*> (o .:? "cameraModel")
                     <*> (o .:? "whiteBalance")
                     <*> (o .:? "lens")
                     <*> (o .:? "flashUsed")
                     <*> (o .:? "exposureBias")
                     <*> (o .:? "meteringMode")
                     <*> (o .:? "exposureMode")
                     <*> (o .:? "sensor")
                     <*> (o .:? "colorSpace"))

instance ToJSON FileImageMediaMetadata where
        toJSON FileImageMediaMetadata'{..}
          = object
              (catMaybes
                 [("rotation" .=) <$> _fimmRotation,
                  ("height" .=) <$> _fimmHeight,
                  ("subjectDistance" .=) <$> _fimmSubjectDistance,
                  ("maxApertureValue" .=) <$> _fimmMaxApertureValue,
                  ("isoSpeed" .=) <$> _fimmIsoSpeed,
                  ("time" .=) <$> _fimmTime,
                  ("location" .=) <$> _fimmLocation,
                  ("aperture" .=) <$> _fimmAperture,
                  ("focalLength" .=) <$> _fimmFocalLength,
                  ("cameraMake" .=) <$> _fimmCameraMake,
                  ("width" .=) <$> _fimmWidth,
                  ("exposureTime" .=) <$> _fimmExposureTime,
                  ("cameraModel" .=) <$> _fimmCameraModel,
                  ("whiteBalance" .=) <$> _fimmWhiteBalance,
                  ("lens" .=) <$> _fimmLens,
                  ("flashUsed" .=) <$> _fimmFlashUsed,
                  ("exposureBias" .=) <$> _fimmExposureBias,
                  ("meteringMode" .=) <$> _fimmMeteringMode,
                  ("exposureMode" .=) <$> _fimmExposureMode,
                  ("sensor" .=) <$> _fimmSensor,
                  ("colorSpace" .=) <$> _fimmColorSpace])

-- | A comment on a file.
--
-- /See:/ 'comment' smart constructor.
data Comment = Comment'
    { _comHTMLContent :: !(Maybe Text)
    , _comModifiedTime :: !(Maybe DateTime')
    , _comCreatedTime :: !(Maybe DateTime')
    , _comKind :: !Text
    , _comResolved :: !(Maybe Bool)
    , _comQuotedFileContent :: !(Maybe CommentQuotedFileContent)
    , _comAnchor :: !(Maybe Text)
    , _comContent :: !(Maybe Text)
    , _comReplies :: !(Maybe [Reply])
    , _comAuthor :: !(Maybe User)
    , _comId :: !(Maybe Text)
    , _comDeleted :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'comHTMLContent'
--
-- * 'comModifiedTime'
--
-- * 'comCreatedTime'
--
-- * 'comKind'
--
-- * 'comResolved'
--
-- * 'comQuotedFileContent'
--
-- * 'comAnchor'
--
-- * 'comContent'
--
-- * 'comReplies'
--
-- * 'comAuthor'
--
-- * 'comId'
--
-- * 'comDeleted'
comment
    :: Comment
comment = 
    Comment'
    { _comHTMLContent = Nothing
    , _comModifiedTime = Nothing
    , _comCreatedTime = Nothing
    , _comKind = "drive#comment"
    , _comResolved = Nothing
    , _comQuotedFileContent = Nothing
    , _comAnchor = Nothing
    , _comContent = Nothing
    , _comReplies = Nothing
    , _comAuthor = Nothing
    , _comId = Nothing
    , _comDeleted = Nothing
    }

-- | The content of the comment with HTML formatting.
comHTMLContent :: Lens' Comment (Maybe Text)
comHTMLContent
  = lens _comHTMLContent
      (\ s a -> s{_comHTMLContent = a})

-- | The last time the comment or any of its replies was modified (RFC 3339
-- date-time).
comModifiedTime :: Lens' Comment (Maybe UTCTime)
comModifiedTime
  = lens _comModifiedTime
      (\ s a -> s{_comModifiedTime = a})
      . mapping _DateTime

-- | The time at which the comment was created (RFC 3339 date-time).
comCreatedTime :: Lens' Comment (Maybe UTCTime)
comCreatedTime
  = lens _comCreatedTime
      (\ s a -> s{_comCreatedTime = a})
      . mapping _DateTime

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#comment\".
comKind :: Lens' Comment Text
comKind = lens _comKind (\ s a -> s{_comKind = a})

-- | Whether the comment has been resolved by one of its replies.
comResolved :: Lens' Comment (Maybe Bool)
comResolved
  = lens _comResolved (\ s a -> s{_comResolved = a})

-- | The file content to which the comment refers, typically within the
-- anchor region. For a text file, for example, this would be the text at
-- the location of the comment.
comQuotedFileContent :: Lens' Comment (Maybe CommentQuotedFileContent)
comQuotedFileContent
  = lens _comQuotedFileContent
      (\ s a -> s{_comQuotedFileContent = a})

-- | A region of the document represented as a JSON string. See anchor
-- documentation for details on how to define and interpret anchor
-- properties.
comAnchor :: Lens' Comment (Maybe Text)
comAnchor
  = lens _comAnchor (\ s a -> s{_comAnchor = a})

-- | The plain text content of the comment. This field is used for setting
-- the content, while htmlContent should be displayed.
comContent :: Lens' Comment (Maybe Text)
comContent
  = lens _comContent (\ s a -> s{_comContent = a})

-- | The full list of replies to the comment in chronological order.
comReplies :: Lens' Comment [Reply]
comReplies
  = lens _comReplies (\ s a -> s{_comReplies = a}) .
      _Default
      . _Coerce

-- | The user who created the comment.
comAuthor :: Lens' Comment (Maybe User)
comAuthor
  = lens _comAuthor (\ s a -> s{_comAuthor = a})

-- | The ID of the comment.
comId :: Lens' Comment (Maybe Text)
comId = lens _comId (\ s a -> s{_comId = a})

-- | Whether the comment has been deleted. A deleted comment has no content.
comDeleted :: Lens' Comment (Maybe Bool)
comDeleted
  = lens _comDeleted (\ s a -> s{_comDeleted = a})

instance FromJSON Comment where
        parseJSON
          = withObject "Comment"
              (\ o ->
                 Comment' <$>
                   (o .:? "htmlContent") <*> (o .:? "modifiedTime") <*>
                     (o .:? "createdTime")
                     <*> (o .:? "kind" .!= "drive#comment")
                     <*> (o .:? "resolved")
                     <*> (o .:? "quotedFileContent")
                     <*> (o .:? "anchor")
                     <*> (o .:? "content")
                     <*> (o .:? "replies" .!= mempty)
                     <*> (o .:? "author")
                     <*> (o .:? "id")
                     <*> (o .:? "deleted"))

instance ToJSON Comment where
        toJSON Comment'{..}
          = object
              (catMaybes
                 [("htmlContent" .=) <$> _comHTMLContent,
                  ("modifiedTime" .=) <$> _comModifiedTime,
                  ("createdTime" .=) <$> _comCreatedTime,
                  Just ("kind" .= _comKind),
                  ("resolved" .=) <$> _comResolved,
                  ("quotedFileContent" .=) <$> _comQuotedFileContent,
                  ("anchor" .=) <$> _comAnchor,
                  ("content" .=) <$> _comContent,
                  ("replies" .=) <$> _comReplies,
                  ("author" .=) <$> _comAuthor, ("id" .=) <$> _comId,
                  ("deleted" .=) <$> _comDeleted])

-- | The metadata for a revision to a file.
--
-- /See:/ 'revision' smart constructor.
data Revision = Revision'
    { _revModifiedTime :: !(Maybe DateTime')
    , _revSize :: !(Maybe (Textual Int64))
    , _revOriginalFilename :: !(Maybe Text)
    , _revKind :: !Text
    , _revPublished :: !(Maybe Bool)
    , _revLastModifyingUser :: !(Maybe User)
    , _revPublishAuto :: !(Maybe Bool)
    , _revMD5Checksum :: !(Maybe Text)
    , _revKeepForever :: !(Maybe Bool)
    , _revMimeType :: !(Maybe Text)
    , _revPublishedOutsideDomain :: !(Maybe Bool)
    , _revId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Revision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'revModifiedTime'
--
-- * 'revSize'
--
-- * 'revOriginalFilename'
--
-- * 'revKind'
--
-- * 'revPublished'
--
-- * 'revLastModifyingUser'
--
-- * 'revPublishAuto'
--
-- * 'revMD5Checksum'
--
-- * 'revKeepForever'
--
-- * 'revMimeType'
--
-- * 'revPublishedOutsideDomain'
--
-- * 'revId'
revision
    :: Revision
revision = 
    Revision'
    { _revModifiedTime = Nothing
    , _revSize = Nothing
    , _revOriginalFilename = Nothing
    , _revKind = "drive#revision"
    , _revPublished = Nothing
    , _revLastModifyingUser = Nothing
    , _revPublishAuto = Nothing
    , _revMD5Checksum = Nothing
    , _revKeepForever = Nothing
    , _revMimeType = Nothing
    , _revPublishedOutsideDomain = Nothing
    , _revId = Nothing
    }

-- | The last time the revision was modified (RFC 3339 date-time).
revModifiedTime :: Lens' Revision (Maybe UTCTime)
revModifiedTime
  = lens _revModifiedTime
      (\ s a -> s{_revModifiedTime = a})
      . mapping _DateTime

-- | The size of the revision\'s content in bytes. This is only applicable to
-- files with binary content in Drive.
revSize :: Lens' Revision (Maybe Int64)
revSize
  = lens _revSize (\ s a -> s{_revSize = a}) .
      mapping _Coerce

-- | The original filename used to create this revision. This is only
-- applicable to files with binary content in Drive.
revOriginalFilename :: Lens' Revision (Maybe Text)
revOriginalFilename
  = lens _revOriginalFilename
      (\ s a -> s{_revOriginalFilename = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#revision\".
revKind :: Lens' Revision Text
revKind = lens _revKind (\ s a -> s{_revKind = a})

-- | Whether this revision is published. This is only applicable to Google
-- Docs.
revPublished :: Lens' Revision (Maybe Bool)
revPublished
  = lens _revPublished (\ s a -> s{_revPublished = a})

-- | The last user to modify this revision.
revLastModifyingUser :: Lens' Revision (Maybe User)
revLastModifyingUser
  = lens _revLastModifyingUser
      (\ s a -> s{_revLastModifyingUser = a})

-- | Whether subsequent revisions will be automatically republished. This is
-- only applicable to Google Docs.
revPublishAuto :: Lens' Revision (Maybe Bool)
revPublishAuto
  = lens _revPublishAuto
      (\ s a -> s{_revPublishAuto = a})

-- | The MD5 checksum of the revision\'s content. This is only applicable to
-- files with binary content in Drive.
revMD5Checksum :: Lens' Revision (Maybe Text)
revMD5Checksum
  = lens _revMD5Checksum
      (\ s a -> s{_revMD5Checksum = a})

-- | Whether to keep this revision forever, even if it is no longer the head
-- revision. If not set, the revision will be automatically purged 30 days
-- after newer content is uploaded. This can be set on a maximum of 200
-- revisions for a file. This field is only applicable to files with binary
-- content in Drive.
revKeepForever :: Lens' Revision (Maybe Bool)
revKeepForever
  = lens _revKeepForever
      (\ s a -> s{_revKeepForever = a})

-- | The MIME type of the revision.
revMimeType :: Lens' Revision (Maybe Text)
revMimeType
  = lens _revMimeType (\ s a -> s{_revMimeType = a})

-- | Whether this revision is published outside the domain. This is only
-- applicable to Google Docs.
revPublishedOutsideDomain :: Lens' Revision (Maybe Bool)
revPublishedOutsideDomain
  = lens _revPublishedOutsideDomain
      (\ s a -> s{_revPublishedOutsideDomain = a})

-- | The ID of the revision.
revId :: Lens' Revision (Maybe Text)
revId = lens _revId (\ s a -> s{_revId = a})

instance FromJSON Revision where
        parseJSON
          = withObject "Revision"
              (\ o ->
                 Revision' <$>
                   (o .:? "modifiedTime") <*> (o .:? "size") <*>
                     (o .:? "originalFilename")
                     <*> (o .:? "kind" .!= "drive#revision")
                     <*> (o .:? "published")
                     <*> (o .:? "lastModifyingUser")
                     <*> (o .:? "publishAuto")
                     <*> (o .:? "md5Checksum")
                     <*> (o .:? "keepForever")
                     <*> (o .:? "mimeType")
                     <*> (o .:? "publishedOutsideDomain")
                     <*> (o .:? "id"))

instance ToJSON Revision where
        toJSON Revision'{..}
          = object
              (catMaybes
                 [("modifiedTime" .=) <$> _revModifiedTime,
                  ("size" .=) <$> _revSize,
                  ("originalFilename" .=) <$> _revOriginalFilename,
                  Just ("kind" .= _revKind),
                  ("published" .=) <$> _revPublished,
                  ("lastModifyingUser" .=) <$> _revLastModifyingUser,
                  ("publishAuto" .=) <$> _revPublishAuto,
                  ("md5Checksum" .=) <$> _revMD5Checksum,
                  ("keepForever" .=) <$> _revKeepForever,
                  ("mimeType" .=) <$> _revMimeType,
                  ("publishedOutsideDomain" .=) <$>
                    _revPublishedOutsideDomain,
                  ("id" .=) <$> _revId])

-- | A permission for a file. A permission grants a user, group, domain or
-- the world access to a file or a folder hierarchy.
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
    { _pPhotoLink :: !(Maybe Text)
    , _pTeamDrivePermissionDetails :: !(Maybe [PermissionTeamDrivePermissionDetailsItem])
    , _pKind :: !Text
    , _pDomain :: !(Maybe Text)
    , _pRole :: !(Maybe Text)
    , _pEmailAddress :: !(Maybe Text)
    , _pAllowFileDiscovery :: !(Maybe Bool)
    , _pDisplayName :: !(Maybe Text)
    , _pId :: !(Maybe Text)
    , _pDeleted :: !(Maybe Bool)
    , _pType :: !(Maybe Text)
    , _pExpirationTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPhotoLink'
--
-- * 'pTeamDrivePermissionDetails'
--
-- * 'pKind'
--
-- * 'pDomain'
--
-- * 'pRole'
--
-- * 'pEmailAddress'
--
-- * 'pAllowFileDiscovery'
--
-- * 'pDisplayName'
--
-- * 'pId'
--
-- * 'pDeleted'
--
-- * 'pType'
--
-- * 'pExpirationTime'
permission
    :: Permission
permission = 
    Permission'
    { _pPhotoLink = Nothing
    , _pTeamDrivePermissionDetails = Nothing
    , _pKind = "drive#permission"
    , _pDomain = Nothing
    , _pRole = Nothing
    , _pEmailAddress = Nothing
    , _pAllowFileDiscovery = Nothing
    , _pDisplayName = Nothing
    , _pId = Nothing
    , _pDeleted = Nothing
    , _pType = Nothing
    , _pExpirationTime = Nothing
    }

-- | A link to the user\'s profile photo, if available.
pPhotoLink :: Lens' Permission (Maybe Text)
pPhotoLink
  = lens _pPhotoLink (\ s a -> s{_pPhotoLink = a})

-- | Details of whether the permissions on this Team Drive item are inherited
-- or directly on this item. This is an output-only field which is present
-- only for Team Drive items.
pTeamDrivePermissionDetails :: Lens' Permission [PermissionTeamDrivePermissionDetailsItem]
pTeamDrivePermissionDetails
  = lens _pTeamDrivePermissionDetails
      (\ s a -> s{_pTeamDrivePermissionDetails = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#permission\".
pKind :: Lens' Permission Text
pKind = lens _pKind (\ s a -> s{_pKind = a})

-- | The domain to which this permission refers.
pDomain :: Lens' Permission (Maybe Text)
pDomain = lens _pDomain (\ s a -> s{_pDomain = a})

-- | The role granted by this permission. While new values may be supported
-- in the future, the following are currently allowed: - organizer - owner
-- - writer - commenter - reader
pRole :: Lens' Permission (Maybe Text)
pRole = lens _pRole (\ s a -> s{_pRole = a})

-- | The email address of the user or group to which this permission refers.
pEmailAddress :: Lens' Permission (Maybe Text)
pEmailAddress
  = lens _pEmailAddress
      (\ s a -> s{_pEmailAddress = a})

-- | Whether the permission allows the file to be discovered through search.
-- This is only applicable for permissions of type domain or anyone.
pAllowFileDiscovery :: Lens' Permission (Maybe Bool)
pAllowFileDiscovery
  = lens _pAllowFileDiscovery
      (\ s a -> s{_pAllowFileDiscovery = a})

-- | A displayable name for users, groups or domains.
pDisplayName :: Lens' Permission (Maybe Text)
pDisplayName
  = lens _pDisplayName (\ s a -> s{_pDisplayName = a})

-- | The ID of this permission. This is a unique identifier for the grantee,
-- and is published in User resources as permissionId.
pId :: Lens' Permission (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

-- | Whether the account associated with this permission has been deleted.
-- This field only pertains to user and group permissions.
pDeleted :: Lens' Permission (Maybe Bool)
pDeleted = lens _pDeleted (\ s a -> s{_pDeleted = a})

-- | The type of the grantee. Valid values are: - user - group - domain -
-- anyone
pType :: Lens' Permission (Maybe Text)
pType = lens _pType (\ s a -> s{_pType = a})

-- | The time at which this permission will expire (RFC 3339 date-time).
-- Expiration times have the following restrictions: - They can only be set
-- on user and group permissions - The time must be in the future - The
-- time cannot be more than a year in the future
pExpirationTime :: Lens' Permission (Maybe UTCTime)
pExpirationTime
  = lens _pExpirationTime
      (\ s a -> s{_pExpirationTime = a})
      . mapping _DateTime

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ o ->
                 Permission' <$>
                   (o .:? "photoLink") <*>
                     (o .:? "teamDrivePermissionDetails" .!= mempty)
                     <*> (o .:? "kind" .!= "drive#permission")
                     <*> (o .:? "domain")
                     <*> (o .:? "role")
                     <*> (o .:? "emailAddress")
                     <*> (o .:? "allowFileDiscovery")
                     <*> (o .:? "displayName")
                     <*> (o .:? "id")
                     <*> (o .:? "deleted")
                     <*> (o .:? "type")
                     <*> (o .:? "expirationTime"))

instance ToJSON Permission where
        toJSON Permission'{..}
          = object
              (catMaybes
                 [("photoLink" .=) <$> _pPhotoLink,
                  ("teamDrivePermissionDetails" .=) <$>
                    _pTeamDrivePermissionDetails,
                  Just ("kind" .= _pKind), ("domain" .=) <$> _pDomain,
                  ("role" .=) <$> _pRole,
                  ("emailAddress" .=) <$> _pEmailAddress,
                  ("allowFileDiscovery" .=) <$> _pAllowFileDiscovery,
                  ("displayName" .=) <$> _pDisplayName,
                  ("id" .=) <$> _pId, ("deleted" .=) <$> _pDeleted,
                  ("type" .=) <$> _pType,
                  ("expirationTime" .=) <$> _pExpirationTime])

-- | The metadata for a file.
--
-- /See:/ 'file' smart constructor.
data File = File'
    { _fOwnedByMe :: !(Maybe Bool)
    , _fThumbnailLink :: !(Maybe Text)
    , _fFullFileExtension :: !(Maybe Text)
    , _fModifiedTime :: !(Maybe DateTime')
    , _fModifiedByMeTime :: !(Maybe DateTime')
    , _fFileExtension :: !(Maybe Text)
    , _fViewedByMe :: !(Maybe Bool)
    , _fOwners :: !(Maybe [User])
    , _fViewedByMeTime :: !(Maybe DateTime')
    , _fModifiedByMe :: !(Maybe Bool)
    , _fSize :: !(Maybe (Textual Int64))
    , _fTrashed :: !(Maybe Bool)
    , _fWebViewLink :: !(Maybe Text)
    , _fCreatedTime :: !(Maybe DateTime')
    , _fTrashedTime :: !(Maybe DateTime')
    , _fOriginalFilename :: !(Maybe Text)
    , _fKind :: !Text
    , _fLastModifyingUser :: !(Maybe User)
    , _fIconLink :: !(Maybe Text)
    , _fHasThumbnail :: !(Maybe Bool)
    , _fThumbnailVersion :: !(Maybe (Textual Int64))
    , _fImageMediaMetadata :: !(Maybe FileImageMediaMetadata)
    , _fExplicitlyTrashed :: !(Maybe Bool)
    , _fShared :: !(Maybe Bool)
    , _fMD5Checksum :: !(Maybe Text)
    , _fTeamDriveId :: !(Maybe Text)
    , _fFolderColorRgb :: !(Maybe Text)
    , _fMimeType :: !(Maybe Text)
    , _fIsAppAuthorized :: !(Maybe Bool)
    , _fName :: !(Maybe Text)
    , _fParents :: !(Maybe [Text])
    , _fStarred :: !(Maybe Bool)
    , _fSpaces :: !(Maybe [Text])
    , _fVersion :: !(Maybe (Textual Int64))
    , _fHasAugmentedPermissions :: !(Maybe Bool)
    , _fWritersCanShare :: !(Maybe Bool)
    , _fTrashingUser :: !(Maybe User)
    , _fId :: !(Maybe Text)
    , _fPermissionIds :: !(Maybe [Text])
    , _fPermissions :: !(Maybe [Permission])
    , _fQuotaBytesUsed :: !(Maybe (Textual Int64))
    , _fAppProperties :: !(Maybe FileAppProperties)
    , _fVideoMediaMetadata :: !(Maybe FileVideoMediaMetadata)
    , _fSharedWithMeTime :: !(Maybe DateTime')
    , _fHeadRevisionId :: !(Maybe Text)
    , _fCapabilities :: !(Maybe FileCapabilities)
    , _fDescription :: !(Maybe Text)
    , _fViewersCanCopyContent :: !(Maybe Bool)
    , _fSharingUser :: !(Maybe User)
    , _fWebContentLink :: !(Maybe Text)
    , _fContentHints :: !(Maybe FileContentHints)
    , _fProperties :: !(Maybe FileProperties)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'File' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fOwnedByMe'
--
-- * 'fThumbnailLink'
--
-- * 'fFullFileExtension'
--
-- * 'fModifiedTime'
--
-- * 'fModifiedByMeTime'
--
-- * 'fFileExtension'
--
-- * 'fViewedByMe'
--
-- * 'fOwners'
--
-- * 'fViewedByMeTime'
--
-- * 'fModifiedByMe'
--
-- * 'fSize'
--
-- * 'fTrashed'
--
-- * 'fWebViewLink'
--
-- * 'fCreatedTime'
--
-- * 'fTrashedTime'
--
-- * 'fOriginalFilename'
--
-- * 'fKind'
--
-- * 'fLastModifyingUser'
--
-- * 'fIconLink'
--
-- * 'fHasThumbnail'
--
-- * 'fThumbnailVersion'
--
-- * 'fImageMediaMetadata'
--
-- * 'fExplicitlyTrashed'
--
-- * 'fShared'
--
-- * 'fMD5Checksum'
--
-- * 'fTeamDriveId'
--
-- * 'fFolderColorRgb'
--
-- * 'fMimeType'
--
-- * 'fIsAppAuthorized'
--
-- * 'fName'
--
-- * 'fParents'
--
-- * 'fStarred'
--
-- * 'fSpaces'
--
-- * 'fVersion'
--
-- * 'fHasAugmentedPermissions'
--
-- * 'fWritersCanShare'
--
-- * 'fTrashingUser'
--
-- * 'fId'
--
-- * 'fPermissionIds'
--
-- * 'fPermissions'
--
-- * 'fQuotaBytesUsed'
--
-- * 'fAppProperties'
--
-- * 'fVideoMediaMetadata'
--
-- * 'fSharedWithMeTime'
--
-- * 'fHeadRevisionId'
--
-- * 'fCapabilities'
--
-- * 'fDescription'
--
-- * 'fViewersCanCopyContent'
--
-- * 'fSharingUser'
--
-- * 'fWebContentLink'
--
-- * 'fContentHints'
--
-- * 'fProperties'
file
    :: File
file = 
    File'
    { _fOwnedByMe = Nothing
    , _fThumbnailLink = Nothing
    , _fFullFileExtension = Nothing
    , _fModifiedTime = Nothing
    , _fModifiedByMeTime = Nothing
    , _fFileExtension = Nothing
    , _fViewedByMe = Nothing
    , _fOwners = Nothing
    , _fViewedByMeTime = Nothing
    , _fModifiedByMe = Nothing
    , _fSize = Nothing
    , _fTrashed = Nothing
    , _fWebViewLink = Nothing
    , _fCreatedTime = Nothing
    , _fTrashedTime = Nothing
    , _fOriginalFilename = Nothing
    , _fKind = "drive#file"
    , _fLastModifyingUser = Nothing
    , _fIconLink = Nothing
    , _fHasThumbnail = Nothing
    , _fThumbnailVersion = Nothing
    , _fImageMediaMetadata = Nothing
    , _fExplicitlyTrashed = Nothing
    , _fShared = Nothing
    , _fMD5Checksum = Nothing
    , _fTeamDriveId = Nothing
    , _fFolderColorRgb = Nothing
    , _fMimeType = Nothing
    , _fIsAppAuthorized = Nothing
    , _fName = Nothing
    , _fParents = Nothing
    , _fStarred = Nothing
    , _fSpaces = Nothing
    , _fVersion = Nothing
    , _fHasAugmentedPermissions = Nothing
    , _fWritersCanShare = Nothing
    , _fTrashingUser = Nothing
    , _fId = Nothing
    , _fPermissionIds = Nothing
    , _fPermissions = Nothing
    , _fQuotaBytesUsed = Nothing
    , _fAppProperties = Nothing
    , _fVideoMediaMetadata = Nothing
    , _fSharedWithMeTime = Nothing
    , _fHeadRevisionId = Nothing
    , _fCapabilities = Nothing
    , _fDescription = Nothing
    , _fViewersCanCopyContent = Nothing
    , _fSharingUser = Nothing
    , _fWebContentLink = Nothing
    , _fContentHints = Nothing
    , _fProperties = Nothing
    }

-- | Whether the user owns the file. Not populated for Team Drive files.
fOwnedByMe :: Lens' File (Maybe Bool)
fOwnedByMe
  = lens _fOwnedByMe (\ s a -> s{_fOwnedByMe = a})

-- | A short-lived link to the file\'s thumbnail, if available. Typically
-- lasts on the order of hours. Only populated when the requesting app can
-- access the file\'s content.
fThumbnailLink :: Lens' File (Maybe Text)
fThumbnailLink
  = lens _fThumbnailLink
      (\ s a -> s{_fThumbnailLink = a})

-- | The full file extension extracted from the name field. May contain
-- multiple concatenated extensions, such as \"tar.gz\". This is only
-- available for files with binary content in Drive. This is automatically
-- updated when the name field changes, however it is not cleared if the
-- new name does not contain a valid extension.
fFullFileExtension :: Lens' File (Maybe Text)
fFullFileExtension
  = lens _fFullFileExtension
      (\ s a -> s{_fFullFileExtension = a})

-- | The last time the file was modified by anyone (RFC 3339 date-time). Note
-- that setting modifiedTime will also update modifiedByMeTime for the
-- user.
fModifiedTime :: Lens' File (Maybe UTCTime)
fModifiedTime
  = lens _fModifiedTime
      (\ s a -> s{_fModifiedTime = a})
      . mapping _DateTime

-- | The last time the file was modified by the user (RFC 3339 date-time).
fModifiedByMeTime :: Lens' File (Maybe UTCTime)
fModifiedByMeTime
  = lens _fModifiedByMeTime
      (\ s a -> s{_fModifiedByMeTime = a})
      . mapping _DateTime

-- | The final component of fullFileExtension. This is only available for
-- files with binary content in Drive.
fFileExtension :: Lens' File (Maybe Text)
fFileExtension
  = lens _fFileExtension
      (\ s a -> s{_fFileExtension = a})

-- | Whether the file has been viewed by this user.
fViewedByMe :: Lens' File (Maybe Bool)
fViewedByMe
  = lens _fViewedByMe (\ s a -> s{_fViewedByMe = a})

-- | The owners of the file. Currently, only certain legacy files may have
-- more than one owner. Not populated for Team Drive files.
fOwners :: Lens' File [User]
fOwners
  = lens _fOwners (\ s a -> s{_fOwners = a}) . _Default
      . _Coerce

-- | The last time the file was viewed by the user (RFC 3339 date-time).
fViewedByMeTime :: Lens' File (Maybe UTCTime)
fViewedByMeTime
  = lens _fViewedByMeTime
      (\ s a -> s{_fViewedByMeTime = a})
      . mapping _DateTime

-- | Whether the file has been modified by this user.
fModifiedByMe :: Lens' File (Maybe Bool)
fModifiedByMe
  = lens _fModifiedByMe
      (\ s a -> s{_fModifiedByMe = a})

-- | The size of the file\'s content in bytes. This is only applicable to
-- files with binary content in Drive.
fSize :: Lens' File (Maybe Int64)
fSize
  = lens _fSize (\ s a -> s{_fSize = a}) .
      mapping _Coerce

-- | Whether the file has been trashed, either explicitly or from a trashed
-- parent folder. Only the owner may trash a file, and other users cannot
-- see files in the owner\'s trash.
fTrashed :: Lens' File (Maybe Bool)
fTrashed = lens _fTrashed (\ s a -> s{_fTrashed = a})

-- | A link for opening the file in a relevant Google editor or viewer in a
-- browser.
fWebViewLink :: Lens' File (Maybe Text)
fWebViewLink
  = lens _fWebViewLink (\ s a -> s{_fWebViewLink = a})

-- | The time at which the file was created (RFC 3339 date-time).
fCreatedTime :: Lens' File (Maybe UTCTime)
fCreatedTime
  = lens _fCreatedTime (\ s a -> s{_fCreatedTime = a})
      . mapping _DateTime

-- | The time that the item was trashed (RFC 3339 date-time). Only populated
-- for Team Drive files.
fTrashedTime :: Lens' File (Maybe UTCTime)
fTrashedTime
  = lens _fTrashedTime (\ s a -> s{_fTrashedTime = a})
      . mapping _DateTime

-- | The original filename of the uploaded content if available, or else the
-- original value of the name field. This is only available for files with
-- binary content in Drive.
fOriginalFilename :: Lens' File (Maybe Text)
fOriginalFilename
  = lens _fOriginalFilename
      (\ s a -> s{_fOriginalFilename = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#file\".
fKind :: Lens' File Text
fKind = lens _fKind (\ s a -> s{_fKind = a})

-- | The last user to modify the file.
fLastModifyingUser :: Lens' File (Maybe User)
fLastModifyingUser
  = lens _fLastModifyingUser
      (\ s a -> s{_fLastModifyingUser = a})

-- | A static, unauthenticated link to the file\'s icon.
fIconLink :: Lens' File (Maybe Text)
fIconLink
  = lens _fIconLink (\ s a -> s{_fIconLink = a})

-- | Whether this file has a thumbnail. This does not indicate whether the
-- requesting app has access to the thumbnail. To check access, look for
-- the presence of the thumbnailLink field.
fHasThumbnail :: Lens' File (Maybe Bool)
fHasThumbnail
  = lens _fHasThumbnail
      (\ s a -> s{_fHasThumbnail = a})

-- | The thumbnail version for use in thumbnail cache invalidation.
fThumbnailVersion :: Lens' File (Maybe Int64)
fThumbnailVersion
  = lens _fThumbnailVersion
      (\ s a -> s{_fThumbnailVersion = a})
      . mapping _Coerce

-- | Additional metadata about image media, if available.
fImageMediaMetadata :: Lens' File (Maybe FileImageMediaMetadata)
fImageMediaMetadata
  = lens _fImageMediaMetadata
      (\ s a -> s{_fImageMediaMetadata = a})

-- | Whether the file has been explicitly trashed, as opposed to recursively
-- trashed from a parent folder.
fExplicitlyTrashed :: Lens' File (Maybe Bool)
fExplicitlyTrashed
  = lens _fExplicitlyTrashed
      (\ s a -> s{_fExplicitlyTrashed = a})

-- | Whether the file has been shared. Not populated for Team Drive files.
fShared :: Lens' File (Maybe Bool)
fShared = lens _fShared (\ s a -> s{_fShared = a})

-- | The MD5 checksum for the content of the file. This is only applicable to
-- files with binary content in Drive.
fMD5Checksum :: Lens' File (Maybe Text)
fMD5Checksum
  = lens _fMD5Checksum (\ s a -> s{_fMD5Checksum = a})

-- | ID of the Team Drive the file resides in.
fTeamDriveId :: Lens' File (Maybe Text)
fTeamDriveId
  = lens _fTeamDriveId (\ s a -> s{_fTeamDriveId = a})

-- | The color for a folder as an RGB hex string. The supported colors are
-- published in the folderColorPalette field of the About resource. If an
-- unsupported color is specified, the closest color in the palette will be
-- used instead.
fFolderColorRgb :: Lens' File (Maybe Text)
fFolderColorRgb
  = lens _fFolderColorRgb
      (\ s a -> s{_fFolderColorRgb = a})

-- | The MIME type of the file. Drive will attempt to automatically detect an
-- appropriate value from uploaded content if no value is provided. The
-- value cannot be changed unless a new revision is uploaded. If a file is
-- created with a Google Doc MIME type, the uploaded content will be
-- imported if possible. The supported import formats are published in the
-- About resource.
fMimeType :: Lens' File (Maybe Text)
fMimeType
  = lens _fMimeType (\ s a -> s{_fMimeType = a})

-- | Whether the file was created or opened by the requesting app.
fIsAppAuthorized :: Lens' File (Maybe Bool)
fIsAppAuthorized
  = lens _fIsAppAuthorized
      (\ s a -> s{_fIsAppAuthorized = a})

-- | The name of the file. This is not necessarily unique within a folder.
-- Note that for immutable items such as the top level folders of Team
-- Drives, My Drive root folder, and Application Data folder the name is
-- constant.
fName :: Lens' File (Maybe Text)
fName = lens _fName (\ s a -> s{_fName = a})

-- | The IDs of the parent folders which contain the file. If not specified
-- as part of a create request, the file will be placed directly in the
-- user\'s My Drive folder. If not specified as part of a copy request, the
-- file will inherit any discoverable parents of the source file. Update
-- requests must use the addParents and removeParents parameters to modify
-- the parents list.
fParents :: Lens' File [Text]
fParents
  = lens _fParents (\ s a -> s{_fParents = a}) .
      _Default
      . _Coerce

-- | Whether the user has starred the file.
fStarred :: Lens' File (Maybe Bool)
fStarred = lens _fStarred (\ s a -> s{_fStarred = a})

-- | The list of spaces which contain the file. The currently supported
-- values are \'drive\', \'appDataFolder\' and \'photos\'.
fSpaces :: Lens' File [Text]
fSpaces
  = lens _fSpaces (\ s a -> s{_fSpaces = a}) . _Default
      . _Coerce

-- | A monotonically increasing version number for the file. This reflects
-- every change made to the file on the server, even those not visible to
-- the user.
fVersion :: Lens' File (Maybe Int64)
fVersion
  = lens _fVersion (\ s a -> s{_fVersion = a}) .
      mapping _Coerce

-- | Whether any users are granted file access directly on this file. This
-- field is only populated for Team Drive files.
fHasAugmentedPermissions :: Lens' File (Maybe Bool)
fHasAugmentedPermissions
  = lens _fHasAugmentedPermissions
      (\ s a -> s{_fHasAugmentedPermissions = a})

-- | Whether users with only writer permission can modify the file\'s
-- permissions. Not populated for Team Drive files.
fWritersCanShare :: Lens' File (Maybe Bool)
fWritersCanShare
  = lens _fWritersCanShare
      (\ s a -> s{_fWritersCanShare = a})

-- | If the file has been explicitly trashed, the user who trashed it. Only
-- populated for Team Drive files.
fTrashingUser :: Lens' File (Maybe User)
fTrashingUser
  = lens _fTrashingUser
      (\ s a -> s{_fTrashingUser = a})

-- | The ID of the file.
fId :: Lens' File (Maybe Text)
fId = lens _fId (\ s a -> s{_fId = a})

-- | List of permission IDs for users with access to this file.
fPermissionIds :: Lens' File [Text]
fPermissionIds
  = lens _fPermissionIds
      (\ s a -> s{_fPermissionIds = a})
      . _Default
      . _Coerce

-- | The full list of permissions for the file. This is only available if the
-- requesting user can share the file. Not populated for Team Drive files.
fPermissions :: Lens' File [Permission]
fPermissions
  = lens _fPermissions (\ s a -> s{_fPermissions = a})
      . _Default
      . _Coerce

-- | The number of storage quota bytes used by the file. This includes the
-- head revision as well as previous revisions with keepForever enabled.
fQuotaBytesUsed :: Lens' File (Maybe Int64)
fQuotaBytesUsed
  = lens _fQuotaBytesUsed
      (\ s a -> s{_fQuotaBytesUsed = a})
      . mapping _Coerce

-- | A collection of arbitrary key-value pairs which are private to the
-- requesting app. Entries with null values are cleared in update and copy
-- requests.
fAppProperties :: Lens' File (Maybe FileAppProperties)
fAppProperties
  = lens _fAppProperties
      (\ s a -> s{_fAppProperties = a})

-- | Additional metadata about video media. This may not be available
-- immediately upon upload.
fVideoMediaMetadata :: Lens' File (Maybe FileVideoMediaMetadata)
fVideoMediaMetadata
  = lens _fVideoMediaMetadata
      (\ s a -> s{_fVideoMediaMetadata = a})

-- | The time at which the file was shared with the user, if applicable (RFC
-- 3339 date-time).
fSharedWithMeTime :: Lens' File (Maybe UTCTime)
fSharedWithMeTime
  = lens _fSharedWithMeTime
      (\ s a -> s{_fSharedWithMeTime = a})
      . mapping _DateTime

-- | The ID of the file\'s head revision. This is currently only available
-- for files with binary content in Drive.
fHeadRevisionId :: Lens' File (Maybe Text)
fHeadRevisionId
  = lens _fHeadRevisionId
      (\ s a -> s{_fHeadRevisionId = a})

-- | Capabilities the current user has on this file. Each capability
-- corresponds to a fine-grained action that a user may take.
fCapabilities :: Lens' File (Maybe FileCapabilities)
fCapabilities
  = lens _fCapabilities
      (\ s a -> s{_fCapabilities = a})

-- | A short description of the file.
fDescription :: Lens' File (Maybe Text)
fDescription
  = lens _fDescription (\ s a -> s{_fDescription = a})

-- | Whether users with only reader or commenter permission can copy the
-- file\'s content. This affects copy, download, and print operations.
fViewersCanCopyContent :: Lens' File (Maybe Bool)
fViewersCanCopyContent
  = lens _fViewersCanCopyContent
      (\ s a -> s{_fViewersCanCopyContent = a})

-- | The user who shared the file with the requesting user, if applicable.
fSharingUser :: Lens' File (Maybe User)
fSharingUser
  = lens _fSharingUser (\ s a -> s{_fSharingUser = a})

-- | A link for downloading the content of the file in a browser. This is
-- only available for files with binary content in Drive.
fWebContentLink :: Lens' File (Maybe Text)
fWebContentLink
  = lens _fWebContentLink
      (\ s a -> s{_fWebContentLink = a})

-- | Additional information about the content of the file. These fields are
-- never populated in responses.
fContentHints :: Lens' File (Maybe FileContentHints)
fContentHints
  = lens _fContentHints
      (\ s a -> s{_fContentHints = a})

-- | A collection of arbitrary key-value pairs which are visible to all apps.
-- Entries with null values are cleared in update and copy requests.
fProperties :: Lens' File (Maybe FileProperties)
fProperties
  = lens _fProperties (\ s a -> s{_fProperties = a})

instance FromJSON File where
        parseJSON
          = withObject "File"
              (\ o ->
                 File' <$>
                   (o .:? "ownedByMe") <*> (o .:? "thumbnailLink") <*>
                     (o .:? "fullFileExtension")
                     <*> (o .:? "modifiedTime")
                     <*> (o .:? "modifiedByMeTime")
                     <*> (o .:? "fileExtension")
                     <*> (o .:? "viewedByMe")
                     <*> (o .:? "owners" .!= mempty)
                     <*> (o .:? "viewedByMeTime")
                     <*> (o .:? "modifiedByMe")
                     <*> (o .:? "size")
                     <*> (o .:? "trashed")
                     <*> (o .:? "webViewLink")
                     <*> (o .:? "createdTime")
                     <*> (o .:? "trashedTime")
                     <*> (o .:? "originalFilename")
                     <*> (o .:? "kind" .!= "drive#file")
                     <*> (o .:? "lastModifyingUser")
                     <*> (o .:? "iconLink")
                     <*> (o .:? "hasThumbnail")
                     <*> (o .:? "thumbnailVersion")
                     <*> (o .:? "imageMediaMetadata")
                     <*> (o .:? "explicitlyTrashed")
                     <*> (o .:? "shared")
                     <*> (o .:? "md5Checksum")
                     <*> (o .:? "teamDriveId")
                     <*> (o .:? "folderColorRgb")
                     <*> (o .:? "mimeType")
                     <*> (o .:? "isAppAuthorized")
                     <*> (o .:? "name")
                     <*> (o .:? "parents" .!= mempty)
                     <*> (o .:? "starred")
                     <*> (o .:? "spaces" .!= mempty)
                     <*> (o .:? "version")
                     <*> (o .:? "hasAugmentedPermissions")
                     <*> (o .:? "writersCanShare")
                     <*> (o .:? "trashingUser")
                     <*> (o .:? "id")
                     <*> (o .:? "permissionIds" .!= mempty)
                     <*> (o .:? "permissions" .!= mempty)
                     <*> (o .:? "quotaBytesUsed")
                     <*> (o .:? "appProperties")
                     <*> (o .:? "videoMediaMetadata")
                     <*> (o .:? "sharedWithMeTime")
                     <*> (o .:? "headRevisionId")
                     <*> (o .:? "capabilities")
                     <*> (o .:? "description")
                     <*> (o .:? "viewersCanCopyContent")
                     <*> (o .:? "sharingUser")
                     <*> (o .:? "webContentLink")
                     <*> (o .:? "contentHints")
                     <*> (o .:? "properties"))

instance ToJSON File where
        toJSON File'{..}
          = object
              (catMaybes
                 [("ownedByMe" .=) <$> _fOwnedByMe,
                  ("thumbnailLink" .=) <$> _fThumbnailLink,
                  ("fullFileExtension" .=) <$> _fFullFileExtension,
                  ("modifiedTime" .=) <$> _fModifiedTime,
                  ("modifiedByMeTime" .=) <$> _fModifiedByMeTime,
                  ("fileExtension" .=) <$> _fFileExtension,
                  ("viewedByMe" .=) <$> _fViewedByMe,
                  ("owners" .=) <$> _fOwners,
                  ("viewedByMeTime" .=) <$> _fViewedByMeTime,
                  ("modifiedByMe" .=) <$> _fModifiedByMe,
                  ("size" .=) <$> _fSize, ("trashed" .=) <$> _fTrashed,
                  ("webViewLink" .=) <$> _fWebViewLink,
                  ("createdTime" .=) <$> _fCreatedTime,
                  ("trashedTime" .=) <$> _fTrashedTime,
                  ("originalFilename" .=) <$> _fOriginalFilename,
                  Just ("kind" .= _fKind),
                  ("lastModifyingUser" .=) <$> _fLastModifyingUser,
                  ("iconLink" .=) <$> _fIconLink,
                  ("hasThumbnail" .=) <$> _fHasThumbnail,
                  ("thumbnailVersion" .=) <$> _fThumbnailVersion,
                  ("imageMediaMetadata" .=) <$> _fImageMediaMetadata,
                  ("explicitlyTrashed" .=) <$> _fExplicitlyTrashed,
                  ("shared" .=) <$> _fShared,
                  ("md5Checksum" .=) <$> _fMD5Checksum,
                  ("teamDriveId" .=) <$> _fTeamDriveId,
                  ("folderColorRgb" .=) <$> _fFolderColorRgb,
                  ("mimeType" .=) <$> _fMimeType,
                  ("isAppAuthorized" .=) <$> _fIsAppAuthorized,
                  ("name" .=) <$> _fName, ("parents" .=) <$> _fParents,
                  ("starred" .=) <$> _fStarred,
                  ("spaces" .=) <$> _fSpaces,
                  ("version" .=) <$> _fVersion,
                  ("hasAugmentedPermissions" .=) <$>
                    _fHasAugmentedPermissions,
                  ("writersCanShare" .=) <$> _fWritersCanShare,
                  ("trashingUser" .=) <$> _fTrashingUser,
                  ("id" .=) <$> _fId,
                  ("permissionIds" .=) <$> _fPermissionIds,
                  ("permissions" .=) <$> _fPermissions,
                  ("quotaBytesUsed" .=) <$> _fQuotaBytesUsed,
                  ("appProperties" .=) <$> _fAppProperties,
                  ("videoMediaMetadata" .=) <$> _fVideoMediaMetadata,
                  ("sharedWithMeTime" .=) <$> _fSharedWithMeTime,
                  ("headRevisionId" .=) <$> _fHeadRevisionId,
                  ("capabilities" .=) <$> _fCapabilities,
                  ("description" .=) <$> _fDescription,
                  ("viewersCanCopyContent" .=) <$>
                    _fViewersCanCopyContent,
                  ("sharingUser" .=) <$> _fSharingUser,
                  ("webContentLink" .=) <$> _fWebContentLink,
                  ("contentHints" .=) <$> _fContentHints,
                  ("properties" .=) <$> _fProperties])

--
-- /See:/ 'permissionTeamDrivePermissionDetailsItem' smart constructor.
data PermissionTeamDrivePermissionDetailsItem = PermissionTeamDrivePermissionDetailsItem'
    { _ptdpdiInherited :: !(Maybe Bool)
    , _ptdpdiTeamDrivePermissionType :: !(Maybe Text)
    , _ptdpdiRole :: !(Maybe Text)
    , _ptdpdiInheritedFrom :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PermissionTeamDrivePermissionDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptdpdiInherited'
--
-- * 'ptdpdiTeamDrivePermissionType'
--
-- * 'ptdpdiRole'
--
-- * 'ptdpdiInheritedFrom'
permissionTeamDrivePermissionDetailsItem
    :: PermissionTeamDrivePermissionDetailsItem
permissionTeamDrivePermissionDetailsItem = 
    PermissionTeamDrivePermissionDetailsItem'
    { _ptdpdiInherited = Nothing
    , _ptdpdiTeamDrivePermissionType = Nothing
    , _ptdpdiRole = Nothing
    , _ptdpdiInheritedFrom = Nothing
    }

-- | Whether this permission is inherited. This field is always populated.
-- This is an output-only field.
ptdpdiInherited :: Lens' PermissionTeamDrivePermissionDetailsItem (Maybe Bool)
ptdpdiInherited
  = lens _ptdpdiInherited
      (\ s a -> s{_ptdpdiInherited = a})

-- | The Team Drive permission type for this user. While new values may be
-- added in future, the following are currently possible: - file - member
ptdpdiTeamDrivePermissionType :: Lens' PermissionTeamDrivePermissionDetailsItem (Maybe Text)
ptdpdiTeamDrivePermissionType
  = lens _ptdpdiTeamDrivePermissionType
      (\ s a -> s{_ptdpdiTeamDrivePermissionType = a})

-- | The primary role for this user. While new values may be added in the
-- future, the following are currently possible: - organizer - writer -
-- commenter - reader
ptdpdiRole :: Lens' PermissionTeamDrivePermissionDetailsItem (Maybe Text)
ptdpdiRole
  = lens _ptdpdiRole (\ s a -> s{_ptdpdiRole = a})

-- | The ID of the item from which this permission is inherited. This is an
-- output-only field and is only populated for members of the Team Drive.
ptdpdiInheritedFrom :: Lens' PermissionTeamDrivePermissionDetailsItem (Maybe Text)
ptdpdiInheritedFrom
  = lens _ptdpdiInheritedFrom
      (\ s a -> s{_ptdpdiInheritedFrom = a})

instance FromJSON
         PermissionTeamDrivePermissionDetailsItem where
        parseJSON
          = withObject
              "PermissionTeamDrivePermissionDetailsItem"
              (\ o ->
                 PermissionTeamDrivePermissionDetailsItem' <$>
                   (o .:? "inherited") <*>
                     (o .:? "teamDrivePermissionType")
                     <*> (o .:? "role")
                     <*> (o .:? "inheritedFrom"))

instance ToJSON
         PermissionTeamDrivePermissionDetailsItem where
        toJSON PermissionTeamDrivePermissionDetailsItem'{..}
          = object
              (catMaybes
                 [("inherited" .=) <$> _ptdpdiInherited,
                  ("teamDrivePermissionType" .=) <$>
                    _ptdpdiTeamDrivePermissionType,
                  ("role" .=) <$> _ptdpdiRole,
                  ("inheritedFrom" .=) <$> _ptdpdiInheritedFrom])

-- | A list of generated file IDs which can be provided in create requests.
--
-- /See:/ 'generatedIds' smart constructor.
data GeneratedIds = GeneratedIds'
    { _giSpace :: !(Maybe Text)
    , _giKind :: !Text
    , _giIds :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GeneratedIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giSpace'
--
-- * 'giKind'
--
-- * 'giIds'
generatedIds
    :: GeneratedIds
generatedIds = 
    GeneratedIds'
    { _giSpace = Nothing
    , _giKind = "drive#generatedIds"
    , _giIds = Nothing
    }

-- | The type of file that can be created with these IDs.
giSpace :: Lens' GeneratedIds (Maybe Text)
giSpace = lens _giSpace (\ s a -> s{_giSpace = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#generatedIds\".
giKind :: Lens' GeneratedIds Text
giKind = lens _giKind (\ s a -> s{_giKind = a})

-- | The IDs generated for the requesting user in the specified space.
giIds :: Lens' GeneratedIds [Text]
giIds
  = lens _giIds (\ s a -> s{_giIds = a}) . _Default .
      _Coerce

instance FromJSON GeneratedIds where
        parseJSON
          = withObject "GeneratedIds"
              (\ o ->
                 GeneratedIds' <$>
                   (o .:? "space") <*>
                     (o .:? "kind" .!= "drive#generatedIds")
                     <*> (o .:? "ids" .!= mempty))

instance ToJSON GeneratedIds where
        toJSON GeneratedIds'{..}
          = object
              (catMaybes
                 [("space" .=) <$> _giSpace, Just ("kind" .= _giKind),
                  ("ids" .=) <$> _giIds])

-- | A list of comments on a file.
--
-- /See:/ 'commentList' smart constructor.
data CommentList = CommentList'
    { _cllNextPageToken :: !(Maybe Text)
    , _cllKind :: !Text
    , _cllComments :: !(Maybe [Comment])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommentList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cllNextPageToken'
--
-- * 'cllKind'
--
-- * 'cllComments'
commentList
    :: CommentList
commentList = 
    CommentList'
    { _cllNextPageToken = Nothing
    , _cllKind = "drive#commentList"
    , _cllComments = Nothing
    }

-- | The page token for the next page of comments. This will be absent if the
-- end of the comments list has been reached. If the token is rejected for
-- any reason, it should be discarded, and pagination should be restarted
-- from the first page of results.
cllNextPageToken :: Lens' CommentList (Maybe Text)
cllNextPageToken
  = lens _cllNextPageToken
      (\ s a -> s{_cllNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#commentList\".
cllKind :: Lens' CommentList Text
cllKind = lens _cllKind (\ s a -> s{_cllKind = a})

-- | The list of comments. If nextPageToken is populated, then this list may
-- be incomplete and an additional page of results should be fetched.
cllComments :: Lens' CommentList [Comment]
cllComments
  = lens _cllComments (\ s a -> s{_cllComments = a}) .
      _Default
      . _Coerce

instance FromJSON CommentList where
        parseJSON
          = withObject "CommentList"
              (\ o ->
                 CommentList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "drive#commentList")
                     <*> (o .:? "comments" .!= mempty))

instance ToJSON CommentList where
        toJSON CommentList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _cllNextPageToken,
                  Just ("kind" .= _cllKind),
                  ("comments" .=) <$> _cllComments])

-- | A list of revisions of a file.
--
-- /See:/ 'revisionList' smart constructor.
data RevisionList = RevisionList'
    { _rllNextPageToken :: !(Maybe Text)
    , _rllKind :: !Text
    , _rllRevisions :: !(Maybe [Revision])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevisionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rllNextPageToken'
--
-- * 'rllKind'
--
-- * 'rllRevisions'
revisionList
    :: RevisionList
revisionList = 
    RevisionList'
    { _rllNextPageToken = Nothing
    , _rllKind = "drive#revisionList"
    , _rllRevisions = Nothing
    }

-- | The page token for the next page of revisions. This will be absent if
-- the end of the revisions list has been reached. If the token is rejected
-- for any reason, it should be discarded, and pagination should be
-- restarted from the first page of results.
rllNextPageToken :: Lens' RevisionList (Maybe Text)
rllNextPageToken
  = lens _rllNextPageToken
      (\ s a -> s{_rllNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#revisionList\".
rllKind :: Lens' RevisionList Text
rllKind = lens _rllKind (\ s a -> s{_rllKind = a})

-- | The list of revisions. If nextPageToken is populated, then this list may
-- be incomplete and an additional page of results should be fetched.
rllRevisions :: Lens' RevisionList [Revision]
rllRevisions
  = lens _rllRevisions (\ s a -> s{_rllRevisions = a})
      . _Default
      . _Coerce

instance FromJSON RevisionList where
        parseJSON
          = withObject "RevisionList"
              (\ o ->
                 RevisionList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "drive#revisionList")
                     <*> (o .:? "revisions" .!= mempty))

instance ToJSON RevisionList where
        toJSON RevisionList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _rllNextPageToken,
                  Just ("kind" .= _rllKind),
                  ("revisions" .=) <$> _rllRevisions])

-- | A list of permissions for a file.
--
-- /See:/ 'permissionList' smart constructor.
data PermissionList = PermissionList'
    { _plNextPageToken :: !(Maybe Text)
    , _plKind :: !Text
    , _plPermissions :: !(Maybe [Permission])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PermissionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plNextPageToken'
--
-- * 'plKind'
--
-- * 'plPermissions'
permissionList
    :: PermissionList
permissionList = 
    PermissionList'
    { _plNextPageToken = Nothing
    , _plKind = "drive#permissionList"
    , _plPermissions = Nothing
    }

-- | The page token for the next page of permissions. This field will be
-- absent if the end of the permissions list has been reached. If the token
-- is rejected for any reason, it should be discarded, and pagination
-- should be restarted from the first page of results.
plNextPageToken :: Lens' PermissionList (Maybe Text)
plNextPageToken
  = lens _plNextPageToken
      (\ s a -> s{_plNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"drive#permissionList\".
plKind :: Lens' PermissionList Text
plKind = lens _plKind (\ s a -> s{_plKind = a})

-- | The list of permissions. If nextPageToken is populated, then this list
-- may be incomplete and an additional page of results should be fetched.
plPermissions :: Lens' PermissionList [Permission]
plPermissions
  = lens _plPermissions
      (\ s a -> s{_plPermissions = a})
      . _Default
      . _Coerce

instance FromJSON PermissionList where
        parseJSON
          = withObject "PermissionList"
              (\ o ->
                 PermissionList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "drive#permissionList")
                     <*> (o .:? "permissions" .!= mempty))

instance ToJSON PermissionList where
        toJSON PermissionList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _plNextPageToken,
                  Just ("kind" .= _plKind),
                  ("permissions" .=) <$> _plPermissions])
