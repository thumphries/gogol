{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.UserAccounts.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.UserAccounts.Types.Product where

import Network.Google.Prelude
import Network.Google.UserAccounts.Types.Sum

--
-- /See:/ 'operationWarningsItemDataItem' smart constructor.
data OperationWarningsItemDataItem = OperationWarningsItemDataItem'
    { _owidiValue :: !(Maybe Text)
    , _owidiKey :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationWarningsItemDataItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'owidiValue'
--
-- * 'owidiKey'
operationWarningsItemDataItem
    :: OperationWarningsItemDataItem
operationWarningsItemDataItem = 
    OperationWarningsItemDataItem'
    { _owidiValue = Nothing
    , _owidiKey = Nothing
    }

-- | [Output Only] A warning data value corresponding to the key.
owidiValue :: Lens' OperationWarningsItemDataItem (Maybe Text)
owidiValue
  = lens _owidiValue (\ s a -> s{_owidiValue = a})

-- | [Output Only] A key that provides more detail on the warning being
-- returned. For example, for warnings where there are no results in a list
-- request for a particular zone, this key might be scope and the key value
-- might be the zone name. Other examples might be a key indicating a
-- deprecated resource and a suggested replacement, or a warning about
-- invalid network settings (for example, if an instance attempts to
-- perform IP forwarding but is not enabled for IP forwarding).
owidiKey :: Lens' OperationWarningsItemDataItem (Maybe Text)
owidiKey = lens _owidiKey (\ s a -> s{_owidiKey = a})

instance FromJSON OperationWarningsItemDataItem where
        parseJSON
          = withObject "OperationWarningsItemDataItem"
              (\ o ->
                 OperationWarningsItemDataItem' <$>
                   (o .:? "value") <*> (o .:? "key"))

instance ToJSON OperationWarningsItemDataItem where
        toJSON OperationWarningsItemDataItem'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _owidiValue,
                  ("key" .=) <$> _owidiKey])

-- | Contains a list of Operation resources.
--
-- /See:/ 'operationList' smart constructor.
data OperationList = OperationList'
    { _olNextPageToken :: !(Maybe Text)
    , _olKind :: !Text
    , _olItems :: !(Maybe [Operation])
    , _olSelfLink :: !(Maybe Text)
    , _olId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olNextPageToken'
--
-- * 'olKind'
--
-- * 'olItems'
--
-- * 'olSelfLink'
--
-- * 'olId'
operationList
    :: OperationList
operationList = 
    OperationList'
    { _olNextPageToken = Nothing
    , _olKind = "clouduseraccounts#operationList"
    , _olItems = Nothing
    , _olSelfLink = Nothing
    , _olId = Nothing
    }

-- | [Output Only] This token allows you to get the next page of results for
-- list requests. If the number of results is larger than maxResults, use
-- the nextPageToken as a value for the query parameter pageToken in the
-- next list request. Subsequent list requests will have their own
-- nextPageToken to continue paging through the results.
olNextPageToken :: Lens' OperationList (Maybe Text)
olNextPageToken
  = lens _olNextPageToken
      (\ s a -> s{_olNextPageToken = a})

-- | [Output Only] Type of resource. Always compute#operations for Operations
-- resource.
olKind :: Lens' OperationList Text
olKind = lens _olKind (\ s a -> s{_olKind = a})

-- | [Output Only] A list of Operation resources.
olItems :: Lens' OperationList [Operation]
olItems
  = lens _olItems (\ s a -> s{_olItems = a}) . _Default
      . _Coerce

-- | [Output Only] Server-defined URL for this resource.
olSelfLink :: Lens' OperationList (Maybe Text)
olSelfLink
  = lens _olSelfLink (\ s a -> s{_olSelfLink = a})

-- | [Output Only] The unique identifier for the resource. This identifier is
-- defined by the server.
olId :: Lens' OperationList (Maybe Text)
olId = lens _olId (\ s a -> s{_olId = a})

instance FromJSON OperationList where
        parseJSON
          = withObject "OperationList"
              (\ o ->
                 OperationList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "clouduseraccounts#operationList")
                     <*> (o .:? "items" .!= mempty)
                     <*> (o .:? "selfLink")
                     <*> (o .:? "id"))

instance ToJSON OperationList where
        toJSON OperationList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _olNextPageToken,
                  Just ("kind" .= _olKind), ("items" .=) <$> _olItems,
                  ("selfLink" .=) <$> _olSelfLink,
                  ("id" .=) <$> _olId])

-- | A Group resource.
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
    { _gKind :: !Text
    , _gMembers :: !(Maybe [Text])
    , _gSelfLink :: !(Maybe Text)
    , _gName :: !(Maybe Text)
    , _gCreationTimestamp :: !(Maybe Text)
    , _gId :: !(Maybe (Textual Word64))
    , _gDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gKind'
--
-- * 'gMembers'
--
-- * 'gSelfLink'
--
-- * 'gName'
--
-- * 'gCreationTimestamp'
--
-- * 'gId'
--
-- * 'gDescription'
group'
    :: Group
group' = 
    Group'
    { _gKind = "clouduseraccounts#group"
    , _gMembers = Nothing
    , _gSelfLink = Nothing
    , _gName = Nothing
    , _gCreationTimestamp = Nothing
    , _gId = Nothing
    , _gDescription = Nothing
    }

-- | [Output Only] Type of the resource. Always clouduseraccounts#group for
-- groups.
gKind :: Lens' Group Text
gKind = lens _gKind (\ s a -> s{_gKind = a})

-- | [Output Only] A list of URLs to User resources who belong to the group.
-- Users may only be members of groups in the same project.
gMembers :: Lens' Group [Text]
gMembers
  = lens _gMembers (\ s a -> s{_gMembers = a}) .
      _Default
      . _Coerce

-- | [Output Only] Server defined URL for the resource.
gSelfLink :: Lens' Group (Maybe Text)
gSelfLink
  = lens _gSelfLink (\ s a -> s{_gSelfLink = a})

-- | Name of the resource; provided by the client when the resource is
-- created.
gName :: Lens' Group (Maybe Text)
gName = lens _gName (\ s a -> s{_gName = a})

-- | [Output Only] Creation timestamp in RFC3339 text format.
gCreationTimestamp :: Lens' Group (Maybe Text)
gCreationTimestamp
  = lens _gCreationTimestamp
      (\ s a -> s{_gCreationTimestamp = a})

-- | [Output Only] Unique identifier for the resource; defined by the server.
gId :: Lens' Group (Maybe Word64)
gId
  = lens _gId (\ s a -> s{_gId = a}) . mapping _Coerce

-- | An optional textual description of the resource; provided by the client
-- when the resource is created.
gDescription :: Lens' Group (Maybe Text)
gDescription
  = lens _gDescription (\ s a -> s{_gDescription = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ o ->
                 Group' <$>
                   (o .:? "kind" .!= "clouduseraccounts#group") <*>
                     (o .:? "members" .!= mempty)
                     <*> (o .:? "selfLink")
                     <*> (o .:? "name")
                     <*> (o .:? "creationTimestamp")
                     <*> (o .:? "id")
                     <*> (o .:? "description"))

instance ToJSON Group where
        toJSON Group'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _gKind),
                  ("members" .=) <$> _gMembers,
                  ("selfLink" .=) <$> _gSelfLink,
                  ("name" .=) <$> _gName,
                  ("creationTimestamp" .=) <$> _gCreationTimestamp,
                  ("id" .=) <$> _gId,
                  ("description" .=) <$> _gDescription])

--
-- /See:/ 'groupList' smart constructor.
data GroupList = GroupList'
    { _glNextPageToken :: !(Maybe Text)
    , _glKind :: !Text
    , _glItems :: !(Maybe [Group])
    , _glSelfLink :: !(Maybe Text)
    , _glId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glNextPageToken'
--
-- * 'glKind'
--
-- * 'glItems'
--
-- * 'glSelfLink'
--
-- * 'glId'
groupList
    :: GroupList
groupList = 
    GroupList'
    { _glNextPageToken = Nothing
    , _glKind = "clouduseraccounts#groupList"
    , _glItems = Nothing
    , _glSelfLink = Nothing
    , _glId = Nothing
    }

-- | [Output Only] A token used to continue a truncated list request.
glNextPageToken :: Lens' GroupList (Maybe Text)
glNextPageToken
  = lens _glNextPageToken
      (\ s a -> s{_glNextPageToken = a})

-- | [Output Only] Type of resource. Always clouduseraccounts#groupList for
-- lists of groups.
glKind :: Lens' GroupList Text
glKind = lens _glKind (\ s a -> s{_glKind = a})

-- | [Output Only] A list of Group resources.
glItems :: Lens' GroupList [Group]
glItems
  = lens _glItems (\ s a -> s{_glItems = a}) . _Default
      . _Coerce

-- | [Output Only] Server defined URL for this resource.
glSelfLink :: Lens' GroupList (Maybe Text)
glSelfLink
  = lens _glSelfLink (\ s a -> s{_glSelfLink = a})

-- | [Output Only] Unique identifier for the resource; defined by the server.
glId :: Lens' GroupList (Maybe Text)
glId = lens _glId (\ s a -> s{_glId = a})

instance FromJSON GroupList where
        parseJSON
          = withObject "GroupList"
              (\ o ->
                 GroupList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "clouduseraccounts#groupList")
                     <*> (o .:? "items" .!= mempty)
                     <*> (o .:? "selfLink")
                     <*> (o .:? "id"))

instance ToJSON GroupList where
        toJSON GroupList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _glNextPageToken,
                  Just ("kind" .= _glKind), ("items" .=) <$> _glItems,
                  ("selfLink" .=) <$> _glSelfLink,
                  ("id" .=) <$> _glId])

-- | An Operation resource, used to manage asynchronous API requests.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oTargetId :: !(Maybe (Textual Word64))
    , _oStatus :: !(Maybe OperationStatus)
    , _oInsertTime :: !(Maybe Text)
    , _oProgress :: !(Maybe (Textual Int32))
    , _oStartTime :: !(Maybe Text)
    , _oKind :: !Text
    , _oError :: !(Maybe OperationError)
    , _oHTTPErrorMessage :: !(Maybe Text)
    , _oZone :: !(Maybe Text)
    , _oWarnings :: !(Maybe [OperationWarningsItem])
    , _oHTTPErrorStatusCode :: !(Maybe (Textual Int32))
    , _oUser :: !(Maybe Text)
    , _oSelfLink :: !(Maybe Text)
    , _oName :: !(Maybe Text)
    , _oStatusMessage :: !(Maybe Text)
    , _oCreationTimestamp :: !(Maybe Text)
    , _oEndTime :: !(Maybe Text)
    , _oId :: !(Maybe (Textual Word64))
    , _oOperationType :: !(Maybe Text)
    , _oRegion :: !(Maybe Text)
    , _oDescription :: !(Maybe Text)
    , _oTargetLink :: !(Maybe Text)
    , _oClientOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oTargetId'
--
-- * 'oStatus'
--
-- * 'oInsertTime'
--
-- * 'oProgress'
--
-- * 'oStartTime'
--
-- * 'oKind'
--
-- * 'oError'
--
-- * 'oHTTPErrorMessage'
--
-- * 'oZone'
--
-- * 'oWarnings'
--
-- * 'oHTTPErrorStatusCode'
--
-- * 'oUser'
--
-- * 'oSelfLink'
--
-- * 'oName'
--
-- * 'oStatusMessage'
--
-- * 'oCreationTimestamp'
--
-- * 'oEndTime'
--
-- * 'oId'
--
-- * 'oOperationType'
--
-- * 'oRegion'
--
-- * 'oDescription'
--
-- * 'oTargetLink'
--
-- * 'oClientOperationId'
operation
    :: Operation
operation = 
    Operation'
    { _oTargetId = Nothing
    , _oStatus = Nothing
    , _oInsertTime = Nothing
    , _oProgress = Nothing
    , _oStartTime = Nothing
    , _oKind = "clouduseraccounts#operation"
    , _oError = Nothing
    , _oHTTPErrorMessage = Nothing
    , _oZone = Nothing
    , _oWarnings = Nothing
    , _oHTTPErrorStatusCode = Nothing
    , _oUser = Nothing
    , _oSelfLink = Nothing
    , _oName = Nothing
    , _oStatusMessage = Nothing
    , _oCreationTimestamp = Nothing
    , _oEndTime = Nothing
    , _oId = Nothing
    , _oOperationType = Nothing
    , _oRegion = Nothing
    , _oDescription = Nothing
    , _oTargetLink = Nothing
    , _oClientOperationId = Nothing
    }

-- | [Output Only] The unique target ID, which identifies a specific
-- incarnation of the target resource.
oTargetId :: Lens' Operation (Maybe Word64)
oTargetId
  = lens _oTargetId (\ s a -> s{_oTargetId = a}) .
      mapping _Coerce

-- | [Output Only] The status of the operation, which can be one of the
-- following: PENDING, RUNNING, or DONE.
oStatus :: Lens' Operation (Maybe OperationStatus)
oStatus = lens _oStatus (\ s a -> s{_oStatus = a})

-- | [Output Only] The time that this operation was requested. This value is
-- in RFC3339 text format.
oInsertTime :: Lens' Operation (Maybe Text)
oInsertTime
  = lens _oInsertTime (\ s a -> s{_oInsertTime = a})

-- | [Output Only] An optional progress indicator that ranges from 0 to 100.
-- There is no requirement that this be linear or support any granularity
-- of operations. This should not be used to guess when the operation will
-- be complete. This number should monotonically increase as the operation
-- progresses.
oProgress :: Lens' Operation (Maybe Int32)
oProgress
  = lens _oProgress (\ s a -> s{_oProgress = a}) .
      mapping _Coerce

-- | [Output Only] The time that this operation was started by the server.
-- This value is in RFC3339 text format.
oStartTime :: Lens' Operation (Maybe Text)
oStartTime
  = lens _oStartTime (\ s a -> s{_oStartTime = a})

-- | [Output Only] Type of the resource. Always compute#operation for
-- Operation resources.
oKind :: Lens' Operation Text
oKind = lens _oKind (\ s a -> s{_oKind = a})

-- | [Output Only] If errors are generated during processing of the
-- operation, this field will be populated.
oError :: Lens' Operation (Maybe OperationError)
oError = lens _oError (\ s a -> s{_oError = a})

-- | [Output Only] If the operation fails, this field contains the HTTP error
-- message that was returned, such as NOT FOUND.
oHTTPErrorMessage :: Lens' Operation (Maybe Text)
oHTTPErrorMessage
  = lens _oHTTPErrorMessage
      (\ s a -> s{_oHTTPErrorMessage = a})

-- | [Output Only] The URL of the zone where the operation resides. Only
-- available when performing per-zone operations.
oZone :: Lens' Operation (Maybe Text)
oZone = lens _oZone (\ s a -> s{_oZone = a})

-- | [Output Only] If warning messages are generated during processing of the
-- operation, this field will be populated.
oWarnings :: Lens' Operation [OperationWarningsItem]
oWarnings
  = lens _oWarnings (\ s a -> s{_oWarnings = a}) .
      _Default
      . _Coerce

-- | [Output Only] If the operation fails, this field contains the HTTP error
-- status code that was returned. For example, a 404 means the resource was
-- not found.
oHTTPErrorStatusCode :: Lens' Operation (Maybe Int32)
oHTTPErrorStatusCode
  = lens _oHTTPErrorStatusCode
      (\ s a -> s{_oHTTPErrorStatusCode = a})
      . mapping _Coerce

-- | [Output Only] User who requested the operation, for example:
-- user\'example.com.
oUser :: Lens' Operation (Maybe Text)
oUser = lens _oUser (\ s a -> s{_oUser = a})

-- | [Output Only] Server-defined URL for the resource.
oSelfLink :: Lens' Operation (Maybe Text)
oSelfLink
  = lens _oSelfLink (\ s a -> s{_oSelfLink = a})

-- | [Output Only] Name of the resource.
oName :: Lens' Operation (Maybe Text)
oName = lens _oName (\ s a -> s{_oName = a})

-- | [Output Only] An optional textual description of the current status of
-- the operation.
oStatusMessage :: Lens' Operation (Maybe Text)
oStatusMessage
  = lens _oStatusMessage
      (\ s a -> s{_oStatusMessage = a})

-- | [Output Only] Creation timestamp in RFC3339 text format.
oCreationTimestamp :: Lens' Operation (Maybe Text)
oCreationTimestamp
  = lens _oCreationTimestamp
      (\ s a -> s{_oCreationTimestamp = a})

-- | [Output Only] The time that this operation was completed. This value is
-- in RFC3339 text format.
oEndTime :: Lens' Operation (Maybe Text)
oEndTime = lens _oEndTime (\ s a -> s{_oEndTime = a})

-- | [Output Only] The unique identifier for the resource. This identifier is
-- defined by the server.
oId :: Lens' Operation (Maybe Word64)
oId
  = lens _oId (\ s a -> s{_oId = a}) . mapping _Coerce

-- | [Output Only] The type of operation, such as insert, update, or delete,
-- and so on.
oOperationType :: Lens' Operation (Maybe Text)
oOperationType
  = lens _oOperationType
      (\ s a -> s{_oOperationType = a})

-- | [Output Only] The URL of the region where the operation resides. Only
-- available when performing regional operations.
oRegion :: Lens' Operation (Maybe Text)
oRegion = lens _oRegion (\ s a -> s{_oRegion = a})

-- | [Output Only] A textual description of the operation, which is set when
-- the operation is created.
oDescription :: Lens' Operation (Maybe Text)
oDescription
  = lens _oDescription (\ s a -> s{_oDescription = a})

-- | [Output Only] The URL of the resource that the operation modifies.
oTargetLink :: Lens' Operation (Maybe Text)
oTargetLink
  = lens _oTargetLink (\ s a -> s{_oTargetLink = a})

-- | [Output Only] Reserved for future use.
oClientOperationId :: Lens' Operation (Maybe Text)
oClientOperationId
  = lens _oClientOperationId
      (\ s a -> s{_oClientOperationId = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "targetId") <*> (o .:? "status") <*>
                     (o .:? "insertTime")
                     <*> (o .:? "progress")
                     <*> (o .:? "startTime")
                     <*> (o .:? "kind" .!= "clouduseraccounts#operation")
                     <*> (o .:? "error")
                     <*> (o .:? "httpErrorMessage")
                     <*> (o .:? "zone")
                     <*> (o .:? "warnings" .!= mempty)
                     <*> (o .:? "httpErrorStatusCode")
                     <*> (o .:? "user")
                     <*> (o .:? "selfLink")
                     <*> (o .:? "name")
                     <*> (o .:? "statusMessage")
                     <*> (o .:? "creationTimestamp")
                     <*> (o .:? "endTime")
                     <*> (o .:? "id")
                     <*> (o .:? "operationType")
                     <*> (o .:? "region")
                     <*> (o .:? "description")
                     <*> (o .:? "targetLink")
                     <*> (o .:? "clientOperationId"))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("targetId" .=) <$> _oTargetId,
                  ("status" .=) <$> _oStatus,
                  ("insertTime" .=) <$> _oInsertTime,
                  ("progress" .=) <$> _oProgress,
                  ("startTime" .=) <$> _oStartTime,
                  Just ("kind" .= _oKind), ("error" .=) <$> _oError,
                  ("httpErrorMessage" .=) <$> _oHTTPErrorMessage,
                  ("zone" .=) <$> _oZone,
                  ("warnings" .=) <$> _oWarnings,
                  ("httpErrorStatusCode" .=) <$> _oHTTPErrorStatusCode,
                  ("user" .=) <$> _oUser,
                  ("selfLink" .=) <$> _oSelfLink,
                  ("name" .=) <$> _oName,
                  ("statusMessage" .=) <$> _oStatusMessage,
                  ("creationTimestamp" .=) <$> _oCreationTimestamp,
                  ("endTime" .=) <$> _oEndTime, ("id" .=) <$> _oId,
                  ("operationType" .=) <$> _oOperationType,
                  ("region" .=) <$> _oRegion,
                  ("description" .=) <$> _oDescription,
                  ("targetLink" .=) <$> _oTargetLink,
                  ("clientOperationId" .=) <$> _oClientOperationId])

--
-- /See:/ 'userList' smart constructor.
data UserList = UserList'
    { _ulNextPageToken :: !(Maybe Text)
    , _ulKind :: !Text
    , _ulItems :: !(Maybe [User])
    , _ulSelfLink :: !(Maybe Text)
    , _ulId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulNextPageToken'
--
-- * 'ulKind'
--
-- * 'ulItems'
--
-- * 'ulSelfLink'
--
-- * 'ulId'
userList
    :: UserList
userList = 
    UserList'
    { _ulNextPageToken = Nothing
    , _ulKind = "clouduseraccounts#userList"
    , _ulItems = Nothing
    , _ulSelfLink = Nothing
    , _ulId = Nothing
    }

-- | [Output Only] A token used to continue a truncated list request.
ulNextPageToken :: Lens' UserList (Maybe Text)
ulNextPageToken
  = lens _ulNextPageToken
      (\ s a -> s{_ulNextPageToken = a})

-- | [Output Only] Type of resource. Always clouduseraccounts#userList for
-- lists of users.
ulKind :: Lens' UserList Text
ulKind = lens _ulKind (\ s a -> s{_ulKind = a})

-- | [Output Only] A list of User resources.
ulItems :: Lens' UserList [User]
ulItems
  = lens _ulItems (\ s a -> s{_ulItems = a}) . _Default
      . _Coerce

-- | [Output Only] Server defined URL for this resource.
ulSelfLink :: Lens' UserList (Maybe Text)
ulSelfLink
  = lens _ulSelfLink (\ s a -> s{_ulSelfLink = a})

-- | [Output Only] Unique identifier for the resource; defined by the server.
ulId :: Lens' UserList (Maybe Text)
ulId = lens _ulId (\ s a -> s{_ulId = a})

instance FromJSON UserList where
        parseJSON
          = withObject "UserList"
              (\ o ->
                 UserList' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "clouduseraccounts#userList")
                     <*> (o .:? "items" .!= mempty)
                     <*> (o .:? "selfLink")
                     <*> (o .:? "id"))

instance ToJSON UserList where
        toJSON UserList'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _ulNextPageToken,
                  Just ("kind" .= _ulKind), ("items" .=) <$> _ulItems,
                  ("selfLink" .=) <$> _ulSelfLink,
                  ("id" .=) <$> _ulId])

-- | A public key for authenticating to guests.
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
    { _pkFingerprint :: !(Maybe Text)
    , _pkKey :: !(Maybe Text)
    , _pkCreationTimestamp :: !(Maybe Text)
    , _pkExpirationTimestamp :: !(Maybe Text)
    , _pkDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkFingerprint'
--
-- * 'pkKey'
--
-- * 'pkCreationTimestamp'
--
-- * 'pkExpirationTimestamp'
--
-- * 'pkDescription'
publicKey
    :: PublicKey
publicKey = 
    PublicKey'
    { _pkFingerprint = Nothing
    , _pkKey = Nothing
    , _pkCreationTimestamp = Nothing
    , _pkExpirationTimestamp = Nothing
    , _pkDescription = Nothing
    }

-- | [Output Only] The fingerprint of the key is defined by RFC4716 to be the
-- MD5 digest of the public key.
pkFingerprint :: Lens' PublicKey (Maybe Text)
pkFingerprint
  = lens _pkFingerprint
      (\ s a -> s{_pkFingerprint = a})

-- | Public key text in SSH format, defined by RFC4253 section 6.6.
pkKey :: Lens' PublicKey (Maybe Text)
pkKey = lens _pkKey (\ s a -> s{_pkKey = a})

-- | [Output Only] Creation timestamp in RFC3339 text format.
pkCreationTimestamp :: Lens' PublicKey (Maybe Text)
pkCreationTimestamp
  = lens _pkCreationTimestamp
      (\ s a -> s{_pkCreationTimestamp = a})

-- | Optional expiration timestamp. If provided, the timestamp must be in
-- RFC3339 text format. If not provided, the public key never expires.
pkExpirationTimestamp :: Lens' PublicKey (Maybe Text)
pkExpirationTimestamp
  = lens _pkExpirationTimestamp
      (\ s a -> s{_pkExpirationTimestamp = a})

-- | An optional textual description of the resource; provided by the client
-- when the resource is created.
pkDescription :: Lens' PublicKey (Maybe Text)
pkDescription
  = lens _pkDescription
      (\ s a -> s{_pkDescription = a})

instance FromJSON PublicKey where
        parseJSON
          = withObject "PublicKey"
              (\ o ->
                 PublicKey' <$>
                   (o .:? "fingerprint") <*> (o .:? "key") <*>
                     (o .:? "creationTimestamp")
                     <*> (o .:? "expirationTimestamp")
                     <*> (o .:? "description"))

instance ToJSON PublicKey where
        toJSON PublicKey'{..}
          = object
              (catMaybes
                 [("fingerprint" .=) <$> _pkFingerprint,
                  ("key" .=) <$> _pkKey,
                  ("creationTimestamp" .=) <$> _pkCreationTimestamp,
                  ("expirationTimestamp" .=) <$>
                    _pkExpirationTimestamp,
                  ("description" .=) <$> _pkDescription])

-- | A list of all Linux accounts for this project. This API is only used by
-- Compute Engine virtual machines to get information about user accounts
-- for a project or instance. Linux resources are read-only views into
-- users and groups managed by the Compute Engine Accounts API.
--
-- /See:/ 'linuxAccountViews' smart constructor.
data LinuxAccountViews = LinuxAccountViews'
    { _lavUserViews :: !(Maybe [LinuxUserView])
    , _lavKind :: !Text
    , _lavGroupViews :: !(Maybe [LinuxGroupView])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinuxAccountViews' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavUserViews'
--
-- * 'lavKind'
--
-- * 'lavGroupViews'
linuxAccountViews
    :: LinuxAccountViews
linuxAccountViews = 
    LinuxAccountViews'
    { _lavUserViews = Nothing
    , _lavKind = "clouduseraccounts#linuxAccountViews"
    , _lavGroupViews = Nothing
    }

-- | [Output Only] A list of all users within a project.
lavUserViews :: Lens' LinuxAccountViews [LinuxUserView]
lavUserViews
  = lens _lavUserViews (\ s a -> s{_lavUserViews = a})
      . _Default
      . _Coerce

-- | [Output Only] Type of the resource. Always
-- clouduseraccounts#linuxAccountViews for Linux resources.
lavKind :: Lens' LinuxAccountViews Text
lavKind = lens _lavKind (\ s a -> s{_lavKind = a})

-- | [Output Only] A list of all groups within a project.
lavGroupViews :: Lens' LinuxAccountViews [LinuxGroupView]
lavGroupViews
  = lens _lavGroupViews
      (\ s a -> s{_lavGroupViews = a})
      . _Default
      . _Coerce

instance FromJSON LinuxAccountViews where
        parseJSON
          = withObject "LinuxAccountViews"
              (\ o ->
                 LinuxAccountViews' <$>
                   (o .:? "userViews" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "clouduseraccounts#linuxAccountViews")
                     <*> (o .:? "groupViews" .!= mempty))

instance ToJSON LinuxAccountViews where
        toJSON LinuxAccountViews'{..}
          = object
              (catMaybes
                 [("userViews" .=) <$> _lavUserViews,
                  Just ("kind" .= _lavKind),
                  ("groupViews" .=) <$> _lavGroupViews])

-- | A User resource.
--
-- /See:/ 'user' smart constructor.
data User = User'
    { _uGroups :: !(Maybe [Text])
    , _uPublicKeys :: !(Maybe [PublicKey])
    , _uKind :: !Text
    , _uOwner :: !(Maybe Text)
    , _uSelfLink :: !(Maybe Text)
    , _uName :: !(Maybe Text)
    , _uCreationTimestamp :: !(Maybe Text)
    , _uId :: !(Maybe (Textual Word64))
    , _uDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uGroups'
--
-- * 'uPublicKeys'
--
-- * 'uKind'
--
-- * 'uOwner'
--
-- * 'uSelfLink'
--
-- * 'uName'
--
-- * 'uCreationTimestamp'
--
-- * 'uId'
--
-- * 'uDescription'
user
    :: User
user = 
    User'
    { _uGroups = Nothing
    , _uPublicKeys = Nothing
    , _uKind = "clouduseraccounts#user"
    , _uOwner = Nothing
    , _uSelfLink = Nothing
    , _uName = Nothing
    , _uCreationTimestamp = Nothing
    , _uId = Nothing
    , _uDescription = Nothing
    }

-- | [Output Only] A list of URLs to Group resources who contain the user.
-- Users are only members of groups in the same project.
uGroups :: Lens' User [Text]
uGroups
  = lens _uGroups (\ s a -> s{_uGroups = a}) . _Default
      . _Coerce

-- | [Output Only] Public keys that this user may use to login.
uPublicKeys :: Lens' User [PublicKey]
uPublicKeys
  = lens _uPublicKeys (\ s a -> s{_uPublicKeys = a}) .
      _Default
      . _Coerce

-- | [Output Only] Type of the resource. Always clouduseraccounts#user for
-- users.
uKind :: Lens' User Text
uKind = lens _uKind (\ s a -> s{_uKind = a})

-- | Email address of account\'s owner. This account will be validated to
-- make sure it exists. The email can belong to any domain, but it must be
-- tied to a Google account.
uOwner :: Lens' User (Maybe Text)
uOwner = lens _uOwner (\ s a -> s{_uOwner = a})

-- | [Output Only] Server defined URL for the resource.
uSelfLink :: Lens' User (Maybe Text)
uSelfLink
  = lens _uSelfLink (\ s a -> s{_uSelfLink = a})

-- | Name of the resource; provided by the client when the resource is
-- created.
uName :: Lens' User (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a})

-- | [Output Only] Creation timestamp in RFC3339 text format.
uCreationTimestamp :: Lens' User (Maybe Text)
uCreationTimestamp
  = lens _uCreationTimestamp
      (\ s a -> s{_uCreationTimestamp = a})

-- | [Output Only] Unique identifier for the resource; defined by the server.
uId :: Lens' User (Maybe Word64)
uId
  = lens _uId (\ s a -> s{_uId = a}) . mapping _Coerce

-- | An optional textual description of the resource; provided by the client
-- when the resource is created.
uDescription :: Lens' User (Maybe Text)
uDescription
  = lens _uDescription (\ s a -> s{_uDescription = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ o ->
                 User' <$>
                   (o .:? "groups" .!= mempty) <*>
                     (o .:? "publicKeys" .!= mempty)
                     <*> (o .:? "kind" .!= "clouduseraccounts#user")
                     <*> (o .:? "owner")
                     <*> (o .:? "selfLink")
                     <*> (o .:? "name")
                     <*> (o .:? "creationTimestamp")
                     <*> (o .:? "id")
                     <*> (o .:? "description"))

instance ToJSON User where
        toJSON User'{..}
          = object
              (catMaybes
                 [("groups" .=) <$> _uGroups,
                  ("publicKeys" .=) <$> _uPublicKeys,
                  Just ("kind" .= _uKind), ("owner" .=) <$> _uOwner,
                  ("selfLink" .=) <$> _uSelfLink,
                  ("name" .=) <$> _uName,
                  ("creationTimestamp" .=) <$> _uCreationTimestamp,
                  ("id" .=) <$> _uId,
                  ("description" .=) <$> _uDescription])

--
-- /See:/ 'groupsAddMemberRequest' smart constructor.
newtype GroupsAddMemberRequest = GroupsAddMemberRequest'
    { _gamrUsers :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupsAddMemberRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gamrUsers'
groupsAddMemberRequest
    :: GroupsAddMemberRequest
groupsAddMemberRequest = 
    GroupsAddMemberRequest'
    { _gamrUsers = Nothing
    }

-- | Fully-qualified URLs of the User resources to add.
gamrUsers :: Lens' GroupsAddMemberRequest [Text]
gamrUsers
  = lens _gamrUsers (\ s a -> s{_gamrUsers = a}) .
      _Default
      . _Coerce

instance FromJSON GroupsAddMemberRequest where
        parseJSON
          = withObject "GroupsAddMemberRequest"
              (\ o ->
                 GroupsAddMemberRequest' <$>
                   (o .:? "users" .!= mempty))

instance ToJSON GroupsAddMemberRequest where
        toJSON GroupsAddMemberRequest'{..}
          = object (catMaybes [("users" .=) <$> _gamrUsers])

-- | A detailed view of a Linux group.
--
-- /See:/ 'linuxGroupView' smart constructor.
data LinuxGroupView = LinuxGroupView'
    { _lgvMembers :: !(Maybe [Text])
    , _lgvGid :: !(Maybe (Textual Word32))
    , _lgvGroupName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinuxGroupView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgvMembers'
--
-- * 'lgvGid'
--
-- * 'lgvGroupName'
linuxGroupView
    :: LinuxGroupView
linuxGroupView = 
    LinuxGroupView'
    { _lgvMembers = Nothing
    , _lgvGid = Nothing
    , _lgvGroupName = Nothing
    }

-- | [Output Only] List of user accounts that belong to the group.
lgvMembers :: Lens' LinuxGroupView [Text]
lgvMembers
  = lens _lgvMembers (\ s a -> s{_lgvMembers = a}) .
      _Default
      . _Coerce

-- | [Output Only] The Group ID.
lgvGid :: Lens' LinuxGroupView (Maybe Word32)
lgvGid
  = lens _lgvGid (\ s a -> s{_lgvGid = a}) .
      mapping _Coerce

-- | [Output Only] Group name.
lgvGroupName :: Lens' LinuxGroupView (Maybe Text)
lgvGroupName
  = lens _lgvGroupName (\ s a -> s{_lgvGroupName = a})

instance FromJSON LinuxGroupView where
        parseJSON
          = withObject "LinuxGroupView"
              (\ o ->
                 LinuxGroupView' <$>
                   (o .:? "members" .!= mempty) <*> (o .:? "gid") <*>
                     (o .:? "groupName"))

instance ToJSON LinuxGroupView where
        toJSON LinuxGroupView'{..}
          = object
              (catMaybes
                 [("members" .=) <$> _lgvMembers,
                  ("gid" .=) <$> _lgvGid,
                  ("groupName" .=) <$> _lgvGroupName])

-- | A detailed view of a Linux user account.
--
-- /See:/ 'linuxUserView' smart constructor.
data LinuxUserView = LinuxUserView'
    { _luvGecos :: !(Maybe Text)
    , _luvUid :: !(Maybe (Textual Word32))
    , _luvUsername :: !(Maybe Text)
    , _luvShell :: !(Maybe Text)
    , _luvGid :: !(Maybe (Textual Word32))
    , _luvHomeDirectory :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinuxUserView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luvGecos'
--
-- * 'luvUid'
--
-- * 'luvUsername'
--
-- * 'luvShell'
--
-- * 'luvGid'
--
-- * 'luvHomeDirectory'
linuxUserView
    :: LinuxUserView
linuxUserView = 
    LinuxUserView'
    { _luvGecos = Nothing
    , _luvUid = Nothing
    , _luvUsername = Nothing
    , _luvShell = Nothing
    , _luvGid = Nothing
    , _luvHomeDirectory = Nothing
    }

-- | [Output Only] The GECOS (user information) entry for this account.
luvGecos :: Lens' LinuxUserView (Maybe Text)
luvGecos = lens _luvGecos (\ s a -> s{_luvGecos = a})

-- | [Output Only] User ID.
luvUid :: Lens' LinuxUserView (Maybe Word32)
luvUid
  = lens _luvUid (\ s a -> s{_luvUid = a}) .
      mapping _Coerce

-- | [Output Only] The username of the account.
luvUsername :: Lens' LinuxUserView (Maybe Text)
luvUsername
  = lens _luvUsername (\ s a -> s{_luvUsername = a})

-- | [Output Only] The path to the login shell for this account.
luvShell :: Lens' LinuxUserView (Maybe Text)
luvShell = lens _luvShell (\ s a -> s{_luvShell = a})

-- | [Output Only] User\'s default group ID.
luvGid :: Lens' LinuxUserView (Maybe Word32)
luvGid
  = lens _luvGid (\ s a -> s{_luvGid = a}) .
      mapping _Coerce

-- | [Output Only] The path to the home directory for this account.
luvHomeDirectory :: Lens' LinuxUserView (Maybe Text)
luvHomeDirectory
  = lens _luvHomeDirectory
      (\ s a -> s{_luvHomeDirectory = a})

instance FromJSON LinuxUserView where
        parseJSON
          = withObject "LinuxUserView"
              (\ o ->
                 LinuxUserView' <$>
                   (o .:? "gecos") <*> (o .:? "uid") <*>
                     (o .:? "username")
                     <*> (o .:? "shell")
                     <*> (o .:? "gid")
                     <*> (o .:? "homeDirectory"))

instance ToJSON LinuxUserView where
        toJSON LinuxUserView'{..}
          = object
              (catMaybes
                 [("gecos" .=) <$> _luvGecos, ("uid" .=) <$> _luvUid,
                  ("username" .=) <$> _luvUsername,
                  ("shell" .=) <$> _luvShell, ("gid" .=) <$> _luvGid,
                  ("homeDirectory" .=) <$> _luvHomeDirectory])

--
-- /See:/ 'linuxGetLinuxAccountViewsResponse' smart constructor.
newtype LinuxGetLinuxAccountViewsResponse = LinuxGetLinuxAccountViewsResponse'
    { _lglavrResource :: Maybe LinuxAccountViews
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinuxGetLinuxAccountViewsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lglavrResource'
linuxGetLinuxAccountViewsResponse
    :: LinuxGetLinuxAccountViewsResponse
linuxGetLinuxAccountViewsResponse = 
    LinuxGetLinuxAccountViewsResponse'
    { _lglavrResource = Nothing
    }

-- | [Output Only] A list of authorized user accounts and groups.
lglavrResource :: Lens' LinuxGetLinuxAccountViewsResponse (Maybe LinuxAccountViews)
lglavrResource
  = lens _lglavrResource
      (\ s a -> s{_lglavrResource = a})

instance FromJSON LinuxGetLinuxAccountViewsResponse
         where
        parseJSON
          = withObject "LinuxGetLinuxAccountViewsResponse"
              (\ o ->
                 LinuxGetLinuxAccountViewsResponse' <$>
                   (o .:? "resource"))

instance ToJSON LinuxGetLinuxAccountViewsResponse
         where
        toJSON LinuxGetLinuxAccountViewsResponse'{..}
          = object
              (catMaybes [("resource" .=) <$> _lglavrResource])

-- | [Output Only] If errors are generated during processing of the
-- operation, this field will be populated.
--
-- /See:/ 'operationError' smart constructor.
newtype OperationError = OperationError'
    { _oeErrors :: Maybe [OperationErrorErrorsItem]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeErrors'
operationError
    :: OperationError
operationError = 
    OperationError'
    { _oeErrors = Nothing
    }

-- | [Output Only] The array of errors encountered while processing this
-- operation.
oeErrors :: Lens' OperationError [OperationErrorErrorsItem]
oeErrors
  = lens _oeErrors (\ s a -> s{_oeErrors = a}) .
      _Default
      . _Coerce

instance FromJSON OperationError where
        parseJSON
          = withObject "OperationError"
              (\ o ->
                 OperationError' <$> (o .:? "errors" .!= mempty))

instance ToJSON OperationError where
        toJSON OperationError'{..}
          = object (catMaybes [("errors" .=) <$> _oeErrors])

--
-- /See:/ 'linuxGetAuthorizedKeysViewResponse' smart constructor.
newtype LinuxGetAuthorizedKeysViewResponse = LinuxGetAuthorizedKeysViewResponse'
    { _lgakvrResource :: Maybe AuthorizedKeysView
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinuxGetAuthorizedKeysViewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgakvrResource'
linuxGetAuthorizedKeysViewResponse
    :: LinuxGetAuthorizedKeysViewResponse
linuxGetAuthorizedKeysViewResponse = 
    LinuxGetAuthorizedKeysViewResponse'
    { _lgakvrResource = Nothing
    }

-- | [Output Only] A list of authorized public keys for a user.
lgakvrResource :: Lens' LinuxGetAuthorizedKeysViewResponse (Maybe AuthorizedKeysView)
lgakvrResource
  = lens _lgakvrResource
      (\ s a -> s{_lgakvrResource = a})

instance FromJSON LinuxGetAuthorizedKeysViewResponse
         where
        parseJSON
          = withObject "LinuxGetAuthorizedKeysViewResponse"
              (\ o ->
                 LinuxGetAuthorizedKeysViewResponse' <$>
                   (o .:? "resource"))

instance ToJSON LinuxGetAuthorizedKeysViewResponse
         where
        toJSON LinuxGetAuthorizedKeysViewResponse'{..}
          = object
              (catMaybes [("resource" .=) <$> _lgakvrResource])

--
-- /See:/ 'operationErrorErrorsItem' smart constructor.
data OperationErrorErrorsItem = OperationErrorErrorsItem'
    { _oeeiLocation :: !(Maybe Text)
    , _oeeiCode :: !(Maybe Text)
    , _oeeiMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationErrorErrorsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeeiLocation'
--
-- * 'oeeiCode'
--
-- * 'oeeiMessage'
operationErrorErrorsItem
    :: OperationErrorErrorsItem
operationErrorErrorsItem = 
    OperationErrorErrorsItem'
    { _oeeiLocation = Nothing
    , _oeeiCode = Nothing
    , _oeeiMessage = Nothing
    }

-- | [Output Only] Indicates the field in the request that caused the error.
-- This property is optional.
oeeiLocation :: Lens' OperationErrorErrorsItem (Maybe Text)
oeeiLocation
  = lens _oeeiLocation (\ s a -> s{_oeeiLocation = a})

-- | [Output Only] The error type identifier for this error.
oeeiCode :: Lens' OperationErrorErrorsItem (Maybe Text)
oeeiCode = lens _oeeiCode (\ s a -> s{_oeeiCode = a})

-- | [Output Only] An optional, human-readable error message.
oeeiMessage :: Lens' OperationErrorErrorsItem (Maybe Text)
oeeiMessage
  = lens _oeeiMessage (\ s a -> s{_oeeiMessage = a})

instance FromJSON OperationErrorErrorsItem where
        parseJSON
          = withObject "OperationErrorErrorsItem"
              (\ o ->
                 OperationErrorErrorsItem' <$>
                   (o .:? "location") <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON OperationErrorErrorsItem where
        toJSON OperationErrorErrorsItem'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _oeeiLocation,
                  ("code" .=) <$> _oeeiCode,
                  ("message" .=) <$> _oeeiMessage])

--
-- /See:/ 'groupsRemoveMemberRequest' smart constructor.
newtype GroupsRemoveMemberRequest = GroupsRemoveMemberRequest'
    { _grmrUsers :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupsRemoveMemberRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grmrUsers'
groupsRemoveMemberRequest
    :: GroupsRemoveMemberRequest
groupsRemoveMemberRequest = 
    GroupsRemoveMemberRequest'
    { _grmrUsers = Nothing
    }

-- | Fully-qualified URLs of the User resources to remove.
grmrUsers :: Lens' GroupsRemoveMemberRequest [Text]
grmrUsers
  = lens _grmrUsers (\ s a -> s{_grmrUsers = a}) .
      _Default
      . _Coerce

instance FromJSON GroupsRemoveMemberRequest where
        parseJSON
          = withObject "GroupsRemoveMemberRequest"
              (\ o ->
                 GroupsRemoveMemberRequest' <$>
                   (o .:? "users" .!= mempty))

instance ToJSON GroupsRemoveMemberRequest where
        toJSON GroupsRemoveMemberRequest'{..}
          = object (catMaybes [("users" .=) <$> _grmrUsers])

-- | A list of authorized public keys for a user account.
--
-- /See:/ 'authorizedKeysView' smart constructor.
data AuthorizedKeysView = AuthorizedKeysView'
    { _akvSudoer :: !(Maybe Bool)
    , _akvKeys :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthorizedKeysView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akvSudoer'
--
-- * 'akvKeys'
authorizedKeysView
    :: AuthorizedKeysView
authorizedKeysView = 
    AuthorizedKeysView'
    { _akvSudoer = Nothing
    , _akvKeys = Nothing
    }

-- | [Output Only] Whether the user has the ability to elevate on the
-- instance that requested the authorized keys.
akvSudoer :: Lens' AuthorizedKeysView (Maybe Bool)
akvSudoer
  = lens _akvSudoer (\ s a -> s{_akvSudoer = a})

-- | [Output Only] The list of authorized public keys in SSH format.
akvKeys :: Lens' AuthorizedKeysView [Text]
akvKeys
  = lens _akvKeys (\ s a -> s{_akvKeys = a}) . _Default
      . _Coerce

instance FromJSON AuthorizedKeysView where
        parseJSON
          = withObject "AuthorizedKeysView"
              (\ o ->
                 AuthorizedKeysView' <$>
                   (o .:? "sudoer") <*> (o .:? "keys" .!= mempty))

instance ToJSON AuthorizedKeysView where
        toJSON AuthorizedKeysView'{..}
          = object
              (catMaybes
                 [("sudoer" .=) <$> _akvSudoer,
                  ("keys" .=) <$> _akvKeys])

--
-- /See:/ 'operationWarningsItem' smart constructor.
data OperationWarningsItem = OperationWarningsItem'
    { _owiData :: !(Maybe [OperationWarningsItemDataItem])
    , _owiCode :: !(Maybe OperationWarningsItemCode)
    , _owiMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationWarningsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'owiData'
--
-- * 'owiCode'
--
-- * 'owiMessage'
operationWarningsItem
    :: OperationWarningsItem
operationWarningsItem = 
    OperationWarningsItem'
    { _owiData = Nothing
    , _owiCode = Nothing
    , _owiMessage = Nothing
    }

-- | [Output Only] Metadata about this warning in key: value format. For
-- example: \"data\": [ { \"key\": \"scope\", \"value\":
-- \"zones\/us-east1-d\" }
owiData :: Lens' OperationWarningsItem [OperationWarningsItemDataItem]
owiData
  = lens _owiData (\ s a -> s{_owiData = a}) . _Default
      . _Coerce

-- | [Output Only] A warning code, if applicable. For example, Compute Engine
-- returns NO_RESULTS_ON_PAGE if there are no results in the response.
owiCode :: Lens' OperationWarningsItem (Maybe OperationWarningsItemCode)
owiCode = lens _owiCode (\ s a -> s{_owiCode = a})

-- | [Output Only] A human-readable description of the warning code.
owiMessage :: Lens' OperationWarningsItem (Maybe Text)
owiMessage
  = lens _owiMessage (\ s a -> s{_owiMessage = a})

instance FromJSON OperationWarningsItem where
        parseJSON
          = withObject "OperationWarningsItem"
              (\ o ->
                 OperationWarningsItem' <$>
                   (o .:? "data" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON OperationWarningsItem where
        toJSON OperationWarningsItem'{..}
          = object
              (catMaybes
                 [("data" .=) <$> _owiData, ("code" .=) <$> _owiCode,
                  ("message" .=) <$> _owiMessage])
