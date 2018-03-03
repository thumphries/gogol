{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Drive.Permissions.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a permission for a file or Team Drive.
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference> for @drive.permissions.create@.
module Network.Google.Resource.Drive.Permissions.Create
    (
    -- * REST Resource
      PermissionsCreateResource

    -- * Creating a Request
    , permissionsCreate
    , PermissionsCreate

    -- * Request Lenses
    , pcSendNotificationEmail
    , pcPayload
    , pcEmailMessage
    , pcUseDomainAdminAccess
    , pcTransferOwnership
    , pcFileId
    , pcSupportsTeamDrives
    ) where

import Network.Google.Drive.Types
import Network.Google.Prelude

-- | A resource alias for @drive.permissions.create@ method which the
-- 'PermissionsCreate' request conforms to.
type PermissionsCreateResource =
     "drive" :>
       "v3" :>
         "files" :>
           Capture "fileId" Text :>
             "permissions" :>
               QueryParam "sendNotificationEmail" Bool :>
                 QueryParam "emailMessage" Text :>
                   QueryParam "useDomainAdminAccess" Bool :>
                     QueryParam "transferOwnership" Bool :>
                       QueryParam "supportsTeamDrives" Bool :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] Permission :> Post '[JSON] Permission

-- | Creates a permission for a file or Team Drive.
--
-- /See:/ 'permissionsCreate' smart constructor.
data PermissionsCreate = PermissionsCreate'
    { _pcSendNotificationEmail :: !(Maybe Bool)
    , _pcPayload :: !Permission
    , _pcEmailMessage :: !(Maybe Text)
    , _pcUseDomainAdminAccess :: !Bool
    , _pcTransferOwnership :: !Bool
    , _pcFileId :: !Text
    , _pcSupportsTeamDrives :: !Bool
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PermissionsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcSendNotificationEmail'
--
-- * 'pcPayload'
--
-- * 'pcEmailMessage'
--
-- * 'pcUseDomainAdminAccess'
--
-- * 'pcTransferOwnership'
--
-- * 'pcFileId'
--
-- * 'pcSupportsTeamDrives'
permissionsCreate
    :: Permission -- ^ 'pcPayload'
    -> Text -- ^ 'pcFileId'
    -> PermissionsCreate
permissionsCreate pPcPayload_ pPcFileId_ = 
    PermissionsCreate'
    { _pcSendNotificationEmail = Nothing
    , _pcPayload = pPcPayload_
    , _pcEmailMessage = Nothing
    , _pcUseDomainAdminAccess = False
    , _pcTransferOwnership = False
    , _pcFileId = pPcFileId_
    , _pcSupportsTeamDrives = False
    }

-- | Whether to send a notification email when sharing to users or groups.
-- This defaults to true for users and groups, and is not allowed for other
-- requests. It must not be disabled for ownership transfers.
pcSendNotificationEmail :: Lens' PermissionsCreate (Maybe Bool)
pcSendNotificationEmail
  = lens _pcSendNotificationEmail
      (\ s a -> s{_pcSendNotificationEmail = a})

-- | Multipart request metadata.
pcPayload :: Lens' PermissionsCreate Permission
pcPayload
  = lens _pcPayload (\ s a -> s{_pcPayload = a})

-- | A plain text custom message to include in the notification email.
pcEmailMessage :: Lens' PermissionsCreate (Maybe Text)
pcEmailMessage
  = lens _pcEmailMessage
      (\ s a -> s{_pcEmailMessage = a})

-- | Whether the request should be treated as if it was issued by a domain
-- administrator; if set to true, then the requester will be granted access
-- if they are an administrator of the domain to which the item belongs.
pcUseDomainAdminAccess :: Lens' PermissionsCreate Bool
pcUseDomainAdminAccess
  = lens _pcUseDomainAdminAccess
      (\ s a -> s{_pcUseDomainAdminAccess = a})

-- | Whether to transfer ownership to the specified user and downgrade the
-- current owner to a writer. This parameter is required as an
-- acknowledgement of the side effect.
pcTransferOwnership :: Lens' PermissionsCreate Bool
pcTransferOwnership
  = lens _pcTransferOwnership
      (\ s a -> s{_pcTransferOwnership = a})

-- | The ID of the file or Team Drive.
pcFileId :: Lens' PermissionsCreate Text
pcFileId = lens _pcFileId (\ s a -> s{_pcFileId = a})

-- | Whether the requesting application supports Team Drives.
pcSupportsTeamDrives :: Lens' PermissionsCreate Bool
pcSupportsTeamDrives
  = lens _pcSupportsTeamDrives
      (\ s a -> s{_pcSupportsTeamDrives = a})

instance GoogleRequest PermissionsCreate where
        type Rs PermissionsCreate = Permission
        type Scopes PermissionsCreate =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.file"]
        requestClient PermissionsCreate'{..}
          = go _pcFileId _pcSendNotificationEmail
              _pcEmailMessage
              (Just _pcUseDomainAdminAccess)
              (Just _pcTransferOwnership)
              (Just _pcSupportsTeamDrives)
              (Just AltJSON)
              _pcPayload
              driveService
          where go
                  = buildClient
                      (Proxy :: Proxy PermissionsCreateResource)
                      mempty
