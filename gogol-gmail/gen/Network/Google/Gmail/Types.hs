{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Gmail.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Gmail.Types
    (
    -- * Service Configuration
      gmailService

    -- * OAuth Scopes
    , gmailSettingsBasicScope
    , mailGoogleComScope
    , gmailModifyScope
    , gmailMetadataScope
    , gmailLabelsScope
    , gmailSettingsSharingScope
    , gmailSendScope
    , gmailInsertScope
    , gmailComposeScope
    , gmailReadOnlyScope

    -- * BatchDeleteMessagesRequest
    , BatchDeleteMessagesRequest
    , batchDeleteMessagesRequest
    , bdmrIds

    -- * FilterCriteriaSizeComparison
    , FilterCriteriaSizeComparison (..)

    -- * UsersMessagesGetFormat
    , UsersMessagesGetFormat (..)

    -- * ModifyThreadRequest
    , ModifyThreadRequest
    , modifyThreadRequest
    , mtrRemoveLabelIds
    , mtrAddLabelIds

    -- * ListFiltersResponse
    , ListFiltersResponse
    , listFiltersResponse
    , lfrFilter

    -- * ModifyMessageRequest
    , ModifyMessageRequest
    , modifyMessageRequest
    , mmrRemoveLabelIds
    , mmrAddLabelIds

    -- * ListForwardingAddressesResponse
    , ListForwardingAddressesResponse
    , listForwardingAddressesResponse
    , lfarForwardingAddresses

    -- * PopSettings
    , PopSettings
    , popSettings
    , psAccessWindow
    , psDisPosition

    -- * PopSettingsAccessWindow
    , PopSettingsAccessWindow (..)

    -- * History
    , History
    , history
    , hLabelsRemoved
    , hMessagesDeleted
    , hMessagesAdded
    , hLabelsAdded
    , hId
    , hMessages

    -- * ForwardingAddressVerificationStatus
    , ForwardingAddressVerificationStatus (..)

    -- * LabelColor
    , LabelColor
    , labelColor
    , lcBackgRoundColor
    , lcTextColor

    -- * FilterCriteria
    , FilterCriteria
    , filterCriteria
    , fcSizeComparison
    , fcSubject
    , fcSize
    , fcExcludeChats
    , fcTo
    , fcFrom
    , fcQuery
    , fcNegatedQuery
    , fcHasAttachment

    -- * ProFile
    , ProFile
    , proFile
    , pfMessagesTotal
    , pfThreadsTotal
    , pfHistoryId
    , pfEmailAddress

    -- * AutoForwardingDisPosition
    , AutoForwardingDisPosition (..)

    -- * MessagePartHeader
    , MessagePartHeader
    , messagePartHeader
    , mphValue
    , mphName

    -- * UsersHistoryListHistoryTypes
    , UsersHistoryListHistoryTypes (..)

    -- * SendAsVerificationStatus
    , SendAsVerificationStatus (..)

    -- * ListHistoryResponse
    , ListHistoryResponse
    , listHistoryResponse
    , lhrNextPageToken
    , lhrHistory
    , lhrHistoryId

    -- * SendAs
    , SendAs
    , sendAs
    , saSignature
    , saReplyToAddress
    , saTreatAsAlias
    , saSendAsEmail
    , saDisplayName
    , saVerificationStatus
    , saSmtpMsa
    , saIsPrimary
    , saIsDefault

    -- * LabelType
    , LabelType (..)

    -- * UsersDraftsGetFormat
    , UsersDraftsGetFormat (..)

    -- * UsersMessagesImportInternalDateSource
    , UsersMessagesImportInternalDateSource (..)

    -- * LabelMessageListVisibility
    , LabelMessageListVisibility (..)

    -- * ListThreadsResponse
    , ListThreadsResponse
    , listThreadsResponse
    , ltrNextPageToken
    , ltrResultSizeEstimate
    , ltrThreads

    -- * MessagePart
    , MessagePart
    , messagePart
    , mpParts
    , mpBody
    , mpMimeType
    , mpHeaders
    , mpPartId
    , mpFilename

    -- * HistoryLabelAdded
    , HistoryLabelAdded
    , historyLabelAdded
    , hlaLabelIds
    , hlaMessage

    -- * ListLabelsResponse
    , ListLabelsResponse
    , listLabelsResponse
    , llrLabels

    -- * VacationSettings
    , VacationSettings
    , vacationSettings
    , vsEnableAutoReply
    , vsResponseBodyPlainText
    , vsRestrictToDomain
    , vsStartTime
    , vsResponseBodyHTML
    , vsRestrictToContacts
    , vsResponseSubject
    , vsEndTime

    -- * LabelLabelListVisibility
    , LabelLabelListVisibility (..)

    -- * HistoryMessageDeleted
    , HistoryMessageDeleted
    , historyMessageDeleted
    , hmdMessage

    -- * MessagePartBody
    , MessagePartBody
    , messagePartBody
    , mpbSize
    , mpbData
    , mpbAttachmentId

    -- * AutoForwarding
    , AutoForwarding
    , autoForwarding
    , afEnabled
    , afDisPosition
    , afEmailAddress

    -- * ListDraftsResponse
    , ListDraftsResponse
    , listDraftsResponse
    , ldrNextPageToken
    , ldrResultSizeEstimate
    , ldrDrafts

    -- * ListSendAsResponse
    , ListSendAsResponse
    , listSendAsResponse
    , lsarSendAs

    -- * WatchResponse
    , WatchResponse
    , watchResponse
    , wrExpiration
    , wrHistoryId

    -- * UsersThreadsGetFormat
    , UsersThreadsGetFormat (..)

    -- * BatchModifyMessagesRequest
    , BatchModifyMessagesRequest
    , batchModifyMessagesRequest
    , bmmrIds
    , bmmrRemoveLabelIds
    , bmmrAddLabelIds

    -- * Draft
    , Draft
    , draft
    , dId
    , dMessage

    -- * SmtpMsa
    , SmtpMsa
    , smtpMsa
    , smSecurityMode
    , smUsername
    , smPassword
    , smHost
    , smPort

    -- * ForwardingAddress
    , ForwardingAddress
    , forwardingAddress
    , faForwardingEmail
    , faVerificationStatus

    -- * PopSettingsDisPosition
    , PopSettingsDisPosition (..)

    -- * Filter
    , Filter
    , filter'
    , fAction
    , fId
    , fCriteria

    -- * WatchRequest
    , WatchRequest
    , watchRequest
    , wrLabelFilterAction
    , wrTopicName
    , wrLabelIds

    -- * WatchRequestLabelFilterAction
    , WatchRequestLabelFilterAction (..)

    -- * ImapSettings
    , ImapSettings
    , imapSettings
    , isEnabled
    , isExpungeBehavior
    , isAutoExpunge
    , isMaxFolderSize

    -- * ImapSettingsExpungeBehavior
    , ImapSettingsExpungeBehavior (..)

    -- * ListSmimeInfoResponse
    , ListSmimeInfoResponse
    , listSmimeInfoResponse
    , lsirSmimeInfo

    -- * SmtpMsaSecurityMode
    , SmtpMsaSecurityMode (..)

    -- * Message
    , Message
    , message
    , mRaw
    , mSnippet
    , mSizeEstimate
    , mPayload
    , mHistoryId
    , mId
    , mLabelIds
    , mThreadId
    , mInternalDate

    -- * UsersMessagesInsertInternalDateSource
    , UsersMessagesInsertInternalDateSource (..)

    -- * HistoryLabelRemoved
    , HistoryLabelRemoved
    , historyLabelRemoved
    , hlrLabelIds
    , hlrMessage

    -- * Thread
    , Thread
    , thread
    , tSnippet
    , tHistoryId
    , tId
    , tMessages

    -- * FilterAction
    , FilterAction
    , filterAction
    , faForward
    , faRemoveLabelIds
    , faAddLabelIds

    -- * Label
    , Label
    , label
    , lThreadsUnread
    , lMessageListVisibility
    , lMessagesTotal
    , lColor
    , lMessagesUnread
    , lName
    , lThreadsTotal
    , lLabelListVisibility
    , lId
    , lType

    -- * SmimeInfo
    , SmimeInfo
    , smimeInfo
    , siPem
    , siExpiration
    , siEncryptedKeyPassword
    , siId
    , siPkcs12
    , siIssuerCn
    , siIsDefault

    -- * ListMessagesResponse
    , ListMessagesResponse
    , listMessagesResponse
    , lmrNextPageToken
    , lmrResultSizeEstimate
    , lmrMessages

    -- * HistoryMessageAdded
    , HistoryMessageAdded
    , historyMessageAdded
    , hmaMessage
    ) where

import Network.Google.Gmail.Types.Product
import Network.Google.Gmail.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Gmail API. This contains the host and root path used as a starting point for constructing service requests.
gmailService :: ServiceConfig
gmailService
  = defaultService (ServiceId "gmail:v1")
      "www.googleapis.com"

-- | Manage your basic mail settings
gmailSettingsBasicScope :: Proxy '["https://www.googleapis.com/auth/gmail.settings.basic"]
gmailSettingsBasicScope = Proxy;

-- | Read, send, delete, and manage your email
mailGoogleComScope :: Proxy '["https://mail.google.com/"]
mailGoogleComScope = Proxy;

-- | View and modify but not delete your email
gmailModifyScope :: Proxy '["https://www.googleapis.com/auth/gmail.modify"]
gmailModifyScope = Proxy;

-- | View your email message metadata such as labels and headers, but not the
-- email body
gmailMetadataScope :: Proxy '["https://www.googleapis.com/auth/gmail.metadata"]
gmailMetadataScope = Proxy;

-- | Manage mailbox labels
gmailLabelsScope :: Proxy '["https://www.googleapis.com/auth/gmail.labels"]
gmailLabelsScope = Proxy;

-- | Manage your sensitive mail settings, including who can manage your mail
gmailSettingsSharingScope :: Proxy '["https://www.googleapis.com/auth/gmail.settings.sharing"]
gmailSettingsSharingScope = Proxy;

-- | Send email on your behalf
gmailSendScope :: Proxy '["https://www.googleapis.com/auth/gmail.send"]
gmailSendScope = Proxy;

-- | Insert mail into your mailbox
gmailInsertScope :: Proxy '["https://www.googleapis.com/auth/gmail.insert"]
gmailInsertScope = Proxy;

-- | Manage drafts and send emails
gmailComposeScope :: Proxy '["https://www.googleapis.com/auth/gmail.compose"]
gmailComposeScope = Proxy;

-- | View your email messages and settings
gmailReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/gmail.readonly"]
gmailReadOnlyScope = Proxy;
