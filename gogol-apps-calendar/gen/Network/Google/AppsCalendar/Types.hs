{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AppsCalendar.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AppsCalendar.Types
    (
    -- * Service Configuration
      appsCalendarService

    -- * OAuth Scopes
    , calendarScope
    , calendarReadOnlyScope

    -- * CalendarListEntry
    , CalendarListEntry
    , calendarListEntry
    , cleSummary
    , cleConferenceProperties
    , cleEtag
    , cleLocation
    , cleKind
    , cleNotificationSettings
    , cleBackgRoundColor
    , cleForegRoundColor
    , cleDefaultReminders
    , cleSelected
    , clePrimary
    , cleHidden
    , cleId
    , cleDeleted
    , cleAccessRole
    , cleSummaryOverride
    , cleColorId
    , cleTimeZone
    , cleDescription

    -- * ConferenceParameters
    , ConferenceParameters
    , conferenceParameters
    , cpAddOnParameters

    -- * Event
    , Event
    , event
    , eSummary
    , eOriginalStartTime
    , eCreator
    , eStatus
    , eGuestsCanModify
    , eEtag
    , eAttachments
    , eLocked
    , eLocation
    , eAttendees
    , eReminders
    , eKind
    , eCreated
    , eTransparency
    , eRecurringEventId
    , eStart
    , ePrivateCopy
    , eEndTimeUnspecified
    , eConferenceData
    , eExtendedProperties
    , eVisibility
    , eGuestsCanInviteOthers
    , eRecurrence
    , eGadget
    , eSequence
    , eICalUId
    , eEnd
    , eAttendeesOmitted
    , eSource
    , eId
    , eHTMLLink
    , eUpdated
    , eColorId
    , eAnyoneCanAddSelf
    , eGuestsCanSeeOtherGuests
    , eHangoutLink
    , eDescription
    , eOrganizer

    -- * CalendarListEntryNotificationSettings
    , CalendarListEntryNotificationSettings
    , calendarListEntryNotificationSettings
    , clensNotifications

    -- * ConferenceProperties
    , ConferenceProperties
    , conferenceProperties
    , cpAllowedConferenceSolutionTypes

    -- * ConferenceSolution
    , ConferenceSolution
    , conferenceSolution
    , csIconURI
    , csKey
    , csName

    -- * ACLRuleScope
    , ACLRuleScope
    , aclRuleScope
    , arsValue
    , arsType

    -- * ColorsEvent
    , ColorsEvent
    , colorsEvent
    , ceAddtional

    -- * Settings
    , Settings
    , settings
    , sEtag
    , sNextPageToken
    , sKind
    , sItems
    , sNextSyncToken

    -- * FreeBusyRequestItem
    , FreeBusyRequestItem
    , freeBusyRequestItem
    , fbriId

    -- * EventAttachment
    , EventAttachment
    , eventAttachment
    , eaFileURL
    , eaIconLink
    , eaMimeType
    , eaTitle
    , eaFileId

    -- * EntryPoint
    , EntryPoint
    , entryPoint
    , epPasscode
    , epURI
    , epMeetingCode
    , epPassword
    , epPin
    , epEntryPointType
    , epLabel
    , epAccessCode

    -- * TimePeriod
    , TimePeriod
    , timePeriod
    , tpStart
    , tpEnd

    -- * ConferenceSolutionKey
    , ConferenceSolutionKey
    , conferenceSolutionKey
    , cskType

    -- * EventCreator
    , EventCreator
    , eventCreator
    , ecEmail
    , ecSelf
    , ecDisplayName
    , ecId

    -- * Error'
    , Error'
    , error'
    , eDomain
    , eReason

    -- * ColorDefinition
    , ColorDefinition
    , colorDefinition
    , cdForegRound
    , cdBackgRound

    -- * EventsListOrderBy
    , EventsListOrderBy (..)

    -- * Channel
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

    -- * ConferenceRequestStatus
    , ConferenceRequestStatus
    , conferenceRequestStatus
    , crsStatusCode

    -- * FreeBusyCalendar
    , FreeBusyCalendar
    , freeBusyCalendar
    , fbcBusy
    , fbcErrors

    -- * ConferenceData
    , ConferenceData
    , conferenceData
    , cdSignature
    , cdConferenceSolution
    , cdCreateRequest
    , cdConferenceId
    , cdParameters
    , cdNotes
    , cdEntryPoints

    -- * Setting
    , Setting
    , setting
    , setEtag
    , setKind
    , setValue
    , setId

    -- * FreeBusyResponseGroups
    , FreeBusyResponseGroups
    , freeBusyResponseGroups
    , fbrgAddtional

    -- * EventReminders
    , EventReminders
    , eventReminders
    , erOverrides
    , erUseDefault

    -- * ColorsCalendar
    , ColorsCalendar
    , colorsCalendar
    , ccAddtional

    -- * ConferenceParametersAddOnParametersParameters
    , ConferenceParametersAddOnParametersParameters
    , conferenceParametersAddOnParametersParameters
    , cpaoppAddtional

    -- * CalendarNotification
    , CalendarNotification
    , calendarNotification
    , cnMethod
    , cnType

    -- * EventExtendedPropertiesPrivate
    , EventExtendedPropertiesPrivate
    , eventExtendedPropertiesPrivate
    , eeppAddtional

    -- * ChannelParams
    , ChannelParams
    , channelParams
    , cpAddtional

    -- * Events
    , Events
    , events
    , eveSummary
    , eveEtag
    , eveNextPageToken
    , eveKind
    , eveItems
    , eveDefaultReminders
    , eveUpdated
    , eveAccessRole
    , eveTimeZone
    , eveNextSyncToken
    , eveDescription

    -- * EventAttendee
    , EventAttendee
    , eventAttendee
    , eaEmail
    , eaResponseStatus
    , eaSelf
    , eaResource
    , eaAdditionalGuests
    , eaDisplayName
    , eaId
    , eaComment
    , eaOptional
    , eaOrganizer

    -- * Calendar
    , Calendar
    , calendar
    , calSummary
    , calConferenceProperties
    , calEtag
    , calLocation
    , calKind
    , calId
    , calTimeZone
    , calDescription

    -- * FreeBusyResponse
    , FreeBusyResponse
    , freeBusyResponse
    , fbrGroups
    , fbrTimeMin
    , fbrKind
    , fbrCalendars
    , fbrTimeMax

    -- * EventReminder
    , EventReminder
    , eventReminder
    , erMethod
    , erMinutes

    -- * EventExtendedProperties
    , EventExtendedProperties
    , eventExtendedProperties
    , eepPrivate
    , eepShared

    -- * EventDateTime
    , EventDateTime
    , eventDateTime
    , edtDate
    , edtTimeZone
    , edtDateTime

    -- * EventOrganizer
    , EventOrganizer
    , eventOrganizer
    , eoEmail
    , eoSelf
    , eoDisplayName
    , eoId

    -- * CalendarList
    , CalendarList
    , calendarList
    , clEtag
    , clNextPageToken
    , clKind
    , clItems
    , clNextSyncToken

    -- * CalendarListListMinAccessRole
    , CalendarListListMinAccessRole (..)

    -- * EventGadget
    , EventGadget
    , eventGadget
    , egHeight
    , egDisplay
    , egPreferences
    , egLink
    , egIconLink
    , egWidth
    , egTitle
    , egType

    -- * EventGadgetPreferences
    , EventGadgetPreferences
    , eventGadgetPreferences
    , egpAddtional

    -- * FreeBusyRequest
    , FreeBusyRequest
    , freeBusyRequest
    , fCalendarExpansionMax
    , fTimeMin
    , fItems
    , fGroupExpansionMax
    , fTimeZone
    , fTimeMax

    -- * ACLRule
    , ACLRule
    , aclRule
    , arEtag
    , arKind
    , arRole
    , arScope
    , arId

    -- * EventsWatchOrderBy
    , EventsWatchOrderBy (..)

    -- * CreateConferenceRequest
    , CreateConferenceRequest
    , createConferenceRequest
    , ccrStatus
    , ccrRequestId
    , ccrConferenceSolutionKey

    -- * EventExtendedPropertiesShared
    , EventExtendedPropertiesShared
    , eventExtendedPropertiesShared
    , eepsAddtional

    -- * CalendarListWatchMinAccessRole
    , CalendarListWatchMinAccessRole (..)

    -- * FreeBusyResponseCalendars
    , FreeBusyResponseCalendars
    , freeBusyResponseCalendars
    , fbrcAddtional

    -- * ACL
    , ACL
    , acl
    , aEtag
    , aNextPageToken
    , aKind
    , aItems
    , aNextSyncToken

    -- * Colors
    , Colors
    , colors
    , colEvent
    , colKind
    , colCalendar
    , colUpdated

    -- * FreeBusyGroup
    , FreeBusyGroup
    , freeBusyGroup
    , fbgCalendars
    , fbgErrors

    -- * ConferenceParametersAddOnParameters
    , ConferenceParametersAddOnParameters
    , conferenceParametersAddOnParameters
    , cpaopParameters

    -- * EventSource
    , EventSource
    , eventSource
    , esURL
    , esTitle
    ) where

import Network.Google.AppsCalendar.Types.Product
import Network.Google.AppsCalendar.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v3' of the Calendar API. This contains the host and root path used as a starting point for constructing service requests.
appsCalendarService :: ServiceConfig
appsCalendarService
  = defaultService (ServiceId "calendar:v3")
      "www.googleapis.com"

-- | Manage your calendars
calendarScope :: Proxy '["https://www.googleapis.com/auth/calendar"]
calendarScope = Proxy;

-- | View your calendars
calendarReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/calendar.readonly"]
calendarReadOnlyScope = Proxy;
