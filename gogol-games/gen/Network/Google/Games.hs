{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Games
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The API for Google Play Game Services.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference>
module Network.Google.Games
    (
    -- * Service Configuration
      gamesService

    -- * OAuth Scopes
    , plusLoginScope
    , gamesScope
    , driveAppDataScope

    -- * API Declaration
    , GamesAPI

    -- * Resources

    -- ** games.achievementDefinitions.list
    , module Network.Google.Resource.Games.AchievementDefinitions.List

    -- ** games.achievements.increment
    , module Network.Google.Resource.Games.Achievements.Increment

    -- ** games.achievements.list
    , module Network.Google.Resource.Games.Achievements.List

    -- ** games.achievements.reveal
    , module Network.Google.Resource.Games.Achievements.Reveal

    -- ** games.achievements.setStepsAtLeast
    , module Network.Google.Resource.Games.Achievements.SetStepsAtLeast

    -- ** games.achievements.unlock
    , module Network.Google.Resource.Games.Achievements.Unlock

    -- ** games.achievements.updateMultiple
    , module Network.Google.Resource.Games.Achievements.UpdateMultiple

    -- ** games.applications.get
    , module Network.Google.Resource.Games.Applications.Get

    -- ** games.applications.played
    , module Network.Google.Resource.Games.Applications.Played

    -- ** games.applications.verify
    , module Network.Google.Resource.Games.Applications.Verify

    -- ** games.events.listByPlayer
    , module Network.Google.Resource.Games.Events.ListByPlayer

    -- ** games.events.listDefinitions
    , module Network.Google.Resource.Games.Events.ListDefinitions

    -- ** games.events.record
    , module Network.Google.Resource.Games.Events.Record

    -- ** games.leaderboards.get
    , module Network.Google.Resource.Games.Leaderboards.Get

    -- ** games.leaderboards.list
    , module Network.Google.Resource.Games.Leaderboards.List

    -- ** games.metagame.getMetagameConfig
    , module Network.Google.Resource.Games.Metagame.GetMetagameConfig

    -- ** games.metagame.listCategoriesByPlayer
    , module Network.Google.Resource.Games.Metagame.ListCategoriesByPlayer

    -- ** games.players.get
    , module Network.Google.Resource.Games.Players.Get

    -- ** games.players.list
    , module Network.Google.Resource.Games.Players.List

    -- ** games.pushtokens.remove
    , module Network.Google.Resource.Games.Pushtokens.Remove

    -- ** games.pushtokens.update
    , module Network.Google.Resource.Games.Pushtokens.Update

    -- ** games.questMilestones.claim
    , module Network.Google.Resource.Games.QuestMilestones.Claim

    -- ** games.quests.accept
    , module Network.Google.Resource.Games.Quests.Accept

    -- ** games.quests.list
    , module Network.Google.Resource.Games.Quests.List

    -- ** games.revisions.check
    , module Network.Google.Resource.Games.Revisions.Check

    -- ** games.rooms.create
    , module Network.Google.Resource.Games.Rooms.Create

    -- ** games.rooms.decline
    , module Network.Google.Resource.Games.Rooms.Decline

    -- ** games.rooms.dismiss
    , module Network.Google.Resource.Games.Rooms.Dismiss

    -- ** games.rooms.get
    , module Network.Google.Resource.Games.Rooms.Get

    -- ** games.rooms.join
    , module Network.Google.Resource.Games.Rooms.Join

    -- ** games.rooms.leave
    , module Network.Google.Resource.Games.Rooms.Leave

    -- ** games.rooms.list
    , module Network.Google.Resource.Games.Rooms.List

    -- ** games.rooms.reportStatus
    , module Network.Google.Resource.Games.Rooms.ReportStatus

    -- ** games.scores.get
    , module Network.Google.Resource.Games.Scores.Get

    -- ** games.scores.list
    , module Network.Google.Resource.Games.Scores.List

    -- ** games.scores.listWindow
    , module Network.Google.Resource.Games.Scores.ListWindow

    -- ** games.scores.submit
    , module Network.Google.Resource.Games.Scores.Submit

    -- ** games.scores.submitMultiple
    , module Network.Google.Resource.Games.Scores.SubmitMultiple

    -- ** games.snapshots.get
    , module Network.Google.Resource.Games.Snapshots.Get

    -- ** games.snapshots.list
    , module Network.Google.Resource.Games.Snapshots.List

    -- ** games.turnBasedMatches.cancel
    , module Network.Google.Resource.Games.TurnBasedMatches.Cancel

    -- ** games.turnBasedMatches.create
    , module Network.Google.Resource.Games.TurnBasedMatches.Create

    -- ** games.turnBasedMatches.decline
    , module Network.Google.Resource.Games.TurnBasedMatches.Decline

    -- ** games.turnBasedMatches.dismiss
    , module Network.Google.Resource.Games.TurnBasedMatches.Dismiss

    -- ** games.turnBasedMatches.finish
    , module Network.Google.Resource.Games.TurnBasedMatches.Finish

    -- ** games.turnBasedMatches.get
    , module Network.Google.Resource.Games.TurnBasedMatches.Get

    -- ** games.turnBasedMatches.join
    , module Network.Google.Resource.Games.TurnBasedMatches.Join

    -- ** games.turnBasedMatches.leave
    , module Network.Google.Resource.Games.TurnBasedMatches.Leave

    -- ** games.turnBasedMatches.leaveTurn
    , module Network.Google.Resource.Games.TurnBasedMatches.LeaveTurn

    -- ** games.turnBasedMatches.list
    , module Network.Google.Resource.Games.TurnBasedMatches.List

    -- ** games.turnBasedMatches.rematch
    , module Network.Google.Resource.Games.TurnBasedMatches.Rematch

    -- ** games.turnBasedMatches.sync
    , module Network.Google.Resource.Games.TurnBasedMatches.Sync

    -- ** games.turnBasedMatches.takeTurn
    , module Network.Google.Resource.Games.TurnBasedMatches.TakeTurn

    -- * Types

    -- ** PlayersListCollection
    , PlayersListCollection (..)

    -- ** RoomJoinRequest
    , RoomJoinRequest
    , roomJoinRequest
    , rjrNetworkDiagnostics
    , rjrKind
    , rjrClientAddress
    , rjrCapabilities

    -- ** PlayerName
    , PlayerName
    , playerName
    , pnGivenName
    , pnFamilyName

    -- ** Snapshot
    , Snapshot
    , snapshot
    , sLastModifiedMillis
    , sKind
    , sProgressValue
    , sUniqueName
    , sCoverImage
    , sId
    , sDurationMillis
    , sTitle
    , sType
    , sDescription
    , sDriveId

    -- ** Room
    , Room
    , room
    , rStatus
    , rVariant
    , rKind
    , rAutoMatchingStatus
    , rCreationDetails
    , rInviterId
    , rLastUpdateDetails
    , rRoomStatusVersion
    , rParticipants
    , rApplicationId
    , rAutoMatchingCriteria
    , rRoomId
    , rDescription

    -- ** QuestListResponse
    , QuestListResponse
    , questListResponse
    , qlrNextPageToken
    , qlrKind
    , qlrItems

    -- ** TurnBasedMatch
    , TurnBasedMatch
    , turnBasedMatch
    , tbmStatus
    , tbmVariant
    , tbmResults
    , tbmMatchNumber
    , tbmKind
    , tbmData
    , tbmWithParticipantId
    , tbmCreationDetails
    , tbmInviterId
    , tbmLastUpdateDetails
    , tbmParticipants
    , tbmApplicationId
    , tbmAutoMatchingCriteria
    , tbmPreviousMatchData
    , tbmPendingParticipantId
    , tbmUserMatchStatus
    , tbmMatchId
    , tbmDescription
    , tbmRematchId
    , tbmMatchVersion

    -- ** TurnBasedMatchData
    , TurnBasedMatchData
    , turnBasedMatchData
    , tbmdKind
    , tbmdData
    , tbmdDataAvailable

    -- ** ScoresListCollection
    , ScoresListCollection (..)

    -- ** PlayerEvent
    , PlayerEvent
    , playerEvent
    , peKind
    , peNumEvents
    , peFormattedNumEvents
    , peDefinitionId
    , pePlayerId

    -- ** PlayerLeaderboardScore
    , PlayerLeaderboardScore
    , playerLeaderboardScore
    , plsScoreTag
    , plsScoreString
    , plsKind
    , plsScoreValue
    , plsTimeSpan
    , plsPublicRank
    , plsSocialRank
    , plsLeaderboardId
    , plsWriteTimestamp

    -- ** Application
    , Application
    , application
    , aThemeColor
    , aLeaderboardCount
    , aKind
    , aCategory
    , aName
    , aEnabledFeatures
    , aInstances
    , aAuthor
    , aId
    , aAchievementCount
    , aAssets
    , aDescription
    , aLastUpdatedTimestamp

    -- ** ApplicationCategory
    , ApplicationCategory
    , applicationCategory
    , acSecondary
    , acKind
    , acPrimary

    -- ** PlayerScoreListResponse
    , PlayerScoreListResponse
    , playerScoreListResponse
    , pslrSubmittedScores
    , pslrKind

    -- ** NetworkDiagnostics
    , NetworkDiagnostics
    , networkDiagnostics
    , ndAndroidNetworkType
    , ndKind
    , ndNetworkOperatorCode
    , ndNetworkOperatorName
    , ndRegistrationLatencyMillis
    , ndIosNetworkType
    , ndAndroidNetworkSubtype

    -- ** TurnBasedMatchTurn
    , TurnBasedMatchTurn
    , turnBasedMatchTurn
    , tbmtResults
    , tbmtKind
    , tbmtData
    , tbmtPendingParticipantId
    , tbmtMatchVersion

    -- ** QuestCriterion
    , QuestCriterion
    , questCriterion
    , qcCurrentContribution
    , qcCompletionContribution
    , qcKind
    , qcInitialPlayerProgress
    , qcEventId

    -- ** TurnBasedMatchList
    , TurnBasedMatchList
    , turnBasedMatchList
    , tbmlNextPageToken
    , tbmlKind
    , tbmlItems

    -- ** PeerChannelDiagnostics
    , PeerChannelDiagnostics
    , peerChannelDiagnostics
    , pcdNumMessagesLost
    , pcdBytesSent
    , pcdKind
    , pcdRoundtripLatencyMillis
    , pcdBytesReceived
    , pcdNumMessagesReceived
    , pcdNumSendFailures
    , pcdNumMessagesSent

    -- ** RoomList
    , RoomList
    , roomList
    , rlNextPageToken
    , rlKind
    , rlItems

    -- ** PushToken
    , PushToken
    , pushToken
    , ptClientRevision
    , ptKind
    , ptLanguage
    , ptId

    -- ** AchievementUpdateResponse
    , AchievementUpdateResponse
    , achievementUpdateResponse
    , aurUpdateOccurred
    , aurAchievementId
    , aurKind
    , aurCurrentState
    , aurNewlyUnlocked
    , aurCurrentSteps

    -- ** LeaderboardEntry
    , LeaderboardEntry
    , leaderboardEntry
    , leScoreTag
    , leWriteTimestampMillis
    , leKind
    , leScoreValue
    , leFormattedScore
    , leTimeSpan
    , leFormattedScoreRank
    , lePlayer
    , leScoreRank

    -- ** SnapshotListResponse
    , SnapshotListResponse
    , snapshotListResponse
    , slrNextPageToken
    , slrKind
    , slrItems

    -- ** PlayerLevel
    , PlayerLevel
    , playerLevel
    , plMaxExperiencePoints
    , plKind
    , plMinExperiencePoints
    , plLevel

    -- ** AchievementUpdateMultipleResponse
    , AchievementUpdateMultipleResponse
    , achievementUpdateMultipleResponse
    , aumrKind
    , aumrUpdatedAchievements

    -- ** RoomParticipant
    , RoomParticipant
    , roomParticipant
    , rpStatus
    , rpConnected
    , rpLeaveReason
    , rpKind
    , rpClientAddress
    , rpId
    , rpAutoMatched
    , rpPlayer
    , rpCapabilities
    , rpAutoMatchedPlayer

    -- ** ApplicationsGetPlatformType
    , ApplicationsGetPlatformType (..)

    -- ** EventDefinitionListResponse
    , EventDefinitionListResponse
    , eventDefinitionListResponse
    , edlrNextPageToken
    , edlrKind
    , edlrItems

    -- ** Category
    , Category
    , category
    , cKind
    , cCategory
    , cExperiencePoints

    -- ** InstanceAndroidDetails
    , InstanceAndroidDetails
    , instanceAndroidDetails
    , iadPackageName
    , iadPreferred
    , iadKind
    , iadEnablePiracyCheck

    -- ** TurnBasedMatchParticipant
    , TurnBasedMatchParticipant
    , turnBasedMatchParticipant
    , tbmpStatus
    , tbmpKind
    , tbmpId
    , tbmpAutoMatched
    , tbmpPlayer
    , tbmpAutoMatchedPlayer

    -- ** AchievementDefinitionsListResponse
    , AchievementDefinitionsListResponse
    , achievementDefinitionsListResponse
    , adlrNextPageToken
    , adlrKind
    , adlrItems

    -- ** PlayerScoreResponse
    , PlayerScoreResponse
    , playerScoreResponse
    , psrScoreTag
    , psrKind
    , psrFormattedScore
    , psrLeaderboardId
    , psrBeatenScoreTimeSpans
    , psrUnbeatenScores

    -- ** AnonymousPlayer
    , AnonymousPlayer
    , anonymousPlayer
    , apAvatarImageURL
    , apKind
    , apDisplayName

    -- ** QuestContribution
    , QuestContribution
    , questContribution
    , qKind
    , qValue
    , qFormattedValue

    -- ** RoomClientAddress
    , RoomClientAddress
    , roomClientAddress
    , rcaKind
    , rcaXmppAddress

    -- ** LeaderboardListResponse
    , LeaderboardListResponse
    , leaderboardListResponse
    , llrNextPageToken
    , llrKind
    , llrItems

    -- ** PlayerScore
    , PlayerScore
    , playerScore
    , psScoreTag
    , psScore
    , psKind
    , psFormattedScore
    , psTimeSpan

    -- ** ScoresListWindowCollection
    , ScoresListWindowCollection (..)

    -- ** TurnBasedAutoMatchingCriteria
    , TurnBasedAutoMatchingCriteria
    , turnBasedAutoMatchingCriteria
    , tbamcKind
    , tbamcExclusiveBitmask
    , tbamcMaxAutoMatchingPlayers
    , tbamcMinAutoMatchingPlayers

    -- ** SnapshotImage
    , SnapshotImage
    , snapshotImage
    , siHeight
    , siKind
    , siURL
    , siMimeType
    , siWidth

    -- ** RoomStatus
    , RoomStatus
    , roomStatus
    , rsStatus
    , rsKind
    , rsAutoMatchingStatus
    , rsStatusVersion
    , rsParticipants
    , rsRoomId

    -- ** PlayerLeaderboardScoreListResponse
    , PlayerLeaderboardScoreListResponse
    , playerLeaderboardScoreListResponse
    , plslrNextPageToken
    , plslrKind
    , plslrItems
    , plslrPlayer

    -- ** InstanceIosDetails
    , InstanceIosDetails
    , instanceIosDetails
    , iidItunesAppId
    , iidPreferredForIPad
    , iidSupportIPhone
    , iidKind
    , iidSupportIPad
    , iidPreferredForIPhone
    , iidBundleIdentifier

    -- ** EventUpdateResponse
    , EventUpdateResponse
    , eventUpdateResponse
    , eurPlayerEvents
    , eurBatchFailures
    , eurEventFailures
    , eurKind

    -- ** RevisionCheckResponse
    , RevisionCheckResponse
    , revisionCheckResponse
    , rcrAPIVersion
    , rcrKind
    , rcrRevisionStatus

    -- ** ParticipantResult
    , ParticipantResult
    , participantResult
    , prParticipantId
    , prKind
    , prResult
    , prPlacing

    -- ** Leaderboard
    , Leaderboard
    , leaderboard
    , lKind
    , lIsIconURLDefault
    , lName
    , lId
    , lIconURL
    , lOrder

    -- ** MetagameConfig
    , MetagameConfig
    , metagameConfig
    , mcKind
    , mcCurrentVersion
    , mcPlayerLevels

    -- ** CategoryListResponse
    , CategoryListResponse
    , categoryListResponse
    , clrNextPageToken
    , clrKind
    , clrItems

    -- ** RoomP2PStatus
    , RoomP2PStatus
    , roomP2PStatus
    , rppsStatus
    , rppsParticipantId
    , rppsKind
    , rppsError
    , rppsErrorReason
    , rppsConnectionSetupLatencyMillis
    , rppsUnreliableRoundtripLatencyMillis

    -- ** TurnBasedMatchModification
    , TurnBasedMatchModification
    , turnBasedMatchModification
    , tbmmParticipantId
    , tbmmKind
    , tbmmModifiedTimestampMillis

    -- ** EventDefinition
    , EventDefinition
    , eventDefinition
    , edIsDefaultImageURL
    , edKind
    , edVisibility
    , edImageURL
    , edDisplayName
    , edId
    , edChildEvents
    , edDescription

    -- ** RoomModification
    , RoomModification
    , roomModification
    , rmParticipantId
    , rmKind
    , rmModifiedTimestampMillis

    -- ** ScoresListWindowTimeSpan
    , ScoresListWindowTimeSpan (..)

    -- ** EventUpdateRequest
    , EventUpdateRequest
    , eventUpdateRequest
    , eUpdateCount
    , eKind
    , eDefinitionId

    -- ** AchievementUnlockResponse
    , AchievementUnlockResponse
    , achievementUnlockResponse
    , achKind
    , achNewlyUnlocked

    -- ** ScoresGetTimeSpan
    , ScoresGetTimeSpan (..)

    -- ** PlayerAchievement
    , PlayerAchievement
    , playerAchievement
    , paKind
    , paAchievementState
    , paFormattedCurrentStepsString
    , paExperiencePoints
    , paId
    , paCurrentSteps
    , paLastUpdatedTimestamp

    -- ** RoomP2PStatuses
    , RoomP2PStatuses
    , roomP2PStatuses
    , rppssKind
    , rppssUpdates

    -- ** ImageAsset
    , ImageAsset
    , imageAsset
    , iaHeight
    , iaKind
    , iaURL
    , iaWidth
    , iaName

    -- ** AchievementUpdateMultipleRequest
    , AchievementUpdateMultipleRequest
    , achievementUpdateMultipleRequest
    , aumruKind
    , aumruUpdates

    -- ** RoomAutoMatchStatus
    , RoomAutoMatchStatus
    , roomAutoMatchStatus
    , ramsKind
    , ramsWaitEstimateSeconds

    -- ** AchievementUpdateRequest
    , AchievementUpdateRequest
    , achievementUpdateRequest
    , auruAchievementId
    , auruKind
    , auruUpdateType
    , auruSetStepsAtLeastPayload
    , auruIncrementPayload

    -- ** ScoresGetIncludeRankType
    , ScoresGetIncludeRankType (..)

    -- ** LeaderboardScoreRank
    , LeaderboardScoreRank
    , leaderboardScoreRank
    , lsrNumScores
    , lsrKind
    , lsrFormattedRank
    , lsrFormattedNumScores
    , lsrRank

    -- ** RoomCreateRequest
    , RoomCreateRequest
    , roomCreateRequest
    , rooRequestId
    , rooVariant
    , rooNetworkDiagnostics
    , rooKind
    , rooInvitedPlayerIds
    , rooClientAddress
    , rooAutoMatchingCriteria
    , rooCapabilities

    -- ** PlayerListResponse
    , PlayerListResponse
    , playerListResponse
    , plrNextPageToken
    , plrKind
    , plrItems

    -- ** LeaderboardScores
    , LeaderboardScores
    , leaderboardScores
    , lsNextPageToken
    , lsNumScores
    , lsKind
    , lsPlayerScore
    , lsItems
    , lsPrevPageToken

    -- ** AchievementDefinition
    , AchievementDefinition
    , achievementDefinition
    , adAchievementType
    , adFormattedTotalSteps
    , adRevealedIconURL
    , adKind
    , adExperiencePoints
    , adInitialState
    , adName
    , adId
    , adIsUnlockedIconURLDefault
    , adTotalSteps
    , adDescription
    , adIsRevealedIconURLDefault
    , adUnlockedIconURL

    -- ** TurnBasedMatchCreateRequest
    , TurnBasedMatchCreateRequest
    , turnBasedMatchCreateRequest
    , tbmcrRequestId
    , tbmcrVariant
    , tbmcrKind
    , tbmcrInvitedPlayerIds
    , tbmcrAutoMatchingCriteria

    -- ** EventBatchRecordFailure
    , EventBatchRecordFailure
    , eventBatchRecordFailure
    , ebrfKind
    , ebrfRange
    , ebrfFailureCause

    -- ** TurnBasedMatchResults
    , TurnBasedMatchResults
    , turnBasedMatchResults
    , tbmrResults
    , tbmrKind
    , tbmrData
    , tbmrMatchVersion

    -- ** PushTokenIdIos
    , PushTokenIdIos
    , pushTokenIdIos
    , ptiiAPNSDeviceToken
    , ptiiAPNSEnvironment

    -- ** RoomLeaveRequest
    , RoomLeaveRequest
    , roomLeaveRequest
    , rlrKind
    , rlrReason
    , rlrLeaveDiagnostics

    -- ** Played
    , Played
    , played
    , pKind
    , pAutoMatched
    , pTimeMillis

    -- ** AchievementIncrementResponse
    , AchievementIncrementResponse
    , achievementIncrementResponse
    , airKind
    , airNewlyUnlocked
    , airCurrentSteps

    -- ** AchievementRevealResponse
    , AchievementRevealResponse
    , achievementRevealResponse
    , arrKind
    , arrCurrentState

    -- ** AchievementSetStepsAtLeastResponse
    , AchievementSetStepsAtLeastResponse
    , achievementSetStepsAtLeastResponse
    , assalrKind
    , assalrNewlyUnlocked
    , assalrCurrentSteps

    -- ** PlayerAchievementListResponse
    , PlayerAchievementListResponse
    , playerAchievementListResponse
    , palrNextPageToken
    , palrKind
    , palrItems

    -- ** EventRecordRequest
    , EventRecordRequest
    , eventRecordRequest
    , errRequestId
    , errKind
    , errCurrentTimeMillis
    , errTimePeriods

    -- ** RoomAutoMatchingCriteria
    , RoomAutoMatchingCriteria
    , roomAutoMatchingCriteria
    , ramcKind
    , ramcExclusiveBitmask
    , ramcMaxAutoMatchingPlayers
    , ramcMinAutoMatchingPlayers

    -- ** ScoresListTimeSpan
    , ScoresListTimeSpan (..)

    -- ** QuestMilestone
    , QuestMilestone
    , questMilestone
    , qmState
    , qmKind
    , qmId
    , qmCompletionRewardData
    , qmCriteria

    -- ** PeerSessionDiagnostics
    , PeerSessionDiagnostics
    , peerSessionDiagnostics
    , psdConnectedTimestampMillis
    , psdParticipantId
    , psdKind
    , psdUnreliableChannel
    , psdReliableChannel

    -- ** PushTokenId
    , PushTokenId
    , pushTokenId
    , ptiIos
    , ptiKind

    -- ** EventPeriodUpdate
    , EventPeriodUpdate
    , eventPeriodUpdate
    , epuKind
    , epuTimePeriod
    , epuUpdates

    -- ** TurnBasedMatchSync
    , TurnBasedMatchSync
    , turnBasedMatchSync
    , tbmsMoreAvailable
    , tbmsNextPageToken
    , tbmsKind
    , tbmsItems

    -- ** ScoreSubmission
    , ScoreSubmission
    , scoreSubmission
    , scoSignature
    , scoScoreTag
    , scoScore
    , scoKind
    , scoLeaderboardId

    -- ** RoomLeaveDiagnostics
    , RoomLeaveDiagnostics
    , roomLeaveDiagnostics
    , rldPeerSession
    , rldAndroidNetworkType
    , rldKind
    , rldNetworkOperatorCode
    , rldNetworkOperatorName
    , rldSocketsUsed
    , rldIosNetworkType
    , rldAndroidNetworkSubtype

    -- ** AggregateStats
    , AggregateStats
    , aggregateStats
    , asMax
    , asKind
    , asCount
    , asMin
    , asSum

    -- ** InstanceWebDetails
    , InstanceWebDetails
    , instanceWebDetails
    , iwdPreferred
    , iwdKind
    , iwdLaunchURL

    -- ** TurnBasedMatchRematch
    , TurnBasedMatchRematch
    , turnBasedMatchRematch
    , tRematch
    , tKind
    , tPreviousMatch

    -- ** PlayerExperienceInfo
    , PlayerExperienceInfo
    , playerExperienceInfo
    , peiKind
    , peiCurrentExperiencePoints
    , peiCurrentLevel
    , peiNextLevel
    , peiLastLevelUpTimestampMillis

    -- ** GamesAchievementSetStepsAtLeast
    , GamesAchievementSetStepsAtLeast
    , gamesAchievementSetStepsAtLeast
    , gassalKind
    , gassalSteps

    -- ** Player
    , Player
    , player
    , plaBannerURLLandscape
    , plaLastPlayedWith
    , plaAvatarImageURL
    , plaKind
    , plaExperienceInfo
    , plaName
    , plaOriginalPlayerId
    , plaDisplayName
    , plaTitle
    , plaBannerURLPortrait
    , plaPlayerId
    , plaProFileSettings

    -- ** GamesAchievementIncrement
    , GamesAchievementIncrement
    , gamesAchievementIncrement
    , gaiRequestId
    , gaiKind
    , gaiSteps

    -- ** Quest
    , Quest
    , quest
    , queLastUpdatedTimestampMillis
    , queBannerURL
    , queState
    , queMilestones
    , queKind
    , queApplicationId
    , queEndTimestampMillis
    , queName
    , queId
    , queIconURL
    , queStartTimestampMillis
    , queNotifyTimestampMillis
    , queDescription
    , queIsDefaultBannerURL
    , queIsDefaultIconURL
    , queAcceptedTimestampMillis

    -- ** EventChild
    , EventChild
    , eventChild
    , ecKind
    , ecChildId

    -- ** ApplicationVerifyResponse
    , ApplicationVerifyResponse
    , applicationVerifyResponse
    , avrKind
    , avrAlternatePlayerId
    , avrPlayerId

    -- ** PlayerEventListResponse
    , PlayerEventListResponse
    , playerEventListResponse
    , pelrNextPageToken
    , pelrKind
    , pelrItems

    -- ** TurnBasedMatchDataRequest
    , TurnBasedMatchDataRequest
    , turnBasedMatchDataRequest
    , tbmdrKind
    , tbmdrData

    -- ** ProFileSettings
    , ProFileSettings
    , proFileSettings
    , pfsProFileVisible
    , pfsKind

    -- ** EventPeriodRange
    , EventPeriodRange
    , eventPeriodRange
    , eprKind
    , eprPeriodStartMillis
    , eprPeriodEndMillis

    -- ** MetagameListCategoriesByPlayerCollection
    , MetagameListCategoriesByPlayerCollection (..)

    -- ** AchievementsListState
    , AchievementsListState (..)

    -- ** EventRecordFailure
    , EventRecordFailure
    , eventRecordFailure
    , erfKind
    , erfFailureCause
    , erfEventId

    -- ** PlayerScoreSubmissionList
    , PlayerScoreSubmissionList
    , playerScoreSubmissionList
    , psslKind
    , psslScores

    -- ** Instance
    , Instance
    , instance'
    , iAndroidInstance
    , iKind
    , iWebInstance
    , iIosInstance
    , iName
    , iAcquisitionURI
    , iPlatformType
    , iTurnBasedPlay
    , iRealtimePlay
    ) where

import Network.Google.Prelude
import Network.Google.Games.Types
import Network.Google.Resource.Games.AchievementDefinitions.List
import Network.Google.Resource.Games.Achievements.Increment
import Network.Google.Resource.Games.Achievements.List
import Network.Google.Resource.Games.Achievements.Reveal
import Network.Google.Resource.Games.Achievements.SetStepsAtLeast
import Network.Google.Resource.Games.Achievements.Unlock
import Network.Google.Resource.Games.Achievements.UpdateMultiple
import Network.Google.Resource.Games.Applications.Get
import Network.Google.Resource.Games.Applications.Played
import Network.Google.Resource.Games.Applications.Verify
import Network.Google.Resource.Games.Events.ListByPlayer
import Network.Google.Resource.Games.Events.ListDefinitions
import Network.Google.Resource.Games.Events.Record
import Network.Google.Resource.Games.Leaderboards.Get
import Network.Google.Resource.Games.Leaderboards.List
import Network.Google.Resource.Games.Metagame.GetMetagameConfig
import Network.Google.Resource.Games.Metagame.ListCategoriesByPlayer
import Network.Google.Resource.Games.Players.Get
import Network.Google.Resource.Games.Players.List
import Network.Google.Resource.Games.Pushtokens.Remove
import Network.Google.Resource.Games.Pushtokens.Update
import Network.Google.Resource.Games.QuestMilestones.Claim
import Network.Google.Resource.Games.Quests.Accept
import Network.Google.Resource.Games.Quests.List
import Network.Google.Resource.Games.Revisions.Check
import Network.Google.Resource.Games.Rooms.Create
import Network.Google.Resource.Games.Rooms.Decline
import Network.Google.Resource.Games.Rooms.Dismiss
import Network.Google.Resource.Games.Rooms.Get
import Network.Google.Resource.Games.Rooms.Join
import Network.Google.Resource.Games.Rooms.Leave
import Network.Google.Resource.Games.Rooms.List
import Network.Google.Resource.Games.Rooms.ReportStatus
import Network.Google.Resource.Games.Scores.Get
import Network.Google.Resource.Games.Scores.List
import Network.Google.Resource.Games.Scores.ListWindow
import Network.Google.Resource.Games.Scores.Submit
import Network.Google.Resource.Games.Scores.SubmitMultiple
import Network.Google.Resource.Games.Snapshots.Get
import Network.Google.Resource.Games.Snapshots.List
import Network.Google.Resource.Games.TurnBasedMatches.Cancel
import Network.Google.Resource.Games.TurnBasedMatches.Create
import Network.Google.Resource.Games.TurnBasedMatches.Decline
import Network.Google.Resource.Games.TurnBasedMatches.Dismiss
import Network.Google.Resource.Games.TurnBasedMatches.Finish
import Network.Google.Resource.Games.TurnBasedMatches.Get
import Network.Google.Resource.Games.TurnBasedMatches.Join
import Network.Google.Resource.Games.TurnBasedMatches.Leave
import Network.Google.Resource.Games.TurnBasedMatches.LeaveTurn
import Network.Google.Resource.Games.TurnBasedMatches.List
import Network.Google.Resource.Games.TurnBasedMatches.Rematch
import Network.Google.Resource.Games.TurnBasedMatches.Sync
import Network.Google.Resource.Games.TurnBasedMatches.TakeTurn

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Play Game Services API service.
type GamesAPI =
     RoomsListResource :<|> RoomsJoinResource :<|>
       RoomsGetResource
       :<|> RoomsCreateResource
       :<|> RoomsDeclineResource
       :<|> RoomsDismissResource
       :<|> RoomsReportStatusResource
       :<|> RoomsLeaveResource
       :<|> LeaderboardsListResource
       :<|> LeaderboardsGetResource
       :<|> MetagameListCategoriesByPlayerResource
       :<|> MetagameGetMetagameConfigResource
       :<|> QuestMilestonesClaimResource
       :<|> AchievementDefinitionsListResource
       :<|> AchievementsListResource
       :<|> AchievementsUnlockResource
       :<|> AchievementsRevealResource
       :<|> AchievementsSetStepsAtLeastResource
       :<|> AchievementsUpdateMultipleResource
       :<|> AchievementsIncrementResource
       :<|> SnapshotsListResource
       :<|> SnapshotsGetResource
       :<|> EventsListDefinitionsResource
       :<|> EventsListByPlayerResource
       :<|> EventsRecordResource
       :<|> QuestsListResource
       :<|> QuestsAcceptResource
       :<|> PlayersListResource
       :<|> PlayersGetResource
       :<|> RevisionsCheckResource
       :<|> ScoresListResource
       :<|> ScoresGetResource
       :<|> ScoresSubmitResource
       :<|> ScoresSubmitMultipleResource
       :<|> ScoresListWindowResource
       :<|> PushtokensRemoveResource
       :<|> PushtokensUpdateResource
       :<|> TurnBasedMatchesRematchResource
       :<|> TurnBasedMatchesListResource
       :<|> TurnBasedMatchesJoinResource
       :<|> TurnBasedMatchesFinishResource
       :<|> TurnBasedMatchesTakeTurnResource
       :<|> TurnBasedMatchesLeaveTurnResource
       :<|> TurnBasedMatchesGetResource
       :<|> TurnBasedMatchesCreateResource
       :<|> TurnBasedMatchesDeclineResource
       :<|> TurnBasedMatchesSyncResource
       :<|> TurnBasedMatchesDismissResource
       :<|> TurnBasedMatchesLeaveResource
       :<|> TurnBasedMatchesCancelResource
       :<|> ApplicationsVerifyResource
       :<|> ApplicationsGetResource
       :<|> ApplicationsPlayedResource
