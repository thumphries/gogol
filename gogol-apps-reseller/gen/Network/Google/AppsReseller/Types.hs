{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AppsReseller.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AppsReseller.Types
    (
    -- * Service Configuration
      appsResellerService

    -- * OAuth Scopes
    , appsOrderReadOnlyScope
    , appsOrderScope

    -- * SubscriptionTrialSettings
    , SubscriptionTrialSettings
    , subscriptionTrialSettings
    , stsIsInTrial
    , stsTrialEndTime

    -- * ResellernotifyResource
    , ResellernotifyResource
    , resellernotifyResource
    , rrTopicName

    -- * ResellernotifyGetwatchdetailsResponse
    , ResellernotifyGetwatchdetailsResponse
    , resellernotifyGetwatchdetailsResponse
    , rgrTopicName
    , rgrServiceAccountEmailAddresses

    -- * Address
    , Address
    , address
    , aOrganizationName
    , aKind
    , aPostalCode
    , aAddressLine1
    , aLocality
    , aContactName
    , aAddressLine2
    , aCountryCode
    , aRegion
    , aAddressLine3

    -- * Customer
    , Customer
    , customer
    , cCustomerDomainVerified
    , cResourceUiURL
    , cKind
    , cCustomerId
    , cAlternateEmail
    , cCustomerDomain
    , cPhoneNumber
    , cPostalAddress

    -- * ChangePlanRequest
    , ChangePlanRequest
    , changePlanRequest
    , cprKind
    , cprDealCode
    , cprPlanName
    , cprPurchaseOrderId
    , cprSeats

    -- * SubscriptionPlanCommitmentInterval
    , SubscriptionPlanCommitmentInterval
    , subscriptionPlanCommitmentInterval
    , spciStartTime
    , spciEndTime

    -- * SubscriptionsDeleteDeletionType
    , SubscriptionsDeleteDeletionType (..)

    -- * SubscriptionPlan
    , SubscriptionPlan
    , subscriptionPlan
    , spCommitmentInterval
    , spIsCommitmentPlan
    , spPlanName

    -- * Subscriptions
    , Subscriptions
    , subscriptions
    , sNextPageToken
    , sKind
    , sSubscriptions

    -- * Seats
    , Seats
    , seats
    , seaNumberOfSeats
    , seaMaximumNumberOfSeats
    , seaLicensedNumberOfSeats
    , seaKind

    -- * RenewalSettings
    , RenewalSettings
    , renewalSettings
    , rsKind
    , rsRenewalType

    -- * Subscription
    , Subscription
    , subscription
    , subCreationTime
    , subBillingMethod
    , subStatus
    , subTrialSettings
    , subSKUName
    , subResourceUiURL
    , subKind
    , subSKUId
    , subPlan
    , subDealCode
    , subCustomerId
    , subCustomerDomain
    , subSuspensionReasons
    , subTransferInfo
    , subPurchaseOrderId
    , subSeats
    , subRenewalSettings
    , subSubscriptionId

    -- * SubscriptionTransferInfo
    , SubscriptionTransferInfo
    , subscriptionTransferInfo
    , stiTransferabilityExpirationTime
    , stiMinimumTransferableSeats
    ) where

import Network.Google.AppsReseller.Types.Product
import Network.Google.AppsReseller.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Enterprise Apps Reseller API. This contains the host and root path used as a starting point for constructing service requests.
appsResellerService :: ServiceConfig
appsResellerService
  = defaultService (ServiceId "reseller:v1")
      "www.googleapis.com"

-- | Manage users on your domain
appsOrderReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/apps.order.readonly"]
appsOrderReadOnlyScope = Proxy;

-- | Manage users on your domain
appsOrderScope :: Proxy '["https://www.googleapis.com/auth/apps.order"]
appsOrderScope = Proxy;
