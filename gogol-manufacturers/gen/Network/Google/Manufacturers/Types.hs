{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Manufacturers.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Manufacturers.Types
    (
    -- * Service Configuration
      manufacturersService

    -- * OAuth Scopes
    , manufacturercenterScope

    -- * Image
    , Image
    , image
    , iStatus
    , iImageURL
    , iType

    -- * FeatureDescription
    , FeatureDescription
    , featureDescription
    , fdImage
    , fdText
    , fdHeadline

    -- * ProductDetail
    , ProductDetail
    , productDetail
    , pdAttributeValue
    , pdAttributeName
    , pdSectionName

    -- * Empty
    , Empty
    , empty

    -- * ImageStatus
    , ImageStatus (..)

    -- * Count
    , Count
    , count
    , cValue
    , cUnit

    -- * Capacity
    , Capacity
    , capacity
    , capValue
    , capUnit

    -- * ImageType
    , ImageType (..)

    -- * Attributes
    , Attributes
    , attributes
    , aProductName
    , aScent
    , aImageLink
    , aFeatureDescription
    , aProductDetail
    , aProductLine
    , aColor
    , aSize
    , aFlavor
    , aPattern
    , aSizeSystem
    , aMaterial
    , aFormat
    , aProductType
    , aCount
    , aDisclosureDate
    , aBrand
    , aAdditionalImageLink
    , aVideoLink
    , aCapacity
    , aGtin
    , aAgeGroup
    , aGender
    , aSuggestedRetailPrice
    , aItemGroupId
    , aTargetClientId
    , aSizeType
    , aReleaseDate
    , aTitle
    , aMpn
    , aProductPageURL
    , aDescription
    , aTheme

    -- * Xgafv
    , Xgafv (..)

    -- * IssueSeverity
    , IssueSeverity (..)

    -- * Price
    , Price
    , price
    , pAmount
    , pCurrency

    -- * Product
    , Product
    , product
    , pParent
    , pManuallyDeletedAttributes
    , pTargetCountry
    , pManuallyProvidedAttributes
    , pName
    , pIssues
    , pUploadedAttributes
    , pContentLanguage
    , pFinalAttributes
    , pProductId

    -- * Issue
    , Issue
    , issue
    , issAttribute
    , issSeverity
    , issType
    , issTimestamp
    , issDescription

    -- * ListProductsResponse
    , ListProductsResponse
    , listProductsResponse
    , lprNextPageToken
    , lprProducts
    ) where

import Network.Google.Manufacturers.Types.Product
import Network.Google.Manufacturers.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Manufacturer Center API. This contains the host and root path used as a starting point for constructing service requests.
manufacturersService :: ServiceConfig
manufacturersService
  = defaultService (ServiceId "manufacturers:v1")
      "manufacturers.googleapis.com"

-- | Manage your product listings for Google Manufacturer Center
manufacturercenterScope :: Proxy '["https://www.googleapis.com/auth/manufacturercenter"]
manufacturercenterScope = Proxy;
