{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.CustomSearch.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.CustomSearch.Types
    (
    -- * Service Configuration
      customSearchService

    -- * CSEListImgType
    , CSEListImgType (..)

    -- * PromotionImage
    , PromotionImage
    , promotionImage
    , piHeight
    , piWidth
    , piSource

    -- * Context
    , Context
    , context
    , cFacets
    , cTitle

    -- * CSEListSiteSearchFilter
    , CSEListSiteSearchFilter (..)

    -- * SearchQueries
    , SearchQueries
    , searchQueries
    , sqAddtional

    -- * ResultPagemapAdditionalItem
    , ResultPagemapAdditionalItem
    , resultPagemapAdditionalItem
    , rpaiAddtional

    -- * SearchURL
    , SearchURL
    , searchURL
    , suType
    , suTemplate

    -- * CSESiterestrictListFilter
    , CSESiterestrictListFilter (..)

    -- * SearchSpelling
    , SearchSpelling
    , searchSpelling
    , ssCorrectedQuery
    , ssHTMLCorrectedQuery

    -- * CSESiterestrictListImgType
    , CSESiterestrictListImgType (..)

    -- * CSEListImgDominantColor
    , CSEListImgDominantColor (..)

    -- * ResultImage
    , ResultImage
    , resultImage
    , riThumbnailLink
    , riHeight
    , riByteSize
    , riContextLink
    , riThumbnailHeight
    , riWidth
    , riThumbnailWidth

    -- * CSEListSafe
    , CSEListSafe (..)

    -- * CSESiterestrictListImgSize
    , CSESiterestrictListImgSize (..)

    -- * ResultPagemap
    , ResultPagemap
    , resultPagemap
    , rpAddtional

    -- * CSESiterestrictListImgColorType
    , CSESiterestrictListImgColorType (..)

    -- * CSEListImgColorType
    , CSEListImgColorType (..)

    -- * CSESiterestrictListSafe
    , CSESiterestrictListSafe (..)

    -- * Result
    , Result
    , result
    , rMime
    , rImage
    , rPagemap
    , rDisplayLink
    , rFileFormat
    , rSnippet
    , rKind
    , rLink
    , rHTMLSnippet
    , rHTMLFormattedURL
    , rCacheId
    , rFormattedURL
    , rHTMLTitle
    , rLabels
    , rTitle

    -- * ResultLabelsItem
    , ResultLabelsItem
    , resultLabelsItem
    , rliName
    , rliDisplayName
    , rliLabelWithOp

    -- * CSESiterestrictListImgDominantColor
    , CSESiterestrictListImgDominantColor (..)

    -- * SearchSearchInformation
    , SearchSearchInformation
    , searchSearchInformation
    , ssiSearchTime
    , ssiFormattedSearchTime
    , ssiTotalResults
    , ssiFormattedTotalResults

    -- * CSEListFilter
    , CSEListFilter (..)

    -- * Query
    , Query
    , query
    , qImgDominantColor
    , qOutputEncoding
    , qSiteSearchFilter
    , qInputEncoding
    , qOrTerms
    , qSearchTerms
    , qStartPage
    , qRights
    , qCount
    , qExcludeTerms
    , qFileType
    , qSearchType
    , qGoogleHost
    , qDisableCnTwTranslation
    , qRelatedSite
    , qHl
    , qSort
    , qLanguage
    , qSiteSearch
    , qFilter
    , qTotalResults
    , qDateRestrict
    , qTitle
    , qLinkSite
    , qLowRange
    , qImgType
    , qGl
    , qCx
    , qImgColorType
    , qImgSize
    , qExactTerms
    , qStartIndex
    , qCr
    , qSafe
    , qHq
    , qHighRange

    -- * CSESiterestrictListSiteSearchFilter
    , CSESiterestrictListSiteSearchFilter (..)

    -- * PromotionBodyLinesItem
    , PromotionBodyLinesItem
    , promotionBodyLinesItem
    , pbliLink
    , pbliURL
    , pbliHTMLTitle
    , pbliTitle

    -- * Promotion
    , Promotion
    , promotion
    , pImage
    , pDisplayLink
    , pBodyLines
    , pLink
    , pHTMLTitle
    , pTitle

    -- * CSESiterestrictListLr
    , CSESiterestrictListLr (..)

    -- * CSESiterestrictListSearchType
    , CSESiterestrictListSearchType (..)

    -- * Search
    , Search
    , search
    , sQueries
    , sContext
    , sKind
    , sURL
    , sItems
    , sSearchInformation
    , sPromotions
    , sSpelling

    -- * CSEListLr
    , CSEListLr (..)

    -- * ContextFacetsItemItem
    , ContextFacetsItemItem
    , contextFacetsItemItem
    , cfiiAnchor
    , cfiiLabelWithOp
    , cfiiLabel

    -- * CSEListSearchType
    , CSEListSearchType (..)

    -- * CSEListImgSize
    , CSEListImgSize (..)
    ) where

import Network.Google.CustomSearch.Types.Product
import Network.Google.CustomSearch.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the CustomSearch API. This contains the host and root path used as a starting point for constructing service requests.
customSearchService :: ServiceConfig
customSearchService
  = defaultService (ServiceId "customsearch:v1")
      "www.googleapis.com"
