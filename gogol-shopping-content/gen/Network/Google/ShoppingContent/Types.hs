{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ShoppingContent.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ShoppingContent.Types
    (
    -- * Service Configuration
      shoppingContentService

    -- * OAuth Scopes
    , contentScope

    -- * OrdersAcknowledgeRequest
    , OrdersAcknowledgeRequest
    , ordersAcknowledgeRequest
    , oarOperationId

    -- * AccountTax
    , AccountTax
    , accountTax
    , atRules
    , atKind
    , atAccountId

    -- * OrdersUpdateMerchantOrderIdRequest
    , OrdersUpdateMerchantOrderIdRequest
    , ordersUpdateMerchantOrderIdRequest
    , oumoirMerchantOrderId
    , oumoirOperationId

    -- * OrdersAdvanceTestOrderResponse
    , OrdersAdvanceTestOrderResponse
    , ordersAdvanceTestOrderResponse
    , oatorKind

    -- * ProductsCustomBatchResponse
    , ProductsCustomBatchResponse
    , productsCustomBatchResponse
    , pcbrEntries
    , pcbrKind

    -- * OrderMerchantProvidedAnnotation
    , OrderMerchantProvidedAnnotation
    , orderMerchantProvidedAnnotation
    , ompaValue
    , ompaKey

    -- * OrdersGettestOrdertemplateTemplateName
    , OrdersGettestOrdertemplateTemplateName (..)

    -- * TestOrderCustomer
    , TestOrderCustomer
    , testOrderCustomer
    , tocFullName
    , tocEmail
    , tocExplicitMarketingPreference

    -- * DatafeedstatusesCustomBatchResponse
    , DatafeedstatusesCustomBatchResponse
    , datafeedstatusesCustomBatchResponse
    , dcbrEntries
    , dcbrKind

    -- * OrderReturn
    , OrderReturn
    , orderReturn
    , orQuantity
    , orActor
    , orReason
    , orCreationDate
    , orReasonText

    -- * AccounttaxCustomBatchResponseEntry
    , AccounttaxCustomBatchResponseEntry
    , accounttaxCustomBatchResponseEntry
    , acbreAccountTax
    , acbreKind
    , acbreErrors
    , acbreBatchId

    -- * InventoryCustomBatchRequest
    , InventoryCustomBatchRequest
    , inventoryCustomBatchRequest
    , icbrEntries

    -- * PosSaleResponse
    , PosSaleResponse
    , posSaleResponse
    , psrStoreCode
    , psrKind
    , psrItemId
    , psrQuantity
    , psrTargetCountry
    , psrGtin
    , psrPrice
    , psrContentLanguage
    , psrTimestamp
    , psrSaleId

    -- * AccountsAuthInfoResponse
    , AccountsAuthInfoResponse
    , accountsAuthInfoResponse
    , aairKind
    , aairAccountIdentifiers

    -- * PosSale
    , PosSale
    , posSale
    , psStoreCode
    , psKind
    , psItemId
    , psQuantity
    , psTargetCountry
    , psGtin
    , psPrice
    , psContentLanguage
    , psTimestamp
    , psSaleId

    -- * ProductStatusDestinationStatus
    , ProductStatusDestinationStatus
    , productStatusDestinationStatus
    , psdsDestination
    , psdsApprovalPending
    , psdsIntention
    , psdsApprovalStatus

    -- * AccountTaxTaxRule
    , AccountTaxTaxRule
    , accountTaxTaxRule
    , attrUseGlobalRate
    , attrCountry
    , attrShippingTaxed
    , attrLocationId
    , attrRatePercent

    -- * PostalCodeGroup
    , PostalCodeGroup
    , postalCodeGroup
    , pcgCountry
    , pcgPostalCodeRanges
    , pcgName

    -- * ProductDestination
    , ProductDestination
    , productDestination
    , pdIntention
    , pdDestinationName

    -- * DatafeedsCustomBatchRequest
    , DatafeedsCustomBatchRequest
    , datafeedsCustomBatchRequest
    , dEntries

    -- * OrdersCustomBatchRequestEntry
    , OrdersCustomBatchRequestEntry
    , ordersCustomBatchRequestEntry
    , ocbreMerchantId
    , ocbreCancelLineItem
    , ocbreInStoreRefundLineItem
    , ocbreRefund
    , ocbreUpdateShipment
    , ocbreReturnLineItem
    , ocbreMerchantOrderId
    , ocbreSetLineItemMetadata
    , ocbreReturnRefundLineItem
    , ocbreMethod
    , ocbreUpdateLineItemShippingDetails
    , ocbreShipLineItems
    , ocbreOperationId
    , ocbreOrderId
    , ocbreRejectReturnLineItem
    , ocbreCancel
    , ocbreBatchId

    -- * OrdersRefundRequest
    , OrdersRefundRequest
    , ordersRefundRequest
    , orrAmount
    , orrReason
    , orrOperationId
    , orrAmountPretax
    , orrAmountTax
    , orrReasonText

    -- * OrdersCustomBatchRequestEntryCancelLineItem
    , OrdersCustomBatchRequestEntryCancelLineItem
    , ordersCustomBatchRequestEntryCancelLineItem
    , ocbrecliAmount
    , ocbrecliQuantity
    , ocbrecliLineItemId
    , ocbrecliReason
    , ocbrecliAmountPretax
    , ocbrecliProductId
    , ocbrecliAmountTax
    , ocbrecliReasonText

    -- * OrderLineItemShippingDetailsMethod
    , OrderLineItemShippingDetailsMethod
    , orderLineItemShippingDetailsMethod
    , olisdmCarrier
    , olisdmMethodName
    , olisdmMaxDaysInTransit
    , olisdmMinDaysInTransit

    -- * Datafeed
    , Datafeed
    , datafeed
    , dKind
    , dFormat
    , dAttributeLanguage
    , dTargetCountry
    , dFetchSchedule
    , dName
    , dIntendedDestinations
    , dTargets
    , dId
    , dContentLanguage
    , dContentType
    , dFileName

    -- * OrdersCreateTestOrderResponse
    , OrdersCreateTestOrderResponse
    , ordersCreateTestOrderResponse
    , octorKind
    , octorOrderId

    -- * AccountsCustomBatchResponseEntry
    , AccountsCustomBatchResponseEntry
    , accountsCustomBatchResponseEntry
    , aKind
    , aAccount
    , aErrors
    , aBatchId

    -- * OrdersListOrderBy
    , OrdersListOrderBy (..)

    -- * AccountIdentifier
    , AccountIdentifier
    , accountIdentifier
    , aiMerchantId
    , aiAggregatorId

    -- * PosCustomBatchRequest
    , PosCustomBatchRequest
    , posCustomBatchRequest
    , pEntries

    -- * TestOrderPaymentMethod
    , TestOrderPaymentMethod
    , testOrderPaymentMethod
    , topmExpirationMonth
    , topmExpirationYear
    , topmLastFourDigits
    , topmType
    , topmPredefinedBillingAddress

    -- * OrderLineItem
    , OrderLineItem
    , orderLineItem
    , oliAnnotations
    , oliQuantityOrdered
    , oliReturnInfo
    , oliQuantityDelivered
    , oliShippingDetails
    , oliQuantityPending
    , oliCancellations
    , oliQuantityCanceled
    , oliId
    , oliTax
    , oliPrice
    , oliQuantityShipped
    , oliQuantityReturned
    , oliProduct
    , oliReturns

    -- * Service
    , Service
    , service
    , sDeliveryCountry
    , sRateGroups
    , sDeliveryTime
    , sActive
    , sName
    , sCurrency
    , sMinimumOrderValue

    -- * ProductstatusesCustomBatchResponse
    , ProductstatusesCustomBatchResponse
    , productstatusesCustomBatchResponse
    , proEntries
    , proKind

    -- * ProductUnitPricingMeasure
    , ProductUnitPricingMeasure
    , productUnitPricingMeasure
    , pupmValue
    , pupmUnit

    -- * OrdersUpdateShipmentRequest
    , OrdersUpdateShipmentRequest
    , ordersUpdateShipmentRequest
    , ousrCarrier
    , ousrStatus
    , ousrTrackingId
    , ousrShipmentId
    , ousrOperationId

    -- * OrderShipmentLineItemShipment
    , OrderShipmentLineItemShipment
    , orderShipmentLineItemShipment
    , oslisQuantity
    , oslisLineItemId
    , oslisProductId

    -- * OrdersListStatuses
    , OrdersListStatuses (..)

    -- * LoyaltyPoints
    , LoyaltyPoints
    , loyaltyPoints
    , lpRatio
    , lpPointsValue
    , lpName

    -- * OrdersCustomBatchRequestEntryShipLineItems
    , OrdersCustomBatchRequestEntryShipLineItems
    , ordersCustomBatchRequestEntryShipLineItems
    , ocbresliCarrier
    , ocbresliTrackingId
    , ocbresliShipmentId
    , ocbresliShipmentInfos
    , ocbresliLineItems

    -- * AccountStatus
    , AccountStatus
    , accountStatus
    , asDataQualityIssues
    , asAccountLevelIssues
    , asKind
    , asAccountId
    , asWebsiteClaimed

    -- * OrdersReturnLineItemRequest
    , OrdersReturnLineItemRequest
    , ordersReturnLineItemRequest
    , orlirQuantity
    , orlirLineItemId
    , orlirReason
    , orlirOperationId
    , orlirProductId
    , orlirReasonText

    -- * ShippingSettingsCustomBatchRequestEntry
    , ShippingSettingsCustomBatchRequestEntry
    , shippingSettingsCustomBatchRequestEntry
    , sscbreMerchantId
    , sscbreAccountId
    , sscbreMethod
    , sscbreShippingSettings
    , sscbreBatchId

    -- * PosInventoryResponse
    , PosInventoryResponse
    , posInventoryResponse
    , pirStoreCode
    , pirKind
    , pirItemId
    , pirQuantity
    , pirTargetCountry
    , pirGtin
    , pirPrice
    , pirContentLanguage
    , pirTimestamp

    -- * OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails
    , OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails
    , ordersCustomBatchRequestEntryUpdateLineItemShippingDetails
    , ocbreulisdShipByDate
    , ocbreulisdLineItemId
    , ocbreulisdDeliverByDate
    , ocbreulisdProductId

    -- * AccountstatusesCustomBatchRequest
    , AccountstatusesCustomBatchRequest
    , accountstatusesCustomBatchRequest
    , acbrEntries

    -- * AccounttaxListResponse
    , AccounttaxListResponse
    , accounttaxListResponse
    , alrNextPageToken
    , alrKind
    , alrResources

    -- * OrdersGetTestOrderTemplateResponse
    , OrdersGetTestOrderTemplateResponse
    , ordersGetTestOrderTemplateResponse
    , ogtotrKind
    , ogtotrTemplate

    -- * AccountsCustomBatchRequestEntry
    , AccountsCustomBatchRequestEntry
    , accountsCustomBatchRequestEntry
    , accMerchantId
    , accForce
    , accAccount
    , accAccountId
    , accMethod
    , accOverwrite
    , accBatchId

    -- * Weight
    , Weight
    , weight
    , wValue
    , wUnit

    -- * Error'
    , Error'
    , error'
    , eDomain
    , eReason
    , eMessage

    -- * ProductstatusesListResponse
    , ProductstatusesListResponse
    , productstatusesListResponse
    , plrNextPageToken
    , plrKind
    , plrResources

    -- * PosCustomBatchResponse
    , PosCustomBatchResponse
    , posCustomBatchResponse
    , posEntries
    , posKind

    -- * OrdersRefundResponse
    , OrdersRefundResponse
    , ordersRefundResponse
    , orrKind
    , orrExecutionStatus

    -- * OrdersCreateTestOrderRequest
    , OrdersCreateTestOrderRequest
    , ordersCreateTestOrderRequest
    , octorTemplateName
    , octorTestOrder

    -- * AccountUser
    , AccountUser
    , accountUser
    , auAdmin
    , auEmailAddress

    -- * AccountStatusExampleItem
    , AccountStatusExampleItem
    , accountStatusExampleItem
    , aseiSubmittedValue
    , aseiLink
    , aseiItemId
    , aseiTitle
    , aseiValueOnLandingPage

    -- * OrdersInStoreRefundLineItemResponse
    , OrdersInStoreRefundLineItemResponse
    , ordersInStoreRefundLineItemResponse
    , oisrlirKind
    , oisrlirExecutionStatus

    -- * DatafeedsCustomBatchRequestEntry
    , DatafeedsCustomBatchRequestEntry
    , datafeedsCustomBatchRequestEntry
    , dcbreMerchantId
    , dcbreDatafeed
    , dcbreMethod
    , dcbreDatafeedId
    , dcbreBatchId

    -- * AccountStatusAccountLevelIssue
    , AccountStatusAccountLevelIssue
    , accountStatusAccountLevelIssue
    , asaliCountry
    , asaliSeverity
    , asaliId
    , asaliTitle
    , asaliDetail

    -- * Value
    , Value
    , value
    , vPricePercentage
    , vCarrierRateName
    , vFlatRate
    , vSubtableName
    , vNoShipping

    -- * Installment
    , Installment
    , installment
    , iAmount
    , iMonths

    -- * DatafeedFetchSchedule
    , DatafeedFetchSchedule
    , datafeedFetchSchedule
    , dfsFetchURL
    , dfsUsername
    , dfsMinuteOfHour
    , dfsPassword
    , dfsDayOfMonth
    , dfsHour
    , dfsWeekday
    , dfsTimeZone
    , dfsPaused

    -- * PosStore
    , PosStore
    , posStore
    , pssStoreCode
    , pssKind
    , pssStoreAddress

    -- * OrdersCustomBatchRequest
    , OrdersCustomBatchRequest
    , ordersCustomBatchRequest
    , ocbrEntries

    -- * ShippingSettingsGetSupportedCarriersResponse
    , ShippingSettingsGetSupportedCarriersResponse
    , shippingSettingsGetSupportedCarriersResponse
    , ssgscrKind
    , ssgscrCarriers

    -- * OrdersCustomBatchRequestEntrySetLineItemMetadata
    , OrdersCustomBatchRequestEntrySetLineItemMetadata
    , ordersCustomBatchRequestEntrySetLineItemMetadata
    , ocbreslimAnnotations
    , ocbreslimLineItemId
    , ocbreslimProductId

    -- * HolidaysHoliday
    , HolidaysHoliday
    , holidaysHoliday
    , hhDeliveryGuaranteeHour
    , hhDate
    , hhDeliveryGuaranteeDate
    , hhCountryCode
    , hhId
    , hhType

    -- * AccountsListResponse
    , AccountsListResponse
    , accountsListResponse
    , accNextPageToken
    , accKind
    , accResources

    -- * ProductStatusDataQualityIssue
    , ProductStatusDataQualityIssue
    , productStatusDataQualityIssue
    , psdqiLocation
    , psdqiFetchStatus
    , psdqiSeverity
    , psdqiValueProvided
    , psdqiId
    , psdqiValueOnLandingPage
    , psdqiTimestamp
    , psdqiDetail

    -- * CarriersCarrier
    , CarriersCarrier
    , carriersCarrier
    , ccCountry
    , ccName
    , ccServices

    -- * CarrierRate
    , CarrierRate
    , carrierRate
    , crOriginPostalCode
    , crFlatAdjustment
    , crCarrierService
    , crName
    , crPercentageAdjustment
    , crCarrierName

    -- * ShippingSettingsListResponse
    , ShippingSettingsListResponse
    , shippingSettingsListResponse
    , sslrNextPageToken
    , sslrKind
    , sslrResources

    -- * OrdersShipLineItemsRequest
    , OrdersShipLineItemsRequest
    , ordersShipLineItemsRequest
    , oslirCarrier
    , oslirTrackingId
    , oslirShipmentId
    , oslirShipmentInfos
    , oslirLineItems
    , oslirOperationId

    -- * AccountsCustomBatchResponse
    , AccountsCustomBatchResponse
    , accountsCustomBatchResponse
    , acbrcEntries
    , acbrcKind

    -- * OrdersUpdateLineItemShippingDetailsRequest
    , OrdersUpdateLineItemShippingDetailsRequest
    , ordersUpdateLineItemShippingDetailsRequest
    , oulisdrShipByDate
    , oulisdrLineItemId
    , oulisdrDeliverByDate
    , oulisdrOperationId
    , oulisdrProductId

    -- * ProductTax
    , ProductTax
    , productTax
    , ptTaxShip
    , ptCountry
    , ptPostalCode
    , ptRate
    , ptRegion
    , ptLocationId

    -- * PosCustomBatchRequestEntry
    , PosCustomBatchRequestEntry
    , posCustomBatchRequestEntry
    , pcbreMerchantId
    , pcbreStoreCode
    , pcbreTargetMerchantId
    , pcbreMethod
    , pcbreStore
    , pcbreInventory
    , pcbreSale
    , pcbreBatchId

    -- * OrderShipment
    , OrderShipment
    , orderShipment
    , osCarrier
    , osStatus
    , osTrackingId
    , osLineItems
    , osId
    , osCreationDate
    , osDeliveryDate

    -- * OrderLineItemReturnInfo
    , OrderLineItemReturnInfo
    , orderLineItemReturnInfo
    , oliriIsReturnable
    , oliriPolicyURL
    , oliriDaysToReturn

    -- * Account
    , Account
    , account
    , aaUsers
    , aaYouTubeChannelLinks
    , aaKind
    , aaSellerId
    , aaName
    , aaReviewsURL
    , aaId
    , aaWebsiteURL
    , aaAdwordsLinks
    , aaGoogleMyBusinessLink
    , aaAdultContent

    -- * InventorySetRequest
    , InventorySetRequest
    , inventorySetRequest
    , isrLoyaltyPoints
    , isrQuantity
    , isrInstallment
    , isrSalePrice
    , isrAvailability
    , isrPickup
    , isrSalePriceEffectiveDate
    , isrSellOnGoogleQuantity
    , isrPrice

    -- * OrdersCancelLineItemRequest
    , OrdersCancelLineItemRequest
    , ordersCancelLineItemRequest
    , oclirAmount
    , oclirQuantity
    , oclirLineItemId
    , oclirReason
    , oclirOperationId
    , oclirAmountPretax
    , oclirProductId
    , oclirAmountTax
    , oclirReasonText

    -- * ProductShippingWeight
    , ProductShippingWeight
    , productShippingWeight
    , pswValue
    , pswUnit

    -- * AccountstatusesCustomBatchRequestEntry
    , AccountstatusesCustomBatchRequestEntry
    , accountstatusesCustomBatchRequestEntry
    , acbrecMerchantId
    , acbrecAccountId
    , acbrecMethod
    , acbrecBatchId

    -- * DeliveryTime
    , DeliveryTime
    , deliveryTime
    , dtHolidayCutoffs
    , dtMinTransitTimeInDays
    , dtMaxTransitTimeInDays

    -- * OrdersReturnRefundLineItemResponse
    , OrdersReturnRefundLineItemResponse
    , ordersReturnRefundLineItemResponse
    , orrlirKind
    , orrlirExecutionStatus

    -- * OrdersCustomBatchRequestEntryRejectReturnLineItem
    , OrdersCustomBatchRequestEntryRejectReturnLineItem
    , ordersCustomBatchRequestEntryRejectReturnLineItem
    , ocbrerrliQuantity
    , ocbrerrliLineItemId
    , ocbrerrliReason
    , ocbrerrliProductId
    , ocbrerrliReasonText

    -- * ProductstatusesCustomBatchResponseEntry
    , ProductstatusesCustomBatchResponseEntry
    , productstatusesCustomBatchResponseEntry
    , pKind
    , pProductStatus
    , pErrors
    , pBatchId

    -- * ShippingSettingsGetSupportedHolidaysResponse
    , ShippingSettingsGetSupportedHolidaysResponse
    , shippingSettingsGetSupportedHolidaysResponse
    , ssgshrKind
    , ssgshrHolidays

    -- * OrdersCustomBatchRequestEntryCancel
    , OrdersCustomBatchRequestEntryCancel
    , ordersCustomBatchRequestEntryCancel
    , ocbrecReason
    , ocbrecReasonText

    -- * DatafeedFormat
    , DatafeedFormat
    , datafeedFormat
    , dfQuotingMode
    , dfFileEncoding
    , dfColumnDelimiter

    -- * ProductShipping
    , ProductShipping
    , productShipping
    , pService
    , pLocationGroupName
    , pCountry
    , pPostalCode
    , pPrice
    , pRegion
    , pLocationId

    -- * ShippingSettingsCustomBatchRequest
    , ShippingSettingsCustomBatchRequest
    , shippingSettingsCustomBatchRequest
    , sscbrEntries

    -- * AccountsCustomBatchRequest
    , AccountsCustomBatchRequest
    , accountsCustomBatchRequest
    , aEntries

    -- * PosCustomBatchResponseEntry
    , PosCustomBatchResponseEntry
    , posCustomBatchResponseEntry
    , pcbrecKind
    , pcbrecStore
    , pcbrecInventory
    , pcbrecErrors
    , pcbrecSale
    , pcbrecBatchId

    -- * ProductCustomAttribute
    , ProductCustomAttribute
    , productCustomAttribute
    , pcaValue
    , pcaName
    , pcaType
    , pcaUnit

    -- * OrdersListResponse
    , OrdersListResponse
    , ordersListResponse
    , olrNextPageToken
    , olrKind
    , olrResources

    -- * OrdersUpdateLineItemShippingDetailsResponse
    , OrdersUpdateLineItemShippingDetailsResponse
    , ordersUpdateLineItemShippingDetailsResponse
    , oulisdrKind
    , oulisdrExecutionStatus

    -- * Headers
    , Headers
    , headers
    , hNumberOfItems
    , hPostalCodeGroupNames
    , hPrices
    , hWeights
    , hLocations

    -- * OrdersShipLineItemsResponse
    , OrdersShipLineItemsResponse
    , ordersShipLineItemsResponse
    , oslirKind
    , oslirExecutionStatus

    -- * ShippingSettings
    , ShippingSettings
    , shippingSettings
    , ssPostalCodeGroups
    , ssAccountId
    , ssServices

    -- * PostalCodeRange
    , PostalCodeRange
    , postalCodeRange
    , pcrPostalCodeRangeBegin
    , pcrPostalCodeRangeEnd

    -- * OrdersUpdateShipmentResponse
    , OrdersUpdateShipmentResponse
    , ordersUpdateShipmentResponse
    , ousrKind
    , ousrExecutionStatus

    -- * ProductstatusesCustomBatchRequest
    , ProductstatusesCustomBatchRequest
    , productstatusesCustomBatchRequest
    , pcbrcEntries

    -- * AccountYouTubeChannelLink
    , AccountYouTubeChannelLink
    , accountYouTubeChannelLink
    , aytclStatus
    , aytclChannelId

    -- * OrdersReturnLineItemResponse
    , OrdersReturnLineItemResponse
    , ordersReturnLineItemResponse
    , orlirKind
    , orlirExecutionStatus

    -- * ProductCustomGroup
    , ProductCustomGroup
    , productCustomGroup
    , pName
    , pAttributes

    -- * PosInventoryRequest
    , PosInventoryRequest
    , posInventoryRequest
    , posStoreCode
    , posItemId
    , posQuantity
    , posTargetCountry
    , posGtin
    , posPrice
    , posContentLanguage
    , posTimestamp

    -- * AccountstatusesCustomBatchResponse
    , AccountstatusesCustomBatchResponse
    , accountstatusesCustomBatchResponse
    , acccEntries
    , acccKind

    -- * ShippingSettingsCustomBatchResponseEntry
    , ShippingSettingsCustomBatchResponseEntry
    , shippingSettingsCustomBatchResponseEntry
    , sKind
    , sShippingSettings
    , sErrors
    , sBatchId

    -- * ProductStatus
    , ProductStatus
    , productStatus
    , ppDataQualityIssues
    , ppKind
    , ppLink
    , ppDestinationStatuses
    , ppLastUpdateDate
    , ppCreationDate
    , ppTitle
    , ppProduct
    , ppGoogleExpirationDate
    , ppProductId
    , ppItemLevelIssues

    -- * AccountstatusesListResponse
    , AccountstatusesListResponse
    , accountstatusesListResponse
    , alrlNextPageToken
    , alrlKind
    , alrlResources

    -- * OrdersSetLineItemMetadataResponse
    , OrdersSetLineItemMetadataResponse
    , ordersSetLineItemMetadataResponse
    , oslimrKind
    , oslimrExecutionStatus

    -- * AccounttaxCustomBatchRequest
    , AccounttaxCustomBatchRequest
    , accounttaxCustomBatchRequest
    , accEntries

    -- * ProductsCustomBatchRequestEntry
    , ProductsCustomBatchRequestEntry
    , productsCustomBatchRequestEntry
    , prorMerchantId
    , prorMethod
    , prorProduct
    , prorProductId
    , prorBatchId

    -- * AccountGoogleMyBusinessLink
    , AccountGoogleMyBusinessLink
    , accountGoogleMyBusinessLink
    , agmblGmbEmail
    , agmblStatus

    -- * DatafeedstatusesCustomBatchRequestEntry
    , DatafeedstatusesCustomBatchRequestEntry
    , datafeedstatusesCustomBatchRequestEntry
    , dMerchantId
    , dCountry
    , dMethod
    , dDatafeedId
    , dLanguage
    , dBatchId

    -- * OrderCustomer
    , OrderCustomer
    , orderCustomer
    , ocFullName
    , ocEmail
    , ocExplicitMarketingPreference

    -- * InventoryCustomBatchResponseEntry
    , InventoryCustomBatchResponseEntry
    , inventoryCustomBatchResponseEntry
    , icbreKind
    , icbreErrors
    , icbreBatchId

    -- * LocationIdSet
    , LocationIdSet
    , locationIdSet
    , lisLocationIds

    -- * Row
    , Row
    , row
    , rCells

    -- * Inventory
    , Inventory
    , inventory
    , iLoyaltyPoints
    , iKind
    , iQuantity
    , iInstallment
    , iSalePrice
    , iAvailability
    , iPickup
    , iSalePriceEffectiveDate
    , iSellOnGoogleQuantity
    , iPrice

    -- * OrdersGetByMerchantOrderIdResponse
    , OrdersGetByMerchantOrderIdResponse
    , ordersGetByMerchantOrderIdResponse
    , ogbmoirKind
    , ogbmoirOrder

    -- * OrderPromotionBenefit
    , OrderPromotionBenefit
    , orderPromotionBenefit
    , opbTaxImpact
    , opbDiscount
    , opbOfferIds
    , opbSubType
    , opbType

    -- * OrdersRejectReturnLineItemRequest
    , OrdersRejectReturnLineItemRequest
    , ordersRejectReturnLineItemRequest
    , orrlirQuantity
    , orrlirLineItemId
    , orrlirReason
    , orrlirOperationId
    , orrlirProductId
    , orrlirReasonText

    -- * OrdersCustomBatchRequestEntryInStoreRefundLineItem
    , OrdersCustomBatchRequestEntryInStoreRefundLineItem
    , ordersCustomBatchRequestEntryInStoreRefundLineItem
    , ocbreisrliQuantity
    , ocbreisrliLineItemId
    , ocbreisrliReason
    , ocbreisrliAmountPretax
    , ocbreisrliProductId
    , ocbreisrliAmountTax
    , ocbreisrliReasonText

    -- * OrdersCancelRequest
    , OrdersCancelRequest
    , ordersCancelRequest
    , ocrReason
    , ocrOperationId
    , ocrReasonText

    -- * ProductStatusItemLevelIssue
    , ProductStatusItemLevelIssue
    , productStatusItemLevelIssue
    , psiliDestination
    , psiliResolution
    , psiliCode
    , psiliServability
    , psiliAttributeName

    -- * OrderLineItemProductVariantAttribute
    , OrderLineItemProductVariantAttribute
    , orderLineItemProductVariantAttribute
    , olipvaDimension
    , olipvaValue

    -- * OrdersCustomBatchResponseEntry
    , OrdersCustomBatchResponseEntry
    , ordersCustomBatchResponseEntry
    , oKind
    , oExecutionStatus
    , oErrors
    , oOrder
    , oBatchId

    -- * RateGroup
    , RateGroup
    , rateGroup
    , rgCarrierRates
    , rgApplicableShippingLabels
    , rgMainTable
    , rgSingleValue
    , rgSubtables

    -- * OrderPromotion
    , OrderPromotion
    , orderPromotion
    , opEffectiveDates
    , opGenericRedemptionCode
    , opRedemptionChannel
    , opBenefits
    , opLongTitle
    , opId
    , opProductApplicability

    -- * Price
    , Price
    , price
    , pValue
    , pCurrency

    -- * OrderLineItemShippingDetails
    , OrderLineItemShippingDetails
    , orderLineItemShippingDetails
    , olisdShipByDate
    , olisdMethod
    , olisdDeliverByDate

    -- * DatafeedsCustomBatchResponse
    , DatafeedsCustomBatchResponse
    , datafeedsCustomBatchResponse
    , datEntries
    , datKind

    -- * PosListResponse
    , PosListResponse
    , posListResponse
    , plrlKind
    , plrlResources

    -- * OrderDeliveryDetails
    , OrderDeliveryDetails
    , orderDeliveryDetails
    , oddAddress
    , oddPhoneNumber

    -- * OrdersCancelResponse
    , OrdersCancelResponse
    , ordersCancelResponse
    , ocrKind
    , ocrExecutionStatus

    -- * OrdersSetLineItemMetadataRequest
    , OrdersSetLineItemMetadataRequest
    , ordersSetLineItemMetadataRequest
    , oslimrAnnotations
    , oslimrLineItemId
    , oslimrOperationId
    , oslimrProductId

    -- * OrdersRejectReturnLineItemResponse
    , OrdersRejectReturnLineItemResponse
    , ordersRejectReturnLineItemResponse
    , ordKind
    , ordExecutionStatus

    -- * TestOrder
    , TestOrder
    , testOrder
    , toKind
    , toLineItems
    , toShippingOption
    , toPredefinedDeliveryAddress
    , toShippingCostTax
    , toCustomer
    , toPaymentMethod
    , toPromotions
    , toNotificationMode
    , toShippingCost

    -- * DatafeedstatusesCustomBatchResponseEntry
    , DatafeedstatusesCustomBatchResponseEntry
    , datafeedstatusesCustomBatchResponseEntry
    , datErrors
    , datDatafeedStatus
    , datBatchId

    -- * OrderRefund
    , OrderRefund
    , orderRefund
    , oAmount
    , oActor
    , oReason
    , oCreationDate
    , oReasonText

    -- * TestOrderLineItemProduct
    , TestOrderLineItemProduct
    , testOrderLineItemProduct
    , tolipImageLink
    , tolipChannel
    , tolipBrand
    , tolipTargetCountry
    , tolipGtin
    , tolipItemGroupId
    , tolipOfferId
    , tolipPrice
    , tolipVariantAttributes
    , tolipTitle
    , tolipContentLanguage
    , tolipMpn
    , tolipCondition

    -- * OrdersCustomBatchRequestEntryReturnRefundLineItem
    , OrdersCustomBatchRequestEntryReturnRefundLineItem
    , ordersCustomBatchRequestEntryReturnRefundLineItem
    , ordQuantity
    , ordLineItemId
    , ordReason
    , ordAmountPretax
    , ordProductId
    , ordAmountTax
    , ordReasonText

    -- * AccounttaxCustomBatchResponse
    , AccounttaxCustomBatchResponse
    , accounttaxCustomBatchResponse
    , acbr1Entries
    , acbr1Kind

    -- * InventoryCustomBatchRequestEntry
    , InventoryCustomBatchRequestEntry
    , inventoryCustomBatchRequestEntry
    , iMerchantId
    , iStoreCode
    , iInventory
    , iProductId
    , iBatchId

    -- * AccountsClaimWebsiteResponse
    , AccountsClaimWebsiteResponse
    , accountsClaimWebsiteResponse
    , acwrKind

    -- * OrderAddress
    , OrderAddress
    , orderAddress
    , oaRecipientName
    , oaStreetAddress
    , oaCountry
    , oaPostalCode
    , oaLocality
    , oaIsPostOfficeBox
    , oaFullAddress
    , oaRegion

    -- * ProductUnitPricingBaseMeasure
    , ProductUnitPricingBaseMeasure
    , productUnitPricingBaseMeasure
    , pupbmValue
    , pupbmUnit

    -- * DatafeedsListResponse
    , DatafeedsListResponse
    , datafeedsListResponse
    , dlrNextPageToken
    , dlrKind
    , dlrResources

    -- * ProductsCustomBatchResponseEntry
    , ProductsCustomBatchResponseEntry
    , productsCustomBatchResponseEntry
    , pcbre1Kind
    , pcbre1Product
    , pcbre1Errors
    , pcbre1BatchId

    -- * OrderPaymentMethod
    , OrderPaymentMethod
    , orderPaymentMethod
    , opmExpirationMonth
    , opmExpirationYear
    , opmPhoneNumber
    , opmBillingAddress
    , opmLastFourDigits
    , opmType

    -- * Product
    , Product
    , product
    , prorDisplayAdsLink
    , prorCustomLabel1
    , prorShippingWidth
    , prorCustomGroups
    , prorImageLink
    , prorDisplayAdsValue
    , prorLoyaltyPoints
    , prorAdditionalImageLinks
    , prorValidatedDestinations
    , prorColor
    , prorCustomLabel0
    , prorKind
    , prorMinHandlingTime
    , prorMultipack
    , prorPattern
    , prorLink
    , prorSizeSystem
    , prorUnitPricingBaseMeasure
    , prorTaxes
    , prorMaterial
    , prorInstallment
    , prorChannel
    , prorProductType
    , prorIdentifierExists
    , prorOnlineOnly
    , prorBrand
    , prorUnitPricingMeasure
    , prorSalePrice
    , prorShippingLength
    , prorCustomLabel3
    , prorMaxHandlingTime
    , prorWarnings
    , prorAdditionalProductTypes
    , prorAvailability
    , prorTargetCountry
    , prorShippingLabel
    , prorCustomAttributes
    , prorGtin
    , prorAgeGroup
    , prorDisplayAdsTitle
    , prorGender
    , prorDestinations
    , prorExpirationDate
    , prorItemGroupId
    , prorAdwordsGrouping
    , prorSalePriceEffectiveDate
    , prorCustomLabel2
    , prorGoogleProductCategory
    , prorShipping
    , prorAdwordsRedirect
    , prorShippingWeight
    , prorSellOnGoogleQuantity
    , prorShippingHeight
    , prorAvailabilityDate
    , prorOfferId
    , prorId
    , prorAdwordsLabels
    , prorPrice
    , prorPromotionIds
    , prorSizeType
    , prorMobileLink
    , prorTitle
    , prorAdult
    , prorContentLanguage
    , prorAspects
    , prorEnergyEfficiencyClass
    , prorDisplayAdsSimilarIds
    , prorMpn
    , prorCondition
    , prorSizes
    , prorIsBundle
    , prorDescription
    , prorCustomLabel4
    , prorDisplayAdsId

    -- * Errors
    , Errors
    , errors
    , errCode
    , errMessage
    , errErrors

    -- * AccountstatusesCustomBatchResponseEntry
    , AccountstatusesCustomBatchResponseEntry
    , accountstatusesCustomBatchResponseEntry
    , aaAccountStatus
    , aaErrors
    , aaBatchId

    -- * InventorySetResponse
    , InventorySetResponse
    , inventorySetResponse
    , isrKind

    -- * OrdersCancelLineItemResponse
    , OrdersCancelLineItemResponse
    , ordersCancelLineItemResponse
    , oclirKind
    , oclirExecutionStatus

    -- * HolidayCutoff
    , HolidayCutoff
    , holidayCutoff
    , hcDeadlineHour
    , hcDeadlineTimezone
    , hcVisibleFromDate
    , hcHolidayId
    , hcDeadlineDate

    -- * TestOrderLineItem
    , TestOrderLineItem
    , testOrderLineItem
    , toliQuantityOrdered
    , toliReturnInfo
    , toliShippingDetails
    , toliProduct
    , toliUnitTax

    -- * ProductstatusesCustomBatchRequestEntry
    , ProductstatusesCustomBatchRequestEntry
    , productstatusesCustomBatchRequestEntry
    , p2MerchantId
    , p2Method
    , p2IncludeAttributes
    , p2ProductId
    , p2BatchId

    -- * ShippingSettingsCustomBatchResponse
    , ShippingSettingsCustomBatchResponse
    , shippingSettingsCustomBatchResponse
    , shiEntries
    , shiKind

    -- * OrdersReturnRefundLineItemRequest
    , OrdersReturnRefundLineItemRequest
    , ordersReturnRefundLineItemRequest
    , orrlirrQuantity
    , orrlirrLineItemId
    , orrlirrReason
    , orrlirrOperationId
    , orrlirrAmountPretax
    , orrlirrProductId
    , orrlirrAmountTax
    , orrlirrReasonText

    -- * OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , ordersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , ocbreslisiCarrier
    , ocbreslisiTrackingId
    , ocbreslisiShipmentId

    -- * ProductAspect
    , ProductAspect
    , productAspect
    , paIntention
    , paAspectName
    , paDestinationName

    -- * DatafeedTarget
    , DatafeedTarget
    , datafeedTarget
    , dtIncludedDestinations
    , dtExcludedDestinations
    , dtCountry
    , dtLanguage

    -- * OrdersUpdateMerchantOrderIdResponse
    , OrdersUpdateMerchantOrderIdResponse
    , ordersUpdateMerchantOrderIdResponse
    , oumoirKind
    , oumoirExecutionStatus

    -- * InventoryPickup
    , InventoryPickup
    , inventoryPickup
    , ipPickupSla
    , ipPickupMethod

    -- * DatafeedStatusExample
    , DatafeedStatusExample
    , datafeedStatusExample
    , dseLineNumber
    , dseItemId
    , dseValue

    -- * OrdersAcknowledgeResponse
    , OrdersAcknowledgeResponse
    , ordersAcknowledgeResponse
    , oarKind
    , oarExecutionStatus

    -- * PosSaleRequest
    , PosSaleRequest
    , posSaleRequest
    , psrsStoreCode
    , psrsItemId
    , psrsQuantity
    , psrsTargetCountry
    , psrsGtin
    , psrsPrice
    , psrsContentLanguage
    , psrsTimestamp
    , psrsSaleId

    -- * Table
    , Table
    , table
    , tRows
    , tName
    , tColumnHeaders
    , tRowHeaders

    -- * Order
    , Order
    , order
    , ordrStatus
    , ordrMerchantId
    , ordrRefunds
    , ordrKind
    , ordrLineItems
    , ordrShipments
    , ordrNetAmount
    , ordrPlacedDate
    , ordrDeliveryDetails
    , ordrShippingOption
    , ordrMerchantOrderId
    , ordrAcknowledged
    , ordrShippingCostTax
    , ordrCustomer
    , ordrId
    , ordrPaymentMethod
    , ordrPromotions
    , ordrChannelType
    , ordrPaymentStatus
    , ordrShippingCost

    -- * InventoryCustomBatchResponse
    , InventoryCustomBatchResponse
    , inventoryCustomBatchResponse
    , invEntries
    , invKind

    -- * OrderLineItemProduct
    , OrderLineItemProduct
    , orderLineItemProduct
    , olipImageLink
    , olipShownImage
    , olipChannel
    , olipBrand
    , olipTargetCountry
    , olipGtin
    , olipItemGroupId
    , olipOfferId
    , olipId
    , olipPrice
    , olipVariantAttributes
    , olipTitle
    , olipContentLanguage
    , olipMpn
    , olipCondition

    -- * AccounttaxCustomBatchRequestEntry
    , AccounttaxCustomBatchRequestEntry
    , accounttaxCustomBatchRequestEntry
    , acccAccountTax
    , acccMerchantId
    , acccAccountId
    , acccMethod
    , acccBatchId

    -- * DatafeedStatusError
    , DatafeedStatusError
    , datafeedStatusError
    , dseCount
    , dseCode
    , dseMessage
    , dseExamples

    -- * ProductsCustomBatchRequest
    , ProductsCustomBatchRequest
    , productsCustomBatchRequest
    , ppEntries

    -- * OrdersCustomBatchRequestEntryReturnLineItem
    , OrdersCustomBatchRequestEntryReturnLineItem
    , ordersCustomBatchRequestEntryReturnLineItem
    , ocbrerliQuantity
    , ocbrerliLineItemId
    , ocbrerliReason
    , ocbrerliProductId
    , ocbrerliReasonText

    -- * PosInventory
    , PosInventory
    , posInventory
    , piStoreCode
    , piKind
    , piItemId
    , piQuantity
    , piTargetCountry
    , piGtin
    , piPrice
    , piContentLanguage
    , piTimestamp

    -- * OrdersCustomBatchRequestEntryUpdateShipment
    , OrdersCustomBatchRequestEntryUpdateShipment
    , ordersCustomBatchRequestEntryUpdateShipment
    , ocbreusCarrier
    , ocbreusStatus
    , ocbreusTrackingId
    , ocbreusShipmentId

    -- * DatafeedStatus
    , DatafeedStatus
    , datafeedStatus
    , dsItemsTotal
    , dsCountry
    , dsKind
    , dsWarnings
    , dsDatafeedId
    , dsProcessingStatus
    , dsLanguage
    , dsLastUploadDate
    , dsItemsValid
    , dsErrors

    -- * DatafeedstatusesCustomBatchRequest
    , DatafeedstatusesCustomBatchRequest
    , datafeedstatusesCustomBatchRequest
    , dcbrcEntries

    -- * AccountStatusDataQualityIssue
    , AccountStatusDataQualityIssue
    , accountStatusDataQualityIssue
    , asdqiSubmittedValue
    , asdqiLocation
    , asdqiCountry
    , asdqiDisplayedValue
    , asdqiNumItems
    , asdqiSeverity
    , asdqiExampleItems
    , asdqiLastChecked
    , asdqiId
    , asdqiDetail

    -- * OrdersInStoreRefundLineItemRequest
    , OrdersInStoreRefundLineItemRequest
    , ordersInStoreRefundLineItemRequest
    , oisrlirQuantity
    , oisrlirLineItemId
    , oisrlirReason
    , oisrlirOperationId
    , oisrlirAmountPretax
    , oisrlirProductId
    , oisrlirAmountTax
    , oisrlirReasonText

    -- * ProductShippingDimension
    , ProductShippingDimension
    , productShippingDimension
    , psdValue
    , psdUnit

    -- * DatafeedsCustomBatchResponseEntry
    , DatafeedsCustomBatchResponseEntry
    , datafeedsCustomBatchResponseEntry
    , dcbrecDatafeed
    , dcbrecErrors
    , dcbrecBatchId

    -- * OrdersCustomBatchRequestEntryRefund
    , OrdersCustomBatchRequestEntryRefund
    , ordersCustomBatchRequestEntryRefund
    , ocbrerAmount
    , ocbrerReason
    , ocbrerAmountPretax
    , ocbrerAmountTax
    , ocbrerReasonText

    -- * DatafeedstatusesListResponse
    , DatafeedstatusesListResponse
    , datafeedstatusesListResponse
    , dlrlNextPageToken
    , dlrlKind
    , dlrlResources

    -- * ProductsListResponse
    , ProductsListResponse
    , productsListResponse
    , plr1NextPageToken
    , plr1Kind
    , plr1Resources

    -- * AccountAdwordsLink
    , AccountAdwordsLink
    , accountAdwordsLink
    , aalStatus
    , aalAdwordsId

    -- * OrderCancellation
    , OrderCancellation
    , orderCancellation
    , ocQuantity
    , ocActor
    , ocReason
    , ocCreationDate
    , ocReasonText

    -- * OrdersCustomBatchResponse
    , OrdersCustomBatchResponse
    , ordersCustomBatchResponse
    , ocbrcEntries
    , ocbrcKind
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types.Product
import Network.Google.ShoppingContent.Types.Sum

-- | Default request referring to version 'v2' of the Content API for Shopping. This contains the host and root path used as a starting point for constructing service requests.
shoppingContentService :: ServiceConfig
shoppingContentService
  = defaultService (ServiceId "content:v2")
      "www.googleapis.com"

-- | Manage your product listings and accounts for Google Shopping
contentScope :: Proxy '["https://www.googleapis.com/auth/content"]
contentScope = Proxy;
