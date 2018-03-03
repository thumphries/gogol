{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Billing.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Billing.Types.Product where

import Network.Google.Billing.Types.Sum
import Network.Google.Prelude

-- | Response message for \`ListServices\`.
--
-- /See:/ 'listServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
    { _lsrNextPageToken :: !(Maybe Text)
    , _lsrServices :: !(Maybe [Service])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrNextPageToken'
--
-- * 'lsrServices'
listServicesResponse
    :: ListServicesResponse
listServicesResponse = 
    ListServicesResponse'
    { _lsrNextPageToken = Nothing
    , _lsrServices = Nothing
    }

-- | A token to retrieve the next page of results. To retrieve the next page,
-- call \`ListServices\` again with the \`page_token\` field set to this
-- value. This field is empty if there are no more results to retrieve.
lsrNextPageToken :: Lens' ListServicesResponse (Maybe Text)
lsrNextPageToken
  = lens _lsrNextPageToken
      (\ s a -> s{_lsrNextPageToken = a})

-- | A list of services.
lsrServices :: Lens' ListServicesResponse [Service]
lsrServices
  = lens _lsrServices (\ s a -> s{_lsrServices = a}) .
      _Default
      . _Coerce

instance FromJSON ListServicesResponse where
        parseJSON
          = withObject "ListServicesResponse"
              (\ o ->
                 ListServicesResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "services" .!= mempty))

instance ToJSON ListServicesResponse where
        toJSON ListServicesResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lsrNextPageToken,
                  ("services" .=) <$> _lsrServices])

-- | A billing account in [Google Cloud
-- Console](https:\/\/console.cloud.google.com\/). You can assign a billing
-- account to one or more projects.
--
-- /See:/ 'billingAccount' smart constructor.
data BillingAccount = BillingAccount'
    { _baOpen :: !(Maybe Bool)
    , _baName :: !(Maybe Text)
    , _baDisplayName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BillingAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baOpen'
--
-- * 'baName'
--
-- * 'baDisplayName'
billingAccount
    :: BillingAccount
billingAccount = 
    BillingAccount'
    { _baOpen = Nothing
    , _baName = Nothing
    , _baDisplayName = Nothing
    }

-- | True if the billing account is open, and will therefore be charged for
-- any usage on associated projects. False if the billing account is
-- closed, and therefore projects associated with it will be unable to use
-- paid services.
baOpen :: Lens' BillingAccount (Maybe Bool)
baOpen = lens _baOpen (\ s a -> s{_baOpen = a})

-- | The resource name of the billing account. The resource name has the form
-- \`billingAccounts\/{billing_account_id}\`. For example,
-- \`billingAccounts\/012345-567890-ABCDEF\` would be the resource name for
-- billing account \`012345-567890-ABCDEF\`.
baName :: Lens' BillingAccount (Maybe Text)
baName = lens _baName (\ s a -> s{_baName = a})

-- | The display name given to the billing account, such as \`My Billing
-- Account\`. This name is displayed in the Google Cloud Console.
baDisplayName :: Lens' BillingAccount (Maybe Text)
baDisplayName
  = lens _baDisplayName
      (\ s a -> s{_baDisplayName = a})

instance FromJSON BillingAccount where
        parseJSON
          = withObject "BillingAccount"
              (\ o ->
                 BillingAccount' <$>
                   (o .:? "open") <*> (o .:? "name") <*>
                     (o .:? "displayName"))

instance ToJSON BillingAccount where
        toJSON BillingAccount'{..}
          = object
              (catMaybes
                 [("open" .=) <$> _baOpen, ("name" .=) <$> _baName,
                  ("displayName" .=) <$> _baDisplayName])

-- | Encapsulates a single service in Google Cloud Platform.
--
-- /See:/ 'service' smart constructor.
data Service = Service'
    { _sName :: !(Maybe Text)
    , _sDisplayName :: !(Maybe Text)
    , _sServiceId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Service' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName'
--
-- * 'sDisplayName'
--
-- * 'sServiceId'
service
    :: Service
service = 
    Service'
    { _sName = Nothing
    , _sDisplayName = Nothing
    , _sServiceId = Nothing
    }

-- | The resource name for the service. Example: \"services\/DA34-426B-A397\"
sName :: Lens' Service (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | A human readable display name for this service.
sDisplayName :: Lens' Service (Maybe Text)
sDisplayName
  = lens _sDisplayName (\ s a -> s{_sDisplayName = a})

-- | The identifier for the service. Example: \"DA34-426B-A397\"
sServiceId :: Lens' Service (Maybe Text)
sServiceId
  = lens _sServiceId (\ s a -> s{_sServiceId = a})

instance FromJSON Service where
        parseJSON
          = withObject "Service"
              (\ o ->
                 Service' <$>
                   (o .:? "name") <*> (o .:? "displayName") <*>
                     (o .:? "serviceId"))

instance ToJSON Service where
        toJSON Service'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _sName,
                  ("displayName" .=) <$> _sDisplayName,
                  ("serviceId" .=) <$> _sServiceId])

-- | Encapsulation of billing information for a Cloud Console project. A
-- project has at most one associated billing account at a time (but a
-- billing account can be assigned to multiple projects).
--
-- /See:/ 'projectBillingInfo' smart constructor.
data ProjectBillingInfo = ProjectBillingInfo'
    { _pbiName :: !(Maybe Text)
    , _pbiBillingAccountName :: !(Maybe Text)
    , _pbiProjectId :: !(Maybe Text)
    , _pbiBillingEnabled :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectBillingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbiName'
--
-- * 'pbiBillingAccountName'
--
-- * 'pbiProjectId'
--
-- * 'pbiBillingEnabled'
projectBillingInfo
    :: ProjectBillingInfo
projectBillingInfo = 
    ProjectBillingInfo'
    { _pbiName = Nothing
    , _pbiBillingAccountName = Nothing
    , _pbiProjectId = Nothing
    , _pbiBillingEnabled = Nothing
    }

-- | The resource name for the \`ProjectBillingInfo\`; has the form
-- \`projects\/{project_id}\/billingInfo\`. For example, the resource name
-- for the billing information for project \`tokyo-rain-123\` would be
-- \`projects\/tokyo-rain-123\/billingInfo\`. This field is read-only.
pbiName :: Lens' ProjectBillingInfo (Maybe Text)
pbiName = lens _pbiName (\ s a -> s{_pbiName = a})

-- | The resource name of the billing account associated with the project, if
-- any. For example, \`billingAccounts\/012345-567890-ABCDEF\`.
pbiBillingAccountName :: Lens' ProjectBillingInfo (Maybe Text)
pbiBillingAccountName
  = lens _pbiBillingAccountName
      (\ s a -> s{_pbiBillingAccountName = a})

-- | The ID of the project that this \`ProjectBillingInfo\` represents, such
-- as \`tokyo-rain-123\`. This is a convenience field so that you don\'t
-- need to parse the \`name\` field to obtain a project ID. This field is
-- read-only.
pbiProjectId :: Lens' ProjectBillingInfo (Maybe Text)
pbiProjectId
  = lens _pbiProjectId (\ s a -> s{_pbiProjectId = a})

-- | True if the project is associated with an open billing account, to which
-- usage on the project is charged. False if the project is associated with
-- a closed billing account, or no billing account at all, and therefore
-- cannot use paid services. This field is read-only.
pbiBillingEnabled :: Lens' ProjectBillingInfo (Maybe Bool)
pbiBillingEnabled
  = lens _pbiBillingEnabled
      (\ s a -> s{_pbiBillingEnabled = a})

instance FromJSON ProjectBillingInfo where
        parseJSON
          = withObject "ProjectBillingInfo"
              (\ o ->
                 ProjectBillingInfo' <$>
                   (o .:? "name") <*> (o .:? "billingAccountName") <*>
                     (o .:? "projectId")
                     <*> (o .:? "billingEnabled"))

instance ToJSON ProjectBillingInfo where
        toJSON ProjectBillingInfo'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _pbiName,
                  ("billingAccountName" .=) <$> _pbiBillingAccountName,
                  ("projectId" .=) <$> _pbiProjectId,
                  ("billingEnabled" .=) <$> _pbiBillingEnabled])

-- | The price rate indicating starting usage and its corresponding price.
--
-- /See:/ 'tierRate' smart constructor.
data TierRate = TierRate'
    { _trUnitPrice :: !(Maybe Money)
    , _trStartUsageAmount :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TierRate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trUnitPrice'
--
-- * 'trStartUsageAmount'
tierRate
    :: TierRate
tierRate = 
    TierRate'
    { _trUnitPrice = Nothing
    , _trStartUsageAmount = Nothing
    }

-- | The price per unit of usage. Example: unit_price of amount $10 indicates
-- that each unit will cost $10.
trUnitPrice :: Lens' TierRate (Maybe Money)
trUnitPrice
  = lens _trUnitPrice (\ s a -> s{_trUnitPrice = a})

-- | Usage is priced at this rate only after this amount. Example:
-- start_usage_amount of 10 indicates that the usage will be priced at the
-- unit_price after the first 10 usage_units.
trStartUsageAmount :: Lens' TierRate (Maybe Double)
trStartUsageAmount
  = lens _trStartUsageAmount
      (\ s a -> s{_trStartUsageAmount = a})
      . mapping _Coerce

instance FromJSON TierRate where
        parseJSON
          = withObject "TierRate"
              (\ o ->
                 TierRate' <$>
                   (o .:? "unitPrice") <*> (o .:? "startUsageAmount"))

instance ToJSON TierRate where
        toJSON TierRate'{..}
          = object
              (catMaybes
                 [("unitPrice" .=) <$> _trUnitPrice,
                  ("startUsageAmount" .=) <$> _trStartUsageAmount])

-- | Represents an amount of money with its currency type.
--
-- /See:/ 'money' smart constructor.
data Money = Money'
    { _mCurrencyCode :: !(Maybe Text)
    , _mNanos :: !(Maybe (Textual Int32))
    , _mUnits :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Money' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mCurrencyCode'
--
-- * 'mNanos'
--
-- * 'mUnits'
money
    :: Money
money = 
    Money'
    { _mCurrencyCode = Nothing
    , _mNanos = Nothing
    , _mUnits = Nothing
    }

-- | The 3-letter currency code defined in ISO 4217.
mCurrencyCode :: Lens' Money (Maybe Text)
mCurrencyCode
  = lens _mCurrencyCode
      (\ s a -> s{_mCurrencyCode = a})

-- | Number of nano (10^-9) units of the amount. The value must be between
-- -999,999,999 and +999,999,999 inclusive. If \`units\` is positive,
-- \`nanos\` must be positive or zero. If \`units\` is zero, \`nanos\` can
-- be positive, zero, or negative. If \`units\` is negative, \`nanos\` must
-- be negative or zero. For example $-1.75 is represented as \`units\`=-1
-- and \`nanos\`=-750,000,000.
mNanos :: Lens' Money (Maybe Int32)
mNanos
  = lens _mNanos (\ s a -> s{_mNanos = a}) .
      mapping _Coerce

-- | The whole units of the amount. For example if \`currencyCode\` is
-- \`\"USD\"\`, then 1 unit is one US dollar.
mUnits :: Lens' Money (Maybe Int64)
mUnits
  = lens _mUnits (\ s a -> s{_mUnits = a}) .
      mapping _Coerce

instance FromJSON Money where
        parseJSON
          = withObject "Money"
              (\ o ->
                 Money' <$>
                   (o .:? "currencyCode") <*> (o .:? "nanos") <*>
                     (o .:? "units"))

instance ToJSON Money where
        toJSON Money'{..}
          = object
              (catMaybes
                 [("currencyCode" .=) <$> _mCurrencyCode,
                  ("nanos" .=) <$> _mNanos, ("units" .=) <$> _mUnits])

-- | Represents the category hierarchy of a SKU.
--
-- /See:/ 'category' smart constructor.
data Category = Category'
    { _cResourceFamily :: !(Maybe Text)
    , _cUsageType :: !(Maybe Text)
    , _cServiceDisplayName :: !(Maybe Text)
    , _cResourceGroup :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cResourceFamily'
--
-- * 'cUsageType'
--
-- * 'cServiceDisplayName'
--
-- * 'cResourceGroup'
category
    :: Category
category = 
    Category'
    { _cResourceFamily = Nothing
    , _cUsageType = Nothing
    , _cServiceDisplayName = Nothing
    , _cResourceGroup = Nothing
    }

-- | The type of product the SKU refers to. Example: \"Compute\",
-- \"Storage\", \"Network\", \"ApplicationServices\" etc.
cResourceFamily :: Lens' Category (Maybe Text)
cResourceFamily
  = lens _cResourceFamily
      (\ s a -> s{_cResourceFamily = a})

-- | Represents how the SKU is consumed. Example: \"OnDemand\",
-- \"Preemptible\", \"Commit1Mo\", \"Commit1Yr\" etc.
cUsageType :: Lens' Category (Maybe Text)
cUsageType
  = lens _cUsageType (\ s a -> s{_cUsageType = a})

-- | The display name of the service this SKU belongs to.
cServiceDisplayName :: Lens' Category (Maybe Text)
cServiceDisplayName
  = lens _cServiceDisplayName
      (\ s a -> s{_cServiceDisplayName = a})

-- | A group classification for related SKUs. Example: \"RAM\", \"GPU\",
-- \"Prediction\", \"Ops\", \"GoogleEgress\" etc.
cResourceGroup :: Lens' Category (Maybe Text)
cResourceGroup
  = lens _cResourceGroup
      (\ s a -> s{_cResourceGroup = a})

instance FromJSON Category where
        parseJSON
          = withObject "Category"
              (\ o ->
                 Category' <$>
                   (o .:? "resourceFamily") <*> (o .:? "usageType") <*>
                     (o .:? "serviceDisplayName")
                     <*> (o .:? "resourceGroup"))

instance ToJSON Category where
        toJSON Category'{..}
          = object
              (catMaybes
                 [("resourceFamily" .=) <$> _cResourceFamily,
                  ("usageType" .=) <$> _cUsageType,
                  ("serviceDisplayName" .=) <$> _cServiceDisplayName,
                  ("resourceGroup" .=) <$> _cResourceGroup])

-- | Request message for \`ListProjectBillingInfoResponse\`.
--
-- /See:/ 'listProjectBillingInfoResponse' smart constructor.
data ListProjectBillingInfoResponse = ListProjectBillingInfoResponse'
    { _lpbirNextPageToken :: !(Maybe Text)
    , _lpbirProjectBillingInfo :: !(Maybe [ProjectBillingInfo])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListProjectBillingInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpbirNextPageToken'
--
-- * 'lpbirProjectBillingInfo'
listProjectBillingInfoResponse
    :: ListProjectBillingInfoResponse
listProjectBillingInfoResponse = 
    ListProjectBillingInfoResponse'
    { _lpbirNextPageToken = Nothing
    , _lpbirProjectBillingInfo = Nothing
    }

-- | A token to retrieve the next page of results. To retrieve the next page,
-- call \`ListProjectBillingInfo\` again with the \`page_token\` field set
-- to this value. This field is empty if there are no more results to
-- retrieve.
lpbirNextPageToken :: Lens' ListProjectBillingInfoResponse (Maybe Text)
lpbirNextPageToken
  = lens _lpbirNextPageToken
      (\ s a -> s{_lpbirNextPageToken = a})

-- | A list of \`ProjectBillingInfo\` resources representing the projects
-- associated with the billing account.
lpbirProjectBillingInfo :: Lens' ListProjectBillingInfoResponse [ProjectBillingInfo]
lpbirProjectBillingInfo
  = lens _lpbirProjectBillingInfo
      (\ s a -> s{_lpbirProjectBillingInfo = a})
      . _Default
      . _Coerce

instance FromJSON ListProjectBillingInfoResponse
         where
        parseJSON
          = withObject "ListProjectBillingInfoResponse"
              (\ o ->
                 ListProjectBillingInfoResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "projectBillingInfo" .!= mempty))

instance ToJSON ListProjectBillingInfoResponse where
        toJSON ListProjectBillingInfoResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lpbirNextPageToken,
                  ("projectBillingInfo" .=) <$>
                    _lpbirProjectBillingInfo])

-- | Response message for \`ListSkus\`.
--
-- /See:/ 'listSKUsResponse' smart constructor.
data ListSKUsResponse = ListSKUsResponse'
    { _lskurNextPageToken :: !(Maybe Text)
    , _lskurSKUs :: !(Maybe [SKU])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSKUsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lskurNextPageToken'
--
-- * 'lskurSKUs'
listSKUsResponse
    :: ListSKUsResponse
listSKUsResponse = 
    ListSKUsResponse'
    { _lskurNextPageToken = Nothing
    , _lskurSKUs = Nothing
    }

-- | A token to retrieve the next page of results. To retrieve the next page,
-- call \`ListSkus\` again with the \`page_token\` field set to this value.
-- This field is empty if there are no more results to retrieve.
lskurNextPageToken :: Lens' ListSKUsResponse (Maybe Text)
lskurNextPageToken
  = lens _lskurNextPageToken
      (\ s a -> s{_lskurNextPageToken = a})

-- | The list of public SKUs of the given service.
lskurSKUs :: Lens' ListSKUsResponse [SKU]
lskurSKUs
  = lens _lskurSKUs (\ s a -> s{_lskurSKUs = a}) .
      _Default
      . _Coerce

instance FromJSON ListSKUsResponse where
        parseJSON
          = withObject "ListSKUsResponse"
              (\ o ->
                 ListSKUsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "skus" .!= mempty))

instance ToJSON ListSKUsResponse where
        toJSON ListSKUsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lskurNextPageToken,
                  ("skus" .=) <$> _lskurSKUs])

-- | Expresses a mathematical pricing formula. For Example:- \`usage_unit:
-- GBy\` \`tiered_rates:\` \`[start_usage_amount: 20, unit_price: $10]\`
-- \`[start_usage_amount: 100, unit_price: $5]\` The above expresses a
-- pricing formula where the first 20GB is free, the next 80GB is priced at
-- $10 per GB followed by $5 per GB for additional usage.
--
-- /See:/ 'pricingExpression' smart constructor.
data PricingExpression = PricingExpression'
    { _peUsageUnitDescription :: !(Maybe Text)
    , _peBaseUnit :: !(Maybe Text)
    , _peBaseUnitConversionFactor :: !(Maybe (Textual Double))
    , _peDisplayQuantity :: !(Maybe (Textual Double))
    , _peTieredRates :: !(Maybe [TierRate])
    , _peBaseUnitDescription :: !(Maybe Text)
    , _peUsageUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PricingExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peUsageUnitDescription'
--
-- * 'peBaseUnit'
--
-- * 'peBaseUnitConversionFactor'
--
-- * 'peDisplayQuantity'
--
-- * 'peTieredRates'
--
-- * 'peBaseUnitDescription'
--
-- * 'peUsageUnit'
pricingExpression
    :: PricingExpression
pricingExpression = 
    PricingExpression'
    { _peUsageUnitDescription = Nothing
    , _peBaseUnit = Nothing
    , _peBaseUnitConversionFactor = Nothing
    , _peDisplayQuantity = Nothing
    , _peTieredRates = Nothing
    , _peBaseUnitDescription = Nothing
    , _peUsageUnit = Nothing
    }

-- | The unit of usage in human readable form. Example: \"gibi byte\".
peUsageUnitDescription :: Lens' PricingExpression (Maybe Text)
peUsageUnitDescription
  = lens _peUsageUnitDescription
      (\ s a -> s{_peUsageUnitDescription = a})

-- | The base unit for the SKU which is the unit used in usage exports.
-- Example: \"By\"
peBaseUnit :: Lens' PricingExpression (Maybe Text)
peBaseUnit
  = lens _peBaseUnit (\ s a -> s{_peBaseUnit = a})

-- | Conversion factor for converting from price per usage_unit to price per
-- base_unit, and start_usage_amount to start_usage_amount in base_unit.
-- unit_price \/ base_unit_conversion_factor = price per base_unit.
-- start_usage_amount * base_unit_conversion_factor = start_usage_amount in
-- base_unit.
peBaseUnitConversionFactor :: Lens' PricingExpression (Maybe Double)
peBaseUnitConversionFactor
  = lens _peBaseUnitConversionFactor
      (\ s a -> s{_peBaseUnitConversionFactor = a})
      . mapping _Coerce

-- | The recommended quantity of units for displaying pricing info. When
-- displaying pricing info it is recommended to display: (unit_price *
-- display_quantity) per display_quantity usage_unit. This field does not
-- affect the pricing formula and is for display purposes only. Example: If
-- the unit_price is \"0.0001 USD\", the usage_unit is \"GB\" and the
-- display_quantity is \"1000\" then the recommended way of displaying the
-- pricing info is \"0.10 USD per 1000 GB\"
peDisplayQuantity :: Lens' PricingExpression (Maybe Double)
peDisplayQuantity
  = lens _peDisplayQuantity
      (\ s a -> s{_peDisplayQuantity = a})
      . mapping _Coerce

-- | The list of tiered rates for this pricing. The total cost is computed by
-- applying each of the tiered rates on usage. This repeated list is sorted
-- by ascending order of start_usage_amount.
peTieredRates :: Lens' PricingExpression [TierRate]
peTieredRates
  = lens _peTieredRates
      (\ s a -> s{_peTieredRates = a})
      . _Default
      . _Coerce

-- | The base unit in human readable form. Example: \"byte\".
peBaseUnitDescription :: Lens' PricingExpression (Maybe Text)
peBaseUnitDescription
  = lens _peBaseUnitDescription
      (\ s a -> s{_peBaseUnitDescription = a})

-- | The short hand for unit of usage this pricing is specified in. Example:
-- usage_unit of \"GiBy\" means that usage is specified in \"Gibi Byte\".
peUsageUnit :: Lens' PricingExpression (Maybe Text)
peUsageUnit
  = lens _peUsageUnit (\ s a -> s{_peUsageUnit = a})

instance FromJSON PricingExpression where
        parseJSON
          = withObject "PricingExpression"
              (\ o ->
                 PricingExpression' <$>
                   (o .:? "usageUnitDescription") <*> (o .:? "baseUnit")
                     <*> (o .:? "baseUnitConversionFactor")
                     <*> (o .:? "displayQuantity")
                     <*> (o .:? "tieredRates" .!= mempty)
                     <*> (o .:? "baseUnitDescription")
                     <*> (o .:? "usageUnit"))

instance ToJSON PricingExpression where
        toJSON PricingExpression'{..}
          = object
              (catMaybes
                 [("usageUnitDescription" .=) <$>
                    _peUsageUnitDescription,
                  ("baseUnit" .=) <$> _peBaseUnit,
                  ("baseUnitConversionFactor" .=) <$>
                    _peBaseUnitConversionFactor,
                  ("displayQuantity" .=) <$> _peDisplayQuantity,
                  ("tieredRates" .=) <$> _peTieredRates,
                  ("baseUnitDescription" .=) <$>
                    _peBaseUnitDescription,
                  ("usageUnit" .=) <$> _peUsageUnit])

-- | Represents the aggregation level and interval for pricing of a single
-- SKU.
--
-- /See:/ 'aggregationInfo' smart constructor.
data AggregationInfo = AggregationInfo'
    { _aiAggregationInterval :: !(Maybe AggregationInfoAggregationInterval)
    , _aiAggregationCount :: !(Maybe (Textual Int32))
    , _aiAggregationLevel :: !(Maybe AggregationInfoAggregationLevel)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AggregationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiAggregationInterval'
--
-- * 'aiAggregationCount'
--
-- * 'aiAggregationLevel'
aggregationInfo
    :: AggregationInfo
aggregationInfo = 
    AggregationInfo'
    { _aiAggregationInterval = Nothing
    , _aiAggregationCount = Nothing
    , _aiAggregationLevel = Nothing
    }

aiAggregationInterval :: Lens' AggregationInfo (Maybe AggregationInfoAggregationInterval)
aiAggregationInterval
  = lens _aiAggregationInterval
      (\ s a -> s{_aiAggregationInterval = a})

-- | The number of intervals to aggregate over. Example: If aggregation_level
-- is \"DAILY\" and aggregation_count is 14, aggregation will be over 14
-- days.
aiAggregationCount :: Lens' AggregationInfo (Maybe Int32)
aiAggregationCount
  = lens _aiAggregationCount
      (\ s a -> s{_aiAggregationCount = a})
      . mapping _Coerce

aiAggregationLevel :: Lens' AggregationInfo (Maybe AggregationInfoAggregationLevel)
aiAggregationLevel
  = lens _aiAggregationLevel
      (\ s a -> s{_aiAggregationLevel = a})

instance FromJSON AggregationInfo where
        parseJSON
          = withObject "AggregationInfo"
              (\ o ->
                 AggregationInfo' <$>
                   (o .:? "aggregationInterval") <*>
                     (o .:? "aggregationCount")
                     <*> (o .:? "aggregationLevel"))

instance ToJSON AggregationInfo where
        toJSON AggregationInfo'{..}
          = object
              (catMaybes
                 [("aggregationInterval" .=) <$>
                    _aiAggregationInterval,
                  ("aggregationCount" .=) <$> _aiAggregationCount,
                  ("aggregationLevel" .=) <$> _aiAggregationLevel])

-- | Encapsulates a single SKU in Google Cloud Platform
--
-- /See:/ 'sKU' smart constructor.
data SKU = SKU'
    { _skukCategory :: !(Maybe Category)
    , _skukSKUId :: !(Maybe Text)
    , _skukServiceProviderName :: !(Maybe Text)
    , _skukServiceRegions :: !(Maybe [Text])
    , _skukName :: !(Maybe Text)
    , _skukPricingInfo :: !(Maybe [PricingInfo])
    , _skukDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SKU' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skukCategory'
--
-- * 'skukSKUId'
--
-- * 'skukServiceProviderName'
--
-- * 'skukServiceRegions'
--
-- * 'skukName'
--
-- * 'skukPricingInfo'
--
-- * 'skukDescription'
sKU
    :: SKU
sKU = 
    SKU'
    { _skukCategory = Nothing
    , _skukSKUId = Nothing
    , _skukServiceProviderName = Nothing
    , _skukServiceRegions = Nothing
    , _skukName = Nothing
    , _skukPricingInfo = Nothing
    , _skukDescription = Nothing
    }

-- | The category hierarchy of this SKU, purely for organizational purpose.
skukCategory :: Lens' SKU (Maybe Category)
skukCategory
  = lens _skukCategory (\ s a -> s{_skukCategory = a})

-- | The identifier for the SKU. Example: \"AA95-CD31-42FE\"
skukSKUId :: Lens' SKU (Maybe Text)
skukSKUId
  = lens _skukSKUId (\ s a -> s{_skukSKUId = a})

-- | Identifies the service provider. This is \'Google\' for first party
-- services in Google Cloud Platform.
skukServiceProviderName :: Lens' SKU (Maybe Text)
skukServiceProviderName
  = lens _skukServiceProviderName
      (\ s a -> s{_skukServiceProviderName = a})

-- | List of service regions this SKU is offered at. Example: \"asia-east1\"
-- Service regions can be found at
-- https:\/\/cloud.google.com\/about\/locations\/
skukServiceRegions :: Lens' SKU [Text]
skukServiceRegions
  = lens _skukServiceRegions
      (\ s a -> s{_skukServiceRegions = a})
      . _Default
      . _Coerce

-- | The resource name for the SKU. Example:
-- \"services\/DA34-426B-A397\/skus\/AA95-CD31-42FE\"
skukName :: Lens' SKU (Maybe Text)
skukName = lens _skukName (\ s a -> s{_skukName = a})

-- | A timeline of pricing info for this SKU in chronological order.
skukPricingInfo :: Lens' SKU [PricingInfo]
skukPricingInfo
  = lens _skukPricingInfo
      (\ s a -> s{_skukPricingInfo = a})
      . _Default
      . _Coerce

-- | A human readable description of the SKU, has a maximum length of 256
-- characters.
skukDescription :: Lens' SKU (Maybe Text)
skukDescription
  = lens _skukDescription
      (\ s a -> s{_skukDescription = a})

instance FromJSON SKU where
        parseJSON
          = withObject "SKU"
              (\ o ->
                 SKU' <$>
                   (o .:? "category") <*> (o .:? "skuId") <*>
                     (o .:? "serviceProviderName")
                     <*> (o .:? "serviceRegions" .!= mempty)
                     <*> (o .:? "name")
                     <*> (o .:? "pricingInfo" .!= mempty)
                     <*> (o .:? "description"))

instance ToJSON SKU where
        toJSON SKU'{..}
          = object
              (catMaybes
                 [("category" .=) <$> _skukCategory,
                  ("skuId" .=) <$> _skukSKUId,
                  ("serviceProviderName" .=) <$>
                    _skukServiceProviderName,
                  ("serviceRegions" .=) <$> _skukServiceRegions,
                  ("name" .=) <$> _skukName,
                  ("pricingInfo" .=) <$> _skukPricingInfo,
                  ("description" .=) <$> _skukDescription])

-- | Response message for \`ListBillingAccounts\`.
--
-- /See:/ 'listBillingAccountsResponse' smart constructor.
data ListBillingAccountsResponse = ListBillingAccountsResponse'
    { _lbarNextPageToken :: !(Maybe Text)
    , _lbarBillingAccounts :: !(Maybe [BillingAccount])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBillingAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbarNextPageToken'
--
-- * 'lbarBillingAccounts'
listBillingAccountsResponse
    :: ListBillingAccountsResponse
listBillingAccountsResponse = 
    ListBillingAccountsResponse'
    { _lbarNextPageToken = Nothing
    , _lbarBillingAccounts = Nothing
    }

-- | A token to retrieve the next page of results. To retrieve the next page,
-- call \`ListBillingAccounts\` again with the \`page_token\` field set to
-- this value. This field is empty if there are no more results to
-- retrieve.
lbarNextPageToken :: Lens' ListBillingAccountsResponse (Maybe Text)
lbarNextPageToken
  = lens _lbarNextPageToken
      (\ s a -> s{_lbarNextPageToken = a})

-- | A list of billing accounts.
lbarBillingAccounts :: Lens' ListBillingAccountsResponse [BillingAccount]
lbarBillingAccounts
  = lens _lbarBillingAccounts
      (\ s a -> s{_lbarBillingAccounts = a})
      . _Default
      . _Coerce

instance FromJSON ListBillingAccountsResponse where
        parseJSON
          = withObject "ListBillingAccountsResponse"
              (\ o ->
                 ListBillingAccountsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "billingAccounts" .!= mempty))

instance ToJSON ListBillingAccountsResponse where
        toJSON ListBillingAccountsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lbarNextPageToken,
                  ("billingAccounts" .=) <$> _lbarBillingAccounts])

-- | Represents the pricing information for a SKU at a single point of time.
--
-- /See:/ 'pricingInfo' smart constructor.
data PricingInfo = PricingInfo'
    { _piSummary :: !(Maybe Text)
    , _piAggregationInfo :: !(Maybe AggregationInfo)
    , _piPricingExpression :: !(Maybe PricingExpression)
    , _piCurrencyConversionRate :: !(Maybe (Textual Double))
    , _piEffectiveTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PricingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piSummary'
--
-- * 'piAggregationInfo'
--
-- * 'piPricingExpression'
--
-- * 'piCurrencyConversionRate'
--
-- * 'piEffectiveTime'
pricingInfo
    :: PricingInfo
pricingInfo = 
    PricingInfo'
    { _piSummary = Nothing
    , _piAggregationInfo = Nothing
    , _piPricingExpression = Nothing
    , _piCurrencyConversionRate = Nothing
    , _piEffectiveTime = Nothing
    }

-- | An optional human readable summary of the pricing information, has a
-- maximum length of 256 characters.
piSummary :: Lens' PricingInfo (Maybe Text)
piSummary
  = lens _piSummary (\ s a -> s{_piSummary = a})

-- | Aggregation Info. This can be left unspecified if the pricing expression
-- doesn\'t require aggregation.
piAggregationInfo :: Lens' PricingInfo (Maybe AggregationInfo)
piAggregationInfo
  = lens _piAggregationInfo
      (\ s a -> s{_piAggregationInfo = a})

-- | Expresses the pricing formula. See \`PricingExpression\` for an example.
piPricingExpression :: Lens' PricingInfo (Maybe PricingExpression)
piPricingExpression
  = lens _piPricingExpression
      (\ s a -> s{_piPricingExpression = a})

-- | Conversion rate used for currency conversion, from USD to the currency
-- specified in the request. This includes any surcharge collected for
-- billing in non USD currency. If a currency is not specified in the
-- request this defaults to 1.0. Example: USD * currency_conversion_rate =
-- JPY
piCurrencyConversionRate :: Lens' PricingInfo (Maybe Double)
piCurrencyConversionRate
  = lens _piCurrencyConversionRate
      (\ s a -> s{_piCurrencyConversionRate = a})
      . mapping _Coerce

-- | The timestamp from which this pricing was effective within the requested
-- time range. This is guaranteed to be greater than or equal to the
-- start_time field in the request and less than the end_time field in the
-- request. If a time range was not specified in the request this field
-- will be equivalent to a time within the last 12 hours, indicating the
-- latest pricing info.
piEffectiveTime :: Lens' PricingInfo (Maybe UTCTime)
piEffectiveTime
  = lens _piEffectiveTime
      (\ s a -> s{_piEffectiveTime = a})
      . mapping _DateTime

instance FromJSON PricingInfo where
        parseJSON
          = withObject "PricingInfo"
              (\ o ->
                 PricingInfo' <$>
                   (o .:? "summary") <*> (o .:? "aggregationInfo") <*>
                     (o .:? "pricingExpression")
                     <*> (o .:? "currencyConversionRate")
                     <*> (o .:? "effectiveTime"))

instance ToJSON PricingInfo where
        toJSON PricingInfo'{..}
          = object
              (catMaybes
                 [("summary" .=) <$> _piSummary,
                  ("aggregationInfo" .=) <$> _piAggregationInfo,
                  ("pricingExpression" .=) <$> _piPricingExpression,
                  ("currencyConversionRate" .=) <$>
                    _piCurrencyConversionRate,
                  ("effectiveTime" .=) <$> _piEffectiveTime])
