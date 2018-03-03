{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ShoppingContent.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ShoppingContent.Types.Product where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types.Sum

--
-- /See:/ 'ordersAcknowledgeRequest' smart constructor.
newtype OrdersAcknowledgeRequest = OrdersAcknowledgeRequest'
    { _oarOperationId :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersAcknowledgeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oarOperationId'
ordersAcknowledgeRequest
    :: OrdersAcknowledgeRequest
ordersAcknowledgeRequest = 
    OrdersAcknowledgeRequest'
    { _oarOperationId = Nothing
    }

-- | The ID of the operation. Unique across all operations for a given order.
oarOperationId :: Lens' OrdersAcknowledgeRequest (Maybe Text)
oarOperationId
  = lens _oarOperationId
      (\ s a -> s{_oarOperationId = a})

instance FromJSON OrdersAcknowledgeRequest where
        parseJSON
          = withObject "OrdersAcknowledgeRequest"
              (\ o ->
                 OrdersAcknowledgeRequest' <$> (o .:? "operationId"))

instance ToJSON OrdersAcknowledgeRequest where
        toJSON OrdersAcknowledgeRequest'{..}
          = object
              (catMaybes [("operationId" .=) <$> _oarOperationId])

-- | The tax settings of a merchant account.
--
-- /See:/ 'accountTax' smart constructor.
data AccountTax = AccountTax'
    { _atRules :: !(Maybe [AccountTaxTaxRule])
    , _atKind :: !Text
    , _atAccountId :: !(Maybe (Textual Word64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountTax' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atRules'
--
-- * 'atKind'
--
-- * 'atAccountId'
accountTax
    :: AccountTax
accountTax = 
    AccountTax'
    { _atRules = Nothing
    , _atKind = "content#accountTax"
    , _atAccountId = Nothing
    }

-- | Tax rules. Updating the tax rules will enable US taxes (not reversible).
-- Defining no rules is equivalent to not charging tax at all.
atRules :: Lens' AccountTax [AccountTaxTaxRule]
atRules
  = lens _atRules (\ s a -> s{_atRules = a}) . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountTax\".
atKind :: Lens' AccountTax Text
atKind = lens _atKind (\ s a -> s{_atKind = a})

-- | The ID of the account to which these account tax settings belong.
atAccountId :: Lens' AccountTax (Maybe Word64)
atAccountId
  = lens _atAccountId (\ s a -> s{_atAccountId = a}) .
      mapping _Coerce

instance FromJSON AccountTax where
        parseJSON
          = withObject "AccountTax"
              (\ o ->
                 AccountTax' <$>
                   (o .:? "rules" .!= mempty) <*>
                     (o .:? "kind" .!= "content#accountTax")
                     <*> (o .:? "accountId"))

instance ToJSON AccountTax where
        toJSON AccountTax'{..}
          = object
              (catMaybes
                 [("rules" .=) <$> _atRules, Just ("kind" .= _atKind),
                  ("accountId" .=) <$> _atAccountId])

--
-- /See:/ 'ordersUpdateMerchantOrderIdRequest' smart constructor.
data OrdersUpdateMerchantOrderIdRequest = OrdersUpdateMerchantOrderIdRequest'
    { _oumoirMerchantOrderId :: !(Maybe Text)
    , _oumoirOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateMerchantOrderIdRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oumoirMerchantOrderId'
--
-- * 'oumoirOperationId'
ordersUpdateMerchantOrderIdRequest
    :: OrdersUpdateMerchantOrderIdRequest
ordersUpdateMerchantOrderIdRequest = 
    OrdersUpdateMerchantOrderIdRequest'
    { _oumoirMerchantOrderId = Nothing
    , _oumoirOperationId = Nothing
    }

-- | The merchant order id to be assigned to the order. Must be unique per
-- merchant.
oumoirMerchantOrderId :: Lens' OrdersUpdateMerchantOrderIdRequest (Maybe Text)
oumoirMerchantOrderId
  = lens _oumoirMerchantOrderId
      (\ s a -> s{_oumoirMerchantOrderId = a})

-- | The ID of the operation. Unique across all operations for a given order.
oumoirOperationId :: Lens' OrdersUpdateMerchantOrderIdRequest (Maybe Text)
oumoirOperationId
  = lens _oumoirOperationId
      (\ s a -> s{_oumoirOperationId = a})

instance FromJSON OrdersUpdateMerchantOrderIdRequest
         where
        parseJSON
          = withObject "OrdersUpdateMerchantOrderIdRequest"
              (\ o ->
                 OrdersUpdateMerchantOrderIdRequest' <$>
                   (o .:? "merchantOrderId") <*> (o .:? "operationId"))

instance ToJSON OrdersUpdateMerchantOrderIdRequest
         where
        toJSON OrdersUpdateMerchantOrderIdRequest'{..}
          = object
              (catMaybes
                 [("merchantOrderId" .=) <$> _oumoirMerchantOrderId,
                  ("operationId" .=) <$> _oumoirOperationId])

--
-- /See:/ 'ordersAdvanceTestOrderResponse' smart constructor.
newtype OrdersAdvanceTestOrderResponse = OrdersAdvanceTestOrderResponse'
    { _oatorKind :: Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersAdvanceTestOrderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oatorKind'
ordersAdvanceTestOrderResponse
    :: OrdersAdvanceTestOrderResponse
ordersAdvanceTestOrderResponse = 
    OrdersAdvanceTestOrderResponse'
    { _oatorKind = "content#ordersAdvanceTestOrderResponse"
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersAdvanceTestOrderResponse\".
oatorKind :: Lens' OrdersAdvanceTestOrderResponse Text
oatorKind
  = lens _oatorKind (\ s a -> s{_oatorKind = a})

instance FromJSON OrdersAdvanceTestOrderResponse
         where
        parseJSON
          = withObject "OrdersAdvanceTestOrderResponse"
              (\ o ->
                 OrdersAdvanceTestOrderResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersAdvanceTestOrderResponse"))

instance ToJSON OrdersAdvanceTestOrderResponse where
        toJSON OrdersAdvanceTestOrderResponse'{..}
          = object (catMaybes [Just ("kind" .= _oatorKind)])

--
-- /See:/ 'productsCustomBatchResponse' smart constructor.
data ProductsCustomBatchResponse = ProductsCustomBatchResponse'
    { _pcbrEntries :: !(Maybe [ProductsCustomBatchResponseEntry])
    , _pcbrKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbrEntries'
--
-- * 'pcbrKind'
productsCustomBatchResponse
    :: ProductsCustomBatchResponse
productsCustomBatchResponse = 
    ProductsCustomBatchResponse'
    { _pcbrEntries = Nothing
    , _pcbrKind = "content#productsCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
pcbrEntries :: Lens' ProductsCustomBatchResponse [ProductsCustomBatchResponseEntry]
pcbrEntries
  = lens _pcbrEntries (\ s a -> s{_pcbrEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productsCustomBatchResponse\".
pcbrKind :: Lens' ProductsCustomBatchResponse Text
pcbrKind = lens _pcbrKind (\ s a -> s{_pcbrKind = a})

instance FromJSON ProductsCustomBatchResponse where
        parseJSON
          = withObject "ProductsCustomBatchResponse"
              (\ o ->
                 ProductsCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#productsCustomBatchResponse"))

instance ToJSON ProductsCustomBatchResponse where
        toJSON ProductsCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _pcbrEntries,
                  Just ("kind" .= _pcbrKind)])

--
-- /See:/ 'orderMerchantProvidedAnnotation' smart constructor.
data OrderMerchantProvidedAnnotation = OrderMerchantProvidedAnnotation'
    { _ompaValue :: !(Maybe Text)
    , _ompaKey :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderMerchantProvidedAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ompaValue'
--
-- * 'ompaKey'
orderMerchantProvidedAnnotation
    :: OrderMerchantProvidedAnnotation
orderMerchantProvidedAnnotation = 
    OrderMerchantProvidedAnnotation'
    { _ompaValue = Nothing
    , _ompaKey = Nothing
    }

-- | Value for additional merchant provided (as key-value pairs) annotation
-- about the line item.
ompaValue :: Lens' OrderMerchantProvidedAnnotation (Maybe Text)
ompaValue
  = lens _ompaValue (\ s a -> s{_ompaValue = a})

-- | Key for additional merchant provided (as key-value pairs) annotation
-- about the line item.
ompaKey :: Lens' OrderMerchantProvidedAnnotation (Maybe Text)
ompaKey = lens _ompaKey (\ s a -> s{_ompaKey = a})

instance FromJSON OrderMerchantProvidedAnnotation
         where
        parseJSON
          = withObject "OrderMerchantProvidedAnnotation"
              (\ o ->
                 OrderMerchantProvidedAnnotation' <$>
                   (o .:? "value") <*> (o .:? "key"))

instance ToJSON OrderMerchantProvidedAnnotation where
        toJSON OrderMerchantProvidedAnnotation'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _ompaValue,
                  ("key" .=) <$> _ompaKey])

--
-- /See:/ 'testOrderCustomer' smart constructor.
data TestOrderCustomer = TestOrderCustomer'
    { _tocFullName :: !(Maybe Text)
    , _tocEmail :: !(Maybe Text)
    , _tocExplicitMarketingPreference :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestOrderCustomer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tocFullName'
--
-- * 'tocEmail'
--
-- * 'tocExplicitMarketingPreference'
testOrderCustomer
    :: TestOrderCustomer
testOrderCustomer = 
    TestOrderCustomer'
    { _tocFullName = Nothing
    , _tocEmail = Nothing
    , _tocExplicitMarketingPreference = Nothing
    }

-- | Full name of the customer.
tocFullName :: Lens' TestOrderCustomer (Maybe Text)
tocFullName
  = lens _tocFullName (\ s a -> s{_tocFullName = a})

-- | Email address of the customer.
tocEmail :: Lens' TestOrderCustomer (Maybe Text)
tocEmail = lens _tocEmail (\ s a -> s{_tocEmail = a})

-- | If set, this indicates the user explicitly chose to opt in or out of
-- providing marketing rights to the merchant. If unset, this indicates the
-- user has already made this choice in a previous purchase, and was thus
-- not shown the marketing right opt in\/out checkbox during the checkout
-- flow. Optional.
tocExplicitMarketingPreference :: Lens' TestOrderCustomer (Maybe Bool)
tocExplicitMarketingPreference
  = lens _tocExplicitMarketingPreference
      (\ s a -> s{_tocExplicitMarketingPreference = a})

instance FromJSON TestOrderCustomer where
        parseJSON
          = withObject "TestOrderCustomer"
              (\ o ->
                 TestOrderCustomer' <$>
                   (o .:? "fullName") <*> (o .:? "email") <*>
                     (o .:? "explicitMarketingPreference"))

instance ToJSON TestOrderCustomer where
        toJSON TestOrderCustomer'{..}
          = object
              (catMaybes
                 [("fullName" .=) <$> _tocFullName,
                  ("email" .=) <$> _tocEmail,
                  ("explicitMarketingPreference" .=) <$>
                    _tocExplicitMarketingPreference])

--
-- /See:/ 'datafeedstatusesCustomBatchResponse' smart constructor.
data DatafeedstatusesCustomBatchResponse = DatafeedstatusesCustomBatchResponse'
    { _dcbrEntries :: !(Maybe [DatafeedstatusesCustomBatchResponseEntry])
    , _dcbrKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedstatusesCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrEntries'
--
-- * 'dcbrKind'
datafeedstatusesCustomBatchResponse
    :: DatafeedstatusesCustomBatchResponse
datafeedstatusesCustomBatchResponse = 
    DatafeedstatusesCustomBatchResponse'
    { _dcbrEntries = Nothing
    , _dcbrKind = "content#datafeedstatusesCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
dcbrEntries :: Lens' DatafeedstatusesCustomBatchResponse [DatafeedstatusesCustomBatchResponseEntry]
dcbrEntries
  = lens _dcbrEntries (\ s a -> s{_dcbrEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeedstatusesCustomBatchResponse\".
dcbrKind :: Lens' DatafeedstatusesCustomBatchResponse Text
dcbrKind = lens _dcbrKind (\ s a -> s{_dcbrKind = a})

instance FromJSON DatafeedstatusesCustomBatchResponse
         where
        parseJSON
          = withObject "DatafeedstatusesCustomBatchResponse"
              (\ o ->
                 DatafeedstatusesCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#datafeedstatusesCustomBatchResponse"))

instance ToJSON DatafeedstatusesCustomBatchResponse
         where
        toJSON DatafeedstatusesCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _dcbrEntries,
                  Just ("kind" .= _dcbrKind)])

--
-- /See:/ 'orderReturn' smart constructor.
data OrderReturn = OrderReturn'
    { _orQuantity :: !(Maybe (Textual Word32))
    , _orActor :: !(Maybe Text)
    , _orReason :: !(Maybe Text)
    , _orCreationDate :: !(Maybe Text)
    , _orReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderReturn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orQuantity'
--
-- * 'orActor'
--
-- * 'orReason'
--
-- * 'orCreationDate'
--
-- * 'orReasonText'
orderReturn
    :: OrderReturn
orderReturn = 
    OrderReturn'
    { _orQuantity = Nothing
    , _orActor = Nothing
    , _orReason = Nothing
    , _orCreationDate = Nothing
    , _orReasonText = Nothing
    }

-- | Quantity that is returned.
orQuantity :: Lens' OrderReturn (Maybe Word32)
orQuantity
  = lens _orQuantity (\ s a -> s{_orQuantity = a}) .
      mapping _Coerce

-- | The actor that created the refund.
orActor :: Lens' OrderReturn (Maybe Text)
orActor = lens _orActor (\ s a -> s{_orActor = a})

-- | The reason for the return.
orReason :: Lens' OrderReturn (Maybe Text)
orReason = lens _orReason (\ s a -> s{_orReason = a})

-- | Date on which the item has been created, in ISO 8601 format.
orCreationDate :: Lens' OrderReturn (Maybe Text)
orCreationDate
  = lens _orCreationDate
      (\ s a -> s{_orCreationDate = a})

-- | The explanation of the reason.
orReasonText :: Lens' OrderReturn (Maybe Text)
orReasonText
  = lens _orReasonText (\ s a -> s{_orReasonText = a})

instance FromJSON OrderReturn where
        parseJSON
          = withObject "OrderReturn"
              (\ o ->
                 OrderReturn' <$>
                   (o .:? "quantity") <*> (o .:? "actor") <*>
                     (o .:? "reason")
                     <*> (o .:? "creationDate")
                     <*> (o .:? "reasonText"))

instance ToJSON OrderReturn where
        toJSON OrderReturn'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _orQuantity,
                  ("actor" .=) <$> _orActor,
                  ("reason" .=) <$> _orReason,
                  ("creationDate" .=) <$> _orCreationDate,
                  ("reasonText" .=) <$> _orReasonText])

-- | A batch entry encoding a single non-batch accounttax response.
--
-- /See:/ 'accounttaxCustomBatchResponseEntry' smart constructor.
data AccounttaxCustomBatchResponseEntry = AccounttaxCustomBatchResponseEntry'
    { _acbreAccountTax :: !(Maybe AccountTax)
    , _acbreKind :: !Text
    , _acbreErrors :: !(Maybe Errors)
    , _acbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbreAccountTax'
--
-- * 'acbreKind'
--
-- * 'acbreErrors'
--
-- * 'acbreBatchId'
accounttaxCustomBatchResponseEntry
    :: AccounttaxCustomBatchResponseEntry
accounttaxCustomBatchResponseEntry = 
    AccounttaxCustomBatchResponseEntry'
    { _acbreAccountTax = Nothing
    , _acbreKind = "content#accounttaxCustomBatchResponseEntry"
    , _acbreErrors = Nothing
    , _acbreBatchId = Nothing
    }

-- | The retrieved or updated account tax settings.
acbreAccountTax :: Lens' AccounttaxCustomBatchResponseEntry (Maybe AccountTax)
acbreAccountTax
  = lens _acbreAccountTax
      (\ s a -> s{_acbreAccountTax = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accounttaxCustomBatchResponseEntry\".
acbreKind :: Lens' AccounttaxCustomBatchResponseEntry Text
acbreKind
  = lens _acbreKind (\ s a -> s{_acbreKind = a})

-- | A list of errors defined if and only if the request failed.
acbreErrors :: Lens' AccounttaxCustomBatchResponseEntry (Maybe Errors)
acbreErrors
  = lens _acbreErrors (\ s a -> s{_acbreErrors = a})

-- | The ID of the request entry this entry responds to.
acbreBatchId :: Lens' AccounttaxCustomBatchResponseEntry (Maybe Word32)
acbreBatchId
  = lens _acbreBatchId (\ s a -> s{_acbreBatchId = a})
      . mapping _Coerce

instance FromJSON AccounttaxCustomBatchResponseEntry
         where
        parseJSON
          = withObject "AccounttaxCustomBatchResponseEntry"
              (\ o ->
                 AccounttaxCustomBatchResponseEntry' <$>
                   (o .:? "accountTax") <*>
                     (o .:? "kind" .!=
                        "content#accounttaxCustomBatchResponseEntry")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON AccounttaxCustomBatchResponseEntry
         where
        toJSON AccounttaxCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [("accountTax" .=) <$> _acbreAccountTax,
                  Just ("kind" .= _acbreKind),
                  ("errors" .=) <$> _acbreErrors,
                  ("batchId" .=) <$> _acbreBatchId])

--
-- /See:/ 'inventoryCustomBatchRequest' smart constructor.
newtype InventoryCustomBatchRequest = InventoryCustomBatchRequest'
    { _icbrEntries :: Maybe [InventoryCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icbrEntries'
inventoryCustomBatchRequest
    :: InventoryCustomBatchRequest
inventoryCustomBatchRequest = 
    InventoryCustomBatchRequest'
    { _icbrEntries = Nothing
    }

-- | The request entries to be processed in the batch.
icbrEntries :: Lens' InventoryCustomBatchRequest [InventoryCustomBatchRequestEntry]
icbrEntries
  = lens _icbrEntries (\ s a -> s{_icbrEntries = a}) .
      _Default
      . _Coerce

instance FromJSON InventoryCustomBatchRequest where
        parseJSON
          = withObject "InventoryCustomBatchRequest"
              (\ o ->
                 InventoryCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON InventoryCustomBatchRequest where
        toJSON InventoryCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _icbrEntries])

--
-- /See:/ 'posSaleResponse' smart constructor.
data PosSaleResponse = PosSaleResponse'
    { _psrStoreCode :: !(Maybe Text)
    , _psrKind :: !Text
    , _psrItemId :: !(Maybe Text)
    , _psrQuantity :: !(Maybe (Textual Int64))
    , _psrTargetCountry :: !(Maybe Text)
    , _psrGtin :: !(Maybe Text)
    , _psrPrice :: !(Maybe Price)
    , _psrContentLanguage :: !(Maybe Text)
    , _psrTimestamp :: !(Maybe Text)
    , _psrSaleId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosSaleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psrStoreCode'
--
-- * 'psrKind'
--
-- * 'psrItemId'
--
-- * 'psrQuantity'
--
-- * 'psrTargetCountry'
--
-- * 'psrGtin'
--
-- * 'psrPrice'
--
-- * 'psrContentLanguage'
--
-- * 'psrTimestamp'
--
-- * 'psrSaleId'
posSaleResponse
    :: PosSaleResponse
posSaleResponse = 
    PosSaleResponse'
    { _psrStoreCode = Nothing
    , _psrKind = "content#posSaleResponse"
    , _psrItemId = Nothing
    , _psrQuantity = Nothing
    , _psrTargetCountry = Nothing
    , _psrGtin = Nothing
    , _psrPrice = Nothing
    , _psrContentLanguage = Nothing
    , _psrTimestamp = Nothing
    , _psrSaleId = Nothing
    }

-- | The identifier of the merchant\'s store.
psrStoreCode :: Lens' PosSaleResponse (Maybe Text)
psrStoreCode
  = lens _psrStoreCode (\ s a -> s{_psrStoreCode = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posSaleResponse\".
psrKind :: Lens' PosSaleResponse Text
psrKind = lens _psrKind (\ s a -> s{_psrKind = a})

-- | A unique identifier for the item.
psrItemId :: Lens' PosSaleResponse (Maybe Text)
psrItemId
  = lens _psrItemId (\ s a -> s{_psrItemId = a})

-- | The relative change of the available quantity. Negative for items sold.
psrQuantity :: Lens' PosSaleResponse (Maybe Int64)
psrQuantity
  = lens _psrQuantity (\ s a -> s{_psrQuantity = a}) .
      mapping _Coerce

-- | The CLDR territory code for the item.
psrTargetCountry :: Lens' PosSaleResponse (Maybe Text)
psrTargetCountry
  = lens _psrTargetCountry
      (\ s a -> s{_psrTargetCountry = a})

-- | Global Trade Item Number.
psrGtin :: Lens' PosSaleResponse (Maybe Text)
psrGtin = lens _psrGtin (\ s a -> s{_psrGtin = a})

-- | The price of the item.
psrPrice :: Lens' PosSaleResponse (Maybe Price)
psrPrice = lens _psrPrice (\ s a -> s{_psrPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
psrContentLanguage :: Lens' PosSaleResponse (Maybe Text)
psrContentLanguage
  = lens _psrContentLanguage
      (\ s a -> s{_psrContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
psrTimestamp :: Lens' PosSaleResponse (Maybe Text)
psrTimestamp
  = lens _psrTimestamp (\ s a -> s{_psrTimestamp = a})

-- | A unique ID to group items from the same sale event.
psrSaleId :: Lens' PosSaleResponse (Maybe Text)
psrSaleId
  = lens _psrSaleId (\ s a -> s{_psrSaleId = a})

instance FromJSON PosSaleResponse where
        parseJSON
          = withObject "PosSaleResponse"
              (\ o ->
                 PosSaleResponse' <$>
                   (o .:? "storeCode") <*>
                     (o .:? "kind" .!= "content#posSaleResponse")
                     <*> (o .:? "itemId")
                     <*> (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp")
                     <*> (o .:? "saleId"))

instance ToJSON PosSaleResponse where
        toJSON PosSaleResponse'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _psrStoreCode,
                  Just ("kind" .= _psrKind),
                  ("itemId" .=) <$> _psrItemId,
                  ("quantity" .=) <$> _psrQuantity,
                  ("targetCountry" .=) <$> _psrTargetCountry,
                  ("gtin" .=) <$> _psrGtin, ("price" .=) <$> _psrPrice,
                  ("contentLanguage" .=) <$> _psrContentLanguage,
                  ("timestamp" .=) <$> _psrTimestamp,
                  ("saleId" .=) <$> _psrSaleId])

--
-- /See:/ 'accountsAuthInfoResponse' smart constructor.
data AccountsAuthInfoResponse = AccountsAuthInfoResponse'
    { _aairKind :: !Text
    , _aairAccountIdentifiers :: !(Maybe [AccountIdentifier])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsAuthInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aairKind'
--
-- * 'aairAccountIdentifiers'
accountsAuthInfoResponse
    :: AccountsAuthInfoResponse
accountsAuthInfoResponse = 
    AccountsAuthInfoResponse'
    { _aairKind = "content#accountsAuthInfoResponse"
    , _aairAccountIdentifiers = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountsAuthInfoResponse\".
aairKind :: Lens' AccountsAuthInfoResponse Text
aairKind = lens _aairKind (\ s a -> s{_aairKind = a})

-- | The account identifiers corresponding to the authenticated user. - For
-- an individual account: only the merchant ID is defined - For an
-- aggregator: only the aggregator ID is defined - For a subaccount of an
-- MCA: both the merchant ID and the aggregator ID are defined.
aairAccountIdentifiers :: Lens' AccountsAuthInfoResponse [AccountIdentifier]
aairAccountIdentifiers
  = lens _aairAccountIdentifiers
      (\ s a -> s{_aairAccountIdentifiers = a})
      . _Default
      . _Coerce

instance FromJSON AccountsAuthInfoResponse where
        parseJSON
          = withObject "AccountsAuthInfoResponse"
              (\ o ->
                 AccountsAuthInfoResponse' <$>
                   (o .:? "kind" .!= "content#accountsAuthInfoResponse")
                     <*> (o .:? "accountIdentifiers" .!= mempty))

instance ToJSON AccountsAuthInfoResponse where
        toJSON AccountsAuthInfoResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _aairKind),
                  ("accountIdentifiers" .=) <$>
                    _aairAccountIdentifiers])

-- | The change of the available quantity of an item at the given store.
--
-- /See:/ 'posSale' smart constructor.
data PosSale = PosSale'
    { _psStoreCode :: !(Maybe Text)
    , _psKind :: !Text
    , _psItemId :: !(Maybe Text)
    , _psQuantity :: !(Maybe (Textual Int64))
    , _psTargetCountry :: !(Maybe Text)
    , _psGtin :: !(Maybe Text)
    , _psPrice :: !(Maybe Price)
    , _psContentLanguage :: !(Maybe Text)
    , _psTimestamp :: !(Maybe Text)
    , _psSaleId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosSale' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psStoreCode'
--
-- * 'psKind'
--
-- * 'psItemId'
--
-- * 'psQuantity'
--
-- * 'psTargetCountry'
--
-- * 'psGtin'
--
-- * 'psPrice'
--
-- * 'psContentLanguage'
--
-- * 'psTimestamp'
--
-- * 'psSaleId'
posSale
    :: PosSale
posSale = 
    PosSale'
    { _psStoreCode = Nothing
    , _psKind = "content#posSale"
    , _psItemId = Nothing
    , _psQuantity = Nothing
    , _psTargetCountry = Nothing
    , _psGtin = Nothing
    , _psPrice = Nothing
    , _psContentLanguage = Nothing
    , _psTimestamp = Nothing
    , _psSaleId = Nothing
    }

-- | The identifier of the merchant\'s store.
psStoreCode :: Lens' PosSale (Maybe Text)
psStoreCode
  = lens _psStoreCode (\ s a -> s{_psStoreCode = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posSale\".
psKind :: Lens' PosSale Text
psKind = lens _psKind (\ s a -> s{_psKind = a})

-- | A unique identifier for the item.
psItemId :: Lens' PosSale (Maybe Text)
psItemId = lens _psItemId (\ s a -> s{_psItemId = a})

-- | The relative change of the available quantity. Negative for items sold.
psQuantity :: Lens' PosSale (Maybe Int64)
psQuantity
  = lens _psQuantity (\ s a -> s{_psQuantity = a}) .
      mapping _Coerce

-- | The CLDR territory code for the item.
psTargetCountry :: Lens' PosSale (Maybe Text)
psTargetCountry
  = lens _psTargetCountry
      (\ s a -> s{_psTargetCountry = a})

-- | Global Trade Item Number.
psGtin :: Lens' PosSale (Maybe Text)
psGtin = lens _psGtin (\ s a -> s{_psGtin = a})

-- | The price of the item.
psPrice :: Lens' PosSale (Maybe Price)
psPrice = lens _psPrice (\ s a -> s{_psPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
psContentLanguage :: Lens' PosSale (Maybe Text)
psContentLanguage
  = lens _psContentLanguage
      (\ s a -> s{_psContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
psTimestamp :: Lens' PosSale (Maybe Text)
psTimestamp
  = lens _psTimestamp (\ s a -> s{_psTimestamp = a})

-- | A unique ID to group items from the same sale event.
psSaleId :: Lens' PosSale (Maybe Text)
psSaleId = lens _psSaleId (\ s a -> s{_psSaleId = a})

instance FromJSON PosSale where
        parseJSON
          = withObject "PosSale"
              (\ o ->
                 PosSale' <$>
                   (o .:? "storeCode") <*>
                     (o .:? "kind" .!= "content#posSale")
                     <*> (o .:? "itemId")
                     <*> (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp")
                     <*> (o .:? "saleId"))

instance ToJSON PosSale where
        toJSON PosSale'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _psStoreCode,
                  Just ("kind" .= _psKind),
                  ("itemId" .=) <$> _psItemId,
                  ("quantity" .=) <$> _psQuantity,
                  ("targetCountry" .=) <$> _psTargetCountry,
                  ("gtin" .=) <$> _psGtin, ("price" .=) <$> _psPrice,
                  ("contentLanguage" .=) <$> _psContentLanguage,
                  ("timestamp" .=) <$> _psTimestamp,
                  ("saleId" .=) <$> _psSaleId])

--
-- /See:/ 'productStatusDestinationStatus' smart constructor.
data ProductStatusDestinationStatus = ProductStatusDestinationStatus'
    { _psdsDestination :: !(Maybe Text)
    , _psdsApprovalPending :: !(Maybe Bool)
    , _psdsIntention :: !(Maybe Text)
    , _psdsApprovalStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductStatusDestinationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psdsDestination'
--
-- * 'psdsApprovalPending'
--
-- * 'psdsIntention'
--
-- * 'psdsApprovalStatus'
productStatusDestinationStatus
    :: ProductStatusDestinationStatus
productStatusDestinationStatus = 
    ProductStatusDestinationStatus'
    { _psdsDestination = Nothing
    , _psdsApprovalPending = Nothing
    , _psdsIntention = Nothing
    , _psdsApprovalStatus = Nothing
    }

-- | The name of the destination
psdsDestination :: Lens' ProductStatusDestinationStatus (Maybe Text)
psdsDestination
  = lens _psdsDestination
      (\ s a -> s{_psdsDestination = a})

-- | Whether the approval status might change due to further processing.
psdsApprovalPending :: Lens' ProductStatusDestinationStatus (Maybe Bool)
psdsApprovalPending
  = lens _psdsApprovalPending
      (\ s a -> s{_psdsApprovalPending = a})

-- | Provided for backward compatibility only. Always set to \"required\".
psdsIntention :: Lens' ProductStatusDestinationStatus (Maybe Text)
psdsIntention
  = lens _psdsIntention
      (\ s a -> s{_psdsIntention = a})

-- | The destination\'s approval status.
psdsApprovalStatus :: Lens' ProductStatusDestinationStatus (Maybe Text)
psdsApprovalStatus
  = lens _psdsApprovalStatus
      (\ s a -> s{_psdsApprovalStatus = a})

instance FromJSON ProductStatusDestinationStatus
         where
        parseJSON
          = withObject "ProductStatusDestinationStatus"
              (\ o ->
                 ProductStatusDestinationStatus' <$>
                   (o .:? "destination") <*> (o .:? "approvalPending")
                     <*> (o .:? "intention")
                     <*> (o .:? "approvalStatus"))

instance ToJSON ProductStatusDestinationStatus where
        toJSON ProductStatusDestinationStatus'{..}
          = object
              (catMaybes
                 [("destination" .=) <$> _psdsDestination,
                  ("approvalPending" .=) <$> _psdsApprovalPending,
                  ("intention" .=) <$> _psdsIntention,
                  ("approvalStatus" .=) <$> _psdsApprovalStatus])

-- | Tax calculation rule to apply in a state or province (USA only).
--
-- /See:/ 'accountTaxTaxRule' smart constructor.
data AccountTaxTaxRule = AccountTaxTaxRule'
    { _attrUseGlobalRate :: !(Maybe Bool)
    , _attrCountry :: !(Maybe Text)
    , _attrShippingTaxed :: !(Maybe Bool)
    , _attrLocationId :: !(Maybe (Textual Word64))
    , _attrRatePercent :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountTaxTaxRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attrUseGlobalRate'
--
-- * 'attrCountry'
--
-- * 'attrShippingTaxed'
--
-- * 'attrLocationId'
--
-- * 'attrRatePercent'
accountTaxTaxRule
    :: AccountTaxTaxRule
accountTaxTaxRule = 
    AccountTaxTaxRule'
    { _attrUseGlobalRate = Nothing
    , _attrCountry = Nothing
    , _attrShippingTaxed = Nothing
    , _attrLocationId = Nothing
    , _attrRatePercent = Nothing
    }

-- | Whether the tax rate is taken from a global tax table or specified
-- explicitly.
attrUseGlobalRate :: Lens' AccountTaxTaxRule (Maybe Bool)
attrUseGlobalRate
  = lens _attrUseGlobalRate
      (\ s a -> s{_attrUseGlobalRate = a})

-- | Country code in which tax is applicable.
attrCountry :: Lens' AccountTaxTaxRule (Maybe Text)
attrCountry
  = lens _attrCountry (\ s a -> s{_attrCountry = a})

-- | If true, shipping charges are also taxed.
attrShippingTaxed :: Lens' AccountTaxTaxRule (Maybe Bool)
attrShippingTaxed
  = lens _attrShippingTaxed
      (\ s a -> s{_attrShippingTaxed = a})

-- | State (or province) is which the tax is applicable, described by its
-- location id (also called criteria id).
attrLocationId :: Lens' AccountTaxTaxRule (Maybe Word64)
attrLocationId
  = lens _attrLocationId
      (\ s a -> s{_attrLocationId = a})
      . mapping _Coerce

-- | Explicit tax rate in percent, represented as a floating point number
-- without the percentage character. Must not be negative.
attrRatePercent :: Lens' AccountTaxTaxRule (Maybe Text)
attrRatePercent
  = lens _attrRatePercent
      (\ s a -> s{_attrRatePercent = a})

instance FromJSON AccountTaxTaxRule where
        parseJSON
          = withObject "AccountTaxTaxRule"
              (\ o ->
                 AccountTaxTaxRule' <$>
                   (o .:? "useGlobalRate") <*> (o .:? "country") <*>
                     (o .:? "shippingTaxed")
                     <*> (o .:? "locationId")
                     <*> (o .:? "ratePercent"))

instance ToJSON AccountTaxTaxRule where
        toJSON AccountTaxTaxRule'{..}
          = object
              (catMaybes
                 [("useGlobalRate" .=) <$> _attrUseGlobalRate,
                  ("country" .=) <$> _attrCountry,
                  ("shippingTaxed" .=) <$> _attrShippingTaxed,
                  ("locationId" .=) <$> _attrLocationId,
                  ("ratePercent" .=) <$> _attrRatePercent])

--
-- /See:/ 'postalCodeGroup' smart constructor.
data PostalCodeGroup = PostalCodeGroup'
    { _pcgCountry :: !(Maybe Text)
    , _pcgPostalCodeRanges :: !(Maybe [PostalCodeRange])
    , _pcgName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PostalCodeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcgCountry'
--
-- * 'pcgPostalCodeRanges'
--
-- * 'pcgName'
postalCodeGroup
    :: PostalCodeGroup
postalCodeGroup = 
    PostalCodeGroup'
    { _pcgCountry = Nothing
    , _pcgPostalCodeRanges = Nothing
    , _pcgName = Nothing
    }

-- | The CLDR territory code of the country the postal code group applies to.
-- Required.
pcgCountry :: Lens' PostalCodeGroup (Maybe Text)
pcgCountry
  = lens _pcgCountry (\ s a -> s{_pcgCountry = a})

-- | A range of postal codes. Required.
pcgPostalCodeRanges :: Lens' PostalCodeGroup [PostalCodeRange]
pcgPostalCodeRanges
  = lens _pcgPostalCodeRanges
      (\ s a -> s{_pcgPostalCodeRanges = a})
      . _Default
      . _Coerce

-- | The name of the postal code group, referred to in headers. Required.
pcgName :: Lens' PostalCodeGroup (Maybe Text)
pcgName = lens _pcgName (\ s a -> s{_pcgName = a})

instance FromJSON PostalCodeGroup where
        parseJSON
          = withObject "PostalCodeGroup"
              (\ o ->
                 PostalCodeGroup' <$>
                   (o .:? "country") <*>
                     (o .:? "postalCodeRanges" .!= mempty)
                     <*> (o .:? "name"))

instance ToJSON PostalCodeGroup where
        toJSON PostalCodeGroup'{..}
          = object
              (catMaybes
                 [("country" .=) <$> _pcgCountry,
                  ("postalCodeRanges" .=) <$> _pcgPostalCodeRanges,
                  ("name" .=) <$> _pcgName])

--
-- /See:/ 'productDestination' smart constructor.
data ProductDestination = ProductDestination'
    { _pdIntention :: !(Maybe Text)
    , _pdDestinationName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdIntention'
--
-- * 'pdDestinationName'
productDestination
    :: ProductDestination
productDestination = 
    ProductDestination'
    { _pdIntention = Nothing
    , _pdDestinationName = Nothing
    }

-- | Whether the destination is required, excluded or should be validated.
pdIntention :: Lens' ProductDestination (Maybe Text)
pdIntention
  = lens _pdIntention (\ s a -> s{_pdIntention = a})

-- | The name of the destination.
pdDestinationName :: Lens' ProductDestination (Maybe Text)
pdDestinationName
  = lens _pdDestinationName
      (\ s a -> s{_pdDestinationName = a})

instance FromJSON ProductDestination where
        parseJSON
          = withObject "ProductDestination"
              (\ o ->
                 ProductDestination' <$>
                   (o .:? "intention") <*> (o .:? "destinationName"))

instance ToJSON ProductDestination where
        toJSON ProductDestination'{..}
          = object
              (catMaybes
                 [("intention" .=) <$> _pdIntention,
                  ("destinationName" .=) <$> _pdDestinationName])

--
-- /See:/ 'datafeedsCustomBatchRequest' smart constructor.
newtype DatafeedsCustomBatchRequest = DatafeedsCustomBatchRequest'
    { _dEntries :: Maybe [DatafeedsCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEntries'
datafeedsCustomBatchRequest
    :: DatafeedsCustomBatchRequest
datafeedsCustomBatchRequest = 
    DatafeedsCustomBatchRequest'
    { _dEntries = Nothing
    }

-- | The request entries to be processed in the batch.
dEntries :: Lens' DatafeedsCustomBatchRequest [DatafeedsCustomBatchRequestEntry]
dEntries
  = lens _dEntries (\ s a -> s{_dEntries = a}) .
      _Default
      . _Coerce

instance FromJSON DatafeedsCustomBatchRequest where
        parseJSON
          = withObject "DatafeedsCustomBatchRequest"
              (\ o ->
                 DatafeedsCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON DatafeedsCustomBatchRequest where
        toJSON DatafeedsCustomBatchRequest'{..}
          = object (catMaybes [("entries" .=) <$> _dEntries])

--
-- /See:/ 'ordersCustomBatchRequestEntry' smart constructor.
data OrdersCustomBatchRequestEntry = OrdersCustomBatchRequestEntry'
    { _ocbreMerchantId :: !(Maybe (Textual Word64))
    , _ocbreCancelLineItem :: !(Maybe OrdersCustomBatchRequestEntryCancelLineItem)
    , _ocbreInStoreRefundLineItem :: !(Maybe OrdersCustomBatchRequestEntryInStoreRefundLineItem)
    , _ocbreRefund :: !(Maybe OrdersCustomBatchRequestEntryRefund)
    , _ocbreUpdateShipment :: !(Maybe OrdersCustomBatchRequestEntryUpdateShipment)
    , _ocbreReturnLineItem :: !(Maybe OrdersCustomBatchRequestEntryReturnLineItem)
    , _ocbreMerchantOrderId :: !(Maybe Text)
    , _ocbreSetLineItemMetadata :: !(Maybe OrdersCustomBatchRequestEntrySetLineItemMetadata)
    , _ocbreReturnRefundLineItem :: !(Maybe OrdersCustomBatchRequestEntryReturnRefundLineItem)
    , _ocbreMethod :: !(Maybe Text)
    , _ocbreUpdateLineItemShippingDetails :: !(Maybe OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails)
    , _ocbreShipLineItems :: !(Maybe OrdersCustomBatchRequestEntryShipLineItems)
    , _ocbreOperationId :: !(Maybe Text)
    , _ocbreOrderId :: !(Maybe Text)
    , _ocbreRejectReturnLineItem :: !(Maybe OrdersCustomBatchRequestEntryRejectReturnLineItem)
    , _ocbreCancel :: !(Maybe OrdersCustomBatchRequestEntryCancel)
    , _ocbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreMerchantId'
--
-- * 'ocbreCancelLineItem'
--
-- * 'ocbreInStoreRefundLineItem'
--
-- * 'ocbreRefund'
--
-- * 'ocbreUpdateShipment'
--
-- * 'ocbreReturnLineItem'
--
-- * 'ocbreMerchantOrderId'
--
-- * 'ocbreSetLineItemMetadata'
--
-- * 'ocbreReturnRefundLineItem'
--
-- * 'ocbreMethod'
--
-- * 'ocbreUpdateLineItemShippingDetails'
--
-- * 'ocbreShipLineItems'
--
-- * 'ocbreOperationId'
--
-- * 'ocbreOrderId'
--
-- * 'ocbreRejectReturnLineItem'
--
-- * 'ocbreCancel'
--
-- * 'ocbreBatchId'
ordersCustomBatchRequestEntry
    :: OrdersCustomBatchRequestEntry
ordersCustomBatchRequestEntry = 
    OrdersCustomBatchRequestEntry'
    { _ocbreMerchantId = Nothing
    , _ocbreCancelLineItem = Nothing
    , _ocbreInStoreRefundLineItem = Nothing
    , _ocbreRefund = Nothing
    , _ocbreUpdateShipment = Nothing
    , _ocbreReturnLineItem = Nothing
    , _ocbreMerchantOrderId = Nothing
    , _ocbreSetLineItemMetadata = Nothing
    , _ocbreReturnRefundLineItem = Nothing
    , _ocbreMethod = Nothing
    , _ocbreUpdateLineItemShippingDetails = Nothing
    , _ocbreShipLineItems = Nothing
    , _ocbreOperationId = Nothing
    , _ocbreOrderId = Nothing
    , _ocbreRejectReturnLineItem = Nothing
    , _ocbreCancel = Nothing
    , _ocbreBatchId = Nothing
    }

-- | The ID of the managing account.
ocbreMerchantId :: Lens' OrdersCustomBatchRequestEntry (Maybe Word64)
ocbreMerchantId
  = lens _ocbreMerchantId
      (\ s a -> s{_ocbreMerchantId = a})
      . mapping _Coerce

-- | Required for cancelLineItem method.
ocbreCancelLineItem :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryCancelLineItem)
ocbreCancelLineItem
  = lens _ocbreCancelLineItem
      (\ s a -> s{_ocbreCancelLineItem = a})

-- | Required for inStoreReturnLineItem method.
ocbreInStoreRefundLineItem :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryInStoreRefundLineItem)
ocbreInStoreRefundLineItem
  = lens _ocbreInStoreRefundLineItem
      (\ s a -> s{_ocbreInStoreRefundLineItem = a})

-- | Required for refund method.
ocbreRefund :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryRefund)
ocbreRefund
  = lens _ocbreRefund (\ s a -> s{_ocbreRefund = a})

-- | Required for updateShipment method.
ocbreUpdateShipment :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryUpdateShipment)
ocbreUpdateShipment
  = lens _ocbreUpdateShipment
      (\ s a -> s{_ocbreUpdateShipment = a})

-- | Required for returnLineItem method.
ocbreReturnLineItem :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryReturnLineItem)
ocbreReturnLineItem
  = lens _ocbreReturnLineItem
      (\ s a -> s{_ocbreReturnLineItem = a})

-- | The merchant order id. Required for updateMerchantOrderId and
-- getByMerchantOrderId methods.
ocbreMerchantOrderId :: Lens' OrdersCustomBatchRequestEntry (Maybe Text)
ocbreMerchantOrderId
  = lens _ocbreMerchantOrderId
      (\ s a -> s{_ocbreMerchantOrderId = a})

-- | Required for setLineItemMetadata method.
ocbreSetLineItemMetadata :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntrySetLineItemMetadata)
ocbreSetLineItemMetadata
  = lens _ocbreSetLineItemMetadata
      (\ s a -> s{_ocbreSetLineItemMetadata = a})

-- | Required for returnRefundLineItem method.
ocbreReturnRefundLineItem :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryReturnRefundLineItem)
ocbreReturnRefundLineItem
  = lens _ocbreReturnRefundLineItem
      (\ s a -> s{_ocbreReturnRefundLineItem = a})

-- | The method to apply.
ocbreMethod :: Lens' OrdersCustomBatchRequestEntry (Maybe Text)
ocbreMethod
  = lens _ocbreMethod (\ s a -> s{_ocbreMethod = a})

-- | Required for updateLineItemShippingDate method.
ocbreUpdateLineItemShippingDetails :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails)
ocbreUpdateLineItemShippingDetails
  = lens _ocbreUpdateLineItemShippingDetails
      (\ s a -> s{_ocbreUpdateLineItemShippingDetails = a})

-- | Required for shipLineItems method.
ocbreShipLineItems :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryShipLineItems)
ocbreShipLineItems
  = lens _ocbreShipLineItems
      (\ s a -> s{_ocbreShipLineItems = a})

-- | The ID of the operation. Unique across all operations for a given order.
-- Required for all methods beside get and getByMerchantOrderId.
ocbreOperationId :: Lens' OrdersCustomBatchRequestEntry (Maybe Text)
ocbreOperationId
  = lens _ocbreOperationId
      (\ s a -> s{_ocbreOperationId = a})

-- | The ID of the order. Required for all methods beside
-- getByMerchantOrderId.
ocbreOrderId :: Lens' OrdersCustomBatchRequestEntry (Maybe Text)
ocbreOrderId
  = lens _ocbreOrderId (\ s a -> s{_ocbreOrderId = a})

-- | Required for rejectReturnLineItem method.
ocbreRejectReturnLineItem :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryRejectReturnLineItem)
ocbreRejectReturnLineItem
  = lens _ocbreRejectReturnLineItem
      (\ s a -> s{_ocbreRejectReturnLineItem = a})

-- | Required for cancel method.
ocbreCancel :: Lens' OrdersCustomBatchRequestEntry (Maybe OrdersCustomBatchRequestEntryCancel)
ocbreCancel
  = lens _ocbreCancel (\ s a -> s{_ocbreCancel = a})

-- | An entry ID, unique within the batch request.
ocbreBatchId :: Lens' OrdersCustomBatchRequestEntry (Maybe Word32)
ocbreBatchId
  = lens _ocbreBatchId (\ s a -> s{_ocbreBatchId = a})
      . mapping _Coerce

instance FromJSON OrdersCustomBatchRequestEntry where
        parseJSON
          = withObject "OrdersCustomBatchRequestEntry"
              (\ o ->
                 OrdersCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "cancelLineItem") <*>
                     (o .:? "inStoreRefundLineItem")
                     <*> (o .:? "refund")
                     <*> (o .:? "updateShipment")
                     <*> (o .:? "returnLineItem")
                     <*> (o .:? "merchantOrderId")
                     <*> (o .:? "setLineItemMetadata")
                     <*> (o .:? "returnRefundLineItem")
                     <*> (o .:? "method")
                     <*> (o .:? "updateLineItemShippingDetails")
                     <*> (o .:? "shipLineItems")
                     <*> (o .:? "operationId")
                     <*> (o .:? "orderId")
                     <*> (o .:? "rejectReturnLineItem")
                     <*> (o .:? "cancel")
                     <*> (o .:? "batchId"))

instance ToJSON OrdersCustomBatchRequestEntry where
        toJSON OrdersCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _ocbreMerchantId,
                  ("cancelLineItem" .=) <$> _ocbreCancelLineItem,
                  ("inStoreRefundLineItem" .=) <$>
                    _ocbreInStoreRefundLineItem,
                  ("refund" .=) <$> _ocbreRefund,
                  ("updateShipment" .=) <$> _ocbreUpdateShipment,
                  ("returnLineItem" .=) <$> _ocbreReturnLineItem,
                  ("merchantOrderId" .=) <$> _ocbreMerchantOrderId,
                  ("setLineItemMetadata" .=) <$>
                    _ocbreSetLineItemMetadata,
                  ("returnRefundLineItem" .=) <$>
                    _ocbreReturnRefundLineItem,
                  ("method" .=) <$> _ocbreMethod,
                  ("updateLineItemShippingDetails" .=) <$>
                    _ocbreUpdateLineItemShippingDetails,
                  ("shipLineItems" .=) <$> _ocbreShipLineItems,
                  ("operationId" .=) <$> _ocbreOperationId,
                  ("orderId" .=) <$> _ocbreOrderId,
                  ("rejectReturnLineItem" .=) <$>
                    _ocbreRejectReturnLineItem,
                  ("cancel" .=) <$> _ocbreCancel,
                  ("batchId" .=) <$> _ocbreBatchId])

--
-- /See:/ 'ordersRefundRequest' smart constructor.
data OrdersRefundRequest = OrdersRefundRequest'
    { _orrAmount :: !(Maybe Price)
    , _orrReason :: !(Maybe Text)
    , _orrOperationId :: !(Maybe Text)
    , _orrAmountPretax :: !(Maybe Price)
    , _orrAmountTax :: !(Maybe Price)
    , _orrReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersRefundRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrAmount'
--
-- * 'orrReason'
--
-- * 'orrOperationId'
--
-- * 'orrAmountPretax'
--
-- * 'orrAmountTax'
--
-- * 'orrReasonText'
ordersRefundRequest
    :: OrdersRefundRequest
ordersRefundRequest = 
    OrdersRefundRequest'
    { _orrAmount = Nothing
    , _orrReason = Nothing
    , _orrOperationId = Nothing
    , _orrAmountPretax = Nothing
    , _orrAmountTax = Nothing
    , _orrReasonText = Nothing
    }

-- | The amount that is refunded.
orrAmount :: Lens' OrdersRefundRequest (Maybe Price)
orrAmount
  = lens _orrAmount (\ s a -> s{_orrAmount = a})

-- | The reason for the refund.
orrReason :: Lens' OrdersRefundRequest (Maybe Text)
orrReason
  = lens _orrReason (\ s a -> s{_orrReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
orrOperationId :: Lens' OrdersRefundRequest (Maybe Text)
orrOperationId
  = lens _orrOperationId
      (\ s a -> s{_orrOperationId = a})

-- | The amount that is refunded. Either amount or amountPretax and amountTax
-- should be filled.
orrAmountPretax :: Lens' OrdersRefundRequest (Maybe Price)
orrAmountPretax
  = lens _orrAmountPretax
      (\ s a -> s{_orrAmountPretax = a})

-- | Tax amount that correspond to refund amount in amountPretax.
orrAmountTax :: Lens' OrdersRefundRequest (Maybe Price)
orrAmountTax
  = lens _orrAmountTax (\ s a -> s{_orrAmountTax = a})

-- | The explanation of the reason.
orrReasonText :: Lens' OrdersRefundRequest (Maybe Text)
orrReasonText
  = lens _orrReasonText
      (\ s a -> s{_orrReasonText = a})

instance FromJSON OrdersRefundRequest where
        parseJSON
          = withObject "OrdersRefundRequest"
              (\ o ->
                 OrdersRefundRequest' <$>
                   (o .:? "amount") <*> (o .:? "reason") <*>
                     (o .:? "operationId")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersRefundRequest where
        toJSON OrdersRefundRequest'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _orrAmount,
                  ("reason" .=) <$> _orrReason,
                  ("operationId" .=) <$> _orrOperationId,
                  ("amountPretax" .=) <$> _orrAmountPretax,
                  ("amountTax" .=) <$> _orrAmountTax,
                  ("reasonText" .=) <$> _orrReasonText])

--
-- /See:/ 'ordersCustomBatchRequestEntryCancelLineItem' smart constructor.
data OrdersCustomBatchRequestEntryCancelLineItem = OrdersCustomBatchRequestEntryCancelLineItem'
    { _ocbrecliAmount :: !(Maybe Price)
    , _ocbrecliQuantity :: !(Maybe (Textual Word32))
    , _ocbrecliLineItemId :: !(Maybe Text)
    , _ocbrecliReason :: !(Maybe Text)
    , _ocbrecliAmountPretax :: !(Maybe Price)
    , _ocbrecliProductId :: !(Maybe Text)
    , _ocbrecliAmountTax :: !(Maybe Price)
    , _ocbrecliReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryCancelLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrecliAmount'
--
-- * 'ocbrecliQuantity'
--
-- * 'ocbrecliLineItemId'
--
-- * 'ocbrecliReason'
--
-- * 'ocbrecliAmountPretax'
--
-- * 'ocbrecliProductId'
--
-- * 'ocbrecliAmountTax'
--
-- * 'ocbrecliReasonText'
ordersCustomBatchRequestEntryCancelLineItem
    :: OrdersCustomBatchRequestEntryCancelLineItem
ordersCustomBatchRequestEntryCancelLineItem = 
    OrdersCustomBatchRequestEntryCancelLineItem'
    { _ocbrecliAmount = Nothing
    , _ocbrecliQuantity = Nothing
    , _ocbrecliLineItemId = Nothing
    , _ocbrecliReason = Nothing
    , _ocbrecliAmountPretax = Nothing
    , _ocbrecliProductId = Nothing
    , _ocbrecliAmountTax = Nothing
    , _ocbrecliReasonText = Nothing
    }

-- | Amount to refund for the cancelation. Optional. If not set, Google will
-- calculate the default based on the price and tax of the items involved.
-- The amount must not be larger than the net amount left on the order.
ocbrecliAmount :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Price)
ocbrecliAmount
  = lens _ocbrecliAmount
      (\ s a -> s{_ocbrecliAmount = a})

-- | The quantity to cancel.
ocbrecliQuantity :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Word32)
ocbrecliQuantity
  = lens _ocbrecliQuantity
      (\ s a -> s{_ocbrecliQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to cancel. Either lineItemId or productId is
-- required.
ocbrecliLineItemId :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Text)
ocbrecliLineItemId
  = lens _ocbrecliLineItemId
      (\ s a -> s{_ocbrecliLineItemId = a})

-- | The reason for the cancellation.
ocbrecliReason :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Text)
ocbrecliReason
  = lens _ocbrecliReason
      (\ s a -> s{_ocbrecliReason = a})

-- | Amount to refund for the cancelation. Optional. If not set, Google will
-- calculate the default based on the price and tax of the items involved.
-- The amount must not be larger than the net amount left on the order.
ocbrecliAmountPretax :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Price)
ocbrecliAmountPretax
  = lens _ocbrecliAmountPretax
      (\ s a -> s{_ocbrecliAmountPretax = a})

-- | The ID of the product to cancel. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbrecliProductId :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Text)
ocbrecliProductId
  = lens _ocbrecliProductId
      (\ s a -> s{_ocbrecliProductId = a})

-- | Tax amount that correspond to cancellation amount in amountPretax.
ocbrecliAmountTax :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Price)
ocbrecliAmountTax
  = lens _ocbrecliAmountTax
      (\ s a -> s{_ocbrecliAmountTax = a})

-- | The explanation of the reason.
ocbrecliReasonText :: Lens' OrdersCustomBatchRequestEntryCancelLineItem (Maybe Text)
ocbrecliReasonText
  = lens _ocbrecliReasonText
      (\ s a -> s{_ocbrecliReasonText = a})

instance FromJSON
         OrdersCustomBatchRequestEntryCancelLineItem where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryCancelLineItem"
              (\ o ->
                 OrdersCustomBatchRequestEntryCancelLineItem' <$>
                   (o .:? "amount") <*> (o .:? "quantity") <*>
                     (o .:? "lineItemId")
                     <*> (o .:? "reason")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON
         OrdersCustomBatchRequestEntryCancelLineItem where
        toJSON
          OrdersCustomBatchRequestEntryCancelLineItem'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _ocbrecliAmount,
                  ("quantity" .=) <$> _ocbrecliQuantity,
                  ("lineItemId" .=) <$> _ocbrecliLineItemId,
                  ("reason" .=) <$> _ocbrecliReason,
                  ("amountPretax" .=) <$> _ocbrecliAmountPretax,
                  ("productId" .=) <$> _ocbrecliProductId,
                  ("amountTax" .=) <$> _ocbrecliAmountTax,
                  ("reasonText" .=) <$> _ocbrecliReasonText])

--
-- /See:/ 'orderLineItemShippingDetailsMethod' smart constructor.
data OrderLineItemShippingDetailsMethod = OrderLineItemShippingDetailsMethod'
    { _olisdmCarrier :: !(Maybe Text)
    , _olisdmMethodName :: !(Maybe Text)
    , _olisdmMaxDaysInTransit :: !(Maybe (Textual Word32))
    , _olisdmMinDaysInTransit :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItemShippingDetailsMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olisdmCarrier'
--
-- * 'olisdmMethodName'
--
-- * 'olisdmMaxDaysInTransit'
--
-- * 'olisdmMinDaysInTransit'
orderLineItemShippingDetailsMethod
    :: OrderLineItemShippingDetailsMethod
orderLineItemShippingDetailsMethod = 
    OrderLineItemShippingDetailsMethod'
    { _olisdmCarrier = Nothing
    , _olisdmMethodName = Nothing
    , _olisdmMaxDaysInTransit = Nothing
    , _olisdmMinDaysInTransit = Nothing
    }

-- | The carrier for the shipping. Optional. See shipments[].carrier for a
-- list of acceptable values.
olisdmCarrier :: Lens' OrderLineItemShippingDetailsMethod (Maybe Text)
olisdmCarrier
  = lens _olisdmCarrier
      (\ s a -> s{_olisdmCarrier = a})

-- | The name of the shipping method.
olisdmMethodName :: Lens' OrderLineItemShippingDetailsMethod (Maybe Text)
olisdmMethodName
  = lens _olisdmMethodName
      (\ s a -> s{_olisdmMethodName = a})

-- | Maximum transit time.
olisdmMaxDaysInTransit :: Lens' OrderLineItemShippingDetailsMethod (Maybe Word32)
olisdmMaxDaysInTransit
  = lens _olisdmMaxDaysInTransit
      (\ s a -> s{_olisdmMaxDaysInTransit = a})
      . mapping _Coerce

-- | Minimum transit time.
olisdmMinDaysInTransit :: Lens' OrderLineItemShippingDetailsMethod (Maybe Word32)
olisdmMinDaysInTransit
  = lens _olisdmMinDaysInTransit
      (\ s a -> s{_olisdmMinDaysInTransit = a})
      . mapping _Coerce

instance FromJSON OrderLineItemShippingDetailsMethod
         where
        parseJSON
          = withObject "OrderLineItemShippingDetailsMethod"
              (\ o ->
                 OrderLineItemShippingDetailsMethod' <$>
                   (o .:? "carrier") <*> (o .:? "methodName") <*>
                     (o .:? "maxDaysInTransit")
                     <*> (o .:? "minDaysInTransit"))

instance ToJSON OrderLineItemShippingDetailsMethod
         where
        toJSON OrderLineItemShippingDetailsMethod'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _olisdmCarrier,
                  ("methodName" .=) <$> _olisdmMethodName,
                  ("maxDaysInTransit" .=) <$> _olisdmMaxDaysInTransit,
                  ("minDaysInTransit" .=) <$> _olisdmMinDaysInTransit])

-- | Datafeed configuration data.
--
-- /See:/ 'datafeed' smart constructor.
data Datafeed = Datafeed'
    { _dKind :: !Text
    , _dFormat :: !(Maybe DatafeedFormat)
    , _dAttributeLanguage :: !(Maybe Text)
    , _dTargetCountry :: !(Maybe Text)
    , _dFetchSchedule :: !(Maybe DatafeedFetchSchedule)
    , _dName :: !(Maybe Text)
    , _dIntendedDestinations :: !(Maybe [Text])
    , _dTargets :: !(Maybe [DatafeedTarget])
    , _dId :: !(Maybe (Textual Int64))
    , _dContentLanguage :: !(Maybe Text)
    , _dContentType :: !(Maybe Text)
    , _dFileName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Datafeed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dKind'
--
-- * 'dFormat'
--
-- * 'dAttributeLanguage'
--
-- * 'dTargetCountry'
--
-- * 'dFetchSchedule'
--
-- * 'dName'
--
-- * 'dIntendedDestinations'
--
-- * 'dTargets'
--
-- * 'dId'
--
-- * 'dContentLanguage'
--
-- * 'dContentType'
--
-- * 'dFileName'
datafeed
    :: Datafeed
datafeed = 
    Datafeed'
    { _dKind = "content#datafeed"
    , _dFormat = Nothing
    , _dAttributeLanguage = Nothing
    , _dTargetCountry = Nothing
    , _dFetchSchedule = Nothing
    , _dName = Nothing
    , _dIntendedDestinations = Nothing
    , _dTargets = Nothing
    , _dId = Nothing
    , _dContentLanguage = Nothing
    , _dContentType = Nothing
    , _dFileName = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeed\".
dKind :: Lens' Datafeed Text
dKind = lens _dKind (\ s a -> s{_dKind = a})

-- | Format of the feed file.
dFormat :: Lens' Datafeed (Maybe DatafeedFormat)
dFormat = lens _dFormat (\ s a -> s{_dFormat = a})

-- | The two-letter ISO 639-1 language in which the attributes are defined in
-- the data feed.
dAttributeLanguage :: Lens' Datafeed (Maybe Text)
dAttributeLanguage
  = lens _dAttributeLanguage
      (\ s a -> s{_dAttributeLanguage = a})

-- | The country where the items in the feed will be included in the search
-- index, represented as a CLDR territory code.
dTargetCountry :: Lens' Datafeed (Maybe Text)
dTargetCountry
  = lens _dTargetCountry
      (\ s a -> s{_dTargetCountry = a})

-- | Fetch schedule for the feed file.
dFetchSchedule :: Lens' Datafeed (Maybe DatafeedFetchSchedule)
dFetchSchedule
  = lens _dFetchSchedule
      (\ s a -> s{_dFetchSchedule = a})

-- | A descriptive name of the data feed.
dName :: Lens' Datafeed (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | [DEPRECATED] Please use targets[].includedDestinations instead. The list
-- of intended destinations (corresponds to checked check boxes in Merchant
-- Center).
dIntendedDestinations :: Lens' Datafeed [Text]
dIntendedDestinations
  = lens _dIntendedDestinations
      (\ s a -> s{_dIntendedDestinations = a})
      . _Default
      . _Coerce

-- | The targets this feed should apply to (country, language, destinations).
dTargets :: Lens' Datafeed [DatafeedTarget]
dTargets
  = lens _dTargets (\ s a -> s{_dTargets = a}) .
      _Default
      . _Coerce

-- | The ID of the data feed.
dId :: Lens' Datafeed (Maybe Int64)
dId
  = lens _dId (\ s a -> s{_dId = a}) . mapping _Coerce

-- | [DEPRECATED] Please use targets[].language instead. The two-letter ISO
-- 639-1 language of the items in the feed. Must be a valid language for
-- targetCountry.
dContentLanguage :: Lens' Datafeed (Maybe Text)
dContentLanguage
  = lens _dContentLanguage
      (\ s a -> s{_dContentLanguage = a})

-- | The type of data feed. For product inventory feeds, only feeds for local
-- stores, not online stores, are supported.
dContentType :: Lens' Datafeed (Maybe Text)
dContentType
  = lens _dContentType (\ s a -> s{_dContentType = a})

-- | The filename of the feed. All feeds must have a unique file name.
dFileName :: Lens' Datafeed (Maybe Text)
dFileName
  = lens _dFileName (\ s a -> s{_dFileName = a})

instance FromJSON Datafeed where
        parseJSON
          = withObject "Datafeed"
              (\ o ->
                 Datafeed' <$>
                   (o .:? "kind" .!= "content#datafeed") <*>
                     (o .:? "format")
                     <*> (o .:? "attributeLanguage")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "fetchSchedule")
                     <*> (o .:? "name")
                     <*> (o .:? "intendedDestinations" .!= mempty)
                     <*> (o .:? "targets" .!= mempty)
                     <*> (o .:? "id")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "contentType")
                     <*> (o .:? "fileName"))

instance ToJSON Datafeed where
        toJSON Datafeed'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _dKind), ("format" .=) <$> _dFormat,
                  ("attributeLanguage" .=) <$> _dAttributeLanguage,
                  ("targetCountry" .=) <$> _dTargetCountry,
                  ("fetchSchedule" .=) <$> _dFetchSchedule,
                  ("name" .=) <$> _dName,
                  ("intendedDestinations" .=) <$>
                    _dIntendedDestinations,
                  ("targets" .=) <$> _dTargets, ("id" .=) <$> _dId,
                  ("contentLanguage" .=) <$> _dContentLanguage,
                  ("contentType" .=) <$> _dContentType,
                  ("fileName" .=) <$> _dFileName])

--
-- /See:/ 'ordersCreateTestOrderResponse' smart constructor.
data OrdersCreateTestOrderResponse = OrdersCreateTestOrderResponse'
    { _octorKind :: !Text
    , _octorOrderId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCreateTestOrderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'octorKind'
--
-- * 'octorOrderId'
ordersCreateTestOrderResponse
    :: OrdersCreateTestOrderResponse
ordersCreateTestOrderResponse = 
    OrdersCreateTestOrderResponse'
    { _octorKind = "content#ordersCreateTestOrderResponse"
    , _octorOrderId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersCreateTestOrderResponse\".
octorKind :: Lens' OrdersCreateTestOrderResponse Text
octorKind
  = lens _octorKind (\ s a -> s{_octorKind = a})

-- | The ID of the newly created test order.
octorOrderId :: Lens' OrdersCreateTestOrderResponse (Maybe Text)
octorOrderId
  = lens _octorOrderId (\ s a -> s{_octorOrderId = a})

instance FromJSON OrdersCreateTestOrderResponse where
        parseJSON
          = withObject "OrdersCreateTestOrderResponse"
              (\ o ->
                 OrdersCreateTestOrderResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersCreateTestOrderResponse")
                     <*> (o .:? "orderId"))

instance ToJSON OrdersCreateTestOrderResponse where
        toJSON OrdersCreateTestOrderResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _octorKind),
                  ("orderId" .=) <$> _octorOrderId])

-- | A batch entry encoding a single non-batch accounts response.
--
-- /See:/ 'accountsCustomBatchResponseEntry' smart constructor.
data AccountsCustomBatchResponseEntry = AccountsCustomBatchResponseEntry'
    { _aKind :: !Text
    , _aAccount :: !(Maybe Account)
    , _aErrors :: !(Maybe Errors)
    , _aBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aKind'
--
-- * 'aAccount'
--
-- * 'aErrors'
--
-- * 'aBatchId'
accountsCustomBatchResponseEntry
    :: AccountsCustomBatchResponseEntry
accountsCustomBatchResponseEntry = 
    AccountsCustomBatchResponseEntry'
    { _aKind = "content#accountsCustomBatchResponseEntry"
    , _aAccount = Nothing
    , _aErrors = Nothing
    , _aBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountsCustomBatchResponseEntry\".
aKind :: Lens' AccountsCustomBatchResponseEntry Text
aKind = lens _aKind (\ s a -> s{_aKind = a})

-- | The retrieved, created, or updated account. Not defined if the method
-- was delete or claimwebsite.
aAccount :: Lens' AccountsCustomBatchResponseEntry (Maybe Account)
aAccount = lens _aAccount (\ s a -> s{_aAccount = a})

-- | A list of errors defined if and only if the request failed.
aErrors :: Lens' AccountsCustomBatchResponseEntry (Maybe Errors)
aErrors = lens _aErrors (\ s a -> s{_aErrors = a})

-- | The ID of the request entry this entry responds to.
aBatchId :: Lens' AccountsCustomBatchResponseEntry (Maybe Word32)
aBatchId
  = lens _aBatchId (\ s a -> s{_aBatchId = a}) .
      mapping _Coerce

instance FromJSON AccountsCustomBatchResponseEntry
         where
        parseJSON
          = withObject "AccountsCustomBatchResponseEntry"
              (\ o ->
                 AccountsCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#accountsCustomBatchResponseEntry")
                     <*> (o .:? "account")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON AccountsCustomBatchResponseEntry
         where
        toJSON AccountsCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _aKind),
                  ("account" .=) <$> _aAccount,
                  ("errors" .=) <$> _aErrors,
                  ("batchId" .=) <$> _aBatchId])

--
-- /See:/ 'accountIdentifier' smart constructor.
data AccountIdentifier = AccountIdentifier'
    { _aiMerchantId :: !(Maybe (Textual Word64))
    , _aiAggregatorId :: !(Maybe (Textual Word64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiMerchantId'
--
-- * 'aiAggregatorId'
accountIdentifier
    :: AccountIdentifier
accountIdentifier = 
    AccountIdentifier'
    { _aiMerchantId = Nothing
    , _aiAggregatorId = Nothing
    }

-- | The merchant account ID, set for individual accounts and subaccounts.
aiMerchantId :: Lens' AccountIdentifier (Maybe Word64)
aiMerchantId
  = lens _aiMerchantId (\ s a -> s{_aiMerchantId = a})
      . mapping _Coerce

-- | The aggregator ID, set for aggregators and subaccounts (in that case, it
-- represents the aggregator of the subaccount).
aiAggregatorId :: Lens' AccountIdentifier (Maybe Word64)
aiAggregatorId
  = lens _aiAggregatorId
      (\ s a -> s{_aiAggregatorId = a})
      . mapping _Coerce

instance FromJSON AccountIdentifier where
        parseJSON
          = withObject "AccountIdentifier"
              (\ o ->
                 AccountIdentifier' <$>
                   (o .:? "merchantId") <*> (o .:? "aggregatorId"))

instance ToJSON AccountIdentifier where
        toJSON AccountIdentifier'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _aiMerchantId,
                  ("aggregatorId" .=) <$> _aiAggregatorId])

--
-- /See:/ 'posCustomBatchRequest' smart constructor.
newtype PosCustomBatchRequest = PosCustomBatchRequest'
    { _pEntries :: Maybe [PosCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pEntries'
posCustomBatchRequest
    :: PosCustomBatchRequest
posCustomBatchRequest = 
    PosCustomBatchRequest'
    { _pEntries = Nothing
    }

-- | The request entries to be processed in the batch.
pEntries :: Lens' PosCustomBatchRequest [PosCustomBatchRequestEntry]
pEntries
  = lens _pEntries (\ s a -> s{_pEntries = a}) .
      _Default
      . _Coerce

instance FromJSON PosCustomBatchRequest where
        parseJSON
          = withObject "PosCustomBatchRequest"
              (\ o ->
                 PosCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON PosCustomBatchRequest where
        toJSON PosCustomBatchRequest'{..}
          = object (catMaybes [("entries" .=) <$> _pEntries])

--
-- /See:/ 'testOrderPaymentMethod' smart constructor.
data TestOrderPaymentMethod = TestOrderPaymentMethod'
    { _topmExpirationMonth :: !(Maybe (Textual Int32))
    , _topmExpirationYear :: !(Maybe (Textual Int32))
    , _topmLastFourDigits :: !(Maybe Text)
    , _topmType :: !(Maybe Text)
    , _topmPredefinedBillingAddress :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestOrderPaymentMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'topmExpirationMonth'
--
-- * 'topmExpirationYear'
--
-- * 'topmLastFourDigits'
--
-- * 'topmType'
--
-- * 'topmPredefinedBillingAddress'
testOrderPaymentMethod
    :: TestOrderPaymentMethod
testOrderPaymentMethod = 
    TestOrderPaymentMethod'
    { _topmExpirationMonth = Nothing
    , _topmExpirationYear = Nothing
    , _topmLastFourDigits = Nothing
    , _topmType = Nothing
    , _topmPredefinedBillingAddress = Nothing
    }

-- | The card expiration month (January = 1, February = 2 etc.).
topmExpirationMonth :: Lens' TestOrderPaymentMethod (Maybe Int32)
topmExpirationMonth
  = lens _topmExpirationMonth
      (\ s a -> s{_topmExpirationMonth = a})
      . mapping _Coerce

-- | The card expiration year (4-digit, e.g. 2015).
topmExpirationYear :: Lens' TestOrderPaymentMethod (Maybe Int32)
topmExpirationYear
  = lens _topmExpirationYear
      (\ s a -> s{_topmExpirationYear = a})
      . mapping _Coerce

-- | The last four digits of the card number.
topmLastFourDigits :: Lens' TestOrderPaymentMethod (Maybe Text)
topmLastFourDigits
  = lens _topmLastFourDigits
      (\ s a -> s{_topmLastFourDigits = a})

-- | The type of instrument. Note that real orders might have different
-- values than the four values accepted by createTestOrder.
topmType :: Lens' TestOrderPaymentMethod (Maybe Text)
topmType = lens _topmType (\ s a -> s{_topmType = a})

-- | The billing address.
topmPredefinedBillingAddress :: Lens' TestOrderPaymentMethod (Maybe Text)
topmPredefinedBillingAddress
  = lens _topmPredefinedBillingAddress
      (\ s a -> s{_topmPredefinedBillingAddress = a})

instance FromJSON TestOrderPaymentMethod where
        parseJSON
          = withObject "TestOrderPaymentMethod"
              (\ o ->
                 TestOrderPaymentMethod' <$>
                   (o .:? "expirationMonth") <*>
                     (o .:? "expirationYear")
                     <*> (o .:? "lastFourDigits")
                     <*> (o .:? "type")
                     <*> (o .:? "predefinedBillingAddress"))

instance ToJSON TestOrderPaymentMethod where
        toJSON TestOrderPaymentMethod'{..}
          = object
              (catMaybes
                 [("expirationMonth" .=) <$> _topmExpirationMonth,
                  ("expirationYear" .=) <$> _topmExpirationYear,
                  ("lastFourDigits" .=) <$> _topmLastFourDigits,
                  ("type" .=) <$> _topmType,
                  ("predefinedBillingAddress" .=) <$>
                    _topmPredefinedBillingAddress])

--
-- /See:/ 'orderLineItem' smart constructor.
data OrderLineItem = OrderLineItem'
    { _oliAnnotations :: !(Maybe [OrderMerchantProvidedAnnotation])
    , _oliQuantityOrdered :: !(Maybe (Textual Word32))
    , _oliReturnInfo :: !(Maybe OrderLineItemReturnInfo)
    , _oliQuantityDelivered :: !(Maybe (Textual Word32))
    , _oliShippingDetails :: !(Maybe OrderLineItemShippingDetails)
    , _oliQuantityPending :: !(Maybe (Textual Word32))
    , _oliCancellations :: !(Maybe [OrderCancellation])
    , _oliQuantityCanceled :: !(Maybe (Textual Word32))
    , _oliId :: !(Maybe Text)
    , _oliTax :: !(Maybe Price)
    , _oliPrice :: !(Maybe Price)
    , _oliQuantityShipped :: !(Maybe (Textual Word32))
    , _oliQuantityReturned :: !(Maybe (Textual Word32))
    , _oliProduct :: !(Maybe OrderLineItemProduct)
    , _oliReturns :: !(Maybe [OrderReturn])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oliAnnotations'
--
-- * 'oliQuantityOrdered'
--
-- * 'oliReturnInfo'
--
-- * 'oliQuantityDelivered'
--
-- * 'oliShippingDetails'
--
-- * 'oliQuantityPending'
--
-- * 'oliCancellations'
--
-- * 'oliQuantityCanceled'
--
-- * 'oliId'
--
-- * 'oliTax'
--
-- * 'oliPrice'
--
-- * 'oliQuantityShipped'
--
-- * 'oliQuantityReturned'
--
-- * 'oliProduct'
--
-- * 'oliReturns'
orderLineItem
    :: OrderLineItem
orderLineItem = 
    OrderLineItem'
    { _oliAnnotations = Nothing
    , _oliQuantityOrdered = Nothing
    , _oliReturnInfo = Nothing
    , _oliQuantityDelivered = Nothing
    , _oliShippingDetails = Nothing
    , _oliQuantityPending = Nothing
    , _oliCancellations = Nothing
    , _oliQuantityCanceled = Nothing
    , _oliId = Nothing
    , _oliTax = Nothing
    , _oliPrice = Nothing
    , _oliQuantityShipped = Nothing
    , _oliQuantityReturned = Nothing
    , _oliProduct = Nothing
    , _oliReturns = Nothing
    }

-- | Annotations that are attached to the line item.
oliAnnotations :: Lens' OrderLineItem [OrderMerchantProvidedAnnotation]
oliAnnotations
  = lens _oliAnnotations
      (\ s a -> s{_oliAnnotations = a})
      . _Default
      . _Coerce

-- | Number of items ordered.
oliQuantityOrdered :: Lens' OrderLineItem (Maybe Word32)
oliQuantityOrdered
  = lens _oliQuantityOrdered
      (\ s a -> s{_oliQuantityOrdered = a})
      . mapping _Coerce

-- | Details of the return policy for the line item.
oliReturnInfo :: Lens' OrderLineItem (Maybe OrderLineItemReturnInfo)
oliReturnInfo
  = lens _oliReturnInfo
      (\ s a -> s{_oliReturnInfo = a})

-- | Number of items delivered.
oliQuantityDelivered :: Lens' OrderLineItem (Maybe Word32)
oliQuantityDelivered
  = lens _oliQuantityDelivered
      (\ s a -> s{_oliQuantityDelivered = a})
      . mapping _Coerce

-- | Details of the requested shipping for the line item.
oliShippingDetails :: Lens' OrderLineItem (Maybe OrderLineItemShippingDetails)
oliShippingDetails
  = lens _oliShippingDetails
      (\ s a -> s{_oliShippingDetails = a})

-- | Number of items pending.
oliQuantityPending :: Lens' OrderLineItem (Maybe Word32)
oliQuantityPending
  = lens _oliQuantityPending
      (\ s a -> s{_oliQuantityPending = a})
      . mapping _Coerce

-- | Cancellations of the line item.
oliCancellations :: Lens' OrderLineItem [OrderCancellation]
oliCancellations
  = lens _oliCancellations
      (\ s a -> s{_oliCancellations = a})
      . _Default
      . _Coerce

-- | Number of items canceled.
oliQuantityCanceled :: Lens' OrderLineItem (Maybe Word32)
oliQuantityCanceled
  = lens _oliQuantityCanceled
      (\ s a -> s{_oliQuantityCanceled = a})
      . mapping _Coerce

-- | The id of the line item.
oliId :: Lens' OrderLineItem (Maybe Text)
oliId = lens _oliId (\ s a -> s{_oliId = a})

-- | Total tax amount for the line item. For example, if two items are
-- purchased, and each have a cost tax of $2, the total tax amount will be
-- $4.
oliTax :: Lens' OrderLineItem (Maybe Price)
oliTax = lens _oliTax (\ s a -> s{_oliTax = a})

-- | Total price for the line item. For example, if two items for $10 are
-- purchased, the total price will be $20.
oliPrice :: Lens' OrderLineItem (Maybe Price)
oliPrice = lens _oliPrice (\ s a -> s{_oliPrice = a})

-- | Number of items shipped.
oliQuantityShipped :: Lens' OrderLineItem (Maybe Word32)
oliQuantityShipped
  = lens _oliQuantityShipped
      (\ s a -> s{_oliQuantityShipped = a})
      . mapping _Coerce

-- | Number of items returned.
oliQuantityReturned :: Lens' OrderLineItem (Maybe Word32)
oliQuantityReturned
  = lens _oliQuantityReturned
      (\ s a -> s{_oliQuantityReturned = a})
      . mapping _Coerce

-- | Product data from the time of the order placement.
oliProduct :: Lens' OrderLineItem (Maybe OrderLineItemProduct)
oliProduct
  = lens _oliProduct (\ s a -> s{_oliProduct = a})

-- | Returns of the line item.
oliReturns :: Lens' OrderLineItem [OrderReturn]
oliReturns
  = lens _oliReturns (\ s a -> s{_oliReturns = a}) .
      _Default
      . _Coerce

instance FromJSON OrderLineItem where
        parseJSON
          = withObject "OrderLineItem"
              (\ o ->
                 OrderLineItem' <$>
                   (o .:? "annotations" .!= mempty) <*>
                     (o .:? "quantityOrdered")
                     <*> (o .:? "returnInfo")
                     <*> (o .:? "quantityDelivered")
                     <*> (o .:? "shippingDetails")
                     <*> (o .:? "quantityPending")
                     <*> (o .:? "cancellations" .!= mempty)
                     <*> (o .:? "quantityCanceled")
                     <*> (o .:? "id")
                     <*> (o .:? "tax")
                     <*> (o .:? "price")
                     <*> (o .:? "quantityShipped")
                     <*> (o .:? "quantityReturned")
                     <*> (o .:? "product")
                     <*> (o .:? "returns" .!= mempty))

instance ToJSON OrderLineItem where
        toJSON OrderLineItem'{..}
          = object
              (catMaybes
                 [("annotations" .=) <$> _oliAnnotations,
                  ("quantityOrdered" .=) <$> _oliQuantityOrdered,
                  ("returnInfo" .=) <$> _oliReturnInfo,
                  ("quantityDelivered" .=) <$> _oliQuantityDelivered,
                  ("shippingDetails" .=) <$> _oliShippingDetails,
                  ("quantityPending" .=) <$> _oliQuantityPending,
                  ("cancellations" .=) <$> _oliCancellations,
                  ("quantityCanceled" .=) <$> _oliQuantityCanceled,
                  ("id" .=) <$> _oliId, ("tax" .=) <$> _oliTax,
                  ("price" .=) <$> _oliPrice,
                  ("quantityShipped" .=) <$> _oliQuantityShipped,
                  ("quantityReturned" .=) <$> _oliQuantityReturned,
                  ("product" .=) <$> _oliProduct,
                  ("returns" .=) <$> _oliReturns])

--
-- /See:/ 'service' smart constructor.
data Service = Service'
    { _sDeliveryCountry :: !(Maybe Text)
    , _sRateGroups :: !(Maybe [RateGroup])
    , _sDeliveryTime :: !(Maybe DeliveryTime)
    , _sActive :: !(Maybe Bool)
    , _sName :: !(Maybe Text)
    , _sCurrency :: !(Maybe Text)
    , _sMinimumOrderValue :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Service' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeliveryCountry'
--
-- * 'sRateGroups'
--
-- * 'sDeliveryTime'
--
-- * 'sActive'
--
-- * 'sName'
--
-- * 'sCurrency'
--
-- * 'sMinimumOrderValue'
service
    :: Service
service = 
    Service'
    { _sDeliveryCountry = Nothing
    , _sRateGroups = Nothing
    , _sDeliveryTime = Nothing
    , _sActive = Nothing
    , _sName = Nothing
    , _sCurrency = Nothing
    , _sMinimumOrderValue = Nothing
    }

-- | The CLDR territory code of the country to which the service applies.
-- Required.
sDeliveryCountry :: Lens' Service (Maybe Text)
sDeliveryCountry
  = lens _sDeliveryCountry
      (\ s a -> s{_sDeliveryCountry = a})

-- | Shipping rate group definitions. Only the last one is allowed to have an
-- empty applicableShippingLabels, which means \"everything else\". The
-- other applicableShippingLabels must not overlap.
sRateGroups :: Lens' Service [RateGroup]
sRateGroups
  = lens _sRateGroups (\ s a -> s{_sRateGroups = a}) .
      _Default
      . _Coerce

-- | Time spent in various aspects from order to the delivery of the product.
-- Required.
sDeliveryTime :: Lens' Service (Maybe DeliveryTime)
sDeliveryTime
  = lens _sDeliveryTime
      (\ s a -> s{_sDeliveryTime = a})

-- | A boolean exposing the active status of the shipping service. Required.
sActive :: Lens' Service (Maybe Bool)
sActive = lens _sActive (\ s a -> s{_sActive = a})

-- | Free-form name of the service. Must be unique within target account.
-- Required.
sName :: Lens' Service (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The CLDR code of the currency to which this service applies. Must match
-- that of the prices in rate groups.
sCurrency :: Lens' Service (Maybe Text)
sCurrency
  = lens _sCurrency (\ s a -> s{_sCurrency = a})

-- | Minimum order value for this service. If set, indicates that customers
-- will have to spend at least this amount. All prices within a service
-- must have the same currency.
sMinimumOrderValue :: Lens' Service (Maybe Price)
sMinimumOrderValue
  = lens _sMinimumOrderValue
      (\ s a -> s{_sMinimumOrderValue = a})

instance FromJSON Service where
        parseJSON
          = withObject "Service"
              (\ o ->
                 Service' <$>
                   (o .:? "deliveryCountry") <*>
                     (o .:? "rateGroups" .!= mempty)
                     <*> (o .:? "deliveryTime")
                     <*> (o .:? "active")
                     <*> (o .:? "name")
                     <*> (o .:? "currency")
                     <*> (o .:? "minimumOrderValue"))

instance ToJSON Service where
        toJSON Service'{..}
          = object
              (catMaybes
                 [("deliveryCountry" .=) <$> _sDeliveryCountry,
                  ("rateGroups" .=) <$> _sRateGroups,
                  ("deliveryTime" .=) <$> _sDeliveryTime,
                  ("active" .=) <$> _sActive, ("name" .=) <$> _sName,
                  ("currency" .=) <$> _sCurrency,
                  ("minimumOrderValue" .=) <$> _sMinimumOrderValue])

--
-- /See:/ 'productstatusesCustomBatchResponse' smart constructor.
data ProductstatusesCustomBatchResponse = ProductstatusesCustomBatchResponse'
    { _proEntries :: !(Maybe [ProductstatusesCustomBatchResponseEntry])
    , _proKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proEntries'
--
-- * 'proKind'
productstatusesCustomBatchResponse
    :: ProductstatusesCustomBatchResponse
productstatusesCustomBatchResponse = 
    ProductstatusesCustomBatchResponse'
    { _proEntries = Nothing
    , _proKind = "content#productstatusesCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
proEntries :: Lens' ProductstatusesCustomBatchResponse [ProductstatusesCustomBatchResponseEntry]
proEntries
  = lens _proEntries (\ s a -> s{_proEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productstatusesCustomBatchResponse\".
proKind :: Lens' ProductstatusesCustomBatchResponse Text
proKind = lens _proKind (\ s a -> s{_proKind = a})

instance FromJSON ProductstatusesCustomBatchResponse
         where
        parseJSON
          = withObject "ProductstatusesCustomBatchResponse"
              (\ o ->
                 ProductstatusesCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#productstatusesCustomBatchResponse"))

instance ToJSON ProductstatusesCustomBatchResponse
         where
        toJSON ProductstatusesCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _proEntries,
                  Just ("kind" .= _proKind)])

--
-- /See:/ 'productUnitPricingMeasure' smart constructor.
data ProductUnitPricingMeasure = ProductUnitPricingMeasure'
    { _pupmValue :: !(Maybe (Textual Double))
    , _pupmUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductUnitPricingMeasure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pupmValue'
--
-- * 'pupmUnit'
productUnitPricingMeasure
    :: ProductUnitPricingMeasure
productUnitPricingMeasure = 
    ProductUnitPricingMeasure'
    { _pupmValue = Nothing
    , _pupmUnit = Nothing
    }

-- | The measure of an item.
pupmValue :: Lens' ProductUnitPricingMeasure (Maybe Double)
pupmValue
  = lens _pupmValue (\ s a -> s{_pupmValue = a}) .
      mapping _Coerce

-- | The unit of the measure.
pupmUnit :: Lens' ProductUnitPricingMeasure (Maybe Text)
pupmUnit = lens _pupmUnit (\ s a -> s{_pupmUnit = a})

instance FromJSON ProductUnitPricingMeasure where
        parseJSON
          = withObject "ProductUnitPricingMeasure"
              (\ o ->
                 ProductUnitPricingMeasure' <$>
                   (o .:? "value") <*> (o .:? "unit"))

instance ToJSON ProductUnitPricingMeasure where
        toJSON ProductUnitPricingMeasure'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pupmValue,
                  ("unit" .=) <$> _pupmUnit])

--
-- /See:/ 'ordersUpdateShipmentRequest' smart constructor.
data OrdersUpdateShipmentRequest = OrdersUpdateShipmentRequest'
    { _ousrCarrier :: !(Maybe Text)
    , _ousrStatus :: !(Maybe Text)
    , _ousrTrackingId :: !(Maybe Text)
    , _ousrShipmentId :: !(Maybe Text)
    , _ousrOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateShipmentRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ousrCarrier'
--
-- * 'ousrStatus'
--
-- * 'ousrTrackingId'
--
-- * 'ousrShipmentId'
--
-- * 'ousrOperationId'
ordersUpdateShipmentRequest
    :: OrdersUpdateShipmentRequest
ordersUpdateShipmentRequest = 
    OrdersUpdateShipmentRequest'
    { _ousrCarrier = Nothing
    , _ousrStatus = Nothing
    , _ousrTrackingId = Nothing
    , _ousrShipmentId = Nothing
    , _ousrOperationId = Nothing
    }

-- | The carrier handling the shipment. Not updated if missing. See
-- shipments[].carrier in the Orders resource representation for a list of
-- acceptable values.
ousrCarrier :: Lens' OrdersUpdateShipmentRequest (Maybe Text)
ousrCarrier
  = lens _ousrCarrier (\ s a -> s{_ousrCarrier = a})

-- | New status for the shipment. Not updated if missing.
ousrStatus :: Lens' OrdersUpdateShipmentRequest (Maybe Text)
ousrStatus
  = lens _ousrStatus (\ s a -> s{_ousrStatus = a})

-- | The tracking id for the shipment. Not updated if missing.
ousrTrackingId :: Lens' OrdersUpdateShipmentRequest (Maybe Text)
ousrTrackingId
  = lens _ousrTrackingId
      (\ s a -> s{_ousrTrackingId = a})

-- | The ID of the shipment.
ousrShipmentId :: Lens' OrdersUpdateShipmentRequest (Maybe Text)
ousrShipmentId
  = lens _ousrShipmentId
      (\ s a -> s{_ousrShipmentId = a})

-- | The ID of the operation. Unique across all operations for a given order.
ousrOperationId :: Lens' OrdersUpdateShipmentRequest (Maybe Text)
ousrOperationId
  = lens _ousrOperationId
      (\ s a -> s{_ousrOperationId = a})

instance FromJSON OrdersUpdateShipmentRequest where
        parseJSON
          = withObject "OrdersUpdateShipmentRequest"
              (\ o ->
                 OrdersUpdateShipmentRequest' <$>
                   (o .:? "carrier") <*> (o .:? "status") <*>
                     (o .:? "trackingId")
                     <*> (o .:? "shipmentId")
                     <*> (o .:? "operationId"))

instance ToJSON OrdersUpdateShipmentRequest where
        toJSON OrdersUpdateShipmentRequest'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _ousrCarrier,
                  ("status" .=) <$> _ousrStatus,
                  ("trackingId" .=) <$> _ousrTrackingId,
                  ("shipmentId" .=) <$> _ousrShipmentId,
                  ("operationId" .=) <$> _ousrOperationId])

--
-- /See:/ 'orderShipmentLineItemShipment' smart constructor.
data OrderShipmentLineItemShipment = OrderShipmentLineItemShipment'
    { _oslisQuantity :: !(Maybe (Textual Word32))
    , _oslisLineItemId :: !(Maybe Text)
    , _oslisProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderShipmentLineItemShipment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslisQuantity'
--
-- * 'oslisLineItemId'
--
-- * 'oslisProductId'
orderShipmentLineItemShipment
    :: OrderShipmentLineItemShipment
orderShipmentLineItemShipment = 
    OrderShipmentLineItemShipment'
    { _oslisQuantity = Nothing
    , _oslisLineItemId = Nothing
    , _oslisProductId = Nothing
    }

-- | The quantity that is shipped.
oslisQuantity :: Lens' OrderShipmentLineItemShipment (Maybe Word32)
oslisQuantity
  = lens _oslisQuantity
      (\ s a -> s{_oslisQuantity = a})
      . mapping _Coerce

-- | The id of the line item that is shipped. Either lineItemId or productId
-- is required.
oslisLineItemId :: Lens' OrderShipmentLineItemShipment (Maybe Text)
oslisLineItemId
  = lens _oslisLineItemId
      (\ s a -> s{_oslisLineItemId = a})

-- | The ID of the product to ship. This is the REST ID used in the products
-- service. Either lineItemId or productId is required.
oslisProductId :: Lens' OrderShipmentLineItemShipment (Maybe Text)
oslisProductId
  = lens _oslisProductId
      (\ s a -> s{_oslisProductId = a})

instance FromJSON OrderShipmentLineItemShipment where
        parseJSON
          = withObject "OrderShipmentLineItemShipment"
              (\ o ->
                 OrderShipmentLineItemShipment' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "productId"))

instance ToJSON OrderShipmentLineItemShipment where
        toJSON OrderShipmentLineItemShipment'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _oslisQuantity,
                  ("lineItemId" .=) <$> _oslisLineItemId,
                  ("productId" .=) <$> _oslisProductId])

--
-- /See:/ 'loyaltyPoints' smart constructor.
data LoyaltyPoints = LoyaltyPoints'
    { _lpRatio :: !(Maybe (Textual Double))
    , _lpPointsValue :: !(Maybe (Textual Int64))
    , _lpName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoyaltyPoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpRatio'
--
-- * 'lpPointsValue'
--
-- * 'lpName'
loyaltyPoints
    :: LoyaltyPoints
loyaltyPoints = 
    LoyaltyPoints'
    { _lpRatio = Nothing
    , _lpPointsValue = Nothing
    , _lpName = Nothing
    }

-- | The ratio of a point when converted to currency. Google assumes currency
-- based on Merchant Center settings. If ratio is left out, it defaults to
-- 1.0.
lpRatio :: Lens' LoyaltyPoints (Maybe Double)
lpRatio
  = lens _lpRatio (\ s a -> s{_lpRatio = a}) .
      mapping _Coerce

-- | The retailer\'s loyalty points in absolute value.
lpPointsValue :: Lens' LoyaltyPoints (Maybe Int64)
lpPointsValue
  = lens _lpPointsValue
      (\ s a -> s{_lpPointsValue = a})
      . mapping _Coerce

-- | Name of loyalty points program. It is recommended to limit the name to
-- 12 full-width characters or 24 Roman characters.
lpName :: Lens' LoyaltyPoints (Maybe Text)
lpName = lens _lpName (\ s a -> s{_lpName = a})

instance FromJSON LoyaltyPoints where
        parseJSON
          = withObject "LoyaltyPoints"
              (\ o ->
                 LoyaltyPoints' <$>
                   (o .:? "ratio") <*> (o .:? "pointsValue") <*>
                     (o .:? "name"))

instance ToJSON LoyaltyPoints where
        toJSON LoyaltyPoints'{..}
          = object
              (catMaybes
                 [("ratio" .=) <$> _lpRatio,
                  ("pointsValue" .=) <$> _lpPointsValue,
                  ("name" .=) <$> _lpName])

--
-- /See:/ 'ordersCustomBatchRequestEntryShipLineItems' smart constructor.
data OrdersCustomBatchRequestEntryShipLineItems = OrdersCustomBatchRequestEntryShipLineItems'
    { _ocbresliCarrier :: !(Maybe Text)
    , _ocbresliTrackingId :: !(Maybe Text)
    , _ocbresliShipmentId :: !(Maybe Text)
    , _ocbresliShipmentInfos :: !(Maybe [OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo])
    , _ocbresliLineItems :: !(Maybe [OrderShipmentLineItemShipment])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryShipLineItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbresliCarrier'
--
-- * 'ocbresliTrackingId'
--
-- * 'ocbresliShipmentId'
--
-- * 'ocbresliShipmentInfos'
--
-- * 'ocbresliLineItems'
ordersCustomBatchRequestEntryShipLineItems
    :: OrdersCustomBatchRequestEntryShipLineItems
ordersCustomBatchRequestEntryShipLineItems = 
    OrdersCustomBatchRequestEntryShipLineItems'
    { _ocbresliCarrier = Nothing
    , _ocbresliTrackingId = Nothing
    , _ocbresliShipmentId = Nothing
    , _ocbresliShipmentInfos = Nothing
    , _ocbresliLineItems = Nothing
    }

-- | Deprecated. Please use shipmentInfo instead. The carrier handling the
-- shipment. See shipments[].carrier in the Orders resource representation
-- for a list of acceptable values.
ocbresliCarrier :: Lens' OrdersCustomBatchRequestEntryShipLineItems (Maybe Text)
ocbresliCarrier
  = lens _ocbresliCarrier
      (\ s a -> s{_ocbresliCarrier = a})

-- | Deprecated. Please use shipmentInfo instead. The tracking id for the
-- shipment.
ocbresliTrackingId :: Lens' OrdersCustomBatchRequestEntryShipLineItems (Maybe Text)
ocbresliTrackingId
  = lens _ocbresliTrackingId
      (\ s a -> s{_ocbresliTrackingId = a})

-- | Deprecated. Please use shipmentInfo instead. The ID of the shipment.
ocbresliShipmentId :: Lens' OrdersCustomBatchRequestEntryShipLineItems (Maybe Text)
ocbresliShipmentId
  = lens _ocbresliShipmentId
      (\ s a -> s{_ocbresliShipmentId = a})

-- | Shipment information. This field is repeated because a single line item
-- can be shipped in several packages (and have several tracking IDs).
ocbresliShipmentInfos :: Lens' OrdersCustomBatchRequestEntryShipLineItems [OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo]
ocbresliShipmentInfos
  = lens _ocbresliShipmentInfos
      (\ s a -> s{_ocbresliShipmentInfos = a})
      . _Default
      . _Coerce

-- | Line items to ship.
ocbresliLineItems :: Lens' OrdersCustomBatchRequestEntryShipLineItems [OrderShipmentLineItemShipment]
ocbresliLineItems
  = lens _ocbresliLineItems
      (\ s a -> s{_ocbresliLineItems = a})
      . _Default
      . _Coerce

instance FromJSON
         OrdersCustomBatchRequestEntryShipLineItems where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryShipLineItems"
              (\ o ->
                 OrdersCustomBatchRequestEntryShipLineItems' <$>
                   (o .:? "carrier") <*> (o .:? "trackingId") <*>
                     (o .:? "shipmentId")
                     <*> (o .:? "shipmentInfos" .!= mempty)
                     <*> (o .:? "lineItems" .!= mempty))

instance ToJSON
         OrdersCustomBatchRequestEntryShipLineItems where
        toJSON
          OrdersCustomBatchRequestEntryShipLineItems'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _ocbresliCarrier,
                  ("trackingId" .=) <$> _ocbresliTrackingId,
                  ("shipmentId" .=) <$> _ocbresliShipmentId,
                  ("shipmentInfos" .=) <$> _ocbresliShipmentInfos,
                  ("lineItems" .=) <$> _ocbresliLineItems])

-- | The status of an account, i.e., information about its products, which is
-- computed offline and not returned immediately at insertion time.
--
-- /See:/ 'accountStatus' smart constructor.
data AccountStatus = AccountStatus'
    { _asDataQualityIssues :: !(Maybe [AccountStatusDataQualityIssue])
    , _asAccountLevelIssues :: !(Maybe [AccountStatusAccountLevelIssue])
    , _asKind :: !Text
    , _asAccountId :: !(Maybe Text)
    , _asWebsiteClaimed :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asDataQualityIssues'
--
-- * 'asAccountLevelIssues'
--
-- * 'asKind'
--
-- * 'asAccountId'
--
-- * 'asWebsiteClaimed'
accountStatus
    :: AccountStatus
accountStatus = 
    AccountStatus'
    { _asDataQualityIssues = Nothing
    , _asAccountLevelIssues = Nothing
    , _asKind = "content#accountStatus"
    , _asAccountId = Nothing
    , _asWebsiteClaimed = Nothing
    }

-- | A list of data quality issues.
asDataQualityIssues :: Lens' AccountStatus [AccountStatusDataQualityIssue]
asDataQualityIssues
  = lens _asDataQualityIssues
      (\ s a -> s{_asDataQualityIssues = a})
      . _Default
      . _Coerce

-- | A list of account level issues.
asAccountLevelIssues :: Lens' AccountStatus [AccountStatusAccountLevelIssue]
asAccountLevelIssues
  = lens _asAccountLevelIssues
      (\ s a -> s{_asAccountLevelIssues = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountStatus\".
asKind :: Lens' AccountStatus Text
asKind = lens _asKind (\ s a -> s{_asKind = a})

-- | The ID of the account for which the status is reported.
asAccountId :: Lens' AccountStatus (Maybe Text)
asAccountId
  = lens _asAccountId (\ s a -> s{_asAccountId = a})

-- | Whether the account\'s website is claimed or not.
asWebsiteClaimed :: Lens' AccountStatus (Maybe Bool)
asWebsiteClaimed
  = lens _asWebsiteClaimed
      (\ s a -> s{_asWebsiteClaimed = a})

instance FromJSON AccountStatus where
        parseJSON
          = withObject "AccountStatus"
              (\ o ->
                 AccountStatus' <$>
                   (o .:? "dataQualityIssues" .!= mempty) <*>
                     (o .:? "accountLevelIssues" .!= mempty)
                     <*> (o .:? "kind" .!= "content#accountStatus")
                     <*> (o .:? "accountId")
                     <*> (o .:? "websiteClaimed"))

instance ToJSON AccountStatus where
        toJSON AccountStatus'{..}
          = object
              (catMaybes
                 [("dataQualityIssues" .=) <$> _asDataQualityIssues,
                  ("accountLevelIssues" .=) <$> _asAccountLevelIssues,
                  Just ("kind" .= _asKind),
                  ("accountId" .=) <$> _asAccountId,
                  ("websiteClaimed" .=) <$> _asWebsiteClaimed])

--
-- /See:/ 'ordersReturnLineItemRequest' smart constructor.
data OrdersReturnLineItemRequest = OrdersReturnLineItemRequest'
    { _orlirQuantity :: !(Maybe (Textual Word32))
    , _orlirLineItemId :: !(Maybe Text)
    , _orlirReason :: !(Maybe Text)
    , _orlirOperationId :: !(Maybe Text)
    , _orlirProductId :: !(Maybe Text)
    , _orlirReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnLineItemRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orlirQuantity'
--
-- * 'orlirLineItemId'
--
-- * 'orlirReason'
--
-- * 'orlirOperationId'
--
-- * 'orlirProductId'
--
-- * 'orlirReasonText'
ordersReturnLineItemRequest
    :: OrdersReturnLineItemRequest
ordersReturnLineItemRequest = 
    OrdersReturnLineItemRequest'
    { _orlirQuantity = Nothing
    , _orlirLineItemId = Nothing
    , _orlirReason = Nothing
    , _orlirOperationId = Nothing
    , _orlirProductId = Nothing
    , _orlirReasonText = Nothing
    }

-- | The quantity to return.
orlirQuantity :: Lens' OrdersReturnLineItemRequest (Maybe Word32)
orlirQuantity
  = lens _orlirQuantity
      (\ s a -> s{_orlirQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
orlirLineItemId :: Lens' OrdersReturnLineItemRequest (Maybe Text)
orlirLineItemId
  = lens _orlirLineItemId
      (\ s a -> s{_orlirLineItemId = a})

-- | The reason for the return.
orlirReason :: Lens' OrdersReturnLineItemRequest (Maybe Text)
orlirReason
  = lens _orlirReason (\ s a -> s{_orlirReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
orlirOperationId :: Lens' OrdersReturnLineItemRequest (Maybe Text)
orlirOperationId
  = lens _orlirOperationId
      (\ s a -> s{_orlirOperationId = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
orlirProductId :: Lens' OrdersReturnLineItemRequest (Maybe Text)
orlirProductId
  = lens _orlirProductId
      (\ s a -> s{_orlirProductId = a})

-- | The explanation of the reason.
orlirReasonText :: Lens' OrdersReturnLineItemRequest (Maybe Text)
orlirReasonText
  = lens _orlirReasonText
      (\ s a -> s{_orlirReasonText = a})

instance FromJSON OrdersReturnLineItemRequest where
        parseJSON
          = withObject "OrdersReturnLineItemRequest"
              (\ o ->
                 OrdersReturnLineItemRequest' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "operationId")
                     <*> (o .:? "productId")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersReturnLineItemRequest where
        toJSON OrdersReturnLineItemRequest'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _orlirQuantity,
                  ("lineItemId" .=) <$> _orlirLineItemId,
                  ("reason" .=) <$> _orlirReason,
                  ("operationId" .=) <$> _orlirOperationId,
                  ("productId" .=) <$> _orlirProductId,
                  ("reasonText" .=) <$> _orlirReasonText])

-- | A batch entry encoding a single non-batch shippingsettings request.
--
-- /See:/ 'shippingSettingsCustomBatchRequestEntry' smart constructor.
data ShippingSettingsCustomBatchRequestEntry = ShippingSettingsCustomBatchRequestEntry'
    { _sscbreMerchantId :: !(Maybe (Textual Word64))
    , _sscbreAccountId :: !(Maybe (Textual Word64))
    , _sscbreMethod :: !(Maybe Text)
    , _sscbreShippingSettings :: !(Maybe ShippingSettings)
    , _sscbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscbreMerchantId'
--
-- * 'sscbreAccountId'
--
-- * 'sscbreMethod'
--
-- * 'sscbreShippingSettings'
--
-- * 'sscbreBatchId'
shippingSettingsCustomBatchRequestEntry
    :: ShippingSettingsCustomBatchRequestEntry
shippingSettingsCustomBatchRequestEntry = 
    ShippingSettingsCustomBatchRequestEntry'
    { _sscbreMerchantId = Nothing
    , _sscbreAccountId = Nothing
    , _sscbreMethod = Nothing
    , _sscbreShippingSettings = Nothing
    , _sscbreBatchId = Nothing
    }

-- | The ID of the managing account.
sscbreMerchantId :: Lens' ShippingSettingsCustomBatchRequestEntry (Maybe Word64)
sscbreMerchantId
  = lens _sscbreMerchantId
      (\ s a -> s{_sscbreMerchantId = a})
      . mapping _Coerce

-- | The ID of the account for which to get\/update account shipping
-- settings.
sscbreAccountId :: Lens' ShippingSettingsCustomBatchRequestEntry (Maybe Word64)
sscbreAccountId
  = lens _sscbreAccountId
      (\ s a -> s{_sscbreAccountId = a})
      . mapping _Coerce

sscbreMethod :: Lens' ShippingSettingsCustomBatchRequestEntry (Maybe Text)
sscbreMethod
  = lens _sscbreMethod (\ s a -> s{_sscbreMethod = a})

-- | The account shipping settings to update. Only defined if the method is
-- update.
sscbreShippingSettings :: Lens' ShippingSettingsCustomBatchRequestEntry (Maybe ShippingSettings)
sscbreShippingSettings
  = lens _sscbreShippingSettings
      (\ s a -> s{_sscbreShippingSettings = a})

-- | An entry ID, unique within the batch request.
sscbreBatchId :: Lens' ShippingSettingsCustomBatchRequestEntry (Maybe Word32)
sscbreBatchId
  = lens _sscbreBatchId
      (\ s a -> s{_sscbreBatchId = a})
      . mapping _Coerce

instance FromJSON
         ShippingSettingsCustomBatchRequestEntry where
        parseJSON
          = withObject
              "ShippingSettingsCustomBatchRequestEntry"
              (\ o ->
                 ShippingSettingsCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "accountId") <*>
                     (o .:? "method")
                     <*> (o .:? "shippingSettings")
                     <*> (o .:? "batchId"))

instance ToJSON
         ShippingSettingsCustomBatchRequestEntry where
        toJSON ShippingSettingsCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _sscbreMerchantId,
                  ("accountId" .=) <$> _sscbreAccountId,
                  ("method" .=) <$> _sscbreMethod,
                  ("shippingSettings" .=) <$> _sscbreShippingSettings,
                  ("batchId" .=) <$> _sscbreBatchId])

--
-- /See:/ 'posInventoryResponse' smart constructor.
data PosInventoryResponse = PosInventoryResponse'
    { _pirStoreCode :: !(Maybe Text)
    , _pirKind :: !Text
    , _pirItemId :: !(Maybe Text)
    , _pirQuantity :: !(Maybe (Textual Int64))
    , _pirTargetCountry :: !(Maybe Text)
    , _pirGtin :: !(Maybe Text)
    , _pirPrice :: !(Maybe Price)
    , _pirContentLanguage :: !(Maybe Text)
    , _pirTimestamp :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosInventoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pirStoreCode'
--
-- * 'pirKind'
--
-- * 'pirItemId'
--
-- * 'pirQuantity'
--
-- * 'pirTargetCountry'
--
-- * 'pirGtin'
--
-- * 'pirPrice'
--
-- * 'pirContentLanguage'
--
-- * 'pirTimestamp'
posInventoryResponse
    :: PosInventoryResponse
posInventoryResponse = 
    PosInventoryResponse'
    { _pirStoreCode = Nothing
    , _pirKind = "content#posInventoryResponse"
    , _pirItemId = Nothing
    , _pirQuantity = Nothing
    , _pirTargetCountry = Nothing
    , _pirGtin = Nothing
    , _pirPrice = Nothing
    , _pirContentLanguage = Nothing
    , _pirTimestamp = Nothing
    }

-- | The identifier of the merchant\'s store.
pirStoreCode :: Lens' PosInventoryResponse (Maybe Text)
pirStoreCode
  = lens _pirStoreCode (\ s a -> s{_pirStoreCode = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posInventoryResponse\".
pirKind :: Lens' PosInventoryResponse Text
pirKind = lens _pirKind (\ s a -> s{_pirKind = a})

-- | A unique identifier for the item.
pirItemId :: Lens' PosInventoryResponse (Maybe Text)
pirItemId
  = lens _pirItemId (\ s a -> s{_pirItemId = a})

-- | The available quantity of the item.
pirQuantity :: Lens' PosInventoryResponse (Maybe Int64)
pirQuantity
  = lens _pirQuantity (\ s a -> s{_pirQuantity = a}) .
      mapping _Coerce

-- | The CLDR territory code for the item.
pirTargetCountry :: Lens' PosInventoryResponse (Maybe Text)
pirTargetCountry
  = lens _pirTargetCountry
      (\ s a -> s{_pirTargetCountry = a})

-- | Global Trade Item Number.
pirGtin :: Lens' PosInventoryResponse (Maybe Text)
pirGtin = lens _pirGtin (\ s a -> s{_pirGtin = a})

-- | The current price of the item.
pirPrice :: Lens' PosInventoryResponse (Maybe Price)
pirPrice = lens _pirPrice (\ s a -> s{_pirPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
pirContentLanguage :: Lens' PosInventoryResponse (Maybe Text)
pirContentLanguage
  = lens _pirContentLanguage
      (\ s a -> s{_pirContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
pirTimestamp :: Lens' PosInventoryResponse (Maybe Text)
pirTimestamp
  = lens _pirTimestamp (\ s a -> s{_pirTimestamp = a})

instance FromJSON PosInventoryResponse where
        parseJSON
          = withObject "PosInventoryResponse"
              (\ o ->
                 PosInventoryResponse' <$>
                   (o .:? "storeCode") <*>
                     (o .:? "kind" .!= "content#posInventoryResponse")
                     <*> (o .:? "itemId")
                     <*> (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp"))

instance ToJSON PosInventoryResponse where
        toJSON PosInventoryResponse'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _pirStoreCode,
                  Just ("kind" .= _pirKind),
                  ("itemId" .=) <$> _pirItemId,
                  ("quantity" .=) <$> _pirQuantity,
                  ("targetCountry" .=) <$> _pirTargetCountry,
                  ("gtin" .=) <$> _pirGtin, ("price" .=) <$> _pirPrice,
                  ("contentLanguage" .=) <$> _pirContentLanguage,
                  ("timestamp" .=) <$> _pirTimestamp])

--
-- /See:/ 'ordersCustomBatchRequestEntryUpdateLineItemShippingDetails' smart constructor.
data OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails = OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails'
    { _ocbreulisdShipByDate :: !(Maybe Text)
    , _ocbreulisdLineItemId :: !(Maybe Text)
    , _ocbreulisdDeliverByDate :: !(Maybe Text)
    , _ocbreulisdProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreulisdShipByDate'
--
-- * 'ocbreulisdLineItemId'
--
-- * 'ocbreulisdDeliverByDate'
--
-- * 'ocbreulisdProductId'
ordersCustomBatchRequestEntryUpdateLineItemShippingDetails
    :: OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails
ordersCustomBatchRequestEntryUpdateLineItemShippingDetails = 
    OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails'
    { _ocbreulisdShipByDate = Nothing
    , _ocbreulisdLineItemId = Nothing
    , _ocbreulisdDeliverByDate = Nothing
    , _ocbreulisdProductId = Nothing
    }

-- | Updated ship by date, in ISO 8601 format. If not specified only deliver
-- by date is updated.
ocbreulisdShipByDate :: Lens' OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails (Maybe Text)
ocbreulisdShipByDate
  = lens _ocbreulisdShipByDate
      (\ s a -> s{_ocbreulisdShipByDate = a})

-- | The ID of the line item to set metadata. Either lineItemId or productId
-- is required.
ocbreulisdLineItemId :: Lens' OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails (Maybe Text)
ocbreulisdLineItemId
  = lens _ocbreulisdLineItemId
      (\ s a -> s{_ocbreulisdLineItemId = a})

-- | Updated delivery by date, in ISO 8601 format. If not specified only ship
-- by date is updated.
ocbreulisdDeliverByDate :: Lens' OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails (Maybe Text)
ocbreulisdDeliverByDate
  = lens _ocbreulisdDeliverByDate
      (\ s a -> s{_ocbreulisdDeliverByDate = a})

-- | The ID of the product to set metadata. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbreulisdProductId :: Lens' OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails (Maybe Text)
ocbreulisdProductId
  = lens _ocbreulisdProductId
      (\ s a -> s{_ocbreulisdProductId = a})

instance FromJSON
         OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails"
              (\ o ->
                 OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails'
                   <$>
                   (o .:? "shipByDate") <*> (o .:? "lineItemId") <*>
                     (o .:? "deliverByDate")
                     <*> (o .:? "productId"))

instance ToJSON
         OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails
         where
        toJSON
          OrdersCustomBatchRequestEntryUpdateLineItemShippingDetails'{..}
          = object
              (catMaybes
                 [("shipByDate" .=) <$> _ocbreulisdShipByDate,
                  ("lineItemId" .=) <$> _ocbreulisdLineItemId,
                  ("deliverByDate" .=) <$> _ocbreulisdDeliverByDate,
                  ("productId" .=) <$> _ocbreulisdProductId])

--
-- /See:/ 'accountstatusesCustomBatchRequest' smart constructor.
newtype AccountstatusesCustomBatchRequest = AccountstatusesCustomBatchRequest'
    { _acbrEntries :: Maybe [AccountstatusesCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbrEntries'
accountstatusesCustomBatchRequest
    :: AccountstatusesCustomBatchRequest
accountstatusesCustomBatchRequest = 
    AccountstatusesCustomBatchRequest'
    { _acbrEntries = Nothing
    }

-- | The request entries to be processed in the batch.
acbrEntries :: Lens' AccountstatusesCustomBatchRequest [AccountstatusesCustomBatchRequestEntry]
acbrEntries
  = lens _acbrEntries (\ s a -> s{_acbrEntries = a}) .
      _Default
      . _Coerce

instance FromJSON AccountstatusesCustomBatchRequest
         where
        parseJSON
          = withObject "AccountstatusesCustomBatchRequest"
              (\ o ->
                 AccountstatusesCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON AccountstatusesCustomBatchRequest
         where
        toJSON AccountstatusesCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _acbrEntries])

--
-- /See:/ 'accounttaxListResponse' smart constructor.
data AccounttaxListResponse = AccounttaxListResponse'
    { _alrNextPageToken :: !(Maybe Text)
    , _alrKind :: !Text
    , _alrResources :: !(Maybe [AccountTax])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alrNextPageToken'
--
-- * 'alrKind'
--
-- * 'alrResources'
accounttaxListResponse
    :: AccounttaxListResponse
accounttaxListResponse = 
    AccounttaxListResponse'
    { _alrNextPageToken = Nothing
    , _alrKind = "content#accounttaxListResponse"
    , _alrResources = Nothing
    }

-- | The token for the retrieval of the next page of account tax settings.
alrNextPageToken :: Lens' AccounttaxListResponse (Maybe Text)
alrNextPageToken
  = lens _alrNextPageToken
      (\ s a -> s{_alrNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accounttaxListResponse\".
alrKind :: Lens' AccounttaxListResponse Text
alrKind = lens _alrKind (\ s a -> s{_alrKind = a})

alrResources :: Lens' AccounttaxListResponse [AccountTax]
alrResources
  = lens _alrResources (\ s a -> s{_alrResources = a})
      . _Default
      . _Coerce

instance FromJSON AccounttaxListResponse where
        parseJSON
          = withObject "AccounttaxListResponse"
              (\ o ->
                 AccounttaxListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "content#accounttaxListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON AccounttaxListResponse where
        toJSON AccounttaxListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _alrNextPageToken,
                  Just ("kind" .= _alrKind),
                  ("resources" .=) <$> _alrResources])

--
-- /See:/ 'ordersGetTestOrderTemplateResponse' smart constructor.
data OrdersGetTestOrderTemplateResponse = OrdersGetTestOrderTemplateResponse'
    { _ogtotrKind :: !Text
    , _ogtotrTemplate :: !(Maybe TestOrder)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersGetTestOrderTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogtotrKind'
--
-- * 'ogtotrTemplate'
ordersGetTestOrderTemplateResponse
    :: OrdersGetTestOrderTemplateResponse
ordersGetTestOrderTemplateResponse = 
    OrdersGetTestOrderTemplateResponse'
    { _ogtotrKind = "content#ordersGetTestOrderTemplateResponse"
    , _ogtotrTemplate = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersGetTestOrderTemplateResponse\".
ogtotrKind :: Lens' OrdersGetTestOrderTemplateResponse Text
ogtotrKind
  = lens _ogtotrKind (\ s a -> s{_ogtotrKind = a})

-- | The requested test order template.
ogtotrTemplate :: Lens' OrdersGetTestOrderTemplateResponse (Maybe TestOrder)
ogtotrTemplate
  = lens _ogtotrTemplate
      (\ s a -> s{_ogtotrTemplate = a})

instance FromJSON OrdersGetTestOrderTemplateResponse
         where
        parseJSON
          = withObject "OrdersGetTestOrderTemplateResponse"
              (\ o ->
                 OrdersGetTestOrderTemplateResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersGetTestOrderTemplateResponse")
                     <*> (o .:? "template"))

instance ToJSON OrdersGetTestOrderTemplateResponse
         where
        toJSON OrdersGetTestOrderTemplateResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ogtotrKind),
                  ("template" .=) <$> _ogtotrTemplate])

-- | A batch entry encoding a single non-batch accounts request.
--
-- /See:/ 'accountsCustomBatchRequestEntry' smart constructor.
data AccountsCustomBatchRequestEntry = AccountsCustomBatchRequestEntry'
    { _accMerchantId :: !(Maybe (Textual Word64))
    , _accForce :: !(Maybe Bool)
    , _accAccount :: !(Maybe Account)
    , _accAccountId :: !(Maybe (Textual Word64))
    , _accMethod :: !(Maybe Text)
    , _accOverwrite :: !(Maybe Bool)
    , _accBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accMerchantId'
--
-- * 'accForce'
--
-- * 'accAccount'
--
-- * 'accAccountId'
--
-- * 'accMethod'
--
-- * 'accOverwrite'
--
-- * 'accBatchId'
accountsCustomBatchRequestEntry
    :: AccountsCustomBatchRequestEntry
accountsCustomBatchRequestEntry = 
    AccountsCustomBatchRequestEntry'
    { _accMerchantId = Nothing
    , _accForce = Nothing
    , _accAccount = Nothing
    , _accAccountId = Nothing
    , _accMethod = Nothing
    , _accOverwrite = Nothing
    , _accBatchId = Nothing
    }

-- | The ID of the managing account.
accMerchantId :: Lens' AccountsCustomBatchRequestEntry (Maybe Word64)
accMerchantId
  = lens _accMerchantId
      (\ s a -> s{_accMerchantId = a})
      . mapping _Coerce

-- | Whether the account should be deleted if the account has offers. Only
-- applicable if the method is delete.
accForce :: Lens' AccountsCustomBatchRequestEntry (Maybe Bool)
accForce = lens _accForce (\ s a -> s{_accForce = a})

-- | The account to create or update. Only defined if the method is insert or
-- update.
accAccount :: Lens' AccountsCustomBatchRequestEntry (Maybe Account)
accAccount
  = lens _accAccount (\ s a -> s{_accAccount = a})

-- | The ID of the targeted account. Only defined if the method is get,
-- delete or claimwebsite.
accAccountId :: Lens' AccountsCustomBatchRequestEntry (Maybe Word64)
accAccountId
  = lens _accAccountId (\ s a -> s{_accAccountId = a})
      . mapping _Coerce

accMethod :: Lens' AccountsCustomBatchRequestEntry (Maybe Text)
accMethod
  = lens _accMethod (\ s a -> s{_accMethod = a})

-- | Only applicable if the method is claimwebsite. Indicates whether or not
-- to take the claim from another account in case there is a conflict.
accOverwrite :: Lens' AccountsCustomBatchRequestEntry (Maybe Bool)
accOverwrite
  = lens _accOverwrite (\ s a -> s{_accOverwrite = a})

-- | An entry ID, unique within the batch request.
accBatchId :: Lens' AccountsCustomBatchRequestEntry (Maybe Word32)
accBatchId
  = lens _accBatchId (\ s a -> s{_accBatchId = a}) .
      mapping _Coerce

instance FromJSON AccountsCustomBatchRequestEntry
         where
        parseJSON
          = withObject "AccountsCustomBatchRequestEntry"
              (\ o ->
                 AccountsCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "force") <*>
                     (o .:? "account")
                     <*> (o .:? "accountId")
                     <*> (o .:? "method")
                     <*> (o .:? "overwrite")
                     <*> (o .:? "batchId"))

instance ToJSON AccountsCustomBatchRequestEntry where
        toJSON AccountsCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _accMerchantId,
                  ("force" .=) <$> _accForce,
                  ("account" .=) <$> _accAccount,
                  ("accountId" .=) <$> _accAccountId,
                  ("method" .=) <$> _accMethod,
                  ("overwrite" .=) <$> _accOverwrite,
                  ("batchId" .=) <$> _accBatchId])

--
-- /See:/ 'weight' smart constructor.
data Weight = Weight'
    { _wValue :: !(Maybe Text)
    , _wUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Weight' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wValue'
--
-- * 'wUnit'
weight
    :: Weight
weight = 
    Weight'
    { _wValue = Nothing
    , _wUnit = Nothing
    }

-- | The weight represented as a number.
wValue :: Lens' Weight (Maybe Text)
wValue = lens _wValue (\ s a -> s{_wValue = a})

-- | The weight unit.
wUnit :: Lens' Weight (Maybe Text)
wUnit = lens _wUnit (\ s a -> s{_wUnit = a})

instance FromJSON Weight where
        parseJSON
          = withObject "Weight"
              (\ o ->
                 Weight' <$> (o .:? "value") <*> (o .:? "unit"))

instance ToJSON Weight where
        toJSON Weight'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _wValue, ("unit" .=) <$> _wUnit])

-- | An error returned by the API.
--
-- /See:/ 'error'' smart constructor.
data Error' = Error''
    { _eDomain :: !(Maybe Text)
    , _eReason :: !(Maybe Text)
    , _eMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Error' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eDomain'
--
-- * 'eReason'
--
-- * 'eMessage'
error'
    :: Error'
error' = 
    Error''
    { _eDomain = Nothing
    , _eReason = Nothing
    , _eMessage = Nothing
    }

-- | The domain of the error.
eDomain :: Lens' Error' (Maybe Text)
eDomain = lens _eDomain (\ s a -> s{_eDomain = a})

-- | The error code.
eReason :: Lens' Error' (Maybe Text)
eReason = lens _eReason (\ s a -> s{_eReason = a})

-- | A description of the error.
eMessage :: Lens' Error' (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromJSON Error' where
        parseJSON
          = withObject "Error"
              (\ o ->
                 Error'' <$>
                   (o .:? "domain") <*> (o .:? "reason") <*>
                     (o .:? "message"))

instance ToJSON Error' where
        toJSON Error''{..}
          = object
              (catMaybes
                 [("domain" .=) <$> _eDomain,
                  ("reason" .=) <$> _eReason,
                  ("message" .=) <$> _eMessage])

--
-- /See:/ 'productstatusesListResponse' smart constructor.
data ProductstatusesListResponse = ProductstatusesListResponse'
    { _plrNextPageToken :: !(Maybe Text)
    , _plrKind :: !Text
    , _plrResources :: !(Maybe [ProductStatus])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plrNextPageToken'
--
-- * 'plrKind'
--
-- * 'plrResources'
productstatusesListResponse
    :: ProductstatusesListResponse
productstatusesListResponse = 
    ProductstatusesListResponse'
    { _plrNextPageToken = Nothing
    , _plrKind = "content#productstatusesListResponse"
    , _plrResources = Nothing
    }

-- | The token for the retrieval of the next page of products statuses.
plrNextPageToken :: Lens' ProductstatusesListResponse (Maybe Text)
plrNextPageToken
  = lens _plrNextPageToken
      (\ s a -> s{_plrNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productstatusesListResponse\".
plrKind :: Lens' ProductstatusesListResponse Text
plrKind = lens _plrKind (\ s a -> s{_plrKind = a})

plrResources :: Lens' ProductstatusesListResponse [ProductStatus]
plrResources
  = lens _plrResources (\ s a -> s{_plrResources = a})
      . _Default
      . _Coerce

instance FromJSON ProductstatusesListResponse where
        parseJSON
          = withObject "ProductstatusesListResponse"
              (\ o ->
                 ProductstatusesListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!=
                        "content#productstatusesListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON ProductstatusesListResponse where
        toJSON ProductstatusesListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _plrNextPageToken,
                  Just ("kind" .= _plrKind),
                  ("resources" .=) <$> _plrResources])

--
-- /See:/ 'posCustomBatchResponse' smart constructor.
data PosCustomBatchResponse = PosCustomBatchResponse'
    { _posEntries :: !(Maybe [PosCustomBatchResponseEntry])
    , _posKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'posEntries'
--
-- * 'posKind'
posCustomBatchResponse
    :: PosCustomBatchResponse
posCustomBatchResponse = 
    PosCustomBatchResponse'
    { _posEntries = Nothing
    , _posKind = "content#posCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
posEntries :: Lens' PosCustomBatchResponse [PosCustomBatchResponseEntry]
posEntries
  = lens _posEntries (\ s a -> s{_posEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posCustomBatchResponse\".
posKind :: Lens' PosCustomBatchResponse Text
posKind = lens _posKind (\ s a -> s{_posKind = a})

instance FromJSON PosCustomBatchResponse where
        parseJSON
          = withObject "PosCustomBatchResponse"
              (\ o ->
                 PosCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!= "content#posCustomBatchResponse"))

instance ToJSON PosCustomBatchResponse where
        toJSON PosCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _posEntries,
                  Just ("kind" .= _posKind)])

--
-- /See:/ 'ordersRefundResponse' smart constructor.
data OrdersRefundResponse = OrdersRefundResponse'
    { _orrKind :: !Text
    , _orrExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersRefundResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrKind'
--
-- * 'orrExecutionStatus'
ordersRefundResponse
    :: OrdersRefundResponse
ordersRefundResponse = 
    OrdersRefundResponse'
    { _orrKind = "content#ordersRefundResponse"
    , _orrExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersRefundResponse\".
orrKind :: Lens' OrdersRefundResponse Text
orrKind = lens _orrKind (\ s a -> s{_orrKind = a})

-- | The status of the execution.
orrExecutionStatus :: Lens' OrdersRefundResponse (Maybe Text)
orrExecutionStatus
  = lens _orrExecutionStatus
      (\ s a -> s{_orrExecutionStatus = a})

instance FromJSON OrdersRefundResponse where
        parseJSON
          = withObject "OrdersRefundResponse"
              (\ o ->
                 OrdersRefundResponse' <$>
                   (o .:? "kind" .!= "content#ordersRefundResponse") <*>
                     (o .:? "executionStatus"))

instance ToJSON OrdersRefundResponse where
        toJSON OrdersRefundResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _orrKind),
                  ("executionStatus" .=) <$> _orrExecutionStatus])

--
-- /See:/ 'ordersCreateTestOrderRequest' smart constructor.
data OrdersCreateTestOrderRequest = OrdersCreateTestOrderRequest'
    { _octorTemplateName :: !(Maybe Text)
    , _octorTestOrder :: !(Maybe TestOrder)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCreateTestOrderRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'octorTemplateName'
--
-- * 'octorTestOrder'
ordersCreateTestOrderRequest
    :: OrdersCreateTestOrderRequest
ordersCreateTestOrderRequest = 
    OrdersCreateTestOrderRequest'
    { _octorTemplateName = Nothing
    , _octorTestOrder = Nothing
    }

-- | The test order template to use. Specify as an alternative to testOrder
-- as a shortcut for retrieving a template and then creating an order using
-- that template.
octorTemplateName :: Lens' OrdersCreateTestOrderRequest (Maybe Text)
octorTemplateName
  = lens _octorTemplateName
      (\ s a -> s{_octorTemplateName = a})

-- | The test order to create.
octorTestOrder :: Lens' OrdersCreateTestOrderRequest (Maybe TestOrder)
octorTestOrder
  = lens _octorTestOrder
      (\ s a -> s{_octorTestOrder = a})

instance FromJSON OrdersCreateTestOrderRequest where
        parseJSON
          = withObject "OrdersCreateTestOrderRequest"
              (\ o ->
                 OrdersCreateTestOrderRequest' <$>
                   (o .:? "templateName") <*> (o .:? "testOrder"))

instance ToJSON OrdersCreateTestOrderRequest where
        toJSON OrdersCreateTestOrderRequest'{..}
          = object
              (catMaybes
                 [("templateName" .=) <$> _octorTemplateName,
                  ("testOrder" .=) <$> _octorTestOrder])

--
-- /See:/ 'accountUser' smart constructor.
data AccountUser = AccountUser'
    { _auAdmin :: !(Maybe Bool)
    , _auEmailAddress :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auAdmin'
--
-- * 'auEmailAddress'
accountUser
    :: AccountUser
accountUser = 
    AccountUser'
    { _auAdmin = Nothing
    , _auEmailAddress = Nothing
    }

-- | Whether user is an admin.
auAdmin :: Lens' AccountUser (Maybe Bool)
auAdmin = lens _auAdmin (\ s a -> s{_auAdmin = a})

-- | User\'s email address.
auEmailAddress :: Lens' AccountUser (Maybe Text)
auEmailAddress
  = lens _auEmailAddress
      (\ s a -> s{_auEmailAddress = a})

instance FromJSON AccountUser where
        parseJSON
          = withObject "AccountUser"
              (\ o ->
                 AccountUser' <$>
                   (o .:? "admin") <*> (o .:? "emailAddress"))

instance ToJSON AccountUser where
        toJSON AccountUser'{..}
          = object
              (catMaybes
                 [("admin" .=) <$> _auAdmin,
                  ("emailAddress" .=) <$> _auEmailAddress])

-- | An example of an item that has poor data quality. An item value on the
-- landing page differs from what is submitted, or conflicts with a policy.
--
-- /See:/ 'accountStatusExampleItem' smart constructor.
data AccountStatusExampleItem = AccountStatusExampleItem'
    { _aseiSubmittedValue :: !(Maybe Text)
    , _aseiLink :: !(Maybe Text)
    , _aseiItemId :: !(Maybe Text)
    , _aseiTitle :: !(Maybe Text)
    , _aseiValueOnLandingPage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountStatusExampleItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aseiSubmittedValue'
--
-- * 'aseiLink'
--
-- * 'aseiItemId'
--
-- * 'aseiTitle'
--
-- * 'aseiValueOnLandingPage'
accountStatusExampleItem
    :: AccountStatusExampleItem
accountStatusExampleItem = 
    AccountStatusExampleItem'
    { _aseiSubmittedValue = Nothing
    , _aseiLink = Nothing
    , _aseiItemId = Nothing
    , _aseiTitle = Nothing
    , _aseiValueOnLandingPage = Nothing
    }

-- | The item value that was submitted.
aseiSubmittedValue :: Lens' AccountStatusExampleItem (Maybe Text)
aseiSubmittedValue
  = lens _aseiSubmittedValue
      (\ s a -> s{_aseiSubmittedValue = a})

-- | Landing page of the item.
aseiLink :: Lens' AccountStatusExampleItem (Maybe Text)
aseiLink = lens _aseiLink (\ s a -> s{_aseiLink = a})

-- | Unique item ID as specified in the uploaded product data.
aseiItemId :: Lens' AccountStatusExampleItem (Maybe Text)
aseiItemId
  = lens _aseiItemId (\ s a -> s{_aseiItemId = a})

-- | Title of the item.
aseiTitle :: Lens' AccountStatusExampleItem (Maybe Text)
aseiTitle
  = lens _aseiTitle (\ s a -> s{_aseiTitle = a})

-- | The actual value on the landing page.
aseiValueOnLandingPage :: Lens' AccountStatusExampleItem (Maybe Text)
aseiValueOnLandingPage
  = lens _aseiValueOnLandingPage
      (\ s a -> s{_aseiValueOnLandingPage = a})

instance FromJSON AccountStatusExampleItem where
        parseJSON
          = withObject "AccountStatusExampleItem"
              (\ o ->
                 AccountStatusExampleItem' <$>
                   (o .:? "submittedValue") <*> (o .:? "link") <*>
                     (o .:? "itemId")
                     <*> (o .:? "title")
                     <*> (o .:? "valueOnLandingPage"))

instance ToJSON AccountStatusExampleItem where
        toJSON AccountStatusExampleItem'{..}
          = object
              (catMaybes
                 [("submittedValue" .=) <$> _aseiSubmittedValue,
                  ("link" .=) <$> _aseiLink,
                  ("itemId" .=) <$> _aseiItemId,
                  ("title" .=) <$> _aseiTitle,
                  ("valueOnLandingPage" .=) <$>
                    _aseiValueOnLandingPage])

--
-- /See:/ 'ordersInStoreRefundLineItemResponse' smart constructor.
data OrdersInStoreRefundLineItemResponse = OrdersInStoreRefundLineItemResponse'
    { _oisrlirKind :: !Text
    , _oisrlirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersInStoreRefundLineItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oisrlirKind'
--
-- * 'oisrlirExecutionStatus'
ordersInStoreRefundLineItemResponse
    :: OrdersInStoreRefundLineItemResponse
ordersInStoreRefundLineItemResponse = 
    OrdersInStoreRefundLineItemResponse'
    { _oisrlirKind = "content#ordersInStoreRefundLineItemResponse"
    , _oisrlirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersInStoreRefundLineItemResponse\".
oisrlirKind :: Lens' OrdersInStoreRefundLineItemResponse Text
oisrlirKind
  = lens _oisrlirKind (\ s a -> s{_oisrlirKind = a})

-- | The status of the execution.
oisrlirExecutionStatus :: Lens' OrdersInStoreRefundLineItemResponse (Maybe Text)
oisrlirExecutionStatus
  = lens _oisrlirExecutionStatus
      (\ s a -> s{_oisrlirExecutionStatus = a})

instance FromJSON OrdersInStoreRefundLineItemResponse
         where
        parseJSON
          = withObject "OrdersInStoreRefundLineItemResponse"
              (\ o ->
                 OrdersInStoreRefundLineItemResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersInStoreRefundLineItemResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersInStoreRefundLineItemResponse
         where
        toJSON OrdersInStoreRefundLineItemResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oisrlirKind),
                  ("executionStatus" .=) <$> _oisrlirExecutionStatus])

-- | A batch entry encoding a single non-batch datafeeds request.
--
-- /See:/ 'datafeedsCustomBatchRequestEntry' smart constructor.
data DatafeedsCustomBatchRequestEntry = DatafeedsCustomBatchRequestEntry'
    { _dcbreMerchantId :: !(Maybe (Textual Word64))
    , _dcbreDatafeed :: !(Maybe Datafeed)
    , _dcbreMethod :: !(Maybe Text)
    , _dcbreDatafeedId :: !(Maybe (Textual Word64))
    , _dcbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbreMerchantId'
--
-- * 'dcbreDatafeed'
--
-- * 'dcbreMethod'
--
-- * 'dcbreDatafeedId'
--
-- * 'dcbreBatchId'
datafeedsCustomBatchRequestEntry
    :: DatafeedsCustomBatchRequestEntry
datafeedsCustomBatchRequestEntry = 
    DatafeedsCustomBatchRequestEntry'
    { _dcbreMerchantId = Nothing
    , _dcbreDatafeed = Nothing
    , _dcbreMethod = Nothing
    , _dcbreDatafeedId = Nothing
    , _dcbreBatchId = Nothing
    }

-- | The ID of the managing account.
dcbreMerchantId :: Lens' DatafeedsCustomBatchRequestEntry (Maybe Word64)
dcbreMerchantId
  = lens _dcbreMerchantId
      (\ s a -> s{_dcbreMerchantId = a})
      . mapping _Coerce

-- | The data feed to insert.
dcbreDatafeed :: Lens' DatafeedsCustomBatchRequestEntry (Maybe Datafeed)
dcbreDatafeed
  = lens _dcbreDatafeed
      (\ s a -> s{_dcbreDatafeed = a})

dcbreMethod :: Lens' DatafeedsCustomBatchRequestEntry (Maybe Text)
dcbreMethod
  = lens _dcbreMethod (\ s a -> s{_dcbreMethod = a})

-- | The ID of the data feed to get or delete.
dcbreDatafeedId :: Lens' DatafeedsCustomBatchRequestEntry (Maybe Word64)
dcbreDatafeedId
  = lens _dcbreDatafeedId
      (\ s a -> s{_dcbreDatafeedId = a})
      . mapping _Coerce

-- | An entry ID, unique within the batch request.
dcbreBatchId :: Lens' DatafeedsCustomBatchRequestEntry (Maybe Word32)
dcbreBatchId
  = lens _dcbreBatchId (\ s a -> s{_dcbreBatchId = a})
      . mapping _Coerce

instance FromJSON DatafeedsCustomBatchRequestEntry
         where
        parseJSON
          = withObject "DatafeedsCustomBatchRequestEntry"
              (\ o ->
                 DatafeedsCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "datafeed") <*>
                     (o .:? "method")
                     <*> (o .:? "datafeedId")
                     <*> (o .:? "batchId"))

instance ToJSON DatafeedsCustomBatchRequestEntry
         where
        toJSON DatafeedsCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _dcbreMerchantId,
                  ("datafeed" .=) <$> _dcbreDatafeed,
                  ("method" .=) <$> _dcbreMethod,
                  ("datafeedId" .=) <$> _dcbreDatafeedId,
                  ("batchId" .=) <$> _dcbreBatchId])

--
-- /See:/ 'accountStatusAccountLevelIssue' smart constructor.
data AccountStatusAccountLevelIssue = AccountStatusAccountLevelIssue'
    { _asaliCountry :: !(Maybe Text)
    , _asaliSeverity :: !(Maybe Text)
    , _asaliId :: !(Maybe Text)
    , _asaliTitle :: !(Maybe Text)
    , _asaliDetail :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountStatusAccountLevelIssue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asaliCountry'
--
-- * 'asaliSeverity'
--
-- * 'asaliId'
--
-- * 'asaliTitle'
--
-- * 'asaliDetail'
accountStatusAccountLevelIssue
    :: AccountStatusAccountLevelIssue
accountStatusAccountLevelIssue = 
    AccountStatusAccountLevelIssue'
    { _asaliCountry = Nothing
    , _asaliSeverity = Nothing
    , _asaliId = Nothing
    , _asaliTitle = Nothing
    , _asaliDetail = Nothing
    }

-- | Country for which this issue is reported.
asaliCountry :: Lens' AccountStatusAccountLevelIssue (Maybe Text)
asaliCountry
  = lens _asaliCountry (\ s a -> s{_asaliCountry = a})

-- | Severity of the issue.
asaliSeverity :: Lens' AccountStatusAccountLevelIssue (Maybe Text)
asaliSeverity
  = lens _asaliSeverity
      (\ s a -> s{_asaliSeverity = a})

-- | Issue identifier.
asaliId :: Lens' AccountStatusAccountLevelIssue (Maybe Text)
asaliId = lens _asaliId (\ s a -> s{_asaliId = a})

-- | Short description of the issue.
asaliTitle :: Lens' AccountStatusAccountLevelIssue (Maybe Text)
asaliTitle
  = lens _asaliTitle (\ s a -> s{_asaliTitle = a})

-- | Additional details about the issue.
asaliDetail :: Lens' AccountStatusAccountLevelIssue (Maybe Text)
asaliDetail
  = lens _asaliDetail (\ s a -> s{_asaliDetail = a})

instance FromJSON AccountStatusAccountLevelIssue
         where
        parseJSON
          = withObject "AccountStatusAccountLevelIssue"
              (\ o ->
                 AccountStatusAccountLevelIssue' <$>
                   (o .:? "country") <*> (o .:? "severity") <*>
                     (o .:? "id")
                     <*> (o .:? "title")
                     <*> (o .:? "detail"))

instance ToJSON AccountStatusAccountLevelIssue where
        toJSON AccountStatusAccountLevelIssue'{..}
          = object
              (catMaybes
                 [("country" .=) <$> _asaliCountry,
                  ("severity" .=) <$> _asaliSeverity,
                  ("id" .=) <$> _asaliId, ("title" .=) <$> _asaliTitle,
                  ("detail" .=) <$> _asaliDetail])

-- | The single value of a rate group or the value of a rate group table\'s
-- cell. Exactly one of noShipping, flatRate, pricePercentage,
-- carrierRateName, subtableName must be set.
--
-- /See:/ 'value' smart constructor.
data Value = Value'
    { _vPricePercentage :: !(Maybe Text)
    , _vCarrierRateName :: !(Maybe Text)
    , _vFlatRate :: !(Maybe Price)
    , _vSubtableName :: !(Maybe Text)
    , _vNoShipping :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Value' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vPricePercentage'
--
-- * 'vCarrierRateName'
--
-- * 'vFlatRate'
--
-- * 'vSubtableName'
--
-- * 'vNoShipping'
value
    :: Value
value = 
    Value'
    { _vPricePercentage = Nothing
    , _vCarrierRateName = Nothing
    , _vFlatRate = Nothing
    , _vSubtableName = Nothing
    , _vNoShipping = Nothing
    }

-- | A percentage of the price represented as a number in decimal notation
-- (e.g., \"5.4\"). Can only be set if all other fields are not set.
vPricePercentage :: Lens' Value (Maybe Text)
vPricePercentage
  = lens _vPricePercentage
      (\ s a -> s{_vPricePercentage = a})

-- | The name of a carrier rate referring to a carrier rate defined in the
-- same rate group. Can only be set if all other fields are not set.
vCarrierRateName :: Lens' Value (Maybe Text)
vCarrierRateName
  = lens _vCarrierRateName
      (\ s a -> s{_vCarrierRateName = a})

-- | A flat rate. Can only be set if all other fields are not set.
vFlatRate :: Lens' Value (Maybe Price)
vFlatRate
  = lens _vFlatRate (\ s a -> s{_vFlatRate = a})

-- | The name of a subtable. Can only be set in table cells (i.e., not for
-- single values), and only if all other fields are not set.
vSubtableName :: Lens' Value (Maybe Text)
vSubtableName
  = lens _vSubtableName
      (\ s a -> s{_vSubtableName = a})

-- | If true, then the product can\'t ship. Must be true when set, can only
-- be set if all other fields are not set.
vNoShipping :: Lens' Value (Maybe Bool)
vNoShipping
  = lens _vNoShipping (\ s a -> s{_vNoShipping = a})

instance FromJSON Value where
        parseJSON
          = withObject "Value"
              (\ o ->
                 Value' <$>
                   (o .:? "pricePercentage") <*>
                     (o .:? "carrierRateName")
                     <*> (o .:? "flatRate")
                     <*> (o .:? "subtableName")
                     <*> (o .:? "noShipping"))

instance ToJSON Value where
        toJSON Value'{..}
          = object
              (catMaybes
                 [("pricePercentage" .=) <$> _vPricePercentage,
                  ("carrierRateName" .=) <$> _vCarrierRateName,
                  ("flatRate" .=) <$> _vFlatRate,
                  ("subtableName" .=) <$> _vSubtableName,
                  ("noShipping" .=) <$> _vNoShipping])

--
-- /See:/ 'installment' smart constructor.
data Installment = Installment'
    { _iAmount :: !(Maybe Price)
    , _iMonths :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Installment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iAmount'
--
-- * 'iMonths'
installment
    :: Installment
installment = 
    Installment'
    { _iAmount = Nothing
    , _iMonths = Nothing
    }

-- | The amount the buyer has to pay per month.
iAmount :: Lens' Installment (Maybe Price)
iAmount = lens _iAmount (\ s a -> s{_iAmount = a})

-- | The number of installments the buyer has to pay.
iMonths :: Lens' Installment (Maybe Int64)
iMonths
  = lens _iMonths (\ s a -> s{_iMonths = a}) .
      mapping _Coerce

instance FromJSON Installment where
        parseJSON
          = withObject "Installment"
              (\ o ->
                 Installment' <$>
                   (o .:? "amount") <*> (o .:? "months"))

instance ToJSON Installment where
        toJSON Installment'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _iAmount,
                  ("months" .=) <$> _iMonths])

-- | The required fields vary based on the frequency of fetching. For a
-- monthly fetch schedule, day_of_month and hour are required. For a weekly
-- fetch schedule, weekday and hour are required. For a daily fetch
-- schedule, only hour is required.
--
-- /See:/ 'datafeedFetchSchedule' smart constructor.
data DatafeedFetchSchedule = DatafeedFetchSchedule'
    { _dfsFetchURL :: !(Maybe Text)
    , _dfsUsername :: !(Maybe Text)
    , _dfsMinuteOfHour :: !(Maybe (Textual Word32))
    , _dfsPassword :: !(Maybe Text)
    , _dfsDayOfMonth :: !(Maybe (Textual Word32))
    , _dfsHour :: !(Maybe (Textual Word32))
    , _dfsWeekday :: !(Maybe Text)
    , _dfsTimeZone :: !(Maybe Text)
    , _dfsPaused :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedFetchSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsFetchURL'
--
-- * 'dfsUsername'
--
-- * 'dfsMinuteOfHour'
--
-- * 'dfsPassword'
--
-- * 'dfsDayOfMonth'
--
-- * 'dfsHour'
--
-- * 'dfsWeekday'
--
-- * 'dfsTimeZone'
--
-- * 'dfsPaused'
datafeedFetchSchedule
    :: DatafeedFetchSchedule
datafeedFetchSchedule = 
    DatafeedFetchSchedule'
    { _dfsFetchURL = Nothing
    , _dfsUsername = Nothing
    , _dfsMinuteOfHour = Nothing
    , _dfsPassword = Nothing
    , _dfsDayOfMonth = Nothing
    , _dfsHour = Nothing
    , _dfsWeekday = Nothing
    , _dfsTimeZone = Nothing
    , _dfsPaused = Nothing
    }

-- | The URL where the feed file can be fetched. Google Merchant Center will
-- support automatic scheduled uploads using the HTTP, HTTPS, FTP, or SFTP
-- protocols, so the value will need to be a valid link using one of those
-- four protocols.
dfsFetchURL :: Lens' DatafeedFetchSchedule (Maybe Text)
dfsFetchURL
  = lens _dfsFetchURL (\ s a -> s{_dfsFetchURL = a})

-- | An optional user name for fetch_url.
dfsUsername :: Lens' DatafeedFetchSchedule (Maybe Text)
dfsUsername
  = lens _dfsUsername (\ s a -> s{_dfsUsername = a})

-- | The minute of the hour the feed file should be fetched (0-59).
-- Read-only.
dfsMinuteOfHour :: Lens' DatafeedFetchSchedule (Maybe Word32)
dfsMinuteOfHour
  = lens _dfsMinuteOfHour
      (\ s a -> s{_dfsMinuteOfHour = a})
      . mapping _Coerce

-- | An optional password for fetch_url.
dfsPassword :: Lens' DatafeedFetchSchedule (Maybe Text)
dfsPassword
  = lens _dfsPassword (\ s a -> s{_dfsPassword = a})

-- | The day of the month the feed file should be fetched (1-31).
dfsDayOfMonth :: Lens' DatafeedFetchSchedule (Maybe Word32)
dfsDayOfMonth
  = lens _dfsDayOfMonth
      (\ s a -> s{_dfsDayOfMonth = a})
      . mapping _Coerce

-- | The hour of the day the feed file should be fetched (0-23).
dfsHour :: Lens' DatafeedFetchSchedule (Maybe Word32)
dfsHour
  = lens _dfsHour (\ s a -> s{_dfsHour = a}) .
      mapping _Coerce

-- | The day of the week the feed file should be fetched.
dfsWeekday :: Lens' DatafeedFetchSchedule (Maybe Text)
dfsWeekday
  = lens _dfsWeekday (\ s a -> s{_dfsWeekday = a})

-- | Time zone used for schedule. UTC by default. E.g.,
-- \"America\/Los_Angeles\".
dfsTimeZone :: Lens' DatafeedFetchSchedule (Maybe Text)
dfsTimeZone
  = lens _dfsTimeZone (\ s a -> s{_dfsTimeZone = a})

-- | Whether the scheduled fetch is paused or not.
dfsPaused :: Lens' DatafeedFetchSchedule (Maybe Bool)
dfsPaused
  = lens _dfsPaused (\ s a -> s{_dfsPaused = a})

instance FromJSON DatafeedFetchSchedule where
        parseJSON
          = withObject "DatafeedFetchSchedule"
              (\ o ->
                 DatafeedFetchSchedule' <$>
                   (o .:? "fetchUrl") <*> (o .:? "username") <*>
                     (o .:? "minuteOfHour")
                     <*> (o .:? "password")
                     <*> (o .:? "dayOfMonth")
                     <*> (o .:? "hour")
                     <*> (o .:? "weekday")
                     <*> (o .:? "timeZone")
                     <*> (o .:? "paused"))

instance ToJSON DatafeedFetchSchedule where
        toJSON DatafeedFetchSchedule'{..}
          = object
              (catMaybes
                 [("fetchUrl" .=) <$> _dfsFetchURL,
                  ("username" .=) <$> _dfsUsername,
                  ("minuteOfHour" .=) <$> _dfsMinuteOfHour,
                  ("password" .=) <$> _dfsPassword,
                  ("dayOfMonth" .=) <$> _dfsDayOfMonth,
                  ("hour" .=) <$> _dfsHour,
                  ("weekday" .=) <$> _dfsWeekday,
                  ("timeZone" .=) <$> _dfsTimeZone,
                  ("paused" .=) <$> _dfsPaused])

-- | Store resource.
--
-- /See:/ 'posStore' smart constructor.
data PosStore = PosStore'
    { _pssStoreCode :: !(Maybe Text)
    , _pssKind :: !Text
    , _pssStoreAddress :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssStoreCode'
--
-- * 'pssKind'
--
-- * 'pssStoreAddress'
posStore
    :: PosStore
posStore = 
    PosStore'
    { _pssStoreCode = Nothing
    , _pssKind = "content#posStore"
    , _pssStoreAddress = Nothing
    }

-- | A store identifier that is unique for the given merchant.
pssStoreCode :: Lens' PosStore (Maybe Text)
pssStoreCode
  = lens _pssStoreCode (\ s a -> s{_pssStoreCode = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posStore\".
pssKind :: Lens' PosStore Text
pssKind = lens _pssKind (\ s a -> s{_pssKind = a})

-- | The street address of the store.
pssStoreAddress :: Lens' PosStore (Maybe Text)
pssStoreAddress
  = lens _pssStoreAddress
      (\ s a -> s{_pssStoreAddress = a})

instance FromJSON PosStore where
        parseJSON
          = withObject "PosStore"
              (\ o ->
                 PosStore' <$>
                   (o .:? "storeCode") <*>
                     (o .:? "kind" .!= "content#posStore")
                     <*> (o .:? "storeAddress"))

instance ToJSON PosStore where
        toJSON PosStore'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _pssStoreCode,
                  Just ("kind" .= _pssKind),
                  ("storeAddress" .=) <$> _pssStoreAddress])

--
-- /See:/ 'ordersCustomBatchRequest' smart constructor.
newtype OrdersCustomBatchRequest = OrdersCustomBatchRequest'
    { _ocbrEntries :: Maybe [OrdersCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrEntries'
ordersCustomBatchRequest
    :: OrdersCustomBatchRequest
ordersCustomBatchRequest = 
    OrdersCustomBatchRequest'
    { _ocbrEntries = Nothing
    }

-- | The request entries to be processed in the batch.
ocbrEntries :: Lens' OrdersCustomBatchRequest [OrdersCustomBatchRequestEntry]
ocbrEntries
  = lens _ocbrEntries (\ s a -> s{_ocbrEntries = a}) .
      _Default
      . _Coerce

instance FromJSON OrdersCustomBatchRequest where
        parseJSON
          = withObject "OrdersCustomBatchRequest"
              (\ o ->
                 OrdersCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON OrdersCustomBatchRequest where
        toJSON OrdersCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _ocbrEntries])

--
-- /See:/ 'shippingSettingsGetSupportedCarriersResponse' smart constructor.
data ShippingSettingsGetSupportedCarriersResponse = ShippingSettingsGetSupportedCarriersResponse'
    { _ssgscrKind :: !Text
    , _ssgscrCarriers :: !(Maybe [CarriersCarrier])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsGetSupportedCarriersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgscrKind'
--
-- * 'ssgscrCarriers'
shippingSettingsGetSupportedCarriersResponse
    :: ShippingSettingsGetSupportedCarriersResponse
shippingSettingsGetSupportedCarriersResponse = 
    ShippingSettingsGetSupportedCarriersResponse'
    { _ssgscrKind = "content#shippingsettingsGetSupportedCarriersResponse"
    , _ssgscrCarriers = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#shippingsettingsGetSupportedCarriersResponse\".
ssgscrKind :: Lens' ShippingSettingsGetSupportedCarriersResponse Text
ssgscrKind
  = lens _ssgscrKind (\ s a -> s{_ssgscrKind = a})

-- | A list of supported carriers. May be empty.
ssgscrCarriers :: Lens' ShippingSettingsGetSupportedCarriersResponse [CarriersCarrier]
ssgscrCarriers
  = lens _ssgscrCarriers
      (\ s a -> s{_ssgscrCarriers = a})
      . _Default
      . _Coerce

instance FromJSON
         ShippingSettingsGetSupportedCarriersResponse where
        parseJSON
          = withObject
              "ShippingSettingsGetSupportedCarriersResponse"
              (\ o ->
                 ShippingSettingsGetSupportedCarriersResponse' <$>
                   (o .:? "kind" .!=
                      "content#shippingsettingsGetSupportedCarriersResponse")
                     <*> (o .:? "carriers" .!= mempty))

instance ToJSON
         ShippingSettingsGetSupportedCarriersResponse where
        toJSON
          ShippingSettingsGetSupportedCarriersResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ssgscrKind),
                  ("carriers" .=) <$> _ssgscrCarriers])

--
-- /See:/ 'ordersCustomBatchRequestEntrySetLineItemMetadata' smart constructor.
data OrdersCustomBatchRequestEntrySetLineItemMetadata = OrdersCustomBatchRequestEntrySetLineItemMetadata'
    { _ocbreslimAnnotations :: !(Maybe [OrderMerchantProvidedAnnotation])
    , _ocbreslimLineItemId :: !(Maybe Text)
    , _ocbreslimProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntrySetLineItemMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreslimAnnotations'
--
-- * 'ocbreslimLineItemId'
--
-- * 'ocbreslimProductId'
ordersCustomBatchRequestEntrySetLineItemMetadata
    :: OrdersCustomBatchRequestEntrySetLineItemMetadata
ordersCustomBatchRequestEntrySetLineItemMetadata = 
    OrdersCustomBatchRequestEntrySetLineItemMetadata'
    { _ocbreslimAnnotations = Nothing
    , _ocbreslimLineItemId = Nothing
    , _ocbreslimProductId = Nothing
    }

ocbreslimAnnotations :: Lens' OrdersCustomBatchRequestEntrySetLineItemMetadata [OrderMerchantProvidedAnnotation]
ocbreslimAnnotations
  = lens _ocbreslimAnnotations
      (\ s a -> s{_ocbreslimAnnotations = a})
      . _Default
      . _Coerce

-- | The ID of the line item to set metadata. Either lineItemId or productId
-- is required.
ocbreslimLineItemId :: Lens' OrdersCustomBatchRequestEntrySetLineItemMetadata (Maybe Text)
ocbreslimLineItemId
  = lens _ocbreslimLineItemId
      (\ s a -> s{_ocbreslimLineItemId = a})

-- | The ID of the product to set metadata. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbreslimProductId :: Lens' OrdersCustomBatchRequestEntrySetLineItemMetadata (Maybe Text)
ocbreslimProductId
  = lens _ocbreslimProductId
      (\ s a -> s{_ocbreslimProductId = a})

instance FromJSON
         OrdersCustomBatchRequestEntrySetLineItemMetadata
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntrySetLineItemMetadata"
              (\ o ->
                 OrdersCustomBatchRequestEntrySetLineItemMetadata' <$>
                   (o .:? "annotations" .!= mempty) <*>
                     (o .:? "lineItemId")
                     <*> (o .:? "productId"))

instance ToJSON
         OrdersCustomBatchRequestEntrySetLineItemMetadata
         where
        toJSON
          OrdersCustomBatchRequestEntrySetLineItemMetadata'{..}
          = object
              (catMaybes
                 [("annotations" .=) <$> _ocbreslimAnnotations,
                  ("lineItemId" .=) <$> _ocbreslimLineItemId,
                  ("productId" .=) <$> _ocbreslimProductId])

--
-- /See:/ 'holidaysHoliday' smart constructor.
data HolidaysHoliday = HolidaysHoliday'
    { _hhDeliveryGuaranteeHour :: !(Maybe (Textual Word64))
    , _hhDate :: !(Maybe Text)
    , _hhDeliveryGuaranteeDate :: !(Maybe Text)
    , _hhCountryCode :: !(Maybe Text)
    , _hhId :: !(Maybe Text)
    , _hhType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'HolidaysHoliday' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hhDeliveryGuaranteeHour'
--
-- * 'hhDate'
--
-- * 'hhDeliveryGuaranteeDate'
--
-- * 'hhCountryCode'
--
-- * 'hhId'
--
-- * 'hhType'
holidaysHoliday
    :: HolidaysHoliday
holidaysHoliday = 
    HolidaysHoliday'
    { _hhDeliveryGuaranteeHour = Nothing
    , _hhDate = Nothing
    , _hhDeliveryGuaranteeDate = Nothing
    , _hhCountryCode = Nothing
    , _hhId = Nothing
    , _hhType = Nothing
    }

-- | Hour of the day in the delivery location\'s timezone on the guaranteed
-- delivery date by which the order has to arrive at the customer\'s.
-- Possible values are: 0 (midnight), 1, ..., 12 (noon), 13, ..., 23.
-- Always present.
hhDeliveryGuaranteeHour :: Lens' HolidaysHoliday (Maybe Word64)
hhDeliveryGuaranteeHour
  = lens _hhDeliveryGuaranteeHour
      (\ s a -> s{_hhDeliveryGuaranteeHour = a})
      . mapping _Coerce

-- | Date of the holiday, in ISO 8601 format. E.g. \"2016-12-25\" for
-- Christmas 2016. Always present.
hhDate :: Lens' HolidaysHoliday (Maybe Text)
hhDate = lens _hhDate (\ s a -> s{_hhDate = a})

-- | Date on which the order has to arrive at the customer\'s, in ISO 8601
-- format. E.g. \"2016-12-24\" for 24th December 2016. Always present.
hhDeliveryGuaranteeDate :: Lens' HolidaysHoliday (Maybe Text)
hhDeliveryGuaranteeDate
  = lens _hhDeliveryGuaranteeDate
      (\ s a -> s{_hhDeliveryGuaranteeDate = a})

-- | The CLDR territory code of the country in which the holiday is
-- available. E.g. \"US\", \"DE\", \"GB\". A holiday cutoff can only be
-- configured in a shipping settings service with matching delivery
-- country. Always present.
hhCountryCode :: Lens' HolidaysHoliday (Maybe Text)
hhCountryCode
  = lens _hhCountryCode
      (\ s a -> s{_hhCountryCode = a})

-- | Unique identifier for the holiday to be used when configuring holiday
-- cutoffs. Always present.
hhId :: Lens' HolidaysHoliday (Maybe Text)
hhId = lens _hhId (\ s a -> s{_hhId = a})

-- | The holiday type. Always present.
hhType :: Lens' HolidaysHoliday (Maybe Text)
hhType = lens _hhType (\ s a -> s{_hhType = a})

instance FromJSON HolidaysHoliday where
        parseJSON
          = withObject "HolidaysHoliday"
              (\ o ->
                 HolidaysHoliday' <$>
                   (o .:? "deliveryGuaranteeHour") <*> (o .:? "date")
                     <*> (o .:? "deliveryGuaranteeDate")
                     <*> (o .:? "countryCode")
                     <*> (o .:? "id")
                     <*> (o .:? "type"))

instance ToJSON HolidaysHoliday where
        toJSON HolidaysHoliday'{..}
          = object
              (catMaybes
                 [("deliveryGuaranteeHour" .=) <$>
                    _hhDeliveryGuaranteeHour,
                  ("date" .=) <$> _hhDate,
                  ("deliveryGuaranteeDate" .=) <$>
                    _hhDeliveryGuaranteeDate,
                  ("countryCode" .=) <$> _hhCountryCode,
                  ("id" .=) <$> _hhId, ("type" .=) <$> _hhType])

--
-- /See:/ 'accountsListResponse' smart constructor.
data AccountsListResponse = AccountsListResponse'
    { _accNextPageToken :: !(Maybe Text)
    , _accKind :: !Text
    , _accResources :: !(Maybe [Account])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accNextPageToken'
--
-- * 'accKind'
--
-- * 'accResources'
accountsListResponse
    :: AccountsListResponse
accountsListResponse = 
    AccountsListResponse'
    { _accNextPageToken = Nothing
    , _accKind = "content#accountsListResponse"
    , _accResources = Nothing
    }

-- | The token for the retrieval of the next page of accounts.
accNextPageToken :: Lens' AccountsListResponse (Maybe Text)
accNextPageToken
  = lens _accNextPageToken
      (\ s a -> s{_accNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountsListResponse\".
accKind :: Lens' AccountsListResponse Text
accKind = lens _accKind (\ s a -> s{_accKind = a})

accResources :: Lens' AccountsListResponse [Account]
accResources
  = lens _accResources (\ s a -> s{_accResources = a})
      . _Default
      . _Coerce

instance FromJSON AccountsListResponse where
        parseJSON
          = withObject "AccountsListResponse"
              (\ o ->
                 AccountsListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "content#accountsListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON AccountsListResponse where
        toJSON AccountsListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _accNextPageToken,
                  Just ("kind" .= _accKind),
                  ("resources" .=) <$> _accResources])

--
-- /See:/ 'productStatusDataQualityIssue' smart constructor.
data ProductStatusDataQualityIssue = ProductStatusDataQualityIssue'
    { _psdqiLocation :: !(Maybe Text)
    , _psdqiFetchStatus :: !(Maybe Text)
    , _psdqiSeverity :: !(Maybe Text)
    , _psdqiValueProvided :: !(Maybe Text)
    , _psdqiId :: !(Maybe Text)
    , _psdqiValueOnLandingPage :: !(Maybe Text)
    , _psdqiTimestamp :: !(Maybe Text)
    , _psdqiDetail :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductStatusDataQualityIssue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psdqiLocation'
--
-- * 'psdqiFetchStatus'
--
-- * 'psdqiSeverity'
--
-- * 'psdqiValueProvided'
--
-- * 'psdqiId'
--
-- * 'psdqiValueOnLandingPage'
--
-- * 'psdqiTimestamp'
--
-- * 'psdqiDetail'
productStatusDataQualityIssue
    :: ProductStatusDataQualityIssue
productStatusDataQualityIssue = 
    ProductStatusDataQualityIssue'
    { _psdqiLocation = Nothing
    , _psdqiFetchStatus = Nothing
    , _psdqiSeverity = Nothing
    , _psdqiValueProvided = Nothing
    , _psdqiId = Nothing
    , _psdqiValueOnLandingPage = Nothing
    , _psdqiTimestamp = Nothing
    , _psdqiDetail = Nothing
    }

-- | The attribute name that is relevant for the issue.
psdqiLocation :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiLocation
  = lens _psdqiLocation
      (\ s a -> s{_psdqiLocation = a})

-- | The fetch status for landing_page_errors.
psdqiFetchStatus :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiFetchStatus
  = lens _psdqiFetchStatus
      (\ s a -> s{_psdqiFetchStatus = a})

-- | The severity of the data quality issue.
psdqiSeverity :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiSeverity
  = lens _psdqiSeverity
      (\ s a -> s{_psdqiSeverity = a})

-- | The value the attribute had at time of evaluation.
psdqiValueProvided :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiValueProvided
  = lens _psdqiValueProvided
      (\ s a -> s{_psdqiValueProvided = a})

-- | The id of the data quality issue.
psdqiId :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiId = lens _psdqiId (\ s a -> s{_psdqiId = a})

-- | The value of that attribute that was found on the landing page
psdqiValueOnLandingPage :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiValueOnLandingPage
  = lens _psdqiValueOnLandingPage
      (\ s a -> s{_psdqiValueOnLandingPage = a})

-- | The time stamp of the data quality issue.
psdqiTimestamp :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiTimestamp
  = lens _psdqiTimestamp
      (\ s a -> s{_psdqiTimestamp = a})

-- | A more detailed error string.
psdqiDetail :: Lens' ProductStatusDataQualityIssue (Maybe Text)
psdqiDetail
  = lens _psdqiDetail (\ s a -> s{_psdqiDetail = a})

instance FromJSON ProductStatusDataQualityIssue where
        parseJSON
          = withObject "ProductStatusDataQualityIssue"
              (\ o ->
                 ProductStatusDataQualityIssue' <$>
                   (o .:? "location") <*> (o .:? "fetchStatus") <*>
                     (o .:? "severity")
                     <*> (o .:? "valueProvided")
                     <*> (o .:? "id")
                     <*> (o .:? "valueOnLandingPage")
                     <*> (o .:? "timestamp")
                     <*> (o .:? "detail"))

instance ToJSON ProductStatusDataQualityIssue where
        toJSON ProductStatusDataQualityIssue'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _psdqiLocation,
                  ("fetchStatus" .=) <$> _psdqiFetchStatus,
                  ("severity" .=) <$> _psdqiSeverity,
                  ("valueProvided" .=) <$> _psdqiValueProvided,
                  ("id" .=) <$> _psdqiId,
                  ("valueOnLandingPage" .=) <$>
                    _psdqiValueOnLandingPage,
                  ("timestamp" .=) <$> _psdqiTimestamp,
                  ("detail" .=) <$> _psdqiDetail])

--
-- /See:/ 'carriersCarrier' smart constructor.
data CarriersCarrier = CarriersCarrier'
    { _ccCountry :: !(Maybe Text)
    , _ccName :: !(Maybe Text)
    , _ccServices :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CarriersCarrier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCountry'
--
-- * 'ccName'
--
-- * 'ccServices'
carriersCarrier
    :: CarriersCarrier
carriersCarrier = 
    CarriersCarrier'
    { _ccCountry = Nothing
    , _ccName = Nothing
    , _ccServices = Nothing
    }

-- | The CLDR country code of the carrier (e.g., \"US\"). Always present.
ccCountry :: Lens' CarriersCarrier (Maybe Text)
ccCountry
  = lens _ccCountry (\ s a -> s{_ccCountry = a})

-- | The name of the carrier (e.g., \"UPS\"). Always present.
ccName :: Lens' CarriersCarrier (Maybe Text)
ccName = lens _ccName (\ s a -> s{_ccName = a})

-- | A list of supported services (e.g., \"ground\") for that carrier.
-- Contains at least one service.
ccServices :: Lens' CarriersCarrier [Text]
ccServices
  = lens _ccServices (\ s a -> s{_ccServices = a}) .
      _Default
      . _Coerce

instance FromJSON CarriersCarrier where
        parseJSON
          = withObject "CarriersCarrier"
              (\ o ->
                 CarriersCarrier' <$>
                   (o .:? "country") <*> (o .:? "name") <*>
                     (o .:? "services" .!= mempty))

instance ToJSON CarriersCarrier where
        toJSON CarriersCarrier'{..}
          = object
              (catMaybes
                 [("country" .=) <$> _ccCountry,
                  ("name" .=) <$> _ccName,
                  ("services" .=) <$> _ccServices])

--
-- /See:/ 'carrierRate' smart constructor.
data CarrierRate = CarrierRate'
    { _crOriginPostalCode :: !(Maybe Text)
    , _crFlatAdjustment :: !(Maybe Price)
    , _crCarrierService :: !(Maybe Text)
    , _crName :: !(Maybe Text)
    , _crPercentageAdjustment :: !(Maybe Text)
    , _crCarrierName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CarrierRate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crOriginPostalCode'
--
-- * 'crFlatAdjustment'
--
-- * 'crCarrierService'
--
-- * 'crName'
--
-- * 'crPercentageAdjustment'
--
-- * 'crCarrierName'
carrierRate
    :: CarrierRate
carrierRate = 
    CarrierRate'
    { _crOriginPostalCode = Nothing
    , _crFlatAdjustment = Nothing
    , _crCarrierService = Nothing
    , _crName = Nothing
    , _crPercentageAdjustment = Nothing
    , _crCarrierName = Nothing
    }

-- | Shipping origin for this carrier rate. Required.
crOriginPostalCode :: Lens' CarrierRate (Maybe Text)
crOriginPostalCode
  = lens _crOriginPostalCode
      (\ s a -> s{_crOriginPostalCode = a})

-- | Additive shipping rate modifier. Can be negative. For example {
-- \"value\": \"1\", \"currency\" : \"USD\" } adds $1 to the rate, {
-- \"value\": \"-3\", \"currency\" : \"USD\" } removes $3 from the rate.
-- Optional.
crFlatAdjustment :: Lens' CarrierRate (Maybe Price)
crFlatAdjustment
  = lens _crFlatAdjustment
      (\ s a -> s{_crFlatAdjustment = a})

-- | Carrier service, such as \"ground\" or \"2 days\". The list of supported
-- services for a carrier can be retrieved via the getSupportedCarriers
-- method. Required.
crCarrierService :: Lens' CarrierRate (Maybe Text)
crCarrierService
  = lens _crCarrierService
      (\ s a -> s{_crCarrierService = a})

-- | Name of the carrier rate. Must be unique per rate group. Required.
crName :: Lens' CarrierRate (Maybe Text)
crName = lens _crName (\ s a -> s{_crName = a})

-- | Multiplicative shipping rate modifier as a number in decimal notation.
-- Can be negative. For example \"5.4\" increases the rate by 5.4%, \"-3\"
-- decreases the rate by 3%. Optional.
crPercentageAdjustment :: Lens' CarrierRate (Maybe Text)
crPercentageAdjustment
  = lens _crPercentageAdjustment
      (\ s a -> s{_crPercentageAdjustment = a})

-- | Carrier service, such as \"UPS\" or \"Fedex\". The list of supported
-- carriers can be retrieved via the getSupportedCarriers method. Required.
crCarrierName :: Lens' CarrierRate (Maybe Text)
crCarrierName
  = lens _crCarrierName
      (\ s a -> s{_crCarrierName = a})

instance FromJSON CarrierRate where
        parseJSON
          = withObject "CarrierRate"
              (\ o ->
                 CarrierRate' <$>
                   (o .:? "originPostalCode") <*>
                     (o .:? "flatAdjustment")
                     <*> (o .:? "carrierService")
                     <*> (o .:? "name")
                     <*> (o .:? "percentageAdjustment")
                     <*> (o .:? "carrierName"))

instance ToJSON CarrierRate where
        toJSON CarrierRate'{..}
          = object
              (catMaybes
                 [("originPostalCode" .=) <$> _crOriginPostalCode,
                  ("flatAdjustment" .=) <$> _crFlatAdjustment,
                  ("carrierService" .=) <$> _crCarrierService,
                  ("name" .=) <$> _crName,
                  ("percentageAdjustment" .=) <$>
                    _crPercentageAdjustment,
                  ("carrierName" .=) <$> _crCarrierName])

--
-- /See:/ 'shippingSettingsListResponse' smart constructor.
data ShippingSettingsListResponse = ShippingSettingsListResponse'
    { _sslrNextPageToken :: !(Maybe Text)
    , _sslrKind :: !Text
    , _sslrResources :: !(Maybe [ShippingSettings])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sslrNextPageToken'
--
-- * 'sslrKind'
--
-- * 'sslrResources'
shippingSettingsListResponse
    :: ShippingSettingsListResponse
shippingSettingsListResponse = 
    ShippingSettingsListResponse'
    { _sslrNextPageToken = Nothing
    , _sslrKind = "content#shippingsettingsListResponse"
    , _sslrResources = Nothing
    }

-- | The token for the retrieval of the next page of shipping settings.
sslrNextPageToken :: Lens' ShippingSettingsListResponse (Maybe Text)
sslrNextPageToken
  = lens _sslrNextPageToken
      (\ s a -> s{_sslrNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#shippingsettingsListResponse\".
sslrKind :: Lens' ShippingSettingsListResponse Text
sslrKind = lens _sslrKind (\ s a -> s{_sslrKind = a})

sslrResources :: Lens' ShippingSettingsListResponse [ShippingSettings]
sslrResources
  = lens _sslrResources
      (\ s a -> s{_sslrResources = a})
      . _Default
      . _Coerce

instance FromJSON ShippingSettingsListResponse where
        parseJSON
          = withObject "ShippingSettingsListResponse"
              (\ o ->
                 ShippingSettingsListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!=
                        "content#shippingsettingsListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON ShippingSettingsListResponse where
        toJSON ShippingSettingsListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _sslrNextPageToken,
                  Just ("kind" .= _sslrKind),
                  ("resources" .=) <$> _sslrResources])

--
-- /See:/ 'ordersShipLineItemsRequest' smart constructor.
data OrdersShipLineItemsRequest = OrdersShipLineItemsRequest'
    { _oslirCarrier :: !(Maybe Text)
    , _oslirTrackingId :: !(Maybe Text)
    , _oslirShipmentId :: !(Maybe Text)
    , _oslirShipmentInfos :: !(Maybe [OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo])
    , _oslirLineItems :: !(Maybe [OrderShipmentLineItemShipment])
    , _oslirOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersShipLineItemsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslirCarrier'
--
-- * 'oslirTrackingId'
--
-- * 'oslirShipmentId'
--
-- * 'oslirShipmentInfos'
--
-- * 'oslirLineItems'
--
-- * 'oslirOperationId'
ordersShipLineItemsRequest
    :: OrdersShipLineItemsRequest
ordersShipLineItemsRequest = 
    OrdersShipLineItemsRequest'
    { _oslirCarrier = Nothing
    , _oslirTrackingId = Nothing
    , _oslirShipmentId = Nothing
    , _oslirShipmentInfos = Nothing
    , _oslirLineItems = Nothing
    , _oslirOperationId = Nothing
    }

-- | Deprecated. Please use shipmentInfo instead. The carrier handling the
-- shipment. See shipments[].carrier in the Orders resource representation
-- for a list of acceptable values.
oslirCarrier :: Lens' OrdersShipLineItemsRequest (Maybe Text)
oslirCarrier
  = lens _oslirCarrier (\ s a -> s{_oslirCarrier = a})

-- | Deprecated. Please use shipmentInfo instead. The tracking id for the
-- shipment.
oslirTrackingId :: Lens' OrdersShipLineItemsRequest (Maybe Text)
oslirTrackingId
  = lens _oslirTrackingId
      (\ s a -> s{_oslirTrackingId = a})

-- | Deprecated. Please use shipmentInfo instead. The ID of the shipment.
oslirShipmentId :: Lens' OrdersShipLineItemsRequest (Maybe Text)
oslirShipmentId
  = lens _oslirShipmentId
      (\ s a -> s{_oslirShipmentId = a})

-- | Shipment information. This field is repeated because a single line item
-- can be shipped in several packages (and have several tracking IDs).
oslirShipmentInfos :: Lens' OrdersShipLineItemsRequest [OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo]
oslirShipmentInfos
  = lens _oslirShipmentInfos
      (\ s a -> s{_oslirShipmentInfos = a})
      . _Default
      . _Coerce

-- | Line items to ship.
oslirLineItems :: Lens' OrdersShipLineItemsRequest [OrderShipmentLineItemShipment]
oslirLineItems
  = lens _oslirLineItems
      (\ s a -> s{_oslirLineItems = a})
      . _Default
      . _Coerce

-- | The ID of the operation. Unique across all operations for a given order.
oslirOperationId :: Lens' OrdersShipLineItemsRequest (Maybe Text)
oslirOperationId
  = lens _oslirOperationId
      (\ s a -> s{_oslirOperationId = a})

instance FromJSON OrdersShipLineItemsRequest where
        parseJSON
          = withObject "OrdersShipLineItemsRequest"
              (\ o ->
                 OrdersShipLineItemsRequest' <$>
                   (o .:? "carrier") <*> (o .:? "trackingId") <*>
                     (o .:? "shipmentId")
                     <*> (o .:? "shipmentInfos" .!= mempty)
                     <*> (o .:? "lineItems" .!= mempty)
                     <*> (o .:? "operationId"))

instance ToJSON OrdersShipLineItemsRequest where
        toJSON OrdersShipLineItemsRequest'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _oslirCarrier,
                  ("trackingId" .=) <$> _oslirTrackingId,
                  ("shipmentId" .=) <$> _oslirShipmentId,
                  ("shipmentInfos" .=) <$> _oslirShipmentInfos,
                  ("lineItems" .=) <$> _oslirLineItems,
                  ("operationId" .=) <$> _oslirOperationId])

--
-- /See:/ 'accountsCustomBatchResponse' smart constructor.
data AccountsCustomBatchResponse = AccountsCustomBatchResponse'
    { _acbrcEntries :: !(Maybe [AccountsCustomBatchResponseEntry])
    , _acbrcKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbrcEntries'
--
-- * 'acbrcKind'
accountsCustomBatchResponse
    :: AccountsCustomBatchResponse
accountsCustomBatchResponse = 
    AccountsCustomBatchResponse'
    { _acbrcEntries = Nothing
    , _acbrcKind = "content#accountsCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
acbrcEntries :: Lens' AccountsCustomBatchResponse [AccountsCustomBatchResponseEntry]
acbrcEntries
  = lens _acbrcEntries (\ s a -> s{_acbrcEntries = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountsCustomBatchResponse\".
acbrcKind :: Lens' AccountsCustomBatchResponse Text
acbrcKind
  = lens _acbrcKind (\ s a -> s{_acbrcKind = a})

instance FromJSON AccountsCustomBatchResponse where
        parseJSON
          = withObject "AccountsCustomBatchResponse"
              (\ o ->
                 AccountsCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#accountsCustomBatchResponse"))

instance ToJSON AccountsCustomBatchResponse where
        toJSON AccountsCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _acbrcEntries,
                  Just ("kind" .= _acbrcKind)])

--
-- /See:/ 'ordersUpdateLineItemShippingDetailsRequest' smart constructor.
data OrdersUpdateLineItemShippingDetailsRequest = OrdersUpdateLineItemShippingDetailsRequest'
    { _oulisdrShipByDate :: !(Maybe Text)
    , _oulisdrLineItemId :: !(Maybe Text)
    , _oulisdrDeliverByDate :: !(Maybe Text)
    , _oulisdrOperationId :: !(Maybe Text)
    , _oulisdrProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateLineItemShippingDetailsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oulisdrShipByDate'
--
-- * 'oulisdrLineItemId'
--
-- * 'oulisdrDeliverByDate'
--
-- * 'oulisdrOperationId'
--
-- * 'oulisdrProductId'
ordersUpdateLineItemShippingDetailsRequest
    :: OrdersUpdateLineItemShippingDetailsRequest
ordersUpdateLineItemShippingDetailsRequest = 
    OrdersUpdateLineItemShippingDetailsRequest'
    { _oulisdrShipByDate = Nothing
    , _oulisdrLineItemId = Nothing
    , _oulisdrDeliverByDate = Nothing
    , _oulisdrOperationId = Nothing
    , _oulisdrProductId = Nothing
    }

-- | Updated ship by date, in ISO 8601 format. If not specified only deliver
-- by date is updated.
oulisdrShipByDate :: Lens' OrdersUpdateLineItemShippingDetailsRequest (Maybe Text)
oulisdrShipByDate
  = lens _oulisdrShipByDate
      (\ s a -> s{_oulisdrShipByDate = a})

-- | The ID of the line item to set metadata. Either lineItemId or productId
-- is required.
oulisdrLineItemId :: Lens' OrdersUpdateLineItemShippingDetailsRequest (Maybe Text)
oulisdrLineItemId
  = lens _oulisdrLineItemId
      (\ s a -> s{_oulisdrLineItemId = a})

-- | Updated delivery by date, in ISO 8601 format. If not specified only ship
-- by date is updated.
oulisdrDeliverByDate :: Lens' OrdersUpdateLineItemShippingDetailsRequest (Maybe Text)
oulisdrDeliverByDate
  = lens _oulisdrDeliverByDate
      (\ s a -> s{_oulisdrDeliverByDate = a})

-- | The ID of the operation. Unique across all operations for a given order.
oulisdrOperationId :: Lens' OrdersUpdateLineItemShippingDetailsRequest (Maybe Text)
oulisdrOperationId
  = lens _oulisdrOperationId
      (\ s a -> s{_oulisdrOperationId = a})

-- | The ID of the product to set metadata. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
oulisdrProductId :: Lens' OrdersUpdateLineItemShippingDetailsRequest (Maybe Text)
oulisdrProductId
  = lens _oulisdrProductId
      (\ s a -> s{_oulisdrProductId = a})

instance FromJSON
         OrdersUpdateLineItemShippingDetailsRequest where
        parseJSON
          = withObject
              "OrdersUpdateLineItemShippingDetailsRequest"
              (\ o ->
                 OrdersUpdateLineItemShippingDetailsRequest' <$>
                   (o .:? "shipByDate") <*> (o .:? "lineItemId") <*>
                     (o .:? "deliverByDate")
                     <*> (o .:? "operationId")
                     <*> (o .:? "productId"))

instance ToJSON
         OrdersUpdateLineItemShippingDetailsRequest where
        toJSON
          OrdersUpdateLineItemShippingDetailsRequest'{..}
          = object
              (catMaybes
                 [("shipByDate" .=) <$> _oulisdrShipByDate,
                  ("lineItemId" .=) <$> _oulisdrLineItemId,
                  ("deliverByDate" .=) <$> _oulisdrDeliverByDate,
                  ("operationId" .=) <$> _oulisdrOperationId,
                  ("productId" .=) <$> _oulisdrProductId])

--
-- /See:/ 'productTax' smart constructor.
data ProductTax = ProductTax'
    { _ptTaxShip :: !(Maybe Bool)
    , _ptCountry :: !(Maybe Text)
    , _ptPostalCode :: !(Maybe Text)
    , _ptRate :: !(Maybe (Textual Double))
    , _ptRegion :: !(Maybe Text)
    , _ptLocationId :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductTax' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptTaxShip'
--
-- * 'ptCountry'
--
-- * 'ptPostalCode'
--
-- * 'ptRate'
--
-- * 'ptRegion'
--
-- * 'ptLocationId'
productTax
    :: ProductTax
productTax = 
    ProductTax'
    { _ptTaxShip = Nothing
    , _ptCountry = Nothing
    , _ptPostalCode = Nothing
    , _ptRate = Nothing
    , _ptRegion = Nothing
    , _ptLocationId = Nothing
    }

-- | Set to true if tax is charged on shipping.
ptTaxShip :: Lens' ProductTax (Maybe Bool)
ptTaxShip
  = lens _ptTaxShip (\ s a -> s{_ptTaxShip = a})

-- | The country within which the item is taxed, specified as a CLDR
-- territory code.
ptCountry :: Lens' ProductTax (Maybe Text)
ptCountry
  = lens _ptCountry (\ s a -> s{_ptCountry = a})

-- | The postal code range that the tax rate applies to, represented by a ZIP
-- code, a ZIP code prefix using * wildcard, a range between two ZIP codes
-- or two ZIP code prefixes of equal length. Examples: 94114, 94*,
-- 94002-95460, 94*-95*.
ptPostalCode :: Lens' ProductTax (Maybe Text)
ptPostalCode
  = lens _ptPostalCode (\ s a -> s{_ptPostalCode = a})

-- | The percentage of tax rate that applies to the item price.
ptRate :: Lens' ProductTax (Maybe Double)
ptRate
  = lens _ptRate (\ s a -> s{_ptRate = a}) .
      mapping _Coerce

-- | The geographic region to which the tax rate applies.
ptRegion :: Lens' ProductTax (Maybe Text)
ptRegion = lens _ptRegion (\ s a -> s{_ptRegion = a})

-- | The numeric id of a location that the tax rate applies to as defined in
-- the AdWords API.
ptLocationId :: Lens' ProductTax (Maybe Int64)
ptLocationId
  = lens _ptLocationId (\ s a -> s{_ptLocationId = a})
      . mapping _Coerce

instance FromJSON ProductTax where
        parseJSON
          = withObject "ProductTax"
              (\ o ->
                 ProductTax' <$>
                   (o .:? "taxShip") <*> (o .:? "country") <*>
                     (o .:? "postalCode")
                     <*> (o .:? "rate")
                     <*> (o .:? "region")
                     <*> (o .:? "locationId"))

instance ToJSON ProductTax where
        toJSON ProductTax'{..}
          = object
              (catMaybes
                 [("taxShip" .=) <$> _ptTaxShip,
                  ("country" .=) <$> _ptCountry,
                  ("postalCode" .=) <$> _ptPostalCode,
                  ("rate" .=) <$> _ptRate, ("region" .=) <$> _ptRegion,
                  ("locationId" .=) <$> _ptLocationId])

--
-- /See:/ 'posCustomBatchRequestEntry' smart constructor.
data PosCustomBatchRequestEntry = PosCustomBatchRequestEntry'
    { _pcbreMerchantId :: !(Maybe (Textual Word64))
    , _pcbreStoreCode :: !(Maybe Text)
    , _pcbreTargetMerchantId :: !(Maybe (Textual Word64))
    , _pcbreMethod :: !(Maybe Text)
    , _pcbreStore :: !(Maybe PosStore)
    , _pcbreInventory :: !(Maybe PosInventory)
    , _pcbreSale :: !(Maybe PosSale)
    , _pcbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbreMerchantId'
--
-- * 'pcbreStoreCode'
--
-- * 'pcbreTargetMerchantId'
--
-- * 'pcbreMethod'
--
-- * 'pcbreStore'
--
-- * 'pcbreInventory'
--
-- * 'pcbreSale'
--
-- * 'pcbreBatchId'
posCustomBatchRequestEntry
    :: PosCustomBatchRequestEntry
posCustomBatchRequestEntry = 
    PosCustomBatchRequestEntry'
    { _pcbreMerchantId = Nothing
    , _pcbreStoreCode = Nothing
    , _pcbreTargetMerchantId = Nothing
    , _pcbreMethod = Nothing
    , _pcbreStore = Nothing
    , _pcbreInventory = Nothing
    , _pcbreSale = Nothing
    , _pcbreBatchId = Nothing
    }

-- | The ID of the POS provider.
pcbreMerchantId :: Lens' PosCustomBatchRequestEntry (Maybe Word64)
pcbreMerchantId
  = lens _pcbreMerchantId
      (\ s a -> s{_pcbreMerchantId = a})
      . mapping _Coerce

-- | The store code. Required only to get\/submit store information.
pcbreStoreCode :: Lens' PosCustomBatchRequestEntry (Maybe Text)
pcbreStoreCode
  = lens _pcbreStoreCode
      (\ s a -> s{_pcbreStoreCode = a})

-- | The ID of the account for which to get\/submit data.
pcbreTargetMerchantId :: Lens' PosCustomBatchRequestEntry (Maybe Word64)
pcbreTargetMerchantId
  = lens _pcbreTargetMerchantId
      (\ s a -> s{_pcbreTargetMerchantId = a})
      . mapping _Coerce

pcbreMethod :: Lens' PosCustomBatchRequestEntry (Maybe Text)
pcbreMethod
  = lens _pcbreMethod (\ s a -> s{_pcbreMethod = a})

-- | The store information to submit. Set this only if the method is insert.
pcbreStore :: Lens' PosCustomBatchRequestEntry (Maybe PosStore)
pcbreStore
  = lens _pcbreStore (\ s a -> s{_pcbreStore = a})

-- | The inventory to submit. Set this only if the method is inventory.
pcbreInventory :: Lens' PosCustomBatchRequestEntry (Maybe PosInventory)
pcbreInventory
  = lens _pcbreInventory
      (\ s a -> s{_pcbreInventory = a})

-- | The sale information to submit. Set this only if the method is sale.
pcbreSale :: Lens' PosCustomBatchRequestEntry (Maybe PosSale)
pcbreSale
  = lens _pcbreSale (\ s a -> s{_pcbreSale = a})

-- | An entry ID, unique within the batch request.
pcbreBatchId :: Lens' PosCustomBatchRequestEntry (Maybe Word32)
pcbreBatchId
  = lens _pcbreBatchId (\ s a -> s{_pcbreBatchId = a})
      . mapping _Coerce

instance FromJSON PosCustomBatchRequestEntry where
        parseJSON
          = withObject "PosCustomBatchRequestEntry"
              (\ o ->
                 PosCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "storeCode") <*>
                     (o .:? "targetMerchantId")
                     <*> (o .:? "method")
                     <*> (o .:? "store")
                     <*> (o .:? "inventory")
                     <*> (o .:? "sale")
                     <*> (o .:? "batchId"))

instance ToJSON PosCustomBatchRequestEntry where
        toJSON PosCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _pcbreMerchantId,
                  ("storeCode" .=) <$> _pcbreStoreCode,
                  ("targetMerchantId" .=) <$> _pcbreTargetMerchantId,
                  ("method" .=) <$> _pcbreMethod,
                  ("store" .=) <$> _pcbreStore,
                  ("inventory" .=) <$> _pcbreInventory,
                  ("sale" .=) <$> _pcbreSale,
                  ("batchId" .=) <$> _pcbreBatchId])

--
-- /See:/ 'orderShipment' smart constructor.
data OrderShipment = OrderShipment'
    { _osCarrier :: !(Maybe Text)
    , _osStatus :: !(Maybe Text)
    , _osTrackingId :: !(Maybe Text)
    , _osLineItems :: !(Maybe [OrderShipmentLineItemShipment])
    , _osId :: !(Maybe Text)
    , _osCreationDate :: !(Maybe Text)
    , _osDeliveryDate :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderShipment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osCarrier'
--
-- * 'osStatus'
--
-- * 'osTrackingId'
--
-- * 'osLineItems'
--
-- * 'osId'
--
-- * 'osCreationDate'
--
-- * 'osDeliveryDate'
orderShipment
    :: OrderShipment
orderShipment = 
    OrderShipment'
    { _osCarrier = Nothing
    , _osStatus = Nothing
    , _osTrackingId = Nothing
    , _osLineItems = Nothing
    , _osId = Nothing
    , _osCreationDate = Nothing
    , _osDeliveryDate = Nothing
    }

-- | The carrier handling the shipment. Acceptable values are: - \"gsx\" -
-- \"ups\" - \"usps\" - \"fedex\" - \"dhl\" - \"ecourier\" - \"cxt\" -
-- \"google\" - \"ontrac\" - \"emsy\" - \"ont\" - \"deliv\" - \"dynamex\" -
-- \"lasership\" - \"mpx\" - \"uds\"
osCarrier :: Lens' OrderShipment (Maybe Text)
osCarrier
  = lens _osCarrier (\ s a -> s{_osCarrier = a})

-- | The status of the shipment.
osStatus :: Lens' OrderShipment (Maybe Text)
osStatus = lens _osStatus (\ s a -> s{_osStatus = a})

-- | The tracking id for the shipment.
osTrackingId :: Lens' OrderShipment (Maybe Text)
osTrackingId
  = lens _osTrackingId (\ s a -> s{_osTrackingId = a})

-- | The line items that are shipped.
osLineItems :: Lens' OrderShipment [OrderShipmentLineItemShipment]
osLineItems
  = lens _osLineItems (\ s a -> s{_osLineItems = a}) .
      _Default
      . _Coerce

-- | The id of the shipment.
osId :: Lens' OrderShipment (Maybe Text)
osId = lens _osId (\ s a -> s{_osId = a})

-- | Date on which the shipment has been created, in ISO 8601 format.
osCreationDate :: Lens' OrderShipment (Maybe Text)
osCreationDate
  = lens _osCreationDate
      (\ s a -> s{_osCreationDate = a})

-- | Date on which the shipment has been delivered, in ISO 8601 format.
-- Present only if status is delievered
osDeliveryDate :: Lens' OrderShipment (Maybe Text)
osDeliveryDate
  = lens _osDeliveryDate
      (\ s a -> s{_osDeliveryDate = a})

instance FromJSON OrderShipment where
        parseJSON
          = withObject "OrderShipment"
              (\ o ->
                 OrderShipment' <$>
                   (o .:? "carrier") <*> (o .:? "status") <*>
                     (o .:? "trackingId")
                     <*> (o .:? "lineItems" .!= mempty)
                     <*> (o .:? "id")
                     <*> (o .:? "creationDate")
                     <*> (o .:? "deliveryDate"))

instance ToJSON OrderShipment where
        toJSON OrderShipment'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _osCarrier,
                  ("status" .=) <$> _osStatus,
                  ("trackingId" .=) <$> _osTrackingId,
                  ("lineItems" .=) <$> _osLineItems,
                  ("id" .=) <$> _osId,
                  ("creationDate" .=) <$> _osCreationDate,
                  ("deliveryDate" .=) <$> _osDeliveryDate])

--
-- /See:/ 'orderLineItemReturnInfo' smart constructor.
data OrderLineItemReturnInfo = OrderLineItemReturnInfo'
    { _oliriIsReturnable :: !(Maybe Bool)
    , _oliriPolicyURL :: !(Maybe Text)
    , _oliriDaysToReturn :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItemReturnInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oliriIsReturnable'
--
-- * 'oliriPolicyURL'
--
-- * 'oliriDaysToReturn'
orderLineItemReturnInfo
    :: OrderLineItemReturnInfo
orderLineItemReturnInfo = 
    OrderLineItemReturnInfo'
    { _oliriIsReturnable = Nothing
    , _oliriPolicyURL = Nothing
    , _oliriDaysToReturn = Nothing
    }

-- | Whether the item is returnable.
oliriIsReturnable :: Lens' OrderLineItemReturnInfo (Maybe Bool)
oliriIsReturnable
  = lens _oliriIsReturnable
      (\ s a -> s{_oliriIsReturnable = a})

-- | URL of the item return policy.
oliriPolicyURL :: Lens' OrderLineItemReturnInfo (Maybe Text)
oliriPolicyURL
  = lens _oliriPolicyURL
      (\ s a -> s{_oliriPolicyURL = a})

-- | How many days later the item can be returned.
oliriDaysToReturn :: Lens' OrderLineItemReturnInfo (Maybe Int32)
oliriDaysToReturn
  = lens _oliriDaysToReturn
      (\ s a -> s{_oliriDaysToReturn = a})
      . mapping _Coerce

instance FromJSON OrderLineItemReturnInfo where
        parseJSON
          = withObject "OrderLineItemReturnInfo"
              (\ o ->
                 OrderLineItemReturnInfo' <$>
                   (o .:? "isReturnable") <*> (o .:? "policyUrl") <*>
                     (o .:? "daysToReturn"))

instance ToJSON OrderLineItemReturnInfo where
        toJSON OrderLineItemReturnInfo'{..}
          = object
              (catMaybes
                 [("isReturnable" .=) <$> _oliriIsReturnable,
                  ("policyUrl" .=) <$> _oliriPolicyURL,
                  ("daysToReturn" .=) <$> _oliriDaysToReturn])

-- | Account data.
--
-- /See:/ 'account' smart constructor.
data Account = Account'
    { _aaUsers :: !(Maybe [AccountUser])
    , _aaYouTubeChannelLinks :: !(Maybe [AccountYouTubeChannelLink])
    , _aaKind :: !Text
    , _aaSellerId :: !(Maybe Text)
    , _aaName :: !(Maybe Text)
    , _aaReviewsURL :: !(Maybe Text)
    , _aaId :: !(Maybe (Textual Word64))
    , _aaWebsiteURL :: !(Maybe Text)
    , _aaAdwordsLinks :: !(Maybe [AccountAdwordsLink])
    , _aaGoogleMyBusinessLink :: !(Maybe AccountGoogleMyBusinessLink)
    , _aaAdultContent :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaUsers'
--
-- * 'aaYouTubeChannelLinks'
--
-- * 'aaKind'
--
-- * 'aaSellerId'
--
-- * 'aaName'
--
-- * 'aaReviewsURL'
--
-- * 'aaId'
--
-- * 'aaWebsiteURL'
--
-- * 'aaAdwordsLinks'
--
-- * 'aaGoogleMyBusinessLink'
--
-- * 'aaAdultContent'
account
    :: Account
account = 
    Account'
    { _aaUsers = Nothing
    , _aaYouTubeChannelLinks = Nothing
    , _aaKind = "content#account"
    , _aaSellerId = Nothing
    , _aaName = Nothing
    , _aaReviewsURL = Nothing
    , _aaId = Nothing
    , _aaWebsiteURL = Nothing
    , _aaAdwordsLinks = Nothing
    , _aaGoogleMyBusinessLink = Nothing
    , _aaAdultContent = Nothing
    }

-- | Users with access to the account. Every account (except for subaccounts)
-- must have at least one admin user.
aaUsers :: Lens' Account [AccountUser]
aaUsers
  = lens _aaUsers (\ s a -> s{_aaUsers = a}) . _Default
      . _Coerce

-- | List of linked YouTube channels that are active or pending approval. To
-- create a new link request, add a new link with status active to the
-- list. It will remain in a pending state until approved or rejected in
-- the YT Creator Studio interface. To delete an active link, or to cancel
-- a link request, remove it from the list.
aaYouTubeChannelLinks :: Lens' Account [AccountYouTubeChannelLink]
aaYouTubeChannelLinks
  = lens _aaYouTubeChannelLinks
      (\ s a -> s{_aaYouTubeChannelLinks = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#account\".
aaKind :: Lens' Account Text
aaKind = lens _aaKind (\ s a -> s{_aaKind = a})

-- | Client-specific, locally-unique, internal ID for the child account.
aaSellerId :: Lens' Account (Maybe Text)
aaSellerId
  = lens _aaSellerId (\ s a -> s{_aaSellerId = a})

-- | Display name for the account.
aaName :: Lens' Account (Maybe Text)
aaName = lens _aaName (\ s a -> s{_aaName = a})

-- | [DEPRECATED] This field is never returned and will be ignored if
-- provided.
aaReviewsURL :: Lens' Account (Maybe Text)
aaReviewsURL
  = lens _aaReviewsURL (\ s a -> s{_aaReviewsURL = a})

-- | Merchant Center account ID.
aaId :: Lens' Account (Maybe Word64)
aaId
  = lens _aaId (\ s a -> s{_aaId = a}) .
      mapping _Coerce

-- | The merchant\'s website.
aaWebsiteURL :: Lens' Account (Maybe Text)
aaWebsiteURL
  = lens _aaWebsiteURL (\ s a -> s{_aaWebsiteURL = a})

-- | List of linked AdWords accounts that are active or pending approval. To
-- create a new link request, add a new link with status active to the
-- list. It will remain in a pending state until approved or rejected
-- either in the AdWords interface or through the AdWords API. To delete an
-- active link, or to cancel a link request, remove it from the list.
aaAdwordsLinks :: Lens' Account [AccountAdwordsLink]
aaAdwordsLinks
  = lens _aaAdwordsLinks
      (\ s a -> s{_aaAdwordsLinks = a})
      . _Default
      . _Coerce

-- | The GMB account which is linked or in the process of being linked with
-- the Merchant Center accounnt.
aaGoogleMyBusinessLink :: Lens' Account (Maybe AccountGoogleMyBusinessLink)
aaGoogleMyBusinessLink
  = lens _aaGoogleMyBusinessLink
      (\ s a -> s{_aaGoogleMyBusinessLink = a})

-- | Indicates whether the merchant sells adult content.
aaAdultContent :: Lens' Account (Maybe Bool)
aaAdultContent
  = lens _aaAdultContent
      (\ s a -> s{_aaAdultContent = a})

instance FromJSON Account where
        parseJSON
          = withObject "Account"
              (\ o ->
                 Account' <$>
                   (o .:? "users" .!= mempty) <*>
                     (o .:? "youtubeChannelLinks" .!= mempty)
                     <*> (o .:? "kind" .!= "content#account")
                     <*> (o .:? "sellerId")
                     <*> (o .:? "name")
                     <*> (o .:? "reviewsUrl")
                     <*> (o .:? "id")
                     <*> (o .:? "websiteUrl")
                     <*> (o .:? "adwordsLinks" .!= mempty)
                     <*> (o .:? "googleMyBusinessLink")
                     <*> (o .:? "adultContent"))

instance ToJSON Account where
        toJSON Account'{..}
          = object
              (catMaybes
                 [("users" .=) <$> _aaUsers,
                  ("youtubeChannelLinks" .=) <$>
                    _aaYouTubeChannelLinks,
                  Just ("kind" .= _aaKind),
                  ("sellerId" .=) <$> _aaSellerId,
                  ("name" .=) <$> _aaName,
                  ("reviewsUrl" .=) <$> _aaReviewsURL,
                  ("id" .=) <$> _aaId,
                  ("websiteUrl" .=) <$> _aaWebsiteURL,
                  ("adwordsLinks" .=) <$> _aaAdwordsLinks,
                  ("googleMyBusinessLink" .=) <$>
                    _aaGoogleMyBusinessLink,
                  ("adultContent" .=) <$> _aaAdultContent])

--
-- /See:/ 'inventorySetRequest' smart constructor.
data InventorySetRequest = InventorySetRequest'
    { _isrLoyaltyPoints :: !(Maybe LoyaltyPoints)
    , _isrQuantity :: !(Maybe (Textual Word32))
    , _isrInstallment :: !(Maybe Installment)
    , _isrSalePrice :: !(Maybe Price)
    , _isrAvailability :: !(Maybe Text)
    , _isrPickup :: !(Maybe InventoryPickup)
    , _isrSalePriceEffectiveDate :: !(Maybe Text)
    , _isrSellOnGoogleQuantity :: !(Maybe (Textual Word32))
    , _isrPrice :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventorySetRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrLoyaltyPoints'
--
-- * 'isrQuantity'
--
-- * 'isrInstallment'
--
-- * 'isrSalePrice'
--
-- * 'isrAvailability'
--
-- * 'isrPickup'
--
-- * 'isrSalePriceEffectiveDate'
--
-- * 'isrSellOnGoogleQuantity'
--
-- * 'isrPrice'
inventorySetRequest
    :: InventorySetRequest
inventorySetRequest = 
    InventorySetRequest'
    { _isrLoyaltyPoints = Nothing
    , _isrQuantity = Nothing
    , _isrInstallment = Nothing
    , _isrSalePrice = Nothing
    , _isrAvailability = Nothing
    , _isrPickup = Nothing
    , _isrSalePriceEffectiveDate = Nothing
    , _isrSellOnGoogleQuantity = Nothing
    , _isrPrice = Nothing
    }

-- | Loyalty points that users receive after purchasing the item. Japan only.
isrLoyaltyPoints :: Lens' InventorySetRequest (Maybe LoyaltyPoints)
isrLoyaltyPoints
  = lens _isrLoyaltyPoints
      (\ s a -> s{_isrLoyaltyPoints = a})

-- | The quantity of the product. Must be equal to or greater than zero.
-- Supported only for local products.
isrQuantity :: Lens' InventorySetRequest (Maybe Word32)
isrQuantity
  = lens _isrQuantity (\ s a -> s{_isrQuantity = a}) .
      mapping _Coerce

-- | Number and amount of installments to pay for an item. Brazil only.
isrInstallment :: Lens' InventorySetRequest (Maybe Installment)
isrInstallment
  = lens _isrInstallment
      (\ s a -> s{_isrInstallment = a})

-- | The sale price of the product. Mandatory if sale_price_effective_date is
-- defined.
isrSalePrice :: Lens' InventorySetRequest (Maybe Price)
isrSalePrice
  = lens _isrSalePrice (\ s a -> s{_isrSalePrice = a})

-- | The availability of the product.
isrAvailability :: Lens' InventorySetRequest (Maybe Text)
isrAvailability
  = lens _isrAvailability
      (\ s a -> s{_isrAvailability = a})

-- | Store pickup information. Only supported for local inventory. Not
-- setting pickup means \"don\'t update\" while setting it to the empty
-- value ({} in JSON) means \"delete\". Otherwise, pickupMethod and
-- pickupSla must be set together, unless pickupMethod is \"not
-- supported\".
isrPickup :: Lens' InventorySetRequest (Maybe InventoryPickup)
isrPickup
  = lens _isrPickup (\ s a -> s{_isrPickup = a})

-- | A date range represented by a pair of ISO 8601 dates separated by a
-- space, comma, or slash. Both dates might be specified as \'null\' if
-- undecided.
isrSalePriceEffectiveDate :: Lens' InventorySetRequest (Maybe Text)
isrSalePriceEffectiveDate
  = lens _isrSalePriceEffectiveDate
      (\ s a -> s{_isrSalePriceEffectiveDate = a})

-- | The quantity of the product that is reserved for sell-on-google ads.
-- Supported only for online products.
isrSellOnGoogleQuantity :: Lens' InventorySetRequest (Maybe Word32)
isrSellOnGoogleQuantity
  = lens _isrSellOnGoogleQuantity
      (\ s a -> s{_isrSellOnGoogleQuantity = a})
      . mapping _Coerce

-- | The price of the product.
isrPrice :: Lens' InventorySetRequest (Maybe Price)
isrPrice = lens _isrPrice (\ s a -> s{_isrPrice = a})

instance FromJSON InventorySetRequest where
        parseJSON
          = withObject "InventorySetRequest"
              (\ o ->
                 InventorySetRequest' <$>
                   (o .:? "loyaltyPoints") <*> (o .:? "quantity") <*>
                     (o .:? "installment")
                     <*> (o .:? "salePrice")
                     <*> (o .:? "availability")
                     <*> (o .:? "pickup")
                     <*> (o .:? "salePriceEffectiveDate")
                     <*> (o .:? "sellOnGoogleQuantity")
                     <*> (o .:? "price"))

instance ToJSON InventorySetRequest where
        toJSON InventorySetRequest'{..}
          = object
              (catMaybes
                 [("loyaltyPoints" .=) <$> _isrLoyaltyPoints,
                  ("quantity" .=) <$> _isrQuantity,
                  ("installment" .=) <$> _isrInstallment,
                  ("salePrice" .=) <$> _isrSalePrice,
                  ("availability" .=) <$> _isrAvailability,
                  ("pickup" .=) <$> _isrPickup,
                  ("salePriceEffectiveDate" .=) <$>
                    _isrSalePriceEffectiveDate,
                  ("sellOnGoogleQuantity" .=) <$>
                    _isrSellOnGoogleQuantity,
                  ("price" .=) <$> _isrPrice])

--
-- /See:/ 'ordersCancelLineItemRequest' smart constructor.
data OrdersCancelLineItemRequest = OrdersCancelLineItemRequest'
    { _oclirAmount :: !(Maybe Price)
    , _oclirQuantity :: !(Maybe (Textual Word32))
    , _oclirLineItemId :: !(Maybe Text)
    , _oclirReason :: !(Maybe Text)
    , _oclirOperationId :: !(Maybe Text)
    , _oclirAmountPretax :: !(Maybe Price)
    , _oclirProductId :: !(Maybe Text)
    , _oclirAmountTax :: !(Maybe Price)
    , _oclirReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancelLineItemRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oclirAmount'
--
-- * 'oclirQuantity'
--
-- * 'oclirLineItemId'
--
-- * 'oclirReason'
--
-- * 'oclirOperationId'
--
-- * 'oclirAmountPretax'
--
-- * 'oclirProductId'
--
-- * 'oclirAmountTax'
--
-- * 'oclirReasonText'
ordersCancelLineItemRequest
    :: OrdersCancelLineItemRequest
ordersCancelLineItemRequest = 
    OrdersCancelLineItemRequest'
    { _oclirAmount = Nothing
    , _oclirQuantity = Nothing
    , _oclirLineItemId = Nothing
    , _oclirReason = Nothing
    , _oclirOperationId = Nothing
    , _oclirAmountPretax = Nothing
    , _oclirProductId = Nothing
    , _oclirAmountTax = Nothing
    , _oclirReasonText = Nothing
    }

-- | Amount to refund for the cancelation. Optional. If not set, Google will
-- calculate the default based on the price and tax of the items involved.
-- The amount must not be larger than the net amount left on the order.
oclirAmount :: Lens' OrdersCancelLineItemRequest (Maybe Price)
oclirAmount
  = lens _oclirAmount (\ s a -> s{_oclirAmount = a})

-- | The quantity to cancel.
oclirQuantity :: Lens' OrdersCancelLineItemRequest (Maybe Word32)
oclirQuantity
  = lens _oclirQuantity
      (\ s a -> s{_oclirQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to cancel. Either lineItemId or productId is
-- required.
oclirLineItemId :: Lens' OrdersCancelLineItemRequest (Maybe Text)
oclirLineItemId
  = lens _oclirLineItemId
      (\ s a -> s{_oclirLineItemId = a})

-- | The reason for the cancellation.
oclirReason :: Lens' OrdersCancelLineItemRequest (Maybe Text)
oclirReason
  = lens _oclirReason (\ s a -> s{_oclirReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
oclirOperationId :: Lens' OrdersCancelLineItemRequest (Maybe Text)
oclirOperationId
  = lens _oclirOperationId
      (\ s a -> s{_oclirOperationId = a})

-- | Amount to refund for the cancelation. Optional. If not set, Google will
-- calculate the default based on the price and tax of the items involved.
-- The amount must not be larger than the net amount left on the order.
oclirAmountPretax :: Lens' OrdersCancelLineItemRequest (Maybe Price)
oclirAmountPretax
  = lens _oclirAmountPretax
      (\ s a -> s{_oclirAmountPretax = a})

-- | The ID of the product to cancel. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
oclirProductId :: Lens' OrdersCancelLineItemRequest (Maybe Text)
oclirProductId
  = lens _oclirProductId
      (\ s a -> s{_oclirProductId = a})

-- | Tax amount that correspond to cancellation amount in amountPretax.
oclirAmountTax :: Lens' OrdersCancelLineItemRequest (Maybe Price)
oclirAmountTax
  = lens _oclirAmountTax
      (\ s a -> s{_oclirAmountTax = a})

-- | The explanation of the reason.
oclirReasonText :: Lens' OrdersCancelLineItemRequest (Maybe Text)
oclirReasonText
  = lens _oclirReasonText
      (\ s a -> s{_oclirReasonText = a})

instance FromJSON OrdersCancelLineItemRequest where
        parseJSON
          = withObject "OrdersCancelLineItemRequest"
              (\ o ->
                 OrdersCancelLineItemRequest' <$>
                   (o .:? "amount") <*> (o .:? "quantity") <*>
                     (o .:? "lineItemId")
                     <*> (o .:? "reason")
                     <*> (o .:? "operationId")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersCancelLineItemRequest where
        toJSON OrdersCancelLineItemRequest'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _oclirAmount,
                  ("quantity" .=) <$> _oclirQuantity,
                  ("lineItemId" .=) <$> _oclirLineItemId,
                  ("reason" .=) <$> _oclirReason,
                  ("operationId" .=) <$> _oclirOperationId,
                  ("amountPretax" .=) <$> _oclirAmountPretax,
                  ("productId" .=) <$> _oclirProductId,
                  ("amountTax" .=) <$> _oclirAmountTax,
                  ("reasonText" .=) <$> _oclirReasonText])

--
-- /See:/ 'productShippingWeight' smart constructor.
data ProductShippingWeight = ProductShippingWeight'
    { _pswValue :: !(Maybe (Textual Double))
    , _pswUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductShippingWeight' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pswValue'
--
-- * 'pswUnit'
productShippingWeight
    :: ProductShippingWeight
productShippingWeight = 
    ProductShippingWeight'
    { _pswValue = Nothing
    , _pswUnit = Nothing
    }

-- | The weight of the product used to calculate the shipping cost of the
-- item.
pswValue :: Lens' ProductShippingWeight (Maybe Double)
pswValue
  = lens _pswValue (\ s a -> s{_pswValue = a}) .
      mapping _Coerce

-- | The unit of value.
pswUnit :: Lens' ProductShippingWeight (Maybe Text)
pswUnit = lens _pswUnit (\ s a -> s{_pswUnit = a})

instance FromJSON ProductShippingWeight where
        parseJSON
          = withObject "ProductShippingWeight"
              (\ o ->
                 ProductShippingWeight' <$>
                   (o .:? "value") <*> (o .:? "unit"))

instance ToJSON ProductShippingWeight where
        toJSON ProductShippingWeight'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pswValue,
                  ("unit" .=) <$> _pswUnit])

-- | A batch entry encoding a single non-batch accountstatuses request.
--
-- /See:/ 'accountstatusesCustomBatchRequestEntry' smart constructor.
data AccountstatusesCustomBatchRequestEntry = AccountstatusesCustomBatchRequestEntry'
    { _acbrecMerchantId :: !(Maybe (Textual Word64))
    , _acbrecAccountId :: !(Maybe (Textual Word64))
    , _acbrecMethod :: !(Maybe Text)
    , _acbrecBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbrecMerchantId'
--
-- * 'acbrecAccountId'
--
-- * 'acbrecMethod'
--
-- * 'acbrecBatchId'
accountstatusesCustomBatchRequestEntry
    :: AccountstatusesCustomBatchRequestEntry
accountstatusesCustomBatchRequestEntry = 
    AccountstatusesCustomBatchRequestEntry'
    { _acbrecMerchantId = Nothing
    , _acbrecAccountId = Nothing
    , _acbrecMethod = Nothing
    , _acbrecBatchId = Nothing
    }

-- | The ID of the managing account.
acbrecMerchantId :: Lens' AccountstatusesCustomBatchRequestEntry (Maybe Word64)
acbrecMerchantId
  = lens _acbrecMerchantId
      (\ s a -> s{_acbrecMerchantId = a})
      . mapping _Coerce

-- | The ID of the (sub-)account whose status to get.
acbrecAccountId :: Lens' AccountstatusesCustomBatchRequestEntry (Maybe Word64)
acbrecAccountId
  = lens _acbrecAccountId
      (\ s a -> s{_acbrecAccountId = a})
      . mapping _Coerce

-- | The method (get).
acbrecMethod :: Lens' AccountstatusesCustomBatchRequestEntry (Maybe Text)
acbrecMethod
  = lens _acbrecMethod (\ s a -> s{_acbrecMethod = a})

-- | An entry ID, unique within the batch request.
acbrecBatchId :: Lens' AccountstatusesCustomBatchRequestEntry (Maybe Word32)
acbrecBatchId
  = lens _acbrecBatchId
      (\ s a -> s{_acbrecBatchId = a})
      . mapping _Coerce

instance FromJSON
         AccountstatusesCustomBatchRequestEntry where
        parseJSON
          = withObject "AccountstatusesCustomBatchRequestEntry"
              (\ o ->
                 AccountstatusesCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "accountId") <*>
                     (o .:? "method")
                     <*> (o .:? "batchId"))

instance ToJSON
         AccountstatusesCustomBatchRequestEntry where
        toJSON AccountstatusesCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _acbrecMerchantId,
                  ("accountId" .=) <$> _acbrecAccountId,
                  ("method" .=) <$> _acbrecMethod,
                  ("batchId" .=) <$> _acbrecBatchId])

--
-- /See:/ 'deliveryTime' smart constructor.
data DeliveryTime = DeliveryTime'
    { _dtHolidayCutoffs :: !(Maybe [HolidayCutoff])
    , _dtMinTransitTimeInDays :: !(Maybe (Textual Word32))
    , _dtMaxTransitTimeInDays :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtHolidayCutoffs'
--
-- * 'dtMinTransitTimeInDays'
--
-- * 'dtMaxTransitTimeInDays'
deliveryTime
    :: DeliveryTime
deliveryTime = 
    DeliveryTime'
    { _dtHolidayCutoffs = Nothing
    , _dtMinTransitTimeInDays = Nothing
    , _dtMaxTransitTimeInDays = Nothing
    }

-- | Holiday cutoff definitions. If configured, they specify order cutoff
-- times for holiday-specific shipping.
dtHolidayCutoffs :: Lens' DeliveryTime [HolidayCutoff]
dtHolidayCutoffs
  = lens _dtHolidayCutoffs
      (\ s a -> s{_dtHolidayCutoffs = a})
      . _Default
      . _Coerce

-- | Minimum number of business days that is spent in transit. 0 means same
-- day delivery, 1 means next day delivery. Required.
dtMinTransitTimeInDays :: Lens' DeliveryTime (Maybe Word32)
dtMinTransitTimeInDays
  = lens _dtMinTransitTimeInDays
      (\ s a -> s{_dtMinTransitTimeInDays = a})
      . mapping _Coerce

-- | Maximum number of business days that is spent in transit. 0 means same
-- day delivery, 1 means next day delivery. Must be greater than or equal
-- to minTransitTimeInDays. Required.
dtMaxTransitTimeInDays :: Lens' DeliveryTime (Maybe Word32)
dtMaxTransitTimeInDays
  = lens _dtMaxTransitTimeInDays
      (\ s a -> s{_dtMaxTransitTimeInDays = a})
      . mapping _Coerce

instance FromJSON DeliveryTime where
        parseJSON
          = withObject "DeliveryTime"
              (\ o ->
                 DeliveryTime' <$>
                   (o .:? "holidayCutoffs" .!= mempty) <*>
                     (o .:? "minTransitTimeInDays")
                     <*> (o .:? "maxTransitTimeInDays"))

instance ToJSON DeliveryTime where
        toJSON DeliveryTime'{..}
          = object
              (catMaybes
                 [("holidayCutoffs" .=) <$> _dtHolidayCutoffs,
                  ("minTransitTimeInDays" .=) <$>
                    _dtMinTransitTimeInDays,
                  ("maxTransitTimeInDays" .=) <$>
                    _dtMaxTransitTimeInDays])

--
-- /See:/ 'ordersReturnRefundLineItemResponse' smart constructor.
data OrdersReturnRefundLineItemResponse = OrdersReturnRefundLineItemResponse'
    { _orrlirKind :: !Text
    , _orrlirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnRefundLineItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrlirKind'
--
-- * 'orrlirExecutionStatus'
ordersReturnRefundLineItemResponse
    :: OrdersReturnRefundLineItemResponse
ordersReturnRefundLineItemResponse = 
    OrdersReturnRefundLineItemResponse'
    { _orrlirKind = "content#ordersReturnRefundLineItemResponse"
    , _orrlirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersReturnRefundLineItemResponse\".
orrlirKind :: Lens' OrdersReturnRefundLineItemResponse Text
orrlirKind
  = lens _orrlirKind (\ s a -> s{_orrlirKind = a})

-- | The status of the execution.
orrlirExecutionStatus :: Lens' OrdersReturnRefundLineItemResponse (Maybe Text)
orrlirExecutionStatus
  = lens _orrlirExecutionStatus
      (\ s a -> s{_orrlirExecutionStatus = a})

instance FromJSON OrdersReturnRefundLineItemResponse
         where
        parseJSON
          = withObject "OrdersReturnRefundLineItemResponse"
              (\ o ->
                 OrdersReturnRefundLineItemResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersReturnRefundLineItemResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersReturnRefundLineItemResponse
         where
        toJSON OrdersReturnRefundLineItemResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _orrlirKind),
                  ("executionStatus" .=) <$> _orrlirExecutionStatus])

--
-- /See:/ 'ordersCustomBatchRequestEntryRejectReturnLineItem' smart constructor.
data OrdersCustomBatchRequestEntryRejectReturnLineItem = OrdersCustomBatchRequestEntryRejectReturnLineItem'
    { _ocbrerrliQuantity :: !(Maybe (Textual Word32))
    , _ocbrerrliLineItemId :: !(Maybe Text)
    , _ocbrerrliReason :: !(Maybe Text)
    , _ocbrerrliProductId :: !(Maybe Text)
    , _ocbrerrliReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryRejectReturnLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrerrliQuantity'
--
-- * 'ocbrerrliLineItemId'
--
-- * 'ocbrerrliReason'
--
-- * 'ocbrerrliProductId'
--
-- * 'ocbrerrliReasonText'
ordersCustomBatchRequestEntryRejectReturnLineItem
    :: OrdersCustomBatchRequestEntryRejectReturnLineItem
ordersCustomBatchRequestEntryRejectReturnLineItem = 
    OrdersCustomBatchRequestEntryRejectReturnLineItem'
    { _ocbrerrliQuantity = Nothing
    , _ocbrerrliLineItemId = Nothing
    , _ocbrerrliReason = Nothing
    , _ocbrerrliProductId = Nothing
    , _ocbrerrliReasonText = Nothing
    }

-- | The quantity to return and refund.
ocbrerrliQuantity :: Lens' OrdersCustomBatchRequestEntryRejectReturnLineItem (Maybe Word32)
ocbrerrliQuantity
  = lens _ocbrerrliQuantity
      (\ s a -> s{_ocbrerrliQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
ocbrerrliLineItemId :: Lens' OrdersCustomBatchRequestEntryRejectReturnLineItem (Maybe Text)
ocbrerrliLineItemId
  = lens _ocbrerrliLineItemId
      (\ s a -> s{_ocbrerrliLineItemId = a})

-- | The reason for the return.
ocbrerrliReason :: Lens' OrdersCustomBatchRequestEntryRejectReturnLineItem (Maybe Text)
ocbrerrliReason
  = lens _ocbrerrliReason
      (\ s a -> s{_ocbrerrliReason = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbrerrliProductId :: Lens' OrdersCustomBatchRequestEntryRejectReturnLineItem (Maybe Text)
ocbrerrliProductId
  = lens _ocbrerrliProductId
      (\ s a -> s{_ocbrerrliProductId = a})

-- | The explanation of the reason.
ocbrerrliReasonText :: Lens' OrdersCustomBatchRequestEntryRejectReturnLineItem (Maybe Text)
ocbrerrliReasonText
  = lens _ocbrerrliReasonText
      (\ s a -> s{_ocbrerrliReasonText = a})

instance FromJSON
         OrdersCustomBatchRequestEntryRejectReturnLineItem
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryRejectReturnLineItem"
              (\ o ->
                 OrdersCustomBatchRequestEntryRejectReturnLineItem'
                   <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "productId")
                     <*> (o .:? "reasonText"))

instance ToJSON
         OrdersCustomBatchRequestEntryRejectReturnLineItem
         where
        toJSON
          OrdersCustomBatchRequestEntryRejectReturnLineItem'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _ocbrerrliQuantity,
                  ("lineItemId" .=) <$> _ocbrerrliLineItemId,
                  ("reason" .=) <$> _ocbrerrliReason,
                  ("productId" .=) <$> _ocbrerrliProductId,
                  ("reasonText" .=) <$> _ocbrerrliReasonText])

-- | A batch entry encoding a single non-batch productstatuses response.
--
-- /See:/ 'productstatusesCustomBatchResponseEntry' smart constructor.
data ProductstatusesCustomBatchResponseEntry = ProductstatusesCustomBatchResponseEntry'
    { _pKind :: !Text
    , _pProductStatus :: !(Maybe ProductStatus)
    , _pErrors :: !(Maybe Errors)
    , _pBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pKind'
--
-- * 'pProductStatus'
--
-- * 'pErrors'
--
-- * 'pBatchId'
productstatusesCustomBatchResponseEntry
    :: ProductstatusesCustomBatchResponseEntry
productstatusesCustomBatchResponseEntry = 
    ProductstatusesCustomBatchResponseEntry'
    { _pKind = "content#productstatusesCustomBatchResponseEntry"
    , _pProductStatus = Nothing
    , _pErrors = Nothing
    , _pBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productstatusesCustomBatchResponseEntry\".
pKind :: Lens' ProductstatusesCustomBatchResponseEntry Text
pKind = lens _pKind (\ s a -> s{_pKind = a})

-- | The requested product status. Only defined if the request was
-- successful.
pProductStatus :: Lens' ProductstatusesCustomBatchResponseEntry (Maybe ProductStatus)
pProductStatus
  = lens _pProductStatus
      (\ s a -> s{_pProductStatus = a})

-- | A list of errors, if the request failed.
pErrors :: Lens' ProductstatusesCustomBatchResponseEntry (Maybe Errors)
pErrors = lens _pErrors (\ s a -> s{_pErrors = a})

-- | The ID of the request entry this entry responds to.
pBatchId :: Lens' ProductstatusesCustomBatchResponseEntry (Maybe Word32)
pBatchId
  = lens _pBatchId (\ s a -> s{_pBatchId = a}) .
      mapping _Coerce

instance FromJSON
         ProductstatusesCustomBatchResponseEntry where
        parseJSON
          = withObject
              "ProductstatusesCustomBatchResponseEntry"
              (\ o ->
                 ProductstatusesCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#productstatusesCustomBatchResponseEntry")
                     <*> (o .:? "productStatus")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON
         ProductstatusesCustomBatchResponseEntry where
        toJSON ProductstatusesCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _pKind),
                  ("productStatus" .=) <$> _pProductStatus,
                  ("errors" .=) <$> _pErrors,
                  ("batchId" .=) <$> _pBatchId])

--
-- /See:/ 'shippingSettingsGetSupportedHolidaysResponse' smart constructor.
data ShippingSettingsGetSupportedHolidaysResponse = ShippingSettingsGetSupportedHolidaysResponse'
    { _ssgshrKind :: !Text
    , _ssgshrHolidays :: !(Maybe [HolidaysHoliday])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsGetSupportedHolidaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgshrKind'
--
-- * 'ssgshrHolidays'
shippingSettingsGetSupportedHolidaysResponse
    :: ShippingSettingsGetSupportedHolidaysResponse
shippingSettingsGetSupportedHolidaysResponse = 
    ShippingSettingsGetSupportedHolidaysResponse'
    { _ssgshrKind = "content#shippingsettingsGetSupportedHolidaysResponse"
    , _ssgshrHolidays = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#shippingsettingsGetSupportedHolidaysResponse\".
ssgshrKind :: Lens' ShippingSettingsGetSupportedHolidaysResponse Text
ssgshrKind
  = lens _ssgshrKind (\ s a -> s{_ssgshrKind = a})

-- | A list of holidays applicable for delivery guarantees. May be empty.
ssgshrHolidays :: Lens' ShippingSettingsGetSupportedHolidaysResponse [HolidaysHoliday]
ssgshrHolidays
  = lens _ssgshrHolidays
      (\ s a -> s{_ssgshrHolidays = a})
      . _Default
      . _Coerce

instance FromJSON
         ShippingSettingsGetSupportedHolidaysResponse where
        parseJSON
          = withObject
              "ShippingSettingsGetSupportedHolidaysResponse"
              (\ o ->
                 ShippingSettingsGetSupportedHolidaysResponse' <$>
                   (o .:? "kind" .!=
                      "content#shippingsettingsGetSupportedHolidaysResponse")
                     <*> (o .:? "holidays" .!= mempty))

instance ToJSON
         ShippingSettingsGetSupportedHolidaysResponse where
        toJSON
          ShippingSettingsGetSupportedHolidaysResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ssgshrKind),
                  ("holidays" .=) <$> _ssgshrHolidays])

--
-- /See:/ 'ordersCustomBatchRequestEntryCancel' smart constructor.
data OrdersCustomBatchRequestEntryCancel = OrdersCustomBatchRequestEntryCancel'
    { _ocbrecReason :: !(Maybe Text)
    , _ocbrecReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryCancel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrecReason'
--
-- * 'ocbrecReasonText'
ordersCustomBatchRequestEntryCancel
    :: OrdersCustomBatchRequestEntryCancel
ordersCustomBatchRequestEntryCancel = 
    OrdersCustomBatchRequestEntryCancel'
    { _ocbrecReason = Nothing
    , _ocbrecReasonText = Nothing
    }

-- | The reason for the cancellation.
ocbrecReason :: Lens' OrdersCustomBatchRequestEntryCancel (Maybe Text)
ocbrecReason
  = lens _ocbrecReason (\ s a -> s{_ocbrecReason = a})

-- | The explanation of the reason.
ocbrecReasonText :: Lens' OrdersCustomBatchRequestEntryCancel (Maybe Text)
ocbrecReasonText
  = lens _ocbrecReasonText
      (\ s a -> s{_ocbrecReasonText = a})

instance FromJSON OrdersCustomBatchRequestEntryCancel
         where
        parseJSON
          = withObject "OrdersCustomBatchRequestEntryCancel"
              (\ o ->
                 OrdersCustomBatchRequestEntryCancel' <$>
                   (o .:? "reason") <*> (o .:? "reasonText"))

instance ToJSON OrdersCustomBatchRequestEntryCancel
         where
        toJSON OrdersCustomBatchRequestEntryCancel'{..}
          = object
              (catMaybes
                 [("reason" .=) <$> _ocbrecReason,
                  ("reasonText" .=) <$> _ocbrecReasonText])

--
-- /See:/ 'datafeedFormat' smart constructor.
data DatafeedFormat = DatafeedFormat'
    { _dfQuotingMode :: !(Maybe Text)
    , _dfFileEncoding :: !(Maybe Text)
    , _dfColumnDelimiter :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfQuotingMode'
--
-- * 'dfFileEncoding'
--
-- * 'dfColumnDelimiter'
datafeedFormat
    :: DatafeedFormat
datafeedFormat = 
    DatafeedFormat'
    { _dfQuotingMode = Nothing
    , _dfFileEncoding = Nothing
    , _dfColumnDelimiter = Nothing
    }

-- | Specifies how double quotes are interpreted. If not specified, the mode
-- will be auto-detected. Ignored for non-DSV data feeds.
dfQuotingMode :: Lens' DatafeedFormat (Maybe Text)
dfQuotingMode
  = lens _dfQuotingMode
      (\ s a -> s{_dfQuotingMode = a})

-- | Character encoding scheme of the data feed. If not specified, the
-- encoding will be auto-detected.
dfFileEncoding :: Lens' DatafeedFormat (Maybe Text)
dfFileEncoding
  = lens _dfFileEncoding
      (\ s a -> s{_dfFileEncoding = a})

-- | Delimiter for the separation of values in a delimiter-separated values
-- feed. If not specified, the delimiter will be auto-detected. Ignored for
-- non-DSV data feeds.
dfColumnDelimiter :: Lens' DatafeedFormat (Maybe Text)
dfColumnDelimiter
  = lens _dfColumnDelimiter
      (\ s a -> s{_dfColumnDelimiter = a})

instance FromJSON DatafeedFormat where
        parseJSON
          = withObject "DatafeedFormat"
              (\ o ->
                 DatafeedFormat' <$>
                   (o .:? "quotingMode") <*> (o .:? "fileEncoding") <*>
                     (o .:? "columnDelimiter"))

instance ToJSON DatafeedFormat where
        toJSON DatafeedFormat'{..}
          = object
              (catMaybes
                 [("quotingMode" .=) <$> _dfQuotingMode,
                  ("fileEncoding" .=) <$> _dfFileEncoding,
                  ("columnDelimiter" .=) <$> _dfColumnDelimiter])

--
-- /See:/ 'productShipping' smart constructor.
data ProductShipping = ProductShipping'
    { _pService :: !(Maybe Text)
    , _pLocationGroupName :: !(Maybe Text)
    , _pCountry :: !(Maybe Text)
    , _pPostalCode :: !(Maybe Text)
    , _pPrice :: !(Maybe Price)
    , _pRegion :: !(Maybe Text)
    , _pLocationId :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductShipping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pService'
--
-- * 'pLocationGroupName'
--
-- * 'pCountry'
--
-- * 'pPostalCode'
--
-- * 'pPrice'
--
-- * 'pRegion'
--
-- * 'pLocationId'
productShipping
    :: ProductShipping
productShipping = 
    ProductShipping'
    { _pService = Nothing
    , _pLocationGroupName = Nothing
    , _pCountry = Nothing
    , _pPostalCode = Nothing
    , _pPrice = Nothing
    , _pRegion = Nothing
    , _pLocationId = Nothing
    }

-- | A free-form description of the service class or delivery speed.
pService :: Lens' ProductShipping (Maybe Text)
pService = lens _pService (\ s a -> s{_pService = a})

-- | The location where the shipping is applicable, represented by a location
-- group name.
pLocationGroupName :: Lens' ProductShipping (Maybe Text)
pLocationGroupName
  = lens _pLocationGroupName
      (\ s a -> s{_pLocationGroupName = a})

-- | The CLDR territory code of the country to which an item will ship.
pCountry :: Lens' ProductShipping (Maybe Text)
pCountry = lens _pCountry (\ s a -> s{_pCountry = a})

-- | The postal code range that the shipping rate applies to, represented by
-- a postal code, a postal code prefix followed by a * wildcard, a range
-- between two postal codes or two postal code prefixes of equal length.
pPostalCode :: Lens' ProductShipping (Maybe Text)
pPostalCode
  = lens _pPostalCode (\ s a -> s{_pPostalCode = a})

-- | Fixed shipping price, represented as a number.
pPrice :: Lens' ProductShipping (Maybe Price)
pPrice = lens _pPrice (\ s a -> s{_pPrice = a})

-- | The geographic region to which a shipping rate applies.
pRegion :: Lens' ProductShipping (Maybe Text)
pRegion = lens _pRegion (\ s a -> s{_pRegion = a})

-- | The numeric id of a location that the shipping rate applies to as
-- defined in the AdWords API.
pLocationId :: Lens' ProductShipping (Maybe Int64)
pLocationId
  = lens _pLocationId (\ s a -> s{_pLocationId = a}) .
      mapping _Coerce

instance FromJSON ProductShipping where
        parseJSON
          = withObject "ProductShipping"
              (\ o ->
                 ProductShipping' <$>
                   (o .:? "service") <*> (o .:? "locationGroupName") <*>
                     (o .:? "country")
                     <*> (o .:? "postalCode")
                     <*> (o .:? "price")
                     <*> (o .:? "region")
                     <*> (o .:? "locationId"))

instance ToJSON ProductShipping where
        toJSON ProductShipping'{..}
          = object
              (catMaybes
                 [("service" .=) <$> _pService,
                  ("locationGroupName" .=) <$> _pLocationGroupName,
                  ("country" .=) <$> _pCountry,
                  ("postalCode" .=) <$> _pPostalCode,
                  ("price" .=) <$> _pPrice, ("region" .=) <$> _pRegion,
                  ("locationId" .=) <$> _pLocationId])

--
-- /See:/ 'shippingSettingsCustomBatchRequest' smart constructor.
newtype ShippingSettingsCustomBatchRequest = ShippingSettingsCustomBatchRequest'
    { _sscbrEntries :: Maybe [ShippingSettingsCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscbrEntries'
shippingSettingsCustomBatchRequest
    :: ShippingSettingsCustomBatchRequest
shippingSettingsCustomBatchRequest = 
    ShippingSettingsCustomBatchRequest'
    { _sscbrEntries = Nothing
    }

-- | The request entries to be processed in the batch.
sscbrEntries :: Lens' ShippingSettingsCustomBatchRequest [ShippingSettingsCustomBatchRequestEntry]
sscbrEntries
  = lens _sscbrEntries (\ s a -> s{_sscbrEntries = a})
      . _Default
      . _Coerce

instance FromJSON ShippingSettingsCustomBatchRequest
         where
        parseJSON
          = withObject "ShippingSettingsCustomBatchRequest"
              (\ o ->
                 ShippingSettingsCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON ShippingSettingsCustomBatchRequest
         where
        toJSON ShippingSettingsCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _sscbrEntries])

--
-- /See:/ 'accountsCustomBatchRequest' smart constructor.
newtype AccountsCustomBatchRequest = AccountsCustomBatchRequest'
    { _aEntries :: Maybe [AccountsCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aEntries'
accountsCustomBatchRequest
    :: AccountsCustomBatchRequest
accountsCustomBatchRequest = 
    AccountsCustomBatchRequest'
    { _aEntries = Nothing
    }

-- | The request entries to be processed in the batch.
aEntries :: Lens' AccountsCustomBatchRequest [AccountsCustomBatchRequestEntry]
aEntries
  = lens _aEntries (\ s a -> s{_aEntries = a}) .
      _Default
      . _Coerce

instance FromJSON AccountsCustomBatchRequest where
        parseJSON
          = withObject "AccountsCustomBatchRequest"
              (\ o ->
                 AccountsCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON AccountsCustomBatchRequest where
        toJSON AccountsCustomBatchRequest'{..}
          = object (catMaybes [("entries" .=) <$> _aEntries])

--
-- /See:/ 'posCustomBatchResponseEntry' smart constructor.
data PosCustomBatchResponseEntry = PosCustomBatchResponseEntry'
    { _pcbrecKind :: !Text
    , _pcbrecStore :: !(Maybe PosStore)
    , _pcbrecInventory :: !(Maybe PosInventory)
    , _pcbrecErrors :: !(Maybe Errors)
    , _pcbrecSale :: !(Maybe PosSale)
    , _pcbrecBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbrecKind'
--
-- * 'pcbrecStore'
--
-- * 'pcbrecInventory'
--
-- * 'pcbrecErrors'
--
-- * 'pcbrecSale'
--
-- * 'pcbrecBatchId'
posCustomBatchResponseEntry
    :: PosCustomBatchResponseEntry
posCustomBatchResponseEntry = 
    PosCustomBatchResponseEntry'
    { _pcbrecKind = "content#posCustomBatchResponseEntry"
    , _pcbrecStore = Nothing
    , _pcbrecInventory = Nothing
    , _pcbrecErrors = Nothing
    , _pcbrecSale = Nothing
    , _pcbrecBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posCustomBatchResponseEntry\".
pcbrecKind :: Lens' PosCustomBatchResponseEntry Text
pcbrecKind
  = lens _pcbrecKind (\ s a -> s{_pcbrecKind = a})

-- | The retrieved or updated store information.
pcbrecStore :: Lens' PosCustomBatchResponseEntry (Maybe PosStore)
pcbrecStore
  = lens _pcbrecStore (\ s a -> s{_pcbrecStore = a})

-- | The updated inventory information.
pcbrecInventory :: Lens' PosCustomBatchResponseEntry (Maybe PosInventory)
pcbrecInventory
  = lens _pcbrecInventory
      (\ s a -> s{_pcbrecInventory = a})

-- | A list of errors defined if, and only if, the request failed.
pcbrecErrors :: Lens' PosCustomBatchResponseEntry (Maybe Errors)
pcbrecErrors
  = lens _pcbrecErrors (\ s a -> s{_pcbrecErrors = a})

-- | The updated sale information.
pcbrecSale :: Lens' PosCustomBatchResponseEntry (Maybe PosSale)
pcbrecSale
  = lens _pcbrecSale (\ s a -> s{_pcbrecSale = a})

-- | The ID of the request entry to which this entry responds.
pcbrecBatchId :: Lens' PosCustomBatchResponseEntry (Maybe Word32)
pcbrecBatchId
  = lens _pcbrecBatchId
      (\ s a -> s{_pcbrecBatchId = a})
      . mapping _Coerce

instance FromJSON PosCustomBatchResponseEntry where
        parseJSON
          = withObject "PosCustomBatchResponseEntry"
              (\ o ->
                 PosCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#posCustomBatchResponseEntry")
                     <*> (o .:? "store")
                     <*> (o .:? "inventory")
                     <*> (o .:? "errors")
                     <*> (o .:? "sale")
                     <*> (o .:? "batchId"))

instance ToJSON PosCustomBatchResponseEntry where
        toJSON PosCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _pcbrecKind),
                  ("store" .=) <$> _pcbrecStore,
                  ("inventory" .=) <$> _pcbrecInventory,
                  ("errors" .=) <$> _pcbrecErrors,
                  ("sale" .=) <$> _pcbrecSale,
                  ("batchId" .=) <$> _pcbrecBatchId])

--
-- /See:/ 'productCustomAttribute' smart constructor.
data ProductCustomAttribute = ProductCustomAttribute'
    { _pcaValue :: !(Maybe Text)
    , _pcaName :: !(Maybe Text)
    , _pcaType :: !(Maybe Text)
    , _pcaUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductCustomAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcaValue'
--
-- * 'pcaName'
--
-- * 'pcaType'
--
-- * 'pcaUnit'
productCustomAttribute
    :: ProductCustomAttribute
productCustomAttribute = 
    ProductCustomAttribute'
    { _pcaValue = Nothing
    , _pcaName = Nothing
    , _pcaType = Nothing
    , _pcaUnit = Nothing
    }

-- | The value of the attribute.
pcaValue :: Lens' ProductCustomAttribute (Maybe Text)
pcaValue = lens _pcaValue (\ s a -> s{_pcaValue = a})

-- | The name of the attribute. Underscores will be replaced by spaces upon
-- insertion.
pcaName :: Lens' ProductCustomAttribute (Maybe Text)
pcaName = lens _pcaName (\ s a -> s{_pcaName = a})

-- | The type of the attribute.
pcaType :: Lens' ProductCustomAttribute (Maybe Text)
pcaType = lens _pcaType (\ s a -> s{_pcaType = a})

-- | Free-form unit of the attribute. Unit can only be used for values of
-- type INT or FLOAT.
pcaUnit :: Lens' ProductCustomAttribute (Maybe Text)
pcaUnit = lens _pcaUnit (\ s a -> s{_pcaUnit = a})

instance FromJSON ProductCustomAttribute where
        parseJSON
          = withObject "ProductCustomAttribute"
              (\ o ->
                 ProductCustomAttribute' <$>
                   (o .:? "value") <*> (o .:? "name") <*> (o .:? "type")
                     <*> (o .:? "unit"))

instance ToJSON ProductCustomAttribute where
        toJSON ProductCustomAttribute'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pcaValue,
                  ("name" .=) <$> _pcaName, ("type" .=) <$> _pcaType,
                  ("unit" .=) <$> _pcaUnit])

--
-- /See:/ 'ordersListResponse' smart constructor.
data OrdersListResponse = OrdersListResponse'
    { _olrNextPageToken :: !(Maybe Text)
    , _olrKind :: !Text
    , _olrResources :: !(Maybe [Order])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrNextPageToken'
--
-- * 'olrKind'
--
-- * 'olrResources'
ordersListResponse
    :: OrdersListResponse
ordersListResponse = 
    OrdersListResponse'
    { _olrNextPageToken = Nothing
    , _olrKind = "content#ordersListResponse"
    , _olrResources = Nothing
    }

-- | The token for the retrieval of the next page of orders.
olrNextPageToken :: Lens' OrdersListResponse (Maybe Text)
olrNextPageToken
  = lens _olrNextPageToken
      (\ s a -> s{_olrNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersListResponse\".
olrKind :: Lens' OrdersListResponse Text
olrKind = lens _olrKind (\ s a -> s{_olrKind = a})

olrResources :: Lens' OrdersListResponse [Order]
olrResources
  = lens _olrResources (\ s a -> s{_olrResources = a})
      . _Default
      . _Coerce

instance FromJSON OrdersListResponse where
        parseJSON
          = withObject "OrdersListResponse"
              (\ o ->
                 OrdersListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "content#ordersListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON OrdersListResponse where
        toJSON OrdersListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _olrNextPageToken,
                  Just ("kind" .= _olrKind),
                  ("resources" .=) <$> _olrResources])

--
-- /See:/ 'ordersUpdateLineItemShippingDetailsResponse' smart constructor.
data OrdersUpdateLineItemShippingDetailsResponse = OrdersUpdateLineItemShippingDetailsResponse'
    { _oulisdrKind :: !Text
    , _oulisdrExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateLineItemShippingDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oulisdrKind'
--
-- * 'oulisdrExecutionStatus'
ordersUpdateLineItemShippingDetailsResponse
    :: OrdersUpdateLineItemShippingDetailsResponse
ordersUpdateLineItemShippingDetailsResponse = 
    OrdersUpdateLineItemShippingDetailsResponse'
    { _oulisdrKind = "content#ordersUpdateLineItemShippingDetailsResponse"
    , _oulisdrExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersUpdateLineItemShippingDetailsResponse\".
oulisdrKind :: Lens' OrdersUpdateLineItemShippingDetailsResponse Text
oulisdrKind
  = lens _oulisdrKind (\ s a -> s{_oulisdrKind = a})

-- | The status of the execution.
oulisdrExecutionStatus :: Lens' OrdersUpdateLineItemShippingDetailsResponse (Maybe Text)
oulisdrExecutionStatus
  = lens _oulisdrExecutionStatus
      (\ s a -> s{_oulisdrExecutionStatus = a})

instance FromJSON
         OrdersUpdateLineItemShippingDetailsResponse where
        parseJSON
          = withObject
              "OrdersUpdateLineItemShippingDetailsResponse"
              (\ o ->
                 OrdersUpdateLineItemShippingDetailsResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersUpdateLineItemShippingDetailsResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON
         OrdersUpdateLineItemShippingDetailsResponse where
        toJSON
          OrdersUpdateLineItemShippingDetailsResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oulisdrKind),
                  ("executionStatus" .=) <$> _oulisdrExecutionStatus])

-- | A non-empty list of row or column headers for a table. Exactly one of
-- prices, weights, numItems, postalCodeGroupNames, or locations must be
-- set.
--
-- /See:/ 'headers' smart constructor.
data Headers = Headers'
    { _hNumberOfItems :: !(Maybe [Text])
    , _hPostalCodeGroupNames :: !(Maybe [Text])
    , _hPrices :: !(Maybe [Price])
    , _hWeights :: !(Maybe [Weight])
    , _hLocations :: !(Maybe [LocationIdSet])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hNumberOfItems'
--
-- * 'hPostalCodeGroupNames'
--
-- * 'hPrices'
--
-- * 'hWeights'
--
-- * 'hLocations'
headers
    :: Headers
headers = 
    Headers'
    { _hNumberOfItems = Nothing
    , _hPostalCodeGroupNames = Nothing
    , _hPrices = Nothing
    , _hWeights = Nothing
    , _hLocations = Nothing
    }

-- | A list of inclusive number of items upper bounds. The last value can be
-- \"infinity\". For example [\"10\", \"50\", \"infinity\"] represents the
-- headers \"\<= 10 items\", \" 50 items\". Must be non-empty. Can only be
-- set if all other fields are not set.
hNumberOfItems :: Lens' Headers [Text]
hNumberOfItems
  = lens _hNumberOfItems
      (\ s a -> s{_hNumberOfItems = a})
      . _Default
      . _Coerce

-- | A list of postal group names. The last value can be \"all other
-- locations\". Example: [\"zone 1\", \"zone 2\", \"all other locations\"].
-- The referred postal code groups must match the delivery country of the
-- service. Must be non-empty. Can only be set if all other fields are not
-- set.
hPostalCodeGroupNames :: Lens' Headers [Text]
hPostalCodeGroupNames
  = lens _hPostalCodeGroupNames
      (\ s a -> s{_hPostalCodeGroupNames = a})
      . _Default
      . _Coerce

-- | A list of inclusive order price upper bounds. The last price\'s value
-- can be \"infinity\". For example [{\"value\": \"10\", \"currency\":
-- \"USD\"}, {\"value\": \"500\", \"currency\": \"USD\"}, {\"value\":
-- \"infinity\", \"currency\": \"USD\"}] represents the headers \"\<=
-- $10\", \" $500\". All prices within a service must have the same
-- currency. Must be non-empty. Can only be set if all other fields are not
-- set.
hPrices :: Lens' Headers [Price]
hPrices
  = lens _hPrices (\ s a -> s{_hPrices = a}) . _Default
      . _Coerce

-- | A list of inclusive order weight upper bounds. The last weight\'s value
-- can be \"infinity\". For example [{\"value\": \"10\", \"unit\": \"kg\"},
-- {\"value\": \"50\", \"unit\": \"kg\"}, {\"value\": \"infinity\",
-- \"unit\": \"kg\"}] represents the headers \"\<= 10kg\", \" 50kg\". All
-- weights within a service must have the same unit. Must be non-empty. Can
-- only be set if all other fields are not set.
hWeights :: Lens' Headers [Weight]
hWeights
  = lens _hWeights (\ s a -> s{_hWeights = a}) .
      _Default
      . _Coerce

-- | A list of location ID sets. Must be non-empty. Can only be set if all
-- other fields are not set.
hLocations :: Lens' Headers [LocationIdSet]
hLocations
  = lens _hLocations (\ s a -> s{_hLocations = a}) .
      _Default
      . _Coerce

instance FromJSON Headers where
        parseJSON
          = withObject "Headers"
              (\ o ->
                 Headers' <$>
                   (o .:? "numberOfItems" .!= mempty) <*>
                     (o .:? "postalCodeGroupNames" .!= mempty)
                     <*> (o .:? "prices" .!= mempty)
                     <*> (o .:? "weights" .!= mempty)
                     <*> (o .:? "locations" .!= mempty))

instance ToJSON Headers where
        toJSON Headers'{..}
          = object
              (catMaybes
                 [("numberOfItems" .=) <$> _hNumberOfItems,
                  ("postalCodeGroupNames" .=) <$>
                    _hPostalCodeGroupNames,
                  ("prices" .=) <$> _hPrices,
                  ("weights" .=) <$> _hWeights,
                  ("locations" .=) <$> _hLocations])

--
-- /See:/ 'ordersShipLineItemsResponse' smart constructor.
data OrdersShipLineItemsResponse = OrdersShipLineItemsResponse'
    { _oslirKind :: !Text
    , _oslirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersShipLineItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslirKind'
--
-- * 'oslirExecutionStatus'
ordersShipLineItemsResponse
    :: OrdersShipLineItemsResponse
ordersShipLineItemsResponse = 
    OrdersShipLineItemsResponse'
    { _oslirKind = "content#ordersShipLineItemsResponse"
    , _oslirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersShipLineItemsResponse\".
oslirKind :: Lens' OrdersShipLineItemsResponse Text
oslirKind
  = lens _oslirKind (\ s a -> s{_oslirKind = a})

-- | The status of the execution.
oslirExecutionStatus :: Lens' OrdersShipLineItemsResponse (Maybe Text)
oslirExecutionStatus
  = lens _oslirExecutionStatus
      (\ s a -> s{_oslirExecutionStatus = a})

instance FromJSON OrdersShipLineItemsResponse where
        parseJSON
          = withObject "OrdersShipLineItemsResponse"
              (\ o ->
                 OrdersShipLineItemsResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersShipLineItemsResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersShipLineItemsResponse where
        toJSON OrdersShipLineItemsResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oslirKind),
                  ("executionStatus" .=) <$> _oslirExecutionStatus])

-- | The merchant account\'s shipping settings.
--
-- /See:/ 'shippingSettings' smart constructor.
data ShippingSettings = ShippingSettings'
    { _ssPostalCodeGroups :: !(Maybe [PostalCodeGroup])
    , _ssAccountId :: !(Maybe (Textual Word64))
    , _ssServices :: !(Maybe [Service])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssPostalCodeGroups'
--
-- * 'ssAccountId'
--
-- * 'ssServices'
shippingSettings
    :: ShippingSettings
shippingSettings = 
    ShippingSettings'
    { _ssPostalCodeGroups = Nothing
    , _ssAccountId = Nothing
    , _ssServices = Nothing
    }

-- | A list of postal code groups that can be referred to in services.
-- Optional.
ssPostalCodeGroups :: Lens' ShippingSettings [PostalCodeGroup]
ssPostalCodeGroups
  = lens _ssPostalCodeGroups
      (\ s a -> s{_ssPostalCodeGroups = a})
      . _Default
      . _Coerce

-- | The ID of the account to which these account shipping settings belong.
-- Ignored upon update, always present in get request responses.
ssAccountId :: Lens' ShippingSettings (Maybe Word64)
ssAccountId
  = lens _ssAccountId (\ s a -> s{_ssAccountId = a}) .
      mapping _Coerce

-- | The target account\'s list of services. Optional.
ssServices :: Lens' ShippingSettings [Service]
ssServices
  = lens _ssServices (\ s a -> s{_ssServices = a}) .
      _Default
      . _Coerce

instance FromJSON ShippingSettings where
        parseJSON
          = withObject "ShippingSettings"
              (\ o ->
                 ShippingSettings' <$>
                   (o .:? "postalCodeGroups" .!= mempty) <*>
                     (o .:? "accountId")
                     <*> (o .:? "services" .!= mempty))

instance ToJSON ShippingSettings where
        toJSON ShippingSettings'{..}
          = object
              (catMaybes
                 [("postalCodeGroups" .=) <$> _ssPostalCodeGroups,
                  ("accountId" .=) <$> _ssAccountId,
                  ("services" .=) <$> _ssServices])

--
-- /See:/ 'postalCodeRange' smart constructor.
data PostalCodeRange = PostalCodeRange'
    { _pcrPostalCodeRangeBegin :: !(Maybe Text)
    , _pcrPostalCodeRangeEnd :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PostalCodeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrPostalCodeRangeBegin'
--
-- * 'pcrPostalCodeRangeEnd'
postalCodeRange
    :: PostalCodeRange
postalCodeRange = 
    PostalCodeRange'
    { _pcrPostalCodeRangeBegin = Nothing
    , _pcrPostalCodeRangeEnd = Nothing
    }

-- | A postal code or a pattern of the form prefix* denoting the inclusive
-- lower bound of the range defining the area. Examples values: \"94108\",
-- \"9410*\", \"9*\". Required.
pcrPostalCodeRangeBegin :: Lens' PostalCodeRange (Maybe Text)
pcrPostalCodeRangeBegin
  = lens _pcrPostalCodeRangeBegin
      (\ s a -> s{_pcrPostalCodeRangeBegin = a})

-- | A postal code or a pattern of the form prefix* denoting the inclusive
-- upper bound of the range defining the area. It must have the same length
-- as postalCodeRangeBegin: if postalCodeRangeBegin is a postal code then
-- postalCodeRangeEnd must be a postal code too; if postalCodeRangeBegin is
-- a pattern then postalCodeRangeEnd must be a pattern with the same prefix
-- length. Optional: if not set, then the area is defined as being all the
-- postal codes matching postalCodeRangeBegin.
pcrPostalCodeRangeEnd :: Lens' PostalCodeRange (Maybe Text)
pcrPostalCodeRangeEnd
  = lens _pcrPostalCodeRangeEnd
      (\ s a -> s{_pcrPostalCodeRangeEnd = a})

instance FromJSON PostalCodeRange where
        parseJSON
          = withObject "PostalCodeRange"
              (\ o ->
                 PostalCodeRange' <$>
                   (o .:? "postalCodeRangeBegin") <*>
                     (o .:? "postalCodeRangeEnd"))

instance ToJSON PostalCodeRange where
        toJSON PostalCodeRange'{..}
          = object
              (catMaybes
                 [("postalCodeRangeBegin" .=) <$>
                    _pcrPostalCodeRangeBegin,
                  ("postalCodeRangeEnd" .=) <$>
                    _pcrPostalCodeRangeEnd])

--
-- /See:/ 'ordersUpdateShipmentResponse' smart constructor.
data OrdersUpdateShipmentResponse = OrdersUpdateShipmentResponse'
    { _ousrKind :: !Text
    , _ousrExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateShipmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ousrKind'
--
-- * 'ousrExecutionStatus'
ordersUpdateShipmentResponse
    :: OrdersUpdateShipmentResponse
ordersUpdateShipmentResponse = 
    OrdersUpdateShipmentResponse'
    { _ousrKind = "content#ordersUpdateShipmentResponse"
    , _ousrExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersUpdateShipmentResponse\".
ousrKind :: Lens' OrdersUpdateShipmentResponse Text
ousrKind = lens _ousrKind (\ s a -> s{_ousrKind = a})

-- | The status of the execution.
ousrExecutionStatus :: Lens' OrdersUpdateShipmentResponse (Maybe Text)
ousrExecutionStatus
  = lens _ousrExecutionStatus
      (\ s a -> s{_ousrExecutionStatus = a})

instance FromJSON OrdersUpdateShipmentResponse where
        parseJSON
          = withObject "OrdersUpdateShipmentResponse"
              (\ o ->
                 OrdersUpdateShipmentResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersUpdateShipmentResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersUpdateShipmentResponse where
        toJSON OrdersUpdateShipmentResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ousrKind),
                  ("executionStatus" .=) <$> _ousrExecutionStatus])

--
-- /See:/ 'productstatusesCustomBatchRequest' smart constructor.
newtype ProductstatusesCustomBatchRequest = ProductstatusesCustomBatchRequest'
    { _pcbrcEntries :: Maybe [ProductstatusesCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbrcEntries'
productstatusesCustomBatchRequest
    :: ProductstatusesCustomBatchRequest
productstatusesCustomBatchRequest = 
    ProductstatusesCustomBatchRequest'
    { _pcbrcEntries = Nothing
    }

-- | The request entries to be processed in the batch.
pcbrcEntries :: Lens' ProductstatusesCustomBatchRequest [ProductstatusesCustomBatchRequestEntry]
pcbrcEntries
  = lens _pcbrcEntries (\ s a -> s{_pcbrcEntries = a})
      . _Default
      . _Coerce

instance FromJSON ProductstatusesCustomBatchRequest
         where
        parseJSON
          = withObject "ProductstatusesCustomBatchRequest"
              (\ o ->
                 ProductstatusesCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON ProductstatusesCustomBatchRequest
         where
        toJSON ProductstatusesCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _pcbrcEntries])

--
-- /See:/ 'accountYouTubeChannelLink' smart constructor.
data AccountYouTubeChannelLink = AccountYouTubeChannelLink'
    { _aytclStatus :: !(Maybe Text)
    , _aytclChannelId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountYouTubeChannelLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aytclStatus'
--
-- * 'aytclChannelId'
accountYouTubeChannelLink
    :: AccountYouTubeChannelLink
accountYouTubeChannelLink = 
    AccountYouTubeChannelLink'
    { _aytclStatus = Nothing
    , _aytclChannelId = Nothing
    }

-- | Status of the link between this Merchant Center account and the YouTube
-- channel. Upon retrieval, it represents the actual status of the link and
-- can be either active if it was approved in YT Creator Studio or pending
-- if it\'s pending approval. Upon insertion, it represents the intended
-- status of the link. Re-uploading a link with status active when it\'s
-- still pending or with status pending when it\'s already active will have
-- no effect: the status will remain unchanged. Re-uploading a link with
-- deprecated status inactive is equivalent to not submitting the link at
-- all and will delete the link if it was active or cancel the link request
-- if it was pending.
aytclStatus :: Lens' AccountYouTubeChannelLink (Maybe Text)
aytclStatus
  = lens _aytclStatus (\ s a -> s{_aytclStatus = a})

-- | Channel ID.
aytclChannelId :: Lens' AccountYouTubeChannelLink (Maybe Text)
aytclChannelId
  = lens _aytclChannelId
      (\ s a -> s{_aytclChannelId = a})

instance FromJSON AccountYouTubeChannelLink where
        parseJSON
          = withObject "AccountYouTubeChannelLink"
              (\ o ->
                 AccountYouTubeChannelLink' <$>
                   (o .:? "status") <*> (o .:? "channelId"))

instance ToJSON AccountYouTubeChannelLink where
        toJSON AccountYouTubeChannelLink'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _aytclStatus,
                  ("channelId" .=) <$> _aytclChannelId])

--
-- /See:/ 'ordersReturnLineItemResponse' smart constructor.
data OrdersReturnLineItemResponse = OrdersReturnLineItemResponse'
    { _orlirKind :: !Text
    , _orlirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnLineItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orlirKind'
--
-- * 'orlirExecutionStatus'
ordersReturnLineItemResponse
    :: OrdersReturnLineItemResponse
ordersReturnLineItemResponse = 
    OrdersReturnLineItemResponse'
    { _orlirKind = "content#ordersReturnLineItemResponse"
    , _orlirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersReturnLineItemResponse\".
orlirKind :: Lens' OrdersReturnLineItemResponse Text
orlirKind
  = lens _orlirKind (\ s a -> s{_orlirKind = a})

-- | The status of the execution.
orlirExecutionStatus :: Lens' OrdersReturnLineItemResponse (Maybe Text)
orlirExecutionStatus
  = lens _orlirExecutionStatus
      (\ s a -> s{_orlirExecutionStatus = a})

instance FromJSON OrdersReturnLineItemResponse where
        parseJSON
          = withObject "OrdersReturnLineItemResponse"
              (\ o ->
                 OrdersReturnLineItemResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersReturnLineItemResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersReturnLineItemResponse where
        toJSON OrdersReturnLineItemResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _orlirKind),
                  ("executionStatus" .=) <$> _orlirExecutionStatus])

--
-- /See:/ 'productCustomGroup' smart constructor.
data ProductCustomGroup = ProductCustomGroup'
    { _pName :: !(Maybe Text)
    , _pAttributes :: !(Maybe [ProductCustomAttribute])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductCustomGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pName'
--
-- * 'pAttributes'
productCustomGroup
    :: ProductCustomGroup
productCustomGroup = 
    ProductCustomGroup'
    { _pName = Nothing
    , _pAttributes = Nothing
    }

-- | The name of the group. Underscores will be replaced by spaces upon
-- insertion.
pName :: Lens' ProductCustomGroup (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | The sub-attributes.
pAttributes :: Lens' ProductCustomGroup [ProductCustomAttribute]
pAttributes
  = lens _pAttributes (\ s a -> s{_pAttributes = a}) .
      _Default
      . _Coerce

instance FromJSON ProductCustomGroup where
        parseJSON
          = withObject "ProductCustomGroup"
              (\ o ->
                 ProductCustomGroup' <$>
                   (o .:? "name") <*> (o .:? "attributes" .!= mempty))

instance ToJSON ProductCustomGroup where
        toJSON ProductCustomGroup'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _pName,
                  ("attributes" .=) <$> _pAttributes])

--
-- /See:/ 'posInventoryRequest' smart constructor.
data PosInventoryRequest = PosInventoryRequest'
    { _posStoreCode :: !(Maybe Text)
    , _posItemId :: !(Maybe Text)
    , _posQuantity :: !(Maybe (Textual Int64))
    , _posTargetCountry :: !(Maybe Text)
    , _posGtin :: !(Maybe Text)
    , _posPrice :: !(Maybe Price)
    , _posContentLanguage :: !(Maybe Text)
    , _posTimestamp :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosInventoryRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'posStoreCode'
--
-- * 'posItemId'
--
-- * 'posQuantity'
--
-- * 'posTargetCountry'
--
-- * 'posGtin'
--
-- * 'posPrice'
--
-- * 'posContentLanguage'
--
-- * 'posTimestamp'
posInventoryRequest
    :: PosInventoryRequest
posInventoryRequest = 
    PosInventoryRequest'
    { _posStoreCode = Nothing
    , _posItemId = Nothing
    , _posQuantity = Nothing
    , _posTargetCountry = Nothing
    , _posGtin = Nothing
    , _posPrice = Nothing
    , _posContentLanguage = Nothing
    , _posTimestamp = Nothing
    }

-- | The identifier of the merchant\'s store.
posStoreCode :: Lens' PosInventoryRequest (Maybe Text)
posStoreCode
  = lens _posStoreCode (\ s a -> s{_posStoreCode = a})

-- | A unique identifier for the item.
posItemId :: Lens' PosInventoryRequest (Maybe Text)
posItemId
  = lens _posItemId (\ s a -> s{_posItemId = a})

-- | The available quantity of the item.
posQuantity :: Lens' PosInventoryRequest (Maybe Int64)
posQuantity
  = lens _posQuantity (\ s a -> s{_posQuantity = a}) .
      mapping _Coerce

-- | The CLDR territory code for the item.
posTargetCountry :: Lens' PosInventoryRequest (Maybe Text)
posTargetCountry
  = lens _posTargetCountry
      (\ s a -> s{_posTargetCountry = a})

-- | Global Trade Item Number.
posGtin :: Lens' PosInventoryRequest (Maybe Text)
posGtin = lens _posGtin (\ s a -> s{_posGtin = a})

-- | The current price of the item.
posPrice :: Lens' PosInventoryRequest (Maybe Price)
posPrice = lens _posPrice (\ s a -> s{_posPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
posContentLanguage :: Lens' PosInventoryRequest (Maybe Text)
posContentLanguage
  = lens _posContentLanguage
      (\ s a -> s{_posContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
posTimestamp :: Lens' PosInventoryRequest (Maybe Text)
posTimestamp
  = lens _posTimestamp (\ s a -> s{_posTimestamp = a})

instance FromJSON PosInventoryRequest where
        parseJSON
          = withObject "PosInventoryRequest"
              (\ o ->
                 PosInventoryRequest' <$>
                   (o .:? "storeCode") <*> (o .:? "itemId") <*>
                     (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp"))

instance ToJSON PosInventoryRequest where
        toJSON PosInventoryRequest'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _posStoreCode,
                  ("itemId" .=) <$> _posItemId,
                  ("quantity" .=) <$> _posQuantity,
                  ("targetCountry" .=) <$> _posTargetCountry,
                  ("gtin" .=) <$> _posGtin, ("price" .=) <$> _posPrice,
                  ("contentLanguage" .=) <$> _posContentLanguage,
                  ("timestamp" .=) <$> _posTimestamp])

--
-- /See:/ 'accountstatusesCustomBatchResponse' smart constructor.
data AccountstatusesCustomBatchResponse = AccountstatusesCustomBatchResponse'
    { _acccEntries :: !(Maybe [AccountstatusesCustomBatchResponseEntry])
    , _acccKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acccEntries'
--
-- * 'acccKind'
accountstatusesCustomBatchResponse
    :: AccountstatusesCustomBatchResponse
accountstatusesCustomBatchResponse = 
    AccountstatusesCustomBatchResponse'
    { _acccEntries = Nothing
    , _acccKind = "content#accountstatusesCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
acccEntries :: Lens' AccountstatusesCustomBatchResponse [AccountstatusesCustomBatchResponseEntry]
acccEntries
  = lens _acccEntries (\ s a -> s{_acccEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountstatusesCustomBatchResponse\".
acccKind :: Lens' AccountstatusesCustomBatchResponse Text
acccKind = lens _acccKind (\ s a -> s{_acccKind = a})

instance FromJSON AccountstatusesCustomBatchResponse
         where
        parseJSON
          = withObject "AccountstatusesCustomBatchResponse"
              (\ o ->
                 AccountstatusesCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#accountstatusesCustomBatchResponse"))

instance ToJSON AccountstatusesCustomBatchResponse
         where
        toJSON AccountstatusesCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _acccEntries,
                  Just ("kind" .= _acccKind)])

-- | A batch entry encoding a single non-batch shipping settings response.
--
-- /See:/ 'shippingSettingsCustomBatchResponseEntry' smart constructor.
data ShippingSettingsCustomBatchResponseEntry = ShippingSettingsCustomBatchResponseEntry'
    { _sKind :: !Text
    , _sShippingSettings :: !(Maybe ShippingSettings)
    , _sErrors :: !(Maybe Errors)
    , _sBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sKind'
--
-- * 'sShippingSettings'
--
-- * 'sErrors'
--
-- * 'sBatchId'
shippingSettingsCustomBatchResponseEntry
    :: ShippingSettingsCustomBatchResponseEntry
shippingSettingsCustomBatchResponseEntry = 
    ShippingSettingsCustomBatchResponseEntry'
    { _sKind = "content#shippingsettingsCustomBatchResponseEntry"
    , _sShippingSettings = Nothing
    , _sErrors = Nothing
    , _sBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#shippingsettingsCustomBatchResponseEntry\".
sKind :: Lens' ShippingSettingsCustomBatchResponseEntry Text
sKind = lens _sKind (\ s a -> s{_sKind = a})

-- | The retrieved or updated account shipping settings.
sShippingSettings :: Lens' ShippingSettingsCustomBatchResponseEntry (Maybe ShippingSettings)
sShippingSettings
  = lens _sShippingSettings
      (\ s a -> s{_sShippingSettings = a})

-- | A list of errors defined if, and only if, the request failed.
sErrors :: Lens' ShippingSettingsCustomBatchResponseEntry (Maybe Errors)
sErrors = lens _sErrors (\ s a -> s{_sErrors = a})

-- | The ID of the request entry to which this entry responds.
sBatchId :: Lens' ShippingSettingsCustomBatchResponseEntry (Maybe Word32)
sBatchId
  = lens _sBatchId (\ s a -> s{_sBatchId = a}) .
      mapping _Coerce

instance FromJSON
         ShippingSettingsCustomBatchResponseEntry where
        parseJSON
          = withObject
              "ShippingSettingsCustomBatchResponseEntry"
              (\ o ->
                 ShippingSettingsCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#shippingsettingsCustomBatchResponseEntry")
                     <*> (o .:? "shippingSettings")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON
         ShippingSettingsCustomBatchResponseEntry where
        toJSON ShippingSettingsCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _sKind),
                  ("shippingSettings" .=) <$> _sShippingSettings,
                  ("errors" .=) <$> _sErrors,
                  ("batchId" .=) <$> _sBatchId])

-- | The status of a product, i.e., information about a product computed
-- asynchronously by the data quality analysis.
--
-- /See:/ 'productStatus' smart constructor.
data ProductStatus = ProductStatus'
    { _ppDataQualityIssues :: !(Maybe [ProductStatusDataQualityIssue])
    , _ppKind :: !Text
    , _ppLink :: !(Maybe Text)
    , _ppDestinationStatuses :: !(Maybe [ProductStatusDestinationStatus])
    , _ppLastUpdateDate :: !(Maybe Text)
    , _ppCreationDate :: !(Maybe Text)
    , _ppTitle :: !(Maybe Text)
    , _ppProduct :: !(Maybe Product)
    , _ppGoogleExpirationDate :: !(Maybe Text)
    , _ppProductId :: !(Maybe Text)
    , _ppItemLevelIssues :: !(Maybe [ProductStatusItemLevelIssue])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppDataQualityIssues'
--
-- * 'ppKind'
--
-- * 'ppLink'
--
-- * 'ppDestinationStatuses'
--
-- * 'ppLastUpdateDate'
--
-- * 'ppCreationDate'
--
-- * 'ppTitle'
--
-- * 'ppProduct'
--
-- * 'ppGoogleExpirationDate'
--
-- * 'ppProductId'
--
-- * 'ppItemLevelIssues'
productStatus
    :: ProductStatus
productStatus = 
    ProductStatus'
    { _ppDataQualityIssues = Nothing
    , _ppKind = "content#productStatus"
    , _ppLink = Nothing
    , _ppDestinationStatuses = Nothing
    , _ppLastUpdateDate = Nothing
    , _ppCreationDate = Nothing
    , _ppTitle = Nothing
    , _ppProduct = Nothing
    , _ppGoogleExpirationDate = Nothing
    , _ppProductId = Nothing
    , _ppItemLevelIssues = Nothing
    }

-- | A list of data quality issues associated with the product.
ppDataQualityIssues :: Lens' ProductStatus [ProductStatusDataQualityIssue]
ppDataQualityIssues
  = lens _ppDataQualityIssues
      (\ s a -> s{_ppDataQualityIssues = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productStatus\".
ppKind :: Lens' ProductStatus Text
ppKind = lens _ppKind (\ s a -> s{_ppKind = a})

-- | The link to the product.
ppLink :: Lens' ProductStatus (Maybe Text)
ppLink = lens _ppLink (\ s a -> s{_ppLink = a})

-- | The intended destinations for the product.
ppDestinationStatuses :: Lens' ProductStatus [ProductStatusDestinationStatus]
ppDestinationStatuses
  = lens _ppDestinationStatuses
      (\ s a -> s{_ppDestinationStatuses = a})
      . _Default
      . _Coerce

-- | Date on which the item has been last updated, in ISO 8601 format.
ppLastUpdateDate :: Lens' ProductStatus (Maybe Text)
ppLastUpdateDate
  = lens _ppLastUpdateDate
      (\ s a -> s{_ppLastUpdateDate = a})

-- | Date on which the item has been created, in ISO 8601 format.
ppCreationDate :: Lens' ProductStatus (Maybe Text)
ppCreationDate
  = lens _ppCreationDate
      (\ s a -> s{_ppCreationDate = a})

-- | The title of the product.
ppTitle :: Lens' ProductStatus (Maybe Text)
ppTitle = lens _ppTitle (\ s a -> s{_ppTitle = a})

-- | Product data after applying all the join inputs.
ppProduct :: Lens' ProductStatus (Maybe Product)
ppProduct
  = lens _ppProduct (\ s a -> s{_ppProduct = a})

-- | Date on which the item expires in Google Shopping, in ISO 8601 format.
ppGoogleExpirationDate :: Lens' ProductStatus (Maybe Text)
ppGoogleExpirationDate
  = lens _ppGoogleExpirationDate
      (\ s a -> s{_ppGoogleExpirationDate = a})

-- | The id of the product for which status is reported.
ppProductId :: Lens' ProductStatus (Maybe Text)
ppProductId
  = lens _ppProductId (\ s a -> s{_ppProductId = a})

-- | A list of all issues associated with the product.
ppItemLevelIssues :: Lens' ProductStatus [ProductStatusItemLevelIssue]
ppItemLevelIssues
  = lens _ppItemLevelIssues
      (\ s a -> s{_ppItemLevelIssues = a})
      . _Default
      . _Coerce

instance FromJSON ProductStatus where
        parseJSON
          = withObject "ProductStatus"
              (\ o ->
                 ProductStatus' <$>
                   (o .:? "dataQualityIssues" .!= mempty) <*>
                     (o .:? "kind" .!= "content#productStatus")
                     <*> (o .:? "link")
                     <*> (o .:? "destinationStatuses" .!= mempty)
                     <*> (o .:? "lastUpdateDate")
                     <*> (o .:? "creationDate")
                     <*> (o .:? "title")
                     <*> (o .:? "product")
                     <*> (o .:? "googleExpirationDate")
                     <*> (o .:? "productId")
                     <*> (o .:? "itemLevelIssues" .!= mempty))

instance ToJSON ProductStatus where
        toJSON ProductStatus'{..}
          = object
              (catMaybes
                 [("dataQualityIssues" .=) <$> _ppDataQualityIssues,
                  Just ("kind" .= _ppKind), ("link" .=) <$> _ppLink,
                  ("destinationStatuses" .=) <$>
                    _ppDestinationStatuses,
                  ("lastUpdateDate" .=) <$> _ppLastUpdateDate,
                  ("creationDate" .=) <$> _ppCreationDate,
                  ("title" .=) <$> _ppTitle,
                  ("product" .=) <$> _ppProduct,
                  ("googleExpirationDate" .=) <$>
                    _ppGoogleExpirationDate,
                  ("productId" .=) <$> _ppProductId,
                  ("itemLevelIssues" .=) <$> _ppItemLevelIssues])

--
-- /See:/ 'accountstatusesListResponse' smart constructor.
data AccountstatusesListResponse = AccountstatusesListResponse'
    { _alrlNextPageToken :: !(Maybe Text)
    , _alrlKind :: !Text
    , _alrlResources :: !(Maybe [AccountStatus])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alrlNextPageToken'
--
-- * 'alrlKind'
--
-- * 'alrlResources'
accountstatusesListResponse
    :: AccountstatusesListResponse
accountstatusesListResponse = 
    AccountstatusesListResponse'
    { _alrlNextPageToken = Nothing
    , _alrlKind = "content#accountstatusesListResponse"
    , _alrlResources = Nothing
    }

-- | The token for the retrieval of the next page of account statuses.
alrlNextPageToken :: Lens' AccountstatusesListResponse (Maybe Text)
alrlNextPageToken
  = lens _alrlNextPageToken
      (\ s a -> s{_alrlNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountstatusesListResponse\".
alrlKind :: Lens' AccountstatusesListResponse Text
alrlKind = lens _alrlKind (\ s a -> s{_alrlKind = a})

alrlResources :: Lens' AccountstatusesListResponse [AccountStatus]
alrlResources
  = lens _alrlResources
      (\ s a -> s{_alrlResources = a})
      . _Default
      . _Coerce

instance FromJSON AccountstatusesListResponse where
        parseJSON
          = withObject "AccountstatusesListResponse"
              (\ o ->
                 AccountstatusesListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!=
                        "content#accountstatusesListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON AccountstatusesListResponse where
        toJSON AccountstatusesListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _alrlNextPageToken,
                  Just ("kind" .= _alrlKind),
                  ("resources" .=) <$> _alrlResources])

--
-- /See:/ 'ordersSetLineItemMetadataResponse' smart constructor.
data OrdersSetLineItemMetadataResponse = OrdersSetLineItemMetadataResponse'
    { _oslimrKind :: !Text
    , _oslimrExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersSetLineItemMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslimrKind'
--
-- * 'oslimrExecutionStatus'
ordersSetLineItemMetadataResponse
    :: OrdersSetLineItemMetadataResponse
ordersSetLineItemMetadataResponse = 
    OrdersSetLineItemMetadataResponse'
    { _oslimrKind = "content#ordersSetLineItemMetadataResponse"
    , _oslimrExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersSetLineItemMetadataResponse\".
oslimrKind :: Lens' OrdersSetLineItemMetadataResponse Text
oslimrKind
  = lens _oslimrKind (\ s a -> s{_oslimrKind = a})

-- | The status of the execution.
oslimrExecutionStatus :: Lens' OrdersSetLineItemMetadataResponse (Maybe Text)
oslimrExecutionStatus
  = lens _oslimrExecutionStatus
      (\ s a -> s{_oslimrExecutionStatus = a})

instance FromJSON OrdersSetLineItemMetadataResponse
         where
        parseJSON
          = withObject "OrdersSetLineItemMetadataResponse"
              (\ o ->
                 OrdersSetLineItemMetadataResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersSetLineItemMetadataResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersSetLineItemMetadataResponse
         where
        toJSON OrdersSetLineItemMetadataResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oslimrKind),
                  ("executionStatus" .=) <$> _oslimrExecutionStatus])

--
-- /See:/ 'accounttaxCustomBatchRequest' smart constructor.
newtype AccounttaxCustomBatchRequest = AccounttaxCustomBatchRequest'
    { _accEntries :: Maybe [AccounttaxCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accEntries'
accounttaxCustomBatchRequest
    :: AccounttaxCustomBatchRequest
accounttaxCustomBatchRequest = 
    AccounttaxCustomBatchRequest'
    { _accEntries = Nothing
    }

-- | The request entries to be processed in the batch.
accEntries :: Lens' AccounttaxCustomBatchRequest [AccounttaxCustomBatchRequestEntry]
accEntries
  = lens _accEntries (\ s a -> s{_accEntries = a}) .
      _Default
      . _Coerce

instance FromJSON AccounttaxCustomBatchRequest where
        parseJSON
          = withObject "AccounttaxCustomBatchRequest"
              (\ o ->
                 AccounttaxCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON AccounttaxCustomBatchRequest where
        toJSON AccounttaxCustomBatchRequest'{..}
          = object (catMaybes [("entries" .=) <$> _accEntries])

-- | A batch entry encoding a single non-batch products request.
--
-- /See:/ 'productsCustomBatchRequestEntry' smart constructor.
data ProductsCustomBatchRequestEntry = ProductsCustomBatchRequestEntry'
    { _prorMerchantId :: !(Maybe (Textual Word64))
    , _prorMethod :: !(Maybe Text)
    , _prorProduct :: !(Maybe Product)
    , _prorProductId :: !(Maybe Text)
    , _prorBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prorMerchantId'
--
-- * 'prorMethod'
--
-- * 'prorProduct'
--
-- * 'prorProductId'
--
-- * 'prorBatchId'
productsCustomBatchRequestEntry
    :: ProductsCustomBatchRequestEntry
productsCustomBatchRequestEntry = 
    ProductsCustomBatchRequestEntry'
    { _prorMerchantId = Nothing
    , _prorMethod = Nothing
    , _prorProduct = Nothing
    , _prorProductId = Nothing
    , _prorBatchId = Nothing
    }

-- | The ID of the managing account.
prorMerchantId :: Lens' ProductsCustomBatchRequestEntry (Maybe Word64)
prorMerchantId
  = lens _prorMerchantId
      (\ s a -> s{_prorMerchantId = a})
      . mapping _Coerce

prorMethod :: Lens' ProductsCustomBatchRequestEntry (Maybe Text)
prorMethod
  = lens _prorMethod (\ s a -> s{_prorMethod = a})

-- | The product to insert. Only required if the method is insert.
prorProduct :: Lens' ProductsCustomBatchRequestEntry (Maybe Product)
prorProduct
  = lens _prorProduct (\ s a -> s{_prorProduct = a})

-- | The ID of the product to get or delete. Only defined if the method is
-- get or delete.
prorProductId :: Lens' ProductsCustomBatchRequestEntry (Maybe Text)
prorProductId
  = lens _prorProductId
      (\ s a -> s{_prorProductId = a})

-- | An entry ID, unique within the batch request.
prorBatchId :: Lens' ProductsCustomBatchRequestEntry (Maybe Word32)
prorBatchId
  = lens _prorBatchId (\ s a -> s{_prorBatchId = a}) .
      mapping _Coerce

instance FromJSON ProductsCustomBatchRequestEntry
         where
        parseJSON
          = withObject "ProductsCustomBatchRequestEntry"
              (\ o ->
                 ProductsCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "method") <*>
                     (o .:? "product")
                     <*> (o .:? "productId")
                     <*> (o .:? "batchId"))

instance ToJSON ProductsCustomBatchRequestEntry where
        toJSON ProductsCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _prorMerchantId,
                  ("method" .=) <$> _prorMethod,
                  ("product" .=) <$> _prorProduct,
                  ("productId" .=) <$> _prorProductId,
                  ("batchId" .=) <$> _prorBatchId])

--
-- /See:/ 'accountGoogleMyBusinessLink' smart constructor.
data AccountGoogleMyBusinessLink = AccountGoogleMyBusinessLink'
    { _agmblGmbEmail :: !(Maybe Text)
    , _agmblStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountGoogleMyBusinessLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agmblGmbEmail'
--
-- * 'agmblStatus'
accountGoogleMyBusinessLink
    :: AccountGoogleMyBusinessLink
accountGoogleMyBusinessLink = 
    AccountGoogleMyBusinessLink'
    { _agmblGmbEmail = Nothing
    , _agmblStatus = Nothing
    }

-- | The GMB email address of which a specific account within a GMB account.
-- A sample account within a GMB account could be a business account with
-- set of locations, managed under the GMB account.
agmblGmbEmail :: Lens' AccountGoogleMyBusinessLink (Maybe Text)
agmblGmbEmail
  = lens _agmblGmbEmail
      (\ s a -> s{_agmblGmbEmail = a})

-- | Status of the link between this Merchant Center account and the GMB
-- account.
agmblStatus :: Lens' AccountGoogleMyBusinessLink (Maybe Text)
agmblStatus
  = lens _agmblStatus (\ s a -> s{_agmblStatus = a})

instance FromJSON AccountGoogleMyBusinessLink where
        parseJSON
          = withObject "AccountGoogleMyBusinessLink"
              (\ o ->
                 AccountGoogleMyBusinessLink' <$>
                   (o .:? "gmbEmail") <*> (o .:? "status"))

instance ToJSON AccountGoogleMyBusinessLink where
        toJSON AccountGoogleMyBusinessLink'{..}
          = object
              (catMaybes
                 [("gmbEmail" .=) <$> _agmblGmbEmail,
                  ("status" .=) <$> _agmblStatus])

-- | A batch entry encoding a single non-batch datafeedstatuses request.
--
-- /See:/ 'datafeedstatusesCustomBatchRequestEntry' smart constructor.
data DatafeedstatusesCustomBatchRequestEntry = DatafeedstatusesCustomBatchRequestEntry'
    { _dMerchantId :: !(Maybe (Textual Word64))
    , _dCountry :: !(Maybe Text)
    , _dMethod :: !(Maybe Text)
    , _dDatafeedId :: !(Maybe (Textual Word64))
    , _dLanguage :: !(Maybe Text)
    , _dBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedstatusesCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMerchantId'
--
-- * 'dCountry'
--
-- * 'dMethod'
--
-- * 'dDatafeedId'
--
-- * 'dLanguage'
--
-- * 'dBatchId'
datafeedstatusesCustomBatchRequestEntry
    :: DatafeedstatusesCustomBatchRequestEntry
datafeedstatusesCustomBatchRequestEntry = 
    DatafeedstatusesCustomBatchRequestEntry'
    { _dMerchantId = Nothing
    , _dCountry = Nothing
    , _dMethod = Nothing
    , _dDatafeedId = Nothing
    , _dLanguage = Nothing
    , _dBatchId = Nothing
    }

-- | The ID of the managing account.
dMerchantId :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Word64)
dMerchantId
  = lens _dMerchantId (\ s a -> s{_dMerchantId = a}) .
      mapping _Coerce

-- | The country for which to get the datafeed status. If this parameter is
-- provided then language must also be provided. Note that for multi-target
-- datafeeds this parameter is required.
dCountry :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Text)
dCountry = lens _dCountry (\ s a -> s{_dCountry = a})

dMethod :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Text)
dMethod = lens _dMethod (\ s a -> s{_dMethod = a})

-- | The ID of the data feed to get.
dDatafeedId :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Word64)
dDatafeedId
  = lens _dDatafeedId (\ s a -> s{_dDatafeedId = a}) .
      mapping _Coerce

-- | The language for which to get the datafeed status. If this parameter is
-- provided then country must also be provided. Note that for multi-target
-- datafeeds this parameter is required.
dLanguage :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Text)
dLanguage
  = lens _dLanguage (\ s a -> s{_dLanguage = a})

-- | An entry ID, unique within the batch request.
dBatchId :: Lens' DatafeedstatusesCustomBatchRequestEntry (Maybe Word32)
dBatchId
  = lens _dBatchId (\ s a -> s{_dBatchId = a}) .
      mapping _Coerce

instance FromJSON
         DatafeedstatusesCustomBatchRequestEntry where
        parseJSON
          = withObject
              "DatafeedstatusesCustomBatchRequestEntry"
              (\ o ->
                 DatafeedstatusesCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "country") <*>
                     (o .:? "method")
                     <*> (o .:? "datafeedId")
                     <*> (o .:? "language")
                     <*> (o .:? "batchId"))

instance ToJSON
         DatafeedstatusesCustomBatchRequestEntry where
        toJSON DatafeedstatusesCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _dMerchantId,
                  ("country" .=) <$> _dCountry,
                  ("method" .=) <$> _dMethod,
                  ("datafeedId" .=) <$> _dDatafeedId,
                  ("language" .=) <$> _dLanguage,
                  ("batchId" .=) <$> _dBatchId])

--
-- /See:/ 'orderCustomer' smart constructor.
data OrderCustomer = OrderCustomer'
    { _ocFullName :: !(Maybe Text)
    , _ocEmail :: !(Maybe Text)
    , _ocExplicitMarketingPreference :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderCustomer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocFullName'
--
-- * 'ocEmail'
--
-- * 'ocExplicitMarketingPreference'
orderCustomer
    :: OrderCustomer
orderCustomer = 
    OrderCustomer'
    { _ocFullName = Nothing
    , _ocEmail = Nothing
    , _ocExplicitMarketingPreference = Nothing
    }

-- | Full name of the customer.
ocFullName :: Lens' OrderCustomer (Maybe Text)
ocFullName
  = lens _ocFullName (\ s a -> s{_ocFullName = a})

-- | Email address of the customer.
ocEmail :: Lens' OrderCustomer (Maybe Text)
ocEmail = lens _ocEmail (\ s a -> s{_ocEmail = a})

-- | If set, this indicates the user explicitly chose to opt in or out of
-- providing marketing rights to the merchant. If unset, this indicates the
-- user has already made this choice in a previous purchase, and was thus
-- not shown the marketing right opt in\/out checkbox during the checkout
-- flow.
ocExplicitMarketingPreference :: Lens' OrderCustomer (Maybe Bool)
ocExplicitMarketingPreference
  = lens _ocExplicitMarketingPreference
      (\ s a -> s{_ocExplicitMarketingPreference = a})

instance FromJSON OrderCustomer where
        parseJSON
          = withObject "OrderCustomer"
              (\ o ->
                 OrderCustomer' <$>
                   (o .:? "fullName") <*> (o .:? "email") <*>
                     (o .:? "explicitMarketingPreference"))

instance ToJSON OrderCustomer where
        toJSON OrderCustomer'{..}
          = object
              (catMaybes
                 [("fullName" .=) <$> _ocFullName,
                  ("email" .=) <$> _ocEmail,
                  ("explicitMarketingPreference" .=) <$>
                    _ocExplicitMarketingPreference])

-- | A batch entry encoding a single non-batch inventory response.
--
-- /See:/ 'inventoryCustomBatchResponseEntry' smart constructor.
data InventoryCustomBatchResponseEntry = InventoryCustomBatchResponseEntry'
    { _icbreKind :: !Text
    , _icbreErrors :: !(Maybe Errors)
    , _icbreBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icbreKind'
--
-- * 'icbreErrors'
--
-- * 'icbreBatchId'
inventoryCustomBatchResponseEntry
    :: InventoryCustomBatchResponseEntry
inventoryCustomBatchResponseEntry = 
    InventoryCustomBatchResponseEntry'
    { _icbreKind = "content#inventoryCustomBatchResponseEntry"
    , _icbreErrors = Nothing
    , _icbreBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#inventoryCustomBatchResponseEntry\".
icbreKind :: Lens' InventoryCustomBatchResponseEntry Text
icbreKind
  = lens _icbreKind (\ s a -> s{_icbreKind = a})

-- | A list of errors defined if and only if the request failed.
icbreErrors :: Lens' InventoryCustomBatchResponseEntry (Maybe Errors)
icbreErrors
  = lens _icbreErrors (\ s a -> s{_icbreErrors = a})

-- | The ID of the request entry this entry responds to.
icbreBatchId :: Lens' InventoryCustomBatchResponseEntry (Maybe Word32)
icbreBatchId
  = lens _icbreBatchId (\ s a -> s{_icbreBatchId = a})
      . mapping _Coerce

instance FromJSON InventoryCustomBatchResponseEntry
         where
        parseJSON
          = withObject "InventoryCustomBatchResponseEntry"
              (\ o ->
                 InventoryCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#inventoryCustomBatchResponseEntry")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON InventoryCustomBatchResponseEntry
         where
        toJSON InventoryCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _icbreKind),
                  ("errors" .=) <$> _icbreErrors,
                  ("batchId" .=) <$> _icbreBatchId])

--
-- /See:/ 'locationIdSet' smart constructor.
newtype LocationIdSet = LocationIdSet'
    { _lisLocationIds :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LocationIdSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisLocationIds'
locationIdSet
    :: LocationIdSet
locationIdSet = 
    LocationIdSet'
    { _lisLocationIds = Nothing
    }

-- | A non-empty list of location IDs. They must all be of the same location
-- type (e.g., state).
lisLocationIds :: Lens' LocationIdSet [Text]
lisLocationIds
  = lens _lisLocationIds
      (\ s a -> s{_lisLocationIds = a})
      . _Default
      . _Coerce

instance FromJSON LocationIdSet where
        parseJSON
          = withObject "LocationIdSet"
              (\ o ->
                 LocationIdSet' <$> (o .:? "locationIds" .!= mempty))

instance ToJSON LocationIdSet where
        toJSON LocationIdSet'{..}
          = object
              (catMaybes [("locationIds" .=) <$> _lisLocationIds])

--
-- /See:/ 'row' smart constructor.
newtype Row = Row'
    { _rCells :: Maybe [Value]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Row' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rCells'
row
    :: Row
row = 
    Row'
    { _rCells = Nothing
    }

-- | The list of cells that constitute the row. Must have the same length as
-- columnHeaders for two-dimensional tables, a length of 1 for
-- one-dimensional tables. Required.
rCells :: Lens' Row [Value]
rCells
  = lens _rCells (\ s a -> s{_rCells = a}) . _Default .
      _Coerce

instance FromJSON Row where
        parseJSON
          = withObject "Row"
              (\ o -> Row' <$> (o .:? "cells" .!= mempty))

instance ToJSON Row where
        toJSON Row'{..}
          = object (catMaybes [("cells" .=) <$> _rCells])

--
-- /See:/ 'inventory' smart constructor.
data Inventory = Inventory'
    { _iLoyaltyPoints :: !(Maybe LoyaltyPoints)
    , _iKind :: !Text
    , _iQuantity :: !(Maybe (Textual Word32))
    , _iInstallment :: !(Maybe Installment)
    , _iSalePrice :: !(Maybe Price)
    , _iAvailability :: !(Maybe Text)
    , _iPickup :: !(Maybe InventoryPickup)
    , _iSalePriceEffectiveDate :: !(Maybe Text)
    , _iSellOnGoogleQuantity :: !(Maybe (Textual Word32))
    , _iPrice :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Inventory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLoyaltyPoints'
--
-- * 'iKind'
--
-- * 'iQuantity'
--
-- * 'iInstallment'
--
-- * 'iSalePrice'
--
-- * 'iAvailability'
--
-- * 'iPickup'
--
-- * 'iSalePriceEffectiveDate'
--
-- * 'iSellOnGoogleQuantity'
--
-- * 'iPrice'
inventory
    :: Inventory
inventory = 
    Inventory'
    { _iLoyaltyPoints = Nothing
    , _iKind = "content#inventory"
    , _iQuantity = Nothing
    , _iInstallment = Nothing
    , _iSalePrice = Nothing
    , _iAvailability = Nothing
    , _iPickup = Nothing
    , _iSalePriceEffectiveDate = Nothing
    , _iSellOnGoogleQuantity = Nothing
    , _iPrice = Nothing
    }

-- | Loyalty points that users receive after purchasing the item. Japan only.
iLoyaltyPoints :: Lens' Inventory (Maybe LoyaltyPoints)
iLoyaltyPoints
  = lens _iLoyaltyPoints
      (\ s a -> s{_iLoyaltyPoints = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#inventory\".
iKind :: Lens' Inventory Text
iKind = lens _iKind (\ s a -> s{_iKind = a})

-- | The quantity of the product. Must be equal to or greater than zero.
-- Supported only for local products.
iQuantity :: Lens' Inventory (Maybe Word32)
iQuantity
  = lens _iQuantity (\ s a -> s{_iQuantity = a}) .
      mapping _Coerce

-- | Number and amount of installments to pay for an item. Brazil only.
iInstallment :: Lens' Inventory (Maybe Installment)
iInstallment
  = lens _iInstallment (\ s a -> s{_iInstallment = a})

-- | The sale price of the product. Mandatory if sale_price_effective_date is
-- defined.
iSalePrice :: Lens' Inventory (Maybe Price)
iSalePrice
  = lens _iSalePrice (\ s a -> s{_iSalePrice = a})

-- | The availability of the product.
iAvailability :: Lens' Inventory (Maybe Text)
iAvailability
  = lens _iAvailability
      (\ s a -> s{_iAvailability = a})

-- | Store pickup information. Only supported for local inventory. Not
-- setting pickup means \"don\'t update\" while setting it to the empty
-- value ({} in JSON) means \"delete\". Otherwise, pickupMethod and
-- pickupSla must be set together, unless pickupMethod is \"not
-- supported\".
iPickup :: Lens' Inventory (Maybe InventoryPickup)
iPickup = lens _iPickup (\ s a -> s{_iPickup = a})

-- | A date range represented by a pair of ISO 8601 dates separated by a
-- space, comma, or slash. Both dates might be specified as \'null\' if
-- undecided.
iSalePriceEffectiveDate :: Lens' Inventory (Maybe Text)
iSalePriceEffectiveDate
  = lens _iSalePriceEffectiveDate
      (\ s a -> s{_iSalePriceEffectiveDate = a})

-- | The quantity of the product that is reserved for sell-on-google ads.
-- Supported only for online products.
iSellOnGoogleQuantity :: Lens' Inventory (Maybe Word32)
iSellOnGoogleQuantity
  = lens _iSellOnGoogleQuantity
      (\ s a -> s{_iSellOnGoogleQuantity = a})
      . mapping _Coerce

-- | The price of the product.
iPrice :: Lens' Inventory (Maybe Price)
iPrice = lens _iPrice (\ s a -> s{_iPrice = a})

instance FromJSON Inventory where
        parseJSON
          = withObject "Inventory"
              (\ o ->
                 Inventory' <$>
                   (o .:? "loyaltyPoints") <*>
                     (o .:? "kind" .!= "content#inventory")
                     <*> (o .:? "quantity")
                     <*> (o .:? "installment")
                     <*> (o .:? "salePrice")
                     <*> (o .:? "availability")
                     <*> (o .:? "pickup")
                     <*> (o .:? "salePriceEffectiveDate")
                     <*> (o .:? "sellOnGoogleQuantity")
                     <*> (o .:? "price"))

instance ToJSON Inventory where
        toJSON Inventory'{..}
          = object
              (catMaybes
                 [("loyaltyPoints" .=) <$> _iLoyaltyPoints,
                  Just ("kind" .= _iKind),
                  ("quantity" .=) <$> _iQuantity,
                  ("installment" .=) <$> _iInstallment,
                  ("salePrice" .=) <$> _iSalePrice,
                  ("availability" .=) <$> _iAvailability,
                  ("pickup" .=) <$> _iPickup,
                  ("salePriceEffectiveDate" .=) <$>
                    _iSalePriceEffectiveDate,
                  ("sellOnGoogleQuantity" .=) <$>
                    _iSellOnGoogleQuantity,
                  ("price" .=) <$> _iPrice])

--
-- /See:/ 'ordersGetByMerchantOrderIdResponse' smart constructor.
data OrdersGetByMerchantOrderIdResponse = OrdersGetByMerchantOrderIdResponse'
    { _ogbmoirKind :: !Text
    , _ogbmoirOrder :: !(Maybe Order)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersGetByMerchantOrderIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogbmoirKind'
--
-- * 'ogbmoirOrder'
ordersGetByMerchantOrderIdResponse
    :: OrdersGetByMerchantOrderIdResponse
ordersGetByMerchantOrderIdResponse = 
    OrdersGetByMerchantOrderIdResponse'
    { _ogbmoirKind = "content#ordersGetByMerchantOrderIdResponse"
    , _ogbmoirOrder = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersGetByMerchantOrderIdResponse\".
ogbmoirKind :: Lens' OrdersGetByMerchantOrderIdResponse Text
ogbmoirKind
  = lens _ogbmoirKind (\ s a -> s{_ogbmoirKind = a})

-- | The requested order.
ogbmoirOrder :: Lens' OrdersGetByMerchantOrderIdResponse (Maybe Order)
ogbmoirOrder
  = lens _ogbmoirOrder (\ s a -> s{_ogbmoirOrder = a})

instance FromJSON OrdersGetByMerchantOrderIdResponse
         where
        parseJSON
          = withObject "OrdersGetByMerchantOrderIdResponse"
              (\ o ->
                 OrdersGetByMerchantOrderIdResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersGetByMerchantOrderIdResponse")
                     <*> (o .:? "order"))

instance ToJSON OrdersGetByMerchantOrderIdResponse
         where
        toJSON OrdersGetByMerchantOrderIdResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ogbmoirKind),
                  ("order" .=) <$> _ogbmoirOrder])

--
-- /See:/ 'orderPromotionBenefit' smart constructor.
data OrderPromotionBenefit = OrderPromotionBenefit'
    { _opbTaxImpact :: !(Maybe Price)
    , _opbDiscount :: !(Maybe Price)
    , _opbOfferIds :: !(Maybe [Text])
    , _opbSubType :: !(Maybe Text)
    , _opbType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderPromotionBenefit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opbTaxImpact'
--
-- * 'opbDiscount'
--
-- * 'opbOfferIds'
--
-- * 'opbSubType'
--
-- * 'opbType'
orderPromotionBenefit
    :: OrderPromotionBenefit
orderPromotionBenefit = 
    OrderPromotionBenefit'
    { _opbTaxImpact = Nothing
    , _opbDiscount = Nothing
    , _opbOfferIds = Nothing
    , _opbSubType = Nothing
    , _opbType = Nothing
    }

-- | The impact on tax when the promotion is applied.
opbTaxImpact :: Lens' OrderPromotionBenefit (Maybe Price)
opbTaxImpact
  = lens _opbTaxImpact (\ s a -> s{_opbTaxImpact = a})

-- | The discount in the order price when the promotion is applied.
opbDiscount :: Lens' OrderPromotionBenefit (Maybe Price)
opbDiscount
  = lens _opbDiscount (\ s a -> s{_opbDiscount = a})

-- | The OfferId(s) that were purchased in this order and map to this
-- specific benefit of the promotion.
opbOfferIds :: Lens' OrderPromotionBenefit [Text]
opbOfferIds
  = lens _opbOfferIds (\ s a -> s{_opbOfferIds = a}) .
      _Default
      . _Coerce

-- | Further describes the benefit of the promotion. Note that we will expand
-- on this enumeration as we support new promotion sub-types.
opbSubType :: Lens' OrderPromotionBenefit (Maybe Text)
opbSubType
  = lens _opbSubType (\ s a -> s{_opbSubType = a})

-- | Describes whether the promotion applies to products (e.g. 20% off) or to
-- shipping (e.g. Free Shipping).
opbType :: Lens' OrderPromotionBenefit (Maybe Text)
opbType = lens _opbType (\ s a -> s{_opbType = a})

instance FromJSON OrderPromotionBenefit where
        parseJSON
          = withObject "OrderPromotionBenefit"
              (\ o ->
                 OrderPromotionBenefit' <$>
                   (o .:? "taxImpact") <*> (o .:? "discount") <*>
                     (o .:? "offerIds" .!= mempty)
                     <*> (o .:? "subType")
                     <*> (o .:? "type"))

instance ToJSON OrderPromotionBenefit where
        toJSON OrderPromotionBenefit'{..}
          = object
              (catMaybes
                 [("taxImpact" .=) <$> _opbTaxImpact,
                  ("discount" .=) <$> _opbDiscount,
                  ("offerIds" .=) <$> _opbOfferIds,
                  ("subType" .=) <$> _opbSubType,
                  ("type" .=) <$> _opbType])

--
-- /See:/ 'ordersRejectReturnLineItemRequest' smart constructor.
data OrdersRejectReturnLineItemRequest = OrdersRejectReturnLineItemRequest'
    { _orrlirQuantity :: !(Maybe (Textual Word32))
    , _orrlirLineItemId :: !(Maybe Text)
    , _orrlirReason :: !(Maybe Text)
    , _orrlirOperationId :: !(Maybe Text)
    , _orrlirProductId :: !(Maybe Text)
    , _orrlirReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersRejectReturnLineItemRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrlirQuantity'
--
-- * 'orrlirLineItemId'
--
-- * 'orrlirReason'
--
-- * 'orrlirOperationId'
--
-- * 'orrlirProductId'
--
-- * 'orrlirReasonText'
ordersRejectReturnLineItemRequest
    :: OrdersRejectReturnLineItemRequest
ordersRejectReturnLineItemRequest = 
    OrdersRejectReturnLineItemRequest'
    { _orrlirQuantity = Nothing
    , _orrlirLineItemId = Nothing
    , _orrlirReason = Nothing
    , _orrlirOperationId = Nothing
    , _orrlirProductId = Nothing
    , _orrlirReasonText = Nothing
    }

-- | The quantity to return and refund.
orrlirQuantity :: Lens' OrdersRejectReturnLineItemRequest (Maybe Word32)
orrlirQuantity
  = lens _orrlirQuantity
      (\ s a -> s{_orrlirQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
orrlirLineItemId :: Lens' OrdersRejectReturnLineItemRequest (Maybe Text)
orrlirLineItemId
  = lens _orrlirLineItemId
      (\ s a -> s{_orrlirLineItemId = a})

-- | The reason for the return.
orrlirReason :: Lens' OrdersRejectReturnLineItemRequest (Maybe Text)
orrlirReason
  = lens _orrlirReason (\ s a -> s{_orrlirReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
orrlirOperationId :: Lens' OrdersRejectReturnLineItemRequest (Maybe Text)
orrlirOperationId
  = lens _orrlirOperationId
      (\ s a -> s{_orrlirOperationId = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
orrlirProductId :: Lens' OrdersRejectReturnLineItemRequest (Maybe Text)
orrlirProductId
  = lens _orrlirProductId
      (\ s a -> s{_orrlirProductId = a})

-- | The explanation of the reason.
orrlirReasonText :: Lens' OrdersRejectReturnLineItemRequest (Maybe Text)
orrlirReasonText
  = lens _orrlirReasonText
      (\ s a -> s{_orrlirReasonText = a})

instance FromJSON OrdersRejectReturnLineItemRequest
         where
        parseJSON
          = withObject "OrdersRejectReturnLineItemRequest"
              (\ o ->
                 OrdersRejectReturnLineItemRequest' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "operationId")
                     <*> (o .:? "productId")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersRejectReturnLineItemRequest
         where
        toJSON OrdersRejectReturnLineItemRequest'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _orrlirQuantity,
                  ("lineItemId" .=) <$> _orrlirLineItemId,
                  ("reason" .=) <$> _orrlirReason,
                  ("operationId" .=) <$> _orrlirOperationId,
                  ("productId" .=) <$> _orrlirProductId,
                  ("reasonText" .=) <$> _orrlirReasonText])

--
-- /See:/ 'ordersCustomBatchRequestEntryInStoreRefundLineItem' smart constructor.
data OrdersCustomBatchRequestEntryInStoreRefundLineItem = OrdersCustomBatchRequestEntryInStoreRefundLineItem'
    { _ocbreisrliQuantity :: !(Maybe (Textual Word32))
    , _ocbreisrliLineItemId :: !(Maybe Text)
    , _ocbreisrliReason :: !(Maybe Text)
    , _ocbreisrliAmountPretax :: !(Maybe Price)
    , _ocbreisrliProductId :: !(Maybe Text)
    , _ocbreisrliAmountTax :: !(Maybe Price)
    , _ocbreisrliReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryInStoreRefundLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreisrliQuantity'
--
-- * 'ocbreisrliLineItemId'
--
-- * 'ocbreisrliReason'
--
-- * 'ocbreisrliAmountPretax'
--
-- * 'ocbreisrliProductId'
--
-- * 'ocbreisrliAmountTax'
--
-- * 'ocbreisrliReasonText'
ordersCustomBatchRequestEntryInStoreRefundLineItem
    :: OrdersCustomBatchRequestEntryInStoreRefundLineItem
ordersCustomBatchRequestEntryInStoreRefundLineItem = 
    OrdersCustomBatchRequestEntryInStoreRefundLineItem'
    { _ocbreisrliQuantity = Nothing
    , _ocbreisrliLineItemId = Nothing
    , _ocbreisrliReason = Nothing
    , _ocbreisrliAmountPretax = Nothing
    , _ocbreisrliProductId = Nothing
    , _ocbreisrliAmountTax = Nothing
    , _ocbreisrliReasonText = Nothing
    }

-- | The quantity to return and refund.
ocbreisrliQuantity :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Word32)
ocbreisrliQuantity
  = lens _ocbreisrliQuantity
      (\ s a -> s{_ocbreisrliQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
ocbreisrliLineItemId :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Text)
ocbreisrliLineItemId
  = lens _ocbreisrliLineItemId
      (\ s a -> s{_ocbreisrliLineItemId = a})

-- | The reason for the return.
ocbreisrliReason :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Text)
ocbreisrliReason
  = lens _ocbreisrliReason
      (\ s a -> s{_ocbreisrliReason = a})

-- | The amount that is refunded. Required.
ocbreisrliAmountPretax :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Price)
ocbreisrliAmountPretax
  = lens _ocbreisrliAmountPretax
      (\ s a -> s{_ocbreisrliAmountPretax = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbreisrliProductId :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Text)
ocbreisrliProductId
  = lens _ocbreisrliProductId
      (\ s a -> s{_ocbreisrliProductId = a})

-- | Tax amount that correspond to refund amount in amountPretax. Required.
ocbreisrliAmountTax :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Price)
ocbreisrliAmountTax
  = lens _ocbreisrliAmountTax
      (\ s a -> s{_ocbreisrliAmountTax = a})

-- | The explanation of the reason.
ocbreisrliReasonText :: Lens' OrdersCustomBatchRequestEntryInStoreRefundLineItem (Maybe Text)
ocbreisrliReasonText
  = lens _ocbreisrliReasonText
      (\ s a -> s{_ocbreisrliReasonText = a})

instance FromJSON
         OrdersCustomBatchRequestEntryInStoreRefundLineItem
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryInStoreRefundLineItem"
              (\ o ->
                 OrdersCustomBatchRequestEntryInStoreRefundLineItem'
                   <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON
         OrdersCustomBatchRequestEntryInStoreRefundLineItem
         where
        toJSON
          OrdersCustomBatchRequestEntryInStoreRefundLineItem'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _ocbreisrliQuantity,
                  ("lineItemId" .=) <$> _ocbreisrliLineItemId,
                  ("reason" .=) <$> _ocbreisrliReason,
                  ("amountPretax" .=) <$> _ocbreisrliAmountPretax,
                  ("productId" .=) <$> _ocbreisrliProductId,
                  ("amountTax" .=) <$> _ocbreisrliAmountTax,
                  ("reasonText" .=) <$> _ocbreisrliReasonText])

--
-- /See:/ 'ordersCancelRequest' smart constructor.
data OrdersCancelRequest = OrdersCancelRequest'
    { _ocrReason :: !(Maybe Text)
    , _ocrOperationId :: !(Maybe Text)
    , _ocrReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrReason'
--
-- * 'ocrOperationId'
--
-- * 'ocrReasonText'
ordersCancelRequest
    :: OrdersCancelRequest
ordersCancelRequest = 
    OrdersCancelRequest'
    { _ocrReason = Nothing
    , _ocrOperationId = Nothing
    , _ocrReasonText = Nothing
    }

-- | The reason for the cancellation.
ocrReason :: Lens' OrdersCancelRequest (Maybe Text)
ocrReason
  = lens _ocrReason (\ s a -> s{_ocrReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
ocrOperationId :: Lens' OrdersCancelRequest (Maybe Text)
ocrOperationId
  = lens _ocrOperationId
      (\ s a -> s{_ocrOperationId = a})

-- | The explanation of the reason.
ocrReasonText :: Lens' OrdersCancelRequest (Maybe Text)
ocrReasonText
  = lens _ocrReasonText
      (\ s a -> s{_ocrReasonText = a})

instance FromJSON OrdersCancelRequest where
        parseJSON
          = withObject "OrdersCancelRequest"
              (\ o ->
                 OrdersCancelRequest' <$>
                   (o .:? "reason") <*> (o .:? "operationId") <*>
                     (o .:? "reasonText"))

instance ToJSON OrdersCancelRequest where
        toJSON OrdersCancelRequest'{..}
          = object
              (catMaybes
                 [("reason" .=) <$> _ocrReason,
                  ("operationId" .=) <$> _ocrOperationId,
                  ("reasonText" .=) <$> _ocrReasonText])

--
-- /See:/ 'productStatusItemLevelIssue' smart constructor.
data ProductStatusItemLevelIssue = ProductStatusItemLevelIssue'
    { _psiliDestination :: !(Maybe Text)
    , _psiliResolution :: !(Maybe Text)
    , _psiliCode :: !(Maybe Text)
    , _psiliServability :: !(Maybe Text)
    , _psiliAttributeName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductStatusItemLevelIssue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psiliDestination'
--
-- * 'psiliResolution'
--
-- * 'psiliCode'
--
-- * 'psiliServability'
--
-- * 'psiliAttributeName'
productStatusItemLevelIssue
    :: ProductStatusItemLevelIssue
productStatusItemLevelIssue = 
    ProductStatusItemLevelIssue'
    { _psiliDestination = Nothing
    , _psiliResolution = Nothing
    , _psiliCode = Nothing
    , _psiliServability = Nothing
    , _psiliAttributeName = Nothing
    }

-- | The destination the issue applies to.
psiliDestination :: Lens' ProductStatusItemLevelIssue (Maybe Text)
psiliDestination
  = lens _psiliDestination
      (\ s a -> s{_psiliDestination = a})

-- | Whether the issue can be resolved by the merchant.
psiliResolution :: Lens' ProductStatusItemLevelIssue (Maybe Text)
psiliResolution
  = lens _psiliResolution
      (\ s a -> s{_psiliResolution = a})

-- | The error code of the issue.
psiliCode :: Lens' ProductStatusItemLevelIssue (Maybe Text)
psiliCode
  = lens _psiliCode (\ s a -> s{_psiliCode = a})

-- | How this issue affects serving of the offer.
psiliServability :: Lens' ProductStatusItemLevelIssue (Maybe Text)
psiliServability
  = lens _psiliServability
      (\ s a -> s{_psiliServability = a})

-- | The attribute\'s name, if the issue is caused by a single attribute.
psiliAttributeName :: Lens' ProductStatusItemLevelIssue (Maybe Text)
psiliAttributeName
  = lens _psiliAttributeName
      (\ s a -> s{_psiliAttributeName = a})

instance FromJSON ProductStatusItemLevelIssue where
        parseJSON
          = withObject "ProductStatusItemLevelIssue"
              (\ o ->
                 ProductStatusItemLevelIssue' <$>
                   (o .:? "destination") <*> (o .:? "resolution") <*>
                     (o .:? "code")
                     <*> (o .:? "servability")
                     <*> (o .:? "attributeName"))

instance ToJSON ProductStatusItemLevelIssue where
        toJSON ProductStatusItemLevelIssue'{..}
          = object
              (catMaybes
                 [("destination" .=) <$> _psiliDestination,
                  ("resolution" .=) <$> _psiliResolution,
                  ("code" .=) <$> _psiliCode,
                  ("servability" .=) <$> _psiliServability,
                  ("attributeName" .=) <$> _psiliAttributeName])

--
-- /See:/ 'orderLineItemProductVariantAttribute' smart constructor.
data OrderLineItemProductVariantAttribute = OrderLineItemProductVariantAttribute'
    { _olipvaDimension :: !(Maybe Text)
    , _olipvaValue :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItemProductVariantAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olipvaDimension'
--
-- * 'olipvaValue'
orderLineItemProductVariantAttribute
    :: OrderLineItemProductVariantAttribute
orderLineItemProductVariantAttribute = 
    OrderLineItemProductVariantAttribute'
    { _olipvaDimension = Nothing
    , _olipvaValue = Nothing
    }

-- | The dimension of the variant.
olipvaDimension :: Lens' OrderLineItemProductVariantAttribute (Maybe Text)
olipvaDimension
  = lens _olipvaDimension
      (\ s a -> s{_olipvaDimension = a})

-- | The value for the dimension.
olipvaValue :: Lens' OrderLineItemProductVariantAttribute (Maybe Text)
olipvaValue
  = lens _olipvaValue (\ s a -> s{_olipvaValue = a})

instance FromJSON
         OrderLineItemProductVariantAttribute where
        parseJSON
          = withObject "OrderLineItemProductVariantAttribute"
              (\ o ->
                 OrderLineItemProductVariantAttribute' <$>
                   (o .:? "dimension") <*> (o .:? "value"))

instance ToJSON OrderLineItemProductVariantAttribute
         where
        toJSON OrderLineItemProductVariantAttribute'{..}
          = object
              (catMaybes
                 [("dimension" .=) <$> _olipvaDimension,
                  ("value" .=) <$> _olipvaValue])

--
-- /See:/ 'ordersCustomBatchResponseEntry' smart constructor.
data OrdersCustomBatchResponseEntry = OrdersCustomBatchResponseEntry'
    { _oKind :: !Text
    , _oExecutionStatus :: !(Maybe Text)
    , _oErrors :: !(Maybe Errors)
    , _oOrder :: !(Maybe Order)
    , _oBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oKind'
--
-- * 'oExecutionStatus'
--
-- * 'oErrors'
--
-- * 'oOrder'
--
-- * 'oBatchId'
ordersCustomBatchResponseEntry
    :: OrdersCustomBatchResponseEntry
ordersCustomBatchResponseEntry = 
    OrdersCustomBatchResponseEntry'
    { _oKind = "content#ordersCustomBatchResponseEntry"
    , _oExecutionStatus = Nothing
    , _oErrors = Nothing
    , _oOrder = Nothing
    , _oBatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersCustomBatchResponseEntry\".
oKind :: Lens' OrdersCustomBatchResponseEntry Text
oKind = lens _oKind (\ s a -> s{_oKind = a})

-- | The status of the execution. Only defined if the method is not get or
-- getByMerchantOrderId and if the request was successful.
oExecutionStatus :: Lens' OrdersCustomBatchResponseEntry (Maybe Text)
oExecutionStatus
  = lens _oExecutionStatus
      (\ s a -> s{_oExecutionStatus = a})

-- | A list of errors defined if and only if the request failed.
oErrors :: Lens' OrdersCustomBatchResponseEntry (Maybe Errors)
oErrors = lens _oErrors (\ s a -> s{_oErrors = a})

-- | The retrieved order. Only defined if the method is get and if the
-- request was successful.
oOrder :: Lens' OrdersCustomBatchResponseEntry (Maybe Order)
oOrder = lens _oOrder (\ s a -> s{_oOrder = a})

-- | The ID of the request entry this entry responds to.
oBatchId :: Lens' OrdersCustomBatchResponseEntry (Maybe Word32)
oBatchId
  = lens _oBatchId (\ s a -> s{_oBatchId = a}) .
      mapping _Coerce

instance FromJSON OrdersCustomBatchResponseEntry
         where
        parseJSON
          = withObject "OrdersCustomBatchResponseEntry"
              (\ o ->
                 OrdersCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#ordersCustomBatchResponseEntry")
                     <*> (o .:? "executionStatus")
                     <*> (o .:? "errors")
                     <*> (o .:? "order")
                     <*> (o .:? "batchId"))

instance ToJSON OrdersCustomBatchResponseEntry where
        toJSON OrdersCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oKind),
                  ("executionStatus" .=) <$> _oExecutionStatus,
                  ("errors" .=) <$> _oErrors, ("order" .=) <$> _oOrder,
                  ("batchId" .=) <$> _oBatchId])

--
-- /See:/ 'rateGroup' smart constructor.
data RateGroup = RateGroup'
    { _rgCarrierRates :: !(Maybe [CarrierRate])
    , _rgApplicableShippingLabels :: !(Maybe [Text])
    , _rgMainTable :: !(Maybe Table)
    , _rgSingleValue :: !(Maybe Value)
    , _rgSubtables :: !(Maybe [Table])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgCarrierRates'
--
-- * 'rgApplicableShippingLabels'
--
-- * 'rgMainTable'
--
-- * 'rgSingleValue'
--
-- * 'rgSubtables'
rateGroup
    :: RateGroup
rateGroup = 
    RateGroup'
    { _rgCarrierRates = Nothing
    , _rgApplicableShippingLabels = Nothing
    , _rgMainTable = Nothing
    , _rgSingleValue = Nothing
    , _rgSubtables = Nothing
    }

-- | A list of carrier rates that can be referred to by mainTable or
-- singleValue.
rgCarrierRates :: Lens' RateGroup [CarrierRate]
rgCarrierRates
  = lens _rgCarrierRates
      (\ s a -> s{_rgCarrierRates = a})
      . _Default
      . _Coerce

-- | A list of shipping labels defining the products to which this rate group
-- applies to. This is a disjunction: only one of the labels has to match
-- for the rate group to apply. May only be empty for the last rate group
-- of a service. Required.
rgApplicableShippingLabels :: Lens' RateGroup [Text]
rgApplicableShippingLabels
  = lens _rgApplicableShippingLabels
      (\ s a -> s{_rgApplicableShippingLabels = a})
      . _Default
      . _Coerce

-- | A table defining the rate group, when singleValue is not expressive
-- enough. Can only be set if singleValue is not set.
rgMainTable :: Lens' RateGroup (Maybe Table)
rgMainTable
  = lens _rgMainTable (\ s a -> s{_rgMainTable = a})

-- | The value of the rate group (e.g. flat rate $10). Can only be set if
-- mainTable and subtables are not set.
rgSingleValue :: Lens' RateGroup (Maybe Value)
rgSingleValue
  = lens _rgSingleValue
      (\ s a -> s{_rgSingleValue = a})

-- | A list of subtables referred to by mainTable. Can only be set if
-- mainTable is set.
rgSubtables :: Lens' RateGroup [Table]
rgSubtables
  = lens _rgSubtables (\ s a -> s{_rgSubtables = a}) .
      _Default
      . _Coerce

instance FromJSON RateGroup where
        parseJSON
          = withObject "RateGroup"
              (\ o ->
                 RateGroup' <$>
                   (o .:? "carrierRates" .!= mempty) <*>
                     (o .:? "applicableShippingLabels" .!= mempty)
                     <*> (o .:? "mainTable")
                     <*> (o .:? "singleValue")
                     <*> (o .:? "subtables" .!= mempty))

instance ToJSON RateGroup where
        toJSON RateGroup'{..}
          = object
              (catMaybes
                 [("carrierRates" .=) <$> _rgCarrierRates,
                  ("applicableShippingLabels" .=) <$>
                    _rgApplicableShippingLabels,
                  ("mainTable" .=) <$> _rgMainTable,
                  ("singleValue" .=) <$> _rgSingleValue,
                  ("subtables" .=) <$> _rgSubtables])

--
-- /See:/ 'orderPromotion' smart constructor.
data OrderPromotion = OrderPromotion'
    { _opEffectiveDates :: !(Maybe Text)
    , _opGenericRedemptionCode :: !(Maybe Text)
    , _opRedemptionChannel :: !(Maybe Text)
    , _opBenefits :: !(Maybe [OrderPromotionBenefit])
    , _opLongTitle :: !(Maybe Text)
    , _opId :: !(Maybe Text)
    , _opProductApplicability :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderPromotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opEffectiveDates'
--
-- * 'opGenericRedemptionCode'
--
-- * 'opRedemptionChannel'
--
-- * 'opBenefits'
--
-- * 'opLongTitle'
--
-- * 'opId'
--
-- * 'opProductApplicability'
orderPromotion
    :: OrderPromotion
orderPromotion = 
    OrderPromotion'
    { _opEffectiveDates = Nothing
    , _opGenericRedemptionCode = Nothing
    , _opRedemptionChannel = Nothing
    , _opBenefits = Nothing
    , _opLongTitle = Nothing
    , _opId = Nothing
    , _opProductApplicability = Nothing
    }

-- | The date and time frame when the promotion is active and ready for
-- validation review. Note that the promotion live time may be delayed for
-- a few hours due to the validation review. Start date and end date are
-- separated by a forward slash (\/). The start date is specified by the
-- format (YYYY-MM-DD), followed by the letter ?T?, the time of the day
-- when the sale starts (in Greenwich Mean Time, GMT), followed by an
-- expression of the time zone for the sale. The end date is in the same
-- format.
opEffectiveDates :: Lens' OrderPromotion (Maybe Text)
opEffectiveDates
  = lens _opEffectiveDates
      (\ s a -> s{_opEffectiveDates = a})

-- | Optional. The text code that corresponds to the promotion when applied
-- on the retailer?s website.
opGenericRedemptionCode :: Lens' OrderPromotion (Maybe Text)
opGenericRedemptionCode
  = lens _opGenericRedemptionCode
      (\ s a -> s{_opGenericRedemptionCode = a})

-- | Indicates that the promotion is valid online.
opRedemptionChannel :: Lens' OrderPromotion (Maybe Text)
opRedemptionChannel
  = lens _opRedemptionChannel
      (\ s a -> s{_opRedemptionChannel = a})

opBenefits :: Lens' OrderPromotion [OrderPromotionBenefit]
opBenefits
  = lens _opBenefits (\ s a -> s{_opBenefits = a}) .
      _Default
      . _Coerce

-- | The full title of the promotion.
opLongTitle :: Lens' OrderPromotion (Maybe Text)
opLongTitle
  = lens _opLongTitle (\ s a -> s{_opLongTitle = a})

-- | The unique ID of the promotion.
opId :: Lens' OrderPromotion (Maybe Text)
opId = lens _opId (\ s a -> s{_opId = a})

-- | Whether the promotion is applicable to all products or only specific
-- products.
opProductApplicability :: Lens' OrderPromotion (Maybe Text)
opProductApplicability
  = lens _opProductApplicability
      (\ s a -> s{_opProductApplicability = a})

instance FromJSON OrderPromotion where
        parseJSON
          = withObject "OrderPromotion"
              (\ o ->
                 OrderPromotion' <$>
                   (o .:? "effectiveDates") <*>
                     (o .:? "genericRedemptionCode")
                     <*> (o .:? "redemptionChannel")
                     <*> (o .:? "benefits" .!= mempty)
                     <*> (o .:? "longTitle")
                     <*> (o .:? "id")
                     <*> (o .:? "productApplicability"))

instance ToJSON OrderPromotion where
        toJSON OrderPromotion'{..}
          = object
              (catMaybes
                 [("effectiveDates" .=) <$> _opEffectiveDates,
                  ("genericRedemptionCode" .=) <$>
                    _opGenericRedemptionCode,
                  ("redemptionChannel" .=) <$> _opRedemptionChannel,
                  ("benefits" .=) <$> _opBenefits,
                  ("longTitle" .=) <$> _opLongTitle,
                  ("id" .=) <$> _opId,
                  ("productApplicability" .=) <$>
                    _opProductApplicability])

--
-- /See:/ 'price' smart constructor.
data Price = Price'
    { _pValue :: !(Maybe Text)
    , _pCurrency :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Price' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue'
--
-- * 'pCurrency'
price
    :: Price
price = 
    Price'
    { _pValue = Nothing
    , _pCurrency = Nothing
    }

-- | The price represented as a number.
pValue :: Lens' Price (Maybe Text)
pValue = lens _pValue (\ s a -> s{_pValue = a})

-- | The currency of the price.
pCurrency :: Lens' Price (Maybe Text)
pCurrency
  = lens _pCurrency (\ s a -> s{_pCurrency = a})

instance FromJSON Price where
        parseJSON
          = withObject "Price"
              (\ o ->
                 Price' <$> (o .:? "value") <*> (o .:? "currency"))

instance ToJSON Price where
        toJSON Price'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pValue,
                  ("currency" .=) <$> _pCurrency])

--
-- /See:/ 'orderLineItemShippingDetails' smart constructor.
data OrderLineItemShippingDetails = OrderLineItemShippingDetails'
    { _olisdShipByDate :: !(Maybe Text)
    , _olisdMethod :: !(Maybe OrderLineItemShippingDetailsMethod)
    , _olisdDeliverByDate :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItemShippingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olisdShipByDate'
--
-- * 'olisdMethod'
--
-- * 'olisdDeliverByDate'
orderLineItemShippingDetails
    :: OrderLineItemShippingDetails
orderLineItemShippingDetails = 
    OrderLineItemShippingDetails'
    { _olisdShipByDate = Nothing
    , _olisdMethod = Nothing
    , _olisdDeliverByDate = Nothing
    }

-- | The ship by date, in ISO 8601 format.
olisdShipByDate :: Lens' OrderLineItemShippingDetails (Maybe Text)
olisdShipByDate
  = lens _olisdShipByDate
      (\ s a -> s{_olisdShipByDate = a})

-- | Details of the shipping method.
olisdMethod :: Lens' OrderLineItemShippingDetails (Maybe OrderLineItemShippingDetailsMethod)
olisdMethod
  = lens _olisdMethod (\ s a -> s{_olisdMethod = a})

-- | The delivery by date, in ISO 8601 format.
olisdDeliverByDate :: Lens' OrderLineItemShippingDetails (Maybe Text)
olisdDeliverByDate
  = lens _olisdDeliverByDate
      (\ s a -> s{_olisdDeliverByDate = a})

instance FromJSON OrderLineItemShippingDetails where
        parseJSON
          = withObject "OrderLineItemShippingDetails"
              (\ o ->
                 OrderLineItemShippingDetails' <$>
                   (o .:? "shipByDate") <*> (o .:? "method") <*>
                     (o .:? "deliverByDate"))

instance ToJSON OrderLineItemShippingDetails where
        toJSON OrderLineItemShippingDetails'{..}
          = object
              (catMaybes
                 [("shipByDate" .=) <$> _olisdShipByDate,
                  ("method" .=) <$> _olisdMethod,
                  ("deliverByDate" .=) <$> _olisdDeliverByDate])

--
-- /See:/ 'datafeedsCustomBatchResponse' smart constructor.
data DatafeedsCustomBatchResponse = DatafeedsCustomBatchResponse'
    { _datEntries :: !(Maybe [DatafeedsCustomBatchResponseEntry])
    , _datKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datEntries'
--
-- * 'datKind'
datafeedsCustomBatchResponse
    :: DatafeedsCustomBatchResponse
datafeedsCustomBatchResponse = 
    DatafeedsCustomBatchResponse'
    { _datEntries = Nothing
    , _datKind = "content#datafeedsCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
datEntries :: Lens' DatafeedsCustomBatchResponse [DatafeedsCustomBatchResponseEntry]
datEntries
  = lens _datEntries (\ s a -> s{_datEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeedsCustomBatchResponse\".
datKind :: Lens' DatafeedsCustomBatchResponse Text
datKind = lens _datKind (\ s a -> s{_datKind = a})

instance FromJSON DatafeedsCustomBatchResponse where
        parseJSON
          = withObject "DatafeedsCustomBatchResponse"
              (\ o ->
                 DatafeedsCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#datafeedsCustomBatchResponse"))

instance ToJSON DatafeedsCustomBatchResponse where
        toJSON DatafeedsCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _datEntries,
                  Just ("kind" .= _datKind)])

--
-- /See:/ 'posListResponse' smart constructor.
data PosListResponse = PosListResponse'
    { _plrlKind :: !Text
    , _plrlResources :: !(Maybe [PosStore])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plrlKind'
--
-- * 'plrlResources'
posListResponse
    :: PosListResponse
posListResponse = 
    PosListResponse'
    { _plrlKind = "content#posListResponse"
    , _plrlResources = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posListResponse\".
plrlKind :: Lens' PosListResponse Text
plrlKind = lens _plrlKind (\ s a -> s{_plrlKind = a})

plrlResources :: Lens' PosListResponse [PosStore]
plrlResources
  = lens _plrlResources
      (\ s a -> s{_plrlResources = a})
      . _Default
      . _Coerce

instance FromJSON PosListResponse where
        parseJSON
          = withObject "PosListResponse"
              (\ o ->
                 PosListResponse' <$>
                   (o .:? "kind" .!= "content#posListResponse") <*>
                     (o .:? "resources" .!= mempty))

instance ToJSON PosListResponse where
        toJSON PosListResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _plrlKind),
                  ("resources" .=) <$> _plrlResources])

--
-- /See:/ 'orderDeliveryDetails' smart constructor.
data OrderDeliveryDetails = OrderDeliveryDetails'
    { _oddAddress :: !(Maybe OrderAddress)
    , _oddPhoneNumber :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderDeliveryDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oddAddress'
--
-- * 'oddPhoneNumber'
orderDeliveryDetails
    :: OrderDeliveryDetails
orderDeliveryDetails = 
    OrderDeliveryDetails'
    { _oddAddress = Nothing
    , _oddPhoneNumber = Nothing
    }

-- | The delivery address
oddAddress :: Lens' OrderDeliveryDetails (Maybe OrderAddress)
oddAddress
  = lens _oddAddress (\ s a -> s{_oddAddress = a})

-- | The phone number of the person receiving the delivery.
oddPhoneNumber :: Lens' OrderDeliveryDetails (Maybe Text)
oddPhoneNumber
  = lens _oddPhoneNumber
      (\ s a -> s{_oddPhoneNumber = a})

instance FromJSON OrderDeliveryDetails where
        parseJSON
          = withObject "OrderDeliveryDetails"
              (\ o ->
                 OrderDeliveryDetails' <$>
                   (o .:? "address") <*> (o .:? "phoneNumber"))

instance ToJSON OrderDeliveryDetails where
        toJSON OrderDeliveryDetails'{..}
          = object
              (catMaybes
                 [("address" .=) <$> _oddAddress,
                  ("phoneNumber" .=) <$> _oddPhoneNumber])

--
-- /See:/ 'ordersCancelResponse' smart constructor.
data OrdersCancelResponse = OrdersCancelResponse'
    { _ocrKind :: !Text
    , _ocrExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrKind'
--
-- * 'ocrExecutionStatus'
ordersCancelResponse
    :: OrdersCancelResponse
ordersCancelResponse = 
    OrdersCancelResponse'
    { _ocrKind = "content#ordersCancelResponse"
    , _ocrExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersCancelResponse\".
ocrKind :: Lens' OrdersCancelResponse Text
ocrKind = lens _ocrKind (\ s a -> s{_ocrKind = a})

-- | The status of the execution.
ocrExecutionStatus :: Lens' OrdersCancelResponse (Maybe Text)
ocrExecutionStatus
  = lens _ocrExecutionStatus
      (\ s a -> s{_ocrExecutionStatus = a})

instance FromJSON OrdersCancelResponse where
        parseJSON
          = withObject "OrdersCancelResponse"
              (\ o ->
                 OrdersCancelResponse' <$>
                   (o .:? "kind" .!= "content#ordersCancelResponse") <*>
                     (o .:? "executionStatus"))

instance ToJSON OrdersCancelResponse where
        toJSON OrdersCancelResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ocrKind),
                  ("executionStatus" .=) <$> _ocrExecutionStatus])

--
-- /See:/ 'ordersSetLineItemMetadataRequest' smart constructor.
data OrdersSetLineItemMetadataRequest = OrdersSetLineItemMetadataRequest'
    { _oslimrAnnotations :: !(Maybe [OrderMerchantProvidedAnnotation])
    , _oslimrLineItemId :: !(Maybe Text)
    , _oslimrOperationId :: !(Maybe Text)
    , _oslimrProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersSetLineItemMetadataRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslimrAnnotations'
--
-- * 'oslimrLineItemId'
--
-- * 'oslimrOperationId'
--
-- * 'oslimrProductId'
ordersSetLineItemMetadataRequest
    :: OrdersSetLineItemMetadataRequest
ordersSetLineItemMetadataRequest = 
    OrdersSetLineItemMetadataRequest'
    { _oslimrAnnotations = Nothing
    , _oslimrLineItemId = Nothing
    , _oslimrOperationId = Nothing
    , _oslimrProductId = Nothing
    }

oslimrAnnotations :: Lens' OrdersSetLineItemMetadataRequest [OrderMerchantProvidedAnnotation]
oslimrAnnotations
  = lens _oslimrAnnotations
      (\ s a -> s{_oslimrAnnotations = a})
      . _Default
      . _Coerce

-- | The ID of the line item to set metadata. Either lineItemId or productId
-- is required.
oslimrLineItemId :: Lens' OrdersSetLineItemMetadataRequest (Maybe Text)
oslimrLineItemId
  = lens _oslimrLineItemId
      (\ s a -> s{_oslimrLineItemId = a})

-- | The ID of the operation. Unique across all operations for a given order.
oslimrOperationId :: Lens' OrdersSetLineItemMetadataRequest (Maybe Text)
oslimrOperationId
  = lens _oslimrOperationId
      (\ s a -> s{_oslimrOperationId = a})

-- | The ID of the product to set metadata. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
oslimrProductId :: Lens' OrdersSetLineItemMetadataRequest (Maybe Text)
oslimrProductId
  = lens _oslimrProductId
      (\ s a -> s{_oslimrProductId = a})

instance FromJSON OrdersSetLineItemMetadataRequest
         where
        parseJSON
          = withObject "OrdersSetLineItemMetadataRequest"
              (\ o ->
                 OrdersSetLineItemMetadataRequest' <$>
                   (o .:? "annotations" .!= mempty) <*>
                     (o .:? "lineItemId")
                     <*> (o .:? "operationId")
                     <*> (o .:? "productId"))

instance ToJSON OrdersSetLineItemMetadataRequest
         where
        toJSON OrdersSetLineItemMetadataRequest'{..}
          = object
              (catMaybes
                 [("annotations" .=) <$> _oslimrAnnotations,
                  ("lineItemId" .=) <$> _oslimrLineItemId,
                  ("operationId" .=) <$> _oslimrOperationId,
                  ("productId" .=) <$> _oslimrProductId])

--
-- /See:/ 'ordersRejectReturnLineItemResponse' smart constructor.
data OrdersRejectReturnLineItemResponse = OrdersRejectReturnLineItemResponse'
    { _ordKind :: !Text
    , _ordExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersRejectReturnLineItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ordKind'
--
-- * 'ordExecutionStatus'
ordersRejectReturnLineItemResponse
    :: OrdersRejectReturnLineItemResponse
ordersRejectReturnLineItemResponse = 
    OrdersRejectReturnLineItemResponse'
    { _ordKind = "content#ordersRejectReturnLineItemResponse"
    , _ordExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersRejectReturnLineItemResponse\".
ordKind :: Lens' OrdersRejectReturnLineItemResponse Text
ordKind = lens _ordKind (\ s a -> s{_ordKind = a})

-- | The status of the execution.
ordExecutionStatus :: Lens' OrdersRejectReturnLineItemResponse (Maybe Text)
ordExecutionStatus
  = lens _ordExecutionStatus
      (\ s a -> s{_ordExecutionStatus = a})

instance FromJSON OrdersRejectReturnLineItemResponse
         where
        parseJSON
          = withObject "OrdersRejectReturnLineItemResponse"
              (\ o ->
                 OrdersRejectReturnLineItemResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersRejectReturnLineItemResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersRejectReturnLineItemResponse
         where
        toJSON OrdersRejectReturnLineItemResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _ordKind),
                  ("executionStatus" .=) <$> _ordExecutionStatus])

--
-- /See:/ 'testOrder' smart constructor.
data TestOrder = TestOrder'
    { _toKind :: !Text
    , _toLineItems :: !(Maybe [TestOrderLineItem])
    , _toShippingOption :: !(Maybe Text)
    , _toPredefinedDeliveryAddress :: !(Maybe Text)
    , _toShippingCostTax :: !(Maybe Price)
    , _toCustomer :: !(Maybe TestOrderCustomer)
    , _toPaymentMethod :: !(Maybe TestOrderPaymentMethod)
    , _toPromotions :: !(Maybe [OrderPromotion])
    , _toNotificationMode :: !(Maybe Text)
    , _toShippingCost :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestOrder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toKind'
--
-- * 'toLineItems'
--
-- * 'toShippingOption'
--
-- * 'toPredefinedDeliveryAddress'
--
-- * 'toShippingCostTax'
--
-- * 'toCustomer'
--
-- * 'toPaymentMethod'
--
-- * 'toPromotions'
--
-- * 'toNotificationMode'
--
-- * 'toShippingCost'
testOrder
    :: TestOrder
testOrder = 
    TestOrder'
    { _toKind = "content#testOrder"
    , _toLineItems = Nothing
    , _toShippingOption = Nothing
    , _toPredefinedDeliveryAddress = Nothing
    , _toShippingCostTax = Nothing
    , _toCustomer = Nothing
    , _toPaymentMethod = Nothing
    , _toPromotions = Nothing
    , _toNotificationMode = Nothing
    , _toShippingCost = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#testOrder\".
toKind :: Lens' TestOrder Text
toKind = lens _toKind (\ s a -> s{_toKind = a})

-- | Line items that are ordered. At least one line item must be provided.
toLineItems :: Lens' TestOrder [TestOrderLineItem]
toLineItems
  = lens _toLineItems (\ s a -> s{_toLineItems = a}) .
      _Default
      . _Coerce

-- | The requested shipping option.
toShippingOption :: Lens' TestOrder (Maybe Text)
toShippingOption
  = lens _toShippingOption
      (\ s a -> s{_toShippingOption = a})

-- | Identifier of one of the predefined delivery addresses for the delivery.
toPredefinedDeliveryAddress :: Lens' TestOrder (Maybe Text)
toPredefinedDeliveryAddress
  = lens _toPredefinedDeliveryAddress
      (\ s a -> s{_toPredefinedDeliveryAddress = a})

-- | The tax for the total shipping cost.
toShippingCostTax :: Lens' TestOrder (Maybe Price)
toShippingCostTax
  = lens _toShippingCostTax
      (\ s a -> s{_toShippingCostTax = a})

-- | The details of the customer who placed the order.
toCustomer :: Lens' TestOrder (Maybe TestOrderCustomer)
toCustomer
  = lens _toCustomer (\ s a -> s{_toCustomer = a})

-- | The details of the payment method.
toPaymentMethod :: Lens' TestOrder (Maybe TestOrderPaymentMethod)
toPaymentMethod
  = lens _toPaymentMethod
      (\ s a -> s{_toPaymentMethod = a})

-- | The details of the merchant provided promotions applied to the order.
-- More details about the program are here.
toPromotions :: Lens' TestOrder [OrderPromotion]
toPromotions
  = lens _toPromotions (\ s a -> s{_toPromotions = a})
      . _Default
      . _Coerce

-- | Determines if test order must be pulled by merchant or pushed to
-- merchant via push integration.
toNotificationMode :: Lens' TestOrder (Maybe Text)
toNotificationMode
  = lens _toNotificationMode
      (\ s a -> s{_toNotificationMode = a})

-- | The total cost of shipping for all items.
toShippingCost :: Lens' TestOrder (Maybe Price)
toShippingCost
  = lens _toShippingCost
      (\ s a -> s{_toShippingCost = a})

instance FromJSON TestOrder where
        parseJSON
          = withObject "TestOrder"
              (\ o ->
                 TestOrder' <$>
                   (o .:? "kind" .!= "content#testOrder") <*>
                     (o .:? "lineItems" .!= mempty)
                     <*> (o .:? "shippingOption")
                     <*> (o .:? "predefinedDeliveryAddress")
                     <*> (o .:? "shippingCostTax")
                     <*> (o .:? "customer")
                     <*> (o .:? "paymentMethod")
                     <*> (o .:? "promotions" .!= mempty)
                     <*> (o .:? "notificationMode")
                     <*> (o .:? "shippingCost"))

instance ToJSON TestOrder where
        toJSON TestOrder'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _toKind),
                  ("lineItems" .=) <$> _toLineItems,
                  ("shippingOption" .=) <$> _toShippingOption,
                  ("predefinedDeliveryAddress" .=) <$>
                    _toPredefinedDeliveryAddress,
                  ("shippingCostTax" .=) <$> _toShippingCostTax,
                  ("customer" .=) <$> _toCustomer,
                  ("paymentMethod" .=) <$> _toPaymentMethod,
                  ("promotions" .=) <$> _toPromotions,
                  ("notificationMode" .=) <$> _toNotificationMode,
                  ("shippingCost" .=) <$> _toShippingCost])

-- | A batch entry encoding a single non-batch datafeedstatuses response.
--
-- /See:/ 'datafeedstatusesCustomBatchResponseEntry' smart constructor.
data DatafeedstatusesCustomBatchResponseEntry = DatafeedstatusesCustomBatchResponseEntry'
    { _datErrors :: !(Maybe Errors)
    , _datDatafeedStatus :: !(Maybe DatafeedStatus)
    , _datBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedstatusesCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datErrors'
--
-- * 'datDatafeedStatus'
--
-- * 'datBatchId'
datafeedstatusesCustomBatchResponseEntry
    :: DatafeedstatusesCustomBatchResponseEntry
datafeedstatusesCustomBatchResponseEntry = 
    DatafeedstatusesCustomBatchResponseEntry'
    { _datErrors = Nothing
    , _datDatafeedStatus = Nothing
    , _datBatchId = Nothing
    }

-- | A list of errors defined if and only if the request failed.
datErrors :: Lens' DatafeedstatusesCustomBatchResponseEntry (Maybe Errors)
datErrors
  = lens _datErrors (\ s a -> s{_datErrors = a})

-- | The requested data feed status. Defined if and only if the request was
-- successful.
datDatafeedStatus :: Lens' DatafeedstatusesCustomBatchResponseEntry (Maybe DatafeedStatus)
datDatafeedStatus
  = lens _datDatafeedStatus
      (\ s a -> s{_datDatafeedStatus = a})

-- | The ID of the request entry this entry responds to.
datBatchId :: Lens' DatafeedstatusesCustomBatchResponseEntry (Maybe Word32)
datBatchId
  = lens _datBatchId (\ s a -> s{_datBatchId = a}) .
      mapping _Coerce

instance FromJSON
         DatafeedstatusesCustomBatchResponseEntry where
        parseJSON
          = withObject
              "DatafeedstatusesCustomBatchResponseEntry"
              (\ o ->
                 DatafeedstatusesCustomBatchResponseEntry' <$>
                   (o .:? "errors") <*> (o .:? "datafeedStatus") <*>
                     (o .:? "batchId"))

instance ToJSON
         DatafeedstatusesCustomBatchResponseEntry where
        toJSON DatafeedstatusesCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [("errors" .=) <$> _datErrors,
                  ("datafeedStatus" .=) <$> _datDatafeedStatus,
                  ("batchId" .=) <$> _datBatchId])

--
-- /See:/ 'orderRefund' smart constructor.
data OrderRefund = OrderRefund'
    { _oAmount :: !(Maybe Price)
    , _oActor :: !(Maybe Text)
    , _oReason :: !(Maybe Text)
    , _oCreationDate :: !(Maybe Text)
    , _oReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderRefund' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oAmount'
--
-- * 'oActor'
--
-- * 'oReason'
--
-- * 'oCreationDate'
--
-- * 'oReasonText'
orderRefund
    :: OrderRefund
orderRefund = 
    OrderRefund'
    { _oAmount = Nothing
    , _oActor = Nothing
    , _oReason = Nothing
    , _oCreationDate = Nothing
    , _oReasonText = Nothing
    }

-- | The amount that is refunded.
oAmount :: Lens' OrderRefund (Maybe Price)
oAmount = lens _oAmount (\ s a -> s{_oAmount = a})

-- | The actor that created the refund.
oActor :: Lens' OrderRefund (Maybe Text)
oActor = lens _oActor (\ s a -> s{_oActor = a})

-- | The reason for the refund.
oReason :: Lens' OrderRefund (Maybe Text)
oReason = lens _oReason (\ s a -> s{_oReason = a})

-- | Date on which the item has been created, in ISO 8601 format.
oCreationDate :: Lens' OrderRefund (Maybe Text)
oCreationDate
  = lens _oCreationDate
      (\ s a -> s{_oCreationDate = a})

-- | The explanation of the reason.
oReasonText :: Lens' OrderRefund (Maybe Text)
oReasonText
  = lens _oReasonText (\ s a -> s{_oReasonText = a})

instance FromJSON OrderRefund where
        parseJSON
          = withObject "OrderRefund"
              (\ o ->
                 OrderRefund' <$>
                   (o .:? "amount") <*> (o .:? "actor") <*>
                     (o .:? "reason")
                     <*> (o .:? "creationDate")
                     <*> (o .:? "reasonText"))

instance ToJSON OrderRefund where
        toJSON OrderRefund'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _oAmount,
                  ("actor" .=) <$> _oActor, ("reason" .=) <$> _oReason,
                  ("creationDate" .=) <$> _oCreationDate,
                  ("reasonText" .=) <$> _oReasonText])

--
-- /See:/ 'testOrderLineItemProduct' smart constructor.
data TestOrderLineItemProduct = TestOrderLineItemProduct'
    { _tolipImageLink :: !(Maybe Text)
    , _tolipChannel :: !(Maybe Text)
    , _tolipBrand :: !(Maybe Text)
    , _tolipTargetCountry :: !(Maybe Text)
    , _tolipGtin :: !(Maybe Text)
    , _tolipItemGroupId :: !(Maybe Text)
    , _tolipOfferId :: !(Maybe Text)
    , _tolipPrice :: !(Maybe Price)
    , _tolipVariantAttributes :: !(Maybe [OrderLineItemProductVariantAttribute])
    , _tolipTitle :: !(Maybe Text)
    , _tolipContentLanguage :: !(Maybe Text)
    , _tolipMpn :: !(Maybe Text)
    , _tolipCondition :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestOrderLineItemProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tolipImageLink'
--
-- * 'tolipChannel'
--
-- * 'tolipBrand'
--
-- * 'tolipTargetCountry'
--
-- * 'tolipGtin'
--
-- * 'tolipItemGroupId'
--
-- * 'tolipOfferId'
--
-- * 'tolipPrice'
--
-- * 'tolipVariantAttributes'
--
-- * 'tolipTitle'
--
-- * 'tolipContentLanguage'
--
-- * 'tolipMpn'
--
-- * 'tolipCondition'
testOrderLineItemProduct
    :: TestOrderLineItemProduct
testOrderLineItemProduct = 
    TestOrderLineItemProduct'
    { _tolipImageLink = Nothing
    , _tolipChannel = Nothing
    , _tolipBrand = Nothing
    , _tolipTargetCountry = Nothing
    , _tolipGtin = Nothing
    , _tolipItemGroupId = Nothing
    , _tolipOfferId = Nothing
    , _tolipPrice = Nothing
    , _tolipVariantAttributes = Nothing
    , _tolipTitle = Nothing
    , _tolipContentLanguage = Nothing
    , _tolipMpn = Nothing
    , _tolipCondition = Nothing
    }

-- | URL of an image of the item.
tolipImageLink :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipImageLink
  = lens _tolipImageLink
      (\ s a -> s{_tolipImageLink = a})

-- | The item\'s channel.
tolipChannel :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipChannel
  = lens _tolipChannel (\ s a -> s{_tolipChannel = a})

-- | Brand of the item.
tolipBrand :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipBrand
  = lens _tolipBrand (\ s a -> s{_tolipBrand = a})

-- | The CLDR territory code of the target country of the product.
tolipTargetCountry :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipTargetCountry
  = lens _tolipTargetCountry
      (\ s a -> s{_tolipTargetCountry = a})

-- | Global Trade Item Number (GTIN) of the item. Optional.
tolipGtin :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipGtin
  = lens _tolipGtin (\ s a -> s{_tolipGtin = a})

-- | Shared identifier for all variants of the same product. Optional.
tolipItemGroupId :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipItemGroupId
  = lens _tolipItemGroupId
      (\ s a -> s{_tolipItemGroupId = a})

-- | An identifier of the item.
tolipOfferId :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipOfferId
  = lens _tolipOfferId (\ s a -> s{_tolipOfferId = a})

-- | The price for the product.
tolipPrice :: Lens' TestOrderLineItemProduct (Maybe Price)
tolipPrice
  = lens _tolipPrice (\ s a -> s{_tolipPrice = a})

-- | Variant attributes for the item. Optional.
tolipVariantAttributes :: Lens' TestOrderLineItemProduct [OrderLineItemProductVariantAttribute]
tolipVariantAttributes
  = lens _tolipVariantAttributes
      (\ s a -> s{_tolipVariantAttributes = a})
      . _Default
      . _Coerce

-- | The title of the product.
tolipTitle :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipTitle
  = lens _tolipTitle (\ s a -> s{_tolipTitle = a})

-- | The two-letter ISO 639-1 language code for the item.
tolipContentLanguage :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipContentLanguage
  = lens _tolipContentLanguage
      (\ s a -> s{_tolipContentLanguage = a})

-- | Manufacturer Part Number (MPN) of the item. Optional.
tolipMpn :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipMpn = lens _tolipMpn (\ s a -> s{_tolipMpn = a})

-- | Condition or state of the item.
tolipCondition :: Lens' TestOrderLineItemProduct (Maybe Text)
tolipCondition
  = lens _tolipCondition
      (\ s a -> s{_tolipCondition = a})

instance FromJSON TestOrderLineItemProduct where
        parseJSON
          = withObject "TestOrderLineItemProduct"
              (\ o ->
                 TestOrderLineItemProduct' <$>
                   (o .:? "imageLink") <*> (o .:? "channel") <*>
                     (o .:? "brand")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "itemGroupId")
                     <*> (o .:? "offerId")
                     <*> (o .:? "price")
                     <*> (o .:? "variantAttributes" .!= mempty)
                     <*> (o .:? "title")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "mpn")
                     <*> (o .:? "condition"))

instance ToJSON TestOrderLineItemProduct where
        toJSON TestOrderLineItemProduct'{..}
          = object
              (catMaybes
                 [("imageLink" .=) <$> _tolipImageLink,
                  ("channel" .=) <$> _tolipChannel,
                  ("brand" .=) <$> _tolipBrand,
                  ("targetCountry" .=) <$> _tolipTargetCountry,
                  ("gtin" .=) <$> _tolipGtin,
                  ("itemGroupId" .=) <$> _tolipItemGroupId,
                  ("offerId" .=) <$> _tolipOfferId,
                  ("price" .=) <$> _tolipPrice,
                  ("variantAttributes" .=) <$> _tolipVariantAttributes,
                  ("title" .=) <$> _tolipTitle,
                  ("contentLanguage" .=) <$> _tolipContentLanguage,
                  ("mpn" .=) <$> _tolipMpn,
                  ("condition" .=) <$> _tolipCondition])

--
-- /See:/ 'ordersCustomBatchRequestEntryReturnRefundLineItem' smart constructor.
data OrdersCustomBatchRequestEntryReturnRefundLineItem = OrdersCustomBatchRequestEntryReturnRefundLineItem'
    { _ordQuantity :: !(Maybe (Textual Word32))
    , _ordLineItemId :: !(Maybe Text)
    , _ordReason :: !(Maybe Text)
    , _ordAmountPretax :: !(Maybe Price)
    , _ordProductId :: !(Maybe Text)
    , _ordAmountTax :: !(Maybe Price)
    , _ordReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryReturnRefundLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ordQuantity'
--
-- * 'ordLineItemId'
--
-- * 'ordReason'
--
-- * 'ordAmountPretax'
--
-- * 'ordProductId'
--
-- * 'ordAmountTax'
--
-- * 'ordReasonText'
ordersCustomBatchRequestEntryReturnRefundLineItem
    :: OrdersCustomBatchRequestEntryReturnRefundLineItem
ordersCustomBatchRequestEntryReturnRefundLineItem = 
    OrdersCustomBatchRequestEntryReturnRefundLineItem'
    { _ordQuantity = Nothing
    , _ordLineItemId = Nothing
    , _ordReason = Nothing
    , _ordAmountPretax = Nothing
    , _ordProductId = Nothing
    , _ordAmountTax = Nothing
    , _ordReasonText = Nothing
    }

-- | The quantity to return and refund.
ordQuantity :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Word32)
ordQuantity
  = lens _ordQuantity (\ s a -> s{_ordQuantity = a}) .
      mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
ordLineItemId :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Text)
ordLineItemId
  = lens _ordLineItemId
      (\ s a -> s{_ordLineItemId = a})

-- | The reason for the return.
ordReason :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Text)
ordReason
  = lens _ordReason (\ s a -> s{_ordReason = a})

-- | The amount that is refunded. Optional, but if filled then both
-- amountPretax and amountTax must be set.
ordAmountPretax :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Price)
ordAmountPretax
  = lens _ordAmountPretax
      (\ s a -> s{_ordAmountPretax = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ordProductId :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Text)
ordProductId
  = lens _ordProductId (\ s a -> s{_ordProductId = a})

-- | Tax amount that correspond to refund amount in amountPretax.
ordAmountTax :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Price)
ordAmountTax
  = lens _ordAmountTax (\ s a -> s{_ordAmountTax = a})

-- | The explanation of the reason.
ordReasonText :: Lens' OrdersCustomBatchRequestEntryReturnRefundLineItem (Maybe Text)
ordReasonText
  = lens _ordReasonText
      (\ s a -> s{_ordReasonText = a})

instance FromJSON
         OrdersCustomBatchRequestEntryReturnRefundLineItem
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryReturnRefundLineItem"
              (\ o ->
                 OrdersCustomBatchRequestEntryReturnRefundLineItem'
                   <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON
         OrdersCustomBatchRequestEntryReturnRefundLineItem
         where
        toJSON
          OrdersCustomBatchRequestEntryReturnRefundLineItem'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _ordQuantity,
                  ("lineItemId" .=) <$> _ordLineItemId,
                  ("reason" .=) <$> _ordReason,
                  ("amountPretax" .=) <$> _ordAmountPretax,
                  ("productId" .=) <$> _ordProductId,
                  ("amountTax" .=) <$> _ordAmountTax,
                  ("reasonText" .=) <$> _ordReasonText])

--
-- /See:/ 'accounttaxCustomBatchResponse' smart constructor.
data AccounttaxCustomBatchResponse = AccounttaxCustomBatchResponse'
    { _acbr1Entries :: !(Maybe [AccounttaxCustomBatchResponseEntry])
    , _acbr1Kind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbr1Entries'
--
-- * 'acbr1Kind'
accounttaxCustomBatchResponse
    :: AccounttaxCustomBatchResponse
accounttaxCustomBatchResponse = 
    AccounttaxCustomBatchResponse'
    { _acbr1Entries = Nothing
    , _acbr1Kind = "content#accounttaxCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
acbr1Entries :: Lens' AccounttaxCustomBatchResponse [AccounttaxCustomBatchResponseEntry]
acbr1Entries
  = lens _acbr1Entries (\ s a -> s{_acbr1Entries = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accounttaxCustomBatchResponse\".
acbr1Kind :: Lens' AccounttaxCustomBatchResponse Text
acbr1Kind
  = lens _acbr1Kind (\ s a -> s{_acbr1Kind = a})

instance FromJSON AccounttaxCustomBatchResponse where
        parseJSON
          = withObject "AccounttaxCustomBatchResponse"
              (\ o ->
                 AccounttaxCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#accounttaxCustomBatchResponse"))

instance ToJSON AccounttaxCustomBatchResponse where
        toJSON AccounttaxCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _acbr1Entries,
                  Just ("kind" .= _acbr1Kind)])

-- | A batch entry encoding a single non-batch inventory request.
--
-- /See:/ 'inventoryCustomBatchRequestEntry' smart constructor.
data InventoryCustomBatchRequestEntry = InventoryCustomBatchRequestEntry'
    { _iMerchantId :: !(Maybe (Textual Word64))
    , _iStoreCode :: !(Maybe Text)
    , _iInventory :: !(Maybe Inventory)
    , _iProductId :: !(Maybe Text)
    , _iBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iMerchantId'
--
-- * 'iStoreCode'
--
-- * 'iInventory'
--
-- * 'iProductId'
--
-- * 'iBatchId'
inventoryCustomBatchRequestEntry
    :: InventoryCustomBatchRequestEntry
inventoryCustomBatchRequestEntry = 
    InventoryCustomBatchRequestEntry'
    { _iMerchantId = Nothing
    , _iStoreCode = Nothing
    , _iInventory = Nothing
    , _iProductId = Nothing
    , _iBatchId = Nothing
    }

-- | The ID of the managing account.
iMerchantId :: Lens' InventoryCustomBatchRequestEntry (Maybe Word64)
iMerchantId
  = lens _iMerchantId (\ s a -> s{_iMerchantId = a}) .
      mapping _Coerce

-- | The code of the store for which to update price and availability. Use
-- online to update price and availability of an online product.
iStoreCode :: Lens' InventoryCustomBatchRequestEntry (Maybe Text)
iStoreCode
  = lens _iStoreCode (\ s a -> s{_iStoreCode = a})

-- | Price and availability of the product.
iInventory :: Lens' InventoryCustomBatchRequestEntry (Maybe Inventory)
iInventory
  = lens _iInventory (\ s a -> s{_iInventory = a})

-- | The ID of the product for which to update price and availability.
iProductId :: Lens' InventoryCustomBatchRequestEntry (Maybe Text)
iProductId
  = lens _iProductId (\ s a -> s{_iProductId = a})

-- | An entry ID, unique within the batch request.
iBatchId :: Lens' InventoryCustomBatchRequestEntry (Maybe Word32)
iBatchId
  = lens _iBatchId (\ s a -> s{_iBatchId = a}) .
      mapping _Coerce

instance FromJSON InventoryCustomBatchRequestEntry
         where
        parseJSON
          = withObject "InventoryCustomBatchRequestEntry"
              (\ o ->
                 InventoryCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "storeCode") <*>
                     (o .:? "inventory")
                     <*> (o .:? "productId")
                     <*> (o .:? "batchId"))

instance ToJSON InventoryCustomBatchRequestEntry
         where
        toJSON InventoryCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _iMerchantId,
                  ("storeCode" .=) <$> _iStoreCode,
                  ("inventory" .=) <$> _iInventory,
                  ("productId" .=) <$> _iProductId,
                  ("batchId" .=) <$> _iBatchId])

--
-- /See:/ 'accountsClaimWebsiteResponse' smart constructor.
newtype AccountsClaimWebsiteResponse = AccountsClaimWebsiteResponse'
    { _acwrKind :: Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsClaimWebsiteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwrKind'
accountsClaimWebsiteResponse
    :: AccountsClaimWebsiteResponse
accountsClaimWebsiteResponse = 
    AccountsClaimWebsiteResponse'
    { _acwrKind = "content#accountsClaimWebsiteResponse"
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#accountsClaimWebsiteResponse\".
acwrKind :: Lens' AccountsClaimWebsiteResponse Text
acwrKind = lens _acwrKind (\ s a -> s{_acwrKind = a})

instance FromJSON AccountsClaimWebsiteResponse where
        parseJSON
          = withObject "AccountsClaimWebsiteResponse"
              (\ o ->
                 AccountsClaimWebsiteResponse' <$>
                   (o .:? "kind" .!=
                      "content#accountsClaimWebsiteResponse"))

instance ToJSON AccountsClaimWebsiteResponse where
        toJSON AccountsClaimWebsiteResponse'{..}
          = object (catMaybes [Just ("kind" .= _acwrKind)])

--
-- /See:/ 'orderAddress' smart constructor.
data OrderAddress = OrderAddress'
    { _oaRecipientName :: !(Maybe Text)
    , _oaStreetAddress :: !(Maybe [Text])
    , _oaCountry :: !(Maybe Text)
    , _oaPostalCode :: !(Maybe Text)
    , _oaLocality :: !(Maybe Text)
    , _oaIsPostOfficeBox :: !(Maybe Bool)
    , _oaFullAddress :: !(Maybe [Text])
    , _oaRegion :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaRecipientName'
--
-- * 'oaStreetAddress'
--
-- * 'oaCountry'
--
-- * 'oaPostalCode'
--
-- * 'oaLocality'
--
-- * 'oaIsPostOfficeBox'
--
-- * 'oaFullAddress'
--
-- * 'oaRegion'
orderAddress
    :: OrderAddress
orderAddress = 
    OrderAddress'
    { _oaRecipientName = Nothing
    , _oaStreetAddress = Nothing
    , _oaCountry = Nothing
    , _oaPostalCode = Nothing
    , _oaLocality = Nothing
    , _oaIsPostOfficeBox = Nothing
    , _oaFullAddress = Nothing
    , _oaRegion = Nothing
    }

-- | Name of the recipient.
oaRecipientName :: Lens' OrderAddress (Maybe Text)
oaRecipientName
  = lens _oaRecipientName
      (\ s a -> s{_oaRecipientName = a})

-- | Street-level part of the address.
oaStreetAddress :: Lens' OrderAddress [Text]
oaStreetAddress
  = lens _oaStreetAddress
      (\ s a -> s{_oaStreetAddress = a})
      . _Default
      . _Coerce

-- | CLDR country code (e.g. \"US\").
oaCountry :: Lens' OrderAddress (Maybe Text)
oaCountry
  = lens _oaCountry (\ s a -> s{_oaCountry = a})

-- | Postal Code or ZIP (e.g. \"94043\").
oaPostalCode :: Lens' OrderAddress (Maybe Text)
oaPostalCode
  = lens _oaPostalCode (\ s a -> s{_oaPostalCode = a})

-- | City, town or commune. May also include dependent localities or
-- sublocalities (e.g. neighborhoods or suburbs).
oaLocality :: Lens' OrderAddress (Maybe Text)
oaLocality
  = lens _oaLocality (\ s a -> s{_oaLocality = a})

-- | Whether the address is a post office box.
oaIsPostOfficeBox :: Lens' OrderAddress (Maybe Bool)
oaIsPostOfficeBox
  = lens _oaIsPostOfficeBox
      (\ s a -> s{_oaIsPostOfficeBox = a})

-- | Strings representing the lines of the printed label for mailing the
-- order, for example: John Smith 1600 Amphitheatre Parkway Mountain View,
-- CA, 94043 United States
oaFullAddress :: Lens' OrderAddress [Text]
oaFullAddress
  = lens _oaFullAddress
      (\ s a -> s{_oaFullAddress = a})
      . _Default
      . _Coerce

-- | Top-level administrative subdivision of the country (e.g. \"CA\").
oaRegion :: Lens' OrderAddress (Maybe Text)
oaRegion = lens _oaRegion (\ s a -> s{_oaRegion = a})

instance FromJSON OrderAddress where
        parseJSON
          = withObject "OrderAddress"
              (\ o ->
                 OrderAddress' <$>
                   (o .:? "recipientName") <*>
                     (o .:? "streetAddress" .!= mempty)
                     <*> (o .:? "country")
                     <*> (o .:? "postalCode")
                     <*> (o .:? "locality")
                     <*> (o .:? "isPostOfficeBox")
                     <*> (o .:? "fullAddress" .!= mempty)
                     <*> (o .:? "region"))

instance ToJSON OrderAddress where
        toJSON OrderAddress'{..}
          = object
              (catMaybes
                 [("recipientName" .=) <$> _oaRecipientName,
                  ("streetAddress" .=) <$> _oaStreetAddress,
                  ("country" .=) <$> _oaCountry,
                  ("postalCode" .=) <$> _oaPostalCode,
                  ("locality" .=) <$> _oaLocality,
                  ("isPostOfficeBox" .=) <$> _oaIsPostOfficeBox,
                  ("fullAddress" .=) <$> _oaFullAddress,
                  ("region" .=) <$> _oaRegion])

--
-- /See:/ 'productUnitPricingBaseMeasure' smart constructor.
data ProductUnitPricingBaseMeasure = ProductUnitPricingBaseMeasure'
    { _pupbmValue :: !(Maybe (Textual Int64))
    , _pupbmUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductUnitPricingBaseMeasure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pupbmValue'
--
-- * 'pupbmUnit'
productUnitPricingBaseMeasure
    :: ProductUnitPricingBaseMeasure
productUnitPricingBaseMeasure = 
    ProductUnitPricingBaseMeasure'
    { _pupbmValue = Nothing
    , _pupbmUnit = Nothing
    }

-- | The denominator of the unit price.
pupbmValue :: Lens' ProductUnitPricingBaseMeasure (Maybe Int64)
pupbmValue
  = lens _pupbmValue (\ s a -> s{_pupbmValue = a}) .
      mapping _Coerce

-- | The unit of the denominator.
pupbmUnit :: Lens' ProductUnitPricingBaseMeasure (Maybe Text)
pupbmUnit
  = lens _pupbmUnit (\ s a -> s{_pupbmUnit = a})

instance FromJSON ProductUnitPricingBaseMeasure where
        parseJSON
          = withObject "ProductUnitPricingBaseMeasure"
              (\ o ->
                 ProductUnitPricingBaseMeasure' <$>
                   (o .:? "value") <*> (o .:? "unit"))

instance ToJSON ProductUnitPricingBaseMeasure where
        toJSON ProductUnitPricingBaseMeasure'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pupbmValue,
                  ("unit" .=) <$> _pupbmUnit])

--
-- /See:/ 'datafeedsListResponse' smart constructor.
data DatafeedsListResponse = DatafeedsListResponse'
    { _dlrNextPageToken :: !(Maybe Text)
    , _dlrKind :: !Text
    , _dlrResources :: !(Maybe [Datafeed])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrNextPageToken'
--
-- * 'dlrKind'
--
-- * 'dlrResources'
datafeedsListResponse
    :: DatafeedsListResponse
datafeedsListResponse = 
    DatafeedsListResponse'
    { _dlrNextPageToken = Nothing
    , _dlrKind = "content#datafeedsListResponse"
    , _dlrResources = Nothing
    }

-- | The token for the retrieval of the next page of datafeeds.
dlrNextPageToken :: Lens' DatafeedsListResponse (Maybe Text)
dlrNextPageToken
  = lens _dlrNextPageToken
      (\ s a -> s{_dlrNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeedsListResponse\".
dlrKind :: Lens' DatafeedsListResponse Text
dlrKind = lens _dlrKind (\ s a -> s{_dlrKind = a})

dlrResources :: Lens' DatafeedsListResponse [Datafeed]
dlrResources
  = lens _dlrResources (\ s a -> s{_dlrResources = a})
      . _Default
      . _Coerce

instance FromJSON DatafeedsListResponse where
        parseJSON
          = withObject "DatafeedsListResponse"
              (\ o ->
                 DatafeedsListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "content#datafeedsListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON DatafeedsListResponse where
        toJSON DatafeedsListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _dlrNextPageToken,
                  Just ("kind" .= _dlrKind),
                  ("resources" .=) <$> _dlrResources])

-- | A batch entry encoding a single non-batch products response.
--
-- /See:/ 'productsCustomBatchResponseEntry' smart constructor.
data ProductsCustomBatchResponseEntry = ProductsCustomBatchResponseEntry'
    { _pcbre1Kind :: !Text
    , _pcbre1Product :: !(Maybe Product)
    , _pcbre1Errors :: !(Maybe Errors)
    , _pcbre1BatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbre1Kind'
--
-- * 'pcbre1Product'
--
-- * 'pcbre1Errors'
--
-- * 'pcbre1BatchId'
productsCustomBatchResponseEntry
    :: ProductsCustomBatchResponseEntry
productsCustomBatchResponseEntry = 
    ProductsCustomBatchResponseEntry'
    { _pcbre1Kind = "content#productsCustomBatchResponseEntry"
    , _pcbre1Product = Nothing
    , _pcbre1Errors = Nothing
    , _pcbre1BatchId = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productsCustomBatchResponseEntry\".
pcbre1Kind :: Lens' ProductsCustomBatchResponseEntry Text
pcbre1Kind
  = lens _pcbre1Kind (\ s a -> s{_pcbre1Kind = a})

-- | The inserted product. Only defined if the method is insert and if the
-- request was successful.
pcbre1Product :: Lens' ProductsCustomBatchResponseEntry (Maybe Product)
pcbre1Product
  = lens _pcbre1Product
      (\ s a -> s{_pcbre1Product = a})

-- | A list of errors defined if and only if the request failed.
pcbre1Errors :: Lens' ProductsCustomBatchResponseEntry (Maybe Errors)
pcbre1Errors
  = lens _pcbre1Errors (\ s a -> s{_pcbre1Errors = a})

-- | The ID of the request entry this entry responds to.
pcbre1BatchId :: Lens' ProductsCustomBatchResponseEntry (Maybe Word32)
pcbre1BatchId
  = lens _pcbre1BatchId
      (\ s a -> s{_pcbre1BatchId = a})
      . mapping _Coerce

instance FromJSON ProductsCustomBatchResponseEntry
         where
        parseJSON
          = withObject "ProductsCustomBatchResponseEntry"
              (\ o ->
                 ProductsCustomBatchResponseEntry' <$>
                   (o .:? "kind" .!=
                      "content#productsCustomBatchResponseEntry")
                     <*> (o .:? "product")
                     <*> (o .:? "errors")
                     <*> (o .:? "batchId"))

instance ToJSON ProductsCustomBatchResponseEntry
         where
        toJSON ProductsCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _pcbre1Kind),
                  ("product" .=) <$> _pcbre1Product,
                  ("errors" .=) <$> _pcbre1Errors,
                  ("batchId" .=) <$> _pcbre1BatchId])

--
-- /See:/ 'orderPaymentMethod' smart constructor.
data OrderPaymentMethod = OrderPaymentMethod'
    { _opmExpirationMonth :: !(Maybe (Textual Int32))
    , _opmExpirationYear :: !(Maybe (Textual Int32))
    , _opmPhoneNumber :: !(Maybe Text)
    , _opmBillingAddress :: !(Maybe OrderAddress)
    , _opmLastFourDigits :: !(Maybe Text)
    , _opmType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderPaymentMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opmExpirationMonth'
--
-- * 'opmExpirationYear'
--
-- * 'opmPhoneNumber'
--
-- * 'opmBillingAddress'
--
-- * 'opmLastFourDigits'
--
-- * 'opmType'
orderPaymentMethod
    :: OrderPaymentMethod
orderPaymentMethod = 
    OrderPaymentMethod'
    { _opmExpirationMonth = Nothing
    , _opmExpirationYear = Nothing
    , _opmPhoneNumber = Nothing
    , _opmBillingAddress = Nothing
    , _opmLastFourDigits = Nothing
    , _opmType = Nothing
    }

-- | The card expiration month (January = 1, February = 2 etc.).
opmExpirationMonth :: Lens' OrderPaymentMethod (Maybe Int32)
opmExpirationMonth
  = lens _opmExpirationMonth
      (\ s a -> s{_opmExpirationMonth = a})
      . mapping _Coerce

-- | The card expiration year (4-digit, e.g. 2015).
opmExpirationYear :: Lens' OrderPaymentMethod (Maybe Int32)
opmExpirationYear
  = lens _opmExpirationYear
      (\ s a -> s{_opmExpirationYear = a})
      . mapping _Coerce

-- | The billing phone number.
opmPhoneNumber :: Lens' OrderPaymentMethod (Maybe Text)
opmPhoneNumber
  = lens _opmPhoneNumber
      (\ s a -> s{_opmPhoneNumber = a})

-- | The billing address.
opmBillingAddress :: Lens' OrderPaymentMethod (Maybe OrderAddress)
opmBillingAddress
  = lens _opmBillingAddress
      (\ s a -> s{_opmBillingAddress = a})

-- | The last four digits of the card number.
opmLastFourDigits :: Lens' OrderPaymentMethod (Maybe Text)
opmLastFourDigits
  = lens _opmLastFourDigits
      (\ s a -> s{_opmLastFourDigits = a})

-- | The type of instrument. Acceptable values are: - \"AMEX\" - \"DISCOVER\"
-- - \"JCB\" - \"MASTERCARD\" - \"UNIONPAY\" - \"VISA\" - \"\"
opmType :: Lens' OrderPaymentMethod (Maybe Text)
opmType = lens _opmType (\ s a -> s{_opmType = a})

instance FromJSON OrderPaymentMethod where
        parseJSON
          = withObject "OrderPaymentMethod"
              (\ o ->
                 OrderPaymentMethod' <$>
                   (o .:? "expirationMonth") <*>
                     (o .:? "expirationYear")
                     <*> (o .:? "phoneNumber")
                     <*> (o .:? "billingAddress")
                     <*> (o .:? "lastFourDigits")
                     <*> (o .:? "type"))

instance ToJSON OrderPaymentMethod where
        toJSON OrderPaymentMethod'{..}
          = object
              (catMaybes
                 [("expirationMonth" .=) <$> _opmExpirationMonth,
                  ("expirationYear" .=) <$> _opmExpirationYear,
                  ("phoneNumber" .=) <$> _opmPhoneNumber,
                  ("billingAddress" .=) <$> _opmBillingAddress,
                  ("lastFourDigits" .=) <$> _opmLastFourDigits,
                  ("type" .=) <$> _opmType])

-- | Product data.
--
-- /See:/ 'product' smart constructor.
data Product = Product'
    { _prorDisplayAdsLink :: !(Maybe Text)
    , _prorCustomLabel1 :: !(Maybe Text)
    , _prorShippingWidth :: !(Maybe ProductShippingDimension)
    , _prorCustomGroups :: !(Maybe [ProductCustomGroup])
    , _prorImageLink :: !(Maybe Text)
    , _prorDisplayAdsValue :: !(Maybe (Textual Double))
    , _prorLoyaltyPoints :: !(Maybe LoyaltyPoints)
    , _prorAdditionalImageLinks :: !(Maybe [Text])
    , _prorValidatedDestinations :: !(Maybe [Text])
    , _prorColor :: !(Maybe Text)
    , _prorCustomLabel0 :: !(Maybe Text)
    , _prorKind :: !Text
    , _prorMinHandlingTime :: !(Maybe (Textual Int64))
    , _prorMultipack :: !(Maybe (Textual Int64))
    , _prorPattern :: !(Maybe Text)
    , _prorLink :: !(Maybe Text)
    , _prorSizeSystem :: !(Maybe Text)
    , _prorUnitPricingBaseMeasure :: !(Maybe ProductUnitPricingBaseMeasure)
    , _prorTaxes :: !(Maybe [ProductTax])
    , _prorMaterial :: !(Maybe Text)
    , _prorInstallment :: !(Maybe Installment)
    , _prorChannel :: !(Maybe Text)
    , _prorProductType :: !(Maybe Text)
    , _prorIdentifierExists :: !(Maybe Bool)
    , _prorOnlineOnly :: !(Maybe Bool)
    , _prorBrand :: !(Maybe Text)
    , _prorUnitPricingMeasure :: !(Maybe ProductUnitPricingMeasure)
    , _prorSalePrice :: !(Maybe Price)
    , _prorShippingLength :: !(Maybe ProductShippingDimension)
    , _prorCustomLabel3 :: !(Maybe Text)
    , _prorMaxHandlingTime :: !(Maybe (Textual Int64))
    , _prorWarnings :: !(Maybe [Error'])
    , _prorAdditionalProductTypes :: !(Maybe [Text])
    , _prorAvailability :: !(Maybe Text)
    , _prorTargetCountry :: !(Maybe Text)
    , _prorShippingLabel :: !(Maybe Text)
    , _prorCustomAttributes :: !(Maybe [ProductCustomAttribute])
    , _prorGtin :: !(Maybe Text)
    , _prorAgeGroup :: !(Maybe Text)
    , _prorDisplayAdsTitle :: !(Maybe Text)
    , _prorGender :: !(Maybe Text)
    , _prorDestinations :: !(Maybe [ProductDestination])
    , _prorExpirationDate :: !(Maybe Text)
    , _prorItemGroupId :: !(Maybe Text)
    , _prorAdwordsGrouping :: !(Maybe Text)
    , _prorSalePriceEffectiveDate :: !(Maybe Text)
    , _prorCustomLabel2 :: !(Maybe Text)
    , _prorGoogleProductCategory :: !(Maybe Text)
    , _prorShipping :: !(Maybe [ProductShipping])
    , _prorAdwordsRedirect :: !(Maybe Text)
    , _prorShippingWeight :: !(Maybe ProductShippingWeight)
    , _prorSellOnGoogleQuantity :: !(Maybe (Textual Int64))
    , _prorShippingHeight :: !(Maybe ProductShippingDimension)
    , _prorAvailabilityDate :: !(Maybe Text)
    , _prorOfferId :: !(Maybe Text)
    , _prorId :: !(Maybe Text)
    , _prorAdwordsLabels :: !(Maybe [Text])
    , _prorPrice :: !(Maybe Price)
    , _prorPromotionIds :: !(Maybe [Text])
    , _prorSizeType :: !(Maybe Text)
    , _prorMobileLink :: !(Maybe Text)
    , _prorTitle :: !(Maybe Text)
    , _prorAdult :: !(Maybe Bool)
    , _prorContentLanguage :: !(Maybe Text)
    , _prorAspects :: !(Maybe [ProductAspect])
    , _prorEnergyEfficiencyClass :: !(Maybe Text)
    , _prorDisplayAdsSimilarIds :: !(Maybe [Text])
    , _prorMpn :: !(Maybe Text)
    , _prorCondition :: !(Maybe Text)
    , _prorSizes :: !(Maybe [Text])
    , _prorIsBundle :: !(Maybe Bool)
    , _prorDescription :: !(Maybe Text)
    , _prorCustomLabel4 :: !(Maybe Text)
    , _prorDisplayAdsId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Product' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prorDisplayAdsLink'
--
-- * 'prorCustomLabel1'
--
-- * 'prorShippingWidth'
--
-- * 'prorCustomGroups'
--
-- * 'prorImageLink'
--
-- * 'prorDisplayAdsValue'
--
-- * 'prorLoyaltyPoints'
--
-- * 'prorAdditionalImageLinks'
--
-- * 'prorValidatedDestinations'
--
-- * 'prorColor'
--
-- * 'prorCustomLabel0'
--
-- * 'prorKind'
--
-- * 'prorMinHandlingTime'
--
-- * 'prorMultipack'
--
-- * 'prorPattern'
--
-- * 'prorLink'
--
-- * 'prorSizeSystem'
--
-- * 'prorUnitPricingBaseMeasure'
--
-- * 'prorTaxes'
--
-- * 'prorMaterial'
--
-- * 'prorInstallment'
--
-- * 'prorChannel'
--
-- * 'prorProductType'
--
-- * 'prorIdentifierExists'
--
-- * 'prorOnlineOnly'
--
-- * 'prorBrand'
--
-- * 'prorUnitPricingMeasure'
--
-- * 'prorSalePrice'
--
-- * 'prorShippingLength'
--
-- * 'prorCustomLabel3'
--
-- * 'prorMaxHandlingTime'
--
-- * 'prorWarnings'
--
-- * 'prorAdditionalProductTypes'
--
-- * 'prorAvailability'
--
-- * 'prorTargetCountry'
--
-- * 'prorShippingLabel'
--
-- * 'prorCustomAttributes'
--
-- * 'prorGtin'
--
-- * 'prorAgeGroup'
--
-- * 'prorDisplayAdsTitle'
--
-- * 'prorGender'
--
-- * 'prorDestinations'
--
-- * 'prorExpirationDate'
--
-- * 'prorItemGroupId'
--
-- * 'prorAdwordsGrouping'
--
-- * 'prorSalePriceEffectiveDate'
--
-- * 'prorCustomLabel2'
--
-- * 'prorGoogleProductCategory'
--
-- * 'prorShipping'
--
-- * 'prorAdwordsRedirect'
--
-- * 'prorShippingWeight'
--
-- * 'prorSellOnGoogleQuantity'
--
-- * 'prorShippingHeight'
--
-- * 'prorAvailabilityDate'
--
-- * 'prorOfferId'
--
-- * 'prorId'
--
-- * 'prorAdwordsLabels'
--
-- * 'prorPrice'
--
-- * 'prorPromotionIds'
--
-- * 'prorSizeType'
--
-- * 'prorMobileLink'
--
-- * 'prorTitle'
--
-- * 'prorAdult'
--
-- * 'prorContentLanguage'
--
-- * 'prorAspects'
--
-- * 'prorEnergyEfficiencyClass'
--
-- * 'prorDisplayAdsSimilarIds'
--
-- * 'prorMpn'
--
-- * 'prorCondition'
--
-- * 'prorSizes'
--
-- * 'prorIsBundle'
--
-- * 'prorDescription'
--
-- * 'prorCustomLabel4'
--
-- * 'prorDisplayAdsId'
product
    :: Product
product = 
    Product'
    { _prorDisplayAdsLink = Nothing
    , _prorCustomLabel1 = Nothing
    , _prorShippingWidth = Nothing
    , _prorCustomGroups = Nothing
    , _prorImageLink = Nothing
    , _prorDisplayAdsValue = Nothing
    , _prorLoyaltyPoints = Nothing
    , _prorAdditionalImageLinks = Nothing
    , _prorValidatedDestinations = Nothing
    , _prorColor = Nothing
    , _prorCustomLabel0 = Nothing
    , _prorKind = "content#product"
    , _prorMinHandlingTime = Nothing
    , _prorMultipack = Nothing
    , _prorPattern = Nothing
    , _prorLink = Nothing
    , _prorSizeSystem = Nothing
    , _prorUnitPricingBaseMeasure = Nothing
    , _prorTaxes = Nothing
    , _prorMaterial = Nothing
    , _prorInstallment = Nothing
    , _prorChannel = Nothing
    , _prorProductType = Nothing
    , _prorIdentifierExists = Nothing
    , _prorOnlineOnly = Nothing
    , _prorBrand = Nothing
    , _prorUnitPricingMeasure = Nothing
    , _prorSalePrice = Nothing
    , _prorShippingLength = Nothing
    , _prorCustomLabel3 = Nothing
    , _prorMaxHandlingTime = Nothing
    , _prorWarnings = Nothing
    , _prorAdditionalProductTypes = Nothing
    , _prorAvailability = Nothing
    , _prorTargetCountry = Nothing
    , _prorShippingLabel = Nothing
    , _prorCustomAttributes = Nothing
    , _prorGtin = Nothing
    , _prorAgeGroup = Nothing
    , _prorDisplayAdsTitle = Nothing
    , _prorGender = Nothing
    , _prorDestinations = Nothing
    , _prorExpirationDate = Nothing
    , _prorItemGroupId = Nothing
    , _prorAdwordsGrouping = Nothing
    , _prorSalePriceEffectiveDate = Nothing
    , _prorCustomLabel2 = Nothing
    , _prorGoogleProductCategory = Nothing
    , _prorShipping = Nothing
    , _prorAdwordsRedirect = Nothing
    , _prorShippingWeight = Nothing
    , _prorSellOnGoogleQuantity = Nothing
    , _prorShippingHeight = Nothing
    , _prorAvailabilityDate = Nothing
    , _prorOfferId = Nothing
    , _prorId = Nothing
    , _prorAdwordsLabels = Nothing
    , _prorPrice = Nothing
    , _prorPromotionIds = Nothing
    , _prorSizeType = Nothing
    , _prorMobileLink = Nothing
    , _prorTitle = Nothing
    , _prorAdult = Nothing
    , _prorContentLanguage = Nothing
    , _prorAspects = Nothing
    , _prorEnergyEfficiencyClass = Nothing
    , _prorDisplayAdsSimilarIds = Nothing
    , _prorMpn = Nothing
    , _prorCondition = Nothing
    , _prorSizes = Nothing
    , _prorIsBundle = Nothing
    , _prorDescription = Nothing
    , _prorCustomLabel4 = Nothing
    , _prorDisplayAdsId = Nothing
    }

-- | URL directly to your item\'s landing page for dynamic remarketing
-- campaigns.
prorDisplayAdsLink :: Lens' Product (Maybe Text)
prorDisplayAdsLink
  = lens _prorDisplayAdsLink
      (\ s a -> s{_prorDisplayAdsLink = a})

-- | Custom label 1 for custom grouping of items in a Shopping campaign.
prorCustomLabel1 :: Lens' Product (Maybe Text)
prorCustomLabel1
  = lens _prorCustomLabel1
      (\ s a -> s{_prorCustomLabel1 = a})

-- | Width of the item for shipping.
prorShippingWidth :: Lens' Product (Maybe ProductShippingDimension)
prorShippingWidth
  = lens _prorShippingWidth
      (\ s a -> s{_prorShippingWidth = a})

-- | A list of custom (merchant-provided) custom attribute groups.
prorCustomGroups :: Lens' Product [ProductCustomGroup]
prorCustomGroups
  = lens _prorCustomGroups
      (\ s a -> s{_prorCustomGroups = a})
      . _Default
      . _Coerce

-- | URL of an image of the item.
prorImageLink :: Lens' Product (Maybe Text)
prorImageLink
  = lens _prorImageLink
      (\ s a -> s{_prorImageLink = a})

-- | Offer margin for dynamic remarketing campaigns.
prorDisplayAdsValue :: Lens' Product (Maybe Double)
prorDisplayAdsValue
  = lens _prorDisplayAdsValue
      (\ s a -> s{_prorDisplayAdsValue = a})
      . mapping _Coerce

-- | Loyalty points that users receive after purchasing the item. Japan only.
prorLoyaltyPoints :: Lens' Product (Maybe LoyaltyPoints)
prorLoyaltyPoints
  = lens _prorLoyaltyPoints
      (\ s a -> s{_prorLoyaltyPoints = a})

-- | Additional URLs of images of the item.
prorAdditionalImageLinks :: Lens' Product [Text]
prorAdditionalImageLinks
  = lens _prorAdditionalImageLinks
      (\ s a -> s{_prorAdditionalImageLinks = a})
      . _Default
      . _Coerce

-- | The read-only list of intended destinations which passed validation.
prorValidatedDestinations :: Lens' Product [Text]
prorValidatedDestinations
  = lens _prorValidatedDestinations
      (\ s a -> s{_prorValidatedDestinations = a})
      . _Default
      . _Coerce

-- | Color of the item.
prorColor :: Lens' Product (Maybe Text)
prorColor
  = lens _prorColor (\ s a -> s{_prorColor = a})

-- | Custom label 0 for custom grouping of items in a Shopping campaign.
prorCustomLabel0 :: Lens' Product (Maybe Text)
prorCustomLabel0
  = lens _prorCustomLabel0
      (\ s a -> s{_prorCustomLabel0 = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#product\".
prorKind :: Lens' Product Text
prorKind = lens _prorKind (\ s a -> s{_prorKind = a})

-- | Minimal product handling time (in business days).
prorMinHandlingTime :: Lens' Product (Maybe Int64)
prorMinHandlingTime
  = lens _prorMinHandlingTime
      (\ s a -> s{_prorMinHandlingTime = a})
      . mapping _Coerce

-- | The number of identical products in a merchant-defined multipack.
prorMultipack :: Lens' Product (Maybe Int64)
prorMultipack
  = lens _prorMultipack
      (\ s a -> s{_prorMultipack = a})
      . mapping _Coerce

-- | The item\'s pattern (e.g. polka dots).
prorPattern :: Lens' Product (Maybe Text)
prorPattern
  = lens _prorPattern (\ s a -> s{_prorPattern = a})

-- | URL directly linking to your item\'s page on your website.
prorLink :: Lens' Product (Maybe Text)
prorLink = lens _prorLink (\ s a -> s{_prorLink = a})

-- | System in which the size is specified. Recommended for apparel items.
prorSizeSystem :: Lens' Product (Maybe Text)
prorSizeSystem
  = lens _prorSizeSystem
      (\ s a -> s{_prorSizeSystem = a})

-- | The preference of the denominator of the unit price.
prorUnitPricingBaseMeasure :: Lens' Product (Maybe ProductUnitPricingBaseMeasure)
prorUnitPricingBaseMeasure
  = lens _prorUnitPricingBaseMeasure
      (\ s a -> s{_prorUnitPricingBaseMeasure = a})

-- | Tax information.
prorTaxes :: Lens' Product [ProductTax]
prorTaxes
  = lens _prorTaxes (\ s a -> s{_prorTaxes = a}) .
      _Default
      . _Coerce

-- | The material of which the item is made.
prorMaterial :: Lens' Product (Maybe Text)
prorMaterial
  = lens _prorMaterial (\ s a -> s{_prorMaterial = a})

-- | Number and amount of installments to pay for an item. Brazil only.
prorInstallment :: Lens' Product (Maybe Installment)
prorInstallment
  = lens _prorInstallment
      (\ s a -> s{_prorInstallment = a})

-- | The item\'s channel (online or local).
prorChannel :: Lens' Product (Maybe Text)
prorChannel
  = lens _prorChannel (\ s a -> s{_prorChannel = a})

-- | Your category of the item (formatted as in products feed specification).
prorProductType :: Lens' Product (Maybe Text)
prorProductType
  = lens _prorProductType
      (\ s a -> s{_prorProductType = a})

-- | False when the item does not have unique product identifiers appropriate
-- to its category, such as GTIN, MPN, and brand. Required according to the
-- Unique Product Identifier Rules for all target countries except for
-- Canada.
prorIdentifierExists :: Lens' Product (Maybe Bool)
prorIdentifierExists
  = lens _prorIdentifierExists
      (\ s a -> s{_prorIdentifierExists = a})

-- | Whether an item is available for purchase only online.
prorOnlineOnly :: Lens' Product (Maybe Bool)
prorOnlineOnly
  = lens _prorOnlineOnly
      (\ s a -> s{_prorOnlineOnly = a})

-- | Brand of the item.
prorBrand :: Lens' Product (Maybe Text)
prorBrand
  = lens _prorBrand (\ s a -> s{_prorBrand = a})

-- | The measure and dimension of an item.
prorUnitPricingMeasure :: Lens' Product (Maybe ProductUnitPricingMeasure)
prorUnitPricingMeasure
  = lens _prorUnitPricingMeasure
      (\ s a -> s{_prorUnitPricingMeasure = a})

-- | Advertised sale price of the item.
prorSalePrice :: Lens' Product (Maybe Price)
prorSalePrice
  = lens _prorSalePrice
      (\ s a -> s{_prorSalePrice = a})

-- | Length of the item for shipping.
prorShippingLength :: Lens' Product (Maybe ProductShippingDimension)
prorShippingLength
  = lens _prorShippingLength
      (\ s a -> s{_prorShippingLength = a})

-- | Custom label 3 for custom grouping of items in a Shopping campaign.
prorCustomLabel3 :: Lens' Product (Maybe Text)
prorCustomLabel3
  = lens _prorCustomLabel3
      (\ s a -> s{_prorCustomLabel3 = a})

-- | Maximal product handling time (in business days).
prorMaxHandlingTime :: Lens' Product (Maybe Int64)
prorMaxHandlingTime
  = lens _prorMaxHandlingTime
      (\ s a -> s{_prorMaxHandlingTime = a})
      . mapping _Coerce

-- | Read-only warnings.
prorWarnings :: Lens' Product [Error']
prorWarnings
  = lens _prorWarnings (\ s a -> s{_prorWarnings = a})
      . _Default
      . _Coerce

-- | Additional categories of the item (formatted as in products feed
-- specification).
prorAdditionalProductTypes :: Lens' Product [Text]
prorAdditionalProductTypes
  = lens _prorAdditionalProductTypes
      (\ s a -> s{_prorAdditionalProductTypes = a})
      . _Default
      . _Coerce

-- | Availability status of the item.
prorAvailability :: Lens' Product (Maybe Text)
prorAvailability
  = lens _prorAvailability
      (\ s a -> s{_prorAvailability = a})

-- | The CLDR territory code for the item.
prorTargetCountry :: Lens' Product (Maybe Text)
prorTargetCountry
  = lens _prorTargetCountry
      (\ s a -> s{_prorTargetCountry = a})

-- | The shipping label of the product, used to group product in
-- account-level shipping rules.
prorShippingLabel :: Lens' Product (Maybe Text)
prorShippingLabel
  = lens _prorShippingLabel
      (\ s a -> s{_prorShippingLabel = a})

-- | A list of custom (merchant-provided) attributes. It can also be used for
-- submitting any attribute of the feed specification in its generic form
-- (e.g., { \"name\": \"size type\", \"type\": \"text\", \"value\":
-- \"regular\" }). This is useful for submitting attributes not explicitly
-- exposed by the API.
prorCustomAttributes :: Lens' Product [ProductCustomAttribute]
prorCustomAttributes
  = lens _prorCustomAttributes
      (\ s a -> s{_prorCustomAttributes = a})
      . _Default
      . _Coerce

-- | Global Trade Item Number (GTIN) of the item.
prorGtin :: Lens' Product (Maybe Text)
prorGtin = lens _prorGtin (\ s a -> s{_prorGtin = a})

-- | Target age group of the item.
prorAgeGroup :: Lens' Product (Maybe Text)
prorAgeGroup
  = lens _prorAgeGroup (\ s a -> s{_prorAgeGroup = a})

-- | Title of an item for dynamic remarketing campaigns.
prorDisplayAdsTitle :: Lens' Product (Maybe Text)
prorDisplayAdsTitle
  = lens _prorDisplayAdsTitle
      (\ s a -> s{_prorDisplayAdsTitle = a})

-- | Target gender of the item.
prorGender :: Lens' Product (Maybe Text)
prorGender
  = lens _prorGender (\ s a -> s{_prorGender = a})

-- | Specifies the intended destinations for the product.
prorDestinations :: Lens' Product [ProductDestination]
prorDestinations
  = lens _prorDestinations
      (\ s a -> s{_prorDestinations = a})
      . _Default
      . _Coerce

-- | Date on which the item should expire, as specified upon insertion, in
-- ISO 8601 format. The actual expiration date in Google Shopping is
-- exposed in productstatuses as googleExpirationDate and might be earlier
-- if expirationDate is too far in the future.
prorExpirationDate :: Lens' Product (Maybe Text)
prorExpirationDate
  = lens _prorExpirationDate
      (\ s a -> s{_prorExpirationDate = a})

-- | Shared identifier for all variants of the same product.
prorItemGroupId :: Lens' Product (Maybe Text)
prorItemGroupId
  = lens _prorItemGroupId
      (\ s a -> s{_prorItemGroupId = a})

-- | Used to group items in an arbitrary way. Only for CPA%, discouraged
-- otherwise.
prorAdwordsGrouping :: Lens' Product (Maybe Text)
prorAdwordsGrouping
  = lens _prorAdwordsGrouping
      (\ s a -> s{_prorAdwordsGrouping = a})

-- | Date range during which the item is on sale (see products feed
-- specification).
prorSalePriceEffectiveDate :: Lens' Product (Maybe Text)
prorSalePriceEffectiveDate
  = lens _prorSalePriceEffectiveDate
      (\ s a -> s{_prorSalePriceEffectiveDate = a})

-- | Custom label 2 for custom grouping of items in a Shopping campaign.
prorCustomLabel2 :: Lens' Product (Maybe Text)
prorCustomLabel2
  = lens _prorCustomLabel2
      (\ s a -> s{_prorCustomLabel2 = a})

-- | Google\'s category of the item (see Google product taxonomy).
prorGoogleProductCategory :: Lens' Product (Maybe Text)
prorGoogleProductCategory
  = lens _prorGoogleProductCategory
      (\ s a -> s{_prorGoogleProductCategory = a})

-- | Shipping rules.
prorShipping :: Lens' Product [ProductShipping]
prorShipping
  = lens _prorShipping (\ s a -> s{_prorShipping = a})
      . _Default
      . _Coerce

-- | Allows advertisers to override the item URL when the product is shown
-- within the context of Product Ads.
prorAdwordsRedirect :: Lens' Product (Maybe Text)
prorAdwordsRedirect
  = lens _prorAdwordsRedirect
      (\ s a -> s{_prorAdwordsRedirect = a})

-- | Weight of the item for shipping.
prorShippingWeight :: Lens' Product (Maybe ProductShippingWeight)
prorShippingWeight
  = lens _prorShippingWeight
      (\ s a -> s{_prorShippingWeight = a})

-- | The quantity of the product that is reserved for sell-on-google ads.
prorSellOnGoogleQuantity :: Lens' Product (Maybe Int64)
prorSellOnGoogleQuantity
  = lens _prorSellOnGoogleQuantity
      (\ s a -> s{_prorSellOnGoogleQuantity = a})
      . mapping _Coerce

-- | Height of the item for shipping.
prorShippingHeight :: Lens' Product (Maybe ProductShippingDimension)
prorShippingHeight
  = lens _prorShippingHeight
      (\ s a -> s{_prorShippingHeight = a})

-- | The day a pre-ordered product becomes available for delivery, in ISO
-- 8601 format.
prorAvailabilityDate :: Lens' Product (Maybe Text)
prorAvailabilityDate
  = lens _prorAvailabilityDate
      (\ s a -> s{_prorAvailabilityDate = a})

-- | A unique identifier for the item. Leading and trailing whitespaces are
-- stripped and multiple whitespaces are replaced by a single whitespace
-- upon submission. Only valid unicode characters are accepted. See the
-- products feed specification for details. Note: Content API methods that
-- operate on products take the REST id of the product, not this
-- identifier.
prorOfferId :: Lens' Product (Maybe Text)
prorOfferId
  = lens _prorOfferId (\ s a -> s{_prorOfferId = a})

-- | The REST id of the product. Content API methods that operate on products
-- take this as their productId parameter. The REST id for a product is of
-- the form channel:contentLanguage:targetCountry:offerId.
prorId :: Lens' Product (Maybe Text)
prorId = lens _prorId (\ s a -> s{_prorId = a})

-- | Similar to adwords_grouping, but only works on CPC.
prorAdwordsLabels :: Lens' Product [Text]
prorAdwordsLabels
  = lens _prorAdwordsLabels
      (\ s a -> s{_prorAdwordsLabels = a})
      . _Default
      . _Coerce

-- | Price of the item.
prorPrice :: Lens' Product (Maybe Price)
prorPrice
  = lens _prorPrice (\ s a -> s{_prorPrice = a})

-- | The unique ID of a promotion.
prorPromotionIds :: Lens' Product [Text]
prorPromotionIds
  = lens _prorPromotionIds
      (\ s a -> s{_prorPromotionIds = a})
      . _Default
      . _Coerce

-- | The cut of the item. Recommended for apparel items.
prorSizeType :: Lens' Product (Maybe Text)
prorSizeType
  = lens _prorSizeType (\ s a -> s{_prorSizeType = a})

-- | Link to a mobile-optimized version of the landing page.
prorMobileLink :: Lens' Product (Maybe Text)
prorMobileLink
  = lens _prorMobileLink
      (\ s a -> s{_prorMobileLink = a})

-- | Title of the item.
prorTitle :: Lens' Product (Maybe Text)
prorTitle
  = lens _prorTitle (\ s a -> s{_prorTitle = a})

-- | Set to true if the item is targeted towards adults.
prorAdult :: Lens' Product (Maybe Bool)
prorAdult
  = lens _prorAdult (\ s a -> s{_prorAdult = a})

-- | The two-letter ISO 639-1 language code for the item.
prorContentLanguage :: Lens' Product (Maybe Text)
prorContentLanguage
  = lens _prorContentLanguage
      (\ s a -> s{_prorContentLanguage = a})

-- | Specifies the intended aspects for the product.
prorAspects :: Lens' Product [ProductAspect]
prorAspects
  = lens _prorAspects (\ s a -> s{_prorAspects = a}) .
      _Default
      . _Coerce

-- | The energy efficiency class as defined in EU directive 2010\/30\/EU.
prorEnergyEfficiencyClass :: Lens' Product (Maybe Text)
prorEnergyEfficiencyClass
  = lens _prorEnergyEfficiencyClass
      (\ s a -> s{_prorEnergyEfficiencyClass = a})

-- | Advertiser-specified recommendations.
prorDisplayAdsSimilarIds :: Lens' Product [Text]
prorDisplayAdsSimilarIds
  = lens _prorDisplayAdsSimilarIds
      (\ s a -> s{_prorDisplayAdsSimilarIds = a})
      . _Default
      . _Coerce

-- | Manufacturer Part Number (MPN) of the item.
prorMpn :: Lens' Product (Maybe Text)
prorMpn = lens _prorMpn (\ s a -> s{_prorMpn = a})

-- | Condition or state of the item.
prorCondition :: Lens' Product (Maybe Text)
prorCondition
  = lens _prorCondition
      (\ s a -> s{_prorCondition = a})

-- | Size of the item.
prorSizes :: Lens' Product [Text]
prorSizes
  = lens _prorSizes (\ s a -> s{_prorSizes = a}) .
      _Default
      . _Coerce

-- | Whether the item is a merchant-defined bundle. A bundle is a custom
-- grouping of different products sold by a merchant for a single price.
prorIsBundle :: Lens' Product (Maybe Bool)
prorIsBundle
  = lens _prorIsBundle (\ s a -> s{_prorIsBundle = a})

-- | Description of the item.
prorDescription :: Lens' Product (Maybe Text)
prorDescription
  = lens _prorDescription
      (\ s a -> s{_prorDescription = a})

-- | Custom label 4 for custom grouping of items in a Shopping campaign.
prorCustomLabel4 :: Lens' Product (Maybe Text)
prorCustomLabel4
  = lens _prorCustomLabel4
      (\ s a -> s{_prorCustomLabel4 = a})

-- | An identifier for an item for dynamic remarketing campaigns.
prorDisplayAdsId :: Lens' Product (Maybe Text)
prorDisplayAdsId
  = lens _prorDisplayAdsId
      (\ s a -> s{_prorDisplayAdsId = a})

instance FromJSON Product where
        parseJSON
          = withObject "Product"
              (\ o ->
                 Product' <$>
                   (o .:? "displayAdsLink") <*> (o .:? "customLabel1")
                     <*> (o .:? "shippingWidth")
                     <*> (o .:? "customGroups" .!= mempty)
                     <*> (o .:? "imageLink")
                     <*> (o .:? "displayAdsValue")
                     <*> (o .:? "loyaltyPoints")
                     <*> (o .:? "additionalImageLinks" .!= mempty)
                     <*> (o .:? "validatedDestinations" .!= mempty)
                     <*> (o .:? "color")
                     <*> (o .:? "customLabel0")
                     <*> (o .:? "kind" .!= "content#product")
                     <*> (o .:? "minHandlingTime")
                     <*> (o .:? "multipack")
                     <*> (o .:? "pattern")
                     <*> (o .:? "link")
                     <*> (o .:? "sizeSystem")
                     <*> (o .:? "unitPricingBaseMeasure")
                     <*> (o .:? "taxes" .!= mempty)
                     <*> (o .:? "material")
                     <*> (o .:? "installment")
                     <*> (o .:? "channel")
                     <*> (o .:? "productType")
                     <*> (o .:? "identifierExists")
                     <*> (o .:? "onlineOnly")
                     <*> (o .:? "brand")
                     <*> (o .:? "unitPricingMeasure")
                     <*> (o .:? "salePrice")
                     <*> (o .:? "shippingLength")
                     <*> (o .:? "customLabel3")
                     <*> (o .:? "maxHandlingTime")
                     <*> (o .:? "warnings" .!= mempty)
                     <*> (o .:? "additionalProductTypes" .!= mempty)
                     <*> (o .:? "availability")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "shippingLabel")
                     <*> (o .:? "customAttributes" .!= mempty)
                     <*> (o .:? "gtin")
                     <*> (o .:? "ageGroup")
                     <*> (o .:? "displayAdsTitle")
                     <*> (o .:? "gender")
                     <*> (o .:? "destinations" .!= mempty)
                     <*> (o .:? "expirationDate")
                     <*> (o .:? "itemGroupId")
                     <*> (o .:? "adwordsGrouping")
                     <*> (o .:? "salePriceEffectiveDate")
                     <*> (o .:? "customLabel2")
                     <*> (o .:? "googleProductCategory")
                     <*> (o .:? "shipping" .!= mempty)
                     <*> (o .:? "adwordsRedirect")
                     <*> (o .:? "shippingWeight")
                     <*> (o .:? "sellOnGoogleQuantity")
                     <*> (o .:? "shippingHeight")
                     <*> (o .:? "availabilityDate")
                     <*> (o .:? "offerId")
                     <*> (o .:? "id")
                     <*> (o .:? "adwordsLabels" .!= mempty)
                     <*> (o .:? "price")
                     <*> (o .:? "promotionIds" .!= mempty)
                     <*> (o .:? "sizeType")
                     <*> (o .:? "mobileLink")
                     <*> (o .:? "title")
                     <*> (o .:? "adult")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "aspects" .!= mempty)
                     <*> (o .:? "energyEfficiencyClass")
                     <*> (o .:? "displayAdsSimilarIds" .!= mempty)
                     <*> (o .:? "mpn")
                     <*> (o .:? "condition")
                     <*> (o .:? "sizes" .!= mempty)
                     <*> (o .:? "isBundle")
                     <*> (o .:? "description")
                     <*> (o .:? "customLabel4")
                     <*> (o .:? "displayAdsId"))

instance ToJSON Product where
        toJSON Product'{..}
          = object
              (catMaybes
                 [("displayAdsLink" .=) <$> _prorDisplayAdsLink,
                  ("customLabel1" .=) <$> _prorCustomLabel1,
                  ("shippingWidth" .=) <$> _prorShippingWidth,
                  ("customGroups" .=) <$> _prorCustomGroups,
                  ("imageLink" .=) <$> _prorImageLink,
                  ("displayAdsValue" .=) <$> _prorDisplayAdsValue,
                  ("loyaltyPoints" .=) <$> _prorLoyaltyPoints,
                  ("additionalImageLinks" .=) <$>
                    _prorAdditionalImageLinks,
                  ("validatedDestinations" .=) <$>
                    _prorValidatedDestinations,
                  ("color" .=) <$> _prorColor,
                  ("customLabel0" .=) <$> _prorCustomLabel0,
                  Just ("kind" .= _prorKind),
                  ("minHandlingTime" .=) <$> _prorMinHandlingTime,
                  ("multipack" .=) <$> _prorMultipack,
                  ("pattern" .=) <$> _prorPattern,
                  ("link" .=) <$> _prorLink,
                  ("sizeSystem" .=) <$> _prorSizeSystem,
                  ("unitPricingBaseMeasure" .=) <$>
                    _prorUnitPricingBaseMeasure,
                  ("taxes" .=) <$> _prorTaxes,
                  ("material" .=) <$> _prorMaterial,
                  ("installment" .=) <$> _prorInstallment,
                  ("channel" .=) <$> _prorChannel,
                  ("productType" .=) <$> _prorProductType,
                  ("identifierExists" .=) <$> _prorIdentifierExists,
                  ("onlineOnly" .=) <$> _prorOnlineOnly,
                  ("brand" .=) <$> _prorBrand,
                  ("unitPricingMeasure" .=) <$>
                    _prorUnitPricingMeasure,
                  ("salePrice" .=) <$> _prorSalePrice,
                  ("shippingLength" .=) <$> _prorShippingLength,
                  ("customLabel3" .=) <$> _prorCustomLabel3,
                  ("maxHandlingTime" .=) <$> _prorMaxHandlingTime,
                  ("warnings" .=) <$> _prorWarnings,
                  ("additionalProductTypes" .=) <$>
                    _prorAdditionalProductTypes,
                  ("availability" .=) <$> _prorAvailability,
                  ("targetCountry" .=) <$> _prorTargetCountry,
                  ("shippingLabel" .=) <$> _prorShippingLabel,
                  ("customAttributes" .=) <$> _prorCustomAttributes,
                  ("gtin" .=) <$> _prorGtin,
                  ("ageGroup" .=) <$> _prorAgeGroup,
                  ("displayAdsTitle" .=) <$> _prorDisplayAdsTitle,
                  ("gender" .=) <$> _prorGender,
                  ("destinations" .=) <$> _prorDestinations,
                  ("expirationDate" .=) <$> _prorExpirationDate,
                  ("itemGroupId" .=) <$> _prorItemGroupId,
                  ("adwordsGrouping" .=) <$> _prorAdwordsGrouping,
                  ("salePriceEffectiveDate" .=) <$>
                    _prorSalePriceEffectiveDate,
                  ("customLabel2" .=) <$> _prorCustomLabel2,
                  ("googleProductCategory" .=) <$>
                    _prorGoogleProductCategory,
                  ("shipping" .=) <$> _prorShipping,
                  ("adwordsRedirect" .=) <$> _prorAdwordsRedirect,
                  ("shippingWeight" .=) <$> _prorShippingWeight,
                  ("sellOnGoogleQuantity" .=) <$>
                    _prorSellOnGoogleQuantity,
                  ("shippingHeight" .=) <$> _prorShippingHeight,
                  ("availabilityDate" .=) <$> _prorAvailabilityDate,
                  ("offerId" .=) <$> _prorOfferId,
                  ("id" .=) <$> _prorId,
                  ("adwordsLabels" .=) <$> _prorAdwordsLabels,
                  ("price" .=) <$> _prorPrice,
                  ("promotionIds" .=) <$> _prorPromotionIds,
                  ("sizeType" .=) <$> _prorSizeType,
                  ("mobileLink" .=) <$> _prorMobileLink,
                  ("title" .=) <$> _prorTitle,
                  ("adult" .=) <$> _prorAdult,
                  ("contentLanguage" .=) <$> _prorContentLanguage,
                  ("aspects" .=) <$> _prorAspects,
                  ("energyEfficiencyClass" .=) <$>
                    _prorEnergyEfficiencyClass,
                  ("displayAdsSimilarIds" .=) <$>
                    _prorDisplayAdsSimilarIds,
                  ("mpn" .=) <$> _prorMpn,
                  ("condition" .=) <$> _prorCondition,
                  ("sizes" .=) <$> _prorSizes,
                  ("isBundle" .=) <$> _prorIsBundle,
                  ("description" .=) <$> _prorDescription,
                  ("customLabel4" .=) <$> _prorCustomLabel4,
                  ("displayAdsId" .=) <$> _prorDisplayAdsId])

-- | A list of errors returned by a failed batch entry.
--
-- /See:/ 'errors' smart constructor.
data Errors = Errors'
    { _errCode :: !(Maybe (Textual Word32))
    , _errMessage :: !(Maybe Text)
    , _errErrors :: !(Maybe [Error'])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Errors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'errCode'
--
-- * 'errMessage'
--
-- * 'errErrors'
errors
    :: Errors
errors = 
    Errors'
    { _errCode = Nothing
    , _errMessage = Nothing
    , _errErrors = Nothing
    }

-- | The HTTP status of the first error in errors.
errCode :: Lens' Errors (Maybe Word32)
errCode
  = lens _errCode (\ s a -> s{_errCode = a}) .
      mapping _Coerce

-- | The message of the first error in errors.
errMessage :: Lens' Errors (Maybe Text)
errMessage
  = lens _errMessage (\ s a -> s{_errMessage = a})

-- | A list of errors.
errErrors :: Lens' Errors [Error']
errErrors
  = lens _errErrors (\ s a -> s{_errErrors = a}) .
      _Default
      . _Coerce

instance FromJSON Errors where
        parseJSON
          = withObject "Errors"
              (\ o ->
                 Errors' <$>
                   (o .:? "code") <*> (o .:? "message") <*>
                     (o .:? "errors" .!= mempty))

instance ToJSON Errors where
        toJSON Errors'{..}
          = object
              (catMaybes
                 [("code" .=) <$> _errCode,
                  ("message" .=) <$> _errMessage,
                  ("errors" .=) <$> _errErrors])

-- | A batch entry encoding a single non-batch accountstatuses response.
--
-- /See:/ 'accountstatusesCustomBatchResponseEntry' smart constructor.
data AccountstatusesCustomBatchResponseEntry = AccountstatusesCustomBatchResponseEntry'
    { _aaAccountStatus :: !(Maybe AccountStatus)
    , _aaErrors :: !(Maybe Errors)
    , _aaBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaAccountStatus'
--
-- * 'aaErrors'
--
-- * 'aaBatchId'
accountstatusesCustomBatchResponseEntry
    :: AccountstatusesCustomBatchResponseEntry
accountstatusesCustomBatchResponseEntry = 
    AccountstatusesCustomBatchResponseEntry'
    { _aaAccountStatus = Nothing
    , _aaErrors = Nothing
    , _aaBatchId = Nothing
    }

-- | The requested account status. Defined if and only if the request was
-- successful.
aaAccountStatus :: Lens' AccountstatusesCustomBatchResponseEntry (Maybe AccountStatus)
aaAccountStatus
  = lens _aaAccountStatus
      (\ s a -> s{_aaAccountStatus = a})

-- | A list of errors defined if and only if the request failed.
aaErrors :: Lens' AccountstatusesCustomBatchResponseEntry (Maybe Errors)
aaErrors = lens _aaErrors (\ s a -> s{_aaErrors = a})

-- | The ID of the request entry this entry responds to.
aaBatchId :: Lens' AccountstatusesCustomBatchResponseEntry (Maybe Word32)
aaBatchId
  = lens _aaBatchId (\ s a -> s{_aaBatchId = a}) .
      mapping _Coerce

instance FromJSON
         AccountstatusesCustomBatchResponseEntry where
        parseJSON
          = withObject
              "AccountstatusesCustomBatchResponseEntry"
              (\ o ->
                 AccountstatusesCustomBatchResponseEntry' <$>
                   (o .:? "accountStatus") <*> (o .:? "errors") <*>
                     (o .:? "batchId"))

instance ToJSON
         AccountstatusesCustomBatchResponseEntry where
        toJSON AccountstatusesCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [("accountStatus" .=) <$> _aaAccountStatus,
                  ("errors" .=) <$> _aaErrors,
                  ("batchId" .=) <$> _aaBatchId])

--
-- /See:/ 'inventorySetResponse' smart constructor.
newtype InventorySetResponse = InventorySetResponse'
    { _isrKind :: Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventorySetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrKind'
inventorySetResponse
    :: InventorySetResponse
inventorySetResponse = 
    InventorySetResponse'
    { _isrKind = "content#inventorySetResponse"
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#inventorySetResponse\".
isrKind :: Lens' InventorySetResponse Text
isrKind = lens _isrKind (\ s a -> s{_isrKind = a})

instance FromJSON InventorySetResponse where
        parseJSON
          = withObject "InventorySetResponse"
              (\ o ->
                 InventorySetResponse' <$>
                   (o .:? "kind" .!= "content#inventorySetResponse"))

instance ToJSON InventorySetResponse where
        toJSON InventorySetResponse'{..}
          = object (catMaybes [Just ("kind" .= _isrKind)])

--
-- /See:/ 'ordersCancelLineItemResponse' smart constructor.
data OrdersCancelLineItemResponse = OrdersCancelLineItemResponse'
    { _oclirKind :: !Text
    , _oclirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancelLineItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oclirKind'
--
-- * 'oclirExecutionStatus'
ordersCancelLineItemResponse
    :: OrdersCancelLineItemResponse
ordersCancelLineItemResponse = 
    OrdersCancelLineItemResponse'
    { _oclirKind = "content#ordersCancelLineItemResponse"
    , _oclirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersCancelLineItemResponse\".
oclirKind :: Lens' OrdersCancelLineItemResponse Text
oclirKind
  = lens _oclirKind (\ s a -> s{_oclirKind = a})

-- | The status of the execution.
oclirExecutionStatus :: Lens' OrdersCancelLineItemResponse (Maybe Text)
oclirExecutionStatus
  = lens _oclirExecutionStatus
      (\ s a -> s{_oclirExecutionStatus = a})

instance FromJSON OrdersCancelLineItemResponse where
        parseJSON
          = withObject "OrdersCancelLineItemResponse"
              (\ o ->
                 OrdersCancelLineItemResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersCancelLineItemResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersCancelLineItemResponse where
        toJSON OrdersCancelLineItemResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oclirKind),
                  ("executionStatus" .=) <$> _oclirExecutionStatus])

--
-- /See:/ 'holidayCutoff' smart constructor.
data HolidayCutoff = HolidayCutoff'
    { _hcDeadlineHour :: !(Maybe (Textual Word32))
    , _hcDeadlineTimezone :: !(Maybe Text)
    , _hcVisibleFromDate :: !(Maybe Text)
    , _hcHolidayId :: !(Maybe Text)
    , _hcDeadlineDate :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'HolidayCutoff' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcDeadlineHour'
--
-- * 'hcDeadlineTimezone'
--
-- * 'hcVisibleFromDate'
--
-- * 'hcHolidayId'
--
-- * 'hcDeadlineDate'
holidayCutoff
    :: HolidayCutoff
holidayCutoff = 
    HolidayCutoff'
    { _hcDeadlineHour = Nothing
    , _hcDeadlineTimezone = Nothing
    , _hcVisibleFromDate = Nothing
    , _hcHolidayId = Nothing
    , _hcDeadlineDate = Nothing
    }

-- | Hour of the day on the deadline date until which the order has to be
-- placed to qualify for the delivery guarantee. Possible values are: 0
-- (midnight), 1, ..., 12 (noon), 13, ..., 23. Required.
hcDeadlineHour :: Lens' HolidayCutoff (Maybe Word32)
hcDeadlineHour
  = lens _hcDeadlineHour
      (\ s a -> s{_hcDeadlineHour = a})
      . mapping _Coerce

-- | Timezone identifier for the deadline hour. A list of identifiers can be
-- found in the AdWords API documentation. E.g. \"Europe\/Zurich\".
-- Required.
hcDeadlineTimezone :: Lens' HolidayCutoff (Maybe Text)
hcDeadlineTimezone
  = lens _hcDeadlineTimezone
      (\ s a -> s{_hcDeadlineTimezone = a})

-- | Date on which the deadline will become visible to consumers in ISO 8601
-- format. E.g. \"2016-10-31\" for 31st October 2016. Required.
hcVisibleFromDate :: Lens' HolidayCutoff (Maybe Text)
hcVisibleFromDate
  = lens _hcVisibleFromDate
      (\ s a -> s{_hcVisibleFromDate = a})

-- | Unique identifier for the holiday. Required.
hcHolidayId :: Lens' HolidayCutoff (Maybe Text)
hcHolidayId
  = lens _hcHolidayId (\ s a -> s{_hcHolidayId = a})

-- | Date of the order deadline, in ISO 8601 format. E.g. \"2016-11-29\" for
-- 29th November 2016. Required.
hcDeadlineDate :: Lens' HolidayCutoff (Maybe Text)
hcDeadlineDate
  = lens _hcDeadlineDate
      (\ s a -> s{_hcDeadlineDate = a})

instance FromJSON HolidayCutoff where
        parseJSON
          = withObject "HolidayCutoff"
              (\ o ->
                 HolidayCutoff' <$>
                   (o .:? "deadlineHour") <*> (o .:? "deadlineTimezone")
                     <*> (o .:? "visibleFromDate")
                     <*> (o .:? "holidayId")
                     <*> (o .:? "deadlineDate"))

instance ToJSON HolidayCutoff where
        toJSON HolidayCutoff'{..}
          = object
              (catMaybes
                 [("deadlineHour" .=) <$> _hcDeadlineHour,
                  ("deadlineTimezone" .=) <$> _hcDeadlineTimezone,
                  ("visibleFromDate" .=) <$> _hcVisibleFromDate,
                  ("holidayId" .=) <$> _hcHolidayId,
                  ("deadlineDate" .=) <$> _hcDeadlineDate])

--
-- /See:/ 'testOrderLineItem' smart constructor.
data TestOrderLineItem = TestOrderLineItem'
    { _toliQuantityOrdered :: !(Maybe (Textual Word32))
    , _toliReturnInfo :: !(Maybe OrderLineItemReturnInfo)
    , _toliShippingDetails :: !(Maybe OrderLineItemShippingDetails)
    , _toliProduct :: !(Maybe TestOrderLineItemProduct)
    , _toliUnitTax :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestOrderLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toliQuantityOrdered'
--
-- * 'toliReturnInfo'
--
-- * 'toliShippingDetails'
--
-- * 'toliProduct'
--
-- * 'toliUnitTax'
testOrderLineItem
    :: TestOrderLineItem
testOrderLineItem = 
    TestOrderLineItem'
    { _toliQuantityOrdered = Nothing
    , _toliReturnInfo = Nothing
    , _toliShippingDetails = Nothing
    , _toliProduct = Nothing
    , _toliUnitTax = Nothing
    }

-- | Number of items ordered.
toliQuantityOrdered :: Lens' TestOrderLineItem (Maybe Word32)
toliQuantityOrdered
  = lens _toliQuantityOrdered
      (\ s a -> s{_toliQuantityOrdered = a})
      . mapping _Coerce

-- | Details of the return policy for the line item.
toliReturnInfo :: Lens' TestOrderLineItem (Maybe OrderLineItemReturnInfo)
toliReturnInfo
  = lens _toliReturnInfo
      (\ s a -> s{_toliReturnInfo = a})

-- | Details of the requested shipping for the line item.
toliShippingDetails :: Lens' TestOrderLineItem (Maybe OrderLineItemShippingDetails)
toliShippingDetails
  = lens _toliShippingDetails
      (\ s a -> s{_toliShippingDetails = a})

-- | Product data from the time of the order placement.
toliProduct :: Lens' TestOrderLineItem (Maybe TestOrderLineItemProduct)
toliProduct
  = lens _toliProduct (\ s a -> s{_toliProduct = a})

-- | Unit tax for the line item.
toliUnitTax :: Lens' TestOrderLineItem (Maybe Price)
toliUnitTax
  = lens _toliUnitTax (\ s a -> s{_toliUnitTax = a})

instance FromJSON TestOrderLineItem where
        parseJSON
          = withObject "TestOrderLineItem"
              (\ o ->
                 TestOrderLineItem' <$>
                   (o .:? "quantityOrdered") <*> (o .:? "returnInfo")
                     <*> (o .:? "shippingDetails")
                     <*> (o .:? "product")
                     <*> (o .:? "unitTax"))

instance ToJSON TestOrderLineItem where
        toJSON TestOrderLineItem'{..}
          = object
              (catMaybes
                 [("quantityOrdered" .=) <$> _toliQuantityOrdered,
                  ("returnInfo" .=) <$> _toliReturnInfo,
                  ("shippingDetails" .=) <$> _toliShippingDetails,
                  ("product" .=) <$> _toliProduct,
                  ("unitTax" .=) <$> _toliUnitTax])

-- | A batch entry encoding a single non-batch productstatuses request.
--
-- /See:/ 'productstatusesCustomBatchRequestEntry' smart constructor.
data ProductstatusesCustomBatchRequestEntry = ProductstatusesCustomBatchRequestEntry'
    { _p2MerchantId :: !(Maybe (Textual Word64))
    , _p2Method :: !(Maybe Text)
    , _p2IncludeAttributes :: !(Maybe Bool)
    , _p2ProductId :: !(Maybe Text)
    , _p2BatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'p2MerchantId'
--
-- * 'p2Method'
--
-- * 'p2IncludeAttributes'
--
-- * 'p2ProductId'
--
-- * 'p2BatchId'
productstatusesCustomBatchRequestEntry
    :: ProductstatusesCustomBatchRequestEntry
productstatusesCustomBatchRequestEntry = 
    ProductstatusesCustomBatchRequestEntry'
    { _p2MerchantId = Nothing
    , _p2Method = Nothing
    , _p2IncludeAttributes = Nothing
    , _p2ProductId = Nothing
    , _p2BatchId = Nothing
    }

-- | The ID of the managing account.
p2MerchantId :: Lens' ProductstatusesCustomBatchRequestEntry (Maybe Word64)
p2MerchantId
  = lens _p2MerchantId (\ s a -> s{_p2MerchantId = a})
      . mapping _Coerce

p2Method :: Lens' ProductstatusesCustomBatchRequestEntry (Maybe Text)
p2Method = lens _p2Method (\ s a -> s{_p2Method = a})

p2IncludeAttributes :: Lens' ProductstatusesCustomBatchRequestEntry (Maybe Bool)
p2IncludeAttributes
  = lens _p2IncludeAttributes
      (\ s a -> s{_p2IncludeAttributes = a})

-- | The ID of the product whose status to get.
p2ProductId :: Lens' ProductstatusesCustomBatchRequestEntry (Maybe Text)
p2ProductId
  = lens _p2ProductId (\ s a -> s{_p2ProductId = a})

-- | An entry ID, unique within the batch request.
p2BatchId :: Lens' ProductstatusesCustomBatchRequestEntry (Maybe Word32)
p2BatchId
  = lens _p2BatchId (\ s a -> s{_p2BatchId = a}) .
      mapping _Coerce

instance FromJSON
         ProductstatusesCustomBatchRequestEntry where
        parseJSON
          = withObject "ProductstatusesCustomBatchRequestEntry"
              (\ o ->
                 ProductstatusesCustomBatchRequestEntry' <$>
                   (o .:? "merchantId") <*> (o .:? "method") <*>
                     (o .:? "includeAttributes")
                     <*> (o .:? "productId")
                     <*> (o .:? "batchId"))

instance ToJSON
         ProductstatusesCustomBatchRequestEntry where
        toJSON ProductstatusesCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("merchantId" .=) <$> _p2MerchantId,
                  ("method" .=) <$> _p2Method,
                  ("includeAttributes" .=) <$> _p2IncludeAttributes,
                  ("productId" .=) <$> _p2ProductId,
                  ("batchId" .=) <$> _p2BatchId])

--
-- /See:/ 'shippingSettingsCustomBatchResponse' smart constructor.
data ShippingSettingsCustomBatchResponse = ShippingSettingsCustomBatchResponse'
    { _shiEntries :: !(Maybe [ShippingSettingsCustomBatchResponseEntry])
    , _shiKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shiEntries'
--
-- * 'shiKind'
shippingSettingsCustomBatchResponse
    :: ShippingSettingsCustomBatchResponse
shippingSettingsCustomBatchResponse = 
    ShippingSettingsCustomBatchResponse'
    { _shiEntries = Nothing
    , _shiKind = "content#shippingsettingsCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
shiEntries :: Lens' ShippingSettingsCustomBatchResponse [ShippingSettingsCustomBatchResponseEntry]
shiEntries
  = lens _shiEntries (\ s a -> s{_shiEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#shippingsettingsCustomBatchResponse\".
shiKind :: Lens' ShippingSettingsCustomBatchResponse Text
shiKind = lens _shiKind (\ s a -> s{_shiKind = a})

instance FromJSON ShippingSettingsCustomBatchResponse
         where
        parseJSON
          = withObject "ShippingSettingsCustomBatchResponse"
              (\ o ->
                 ShippingSettingsCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#shippingsettingsCustomBatchResponse"))

instance ToJSON ShippingSettingsCustomBatchResponse
         where
        toJSON ShippingSettingsCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _shiEntries,
                  Just ("kind" .= _shiKind)])

--
-- /See:/ 'ordersReturnRefundLineItemRequest' smart constructor.
data OrdersReturnRefundLineItemRequest = OrdersReturnRefundLineItemRequest'
    { _orrlirrQuantity :: !(Maybe (Textual Word32))
    , _orrlirrLineItemId :: !(Maybe Text)
    , _orrlirrReason :: !(Maybe Text)
    , _orrlirrOperationId :: !(Maybe Text)
    , _orrlirrAmountPretax :: !(Maybe Price)
    , _orrlirrProductId :: !(Maybe Text)
    , _orrlirrAmountTax :: !(Maybe Price)
    , _orrlirrReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnRefundLineItemRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrlirrQuantity'
--
-- * 'orrlirrLineItemId'
--
-- * 'orrlirrReason'
--
-- * 'orrlirrOperationId'
--
-- * 'orrlirrAmountPretax'
--
-- * 'orrlirrProductId'
--
-- * 'orrlirrAmountTax'
--
-- * 'orrlirrReasonText'
ordersReturnRefundLineItemRequest
    :: OrdersReturnRefundLineItemRequest
ordersReturnRefundLineItemRequest = 
    OrdersReturnRefundLineItemRequest'
    { _orrlirrQuantity = Nothing
    , _orrlirrLineItemId = Nothing
    , _orrlirrReason = Nothing
    , _orrlirrOperationId = Nothing
    , _orrlirrAmountPretax = Nothing
    , _orrlirrProductId = Nothing
    , _orrlirrAmountTax = Nothing
    , _orrlirrReasonText = Nothing
    }

-- | The quantity to return and refund.
orrlirrQuantity :: Lens' OrdersReturnRefundLineItemRequest (Maybe Word32)
orrlirrQuantity
  = lens _orrlirrQuantity
      (\ s a -> s{_orrlirrQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
orrlirrLineItemId :: Lens' OrdersReturnRefundLineItemRequest (Maybe Text)
orrlirrLineItemId
  = lens _orrlirrLineItemId
      (\ s a -> s{_orrlirrLineItemId = a})

-- | The reason for the return.
orrlirrReason :: Lens' OrdersReturnRefundLineItemRequest (Maybe Text)
orrlirrReason
  = lens _orrlirrReason
      (\ s a -> s{_orrlirrReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
orrlirrOperationId :: Lens' OrdersReturnRefundLineItemRequest (Maybe Text)
orrlirrOperationId
  = lens _orrlirrOperationId
      (\ s a -> s{_orrlirrOperationId = a})

-- | The amount that is refunded. Optional, but if filled then both
-- amountPretax and amountTax must be set.
orrlirrAmountPretax :: Lens' OrdersReturnRefundLineItemRequest (Maybe Price)
orrlirrAmountPretax
  = lens _orrlirrAmountPretax
      (\ s a -> s{_orrlirrAmountPretax = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
orrlirrProductId :: Lens' OrdersReturnRefundLineItemRequest (Maybe Text)
orrlirrProductId
  = lens _orrlirrProductId
      (\ s a -> s{_orrlirrProductId = a})

-- | Tax amount that correspond to refund amount in amountPretax.
orrlirrAmountTax :: Lens' OrdersReturnRefundLineItemRequest (Maybe Price)
orrlirrAmountTax
  = lens _orrlirrAmountTax
      (\ s a -> s{_orrlirrAmountTax = a})

-- | The explanation of the reason.
orrlirrReasonText :: Lens' OrdersReturnRefundLineItemRequest (Maybe Text)
orrlirrReasonText
  = lens _orrlirrReasonText
      (\ s a -> s{_orrlirrReasonText = a})

instance FromJSON OrdersReturnRefundLineItemRequest
         where
        parseJSON
          = withObject "OrdersReturnRefundLineItemRequest"
              (\ o ->
                 OrdersReturnRefundLineItemRequest' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "operationId")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersReturnRefundLineItemRequest
         where
        toJSON OrdersReturnRefundLineItemRequest'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _orrlirrQuantity,
                  ("lineItemId" .=) <$> _orrlirrLineItemId,
                  ("reason" .=) <$> _orrlirrReason,
                  ("operationId" .=) <$> _orrlirrOperationId,
                  ("amountPretax" .=) <$> _orrlirrAmountPretax,
                  ("productId" .=) <$> _orrlirrProductId,
                  ("amountTax" .=) <$> _orrlirrAmountTax,
                  ("reasonText" .=) <$> _orrlirrReasonText])

--
-- /See:/ 'ordersCustomBatchRequestEntryShipLineItemsShipmentInfo' smart constructor.
data OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo = OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo'
    { _ocbreslisiCarrier :: !(Maybe Text)
    , _ocbreslisiTrackingId :: !(Maybe Text)
    , _ocbreslisiShipmentId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreslisiCarrier'
--
-- * 'ocbreslisiTrackingId'
--
-- * 'ocbreslisiShipmentId'
ordersCustomBatchRequestEntryShipLineItemsShipmentInfo
    :: OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
ordersCustomBatchRequestEntryShipLineItemsShipmentInfo = 
    OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo'
    { _ocbreslisiCarrier = Nothing
    , _ocbreslisiTrackingId = Nothing
    , _ocbreslisiShipmentId = Nothing
    }

-- | The carrier handling the shipment. See shipments[].carrier in the Orders
-- resource representation for a list of acceptable values.
ocbreslisiCarrier :: Lens' OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo (Maybe Text)
ocbreslisiCarrier
  = lens _ocbreslisiCarrier
      (\ s a -> s{_ocbreslisiCarrier = a})

-- | The tracking id for the shipment.
ocbreslisiTrackingId :: Lens' OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo (Maybe Text)
ocbreslisiTrackingId
  = lens _ocbreslisiTrackingId
      (\ s a -> s{_ocbreslisiTrackingId = a})

-- | The ID of the shipment.
ocbreslisiShipmentId :: Lens' OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo (Maybe Text)
ocbreslisiShipmentId
  = lens _ocbreslisiShipmentId
      (\ s a -> s{_ocbreslisiShipmentId = a})

instance FromJSON
         OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
         where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo"
              (\ o ->
                 OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo'
                   <$>
                   (o .:? "carrier") <*> (o .:? "trackingId") <*>
                     (o .:? "shipmentId"))

instance ToJSON
         OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
         where
        toJSON
          OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _ocbreslisiCarrier,
                  ("trackingId" .=) <$> _ocbreslisiTrackingId,
                  ("shipmentId" .=) <$> _ocbreslisiShipmentId])

--
-- /See:/ 'productAspect' smart constructor.
data ProductAspect = ProductAspect'
    { _paIntention :: !(Maybe Text)
    , _paAspectName :: !(Maybe Text)
    , _paDestinationName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductAspect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paIntention'
--
-- * 'paAspectName'
--
-- * 'paDestinationName'
productAspect
    :: ProductAspect
productAspect = 
    ProductAspect'
    { _paIntention = Nothing
    , _paAspectName = Nothing
    , _paDestinationName = Nothing
    }

-- | Whether the aspect is required, excluded or should be validated.
paIntention :: Lens' ProductAspect (Maybe Text)
paIntention
  = lens _paIntention (\ s a -> s{_paIntention = a})

-- | The name of the aspect.
paAspectName :: Lens' ProductAspect (Maybe Text)
paAspectName
  = lens _paAspectName (\ s a -> s{_paAspectName = a})

-- | The name of the destination. Leave out to apply to all destinations.
paDestinationName :: Lens' ProductAspect (Maybe Text)
paDestinationName
  = lens _paDestinationName
      (\ s a -> s{_paDestinationName = a})

instance FromJSON ProductAspect where
        parseJSON
          = withObject "ProductAspect"
              (\ o ->
                 ProductAspect' <$>
                   (o .:? "intention") <*> (o .:? "aspectName") <*>
                     (o .:? "destinationName"))

instance ToJSON ProductAspect where
        toJSON ProductAspect'{..}
          = object
              (catMaybes
                 [("intention" .=) <$> _paIntention,
                  ("aspectName" .=) <$> _paAspectName,
                  ("destinationName" .=) <$> _paDestinationName])

--
-- /See:/ 'datafeedTarget' smart constructor.
data DatafeedTarget = DatafeedTarget'
    { _dtIncludedDestinations :: !(Maybe [Text])
    , _dtExcludedDestinations :: !(Maybe [Text])
    , _dtCountry :: !(Maybe Text)
    , _dtLanguage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtIncludedDestinations'
--
-- * 'dtExcludedDestinations'
--
-- * 'dtCountry'
--
-- * 'dtLanguage'
datafeedTarget
    :: DatafeedTarget
datafeedTarget = 
    DatafeedTarget'
    { _dtIncludedDestinations = Nothing
    , _dtExcludedDestinations = Nothing
    , _dtCountry = Nothing
    , _dtLanguage = Nothing
    }

-- | The list of destinations to include for this target (corresponds to
-- checked check boxes in Merchant Center). Default destinations are always
-- included unless provided in the excluded_destination field.
dtIncludedDestinations :: Lens' DatafeedTarget [Text]
dtIncludedDestinations
  = lens _dtIncludedDestinations
      (\ s a -> s{_dtIncludedDestinations = a})
      . _Default
      . _Coerce

-- | The list of destinations to exclude for this target (corresponds to
-- unchecked check boxes in Merchant Center).
dtExcludedDestinations :: Lens' DatafeedTarget [Text]
dtExcludedDestinations
  = lens _dtExcludedDestinations
      (\ s a -> s{_dtExcludedDestinations = a})
      . _Default
      . _Coerce

-- | The country where the items in the feed will be included in the search
-- index, represented as a CLDR territory code.
dtCountry :: Lens' DatafeedTarget (Maybe Text)
dtCountry
  = lens _dtCountry (\ s a -> s{_dtCountry = a})

-- | The two-letter ISO 639-1 language of the items in the feed. Must be a
-- valid language for targets[].country.
dtLanguage :: Lens' DatafeedTarget (Maybe Text)
dtLanguage
  = lens _dtLanguage (\ s a -> s{_dtLanguage = a})

instance FromJSON DatafeedTarget where
        parseJSON
          = withObject "DatafeedTarget"
              (\ o ->
                 DatafeedTarget' <$>
                   (o .:? "includedDestinations" .!= mempty) <*>
                     (o .:? "excludedDestinations" .!= mempty)
                     <*> (o .:? "country")
                     <*> (o .:? "language"))

instance ToJSON DatafeedTarget where
        toJSON DatafeedTarget'{..}
          = object
              (catMaybes
                 [("includedDestinations" .=) <$>
                    _dtIncludedDestinations,
                  ("excludedDestinations" .=) <$>
                    _dtExcludedDestinations,
                  ("country" .=) <$> _dtCountry,
                  ("language" .=) <$> _dtLanguage])

--
-- /See:/ 'ordersUpdateMerchantOrderIdResponse' smart constructor.
data OrdersUpdateMerchantOrderIdResponse = OrdersUpdateMerchantOrderIdResponse'
    { _oumoirKind :: !Text
    , _oumoirExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersUpdateMerchantOrderIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oumoirKind'
--
-- * 'oumoirExecutionStatus'
ordersUpdateMerchantOrderIdResponse
    :: OrdersUpdateMerchantOrderIdResponse
ordersUpdateMerchantOrderIdResponse = 
    OrdersUpdateMerchantOrderIdResponse'
    { _oumoirKind = "content#ordersUpdateMerchantOrderIdResponse"
    , _oumoirExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersUpdateMerchantOrderIdResponse\".
oumoirKind :: Lens' OrdersUpdateMerchantOrderIdResponse Text
oumoirKind
  = lens _oumoirKind (\ s a -> s{_oumoirKind = a})

-- | The status of the execution.
oumoirExecutionStatus :: Lens' OrdersUpdateMerchantOrderIdResponse (Maybe Text)
oumoirExecutionStatus
  = lens _oumoirExecutionStatus
      (\ s a -> s{_oumoirExecutionStatus = a})

instance FromJSON OrdersUpdateMerchantOrderIdResponse
         where
        parseJSON
          = withObject "OrdersUpdateMerchantOrderIdResponse"
              (\ o ->
                 OrdersUpdateMerchantOrderIdResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersUpdateMerchantOrderIdResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersUpdateMerchantOrderIdResponse
         where
        toJSON OrdersUpdateMerchantOrderIdResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oumoirKind),
                  ("executionStatus" .=) <$> _oumoirExecutionStatus])

--
-- /See:/ 'inventoryPickup' smart constructor.
data InventoryPickup = InventoryPickup'
    { _ipPickupSla :: !(Maybe Text)
    , _ipPickupMethod :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryPickup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipPickupSla'
--
-- * 'ipPickupMethod'
inventoryPickup
    :: InventoryPickup
inventoryPickup = 
    InventoryPickup'
    { _ipPickupSla = Nothing
    , _ipPickupMethod = Nothing
    }

-- | The expected date that an order will be ready for pickup, relative to
-- when the order is placed. Only supported for local inventory. Must be
-- submitted together with pickupMethod.
ipPickupSla :: Lens' InventoryPickup (Maybe Text)
ipPickupSla
  = lens _ipPickupSla (\ s a -> s{_ipPickupSla = a})

-- | Whether store pickup is available for this offer and whether the pickup
-- option should be shown as buy, reserve, or not supported. Only supported
-- for local inventory. Unless the value is \"not supported\", must be
-- submitted together with pickupSla.
ipPickupMethod :: Lens' InventoryPickup (Maybe Text)
ipPickupMethod
  = lens _ipPickupMethod
      (\ s a -> s{_ipPickupMethod = a})

instance FromJSON InventoryPickup where
        parseJSON
          = withObject "InventoryPickup"
              (\ o ->
                 InventoryPickup' <$>
                   (o .:? "pickupSla") <*> (o .:? "pickupMethod"))

instance ToJSON InventoryPickup where
        toJSON InventoryPickup'{..}
          = object
              (catMaybes
                 [("pickupSla" .=) <$> _ipPickupSla,
                  ("pickupMethod" .=) <$> _ipPickupMethod])

-- | An example occurrence for a particular error.
--
-- /See:/ 'datafeedStatusExample' smart constructor.
data DatafeedStatusExample = DatafeedStatusExample'
    { _dseLineNumber :: !(Maybe (Textual Word64))
    , _dseItemId :: !(Maybe Text)
    , _dseValue :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedStatusExample' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseLineNumber'
--
-- * 'dseItemId'
--
-- * 'dseValue'
datafeedStatusExample
    :: DatafeedStatusExample
datafeedStatusExample = 
    DatafeedStatusExample'
    { _dseLineNumber = Nothing
    , _dseItemId = Nothing
    , _dseValue = Nothing
    }

-- | Line number in the data feed where the example is found.
dseLineNumber :: Lens' DatafeedStatusExample (Maybe Word64)
dseLineNumber
  = lens _dseLineNumber
      (\ s a -> s{_dseLineNumber = a})
      . mapping _Coerce

-- | The ID of the example item.
dseItemId :: Lens' DatafeedStatusExample (Maybe Text)
dseItemId
  = lens _dseItemId (\ s a -> s{_dseItemId = a})

-- | The problematic value.
dseValue :: Lens' DatafeedStatusExample (Maybe Text)
dseValue = lens _dseValue (\ s a -> s{_dseValue = a})

instance FromJSON DatafeedStatusExample where
        parseJSON
          = withObject "DatafeedStatusExample"
              (\ o ->
                 DatafeedStatusExample' <$>
                   (o .:? "lineNumber") <*> (o .:? "itemId") <*>
                     (o .:? "value"))

instance ToJSON DatafeedStatusExample where
        toJSON DatafeedStatusExample'{..}
          = object
              (catMaybes
                 [("lineNumber" .=) <$> _dseLineNumber,
                  ("itemId" .=) <$> _dseItemId,
                  ("value" .=) <$> _dseValue])

--
-- /See:/ 'ordersAcknowledgeResponse' smart constructor.
data OrdersAcknowledgeResponse = OrdersAcknowledgeResponse'
    { _oarKind :: !Text
    , _oarExecutionStatus :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersAcknowledgeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oarKind'
--
-- * 'oarExecutionStatus'
ordersAcknowledgeResponse
    :: OrdersAcknowledgeResponse
ordersAcknowledgeResponse = 
    OrdersAcknowledgeResponse'
    { _oarKind = "content#ordersAcknowledgeResponse"
    , _oarExecutionStatus = Nothing
    }

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersAcknowledgeResponse\".
oarKind :: Lens' OrdersAcknowledgeResponse Text
oarKind = lens _oarKind (\ s a -> s{_oarKind = a})

-- | The status of the execution.
oarExecutionStatus :: Lens' OrdersAcknowledgeResponse (Maybe Text)
oarExecutionStatus
  = lens _oarExecutionStatus
      (\ s a -> s{_oarExecutionStatus = a})

instance FromJSON OrdersAcknowledgeResponse where
        parseJSON
          = withObject "OrdersAcknowledgeResponse"
              (\ o ->
                 OrdersAcknowledgeResponse' <$>
                   (o .:? "kind" .!=
                      "content#ordersAcknowledgeResponse")
                     <*> (o .:? "executionStatus"))

instance ToJSON OrdersAcknowledgeResponse where
        toJSON OrdersAcknowledgeResponse'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _oarKind),
                  ("executionStatus" .=) <$> _oarExecutionStatus])

--
-- /See:/ 'posSaleRequest' smart constructor.
data PosSaleRequest = PosSaleRequest'
    { _psrsStoreCode :: !(Maybe Text)
    , _psrsItemId :: !(Maybe Text)
    , _psrsQuantity :: !(Maybe (Textual Int64))
    , _psrsTargetCountry :: !(Maybe Text)
    , _psrsGtin :: !(Maybe Text)
    , _psrsPrice :: !(Maybe Price)
    , _psrsContentLanguage :: !(Maybe Text)
    , _psrsTimestamp :: !(Maybe Text)
    , _psrsSaleId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosSaleRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psrsStoreCode'
--
-- * 'psrsItemId'
--
-- * 'psrsQuantity'
--
-- * 'psrsTargetCountry'
--
-- * 'psrsGtin'
--
-- * 'psrsPrice'
--
-- * 'psrsContentLanguage'
--
-- * 'psrsTimestamp'
--
-- * 'psrsSaleId'
posSaleRequest
    :: PosSaleRequest
posSaleRequest = 
    PosSaleRequest'
    { _psrsStoreCode = Nothing
    , _psrsItemId = Nothing
    , _psrsQuantity = Nothing
    , _psrsTargetCountry = Nothing
    , _psrsGtin = Nothing
    , _psrsPrice = Nothing
    , _psrsContentLanguage = Nothing
    , _psrsTimestamp = Nothing
    , _psrsSaleId = Nothing
    }

-- | The identifier of the merchant\'s store.
psrsStoreCode :: Lens' PosSaleRequest (Maybe Text)
psrsStoreCode
  = lens _psrsStoreCode
      (\ s a -> s{_psrsStoreCode = a})

-- | A unique identifier for the item.
psrsItemId :: Lens' PosSaleRequest (Maybe Text)
psrsItemId
  = lens _psrsItemId (\ s a -> s{_psrsItemId = a})

-- | The relative change of the available quantity. Negative for items sold.
psrsQuantity :: Lens' PosSaleRequest (Maybe Int64)
psrsQuantity
  = lens _psrsQuantity (\ s a -> s{_psrsQuantity = a})
      . mapping _Coerce

-- | The CLDR territory code for the item.
psrsTargetCountry :: Lens' PosSaleRequest (Maybe Text)
psrsTargetCountry
  = lens _psrsTargetCountry
      (\ s a -> s{_psrsTargetCountry = a})

-- | Global Trade Item Number.
psrsGtin :: Lens' PosSaleRequest (Maybe Text)
psrsGtin = lens _psrsGtin (\ s a -> s{_psrsGtin = a})

-- | The price of the item.
psrsPrice :: Lens' PosSaleRequest (Maybe Price)
psrsPrice
  = lens _psrsPrice (\ s a -> s{_psrsPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
psrsContentLanguage :: Lens' PosSaleRequest (Maybe Text)
psrsContentLanguage
  = lens _psrsContentLanguage
      (\ s a -> s{_psrsContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
psrsTimestamp :: Lens' PosSaleRequest (Maybe Text)
psrsTimestamp
  = lens _psrsTimestamp
      (\ s a -> s{_psrsTimestamp = a})

-- | A unique ID to group items from the same sale event.
psrsSaleId :: Lens' PosSaleRequest (Maybe Text)
psrsSaleId
  = lens _psrsSaleId (\ s a -> s{_psrsSaleId = a})

instance FromJSON PosSaleRequest where
        parseJSON
          = withObject "PosSaleRequest"
              (\ o ->
                 PosSaleRequest' <$>
                   (o .:? "storeCode") <*> (o .:? "itemId") <*>
                     (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp")
                     <*> (o .:? "saleId"))

instance ToJSON PosSaleRequest where
        toJSON PosSaleRequest'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _psrsStoreCode,
                  ("itemId" .=) <$> _psrsItemId,
                  ("quantity" .=) <$> _psrsQuantity,
                  ("targetCountry" .=) <$> _psrsTargetCountry,
                  ("gtin" .=) <$> _psrsGtin,
                  ("price" .=) <$> _psrsPrice,
                  ("contentLanguage" .=) <$> _psrsContentLanguage,
                  ("timestamp" .=) <$> _psrsTimestamp,
                  ("saleId" .=) <$> _psrsSaleId])

--
-- /See:/ 'table' smart constructor.
data Table = Table'
    { _tRows :: !(Maybe [Row])
    , _tName :: !(Maybe Text)
    , _tColumnHeaders :: !(Maybe Headers)
    , _tRowHeaders :: !(Maybe Headers)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Table' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRows'
--
-- * 'tName'
--
-- * 'tColumnHeaders'
--
-- * 'tRowHeaders'
table
    :: Table
table = 
    Table'
    { _tRows = Nothing
    , _tName = Nothing
    , _tColumnHeaders = Nothing
    , _tRowHeaders = Nothing
    }

-- | The list of rows that constitute the table. Must have the same length as
-- rowHeaders. Required.
tRows :: Lens' Table [Row]
tRows
  = lens _tRows (\ s a -> s{_tRows = a}) . _Default .
      _Coerce

-- | Name of the table. Required for subtables, ignored for the main table.
tName :: Lens' Table (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

-- | Headers of the table\'s columns. Optional: if not set then the table has
-- only one dimension.
tColumnHeaders :: Lens' Table (Maybe Headers)
tColumnHeaders
  = lens _tColumnHeaders
      (\ s a -> s{_tColumnHeaders = a})

-- | Headers of the table\'s rows. Required.
tRowHeaders :: Lens' Table (Maybe Headers)
tRowHeaders
  = lens _tRowHeaders (\ s a -> s{_tRowHeaders = a})

instance FromJSON Table where
        parseJSON
          = withObject "Table"
              (\ o ->
                 Table' <$>
                   (o .:? "rows" .!= mempty) <*> (o .:? "name") <*>
                     (o .:? "columnHeaders")
                     <*> (o .:? "rowHeaders"))

instance ToJSON Table where
        toJSON Table'{..}
          = object
              (catMaybes
                 [("rows" .=) <$> _tRows, ("name" .=) <$> _tName,
                  ("columnHeaders" .=) <$> _tColumnHeaders,
                  ("rowHeaders" .=) <$> _tRowHeaders])

--
-- /See:/ 'order' smart constructor.
data Order = Order'
    { _ordrStatus :: !(Maybe Text)
    , _ordrMerchantId :: !(Maybe (Textual Word64))
    , _ordrRefunds :: !(Maybe [OrderRefund])
    , _ordrKind :: !Text
    , _ordrLineItems :: !(Maybe [OrderLineItem])
    , _ordrShipments :: !(Maybe [OrderShipment])
    , _ordrNetAmount :: !(Maybe Price)
    , _ordrPlacedDate :: !(Maybe Text)
    , _ordrDeliveryDetails :: !(Maybe OrderDeliveryDetails)
    , _ordrShippingOption :: !(Maybe Text)
    , _ordrMerchantOrderId :: !(Maybe Text)
    , _ordrAcknowledged :: !(Maybe Bool)
    , _ordrShippingCostTax :: !(Maybe Price)
    , _ordrCustomer :: !(Maybe OrderCustomer)
    , _ordrId :: !(Maybe Text)
    , _ordrPaymentMethod :: !(Maybe OrderPaymentMethod)
    , _ordrPromotions :: !(Maybe [OrderPromotion])
    , _ordrChannelType :: !(Maybe Text)
    , _ordrPaymentStatus :: !(Maybe Text)
    , _ordrShippingCost :: !(Maybe Price)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Order' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ordrStatus'
--
-- * 'ordrMerchantId'
--
-- * 'ordrRefunds'
--
-- * 'ordrKind'
--
-- * 'ordrLineItems'
--
-- * 'ordrShipments'
--
-- * 'ordrNetAmount'
--
-- * 'ordrPlacedDate'
--
-- * 'ordrDeliveryDetails'
--
-- * 'ordrShippingOption'
--
-- * 'ordrMerchantOrderId'
--
-- * 'ordrAcknowledged'
--
-- * 'ordrShippingCostTax'
--
-- * 'ordrCustomer'
--
-- * 'ordrId'
--
-- * 'ordrPaymentMethod'
--
-- * 'ordrPromotions'
--
-- * 'ordrChannelType'
--
-- * 'ordrPaymentStatus'
--
-- * 'ordrShippingCost'
order
    :: Order
order = 
    Order'
    { _ordrStatus = Nothing
    , _ordrMerchantId = Nothing
    , _ordrRefunds = Nothing
    , _ordrKind = "content#order"
    , _ordrLineItems = Nothing
    , _ordrShipments = Nothing
    , _ordrNetAmount = Nothing
    , _ordrPlacedDate = Nothing
    , _ordrDeliveryDetails = Nothing
    , _ordrShippingOption = Nothing
    , _ordrMerchantOrderId = Nothing
    , _ordrAcknowledged = Nothing
    , _ordrShippingCostTax = Nothing
    , _ordrCustomer = Nothing
    , _ordrId = Nothing
    , _ordrPaymentMethod = Nothing
    , _ordrPromotions = Nothing
    , _ordrChannelType = Nothing
    , _ordrPaymentStatus = Nothing
    , _ordrShippingCost = Nothing
    }

-- | The status of the order.
ordrStatus :: Lens' Order (Maybe Text)
ordrStatus
  = lens _ordrStatus (\ s a -> s{_ordrStatus = a})

ordrMerchantId :: Lens' Order (Maybe Word64)
ordrMerchantId
  = lens _ordrMerchantId
      (\ s a -> s{_ordrMerchantId = a})
      . mapping _Coerce

-- | Refunds for the order.
ordrRefunds :: Lens' Order [OrderRefund]
ordrRefunds
  = lens _ordrRefunds (\ s a -> s{_ordrRefunds = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#order\".
ordrKind :: Lens' Order Text
ordrKind = lens _ordrKind (\ s a -> s{_ordrKind = a})

-- | Line items that are ordered.
ordrLineItems :: Lens' Order [OrderLineItem]
ordrLineItems
  = lens _ordrLineItems
      (\ s a -> s{_ordrLineItems = a})
      . _Default
      . _Coerce

-- | Shipments of the order.
ordrShipments :: Lens' Order [OrderShipment]
ordrShipments
  = lens _ordrShipments
      (\ s a -> s{_ordrShipments = a})
      . _Default
      . _Coerce

-- | The net amount for the order. For example, if an order was originally
-- for a grand total of $100 and a refund was issued for $20, the net
-- amount will be $80.
ordrNetAmount :: Lens' Order (Maybe Price)
ordrNetAmount
  = lens _ordrNetAmount
      (\ s a -> s{_ordrNetAmount = a})

-- | The date when the order was placed, in ISO 8601 format.
ordrPlacedDate :: Lens' Order (Maybe Text)
ordrPlacedDate
  = lens _ordrPlacedDate
      (\ s a -> s{_ordrPlacedDate = a})

-- | The details for the delivery.
ordrDeliveryDetails :: Lens' Order (Maybe OrderDeliveryDetails)
ordrDeliveryDetails
  = lens _ordrDeliveryDetails
      (\ s a -> s{_ordrDeliveryDetails = a})

-- | The requested shipping option.
ordrShippingOption :: Lens' Order (Maybe Text)
ordrShippingOption
  = lens _ordrShippingOption
      (\ s a -> s{_ordrShippingOption = a})

-- | Merchant-provided id of the order.
ordrMerchantOrderId :: Lens' Order (Maybe Text)
ordrMerchantOrderId
  = lens _ordrMerchantOrderId
      (\ s a -> s{_ordrMerchantOrderId = a})

-- | Whether the order was acknowledged.
ordrAcknowledged :: Lens' Order (Maybe Bool)
ordrAcknowledged
  = lens _ordrAcknowledged
      (\ s a -> s{_ordrAcknowledged = a})

-- | The tax for the total shipping cost.
ordrShippingCostTax :: Lens' Order (Maybe Price)
ordrShippingCostTax
  = lens _ordrShippingCostTax
      (\ s a -> s{_ordrShippingCostTax = a})

-- | The details of the customer who placed the order.
ordrCustomer :: Lens' Order (Maybe OrderCustomer)
ordrCustomer
  = lens _ordrCustomer (\ s a -> s{_ordrCustomer = a})

-- | The REST id of the order. Globally unique.
ordrId :: Lens' Order (Maybe Text)
ordrId = lens _ordrId (\ s a -> s{_ordrId = a})

-- | The details of the payment method.
ordrPaymentMethod :: Lens' Order (Maybe OrderPaymentMethod)
ordrPaymentMethod
  = lens _ordrPaymentMethod
      (\ s a -> s{_ordrPaymentMethod = a})

-- | The details of the merchant provided promotions applied to the order.
-- More details about the program are here.
ordrPromotions :: Lens' Order [OrderPromotion]
ordrPromotions
  = lens _ordrPromotions
      (\ s a -> s{_ordrPromotions = a})
      . _Default
      . _Coerce

-- | The channel type of the order: \"purchaseOnGoogle\" or
-- \"googleExpress\".
ordrChannelType :: Lens' Order (Maybe Text)
ordrChannelType
  = lens _ordrChannelType
      (\ s a -> s{_ordrChannelType = a})

-- | The status of the payment.
ordrPaymentStatus :: Lens' Order (Maybe Text)
ordrPaymentStatus
  = lens _ordrPaymentStatus
      (\ s a -> s{_ordrPaymentStatus = a})

-- | The total cost of shipping for all items.
ordrShippingCost :: Lens' Order (Maybe Price)
ordrShippingCost
  = lens _ordrShippingCost
      (\ s a -> s{_ordrShippingCost = a})

instance FromJSON Order where
        parseJSON
          = withObject "Order"
              (\ o ->
                 Order' <$>
                   (o .:? "status") <*> (o .:? "merchantId") <*>
                     (o .:? "refunds" .!= mempty)
                     <*> (o .:? "kind" .!= "content#order")
                     <*> (o .:? "lineItems" .!= mempty)
                     <*> (o .:? "shipments" .!= mempty)
                     <*> (o .:? "netAmount")
                     <*> (o .:? "placedDate")
                     <*> (o .:? "deliveryDetails")
                     <*> (o .:? "shippingOption")
                     <*> (o .:? "merchantOrderId")
                     <*> (o .:? "acknowledged")
                     <*> (o .:? "shippingCostTax")
                     <*> (o .:? "customer")
                     <*> (o .:? "id")
                     <*> (o .:? "paymentMethod")
                     <*> (o .:? "promotions" .!= mempty)
                     <*> (o .:? "channelType")
                     <*> (o .:? "paymentStatus")
                     <*> (o .:? "shippingCost"))

instance ToJSON Order where
        toJSON Order'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _ordrStatus,
                  ("merchantId" .=) <$> _ordrMerchantId,
                  ("refunds" .=) <$> _ordrRefunds,
                  Just ("kind" .= _ordrKind),
                  ("lineItems" .=) <$> _ordrLineItems,
                  ("shipments" .=) <$> _ordrShipments,
                  ("netAmount" .=) <$> _ordrNetAmount,
                  ("placedDate" .=) <$> _ordrPlacedDate,
                  ("deliveryDetails" .=) <$> _ordrDeliveryDetails,
                  ("shippingOption" .=) <$> _ordrShippingOption,
                  ("merchantOrderId" .=) <$> _ordrMerchantOrderId,
                  ("acknowledged" .=) <$> _ordrAcknowledged,
                  ("shippingCostTax" .=) <$> _ordrShippingCostTax,
                  ("customer" .=) <$> _ordrCustomer,
                  ("id" .=) <$> _ordrId,
                  ("paymentMethod" .=) <$> _ordrPaymentMethod,
                  ("promotions" .=) <$> _ordrPromotions,
                  ("channelType" .=) <$> _ordrChannelType,
                  ("paymentStatus" .=) <$> _ordrPaymentStatus,
                  ("shippingCost" .=) <$> _ordrShippingCost])

--
-- /See:/ 'inventoryCustomBatchResponse' smart constructor.
data InventoryCustomBatchResponse = InventoryCustomBatchResponse'
    { _invEntries :: !(Maybe [InventoryCustomBatchResponseEntry])
    , _invKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InventoryCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'invEntries'
--
-- * 'invKind'
inventoryCustomBatchResponse
    :: InventoryCustomBatchResponse
inventoryCustomBatchResponse = 
    InventoryCustomBatchResponse'
    { _invEntries = Nothing
    , _invKind = "content#inventoryCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
invEntries :: Lens' InventoryCustomBatchResponse [InventoryCustomBatchResponseEntry]
invEntries
  = lens _invEntries (\ s a -> s{_invEntries = a}) .
      _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#inventoryCustomBatchResponse\".
invKind :: Lens' InventoryCustomBatchResponse Text
invKind = lens _invKind (\ s a -> s{_invKind = a})

instance FromJSON InventoryCustomBatchResponse where
        parseJSON
          = withObject "InventoryCustomBatchResponse"
              (\ o ->
                 InventoryCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#inventoryCustomBatchResponse"))

instance ToJSON InventoryCustomBatchResponse where
        toJSON InventoryCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _invEntries,
                  Just ("kind" .= _invKind)])

--
-- /See:/ 'orderLineItemProduct' smart constructor.
data OrderLineItemProduct = OrderLineItemProduct'
    { _olipImageLink :: !(Maybe Text)
    , _olipShownImage :: !(Maybe Text)
    , _olipChannel :: !(Maybe Text)
    , _olipBrand :: !(Maybe Text)
    , _olipTargetCountry :: !(Maybe Text)
    , _olipGtin :: !(Maybe Text)
    , _olipItemGroupId :: !(Maybe Text)
    , _olipOfferId :: !(Maybe Text)
    , _olipId :: !(Maybe Text)
    , _olipPrice :: !(Maybe Price)
    , _olipVariantAttributes :: !(Maybe [OrderLineItemProductVariantAttribute])
    , _olipTitle :: !(Maybe Text)
    , _olipContentLanguage :: !(Maybe Text)
    , _olipMpn :: !(Maybe Text)
    , _olipCondition :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderLineItemProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olipImageLink'
--
-- * 'olipShownImage'
--
-- * 'olipChannel'
--
-- * 'olipBrand'
--
-- * 'olipTargetCountry'
--
-- * 'olipGtin'
--
-- * 'olipItemGroupId'
--
-- * 'olipOfferId'
--
-- * 'olipId'
--
-- * 'olipPrice'
--
-- * 'olipVariantAttributes'
--
-- * 'olipTitle'
--
-- * 'olipContentLanguage'
--
-- * 'olipMpn'
--
-- * 'olipCondition'
orderLineItemProduct
    :: OrderLineItemProduct
orderLineItemProduct = 
    OrderLineItemProduct'
    { _olipImageLink = Nothing
    , _olipShownImage = Nothing
    , _olipChannel = Nothing
    , _olipBrand = Nothing
    , _olipTargetCountry = Nothing
    , _olipGtin = Nothing
    , _olipItemGroupId = Nothing
    , _olipOfferId = Nothing
    , _olipId = Nothing
    , _olipPrice = Nothing
    , _olipVariantAttributes = Nothing
    , _olipTitle = Nothing
    , _olipContentLanguage = Nothing
    , _olipMpn = Nothing
    , _olipCondition = Nothing
    }

-- | URL of an image of the item.
olipImageLink :: Lens' OrderLineItemProduct (Maybe Text)
olipImageLink
  = lens _olipImageLink
      (\ s a -> s{_olipImageLink = a})

-- | URL to the cached image shown to the user when order was placed.
olipShownImage :: Lens' OrderLineItemProduct (Maybe Text)
olipShownImage
  = lens _olipShownImage
      (\ s a -> s{_olipShownImage = a})

-- | The item\'s channel (online or local).
olipChannel :: Lens' OrderLineItemProduct (Maybe Text)
olipChannel
  = lens _olipChannel (\ s a -> s{_olipChannel = a})

-- | Brand of the item.
olipBrand :: Lens' OrderLineItemProduct (Maybe Text)
olipBrand
  = lens _olipBrand (\ s a -> s{_olipBrand = a})

-- | The CLDR territory code of the target country of the product.
olipTargetCountry :: Lens' OrderLineItemProduct (Maybe Text)
olipTargetCountry
  = lens _olipTargetCountry
      (\ s a -> s{_olipTargetCountry = a})

-- | Global Trade Item Number (GTIN) of the item.
olipGtin :: Lens' OrderLineItemProduct (Maybe Text)
olipGtin = lens _olipGtin (\ s a -> s{_olipGtin = a})

-- | Shared identifier for all variants of the same product.
olipItemGroupId :: Lens' OrderLineItemProduct (Maybe Text)
olipItemGroupId
  = lens _olipItemGroupId
      (\ s a -> s{_olipItemGroupId = a})

-- | An identifier of the item.
olipOfferId :: Lens' OrderLineItemProduct (Maybe Text)
olipOfferId
  = lens _olipOfferId (\ s a -> s{_olipOfferId = a})

-- | The REST id of the product.
olipId :: Lens' OrderLineItemProduct (Maybe Text)
olipId = lens _olipId (\ s a -> s{_olipId = a})

-- | Price of the item.
olipPrice :: Lens' OrderLineItemProduct (Maybe Price)
olipPrice
  = lens _olipPrice (\ s a -> s{_olipPrice = a})

-- | Variant attributes for the item. These are dimensions of the product,
-- such as color, gender, material, pattern, and size. You can find a
-- comprehensive list of variant attributes here.
olipVariantAttributes :: Lens' OrderLineItemProduct [OrderLineItemProductVariantAttribute]
olipVariantAttributes
  = lens _olipVariantAttributes
      (\ s a -> s{_olipVariantAttributes = a})
      . _Default
      . _Coerce

-- | The title of the product.
olipTitle :: Lens' OrderLineItemProduct (Maybe Text)
olipTitle
  = lens _olipTitle (\ s a -> s{_olipTitle = a})

-- | The two-letter ISO 639-1 language code for the item.
olipContentLanguage :: Lens' OrderLineItemProduct (Maybe Text)
olipContentLanguage
  = lens _olipContentLanguage
      (\ s a -> s{_olipContentLanguage = a})

-- | Manufacturer Part Number (MPN) of the item.
olipMpn :: Lens' OrderLineItemProduct (Maybe Text)
olipMpn = lens _olipMpn (\ s a -> s{_olipMpn = a})

-- | Condition or state of the item.
olipCondition :: Lens' OrderLineItemProduct (Maybe Text)
olipCondition
  = lens _olipCondition
      (\ s a -> s{_olipCondition = a})

instance FromJSON OrderLineItemProduct where
        parseJSON
          = withObject "OrderLineItemProduct"
              (\ o ->
                 OrderLineItemProduct' <$>
                   (o .:? "imageLink") <*> (o .:? "shownImage") <*>
                     (o .:? "channel")
                     <*> (o .:? "brand")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "itemGroupId")
                     <*> (o .:? "offerId")
                     <*> (o .:? "id")
                     <*> (o .:? "price")
                     <*> (o .:? "variantAttributes" .!= mempty)
                     <*> (o .:? "title")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "mpn")
                     <*> (o .:? "condition"))

instance ToJSON OrderLineItemProduct where
        toJSON OrderLineItemProduct'{..}
          = object
              (catMaybes
                 [("imageLink" .=) <$> _olipImageLink,
                  ("shownImage" .=) <$> _olipShownImage,
                  ("channel" .=) <$> _olipChannel,
                  ("brand" .=) <$> _olipBrand,
                  ("targetCountry" .=) <$> _olipTargetCountry,
                  ("gtin" .=) <$> _olipGtin,
                  ("itemGroupId" .=) <$> _olipItemGroupId,
                  ("offerId" .=) <$> _olipOfferId,
                  ("id" .=) <$> _olipId, ("price" .=) <$> _olipPrice,
                  ("variantAttributes" .=) <$> _olipVariantAttributes,
                  ("title" .=) <$> _olipTitle,
                  ("contentLanguage" .=) <$> _olipContentLanguage,
                  ("mpn" .=) <$> _olipMpn,
                  ("condition" .=) <$> _olipCondition])

-- | A batch entry encoding a single non-batch accounttax request.
--
-- /See:/ 'accounttaxCustomBatchRequestEntry' smart constructor.
data AccounttaxCustomBatchRequestEntry = AccounttaxCustomBatchRequestEntry'
    { _acccAccountTax :: !(Maybe AccountTax)
    , _acccMerchantId :: !(Maybe (Textual Word64))
    , _acccAccountId :: !(Maybe (Textual Word64))
    , _acccMethod :: !(Maybe Text)
    , _acccBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxCustomBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acccAccountTax'
--
-- * 'acccMerchantId'
--
-- * 'acccAccountId'
--
-- * 'acccMethod'
--
-- * 'acccBatchId'
accounttaxCustomBatchRequestEntry
    :: AccounttaxCustomBatchRequestEntry
accounttaxCustomBatchRequestEntry = 
    AccounttaxCustomBatchRequestEntry'
    { _acccAccountTax = Nothing
    , _acccMerchantId = Nothing
    , _acccAccountId = Nothing
    , _acccMethod = Nothing
    , _acccBatchId = Nothing
    }

-- | The account tax settings to update. Only defined if the method is
-- update.
acccAccountTax :: Lens' AccounttaxCustomBatchRequestEntry (Maybe AccountTax)
acccAccountTax
  = lens _acccAccountTax
      (\ s a -> s{_acccAccountTax = a})

-- | The ID of the managing account.
acccMerchantId :: Lens' AccounttaxCustomBatchRequestEntry (Maybe Word64)
acccMerchantId
  = lens _acccMerchantId
      (\ s a -> s{_acccMerchantId = a})
      . mapping _Coerce

-- | The ID of the account for which to get\/update account tax settings.
acccAccountId :: Lens' AccounttaxCustomBatchRequestEntry (Maybe Word64)
acccAccountId
  = lens _acccAccountId
      (\ s a -> s{_acccAccountId = a})
      . mapping _Coerce

acccMethod :: Lens' AccounttaxCustomBatchRequestEntry (Maybe Text)
acccMethod
  = lens _acccMethod (\ s a -> s{_acccMethod = a})

-- | An entry ID, unique within the batch request.
acccBatchId :: Lens' AccounttaxCustomBatchRequestEntry (Maybe Word32)
acccBatchId
  = lens _acccBatchId (\ s a -> s{_acccBatchId = a}) .
      mapping _Coerce

instance FromJSON AccounttaxCustomBatchRequestEntry
         where
        parseJSON
          = withObject "AccounttaxCustomBatchRequestEntry"
              (\ o ->
                 AccounttaxCustomBatchRequestEntry' <$>
                   (o .:? "accountTax") <*> (o .:? "merchantId") <*>
                     (o .:? "accountId")
                     <*> (o .:? "method")
                     <*> (o .:? "batchId"))

instance ToJSON AccounttaxCustomBatchRequestEntry
         where
        toJSON AccounttaxCustomBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("accountTax" .=) <$> _acccAccountTax,
                  ("merchantId" .=) <$> _acccMerchantId,
                  ("accountId" .=) <$> _acccAccountId,
                  ("method" .=) <$> _acccMethod,
                  ("batchId" .=) <$> _acccBatchId])

-- | An error occurring in the feed, like \"invalid price\".
--
-- /See:/ 'datafeedStatusError' smart constructor.
data DatafeedStatusError = DatafeedStatusError'
    { _dseCount :: !(Maybe (Textual Word64))
    , _dseCode :: !(Maybe Text)
    , _dseMessage :: !(Maybe Text)
    , _dseExamples :: !(Maybe [DatafeedStatusExample])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedStatusError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseCount'
--
-- * 'dseCode'
--
-- * 'dseMessage'
--
-- * 'dseExamples'
datafeedStatusError
    :: DatafeedStatusError
datafeedStatusError = 
    DatafeedStatusError'
    { _dseCount = Nothing
    , _dseCode = Nothing
    , _dseMessage = Nothing
    , _dseExamples = Nothing
    }

-- | The number of occurrences of the error in the feed.
dseCount :: Lens' DatafeedStatusError (Maybe Word64)
dseCount
  = lens _dseCount (\ s a -> s{_dseCount = a}) .
      mapping _Coerce

-- | The code of the error, e.g., \"validation\/invalid_value\".
dseCode :: Lens' DatafeedStatusError (Maybe Text)
dseCode = lens _dseCode (\ s a -> s{_dseCode = a})

-- | The error message, e.g., \"Invalid price\".
dseMessage :: Lens' DatafeedStatusError (Maybe Text)
dseMessage
  = lens _dseMessage (\ s a -> s{_dseMessage = a})

-- | A list of example occurrences of the error, grouped by product.
dseExamples :: Lens' DatafeedStatusError [DatafeedStatusExample]
dseExamples
  = lens _dseExamples (\ s a -> s{_dseExamples = a}) .
      _Default
      . _Coerce

instance FromJSON DatafeedStatusError where
        parseJSON
          = withObject "DatafeedStatusError"
              (\ o ->
                 DatafeedStatusError' <$>
                   (o .:? "count") <*> (o .:? "code") <*>
                     (o .:? "message")
                     <*> (o .:? "examples" .!= mempty))

instance ToJSON DatafeedStatusError where
        toJSON DatafeedStatusError'{..}
          = object
              (catMaybes
                 [("count" .=) <$> _dseCount,
                  ("code" .=) <$> _dseCode,
                  ("message" .=) <$> _dseMessage,
                  ("examples" .=) <$> _dseExamples])

--
-- /See:/ 'productsCustomBatchRequest' smart constructor.
newtype ProductsCustomBatchRequest = ProductsCustomBatchRequest'
    { _ppEntries :: Maybe [ProductsCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppEntries'
productsCustomBatchRequest
    :: ProductsCustomBatchRequest
productsCustomBatchRequest = 
    ProductsCustomBatchRequest'
    { _ppEntries = Nothing
    }

-- | The request entries to be processed in the batch.
ppEntries :: Lens' ProductsCustomBatchRequest [ProductsCustomBatchRequestEntry]
ppEntries
  = lens _ppEntries (\ s a -> s{_ppEntries = a}) .
      _Default
      . _Coerce

instance FromJSON ProductsCustomBatchRequest where
        parseJSON
          = withObject "ProductsCustomBatchRequest"
              (\ o ->
                 ProductsCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON ProductsCustomBatchRequest where
        toJSON ProductsCustomBatchRequest'{..}
          = object (catMaybes [("entries" .=) <$> _ppEntries])

--
-- /See:/ 'ordersCustomBatchRequestEntryReturnLineItem' smart constructor.
data OrdersCustomBatchRequestEntryReturnLineItem = OrdersCustomBatchRequestEntryReturnLineItem'
    { _ocbrerliQuantity :: !(Maybe (Textual Word32))
    , _ocbrerliLineItemId :: !(Maybe Text)
    , _ocbrerliReason :: !(Maybe Text)
    , _ocbrerliProductId :: !(Maybe Text)
    , _ocbrerliReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryReturnLineItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrerliQuantity'
--
-- * 'ocbrerliLineItemId'
--
-- * 'ocbrerliReason'
--
-- * 'ocbrerliProductId'
--
-- * 'ocbrerliReasonText'
ordersCustomBatchRequestEntryReturnLineItem
    :: OrdersCustomBatchRequestEntryReturnLineItem
ordersCustomBatchRequestEntryReturnLineItem = 
    OrdersCustomBatchRequestEntryReturnLineItem'
    { _ocbrerliQuantity = Nothing
    , _ocbrerliLineItemId = Nothing
    , _ocbrerliReason = Nothing
    , _ocbrerliProductId = Nothing
    , _ocbrerliReasonText = Nothing
    }

-- | The quantity to return.
ocbrerliQuantity :: Lens' OrdersCustomBatchRequestEntryReturnLineItem (Maybe Word32)
ocbrerliQuantity
  = lens _ocbrerliQuantity
      (\ s a -> s{_ocbrerliQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
ocbrerliLineItemId :: Lens' OrdersCustomBatchRequestEntryReturnLineItem (Maybe Text)
ocbrerliLineItemId
  = lens _ocbrerliLineItemId
      (\ s a -> s{_ocbrerliLineItemId = a})

-- | The reason for the return.
ocbrerliReason :: Lens' OrdersCustomBatchRequestEntryReturnLineItem (Maybe Text)
ocbrerliReason
  = lens _ocbrerliReason
      (\ s a -> s{_ocbrerliReason = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
ocbrerliProductId :: Lens' OrdersCustomBatchRequestEntryReturnLineItem (Maybe Text)
ocbrerliProductId
  = lens _ocbrerliProductId
      (\ s a -> s{_ocbrerliProductId = a})

-- | The explanation of the reason.
ocbrerliReasonText :: Lens' OrdersCustomBatchRequestEntryReturnLineItem (Maybe Text)
ocbrerliReasonText
  = lens _ocbrerliReasonText
      (\ s a -> s{_ocbrerliReasonText = a})

instance FromJSON
         OrdersCustomBatchRequestEntryReturnLineItem where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryReturnLineItem"
              (\ o ->
                 OrdersCustomBatchRequestEntryReturnLineItem' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "productId")
                     <*> (o .:? "reasonText"))

instance ToJSON
         OrdersCustomBatchRequestEntryReturnLineItem where
        toJSON
          OrdersCustomBatchRequestEntryReturnLineItem'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _ocbrerliQuantity,
                  ("lineItemId" .=) <$> _ocbrerliLineItemId,
                  ("reason" .=) <$> _ocbrerliReason,
                  ("productId" .=) <$> _ocbrerliProductId,
                  ("reasonText" .=) <$> _ocbrerliReasonText])

-- | The absolute quantity of an item available at the given store.
--
-- /See:/ 'posInventory' smart constructor.
data PosInventory = PosInventory'
    { _piStoreCode :: !(Maybe Text)
    , _piKind :: !Text
    , _piItemId :: !(Maybe Text)
    , _piQuantity :: !(Maybe (Textual Int64))
    , _piTargetCountry :: !(Maybe Text)
    , _piGtin :: !(Maybe Text)
    , _piPrice :: !(Maybe Price)
    , _piContentLanguage :: !(Maybe Text)
    , _piTimestamp :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosInventory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piStoreCode'
--
-- * 'piKind'
--
-- * 'piItemId'
--
-- * 'piQuantity'
--
-- * 'piTargetCountry'
--
-- * 'piGtin'
--
-- * 'piPrice'
--
-- * 'piContentLanguage'
--
-- * 'piTimestamp'
posInventory
    :: PosInventory
posInventory = 
    PosInventory'
    { _piStoreCode = Nothing
    , _piKind = "content#posInventory"
    , _piItemId = Nothing
    , _piQuantity = Nothing
    , _piTargetCountry = Nothing
    , _piGtin = Nothing
    , _piPrice = Nothing
    , _piContentLanguage = Nothing
    , _piTimestamp = Nothing
    }

-- | The identifier of the merchant\'s store.
piStoreCode :: Lens' PosInventory (Maybe Text)
piStoreCode
  = lens _piStoreCode (\ s a -> s{_piStoreCode = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#posInventory\".
piKind :: Lens' PosInventory Text
piKind = lens _piKind (\ s a -> s{_piKind = a})

-- | A unique identifier for the item.
piItemId :: Lens' PosInventory (Maybe Text)
piItemId = lens _piItemId (\ s a -> s{_piItemId = a})

-- | The available quantity of the item.
piQuantity :: Lens' PosInventory (Maybe Int64)
piQuantity
  = lens _piQuantity (\ s a -> s{_piQuantity = a}) .
      mapping _Coerce

-- | The CLDR territory code for the item.
piTargetCountry :: Lens' PosInventory (Maybe Text)
piTargetCountry
  = lens _piTargetCountry
      (\ s a -> s{_piTargetCountry = a})

-- | Global Trade Item Number.
piGtin :: Lens' PosInventory (Maybe Text)
piGtin = lens _piGtin (\ s a -> s{_piGtin = a})

-- | The current price of the item.
piPrice :: Lens' PosInventory (Maybe Price)
piPrice = lens _piPrice (\ s a -> s{_piPrice = a})

-- | The two-letter ISO 639-1 language code for the item.
piContentLanguage :: Lens' PosInventory (Maybe Text)
piContentLanguage
  = lens _piContentLanguage
      (\ s a -> s{_piContentLanguage = a})

-- | The inventory timestamp, in ISO 8601 format.
piTimestamp :: Lens' PosInventory (Maybe Text)
piTimestamp
  = lens _piTimestamp (\ s a -> s{_piTimestamp = a})

instance FromJSON PosInventory where
        parseJSON
          = withObject "PosInventory"
              (\ o ->
                 PosInventory' <$>
                   (o .:? "storeCode") <*>
                     (o .:? "kind" .!= "content#posInventory")
                     <*> (o .:? "itemId")
                     <*> (o .:? "quantity")
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "gtin")
                     <*> (o .:? "price")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "timestamp"))

instance ToJSON PosInventory where
        toJSON PosInventory'{..}
          = object
              (catMaybes
                 [("storeCode" .=) <$> _piStoreCode,
                  Just ("kind" .= _piKind),
                  ("itemId" .=) <$> _piItemId,
                  ("quantity" .=) <$> _piQuantity,
                  ("targetCountry" .=) <$> _piTargetCountry,
                  ("gtin" .=) <$> _piGtin, ("price" .=) <$> _piPrice,
                  ("contentLanguage" .=) <$> _piContentLanguage,
                  ("timestamp" .=) <$> _piTimestamp])

--
-- /See:/ 'ordersCustomBatchRequestEntryUpdateShipment' smart constructor.
data OrdersCustomBatchRequestEntryUpdateShipment = OrdersCustomBatchRequestEntryUpdateShipment'
    { _ocbreusCarrier :: !(Maybe Text)
    , _ocbreusStatus :: !(Maybe Text)
    , _ocbreusTrackingId :: !(Maybe Text)
    , _ocbreusShipmentId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryUpdateShipment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbreusCarrier'
--
-- * 'ocbreusStatus'
--
-- * 'ocbreusTrackingId'
--
-- * 'ocbreusShipmentId'
ordersCustomBatchRequestEntryUpdateShipment
    :: OrdersCustomBatchRequestEntryUpdateShipment
ordersCustomBatchRequestEntryUpdateShipment = 
    OrdersCustomBatchRequestEntryUpdateShipment'
    { _ocbreusCarrier = Nothing
    , _ocbreusStatus = Nothing
    , _ocbreusTrackingId = Nothing
    , _ocbreusShipmentId = Nothing
    }

-- | The carrier handling the shipment. Not updated if missing. See
-- shipments[].carrier in the Orders resource representation for a list of
-- acceptable values.
ocbreusCarrier :: Lens' OrdersCustomBatchRequestEntryUpdateShipment (Maybe Text)
ocbreusCarrier
  = lens _ocbreusCarrier
      (\ s a -> s{_ocbreusCarrier = a})

-- | New status for the shipment. Not updated if missing.
ocbreusStatus :: Lens' OrdersCustomBatchRequestEntryUpdateShipment (Maybe Text)
ocbreusStatus
  = lens _ocbreusStatus
      (\ s a -> s{_ocbreusStatus = a})

-- | The tracking id for the shipment. Not updated if missing.
ocbreusTrackingId :: Lens' OrdersCustomBatchRequestEntryUpdateShipment (Maybe Text)
ocbreusTrackingId
  = lens _ocbreusTrackingId
      (\ s a -> s{_ocbreusTrackingId = a})

-- | The ID of the shipment.
ocbreusShipmentId :: Lens' OrdersCustomBatchRequestEntryUpdateShipment (Maybe Text)
ocbreusShipmentId
  = lens _ocbreusShipmentId
      (\ s a -> s{_ocbreusShipmentId = a})

instance FromJSON
         OrdersCustomBatchRequestEntryUpdateShipment where
        parseJSON
          = withObject
              "OrdersCustomBatchRequestEntryUpdateShipment"
              (\ o ->
                 OrdersCustomBatchRequestEntryUpdateShipment' <$>
                   (o .:? "carrier") <*> (o .:? "status") <*>
                     (o .:? "trackingId")
                     <*> (o .:? "shipmentId"))

instance ToJSON
         OrdersCustomBatchRequestEntryUpdateShipment where
        toJSON
          OrdersCustomBatchRequestEntryUpdateShipment'{..}
          = object
              (catMaybes
                 [("carrier" .=) <$> _ocbreusCarrier,
                  ("status" .=) <$> _ocbreusStatus,
                  ("trackingId" .=) <$> _ocbreusTrackingId,
                  ("shipmentId" .=) <$> _ocbreusShipmentId])

-- | The status of a datafeed, i.e., the result of the last retrieval of the
-- datafeed computed asynchronously when the feed processing is finished.
--
-- /See:/ 'datafeedStatus' smart constructor.
data DatafeedStatus = DatafeedStatus'
    { _dsItemsTotal :: !(Maybe (Textual Word64))
    , _dsCountry :: !(Maybe Text)
    , _dsKind :: !Text
    , _dsWarnings :: !(Maybe [DatafeedStatusError])
    , _dsDatafeedId :: !(Maybe (Textual Word64))
    , _dsProcessingStatus :: !(Maybe Text)
    , _dsLanguage :: !(Maybe Text)
    , _dsLastUploadDate :: !(Maybe Text)
    , _dsItemsValid :: !(Maybe (Textual Word64))
    , _dsErrors :: !(Maybe [DatafeedStatusError])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsItemsTotal'
--
-- * 'dsCountry'
--
-- * 'dsKind'
--
-- * 'dsWarnings'
--
-- * 'dsDatafeedId'
--
-- * 'dsProcessingStatus'
--
-- * 'dsLanguage'
--
-- * 'dsLastUploadDate'
--
-- * 'dsItemsValid'
--
-- * 'dsErrors'
datafeedStatus
    :: DatafeedStatus
datafeedStatus = 
    DatafeedStatus'
    { _dsItemsTotal = Nothing
    , _dsCountry = Nothing
    , _dsKind = "content#datafeedStatus"
    , _dsWarnings = Nothing
    , _dsDatafeedId = Nothing
    , _dsProcessingStatus = Nothing
    , _dsLanguage = Nothing
    , _dsLastUploadDate = Nothing
    , _dsItemsValid = Nothing
    , _dsErrors = Nothing
    }

-- | The number of items in the feed that were processed.
dsItemsTotal :: Lens' DatafeedStatus (Maybe Word64)
dsItemsTotal
  = lens _dsItemsTotal (\ s a -> s{_dsItemsTotal = a})
      . mapping _Coerce

-- | The country for which the status is reported, represented as a CLDR
-- territory code.
dsCountry :: Lens' DatafeedStatus (Maybe Text)
dsCountry
  = lens _dsCountry (\ s a -> s{_dsCountry = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeedStatus\".
dsKind :: Lens' DatafeedStatus Text
dsKind = lens _dsKind (\ s a -> s{_dsKind = a})

-- | The list of errors occurring in the feed.
dsWarnings :: Lens' DatafeedStatus [DatafeedStatusError]
dsWarnings
  = lens _dsWarnings (\ s a -> s{_dsWarnings = a}) .
      _Default
      . _Coerce

-- | The ID of the feed for which the status is reported.
dsDatafeedId :: Lens' DatafeedStatus (Maybe Word64)
dsDatafeedId
  = lens _dsDatafeedId (\ s a -> s{_dsDatafeedId = a})
      . mapping _Coerce

-- | The processing status of the feed.
dsProcessingStatus :: Lens' DatafeedStatus (Maybe Text)
dsProcessingStatus
  = lens _dsProcessingStatus
      (\ s a -> s{_dsProcessingStatus = a})

-- | The two-letter ISO 639-1 language for which the status is reported.
dsLanguage :: Lens' DatafeedStatus (Maybe Text)
dsLanguage
  = lens _dsLanguage (\ s a -> s{_dsLanguage = a})

-- | The last date at which the feed was uploaded.
dsLastUploadDate :: Lens' DatafeedStatus (Maybe Text)
dsLastUploadDate
  = lens _dsLastUploadDate
      (\ s a -> s{_dsLastUploadDate = a})

-- | The number of items in the feed that were valid.
dsItemsValid :: Lens' DatafeedStatus (Maybe Word64)
dsItemsValid
  = lens _dsItemsValid (\ s a -> s{_dsItemsValid = a})
      . mapping _Coerce

-- | The list of errors occurring in the feed.
dsErrors :: Lens' DatafeedStatus [DatafeedStatusError]
dsErrors
  = lens _dsErrors (\ s a -> s{_dsErrors = a}) .
      _Default
      . _Coerce

instance FromJSON DatafeedStatus where
        parseJSON
          = withObject "DatafeedStatus"
              (\ o ->
                 DatafeedStatus' <$>
                   (o .:? "itemsTotal") <*> (o .:? "country") <*>
                     (o .:? "kind" .!= "content#datafeedStatus")
                     <*> (o .:? "warnings" .!= mempty)
                     <*> (o .:? "datafeedId")
                     <*> (o .:? "processingStatus")
                     <*> (o .:? "language")
                     <*> (o .:? "lastUploadDate")
                     <*> (o .:? "itemsValid")
                     <*> (o .:? "errors" .!= mempty))

instance ToJSON DatafeedStatus where
        toJSON DatafeedStatus'{..}
          = object
              (catMaybes
                 [("itemsTotal" .=) <$> _dsItemsTotal,
                  ("country" .=) <$> _dsCountry,
                  Just ("kind" .= _dsKind),
                  ("warnings" .=) <$> _dsWarnings,
                  ("datafeedId" .=) <$> _dsDatafeedId,
                  ("processingStatus" .=) <$> _dsProcessingStatus,
                  ("language" .=) <$> _dsLanguage,
                  ("lastUploadDate" .=) <$> _dsLastUploadDate,
                  ("itemsValid" .=) <$> _dsItemsValid,
                  ("errors" .=) <$> _dsErrors])

--
-- /See:/ 'datafeedstatusesCustomBatchRequest' smart constructor.
newtype DatafeedstatusesCustomBatchRequest = DatafeedstatusesCustomBatchRequest'
    { _dcbrcEntries :: Maybe [DatafeedstatusesCustomBatchRequestEntry]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedstatusesCustomBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrcEntries'
datafeedstatusesCustomBatchRequest
    :: DatafeedstatusesCustomBatchRequest
datafeedstatusesCustomBatchRequest = 
    DatafeedstatusesCustomBatchRequest'
    { _dcbrcEntries = Nothing
    }

-- | The request entries to be processed in the batch.
dcbrcEntries :: Lens' DatafeedstatusesCustomBatchRequest [DatafeedstatusesCustomBatchRequestEntry]
dcbrcEntries
  = lens _dcbrcEntries (\ s a -> s{_dcbrcEntries = a})
      . _Default
      . _Coerce

instance FromJSON DatafeedstatusesCustomBatchRequest
         where
        parseJSON
          = withObject "DatafeedstatusesCustomBatchRequest"
              (\ o ->
                 DatafeedstatusesCustomBatchRequest' <$>
                   (o .:? "entries" .!= mempty))

instance ToJSON DatafeedstatusesCustomBatchRequest
         where
        toJSON DatafeedstatusesCustomBatchRequest'{..}
          = object
              (catMaybes [("entries" .=) <$> _dcbrcEntries])

--
-- /See:/ 'accountStatusDataQualityIssue' smart constructor.
data AccountStatusDataQualityIssue = AccountStatusDataQualityIssue'
    { _asdqiSubmittedValue :: !(Maybe Text)
    , _asdqiLocation :: !(Maybe Text)
    , _asdqiCountry :: !(Maybe Text)
    , _asdqiDisplayedValue :: !(Maybe Text)
    , _asdqiNumItems :: !(Maybe (Textual Word32))
    , _asdqiSeverity :: !(Maybe Text)
    , _asdqiExampleItems :: !(Maybe [AccountStatusExampleItem])
    , _asdqiLastChecked :: !(Maybe Text)
    , _asdqiId :: !(Maybe Text)
    , _asdqiDetail :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountStatusDataQualityIssue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asdqiSubmittedValue'
--
-- * 'asdqiLocation'
--
-- * 'asdqiCountry'
--
-- * 'asdqiDisplayedValue'
--
-- * 'asdqiNumItems'
--
-- * 'asdqiSeverity'
--
-- * 'asdqiExampleItems'
--
-- * 'asdqiLastChecked'
--
-- * 'asdqiId'
--
-- * 'asdqiDetail'
accountStatusDataQualityIssue
    :: AccountStatusDataQualityIssue
accountStatusDataQualityIssue = 
    AccountStatusDataQualityIssue'
    { _asdqiSubmittedValue = Nothing
    , _asdqiLocation = Nothing
    , _asdqiCountry = Nothing
    , _asdqiDisplayedValue = Nothing
    , _asdqiNumItems = Nothing
    , _asdqiSeverity = Nothing
    , _asdqiExampleItems = Nothing
    , _asdqiLastChecked = Nothing
    , _asdqiId = Nothing
    , _asdqiDetail = Nothing
    }

-- | Submitted value that causes the issue.
asdqiSubmittedValue :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiSubmittedValue
  = lens _asdqiSubmittedValue
      (\ s a -> s{_asdqiSubmittedValue = a})

-- | The attribute name that is relevant for the issue.
asdqiLocation :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiLocation
  = lens _asdqiLocation
      (\ s a -> s{_asdqiLocation = a})

-- | Country for which this issue is reported.
asdqiCountry :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiCountry
  = lens _asdqiCountry (\ s a -> s{_asdqiCountry = a})

-- | Actual value displayed on the landing page.
asdqiDisplayedValue :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiDisplayedValue
  = lens _asdqiDisplayedValue
      (\ s a -> s{_asdqiDisplayedValue = a})

-- | Number of items in the account found to have the said issue.
asdqiNumItems :: Lens' AccountStatusDataQualityIssue (Maybe Word32)
asdqiNumItems
  = lens _asdqiNumItems
      (\ s a -> s{_asdqiNumItems = a})
      . mapping _Coerce

-- | Severity of the problem.
asdqiSeverity :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiSeverity
  = lens _asdqiSeverity
      (\ s a -> s{_asdqiSeverity = a})

-- | Example items featuring the issue.
asdqiExampleItems :: Lens' AccountStatusDataQualityIssue [AccountStatusExampleItem]
asdqiExampleItems
  = lens _asdqiExampleItems
      (\ s a -> s{_asdqiExampleItems = a})
      . _Default
      . _Coerce

-- | Last time the account was checked for this issue.
asdqiLastChecked :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiLastChecked
  = lens _asdqiLastChecked
      (\ s a -> s{_asdqiLastChecked = a})

-- | Issue identifier.
asdqiId :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiId = lens _asdqiId (\ s a -> s{_asdqiId = a})

-- | A more detailed description of the issue.
asdqiDetail :: Lens' AccountStatusDataQualityIssue (Maybe Text)
asdqiDetail
  = lens _asdqiDetail (\ s a -> s{_asdqiDetail = a})

instance FromJSON AccountStatusDataQualityIssue where
        parseJSON
          = withObject "AccountStatusDataQualityIssue"
              (\ o ->
                 AccountStatusDataQualityIssue' <$>
                   (o .:? "submittedValue") <*> (o .:? "location") <*>
                     (o .:? "country")
                     <*> (o .:? "displayedValue")
                     <*> (o .:? "numItems")
                     <*> (o .:? "severity")
                     <*> (o .:? "exampleItems" .!= mempty)
                     <*> (o .:? "lastChecked")
                     <*> (o .:? "id")
                     <*> (o .:? "detail"))

instance ToJSON AccountStatusDataQualityIssue where
        toJSON AccountStatusDataQualityIssue'{..}
          = object
              (catMaybes
                 [("submittedValue" .=) <$> _asdqiSubmittedValue,
                  ("location" .=) <$> _asdqiLocation,
                  ("country" .=) <$> _asdqiCountry,
                  ("displayedValue" .=) <$> _asdqiDisplayedValue,
                  ("numItems" .=) <$> _asdqiNumItems,
                  ("severity" .=) <$> _asdqiSeverity,
                  ("exampleItems" .=) <$> _asdqiExampleItems,
                  ("lastChecked" .=) <$> _asdqiLastChecked,
                  ("id" .=) <$> _asdqiId,
                  ("detail" .=) <$> _asdqiDetail])

--
-- /See:/ 'ordersInStoreRefundLineItemRequest' smart constructor.
data OrdersInStoreRefundLineItemRequest = OrdersInStoreRefundLineItemRequest'
    { _oisrlirQuantity :: !(Maybe (Textual Word32))
    , _oisrlirLineItemId :: !(Maybe Text)
    , _oisrlirReason :: !(Maybe Text)
    , _oisrlirOperationId :: !(Maybe Text)
    , _oisrlirAmountPretax :: !(Maybe Price)
    , _oisrlirProductId :: !(Maybe Text)
    , _oisrlirAmountTax :: !(Maybe Price)
    , _oisrlirReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersInStoreRefundLineItemRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oisrlirQuantity'
--
-- * 'oisrlirLineItemId'
--
-- * 'oisrlirReason'
--
-- * 'oisrlirOperationId'
--
-- * 'oisrlirAmountPretax'
--
-- * 'oisrlirProductId'
--
-- * 'oisrlirAmountTax'
--
-- * 'oisrlirReasonText'
ordersInStoreRefundLineItemRequest
    :: OrdersInStoreRefundLineItemRequest
ordersInStoreRefundLineItemRequest = 
    OrdersInStoreRefundLineItemRequest'
    { _oisrlirQuantity = Nothing
    , _oisrlirLineItemId = Nothing
    , _oisrlirReason = Nothing
    , _oisrlirOperationId = Nothing
    , _oisrlirAmountPretax = Nothing
    , _oisrlirProductId = Nothing
    , _oisrlirAmountTax = Nothing
    , _oisrlirReasonText = Nothing
    }

-- | The quantity to return and refund.
oisrlirQuantity :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Word32)
oisrlirQuantity
  = lens _oisrlirQuantity
      (\ s a -> s{_oisrlirQuantity = a})
      . mapping _Coerce

-- | The ID of the line item to return. Either lineItemId or productId is
-- required.
oisrlirLineItemId :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Text)
oisrlirLineItemId
  = lens _oisrlirLineItemId
      (\ s a -> s{_oisrlirLineItemId = a})

-- | The reason for the return.
oisrlirReason :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Text)
oisrlirReason
  = lens _oisrlirReason
      (\ s a -> s{_oisrlirReason = a})

-- | The ID of the operation. Unique across all operations for a given order.
oisrlirOperationId :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Text)
oisrlirOperationId
  = lens _oisrlirOperationId
      (\ s a -> s{_oisrlirOperationId = a})

-- | The amount that is refunded. Required.
oisrlirAmountPretax :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Price)
oisrlirAmountPretax
  = lens _oisrlirAmountPretax
      (\ s a -> s{_oisrlirAmountPretax = a})

-- | The ID of the product to return. This is the REST ID used in the
-- products service. Either lineItemId or productId is required.
oisrlirProductId :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Text)
oisrlirProductId
  = lens _oisrlirProductId
      (\ s a -> s{_oisrlirProductId = a})

-- | Tax amount that correspond to refund amount in amountPretax. Required.
oisrlirAmountTax :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Price)
oisrlirAmountTax
  = lens _oisrlirAmountTax
      (\ s a -> s{_oisrlirAmountTax = a})

-- | The explanation of the reason.
oisrlirReasonText :: Lens' OrdersInStoreRefundLineItemRequest (Maybe Text)
oisrlirReasonText
  = lens _oisrlirReasonText
      (\ s a -> s{_oisrlirReasonText = a})

instance FromJSON OrdersInStoreRefundLineItemRequest
         where
        parseJSON
          = withObject "OrdersInStoreRefundLineItemRequest"
              (\ o ->
                 OrdersInStoreRefundLineItemRequest' <$>
                   (o .:? "quantity") <*> (o .:? "lineItemId") <*>
                     (o .:? "reason")
                     <*> (o .:? "operationId")
                     <*> (o .:? "amountPretax")
                     <*> (o .:? "productId")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersInStoreRefundLineItemRequest
         where
        toJSON OrdersInStoreRefundLineItemRequest'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _oisrlirQuantity,
                  ("lineItemId" .=) <$> _oisrlirLineItemId,
                  ("reason" .=) <$> _oisrlirReason,
                  ("operationId" .=) <$> _oisrlirOperationId,
                  ("amountPretax" .=) <$> _oisrlirAmountPretax,
                  ("productId" .=) <$> _oisrlirProductId,
                  ("amountTax" .=) <$> _oisrlirAmountTax,
                  ("reasonText" .=) <$> _oisrlirReasonText])

--
-- /See:/ 'productShippingDimension' smart constructor.
data ProductShippingDimension = ProductShippingDimension'
    { _psdValue :: !(Maybe (Textual Double))
    , _psdUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductShippingDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psdValue'
--
-- * 'psdUnit'
productShippingDimension
    :: ProductShippingDimension
productShippingDimension = 
    ProductShippingDimension'
    { _psdValue = Nothing
    , _psdUnit = Nothing
    }

-- | The dimension of the product used to calculate the shipping cost of the
-- item.
psdValue :: Lens' ProductShippingDimension (Maybe Double)
psdValue
  = lens _psdValue (\ s a -> s{_psdValue = a}) .
      mapping _Coerce

-- | The unit of value. Acceptable values are: - \"cm\" - \"in\"
psdUnit :: Lens' ProductShippingDimension (Maybe Text)
psdUnit = lens _psdUnit (\ s a -> s{_psdUnit = a})

instance FromJSON ProductShippingDimension where
        parseJSON
          = withObject "ProductShippingDimension"
              (\ o ->
                 ProductShippingDimension' <$>
                   (o .:? "value") <*> (o .:? "unit"))

instance ToJSON ProductShippingDimension where
        toJSON ProductShippingDimension'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _psdValue,
                  ("unit" .=) <$> _psdUnit])

-- | A batch entry encoding a single non-batch datafeeds response.
--
-- /See:/ 'datafeedsCustomBatchResponseEntry' smart constructor.
data DatafeedsCustomBatchResponseEntry = DatafeedsCustomBatchResponseEntry'
    { _dcbrecDatafeed :: !(Maybe Datafeed)
    , _dcbrecErrors :: !(Maybe Errors)
    , _dcbrecBatchId :: !(Maybe (Textual Word32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsCustomBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrecDatafeed'
--
-- * 'dcbrecErrors'
--
-- * 'dcbrecBatchId'
datafeedsCustomBatchResponseEntry
    :: DatafeedsCustomBatchResponseEntry
datafeedsCustomBatchResponseEntry = 
    DatafeedsCustomBatchResponseEntry'
    { _dcbrecDatafeed = Nothing
    , _dcbrecErrors = Nothing
    , _dcbrecBatchId = Nothing
    }

-- | The requested data feed. Defined if and only if the request was
-- successful.
dcbrecDatafeed :: Lens' DatafeedsCustomBatchResponseEntry (Maybe Datafeed)
dcbrecDatafeed
  = lens _dcbrecDatafeed
      (\ s a -> s{_dcbrecDatafeed = a})

-- | A list of errors defined if and only if the request failed.
dcbrecErrors :: Lens' DatafeedsCustomBatchResponseEntry (Maybe Errors)
dcbrecErrors
  = lens _dcbrecErrors (\ s a -> s{_dcbrecErrors = a})

-- | The ID of the request entry this entry responds to.
dcbrecBatchId :: Lens' DatafeedsCustomBatchResponseEntry (Maybe Word32)
dcbrecBatchId
  = lens _dcbrecBatchId
      (\ s a -> s{_dcbrecBatchId = a})
      . mapping _Coerce

instance FromJSON DatafeedsCustomBatchResponseEntry
         where
        parseJSON
          = withObject "DatafeedsCustomBatchResponseEntry"
              (\ o ->
                 DatafeedsCustomBatchResponseEntry' <$>
                   (o .:? "datafeed") <*> (o .:? "errors") <*>
                     (o .:? "batchId"))

instance ToJSON DatafeedsCustomBatchResponseEntry
         where
        toJSON DatafeedsCustomBatchResponseEntry'{..}
          = object
              (catMaybes
                 [("datafeed" .=) <$> _dcbrecDatafeed,
                  ("errors" .=) <$> _dcbrecErrors,
                  ("batchId" .=) <$> _dcbrecBatchId])

--
-- /See:/ 'ordersCustomBatchRequestEntryRefund' smart constructor.
data OrdersCustomBatchRequestEntryRefund = OrdersCustomBatchRequestEntryRefund'
    { _ocbrerAmount :: !(Maybe Price)
    , _ocbrerReason :: !(Maybe Text)
    , _ocbrerAmountPretax :: !(Maybe Price)
    , _ocbrerAmountTax :: !(Maybe Price)
    , _ocbrerReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchRequestEntryRefund' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrerAmount'
--
-- * 'ocbrerReason'
--
-- * 'ocbrerAmountPretax'
--
-- * 'ocbrerAmountTax'
--
-- * 'ocbrerReasonText'
ordersCustomBatchRequestEntryRefund
    :: OrdersCustomBatchRequestEntryRefund
ordersCustomBatchRequestEntryRefund = 
    OrdersCustomBatchRequestEntryRefund'
    { _ocbrerAmount = Nothing
    , _ocbrerReason = Nothing
    , _ocbrerAmountPretax = Nothing
    , _ocbrerAmountTax = Nothing
    , _ocbrerReasonText = Nothing
    }

-- | The amount that is refunded.
ocbrerAmount :: Lens' OrdersCustomBatchRequestEntryRefund (Maybe Price)
ocbrerAmount
  = lens _ocbrerAmount (\ s a -> s{_ocbrerAmount = a})

-- | The reason for the refund.
ocbrerReason :: Lens' OrdersCustomBatchRequestEntryRefund (Maybe Text)
ocbrerReason
  = lens _ocbrerReason (\ s a -> s{_ocbrerReason = a})

-- | The amount that is refunded. Either amount or amountPretax and amountTax
-- should be filled.
ocbrerAmountPretax :: Lens' OrdersCustomBatchRequestEntryRefund (Maybe Price)
ocbrerAmountPretax
  = lens _ocbrerAmountPretax
      (\ s a -> s{_ocbrerAmountPretax = a})

-- | Tax amount that correspond to refund amount in amountPretax.
ocbrerAmountTax :: Lens' OrdersCustomBatchRequestEntryRefund (Maybe Price)
ocbrerAmountTax
  = lens _ocbrerAmountTax
      (\ s a -> s{_ocbrerAmountTax = a})

-- | The explanation of the reason.
ocbrerReasonText :: Lens' OrdersCustomBatchRequestEntryRefund (Maybe Text)
ocbrerReasonText
  = lens _ocbrerReasonText
      (\ s a -> s{_ocbrerReasonText = a})

instance FromJSON OrdersCustomBatchRequestEntryRefund
         where
        parseJSON
          = withObject "OrdersCustomBatchRequestEntryRefund"
              (\ o ->
                 OrdersCustomBatchRequestEntryRefund' <$>
                   (o .:? "amount") <*> (o .:? "reason") <*>
                     (o .:? "amountPretax")
                     <*> (o .:? "amountTax")
                     <*> (o .:? "reasonText"))

instance ToJSON OrdersCustomBatchRequestEntryRefund
         where
        toJSON OrdersCustomBatchRequestEntryRefund'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _ocbrerAmount,
                  ("reason" .=) <$> _ocbrerReason,
                  ("amountPretax" .=) <$> _ocbrerAmountPretax,
                  ("amountTax" .=) <$> _ocbrerAmountTax,
                  ("reasonText" .=) <$> _ocbrerReasonText])

--
-- /See:/ 'datafeedstatusesListResponse' smart constructor.
data DatafeedstatusesListResponse = DatafeedstatusesListResponse'
    { _dlrlNextPageToken :: !(Maybe Text)
    , _dlrlKind :: !Text
    , _dlrlResources :: !(Maybe [DatafeedStatus])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedstatusesListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrlNextPageToken'
--
-- * 'dlrlKind'
--
-- * 'dlrlResources'
datafeedstatusesListResponse
    :: DatafeedstatusesListResponse
datafeedstatusesListResponse = 
    DatafeedstatusesListResponse'
    { _dlrlNextPageToken = Nothing
    , _dlrlKind = "content#datafeedstatusesListResponse"
    , _dlrlResources = Nothing
    }

-- | The token for the retrieval of the next page of datafeed statuses.
dlrlNextPageToken :: Lens' DatafeedstatusesListResponse (Maybe Text)
dlrlNextPageToken
  = lens _dlrlNextPageToken
      (\ s a -> s{_dlrlNextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#datafeedstatusesListResponse\".
dlrlKind :: Lens' DatafeedstatusesListResponse Text
dlrlKind = lens _dlrlKind (\ s a -> s{_dlrlKind = a})

dlrlResources :: Lens' DatafeedstatusesListResponse [DatafeedStatus]
dlrlResources
  = lens _dlrlResources
      (\ s a -> s{_dlrlResources = a})
      . _Default
      . _Coerce

instance FromJSON DatafeedstatusesListResponse where
        parseJSON
          = withObject "DatafeedstatusesListResponse"
              (\ o ->
                 DatafeedstatusesListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!=
                        "content#datafeedstatusesListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON DatafeedstatusesListResponse where
        toJSON DatafeedstatusesListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _dlrlNextPageToken,
                  Just ("kind" .= _dlrlKind),
                  ("resources" .=) <$> _dlrlResources])

--
-- /See:/ 'productsListResponse' smart constructor.
data ProductsListResponse = ProductsListResponse'
    { _plr1NextPageToken :: !(Maybe Text)
    , _plr1Kind :: !Text
    , _plr1Resources :: !(Maybe [Product])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plr1NextPageToken'
--
-- * 'plr1Kind'
--
-- * 'plr1Resources'
productsListResponse
    :: ProductsListResponse
productsListResponse = 
    ProductsListResponse'
    { _plr1NextPageToken = Nothing
    , _plr1Kind = "content#productsListResponse"
    , _plr1Resources = Nothing
    }

-- | The token for the retrieval of the next page of products.
plr1NextPageToken :: Lens' ProductsListResponse (Maybe Text)
plr1NextPageToken
  = lens _plr1NextPageToken
      (\ s a -> s{_plr1NextPageToken = a})

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#productsListResponse\".
plr1Kind :: Lens' ProductsListResponse Text
plr1Kind = lens _plr1Kind (\ s a -> s{_plr1Kind = a})

plr1Resources :: Lens' ProductsListResponse [Product]
plr1Resources
  = lens _plr1Resources
      (\ s a -> s{_plr1Resources = a})
      . _Default
      . _Coerce

instance FromJSON ProductsListResponse where
        parseJSON
          = withObject "ProductsListResponse"
              (\ o ->
                 ProductsListResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "content#productsListResponse")
                     <*> (o .:? "resources" .!= mempty))

instance ToJSON ProductsListResponse where
        toJSON ProductsListResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _plr1NextPageToken,
                  Just ("kind" .= _plr1Kind),
                  ("resources" .=) <$> _plr1Resources])

--
-- /See:/ 'accountAdwordsLink' smart constructor.
data AccountAdwordsLink = AccountAdwordsLink'
    { _aalStatus :: !(Maybe Text)
    , _aalAdwordsId :: !(Maybe (Textual Word64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountAdwordsLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aalStatus'
--
-- * 'aalAdwordsId'
accountAdwordsLink
    :: AccountAdwordsLink
accountAdwordsLink = 
    AccountAdwordsLink'
    { _aalStatus = Nothing
    , _aalAdwordsId = Nothing
    }

-- | Status of the link between this Merchant Center account and the AdWords
-- account. Upon retrieval, it represents the actual status of the link and
-- can be either active if it was approved in Google AdWords or pending if
-- it\'s pending approval. Upon insertion, it represents the intended
-- status of the link. Re-uploading a link with status active when it\'s
-- still pending or with status pending when it\'s already active will have
-- no effect: the status will remain unchanged. Re-uploading a link with
-- deprecated status inactive is equivalent to not submitting the link at
-- all and will delete the link if it was active or cancel the link request
-- if it was pending.
aalStatus :: Lens' AccountAdwordsLink (Maybe Text)
aalStatus
  = lens _aalStatus (\ s a -> s{_aalStatus = a})

-- | Customer ID of the AdWords account.
aalAdwordsId :: Lens' AccountAdwordsLink (Maybe Word64)
aalAdwordsId
  = lens _aalAdwordsId (\ s a -> s{_aalAdwordsId = a})
      . mapping _Coerce

instance FromJSON AccountAdwordsLink where
        parseJSON
          = withObject "AccountAdwordsLink"
              (\ o ->
                 AccountAdwordsLink' <$>
                   (o .:? "status") <*> (o .:? "adwordsId"))

instance ToJSON AccountAdwordsLink where
        toJSON AccountAdwordsLink'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _aalStatus,
                  ("adwordsId" .=) <$> _aalAdwordsId])

--
-- /See:/ 'orderCancellation' smart constructor.
data OrderCancellation = OrderCancellation'
    { _ocQuantity :: !(Maybe (Textual Word32))
    , _ocActor :: !(Maybe Text)
    , _ocReason :: !(Maybe Text)
    , _ocCreationDate :: !(Maybe Text)
    , _ocReasonText :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderCancellation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocQuantity'
--
-- * 'ocActor'
--
-- * 'ocReason'
--
-- * 'ocCreationDate'
--
-- * 'ocReasonText'
orderCancellation
    :: OrderCancellation
orderCancellation = 
    OrderCancellation'
    { _ocQuantity = Nothing
    , _ocActor = Nothing
    , _ocReason = Nothing
    , _ocCreationDate = Nothing
    , _ocReasonText = Nothing
    }

-- | The quantity that was canceled.
ocQuantity :: Lens' OrderCancellation (Maybe Word32)
ocQuantity
  = lens _ocQuantity (\ s a -> s{_ocQuantity = a}) .
      mapping _Coerce

-- | The actor that created the cancellation.
ocActor :: Lens' OrderCancellation (Maybe Text)
ocActor = lens _ocActor (\ s a -> s{_ocActor = a})

-- | The reason for the cancellation. Orders that are cancelled with a
-- noInventory reason will lead to the removal of the product from POG
-- until you make an update to that product. This will not affect your
-- Shopping ads.
ocReason :: Lens' OrderCancellation (Maybe Text)
ocReason = lens _ocReason (\ s a -> s{_ocReason = a})

-- | Date on which the cancellation has been created, in ISO 8601 format.
ocCreationDate :: Lens' OrderCancellation (Maybe Text)
ocCreationDate
  = lens _ocCreationDate
      (\ s a -> s{_ocCreationDate = a})

-- | The explanation of the reason.
ocReasonText :: Lens' OrderCancellation (Maybe Text)
ocReasonText
  = lens _ocReasonText (\ s a -> s{_ocReasonText = a})

instance FromJSON OrderCancellation where
        parseJSON
          = withObject "OrderCancellation"
              (\ o ->
                 OrderCancellation' <$>
                   (o .:? "quantity") <*> (o .:? "actor") <*>
                     (o .:? "reason")
                     <*> (o .:? "creationDate")
                     <*> (o .:? "reasonText"))

instance ToJSON OrderCancellation where
        toJSON OrderCancellation'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _ocQuantity,
                  ("actor" .=) <$> _ocActor,
                  ("reason" .=) <$> _ocReason,
                  ("creationDate" .=) <$> _ocCreationDate,
                  ("reasonText" .=) <$> _ocReasonText])

--
-- /See:/ 'ordersCustomBatchResponse' smart constructor.
data OrdersCustomBatchResponse = OrdersCustomBatchResponse'
    { _ocbrcEntries :: !(Maybe [OrdersCustomBatchResponseEntry])
    , _ocbrcKind :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCustomBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocbrcEntries'
--
-- * 'ocbrcKind'
ordersCustomBatchResponse
    :: OrdersCustomBatchResponse
ordersCustomBatchResponse = 
    OrdersCustomBatchResponse'
    { _ocbrcEntries = Nothing
    , _ocbrcKind = "content#ordersCustomBatchResponse"
    }

-- | The result of the execution of the batch requests.
ocbrcEntries :: Lens' OrdersCustomBatchResponse [OrdersCustomBatchResponseEntry]
ocbrcEntries
  = lens _ocbrcEntries (\ s a -> s{_ocbrcEntries = a})
      . _Default
      . _Coerce

-- | Identifies what kind of resource this is. Value: the fixed string
-- \"content#ordersCustomBatchResponse\".
ocbrcKind :: Lens' OrdersCustomBatchResponse Text
ocbrcKind
  = lens _ocbrcKind (\ s a -> s{_ocbrcKind = a})

instance FromJSON OrdersCustomBatchResponse where
        parseJSON
          = withObject "OrdersCustomBatchResponse"
              (\ o ->
                 OrdersCustomBatchResponse' <$>
                   (o .:? "entries" .!= mempty) <*>
                     (o .:? "kind" .!=
                        "content#ordersCustomBatchResponse"))

instance ToJSON OrdersCustomBatchResponse where
        toJSON OrdersCustomBatchResponse'{..}
          = object
              (catMaybes
                 [("entries" .=) <$> _ocbrcEntries,
                  Just ("kind" .= _ocbrcKind)])
