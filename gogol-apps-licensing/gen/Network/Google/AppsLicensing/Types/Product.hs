{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AppsLicensing.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AppsLicensing.Types.Product where

import Network.Google.AppsLicensing.Types.Sum
import Network.Google.Prelude

-- | Template for LicenseAssignment Insert request
--
-- /See:/ 'licenseAssignmentInsert' smart constructor.
newtype LicenseAssignmentInsert = LicenseAssignmentInsert'
    { _laiUserId :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LicenseAssignmentInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laiUserId'
licenseAssignmentInsert
    :: LicenseAssignmentInsert
licenseAssignmentInsert = 
    LicenseAssignmentInsert'
    { _laiUserId = Nothing
    }

-- | Email id of the user
laiUserId :: Lens' LicenseAssignmentInsert (Maybe Text)
laiUserId
  = lens _laiUserId (\ s a -> s{_laiUserId = a})

instance FromJSON LicenseAssignmentInsert where
        parseJSON
          = withObject "LicenseAssignmentInsert"
              (\ o ->
                 LicenseAssignmentInsert' <$> (o .:? "userId"))

instance ToJSON LicenseAssignmentInsert where
        toJSON LicenseAssignmentInsert'{..}
          = object (catMaybes [("userId" .=) <$> _laiUserId])

-- | LicesnseAssignment List for a given product\/sku for a customer.
--
-- /See:/ 'licenseAssignmentList' smart constructor.
data LicenseAssignmentList = LicenseAssignmentList'
    { _lalEtag :: !(Maybe Text)
    , _lalNextPageToken :: !(Maybe Text)
    , _lalKind :: !Text
    , _lalItems :: !(Maybe [LicenseAssignment])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LicenseAssignmentList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lalEtag'
--
-- * 'lalNextPageToken'
--
-- * 'lalKind'
--
-- * 'lalItems'
licenseAssignmentList
    :: LicenseAssignmentList
licenseAssignmentList = 
    LicenseAssignmentList'
    { _lalEtag = Nothing
    , _lalNextPageToken = Nothing
    , _lalKind = "licensing#licenseAssignmentList"
    , _lalItems = Nothing
    }

-- | ETag of the resource.
lalEtag :: Lens' LicenseAssignmentList (Maybe Text)
lalEtag = lens _lalEtag (\ s a -> s{_lalEtag = a})

-- | The continuation token, used to page through large result sets. Provide
-- this value in a subsequent request to return the next page of results.
lalNextPageToken :: Lens' LicenseAssignmentList (Maybe Text)
lalNextPageToken
  = lens _lalNextPageToken
      (\ s a -> s{_lalNextPageToken = a})

-- | Identifies the resource as a collection of LicenseAssignments.
lalKind :: Lens' LicenseAssignmentList Text
lalKind = lens _lalKind (\ s a -> s{_lalKind = a})

-- | The LicenseAssignments in this page of results.
lalItems :: Lens' LicenseAssignmentList [LicenseAssignment]
lalItems
  = lens _lalItems (\ s a -> s{_lalItems = a}) .
      _Default
      . _Coerce

instance FromJSON LicenseAssignmentList where
        parseJSON
          = withObject "LicenseAssignmentList"
              (\ o ->
                 LicenseAssignmentList' <$>
                   (o .:? "etag") <*> (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "licensing#licenseAssignmentList")
                     <*> (o .:? "items" .!= mempty))

instance ToJSON LicenseAssignmentList where
        toJSON LicenseAssignmentList'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _lalEtag,
                  ("nextPageToken" .=) <$> _lalNextPageToken,
                  Just ("kind" .= _lalKind),
                  ("items" .=) <$> _lalItems])

-- | Template for LiscenseAssignment Resource
--
-- /See:/ 'licenseAssignment' smart constructor.
data LicenseAssignment = LicenseAssignment'
    { _laProductName :: !(Maybe Text)
    , _laEtags :: !(Maybe Text)
    , _laSKUName :: !(Maybe Text)
    , _laKind :: !Text
    , _laSKUId :: !(Maybe Text)
    , _laUserId :: !(Maybe Text)
    , _laSelfLink :: !(Maybe Text)
    , _laProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LicenseAssignment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laProductName'
--
-- * 'laEtags'
--
-- * 'laSKUName'
--
-- * 'laKind'
--
-- * 'laSKUId'
--
-- * 'laUserId'
--
-- * 'laSelfLink'
--
-- * 'laProductId'
licenseAssignment
    :: LicenseAssignment
licenseAssignment = 
    LicenseAssignment'
    { _laProductName = Nothing
    , _laEtags = Nothing
    , _laSKUName = Nothing
    , _laKind = "licensing#licenseAssignment"
    , _laSKUId = Nothing
    , _laUserId = Nothing
    , _laSelfLink = Nothing
    , _laProductId = Nothing
    }

-- | Display Name of the product.
laProductName :: Lens' LicenseAssignment (Maybe Text)
laProductName
  = lens _laProductName
      (\ s a -> s{_laProductName = a})

-- | ETag of the resource.
laEtags :: Lens' LicenseAssignment (Maybe Text)
laEtags = lens _laEtags (\ s a -> s{_laEtags = a})

-- | Display Name of the sku of the product.
laSKUName :: Lens' LicenseAssignment (Maybe Text)
laSKUName
  = lens _laSKUName (\ s a -> s{_laSKUName = a})

-- | Identifies the resource as a LicenseAssignment.
laKind :: Lens' LicenseAssignment Text
laKind = lens _laKind (\ s a -> s{_laKind = a})

-- | Id of the sku of the product.
laSKUId :: Lens' LicenseAssignment (Maybe Text)
laSKUId = lens _laSKUId (\ s a -> s{_laSKUId = a})

-- | Email id of the user.
laUserId :: Lens' LicenseAssignment (Maybe Text)
laUserId = lens _laUserId (\ s a -> s{_laUserId = a})

-- | Link to this page.
laSelfLink :: Lens' LicenseAssignment (Maybe Text)
laSelfLink
  = lens _laSelfLink (\ s a -> s{_laSelfLink = a})

-- | Id of the product.
laProductId :: Lens' LicenseAssignment (Maybe Text)
laProductId
  = lens _laProductId (\ s a -> s{_laProductId = a})

instance FromJSON LicenseAssignment where
        parseJSON
          = withObject "LicenseAssignment"
              (\ o ->
                 LicenseAssignment' <$>
                   (o .:? "productName") <*> (o .:? "etags") <*>
                     (o .:? "skuName")
                     <*> (o .:? "kind" .!= "licensing#licenseAssignment")
                     <*> (o .:? "skuId")
                     <*> (o .:? "userId")
                     <*> (o .:? "selfLink")
                     <*> (o .:? "productId"))

instance ToJSON LicenseAssignment where
        toJSON LicenseAssignment'{..}
          = object
              (catMaybes
                 [("productName" .=) <$> _laProductName,
                  ("etags" .=) <$> _laEtags,
                  ("skuName" .=) <$> _laSKUName,
                  Just ("kind" .= _laKind), ("skuId" .=) <$> _laSKUId,
                  ("userId" .=) <$> _laUserId,
                  ("selfLink" .=) <$> _laSelfLink,
                  ("productId" .=) <$> _laProductId])
