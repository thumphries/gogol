{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Manufacturers.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Manufacturers.Types.Product where

import Network.Google.Manufacturers.Types.Sum
import Network.Google.Prelude

-- | An image.
--
-- /See:/ 'image' smart constructor.
data Image = Image'
    { _iStatus :: !(Maybe ImageStatus)
    , _iImageURL :: !(Maybe Text)
    , _iType :: !(Maybe ImageType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iStatus'
--
-- * 'iImageURL'
--
-- * 'iType'
image
    :: Image
image = 
    Image'
    { _iStatus = Nothing
    , _iImageURL = Nothing
    , _iType = Nothing
    }

-- | The status of the image. \'OutputOnly
iStatus :: Lens' Image (Maybe ImageStatus)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | The URL of the image. For crawled images, this is the provided URL. For
-- uploaded images, this is a serving URL from Google if the image has been
-- processed successfully.
iImageURL :: Lens' Image (Maybe Text)
iImageURL
  = lens _iImageURL (\ s a -> s{_iImageURL = a})

-- | The type of the image, i.e., crawled or uploaded. \'OutputOnly
iType :: Lens' Image (Maybe ImageType)
iType = lens _iType (\ s a -> s{_iType = a})

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ o ->
                 Image' <$>
                   (o .:? "status") <*> (o .:? "imageUrl") <*>
                     (o .:? "type"))

instance ToJSON Image where
        toJSON Image'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _iStatus,
                  ("imageUrl" .=) <$> _iImageURL,
                  ("type" .=) <$> _iType])

-- | A feature description of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#featuredesc.
--
-- /See:/ 'featureDescription' smart constructor.
data FeatureDescription = FeatureDescription'
    { _fdImage :: !(Maybe Image)
    , _fdText :: !(Maybe Text)
    , _fdHeadline :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FeatureDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdImage'
--
-- * 'fdText'
--
-- * 'fdHeadline'
featureDescription
    :: FeatureDescription
featureDescription = 
    FeatureDescription'
    { _fdImage = Nothing
    , _fdText = Nothing
    , _fdHeadline = Nothing
    }

-- | An optional image describing the feature.
fdImage :: Lens' FeatureDescription (Maybe Image)
fdImage = lens _fdImage (\ s a -> s{_fdImage = a})

-- | A detailed description of the feature.
fdText :: Lens' FeatureDescription (Maybe Text)
fdText = lens _fdText (\ s a -> s{_fdText = a})

-- | A short description of the feature.
fdHeadline :: Lens' FeatureDescription (Maybe Text)
fdHeadline
  = lens _fdHeadline (\ s a -> s{_fdHeadline = a})

instance FromJSON FeatureDescription where
        parseJSON
          = withObject "FeatureDescription"
              (\ o ->
                 FeatureDescription' <$>
                   (o .:? "image") <*> (o .:? "text") <*>
                     (o .:? "headline"))

instance ToJSON FeatureDescription where
        toJSON FeatureDescription'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _fdImage, ("text" .=) <$> _fdText,
                  ("headline" .=) <$> _fdHeadline])

-- | A product detail of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#productdetail.
--
-- /See:/ 'productDetail' smart constructor.
data ProductDetail = ProductDetail'
    { _pdAttributeValue :: !(Maybe Text)
    , _pdAttributeName :: !(Maybe Text)
    , _pdSectionName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdAttributeValue'
--
-- * 'pdAttributeName'
--
-- * 'pdSectionName'
productDetail
    :: ProductDetail
productDetail = 
    ProductDetail'
    { _pdAttributeValue = Nothing
    , _pdAttributeName = Nothing
    , _pdSectionName = Nothing
    }

-- | The value of the attribute.
pdAttributeValue :: Lens' ProductDetail (Maybe Text)
pdAttributeValue
  = lens _pdAttributeValue
      (\ s a -> s{_pdAttributeValue = a})

-- | The name of the attribute.
pdAttributeName :: Lens' ProductDetail (Maybe Text)
pdAttributeName
  = lens _pdAttributeName
      (\ s a -> s{_pdAttributeName = a})

-- | A short section name that can be reused between multiple product
-- details.
pdSectionName :: Lens' ProductDetail (Maybe Text)
pdSectionName
  = lens _pdSectionName
      (\ s a -> s{_pdSectionName = a})

instance FromJSON ProductDetail where
        parseJSON
          = withObject "ProductDetail"
              (\ o ->
                 ProductDetail' <$>
                   (o .:? "attributeValue") <*> (o .:? "attributeName")
                     <*> (o .:? "sectionName"))

instance ToJSON ProductDetail where
        toJSON ProductDetail'{..}
          = object
              (catMaybes
                 [("attributeValue" .=) <$> _pdAttributeValue,
                  ("attributeName" .=) <$> _pdAttributeName,
                  ("sectionName" .=) <$> _pdSectionName])

-- | A generic empty message that you can re-use to avoid defining duplicated
-- empty messages in your APIs. A typical example is to use it as the
-- request or the response type of an API method. For instance: service Foo
-- { rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty); } The
-- JSON representation for \`Empty\` is empty JSON object \`{}\`.
--
-- /See:/ 'empty' smart constructor.
data Empty =
    Empty' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Empty' with the minimum fields required to make a request.
--
empty
    :: Empty
empty = Empty'

instance FromJSON Empty where
        parseJSON = withObject "Empty" (\ o -> pure Empty')

instance ToJSON Empty where
        toJSON = const emptyObject

-- | The number of products in a single package. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#count.
--
-- /See:/ 'count' smart constructor.
data Count = Count'
    { _cValue :: !(Maybe (Textual Int64))
    , _cUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Count' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cValue'
--
-- * 'cUnit'
count
    :: Count
count = 
    Count'
    { _cValue = Nothing
    , _cUnit = Nothing
    }

-- | The numeric value of the number of products in a package.
cValue :: Lens' Count (Maybe Int64)
cValue
  = lens _cValue (\ s a -> s{_cValue = a}) .
      mapping _Coerce

-- | The unit in which these products are counted.
cUnit :: Lens' Count (Maybe Text)
cUnit = lens _cUnit (\ s a -> s{_cUnit = a})

instance FromJSON Count where
        parseJSON
          = withObject "Count"
              (\ o ->
                 Count' <$> (o .:? "value") <*> (o .:? "unit"))

instance ToJSON Count where
        toJSON Count'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _cValue, ("unit" .=) <$> _cUnit])

-- | The capacity of a product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#capacity.
--
-- /See:/ 'capacity' smart constructor.
data Capacity = Capacity'
    { _capValue :: !(Maybe (Textual Int64))
    , _capUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Capacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capValue'
--
-- * 'capUnit'
capacity
    :: Capacity
capacity = 
    Capacity'
    { _capValue = Nothing
    , _capUnit = Nothing
    }

-- | The numeric value of the capacity.
capValue :: Lens' Capacity (Maybe Int64)
capValue
  = lens _capValue (\ s a -> s{_capValue = a}) .
      mapping _Coerce

-- | The unit of the capacity, i.e., MB, GB, or TB.
capUnit :: Lens' Capacity (Maybe Text)
capUnit = lens _capUnit (\ s a -> s{_capUnit = a})

instance FromJSON Capacity where
        parseJSON
          = withObject "Capacity"
              (\ o ->
                 Capacity' <$> (o .:? "value") <*> (o .:? "unit"))

instance ToJSON Capacity where
        toJSON Capacity'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _capValue,
                  ("unit" .=) <$> _capUnit])

-- | Attributes of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116.
--
-- /See:/ 'attributes' smart constructor.
data Attributes = Attributes'
    { _aProductName :: !(Maybe Text)
    , _aScent :: !(Maybe Text)
    , _aImageLink :: !(Maybe Image)
    , _aFeatureDescription :: !(Maybe [FeatureDescription])
    , _aProductDetail :: !(Maybe [ProductDetail])
    , _aProductLine :: !(Maybe Text)
    , _aColor :: !(Maybe Text)
    , _aSize :: !(Maybe Text)
    , _aFlavor :: !(Maybe Text)
    , _aPattern :: !(Maybe Text)
    , _aSizeSystem :: !(Maybe Text)
    , _aMaterial :: !(Maybe Text)
    , _aFormat :: !(Maybe Text)
    , _aProductType :: !(Maybe [Text])
    , _aCount :: !(Maybe Count)
    , _aDisclosureDate :: !(Maybe Text)
    , _aBrand :: !(Maybe Text)
    , _aAdditionalImageLink :: !(Maybe [Image])
    , _aVideoLink :: !(Maybe [Text])
    , _aCapacity :: !(Maybe Capacity)
    , _aGtin :: !(Maybe [Text])
    , _aAgeGroup :: !(Maybe Text)
    , _aGender :: !(Maybe Text)
    , _aSuggestedRetailPrice :: !(Maybe Price)
    , _aItemGroupId :: !(Maybe Text)
    , _aTargetClientId :: !(Maybe Text)
    , _aSizeType :: !(Maybe Text)
    , _aReleaseDate :: !(Maybe Text)
    , _aTitle :: !(Maybe Text)
    , _aMpn :: !(Maybe Text)
    , _aProductPageURL :: !(Maybe Text)
    , _aDescription :: !(Maybe Text)
    , _aTheme :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aProductName'
--
-- * 'aScent'
--
-- * 'aImageLink'
--
-- * 'aFeatureDescription'
--
-- * 'aProductDetail'
--
-- * 'aProductLine'
--
-- * 'aColor'
--
-- * 'aSize'
--
-- * 'aFlavor'
--
-- * 'aPattern'
--
-- * 'aSizeSystem'
--
-- * 'aMaterial'
--
-- * 'aFormat'
--
-- * 'aProductType'
--
-- * 'aCount'
--
-- * 'aDisclosureDate'
--
-- * 'aBrand'
--
-- * 'aAdditionalImageLink'
--
-- * 'aVideoLink'
--
-- * 'aCapacity'
--
-- * 'aGtin'
--
-- * 'aAgeGroup'
--
-- * 'aGender'
--
-- * 'aSuggestedRetailPrice'
--
-- * 'aItemGroupId'
--
-- * 'aTargetClientId'
--
-- * 'aSizeType'
--
-- * 'aReleaseDate'
--
-- * 'aTitle'
--
-- * 'aMpn'
--
-- * 'aProductPageURL'
--
-- * 'aDescription'
--
-- * 'aTheme'
attributes
    :: Attributes
attributes = 
    Attributes'
    { _aProductName = Nothing
    , _aScent = Nothing
    , _aImageLink = Nothing
    , _aFeatureDescription = Nothing
    , _aProductDetail = Nothing
    , _aProductLine = Nothing
    , _aColor = Nothing
    , _aSize = Nothing
    , _aFlavor = Nothing
    , _aPattern = Nothing
    , _aSizeSystem = Nothing
    , _aMaterial = Nothing
    , _aFormat = Nothing
    , _aProductType = Nothing
    , _aCount = Nothing
    , _aDisclosureDate = Nothing
    , _aBrand = Nothing
    , _aAdditionalImageLink = Nothing
    , _aVideoLink = Nothing
    , _aCapacity = Nothing
    , _aGtin = Nothing
    , _aAgeGroup = Nothing
    , _aGender = Nothing
    , _aSuggestedRetailPrice = Nothing
    , _aItemGroupId = Nothing
    , _aTargetClientId = Nothing
    , _aSizeType = Nothing
    , _aReleaseDate = Nothing
    , _aTitle = Nothing
    , _aMpn = Nothing
    , _aProductPageURL = Nothing
    , _aDescription = Nothing
    , _aTheme = Nothing
    }

-- | The canonical name of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#productname.
aProductName :: Lens' Attributes (Maybe Text)
aProductName
  = lens _aProductName (\ s a -> s{_aProductName = a})

-- | The scent of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#scent.
aScent :: Lens' Attributes (Maybe Text)
aScent = lens _aScent (\ s a -> s{_aScent = a})

-- | The image of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#image.
aImageLink :: Lens' Attributes (Maybe Image)
aImageLink
  = lens _aImageLink (\ s a -> s{_aImageLink = a})

-- | The rich format description of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#featuredesc.
aFeatureDescription :: Lens' Attributes [FeatureDescription]
aFeatureDescription
  = lens _aFeatureDescription
      (\ s a -> s{_aFeatureDescription = a})
      . _Default
      . _Coerce

-- | The details of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#productdetail.
aProductDetail :: Lens' Attributes [ProductDetail]
aProductDetail
  = lens _aProductDetail
      (\ s a -> s{_aProductDetail = a})
      . _Default
      . _Coerce

-- | The name of the group of products related to the product. For more
-- information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#productline.
aProductLine :: Lens' Attributes (Maybe Text)
aProductLine
  = lens _aProductLine (\ s a -> s{_aProductLine = a})

-- | The color of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#color.
aColor :: Lens' Attributes (Maybe Text)
aColor = lens _aColor (\ s a -> s{_aColor = a})

-- | The size of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#size.
aSize :: Lens' Attributes (Maybe Text)
aSize = lens _aSize (\ s a -> s{_aSize = a})

-- | The flavor of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#flavor.
aFlavor :: Lens' Attributes (Maybe Text)
aFlavor = lens _aFlavor (\ s a -> s{_aFlavor = a})

-- | The pattern of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#pattern.
aPattern :: Lens' Attributes (Maybe Text)
aPattern = lens _aPattern (\ s a -> s{_aPattern = a})

-- | The size system of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#sizesystem.
aSizeSystem :: Lens' Attributes (Maybe Text)
aSizeSystem
  = lens _aSizeSystem (\ s a -> s{_aSizeSystem = a})

-- | The material of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#material.
aMaterial :: Lens' Attributes (Maybe Text)
aMaterial
  = lens _aMaterial (\ s a -> s{_aMaterial = a})

-- | The format of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#format.
aFormat :: Lens' Attributes (Maybe Text)
aFormat = lens _aFormat (\ s a -> s{_aFormat = a})

-- | The type or category of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#producttype.
aProductType :: Lens' Attributes [Text]
aProductType
  = lens _aProductType (\ s a -> s{_aProductType = a})
      . _Default
      . _Coerce

-- | The count of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#count.
aCount :: Lens' Attributes (Maybe Count)
aCount = lens _aCount (\ s a -> s{_aCount = a})

-- | The disclosure date of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#disclosure.
aDisclosureDate :: Lens' Attributes (Maybe Text)
aDisclosureDate
  = lens _aDisclosureDate
      (\ s a -> s{_aDisclosureDate = a})

-- | The brand name of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#brand.
aBrand :: Lens' Attributes (Maybe Text)
aBrand = lens _aBrand (\ s a -> s{_aBrand = a})

-- | The additional images of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#addlimage.
aAdditionalImageLink :: Lens' Attributes [Image]
aAdditionalImageLink
  = lens _aAdditionalImageLink
      (\ s a -> s{_aAdditionalImageLink = a})
      . _Default
      . _Coerce

-- | The videos of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#video.
aVideoLink :: Lens' Attributes [Text]
aVideoLink
  = lens _aVideoLink (\ s a -> s{_aVideoLink = a}) .
      _Default
      . _Coerce

-- | The capacity of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#capacity.
aCapacity :: Lens' Attributes (Maybe Capacity)
aCapacity
  = lens _aCapacity (\ s a -> s{_aCapacity = a})

-- | The Global Trade Item Number (GTIN) of the product. For more
-- information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#gtin.
aGtin :: Lens' Attributes [Text]
aGtin
  = lens _aGtin (\ s a -> s{_aGtin = a}) . _Default .
      _Coerce

-- | The target age group of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#agegroup.
aAgeGroup :: Lens' Attributes (Maybe Text)
aAgeGroup
  = lens _aAgeGroup (\ s a -> s{_aAgeGroup = a})

-- | The target gender of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#gender.
aGender :: Lens' Attributes (Maybe Text)
aGender = lens _aGender (\ s a -> s{_aGender = a})

-- | The suggested retail price (MSRP) of the product. For more information,
-- see https:\/\/support.google.com\/manufacturers\/answer\/6124116#price.
aSuggestedRetailPrice :: Lens' Attributes (Maybe Price)
aSuggestedRetailPrice
  = lens _aSuggestedRetailPrice
      (\ s a -> s{_aSuggestedRetailPrice = a})

-- | The item group id of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#itemgroupid.
aItemGroupId :: Lens' Attributes (Maybe Text)
aItemGroupId
  = lens _aItemGroupId (\ s a -> s{_aItemGroupId = a})

-- | The target client id. Should only be used in the accounts of the data
-- partners.
aTargetClientId :: Lens' Attributes (Maybe Text)
aTargetClientId
  = lens _aTargetClientId
      (\ s a -> s{_aTargetClientId = a})

-- | The size type of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#sizetype.
aSizeType :: Lens' Attributes (Maybe Text)
aSizeType
  = lens _aSizeType (\ s a -> s{_aSizeType = a})

-- | The release date of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#release.
aReleaseDate :: Lens' Attributes (Maybe Text)
aReleaseDate
  = lens _aReleaseDate (\ s a -> s{_aReleaseDate = a})

-- | The title of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#title.
aTitle :: Lens' Attributes (Maybe Text)
aTitle = lens _aTitle (\ s a -> s{_aTitle = a})

-- | The Manufacturer Part Number (MPN) of the product. For more information,
-- see https:\/\/support.google.com\/manufacturers\/answer\/6124116#mpn.
aMpn :: Lens' Attributes (Maybe Text)
aMpn = lens _aMpn (\ s a -> s{_aMpn = a})

-- | The URL of the detail page of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#productpage.
aProductPageURL :: Lens' Attributes (Maybe Text)
aProductPageURL
  = lens _aProductPageURL
      (\ s a -> s{_aProductPageURL = a})

-- | The description of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#description.
aDescription :: Lens' Attributes (Maybe Text)
aDescription
  = lens _aDescription (\ s a -> s{_aDescription = a})

-- | The theme of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#theme.
aTheme :: Lens' Attributes (Maybe Text)
aTheme = lens _aTheme (\ s a -> s{_aTheme = a})

instance FromJSON Attributes where
        parseJSON
          = withObject "Attributes"
              (\ o ->
                 Attributes' <$>
                   (o .:? "productName") <*> (o .:? "scent") <*>
                     (o .:? "imageLink")
                     <*> (o .:? "featureDescription" .!= mempty)
                     <*> (o .:? "productDetail" .!= mempty)
                     <*> (o .:? "productLine")
                     <*> (o .:? "color")
                     <*> (o .:? "size")
                     <*> (o .:? "flavor")
                     <*> (o .:? "pattern")
                     <*> (o .:? "sizeSystem")
                     <*> (o .:? "material")
                     <*> (o .:? "format")
                     <*> (o .:? "productType" .!= mempty)
                     <*> (o .:? "count")
                     <*> (o .:? "disclosureDate")
                     <*> (o .:? "brand")
                     <*> (o .:? "additionalImageLink" .!= mempty)
                     <*> (o .:? "videoLink" .!= mempty)
                     <*> (o .:? "capacity")
                     <*> (o .:? "gtin" .!= mempty)
                     <*> (o .:? "ageGroup")
                     <*> (o .:? "gender")
                     <*> (o .:? "suggestedRetailPrice")
                     <*> (o .:? "itemGroupId")
                     <*> (o .:? "targetClientId")
                     <*> (o .:? "sizeType")
                     <*> (o .:? "releaseDate")
                     <*> (o .:? "title")
                     <*> (o .:? "mpn")
                     <*> (o .:? "productPageUrl")
                     <*> (o .:? "description")
                     <*> (o .:? "theme"))

instance ToJSON Attributes where
        toJSON Attributes'{..}
          = object
              (catMaybes
                 [("productName" .=) <$> _aProductName,
                  ("scent" .=) <$> _aScent,
                  ("imageLink" .=) <$> _aImageLink,
                  ("featureDescription" .=) <$> _aFeatureDescription,
                  ("productDetail" .=) <$> _aProductDetail,
                  ("productLine" .=) <$> _aProductLine,
                  ("color" .=) <$> _aColor, ("size" .=) <$> _aSize,
                  ("flavor" .=) <$> _aFlavor,
                  ("pattern" .=) <$> _aPattern,
                  ("sizeSystem" .=) <$> _aSizeSystem,
                  ("material" .=) <$> _aMaterial,
                  ("format" .=) <$> _aFormat,
                  ("productType" .=) <$> _aProductType,
                  ("count" .=) <$> _aCount,
                  ("disclosureDate" .=) <$> _aDisclosureDate,
                  ("brand" .=) <$> _aBrand,
                  ("additionalImageLink" .=) <$> _aAdditionalImageLink,
                  ("videoLink" .=) <$> _aVideoLink,
                  ("capacity" .=) <$> _aCapacity,
                  ("gtin" .=) <$> _aGtin,
                  ("ageGroup" .=) <$> _aAgeGroup,
                  ("gender" .=) <$> _aGender,
                  ("suggestedRetailPrice" .=) <$>
                    _aSuggestedRetailPrice,
                  ("itemGroupId" .=) <$> _aItemGroupId,
                  ("targetClientId" .=) <$> _aTargetClientId,
                  ("sizeType" .=) <$> _aSizeType,
                  ("releaseDate" .=) <$> _aReleaseDate,
                  ("title" .=) <$> _aTitle, ("mpn" .=) <$> _aMpn,
                  ("productPageUrl" .=) <$> _aProductPageURL,
                  ("description" .=) <$> _aDescription,
                  ("theme" .=) <$> _aTheme])

-- | A price.
--
-- /See:/ 'price' smart constructor.
data Price = Price'
    { _pAmount :: !(Maybe Text)
    , _pCurrency :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Price' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pAmount'
--
-- * 'pCurrency'
price
    :: Price
price = 
    Price'
    { _pAmount = Nothing
    , _pCurrency = Nothing
    }

-- | The numeric value of the price.
pAmount :: Lens' Price (Maybe Text)
pAmount = lens _pAmount (\ s a -> s{_pAmount = a})

-- | The currency in which the price is denoted.
pCurrency :: Lens' Price (Maybe Text)
pCurrency
  = lens _pCurrency (\ s a -> s{_pCurrency = a})

instance FromJSON Price where
        parseJSON
          = withObject "Price"
              (\ o ->
                 Price' <$> (o .:? "amount") <*> (o .:? "currency"))

instance ToJSON Price where
        toJSON Price'{..}
          = object
              (catMaybes
                 [("amount" .=) <$> _pAmount,
                  ("currency" .=) <$> _pCurrency])

-- | Product data.
--
-- /See:/ 'product' smart constructor.
data Product = Product'
    { _pParent :: !(Maybe Text)
    , _pManuallyDeletedAttributes :: !(Maybe [Text])
    , _pTargetCountry :: !(Maybe Text)
    , _pManuallyProvidedAttributes :: !(Maybe Attributes)
    , _pName :: !(Maybe Text)
    , _pIssues :: !(Maybe [Issue])
    , _pUploadedAttributes :: !(Maybe Attributes)
    , _pContentLanguage :: !(Maybe Text)
    , _pFinalAttributes :: !(Maybe Attributes)
    , _pProductId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Product' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParent'
--
-- * 'pManuallyDeletedAttributes'
--
-- * 'pTargetCountry'
--
-- * 'pManuallyProvidedAttributes'
--
-- * 'pName'
--
-- * 'pIssues'
--
-- * 'pUploadedAttributes'
--
-- * 'pContentLanguage'
--
-- * 'pFinalAttributes'
--
-- * 'pProductId'
product
    :: Product
product = 
    Product'
    { _pParent = Nothing
    , _pManuallyDeletedAttributes = Nothing
    , _pTargetCountry = Nothing
    , _pManuallyProvidedAttributes = Nothing
    , _pName = Nothing
    , _pIssues = Nothing
    , _pUploadedAttributes = Nothing
    , _pContentLanguage = Nothing
    , _pFinalAttributes = Nothing
    , _pProductId = Nothing
    }

-- | Parent ID in the format \`accounts\/{account_id}\`. \`account_id\` - The
-- ID of the Manufacturer Center account. \'OutputOnly
pParent :: Lens' Product (Maybe Text)
pParent = lens _pParent (\ s a -> s{_pParent = a})

-- | Names of the attributes of the product deleted manually via the
-- Manufacturer Center UI. \'OutputOnly
pManuallyDeletedAttributes :: Lens' Product [Text]
pManuallyDeletedAttributes
  = lens _pManuallyDeletedAttributes
      (\ s a -> s{_pManuallyDeletedAttributes = a})
      . _Default
      . _Coerce

-- | The target country of the product as a CLDR territory code (for example,
-- US). \'OutputOnly
pTargetCountry :: Lens' Product (Maybe Text)
pTargetCountry
  = lens _pTargetCountry
      (\ s a -> s{_pTargetCountry = a})

-- | Attributes of the product provided manually via the Manufacturer Center
-- UI. \'OutputOnly
pManuallyProvidedAttributes :: Lens' Product (Maybe Attributes)
pManuallyProvidedAttributes
  = lens _pManuallyProvidedAttributes
      (\ s a -> s{_pManuallyProvidedAttributes = a})

-- | Name in the format \`{target_country}:{content_language}:{product_id}\`.
-- \`target_country\` - The target country of the product as a CLDR
-- territory code (for example, US). \`content_language\` - The content
-- language of the product as a two-letter ISO 639-1 language code (for
-- example, en). \`product_id\` - The ID of the product. For more
-- information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#id.
-- \'OutputOnly
pName :: Lens' Product (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | A server-generated list of issues associated with the product.
-- \'OutputOnly
pIssues :: Lens' Product [Issue]
pIssues
  = lens _pIssues (\ s a -> s{_pIssues = a}) . _Default
      . _Coerce

-- | Attributes of the product uploaded via the Manufacturer Center API or
-- via feeds.
pUploadedAttributes :: Lens' Product (Maybe Attributes)
pUploadedAttributes
  = lens _pUploadedAttributes
      (\ s a -> s{_pUploadedAttributes = a})

-- | The content language of the product as a two-letter ISO 639-1 language
-- code (for example, en). \'OutputOnly
pContentLanguage :: Lens' Product (Maybe Text)
pContentLanguage
  = lens _pContentLanguage
      (\ s a -> s{_pContentLanguage = a})

-- | Final attributes of the product. The final attributes are obtained by
-- overriding the uploaded attributes with the manually provided and
-- deleted attributes. Google systems only process, evaluate, review,
-- and\/or use final attributes. \'OutputOnly
pFinalAttributes :: Lens' Product (Maybe Attributes)
pFinalAttributes
  = lens _pFinalAttributes
      (\ s a -> s{_pFinalAttributes = a})

-- | The ID of the product. For more information, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116#id.
-- \'OutputOnly
pProductId :: Lens' Product (Maybe Text)
pProductId
  = lens _pProductId (\ s a -> s{_pProductId = a})

instance FromJSON Product where
        parseJSON
          = withObject "Product"
              (\ o ->
                 Product' <$>
                   (o .:? "parent") <*>
                     (o .:? "manuallyDeletedAttributes" .!= mempty)
                     <*> (o .:? "targetCountry")
                     <*> (o .:? "manuallyProvidedAttributes")
                     <*> (o .:? "name")
                     <*> (o .:? "issues" .!= mempty)
                     <*> (o .:? "uploadedAttributes")
                     <*> (o .:? "contentLanguage")
                     <*> (o .:? "finalAttributes")
                     <*> (o .:? "productId"))

instance ToJSON Product where
        toJSON Product'{..}
          = object
              (catMaybes
                 [("parent" .=) <$> _pParent,
                  ("manuallyDeletedAttributes" .=) <$>
                    _pManuallyDeletedAttributes,
                  ("targetCountry" .=) <$> _pTargetCountry,
                  ("manuallyProvidedAttributes" .=) <$>
                    _pManuallyProvidedAttributes,
                  ("name" .=) <$> _pName, ("issues" .=) <$> _pIssues,
                  ("uploadedAttributes" .=) <$> _pUploadedAttributes,
                  ("contentLanguage" .=) <$> _pContentLanguage,
                  ("finalAttributes" .=) <$> _pFinalAttributes,
                  ("productId" .=) <$> _pProductId])

-- | Product issue.
--
-- /See:/ 'issue' smart constructor.
data Issue = Issue'
    { _issAttribute :: !(Maybe Text)
    , _issSeverity :: !(Maybe IssueSeverity)
    , _issType :: !(Maybe Text)
    , _issTimestamp :: !(Maybe DateTime')
    , _issDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Issue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issAttribute'
--
-- * 'issSeverity'
--
-- * 'issType'
--
-- * 'issTimestamp'
--
-- * 'issDescription'
issue
    :: Issue
issue = 
    Issue'
    { _issAttribute = Nothing
    , _issSeverity = Nothing
    , _issType = Nothing
    , _issTimestamp = Nothing
    , _issDescription = Nothing
    }

-- | If present, the attribute that triggered the issue. For more information
-- about attributes, see
-- https:\/\/support.google.com\/manufacturers\/answer\/6124116.
issAttribute :: Lens' Issue (Maybe Text)
issAttribute
  = lens _issAttribute (\ s a -> s{_issAttribute = a})

-- | The severity of the issue.
issSeverity :: Lens' Issue (Maybe IssueSeverity)
issSeverity
  = lens _issSeverity (\ s a -> s{_issSeverity = a})

-- | The server-generated type of the issue, for example,
-- “INCORRECT_TEXT_FORMATTING”, “IMAGE_NOT_SERVEABLE”, etc.
issType :: Lens' Issue (Maybe Text)
issType = lens _issType (\ s a -> s{_issType = a})

-- | The timestamp when this issue appeared.
issTimestamp :: Lens' Issue (Maybe UTCTime)
issTimestamp
  = lens _issTimestamp (\ s a -> s{_issTimestamp = a})
      . mapping _DateTime

-- | Longer description of the issue focused on how to resolve it.
issDescription :: Lens' Issue (Maybe Text)
issDescription
  = lens _issDescription
      (\ s a -> s{_issDescription = a})

instance FromJSON Issue where
        parseJSON
          = withObject "Issue"
              (\ o ->
                 Issue' <$>
                   (o .:? "attribute") <*> (o .:? "severity") <*>
                     (o .:? "type")
                     <*> (o .:? "timestamp")
                     <*> (o .:? "description"))

instance ToJSON Issue where
        toJSON Issue'{..}
          = object
              (catMaybes
                 [("attribute" .=) <$> _issAttribute,
                  ("severity" .=) <$> _issSeverity,
                  ("type" .=) <$> _issType,
                  ("timestamp" .=) <$> _issTimestamp,
                  ("description" .=) <$> _issDescription])

--
-- /See:/ 'listProductsResponse' smart constructor.
data ListProductsResponse = ListProductsResponse'
    { _lprNextPageToken :: !(Maybe Text)
    , _lprProducts :: !(Maybe [Product])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprNextPageToken'
--
-- * 'lprProducts'
listProductsResponse
    :: ListProductsResponse
listProductsResponse = 
    ListProductsResponse'
    { _lprNextPageToken = Nothing
    , _lprProducts = Nothing
    }

-- | The token for the retrieval of the next page of product statuses.
lprNextPageToken :: Lens' ListProductsResponse (Maybe Text)
lprNextPageToken
  = lens _lprNextPageToken
      (\ s a -> s{_lprNextPageToken = a})

-- | List of the products.
lprProducts :: Lens' ListProductsResponse [Product]
lprProducts
  = lens _lprProducts (\ s a -> s{_lprProducts = a}) .
      _Default
      . _Coerce

instance FromJSON ListProductsResponse where
        parseJSON
          = withObject "ListProductsResponse"
              (\ o ->
                 ListProductsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "products" .!= mempty))

instance ToJSON ListProductsResponse where
        toJSON ListProductsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lprNextPageToken,
                  ("products" .=) <$> _lprProducts])
