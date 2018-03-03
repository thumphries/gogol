{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Vision.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Vision.Types.Product where

import Network.Google.Prelude
import Network.Google.Vision.Types.Sum

-- | An object representing a latitude\/longitude pair. This is expressed as
-- a pair of doubles representing degrees latitude and degrees longitude.
-- Unless specified otherwise, this must conform to the
-- <http://www.unoosa.org/pdf/icg/2012/template/WGS_84.pdf WGS84 standard>.
-- Values must be within normalized ranges.
--
-- /See:/ 'latLng' smart constructor.
data LatLng = LatLng'
    { _llLatitude :: !(Maybe (Textual Double))
    , _llLongitude :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LatLng' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llLatitude'
--
-- * 'llLongitude'
latLng
    :: LatLng
latLng = 
    LatLng'
    { _llLatitude = Nothing
    , _llLongitude = Nothing
    }

-- | The latitude in degrees. It must be in the range [-90.0, +90.0].
llLatitude :: Lens' LatLng (Maybe Double)
llLatitude
  = lens _llLatitude (\ s a -> s{_llLatitude = a}) .
      mapping _Coerce

-- | The longitude in degrees. It must be in the range [-180.0, +180.0].
llLongitude :: Lens' LatLng (Maybe Double)
llLongitude
  = lens _llLongitude (\ s a -> s{_llLongitude = a}) .
      mapping _Coerce

instance FromJSON LatLng where
        parseJSON
          = withObject "LatLng"
              (\ o ->
                 LatLng' <$>
                   (o .:? "latitude") <*> (o .:? "longitude"))

instance ToJSON LatLng where
        toJSON LatLng'{..}
          = object
              (catMaybes
                 [("latitude" .=) <$> _llLatitude,
                  ("longitude" .=) <$> _llLongitude])

-- | Response to an async batch file annotation request.
--
-- /See:/ 'googleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse' smart constructor.
newtype GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse = GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse'
    { _gcvvabafrResponses :: Maybe [GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvvabafrResponses'
googleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse
    :: GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse
googleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse = 
    GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse'
    { _gcvvabafrResponses = Nothing
    }

-- | The list of file annotation responses, one for each request in
-- AsyncBatchAnnotateFilesRequest.
gcvvabafrResponses :: Lens' GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse [GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse]
gcvvabafrResponses
  = lens _gcvvabafrResponses
      (\ s a -> s{_gcvvabafrResponses = a})
      . _Default
      . _Coerce

instance FromJSON
         GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse
         where
        parseJSON
          = withObject
              "GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse"
              (\ o ->
                 GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse'
                   <$> (o .:? "responses" .!= mempty))

instance ToJSON
         GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse
         where
        toJSON
          GoogleCloudVisionV1p2beta1AsyncBatchAnnotateFilesResponse'{..}
          = object
              (catMaybes
                 [("responses" .=) <$> _gcvvabafrResponses])

-- | The type of Google Cloud Vision API detection to perform, and the
-- maximum number of results to return for that type. Multiple \`Feature\`
-- objects can be specified in the \`features\` list.
--
-- /See:/ 'feature' smart constructor.
data Feature = Feature'
    { _fModel :: !(Maybe Text)
    , _fType :: !(Maybe FeatureType)
    , _fMaxResults :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Feature' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fModel'
--
-- * 'fType'
--
-- * 'fMaxResults'
feature
    :: Feature
feature = 
    Feature'
    { _fModel = Nothing
    , _fType = Nothing
    , _fMaxResults = Nothing
    }

-- | Model to use for the feature. Supported values: \"builtin\/stable\" (the
-- default if unset) and \"builtin\/latest\".
fModel :: Lens' Feature (Maybe Text)
fModel = lens _fModel (\ s a -> s{_fModel = a})

-- | The feature type.
fType :: Lens' Feature (Maybe FeatureType)
fType = lens _fType (\ s a -> s{_fType = a})

-- | Maximum number of results of this type. Does not apply to
-- \`TEXT_DETECTION\`, \`DOCUMENT_TEXT_DETECTION\`, or \`CROP_HINTS\`.
fMaxResults :: Lens' Feature (Maybe Int32)
fMaxResults
  = lens _fMaxResults (\ s a -> s{_fMaxResults = a}) .
      mapping _Coerce

instance FromJSON Feature where
        parseJSON
          = withObject "Feature"
              (\ o ->
                 Feature' <$>
                   (o .:? "model") <*> (o .:? "type") <*>
                     (o .:? "maxResults"))

instance ToJSON Feature where
        toJSON Feature'{..}
          = object
              (catMaybes
                 [("model" .=) <$> _fModel, ("type" .=) <$> _fType,
                  ("maxResults" .=) <$> _fMaxResults])

-- | The \`Status\` type defines a logical error model that is suitable for
-- different programming environments, including REST APIs and RPC APIs. It
-- is used by [gRPC](https:\/\/github.com\/grpc). The error model is
-- designed to be: - Simple to use and understand for most users - Flexible
-- enough to meet unexpected needs # Overview The \`Status\` message
-- contains three pieces of data: error code, error message, and error
-- details. The error code should be an enum value of google.rpc.Code, but
-- it may accept additional error codes if needed. The error message should
-- be a developer-facing English message that helps developers *understand*
-- and *resolve* the error. If a localized user-facing error message is
-- needed, put the localized message in the error details or localize it in
-- the client. The optional error details may contain arbitrary information
-- about the error. There is a predefined set of error detail types in the
-- package \`google.rpc\` that can be used for common error conditions. #
-- Language mapping The \`Status\` message is the logical representation of
-- the error model, but it is not necessarily the actual wire format. When
-- the \`Status\` message is exposed in different client libraries and
-- different wire protocols, it can be mapped differently. For example, it
-- will likely be mapped to some exceptions in Java, but more likely mapped
-- to some error codes in C. # Other uses The error model and the
-- \`Status\` message can be used in a variety of environments, either with
-- or without APIs, to provide a consistent developer experience across
-- different environments. Example uses of this error model include: -
-- Partial errors. If a service needs to return partial errors to the
-- client, it may embed the \`Status\` in the normal response to indicate
-- the partial errors. - Workflow errors. A typical workflow has multiple
-- steps. Each step may have a \`Status\` message for error reporting. -
-- Batch operations. If a client uses batch request and batch response, the
-- \`Status\` message should be used directly inside batch response, one
-- for each error sub-response. - Asynchronous operations. If an API call
-- embeds asynchronous operation results in its response, the status of
-- those operations should be represented directly using the \`Status\`
-- message. - Logging. If some API errors are stored in logs, the message
-- \`Status\` could be used directly after any stripping needed for
-- security\/privacy reasons.
--
-- /See:/ 'status' smart constructor.
data Status = Status'
    { _sDetails :: !(Maybe [StatusDetailsItem])
    , _sCode :: !(Maybe (Textual Int32))
    , _sMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Status' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetails'
--
-- * 'sCode'
--
-- * 'sMessage'
status
    :: Status
status = 
    Status'
    { _sDetails = Nothing
    , _sCode = Nothing
    , _sMessage = Nothing
    }

-- | A list of messages that carry the error details. There is a common set
-- of message types for APIs to use.
sDetails :: Lens' Status [StatusDetailsItem]
sDetails
  = lens _sDetails (\ s a -> s{_sDetails = a}) .
      _Default
      . _Coerce

-- | The status code, which should be an enum value of google.rpc.Code.
sCode :: Lens' Status (Maybe Int32)
sCode
  = lens _sCode (\ s a -> s{_sCode = a}) .
      mapping _Coerce

-- | A developer-facing error message, which should be in English. Any
-- user-facing error message should be localized and sent in the
-- google.rpc.Status.details field, or localized by the client.
sMessage :: Lens' Status (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a})

instance FromJSON Status where
        parseJSON
          = withObject "Status"
              (\ o ->
                 Status' <$>
                   (o .:? "details" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON Status where
        toJSON Status'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _sDetails,
                  ("code" .=) <$> _sCode,
                  ("message" .=) <$> _sMessage])

-- | The desired output location and metadata.
--
-- /See:/ 'googleCloudVisionV1p2beta1OutputConfig' smart constructor.
data GoogleCloudVisionV1p2beta1OutputConfig = GoogleCloudVisionV1p2beta1OutputConfig'
    { _gcvvocGcsDestination :: !(Maybe GoogleCloudVisionV1p2beta1GcsDestination)
    , _gcvvocBatchSize :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleCloudVisionV1p2beta1OutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvvocGcsDestination'
--
-- * 'gcvvocBatchSize'
googleCloudVisionV1p2beta1OutputConfig
    :: GoogleCloudVisionV1p2beta1OutputConfig
googleCloudVisionV1p2beta1OutputConfig = 
    GoogleCloudVisionV1p2beta1OutputConfig'
    { _gcvvocGcsDestination = Nothing
    , _gcvvocBatchSize = Nothing
    }

-- | The Google Cloud Storage location to write the output(s) to.
gcvvocGcsDestination :: Lens' GoogleCloudVisionV1p2beta1OutputConfig (Maybe GoogleCloudVisionV1p2beta1GcsDestination)
gcvvocGcsDestination
  = lens _gcvvocGcsDestination
      (\ s a -> s{_gcvvocGcsDestination = a})

-- | The max number of response protos to put into each output JSON file on
-- GCS. The valid range is [1, 100]. If not specified, the default value is
-- 20. For example, for one pdf file with 100 pages, 100 response protos
-- will be generated. If \`batch_size\` = 20, then 5 json files each
-- containing 20 response protos will be written under the prefix
-- \`gcs_destination\`.\`uri\`. Currently, batch_size only applies to
-- GcsDestination, with potential future support for other output
-- configurations.
gcvvocBatchSize :: Lens' GoogleCloudVisionV1p2beta1OutputConfig (Maybe Int32)
gcvvocBatchSize
  = lens _gcvvocBatchSize
      (\ s a -> s{_gcvvocBatchSize = a})
      . mapping _Coerce

instance FromJSON
         GoogleCloudVisionV1p2beta1OutputConfig where
        parseJSON
          = withObject "GoogleCloudVisionV1p2beta1OutputConfig"
              (\ o ->
                 GoogleCloudVisionV1p2beta1OutputConfig' <$>
                   (o .:? "gcsDestination") <*> (o .:? "batchSize"))

instance ToJSON
         GoogleCloudVisionV1p2beta1OutputConfig where
        toJSON GoogleCloudVisionV1p2beta1OutputConfig'{..}
          = object
              (catMaybes
                 [("gcsDestination" .=) <$> _gcvvocGcsDestination,
                  ("batchSize" .=) <$> _gcvvocBatchSize])

-- | A \`Property\` consists of a user-supplied name\/value pair.
--
-- /See:/ 'property' smart constructor.
data Property = Property'
    { _pUint64Value :: !(Maybe (Textual Word64))
    , _pValue :: !(Maybe Text)
    , _pName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Property' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pUint64Value'
--
-- * 'pValue'
--
-- * 'pName'
property
    :: Property
property = 
    Property'
    { _pUint64Value = Nothing
    , _pValue = Nothing
    , _pName = Nothing
    }

-- | Value of numeric properties.
pUint64Value :: Lens' Property (Maybe Word64)
pUint64Value
  = lens _pUint64Value (\ s a -> s{_pUint64Value = a})
      . mapping _Coerce

-- | Value of the property.
pValue :: Lens' Property (Maybe Text)
pValue = lens _pValue (\ s a -> s{_pValue = a})

-- | Name of the property.
pName :: Lens' Property (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

instance FromJSON Property where
        parseJSON
          = withObject "Property"
              (\ o ->
                 Property' <$>
                   (o .:? "uint64Value") <*> (o .:? "value") <*>
                     (o .:? "name"))

instance ToJSON Property where
        toJSON Property'{..}
          = object
              (catMaybes
                 [("uint64Value" .=) <$> _pUint64Value,
                  ("value" .=) <$> _pValue, ("name" .=) <$> _pName])

-- | Client image to perform Google Cloud Vision API tasks over.
--
-- /See:/ 'image' smart constructor.
data Image = Image'
    { _iContent :: !(Maybe Bytes)
    , _iSource :: !(Maybe ImageSource)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iContent'
--
-- * 'iSource'
image
    :: Image
image = 
    Image'
    { _iContent = Nothing
    , _iSource = Nothing
    }

-- | Image content, represented as a stream of bytes. Note: As with all
-- \`bytes\` fields, protobuffers use a pure binary representation, whereas
-- JSON representations use base64.
iContent :: Lens' Image (Maybe ByteString)
iContent
  = lens _iContent (\ s a -> s{_iContent = a}) .
      mapping _Bytes

-- | Google Cloud Storage image location, or publicly-accessible image URL.
-- If both \`content\` and \`source\` are provided for an image,
-- \`content\` takes precedence and is used to perform the image annotation
-- request.
iSource :: Lens' Image (Maybe ImageSource)
iSource = lens _iSource (\ s a -> s{_iSource = a})

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ o ->
                 Image' <$> (o .:? "content") <*> (o .:? "source"))

instance ToJSON Image where
        toJSON Image'{..}
          = object
              (catMaybes
                 [("content" .=) <$> _iContent,
                  ("source" .=) <$> _iSource])

-- | The response message for Operations.ListOperations.
--
-- /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
    { _lorNextPageToken :: !(Maybe Text)
    , _lorOperations :: !(Maybe [Operation])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorNextPageToken'
--
-- * 'lorOperations'
listOperationsResponse
    :: ListOperationsResponse
listOperationsResponse = 
    ListOperationsResponse'
    { _lorNextPageToken = Nothing
    , _lorOperations = Nothing
    }

-- | The standard List next-page token.
lorNextPageToken :: Lens' ListOperationsResponse (Maybe Text)
lorNextPageToken
  = lens _lorNextPageToken
      (\ s a -> s{_lorNextPageToken = a})

-- | A list of operations that matches the specified filter in the request.
lorOperations :: Lens' ListOperationsResponse [Operation]
lorOperations
  = lens _lorOperations
      (\ s a -> s{_lorOperations = a})
      . _Default
      . _Coerce

instance FromJSON ListOperationsResponse where
        parseJSON
          = withObject "ListOperationsResponse"
              (\ o ->
                 ListOperationsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "operations" .!= mempty))

instance ToJSON ListOperationsResponse where
        toJSON ListOperationsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lorNextPageToken,
                  ("operations" .=) <$> _lorOperations])

-- | A face-specific landmark (for example, a face feature).
--
-- /See:/ 'landmark' smart constructor.
data Landmark = Landmark'
    { _lType :: !(Maybe LandmarkType)
    , _lPosition :: !(Maybe Position)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Landmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lType'
--
-- * 'lPosition'
landmark
    :: Landmark
landmark = 
    Landmark'
    { _lType = Nothing
    , _lPosition = Nothing
    }

-- | Face landmark type.
lType :: Lens' Landmark (Maybe LandmarkType)
lType = lens _lType (\ s a -> s{_lType = a})

-- | Face landmark position.
lPosition :: Lens' Landmark (Maybe Position)
lPosition
  = lens _lPosition (\ s a -> s{_lPosition = a})

instance FromJSON Landmark where
        parseJSON
          = withObject "Landmark"
              (\ o ->
                 Landmark' <$> (o .:? "type") <*> (o .:? "position"))

instance ToJSON Landmark where
        toJSON Landmark'{..}
          = object
              (catMaybes
                 [("type" .=) <$> _lType,
                  ("position" .=) <$> _lPosition])

-- | The request message for Operations.CancelOperation.
--
-- /See:/ 'cancelOperationRequest' smart constructor.
data CancelOperationRequest =
    CancelOperationRequest' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelOperationRequest' with the minimum fields required to make a request.
--
cancelOperationRequest
    :: CancelOperationRequest
cancelOperationRequest = CancelOperationRequest'

instance FromJSON CancelOperationRequest where
        parseJSON
          = withObject "CancelOperationRequest"
              (\ o -> pure CancelOperationRequest')

instance ToJSON CancelOperationRequest where
        toJSON = const emptyObject

-- | Contains metadata for the BatchAnnotateImages operation.
--
-- /See:/ 'googleCloudVisionV1p2beta1OperationMetadata' smart constructor.
data GoogleCloudVisionV1p2beta1OperationMetadata = GoogleCloudVisionV1p2beta1OperationMetadata'
    { _gcvvomState :: !(Maybe GoogleCloudVisionV1p2beta1OperationMetadataState)
    , _gcvvomUpdateTime :: !(Maybe DateTime')
    , _gcvvomCreateTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleCloudVisionV1p2beta1OperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvvomState'
--
-- * 'gcvvomUpdateTime'
--
-- * 'gcvvomCreateTime'
googleCloudVisionV1p2beta1OperationMetadata
    :: GoogleCloudVisionV1p2beta1OperationMetadata
googleCloudVisionV1p2beta1OperationMetadata = 
    GoogleCloudVisionV1p2beta1OperationMetadata'
    { _gcvvomState = Nothing
    , _gcvvomUpdateTime = Nothing
    , _gcvvomCreateTime = Nothing
    }

-- | Current state of the batch operation.
gcvvomState :: Lens' GoogleCloudVisionV1p2beta1OperationMetadata (Maybe GoogleCloudVisionV1p2beta1OperationMetadataState)
gcvvomState
  = lens _gcvvomState (\ s a -> s{_gcvvomState = a})

-- | The time when the operation result was last updated.
gcvvomUpdateTime :: Lens' GoogleCloudVisionV1p2beta1OperationMetadata (Maybe UTCTime)
gcvvomUpdateTime
  = lens _gcvvomUpdateTime
      (\ s a -> s{_gcvvomUpdateTime = a})
      . mapping _DateTime

-- | The time when the batch request was received.
gcvvomCreateTime :: Lens' GoogleCloudVisionV1p2beta1OperationMetadata (Maybe UTCTime)
gcvvomCreateTime
  = lens _gcvvomCreateTime
      (\ s a -> s{_gcvvomCreateTime = a})
      . mapping _DateTime

instance FromJSON
         GoogleCloudVisionV1p2beta1OperationMetadata where
        parseJSON
          = withObject
              "GoogleCloudVisionV1p2beta1OperationMetadata"
              (\ o ->
                 GoogleCloudVisionV1p2beta1OperationMetadata' <$>
                   (o .:? "state") <*> (o .:? "updateTime") <*>
                     (o .:? "createTime"))

instance ToJSON
         GoogleCloudVisionV1p2beta1OperationMetadata where
        toJSON
          GoogleCloudVisionV1p2beta1OperationMetadata'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _gcvvomState,
                  ("updateTime" .=) <$> _gcvvomUpdateTime,
                  ("createTime" .=) <$> _gcvvomCreateTime])

-- | Parameters for crop hints annotation request.
--
-- /See:/ 'cropHintsParams' smart constructor.
newtype CropHintsParams = CropHintsParams'
    { _chpAspectRatios :: Maybe [Textual Double]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CropHintsParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chpAspectRatios'
cropHintsParams
    :: CropHintsParams
cropHintsParams = 
    CropHintsParams'
    { _chpAspectRatios = Nothing
    }

-- | Aspect ratios in floats, representing the ratio of the width to the
-- height of the image. For example, if the desired aspect ratio is 4\/3,
-- the corresponding float value should be 1.33333. If not specified, the
-- best possible crop is returned. The number of provided aspect ratios is
-- limited to a maximum of 16; any aspect ratios provided after the 16th
-- are ignored.
chpAspectRatios :: Lens' CropHintsParams [Double]
chpAspectRatios
  = lens _chpAspectRatios
      (\ s a -> s{_chpAspectRatios = a})
      . _Default
      . _Coerce

instance FromJSON CropHintsParams where
        parseJSON
          = withObject "CropHintsParams"
              (\ o ->
                 CropHintsParams' <$>
                   (o .:? "aspectRatios" .!= mempty))

instance ToJSON CropHintsParams where
        toJSON CropHintsParams'{..}
          = object
              (catMaybes
                 [("aspectRatios" .=) <$> _chpAspectRatios])

-- | Additional information detected on the structural component.
--
-- /See:/ 'textProperty' smart constructor.
data TextProperty = TextProperty'
    { _tpDetectedLanguages :: !(Maybe [DetectedLanguage])
    , _tpDetectedBreak :: !(Maybe DetectedBreak)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TextProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpDetectedLanguages'
--
-- * 'tpDetectedBreak'
textProperty
    :: TextProperty
textProperty = 
    TextProperty'
    { _tpDetectedLanguages = Nothing
    , _tpDetectedBreak = Nothing
    }

-- | A list of detected languages together with confidence.
tpDetectedLanguages :: Lens' TextProperty [DetectedLanguage]
tpDetectedLanguages
  = lens _tpDetectedLanguages
      (\ s a -> s{_tpDetectedLanguages = a})
      . _Default
      . _Coerce

-- | Detected start or end of a text segment.
tpDetectedBreak :: Lens' TextProperty (Maybe DetectedBreak)
tpDetectedBreak
  = lens _tpDetectedBreak
      (\ s a -> s{_tpDetectedBreak = a})

instance FromJSON TextProperty where
        parseJSON
          = withObject "TextProperty"
              (\ o ->
                 TextProperty' <$>
                   (o .:? "detectedLanguages" .!= mempty) <*>
                     (o .:? "detectedBreak"))

instance ToJSON TextProperty where
        toJSON TextProperty'{..}
          = object
              (catMaybes
                 [("detectedLanguages" .=) <$> _tpDetectedLanguages,
                  ("detectedBreak" .=) <$> _tpDetectedBreak])

-- | TextAnnotation contains a structured representation of OCR extracted
-- text. The hierarchy of an OCR extracted text structure is like this:
-- TextAnnotation -> Page -> Block -> Paragraph -> Word -> Symbol Each
-- structural component, starting from Page, may further have their own
-- properties. Properties describe detected languages, breaks etc.. Please
-- refer to the TextAnnotation.TextProperty message definition below for
-- more detail.
--
-- /See:/ 'textAnnotation' smart constructor.
data TextAnnotation = TextAnnotation'
    { _taText :: !(Maybe Text)
    , _taPages :: !(Maybe [Page])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TextAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taText'
--
-- * 'taPages'
textAnnotation
    :: TextAnnotation
textAnnotation = 
    TextAnnotation'
    { _taText = Nothing
    , _taPages = Nothing
    }

-- | UTF-8 text detected on the pages.
taText :: Lens' TextAnnotation (Maybe Text)
taText = lens _taText (\ s a -> s{_taText = a})

-- | List of pages detected by OCR.
taPages :: Lens' TextAnnotation [Page]
taPages
  = lens _taPages (\ s a -> s{_taPages = a}) . _Default
      . _Coerce

instance FromJSON TextAnnotation where
        parseJSON
          = withObject "TextAnnotation"
              (\ o ->
                 TextAnnotation' <$>
                   (o .:? "text") <*> (o .:? "pages" .!= mempty))

instance ToJSON TextAnnotation where
        toJSON TextAnnotation'{..}
          = object
              (catMaybes
                 [("text" .=) <$> _taText, ("pages" .=) <$> _taPages])

-- | This resource represents a long-running operation that is the result of
-- a network API call.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oDone :: !(Maybe Bool)
    , _oError :: !(Maybe Status)
    , _oResponse :: !(Maybe OperationResponse)
    , _oName :: !(Maybe Text)
    , _oMetadata :: !(Maybe OperationMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDone'
--
-- * 'oError'
--
-- * 'oResponse'
--
-- * 'oName'
--
-- * 'oMetadata'
operation
    :: Operation
operation = 
    Operation'
    { _oDone = Nothing
    , _oError = Nothing
    , _oResponse = Nothing
    , _oName = Nothing
    , _oMetadata = Nothing
    }

-- | If the value is \`false\`, it means the operation is still in progress.
-- If \`true\`, the operation is completed, and either \`error\` or
-- \`response\` is available.
oDone :: Lens' Operation (Maybe Bool)
oDone = lens _oDone (\ s a -> s{_oDone = a})

-- | The error result of the operation in case of failure or cancellation.
oError :: Lens' Operation (Maybe Status)
oError = lens _oError (\ s a -> s{_oError = a})

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
oResponse :: Lens' Operation (Maybe OperationResponse)
oResponse
  = lens _oResponse (\ s a -> s{_oResponse = a})

-- | The server-assigned name, which is only unique within the same service
-- that originally returns it. If you use the default HTTP mapping, the
-- \`name\` should have the format of \`operations\/some\/unique\/name\`.
oName :: Lens' Operation (Maybe Text)
oName = lens _oName (\ s a -> s{_oName = a})

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
oMetadata :: Lens' Operation (Maybe OperationMetadata)
oMetadata
  = lens _oMetadata (\ s a -> s{_oMetadata = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "done") <*> (o .:? "error") <*>
                     (o .:? "response")
                     <*> (o .:? "name")
                     <*> (o .:? "metadata"))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("done" .=) <$> _oDone, ("error" .=) <$> _oError,
                  ("response" .=) <$> _oResponse,
                  ("name" .=) <$> _oName,
                  ("metadata" .=) <$> _oMetadata])

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

-- | Represents a color in the RGBA color space. This representation is
-- designed for simplicity of conversion to\/from color representations in
-- various languages over compactness; for example, the fields of this
-- representation can be trivially provided to the constructor of
-- \"java.awt.Color\" in Java; it can also be trivially provided to
-- UIColor\'s \"+colorWithRed:green:blue:alpha\" method in iOS; and, with
-- just a little work, it can be easily formatted into a CSS \"rgba()\"
-- string in JavaScript, as well. Here are some examples: Example (Java):
-- import com.google.type.Color; \/\/ ... public static java.awt.Color
-- fromProto(Color protocolor) { float alpha = protocolor.hasAlpha() ?
-- protocolor.getAlpha().getValue() : 1.0; return new java.awt.Color(
-- protocolor.getRed(), protocolor.getGreen(), protocolor.getBlue(),
-- alpha); } public static Color toProto(java.awt.Color color) { float red
-- = (float) color.getRed(); float green = (float) color.getGreen(); float
-- blue = (float) color.getBlue(); float denominator = 255.0; Color.Builder
-- resultBuilder = Color .newBuilder() .setRed(red \/ denominator)
-- .setGreen(green \/ denominator) .setBlue(blue \/ denominator); int alpha
-- = color.getAlpha(); if (alpha != 255) { result.setAlpha( FloatValue
-- .newBuilder() .setValue(((float) alpha) \/ denominator) .build()); }
-- return resultBuilder.build(); } \/\/ ... Example (iOS \/ Obj-C): \/\/
-- ... static UIColor* fromProto(Color* protocolor) { float red =
-- [protocolor red]; float green = [protocolor green]; float blue =
-- [protocolor blue]; FloatValue* alpha_wrapper = [protocolor alpha]; float
-- alpha = 1.0; if (alpha_wrapper != nil) { alpha = [alpha_wrapper value];
-- } return [UIColor colorWithRed:red green:green blue:blue alpha:alpha]; }
-- static Color* toProto(UIColor* color) { CGFloat red, green, blue, alpha;
-- if (![color getRed:&red green:&green blue:&blue alpha:&alpha]) { return
-- nil; } Color* result = [Color alloc] init]; [result setRed:red]; [result
-- setGreen:green]; [result setBlue:blue]; if (alpha \<= 0.9999) { [result
-- setAlpha:floatWrapperWithValue(alpha)]; } [result autorelease]; return
-- result; } \/\/ ... Example (JavaScript): \/\/ ... var protoToCssColor =
-- function(rgb_color) { var redFrac = rgb_color.red || 0.0; var greenFrac
-- = rgb_color.green || 0.0; var blueFrac = rgb_color.blue || 0.0; var red
-- = Math.floor(redFrac * 255); var green = Math.floor(greenFrac * 255);
-- var blue = Math.floor(blueFrac * 255); if (!(\'alpha\' in rgb_color)) {
-- return rgbToCssColor_(red, green, blue); } var alphaFrac =
-- rgb_color.alpha.value || 0.0; var rgbParams = [red, green,
-- blue].join(\',\'); return [\'rgba(\', rgbParams, \',\', alphaFrac,
-- \')\'].join(\'\'); }; var rgbToCssColor_ = function(red, green, blue) {
-- var rgbNumber = new Number((red \<\< 16) | (green \<\< 8) | blue); var
-- hexString = rgbNumber.toString(16); var missingZeros = 6 -
-- hexString.length; var resultBuilder = [\'#\']; for (var i = 0; i \<
-- missingZeros; i++) { resultBuilder.push(\'0\'); }
-- resultBuilder.push(hexString); return resultBuilder.join(\'\'); }; \/\/
-- ...
--
-- /See:/ 'color' smart constructor.
data Color = Color'
    { _cRed :: !(Maybe (Textual Double))
    , _cAlpha :: !(Maybe (Textual Double))
    , _cGreen :: !(Maybe (Textual Double))
    , _cBlue :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Color' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cRed'
--
-- * 'cAlpha'
--
-- * 'cGreen'
--
-- * 'cBlue'
color
    :: Color
color = 
    Color'
    { _cRed = Nothing
    , _cAlpha = Nothing
    , _cGreen = Nothing
    , _cBlue = Nothing
    }

-- | The amount of red in the color as a value in the interval [0, 1].
cRed :: Lens' Color (Maybe Double)
cRed
  = lens _cRed (\ s a -> s{_cRed = a}) .
      mapping _Coerce

-- | The fraction of this color that should be applied to the pixel. That is,
-- the final pixel color is defined by the equation: pixel color = alpha *
-- (this color) + (1.0 - alpha) * (background color) This means that a
-- value of 1.0 corresponds to a solid color, whereas a value of 0.0
-- corresponds to a completely transparent color. This uses a wrapper
-- message rather than a simple float scalar so that it is possible to
-- distinguish between a default value and the value being unset. If
-- omitted, this color object is to be rendered as a solid color (as if the
-- alpha value had been explicitly given with a value of 1.0).
cAlpha :: Lens' Color (Maybe Double)
cAlpha
  = lens _cAlpha (\ s a -> s{_cAlpha = a}) .
      mapping _Coerce

-- | The amount of green in the color as a value in the interval [0, 1].
cGreen :: Lens' Color (Maybe Double)
cGreen
  = lens _cGreen (\ s a -> s{_cGreen = a}) .
      mapping _Coerce

-- | The amount of blue in the color as a value in the interval [0, 1].
cBlue :: Lens' Color (Maybe Double)
cBlue
  = lens _cBlue (\ s a -> s{_cBlue = a}) .
      mapping _Coerce

instance FromJSON Color where
        parseJSON
          = withObject "Color"
              (\ o ->
                 Color' <$>
                   (o .:? "red") <*> (o .:? "alpha") <*> (o .:? "green")
                     <*> (o .:? "blue"))

instance ToJSON Color where
        toJSON Color'{..}
          = object
              (catMaybes
                 [("red" .=) <$> _cRed, ("alpha" .=) <$> _cAlpha,
                  ("green" .=) <$> _cGreen, ("blue" .=) <$> _cBlue])

-- | A bounding polygon for the detected image annotation.
--
-- /See:/ 'boundingPoly' smart constructor.
newtype BoundingPoly = BoundingPoly'
    { _bpVertices :: Maybe [Vertex]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BoundingPoly' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpVertices'
boundingPoly
    :: BoundingPoly
boundingPoly = 
    BoundingPoly'
    { _bpVertices = Nothing
    }

-- | The bounding polygon vertices.
bpVertices :: Lens' BoundingPoly [Vertex]
bpVertices
  = lens _bpVertices (\ s a -> s{_bpVertices = a}) .
      _Default
      . _Coerce

instance FromJSON BoundingPoly where
        parseJSON
          = withObject "BoundingPoly"
              (\ o ->
                 BoundingPoly' <$> (o .:? "vertices" .!= mempty))

instance ToJSON BoundingPoly where
        toJSON BoundingPoly'{..}
          = object
              (catMaybes [("vertices" .=) <$> _bpVertices])

-- | A vertex represents a 2D point in the image. NOTE: the vertex
-- coordinates are in the same scale as the original image.
--
-- /See:/ 'vertex' smart constructor.
data Vertex = Vertex'
    { _vX :: !(Maybe (Textual Int32))
    , _vY :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Vertex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vX'
--
-- * 'vY'
vertex
    :: Vertex
vertex = 
    Vertex'
    { _vX = Nothing
    , _vY = Nothing
    }

-- | X coordinate.
vX :: Lens' Vertex (Maybe Int32)
vX = lens _vX (\ s a -> s{_vX = a}) . mapping _Coerce

-- | Y coordinate.
vY :: Lens' Vertex (Maybe Int32)
vY = lens _vY (\ s a -> s{_vY = a}) . mapping _Coerce

instance FromJSON Vertex where
        parseJSON
          = withObject "Vertex"
              (\ o -> Vertex' <$> (o .:? "x") <*> (o .:? "y"))

instance ToJSON Vertex where
        toJSON Vertex'{..}
          = object
              (catMaybes [("x" .=) <$> _vX, ("y" .=) <$> _vY])

-- | Entity deduced from similar images on the Internet.
--
-- /See:/ 'webEntity' smart constructor.
data WebEntity = WebEntity'
    { _weScore :: !(Maybe (Textual Double))
    , _weEntityId :: !(Maybe Text)
    , _weDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weScore'
--
-- * 'weEntityId'
--
-- * 'weDescription'
webEntity
    :: WebEntity
webEntity = 
    WebEntity'
    { _weScore = Nothing
    , _weEntityId = Nothing
    , _weDescription = Nothing
    }

-- | Overall relevancy score for the entity. Not normalized and not
-- comparable across different image queries.
weScore :: Lens' WebEntity (Maybe Double)
weScore
  = lens _weScore (\ s a -> s{_weScore = a}) .
      mapping _Coerce

-- | Opaque entity ID.
weEntityId :: Lens' WebEntity (Maybe Text)
weEntityId
  = lens _weEntityId (\ s a -> s{_weEntityId = a})

-- | Canonical description of the entity, in English.
weDescription :: Lens' WebEntity (Maybe Text)
weDescription
  = lens _weDescription
      (\ s a -> s{_weDescription = a})

instance FromJSON WebEntity where
        parseJSON
          = withObject "WebEntity"
              (\ o ->
                 WebEntity' <$>
                   (o .:? "score") <*> (o .:? "entityId") <*>
                     (o .:? "description"))

instance ToJSON WebEntity where
        toJSON WebEntity'{..}
          = object
              (catMaybes
                 [("score" .=) <$> _weScore,
                  ("entityId" .=) <$> _weEntityId,
                  ("description" .=) <$> _weDescription])

-- | Detected entity location information.
--
-- /See:/ 'locationInfo' smart constructor.
newtype LocationInfo = LocationInfo'
    { _liLatLng :: Maybe LatLng
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LocationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liLatLng'
locationInfo
    :: LocationInfo
locationInfo = 
    LocationInfo'
    { _liLatLng = Nothing
    }

-- | lat\/long location coordinates.
liLatLng :: Lens' LocationInfo (Maybe LatLng)
liLatLng = lens _liLatLng (\ s a -> s{_liLatLng = a})

instance FromJSON LocationInfo where
        parseJSON
          = withObject "LocationInfo"
              (\ o -> LocationInfo' <$> (o .:? "latLng"))

instance ToJSON LocationInfo where
        toJSON LocationInfo'{..}
          = object (catMaybes [("latLng" .=) <$> _liLatLng])

-- | Parameters for web detection request.
--
-- /See:/ 'webDetectionParams' smart constructor.
newtype WebDetectionParams = WebDetectionParams'
    { _wdpIncludeGeoResults :: Maybe Bool
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebDetectionParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wdpIncludeGeoResults'
webDetectionParams
    :: WebDetectionParams
webDetectionParams = 
    WebDetectionParams'
    { _wdpIncludeGeoResults = Nothing
    }

-- | Whether to include results derived from the geo information in the
-- image.
wdpIncludeGeoResults :: Lens' WebDetectionParams (Maybe Bool)
wdpIncludeGeoResults
  = lens _wdpIncludeGeoResults
      (\ s a -> s{_wdpIncludeGeoResults = a})

instance FromJSON WebDetectionParams where
        parseJSON
          = withObject "WebDetectionParams"
              (\ o ->
                 WebDetectionParams' <$> (o .:? "includeGeoResults"))

instance ToJSON WebDetectionParams where
        toJSON WebDetectionParams'{..}
          = object
              (catMaybes
                 [("includeGeoResults" .=) <$> _wdpIncludeGeoResults])

--
-- /See:/ 'statusDetailsItem' smart constructor.
newtype StatusDetailsItem = StatusDetailsItem'
    { _sdiAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiAddtional'
statusDetailsItem
    :: HashMap Text JSONValue -- ^ 'sdiAddtional'
    -> StatusDetailsItem
statusDetailsItem pSdiAddtional_ = 
    StatusDetailsItem'
    { _sdiAddtional = _Coerce # pSdiAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
sdiAddtional :: Lens' StatusDetailsItem (HashMap Text JSONValue)
sdiAddtional
  = lens _sdiAddtional (\ s a -> s{_sdiAddtional = a})
      . _Coerce

instance FromJSON StatusDetailsItem where
        parseJSON
          = withObject "StatusDetailsItem"
              (\ o -> StatusDetailsItem' <$> (parseJSONObject o))

instance ToJSON StatusDetailsItem where
        toJSON = toJSON . _sdiAddtional

-- | Multiple image annotation requests are batched into a single service
-- call.
--
-- /See:/ 'batchAnnotateImagesRequest' smart constructor.
newtype BatchAnnotateImagesRequest = BatchAnnotateImagesRequest'
    { _bairRequests :: Maybe [AnnotateImageRequest]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchAnnotateImagesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bairRequests'
batchAnnotateImagesRequest
    :: BatchAnnotateImagesRequest
batchAnnotateImagesRequest = 
    BatchAnnotateImagesRequest'
    { _bairRequests = Nothing
    }

-- | Individual image annotation requests for this batch.
bairRequests :: Lens' BatchAnnotateImagesRequest [AnnotateImageRequest]
bairRequests
  = lens _bairRequests (\ s a -> s{_bairRequests = a})
      . _Default
      . _Coerce

instance FromJSON BatchAnnotateImagesRequest where
        parseJSON
          = withObject "BatchAnnotateImagesRequest"
              (\ o ->
                 BatchAnnotateImagesRequest' <$>
                   (o .:? "requests" .!= mempty))

instance ToJSON BatchAnnotateImagesRequest where
        toJSON BatchAnnotateImagesRequest'{..}
          = object
              (catMaybes [("requests" .=) <$> _bairRequests])

-- | Detected page from OCR.
--
-- /See:/ 'page' smart constructor.
data Page = Page'
    { _pProperty :: !(Maybe TextProperty)
    , _pHeight :: !(Maybe (Textual Int32))
    , _pBlocks :: !(Maybe [Block])
    , _pWidth :: !(Maybe (Textual Int32))
    , _pConfidence :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Page' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pProperty'
--
-- * 'pHeight'
--
-- * 'pBlocks'
--
-- * 'pWidth'
--
-- * 'pConfidence'
page
    :: Page
page = 
    Page'
    { _pProperty = Nothing
    , _pHeight = Nothing
    , _pBlocks = Nothing
    , _pWidth = Nothing
    , _pConfidence = Nothing
    }

-- | Additional information detected on the page.
pProperty :: Lens' Page (Maybe TextProperty)
pProperty
  = lens _pProperty (\ s a -> s{_pProperty = a})

-- | Page height. For PDFs the unit is points. For images (including TIFFs)
-- the unit is pixels.
pHeight :: Lens' Page (Maybe Int32)
pHeight
  = lens _pHeight (\ s a -> s{_pHeight = a}) .
      mapping _Coerce

-- | List of blocks of text, images etc on this page.
pBlocks :: Lens' Page [Block]
pBlocks
  = lens _pBlocks (\ s a -> s{_pBlocks = a}) . _Default
      . _Coerce

-- | Page width. For PDFs the unit is points. For images (including TIFFs)
-- the unit is pixels.
pWidth :: Lens' Page (Maybe Int32)
pWidth
  = lens _pWidth (\ s a -> s{_pWidth = a}) .
      mapping _Coerce

-- | Confidence of the OCR results on the page. Range [0, 1].
pConfidence :: Lens' Page (Maybe Double)
pConfidence
  = lens _pConfidence (\ s a -> s{_pConfidence = a}) .
      mapping _Coerce

instance FromJSON Page where
        parseJSON
          = withObject "Page"
              (\ o ->
                 Page' <$>
                   (o .:? "property") <*> (o .:? "height") <*>
                     (o .:? "blocks" .!= mempty)
                     <*> (o .:? "width")
                     <*> (o .:? "confidence"))

instance ToJSON Page where
        toJSON Page'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _pProperty,
                  ("height" .=) <$> _pHeight,
                  ("blocks" .=) <$> _pBlocks, ("width" .=) <$> _pWidth,
                  ("confidence" .=) <$> _pConfidence])

-- | Color information consists of RGB channels, score, and the fraction of
-- the image that the color occupies in the image.
--
-- /See:/ 'colorInfo' smart constructor.
data ColorInfo = ColorInfo'
    { _ciColor :: !(Maybe Color)
    , _ciScore :: !(Maybe (Textual Double))
    , _ciPixelFraction :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ColorInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciColor'
--
-- * 'ciScore'
--
-- * 'ciPixelFraction'
colorInfo
    :: ColorInfo
colorInfo = 
    ColorInfo'
    { _ciColor = Nothing
    , _ciScore = Nothing
    , _ciPixelFraction = Nothing
    }

-- | RGB components of the color.
ciColor :: Lens' ColorInfo (Maybe Color)
ciColor = lens _ciColor (\ s a -> s{_ciColor = a})

-- | Image-specific score for this color. Value in range [0, 1].
ciScore :: Lens' ColorInfo (Maybe Double)
ciScore
  = lens _ciScore (\ s a -> s{_ciScore = a}) .
      mapping _Coerce

-- | The fraction of pixels the color occupies in the image. Value in range
-- [0, 1].
ciPixelFraction :: Lens' ColorInfo (Maybe Double)
ciPixelFraction
  = lens _ciPixelFraction
      (\ s a -> s{_ciPixelFraction = a})
      . mapping _Coerce

instance FromJSON ColorInfo where
        parseJSON
          = withObject "ColorInfo"
              (\ o ->
                 ColorInfo' <$>
                   (o .:? "color") <*> (o .:? "score") <*>
                     (o .:? "pixelFraction"))

instance ToJSON ColorInfo where
        toJSON ColorInfo'{..}
          = object
              (catMaybes
                 [("color" .=) <$> _ciColor,
                  ("score" .=) <$> _ciScore,
                  ("pixelFraction" .=) <$> _ciPixelFraction])

-- | Label to provide extra metadata for the web detection.
--
-- /See:/ 'webLabel' smart constructor.
data WebLabel = WebLabel'
    { _wlLanguageCode :: !(Maybe Text)
    , _wlLabel :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wlLanguageCode'
--
-- * 'wlLabel'
webLabel
    :: WebLabel
webLabel = 
    WebLabel'
    { _wlLanguageCode = Nothing
    , _wlLabel = Nothing
    }

-- | The BCP-47 language code for \`label\`, such as \"en-US\" or
-- \"sr-Latn\". For more information, see
-- http:\/\/www.unicode.org\/reports\/tr35\/#Unicode_locale_identifier.
wlLanguageCode :: Lens' WebLabel (Maybe Text)
wlLanguageCode
  = lens _wlLanguageCode
      (\ s a -> s{_wlLanguageCode = a})

-- | Label for extra metadata.
wlLabel :: Lens' WebLabel (Maybe Text)
wlLabel = lens _wlLabel (\ s a -> s{_wlLabel = a})

instance FromJSON WebLabel where
        parseJSON
          = withObject "WebLabel"
              (\ o ->
                 WebLabel' <$>
                   (o .:? "languageCode") <*> (o .:? "label"))

instance ToJSON WebLabel where
        toJSON WebLabel'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _wlLanguageCode,
                  ("label" .=) <$> _wlLabel])

-- | Structural unit of text representing a number of words in certain order.
--
-- /See:/ 'paragraph' smart constructor.
data Paragraph = Paragraph'
    { _parProperty :: !(Maybe TextProperty)
    , _parBoundingBox :: !(Maybe BoundingPoly)
    , _parConfidence :: !(Maybe (Textual Double))
    , _parWords :: !(Maybe [Word])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Paragraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parProperty'
--
-- * 'parBoundingBox'
--
-- * 'parConfidence'
--
-- * 'parWords'
paragraph
    :: Paragraph
paragraph = 
    Paragraph'
    { _parProperty = Nothing
    , _parBoundingBox = Nothing
    , _parConfidence = Nothing
    , _parWords = Nothing
    }

-- | Additional information detected for the paragraph.
parProperty :: Lens' Paragraph (Maybe TextProperty)
parProperty
  = lens _parProperty (\ s a -> s{_parProperty = a})

-- | The bounding box for the paragraph. The vertices are in the order of
-- top-left, top-right, bottom-right, bottom-left. When a rotation of the
-- bounding box is detected the rotation is represented as around the
-- top-left corner as defined when the text is read in the \'natural\'
-- orientation. For example: * when the text is horizontal it might look
-- like: 0----1 | | 3----2 * when it\'s rotated 180 degrees around the
-- top-left corner it becomes: 2----3 | | 1----0 and the vertice order will
-- still be (0, 1, 2, 3).
parBoundingBox :: Lens' Paragraph (Maybe BoundingPoly)
parBoundingBox
  = lens _parBoundingBox
      (\ s a -> s{_parBoundingBox = a})

-- | Confidence of the OCR results for the paragraph. Range [0, 1].
parConfidence :: Lens' Paragraph (Maybe Double)
parConfidence
  = lens _parConfidence
      (\ s a -> s{_parConfidence = a})
      . mapping _Coerce

-- | List of words in this paragraph.
parWords :: Lens' Paragraph [Word]
parWords
  = lens _parWords (\ s a -> s{_parWords = a}) .
      _Default
      . _Coerce

instance FromJSON Paragraph where
        parseJSON
          = withObject "Paragraph"
              (\ o ->
                 Paragraph' <$>
                   (o .:? "property") <*> (o .:? "boundingBox") <*>
                     (o .:? "confidence")
                     <*> (o .:? "words" .!= mempty))

instance ToJSON Paragraph where
        toJSON Paragraph'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _parProperty,
                  ("boundingBox" .=) <$> _parBoundingBox,
                  ("confidence" .=) <$> _parConfidence,
                  ("words" .=) <$> _parWords])

-- | A single symbol representation.
--
-- /See:/ 'symbol' smart constructor.
data Symbol = Symbol'
    { _sProperty :: !(Maybe TextProperty)
    , _sBoundingBox :: !(Maybe BoundingPoly)
    , _sText :: !(Maybe Text)
    , _sConfidence :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Symbol' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProperty'
--
-- * 'sBoundingBox'
--
-- * 'sText'
--
-- * 'sConfidence'
symbol
    :: Symbol
symbol = 
    Symbol'
    { _sProperty = Nothing
    , _sBoundingBox = Nothing
    , _sText = Nothing
    , _sConfidence = Nothing
    }

-- | Additional information detected for the symbol.
sProperty :: Lens' Symbol (Maybe TextProperty)
sProperty
  = lens _sProperty (\ s a -> s{_sProperty = a})

-- | The bounding box for the symbol. The vertices are in the order of
-- top-left, top-right, bottom-right, bottom-left. When a rotation of the
-- bounding box is detected the rotation is represented as around the
-- top-left corner as defined when the text is read in the \'natural\'
-- orientation. For example: * when the text is horizontal it might look
-- like: 0----1 | | 3----2 * when it\'s rotated 180 degrees around the
-- top-left corner it becomes: 2----3 | | 1----0 and the vertice order will
-- still be (0, 1, 2, 3).
sBoundingBox :: Lens' Symbol (Maybe BoundingPoly)
sBoundingBox
  = lens _sBoundingBox (\ s a -> s{_sBoundingBox = a})

-- | The actual UTF-8 representation of the symbol.
sText :: Lens' Symbol (Maybe Text)
sText = lens _sText (\ s a -> s{_sText = a})

-- | Confidence of the OCR results for the symbol. Range [0, 1].
sConfidence :: Lens' Symbol (Maybe Double)
sConfidence
  = lens _sConfidence (\ s a -> s{_sConfidence = a}) .
      mapping _Coerce

instance FromJSON Symbol where
        parseJSON
          = withObject "Symbol"
              (\ o ->
                 Symbol' <$>
                   (o .:? "property") <*> (o .:? "boundingBox") <*>
                     (o .:? "text")
                     <*> (o .:? "confidence"))

instance ToJSON Symbol where
        toJSON Symbol'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _sProperty,
                  ("boundingBox" .=) <$> _sBoundingBox,
                  ("text" .=) <$> _sText,
                  ("confidence" .=) <$> _sConfidence])

-- | Response to an image annotation request.
--
-- /See:/ 'annotateImageResponse' smart constructor.
data AnnotateImageResponse = AnnotateImageResponse'
    { _airLogoAnnotations :: !(Maybe [EntityAnnotation])
    , _airLabelAnnotations :: !(Maybe [EntityAnnotation])
    , _airFaceAnnotations :: !(Maybe [FaceAnnotation])
    , _airError :: !(Maybe Status)
    , _airWebDetection :: !(Maybe WebDetection)
    , _airSafeSearchAnnotation :: !(Maybe SafeSearchAnnotation)
    , _airLandmarkAnnotations :: !(Maybe [EntityAnnotation])
    , _airTextAnnotations :: !(Maybe [EntityAnnotation])
    , _airCropHintsAnnotation :: !(Maybe CropHintsAnnotation)
    , _airFullTextAnnotation :: !(Maybe TextAnnotation)
    , _airImagePropertiesAnnotation :: !(Maybe ImageProperties)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AnnotateImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airLogoAnnotations'
--
-- * 'airLabelAnnotations'
--
-- * 'airFaceAnnotations'
--
-- * 'airError'
--
-- * 'airWebDetection'
--
-- * 'airSafeSearchAnnotation'
--
-- * 'airLandmarkAnnotations'
--
-- * 'airTextAnnotations'
--
-- * 'airCropHintsAnnotation'
--
-- * 'airFullTextAnnotation'
--
-- * 'airImagePropertiesAnnotation'
annotateImageResponse
    :: AnnotateImageResponse
annotateImageResponse = 
    AnnotateImageResponse'
    { _airLogoAnnotations = Nothing
    , _airLabelAnnotations = Nothing
    , _airFaceAnnotations = Nothing
    , _airError = Nothing
    , _airWebDetection = Nothing
    , _airSafeSearchAnnotation = Nothing
    , _airLandmarkAnnotations = Nothing
    , _airTextAnnotations = Nothing
    , _airCropHintsAnnotation = Nothing
    , _airFullTextAnnotation = Nothing
    , _airImagePropertiesAnnotation = Nothing
    }

-- | If present, logo detection has completed successfully.
airLogoAnnotations :: Lens' AnnotateImageResponse [EntityAnnotation]
airLogoAnnotations
  = lens _airLogoAnnotations
      (\ s a -> s{_airLogoAnnotations = a})
      . _Default
      . _Coerce

-- | If present, label detection has completed successfully.
airLabelAnnotations :: Lens' AnnotateImageResponse [EntityAnnotation]
airLabelAnnotations
  = lens _airLabelAnnotations
      (\ s a -> s{_airLabelAnnotations = a})
      . _Default
      . _Coerce

-- | If present, face detection has completed successfully.
airFaceAnnotations :: Lens' AnnotateImageResponse [FaceAnnotation]
airFaceAnnotations
  = lens _airFaceAnnotations
      (\ s a -> s{_airFaceAnnotations = a})
      . _Default
      . _Coerce

-- | If set, represents the error message for the operation. Note that
-- filled-in image annotations are guaranteed to be correct, even when
-- \`error\` is set.
airError :: Lens' AnnotateImageResponse (Maybe Status)
airError = lens _airError (\ s a -> s{_airError = a})

-- | If present, web detection has completed successfully.
airWebDetection :: Lens' AnnotateImageResponse (Maybe WebDetection)
airWebDetection
  = lens _airWebDetection
      (\ s a -> s{_airWebDetection = a})

-- | If present, safe-search annotation has completed successfully.
airSafeSearchAnnotation :: Lens' AnnotateImageResponse (Maybe SafeSearchAnnotation)
airSafeSearchAnnotation
  = lens _airSafeSearchAnnotation
      (\ s a -> s{_airSafeSearchAnnotation = a})

-- | If present, landmark detection has completed successfully.
airLandmarkAnnotations :: Lens' AnnotateImageResponse [EntityAnnotation]
airLandmarkAnnotations
  = lens _airLandmarkAnnotations
      (\ s a -> s{_airLandmarkAnnotations = a})
      . _Default
      . _Coerce

-- | If present, text (OCR) detection has completed successfully.
airTextAnnotations :: Lens' AnnotateImageResponse [EntityAnnotation]
airTextAnnotations
  = lens _airTextAnnotations
      (\ s a -> s{_airTextAnnotations = a})
      . _Default
      . _Coerce

-- | If present, crop hints have completed successfully.
airCropHintsAnnotation :: Lens' AnnotateImageResponse (Maybe CropHintsAnnotation)
airCropHintsAnnotation
  = lens _airCropHintsAnnotation
      (\ s a -> s{_airCropHintsAnnotation = a})

-- | If present, text (OCR) detection or document (OCR) text detection has
-- completed successfully. This annotation provides the structural
-- hierarchy for the OCR detected text.
airFullTextAnnotation :: Lens' AnnotateImageResponse (Maybe TextAnnotation)
airFullTextAnnotation
  = lens _airFullTextAnnotation
      (\ s a -> s{_airFullTextAnnotation = a})

-- | If present, image properties were extracted successfully.
airImagePropertiesAnnotation :: Lens' AnnotateImageResponse (Maybe ImageProperties)
airImagePropertiesAnnotation
  = lens _airImagePropertiesAnnotation
      (\ s a -> s{_airImagePropertiesAnnotation = a})

instance FromJSON AnnotateImageResponse where
        parseJSON
          = withObject "AnnotateImageResponse"
              (\ o ->
                 AnnotateImageResponse' <$>
                   (o .:? "logoAnnotations" .!= mempty) <*>
                     (o .:? "labelAnnotations" .!= mempty)
                     <*> (o .:? "faceAnnotations" .!= mempty)
                     <*> (o .:? "error")
                     <*> (o .:? "webDetection")
                     <*> (o .:? "safeSearchAnnotation")
                     <*> (o .:? "landmarkAnnotations" .!= mempty)
                     <*> (o .:? "textAnnotations" .!= mempty)
                     <*> (o .:? "cropHintsAnnotation")
                     <*> (o .:? "fullTextAnnotation")
                     <*> (o .:? "imagePropertiesAnnotation"))

instance ToJSON AnnotateImageResponse where
        toJSON AnnotateImageResponse'{..}
          = object
              (catMaybes
                 [("logoAnnotations" .=) <$> _airLogoAnnotations,
                  ("labelAnnotations" .=) <$> _airLabelAnnotations,
                  ("faceAnnotations" .=) <$> _airFaceAnnotations,
                  ("error" .=) <$> _airError,
                  ("webDetection" .=) <$> _airWebDetection,
                  ("safeSearchAnnotation" .=) <$>
                    _airSafeSearchAnnotation,
                  ("landmarkAnnotations" .=) <$>
                    _airLandmarkAnnotations,
                  ("textAnnotations" .=) <$> _airTextAnnotations,
                  ("cropHintsAnnotation" .=) <$>
                    _airCropHintsAnnotation,
                  ("fullTextAnnotation" .=) <$> _airFullTextAnnotation,
                  ("imagePropertiesAnnotation" .=) <$>
                    _airImagePropertiesAnnotation])

-- | Stores image properties, such as dominant colors.
--
-- /See:/ 'imageProperties' smart constructor.
newtype ImageProperties = ImageProperties'
    { _ipDominantColors :: Maybe DominantColorsAnnotation
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipDominantColors'
imageProperties
    :: ImageProperties
imageProperties = 
    ImageProperties'
    { _ipDominantColors = Nothing
    }

-- | If present, dominant colors completed successfully.
ipDominantColors :: Lens' ImageProperties (Maybe DominantColorsAnnotation)
ipDominantColors
  = lens _ipDominantColors
      (\ s a -> s{_ipDominantColors = a})

instance FromJSON ImageProperties where
        parseJSON
          = withObject "ImageProperties"
              (\ o ->
                 ImageProperties' <$> (o .:? "dominantColors"))

instance ToJSON ImageProperties where
        toJSON ImageProperties'{..}
          = object
              (catMaybes
                 [("dominantColors" .=) <$> _ipDominantColors])

-- | A face annotation object contains the results of face detection.
--
-- /See:/ 'faceAnnotation' smart constructor.
data FaceAnnotation = FaceAnnotation'
    { _faTiltAngle :: !(Maybe (Textual Double))
    , _faBlurredLikelihood :: !(Maybe FaceAnnotationBlurredLikelihood)
    , _faBoundingPoly :: !(Maybe BoundingPoly)
    , _faSurpriseLikelihood :: !(Maybe FaceAnnotationSurpriseLikelihood)
    , _faLandmarkingConfidence :: !(Maybe (Textual Double))
    , _faPanAngle :: !(Maybe (Textual Double))
    , _faRollAngle :: !(Maybe (Textual Double))
    , _faUnderExposedLikelihood :: !(Maybe FaceAnnotationUnderExposedLikelihood)
    , _faFdBoundingPoly :: !(Maybe BoundingPoly)
    , _faAngerLikelihood :: !(Maybe FaceAnnotationAngerLikelihood)
    , _faDetectionConfidence :: !(Maybe (Textual Double))
    , _faHeadwearLikelihood :: !(Maybe FaceAnnotationHeadwearLikelihood)
    , _faSorrowLikelihood :: !(Maybe FaceAnnotationSorrowLikelihood)
    , _faJoyLikelihood :: !(Maybe FaceAnnotationJoyLikelihood)
    , _faLandmarks :: !(Maybe [Landmark])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FaceAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faTiltAngle'
--
-- * 'faBlurredLikelihood'
--
-- * 'faBoundingPoly'
--
-- * 'faSurpriseLikelihood'
--
-- * 'faLandmarkingConfidence'
--
-- * 'faPanAngle'
--
-- * 'faRollAngle'
--
-- * 'faUnderExposedLikelihood'
--
-- * 'faFdBoundingPoly'
--
-- * 'faAngerLikelihood'
--
-- * 'faDetectionConfidence'
--
-- * 'faHeadwearLikelihood'
--
-- * 'faSorrowLikelihood'
--
-- * 'faJoyLikelihood'
--
-- * 'faLandmarks'
faceAnnotation
    :: FaceAnnotation
faceAnnotation = 
    FaceAnnotation'
    { _faTiltAngle = Nothing
    , _faBlurredLikelihood = Nothing
    , _faBoundingPoly = Nothing
    , _faSurpriseLikelihood = Nothing
    , _faLandmarkingConfidence = Nothing
    , _faPanAngle = Nothing
    , _faRollAngle = Nothing
    , _faUnderExposedLikelihood = Nothing
    , _faFdBoundingPoly = Nothing
    , _faAngerLikelihood = Nothing
    , _faDetectionConfidence = Nothing
    , _faHeadwearLikelihood = Nothing
    , _faSorrowLikelihood = Nothing
    , _faJoyLikelihood = Nothing
    , _faLandmarks = Nothing
    }

-- | Pitch angle, which indicates the upwards\/downwards angle that the face
-- is pointing relative to the image\'s horizontal plane. Range [-180,180].
faTiltAngle :: Lens' FaceAnnotation (Maybe Double)
faTiltAngle
  = lens _faTiltAngle (\ s a -> s{_faTiltAngle = a}) .
      mapping _Coerce

-- | Blurred likelihood.
faBlurredLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationBlurredLikelihood)
faBlurredLikelihood
  = lens _faBlurredLikelihood
      (\ s a -> s{_faBlurredLikelihood = a})

-- | The bounding polygon around the face. The coordinates of the bounding
-- box are in the original image\'s scale, as returned in \`ImageParams\`.
-- The bounding box is computed to \"frame\" the face in accordance with
-- human expectations. It is based on the landmarker results. Note that one
-- or more x and\/or y coordinates may not be generated in the
-- \`BoundingPoly\` (the polygon will be unbounded) if only a partial face
-- appears in the image to be annotated.
faBoundingPoly :: Lens' FaceAnnotation (Maybe BoundingPoly)
faBoundingPoly
  = lens _faBoundingPoly
      (\ s a -> s{_faBoundingPoly = a})

-- | Surprise likelihood.
faSurpriseLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationSurpriseLikelihood)
faSurpriseLikelihood
  = lens _faSurpriseLikelihood
      (\ s a -> s{_faSurpriseLikelihood = a})

-- | Face landmarking confidence. Range [0, 1].
faLandmarkingConfidence :: Lens' FaceAnnotation (Maybe Double)
faLandmarkingConfidence
  = lens _faLandmarkingConfidence
      (\ s a -> s{_faLandmarkingConfidence = a})
      . mapping _Coerce

-- | Yaw angle, which indicates the leftward\/rightward angle that the face
-- is pointing relative to the vertical plane perpendicular to the image.
-- Range [-180,180].
faPanAngle :: Lens' FaceAnnotation (Maybe Double)
faPanAngle
  = lens _faPanAngle (\ s a -> s{_faPanAngle = a}) .
      mapping _Coerce

-- | Roll angle, which indicates the amount of clockwise\/anti-clockwise
-- rotation of the face relative to the image vertical about the axis
-- perpendicular to the face. Range [-180,180].
faRollAngle :: Lens' FaceAnnotation (Maybe Double)
faRollAngle
  = lens _faRollAngle (\ s a -> s{_faRollAngle = a}) .
      mapping _Coerce

-- | Under-exposed likelihood.
faUnderExposedLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationUnderExposedLikelihood)
faUnderExposedLikelihood
  = lens _faUnderExposedLikelihood
      (\ s a -> s{_faUnderExposedLikelihood = a})

-- | The \`fd_bounding_poly\` bounding polygon is tighter than the
-- \`boundingPoly\`, and encloses only the skin part of the face.
-- Typically, it is used to eliminate the face from any image analysis that
-- detects the \"amount of skin\" visible in an image. It is not based on
-- the landmarker results, only on the initial face detection, hence the
-- 'fd' (face detection) prefix.
faFdBoundingPoly :: Lens' FaceAnnotation (Maybe BoundingPoly)
faFdBoundingPoly
  = lens _faFdBoundingPoly
      (\ s a -> s{_faFdBoundingPoly = a})

-- | Anger likelihood.
faAngerLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationAngerLikelihood)
faAngerLikelihood
  = lens _faAngerLikelihood
      (\ s a -> s{_faAngerLikelihood = a})

-- | Detection confidence. Range [0, 1].
faDetectionConfidence :: Lens' FaceAnnotation (Maybe Double)
faDetectionConfidence
  = lens _faDetectionConfidence
      (\ s a -> s{_faDetectionConfidence = a})
      . mapping _Coerce

-- | Headwear likelihood.
faHeadwearLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationHeadwearLikelihood)
faHeadwearLikelihood
  = lens _faHeadwearLikelihood
      (\ s a -> s{_faHeadwearLikelihood = a})

-- | Sorrow likelihood.
faSorrowLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationSorrowLikelihood)
faSorrowLikelihood
  = lens _faSorrowLikelihood
      (\ s a -> s{_faSorrowLikelihood = a})

-- | Joy likelihood.
faJoyLikelihood :: Lens' FaceAnnotation (Maybe FaceAnnotationJoyLikelihood)
faJoyLikelihood
  = lens _faJoyLikelihood
      (\ s a -> s{_faJoyLikelihood = a})

-- | Detected face landmarks.
faLandmarks :: Lens' FaceAnnotation [Landmark]
faLandmarks
  = lens _faLandmarks (\ s a -> s{_faLandmarks = a}) .
      _Default
      . _Coerce

instance FromJSON FaceAnnotation where
        parseJSON
          = withObject "FaceAnnotation"
              (\ o ->
                 FaceAnnotation' <$>
                   (o .:? "tiltAngle") <*> (o .:? "blurredLikelihood")
                     <*> (o .:? "boundingPoly")
                     <*> (o .:? "surpriseLikelihood")
                     <*> (o .:? "landmarkingConfidence")
                     <*> (o .:? "panAngle")
                     <*> (o .:? "rollAngle")
                     <*> (o .:? "underExposedLikelihood")
                     <*> (o .:? "fdBoundingPoly")
                     <*> (o .:? "angerLikelihood")
                     <*> (o .:? "detectionConfidence")
                     <*> (o .:? "headwearLikelihood")
                     <*> (o .:? "sorrowLikelihood")
                     <*> (o .:? "joyLikelihood")
                     <*> (o .:? "landmarks" .!= mempty))

instance ToJSON FaceAnnotation where
        toJSON FaceAnnotation'{..}
          = object
              (catMaybes
                 [("tiltAngle" .=) <$> _faTiltAngle,
                  ("blurredLikelihood" .=) <$> _faBlurredLikelihood,
                  ("boundingPoly" .=) <$> _faBoundingPoly,
                  ("surpriseLikelihood" .=) <$> _faSurpriseLikelihood,
                  ("landmarkingConfidence" .=) <$>
                    _faLandmarkingConfidence,
                  ("panAngle" .=) <$> _faPanAngle,
                  ("rollAngle" .=) <$> _faRollAngle,
                  ("underExposedLikelihood" .=) <$>
                    _faUnderExposedLikelihood,
                  ("fdBoundingPoly" .=) <$> _faFdBoundingPoly,
                  ("angerLikelihood" .=) <$> _faAngerLikelihood,
                  ("detectionConfidence" .=) <$>
                    _faDetectionConfidence,
                  ("headwearLikelihood" .=) <$> _faHeadwearLikelihood,
                  ("sorrowLikelihood" .=) <$> _faSorrowLikelihood,
                  ("joyLikelihood" .=) <$> _faJoyLikelihood,
                  ("landmarks" .=) <$> _faLandmarks])

-- | Detected start or end of a structural component.
--
-- /See:/ 'detectedBreak' smart constructor.
data DetectedBreak = DetectedBreak'
    { _dbIsPrefix :: !(Maybe Bool)
    , _dbType :: !(Maybe DetectedBreakType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetectedBreak' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbIsPrefix'
--
-- * 'dbType'
detectedBreak
    :: DetectedBreak
detectedBreak = 
    DetectedBreak'
    { _dbIsPrefix = Nothing
    , _dbType = Nothing
    }

-- | True if break prepends the element.
dbIsPrefix :: Lens' DetectedBreak (Maybe Bool)
dbIsPrefix
  = lens _dbIsPrefix (\ s a -> s{_dbIsPrefix = a})

-- | Detected break type.
dbType :: Lens' DetectedBreak (Maybe DetectedBreakType)
dbType = lens _dbType (\ s a -> s{_dbType = a})

instance FromJSON DetectedBreak where
        parseJSON
          = withObject "DetectedBreak"
              (\ o ->
                 DetectedBreak' <$>
                   (o .:? "isPrefix") <*> (o .:? "type"))

instance ToJSON DetectedBreak where
        toJSON DetectedBreak'{..}
          = object
              (catMaybes
                 [("isPrefix" .=) <$> _dbIsPrefix,
                  ("type" .=) <$> _dbType])

-- | Logical element on the page.
--
-- /See:/ 'block' smart constructor.
data Block = Block'
    { _bProperty :: !(Maybe TextProperty)
    , _bBoundingBox :: !(Maybe BoundingPoly)
    , _bParagraphs :: !(Maybe [Paragraph])
    , _bConfidence :: !(Maybe (Textual Double))
    , _bBlockType :: !(Maybe BlockBlockType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Block' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bProperty'
--
-- * 'bBoundingBox'
--
-- * 'bParagraphs'
--
-- * 'bConfidence'
--
-- * 'bBlockType'
block
    :: Block
block = 
    Block'
    { _bProperty = Nothing
    , _bBoundingBox = Nothing
    , _bParagraphs = Nothing
    , _bConfidence = Nothing
    , _bBlockType = Nothing
    }

-- | Additional information detected for the block.
bProperty :: Lens' Block (Maybe TextProperty)
bProperty
  = lens _bProperty (\ s a -> s{_bProperty = a})

-- | The bounding box for the block. The vertices are in the order of
-- top-left, top-right, bottom-right, bottom-left. When a rotation of the
-- bounding box is detected the rotation is represented as around the
-- top-left corner as defined when the text is read in the \'natural\'
-- orientation. For example: * when the text is horizontal it might look
-- like: 0----1 | | 3----2 * when it\'s rotated 180 degrees around the
-- top-left corner it becomes: 2----3 | | 1----0 and the vertice order will
-- still be (0, 1, 2, 3).
bBoundingBox :: Lens' Block (Maybe BoundingPoly)
bBoundingBox
  = lens _bBoundingBox (\ s a -> s{_bBoundingBox = a})

-- | List of paragraphs in this block (if this blocks is of type text).
bParagraphs :: Lens' Block [Paragraph]
bParagraphs
  = lens _bParagraphs (\ s a -> s{_bParagraphs = a}) .
      _Default
      . _Coerce

-- | Confidence of the OCR results on the block. Range [0, 1].
bConfidence :: Lens' Block (Maybe Double)
bConfidence
  = lens _bConfidence (\ s a -> s{_bConfidence = a}) .
      mapping _Coerce

-- | Detected block type (text, image etc) for this block.
bBlockType :: Lens' Block (Maybe BlockBlockType)
bBlockType
  = lens _bBlockType (\ s a -> s{_bBlockType = a})

instance FromJSON Block where
        parseJSON
          = withObject "Block"
              (\ o ->
                 Block' <$>
                   (o .:? "property") <*> (o .:? "boundingBox") <*>
                     (o .:? "paragraphs" .!= mempty)
                     <*> (o .:? "confidence")
                     <*> (o .:? "blockType"))

instance ToJSON Block where
        toJSON Block'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _bProperty,
                  ("boundingBox" .=) <$> _bBoundingBox,
                  ("paragraphs" .=) <$> _bParagraphs,
                  ("confidence" .=) <$> _bConfidence,
                  ("blockType" .=) <$> _bBlockType])

-- | Set of detected entity features.
--
-- /See:/ 'entityAnnotation' smart constructor.
data EntityAnnotation = EntityAnnotation'
    { _eaScore :: !(Maybe (Textual Double))
    , _eaTopicality :: !(Maybe (Textual Double))
    , _eaLocale :: !(Maybe Text)
    , _eaBoundingPoly :: !(Maybe BoundingPoly)
    , _eaConfidence :: !(Maybe (Textual Double))
    , _eaMid :: !(Maybe Text)
    , _eaLocations :: !(Maybe [LocationInfo])
    , _eaDescription :: !(Maybe Text)
    , _eaProperties :: !(Maybe [Property])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EntityAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaScore'
--
-- * 'eaTopicality'
--
-- * 'eaLocale'
--
-- * 'eaBoundingPoly'
--
-- * 'eaConfidence'
--
-- * 'eaMid'
--
-- * 'eaLocations'
--
-- * 'eaDescription'
--
-- * 'eaProperties'
entityAnnotation
    :: EntityAnnotation
entityAnnotation = 
    EntityAnnotation'
    { _eaScore = Nothing
    , _eaTopicality = Nothing
    , _eaLocale = Nothing
    , _eaBoundingPoly = Nothing
    , _eaConfidence = Nothing
    , _eaMid = Nothing
    , _eaLocations = Nothing
    , _eaDescription = Nothing
    , _eaProperties = Nothing
    }

-- | Overall score of the result. Range [0, 1].
eaScore :: Lens' EntityAnnotation (Maybe Double)
eaScore
  = lens _eaScore (\ s a -> s{_eaScore = a}) .
      mapping _Coerce

-- | The relevancy of the ICA (Image Content Annotation) label to the image.
-- For example, the relevancy of \"tower\" is likely higher to an image
-- containing the detected \"Eiffel Tower\" than to an image containing a
-- detected distant towering building, even though the confidence that
-- there is a tower in each image may be the same. Range [0, 1].
eaTopicality :: Lens' EntityAnnotation (Maybe Double)
eaTopicality
  = lens _eaTopicality (\ s a -> s{_eaTopicality = a})
      . mapping _Coerce

-- | The language code for the locale in which the entity textual
-- \`description\` is expressed.
eaLocale :: Lens' EntityAnnotation (Maybe Text)
eaLocale = lens _eaLocale (\ s a -> s{_eaLocale = a})

-- | Image region to which this entity belongs. Not produced for
-- \`LABEL_DETECTION\` features.
eaBoundingPoly :: Lens' EntityAnnotation (Maybe BoundingPoly)
eaBoundingPoly
  = lens _eaBoundingPoly
      (\ s a -> s{_eaBoundingPoly = a})

-- | **Deprecated. Use \`score\` instead.** The accuracy of the entity
-- detection in an image. For example, for an image in which the \"Eiffel
-- Tower\" entity is detected, this field represents the confidence that
-- there is a tower in the query image. Range [0, 1].
eaConfidence :: Lens' EntityAnnotation (Maybe Double)
eaConfidence
  = lens _eaConfidence (\ s a -> s{_eaConfidence = a})
      . mapping _Coerce

-- | Opaque entity ID. Some IDs may be available in [Google Knowledge Graph
-- Search API](https:\/\/developers.google.com\/knowledge-graph\/).
eaMid :: Lens' EntityAnnotation (Maybe Text)
eaMid = lens _eaMid (\ s a -> s{_eaMid = a})

-- | The location information for the detected entity. Multiple
-- \`LocationInfo\` elements can be present because one location may
-- indicate the location of the scene in the image, and another location
-- may indicate the location of the place where the image was taken.
-- Location information is usually present for landmarks.
eaLocations :: Lens' EntityAnnotation [LocationInfo]
eaLocations
  = lens _eaLocations (\ s a -> s{_eaLocations = a}) .
      _Default
      . _Coerce

-- | Entity textual description, expressed in its \`locale\` language.
eaDescription :: Lens' EntityAnnotation (Maybe Text)
eaDescription
  = lens _eaDescription
      (\ s a -> s{_eaDescription = a})

-- | Some entities may have optional user-supplied \`Property\` (name\/value)
-- fields, such a score or string that qualifies the entity.
eaProperties :: Lens' EntityAnnotation [Property]
eaProperties
  = lens _eaProperties (\ s a -> s{_eaProperties = a})
      . _Default
      . _Coerce

instance FromJSON EntityAnnotation where
        parseJSON
          = withObject "EntityAnnotation"
              (\ o ->
                 EntityAnnotation' <$>
                   (o .:? "score") <*> (o .:? "topicality") <*>
                     (o .:? "locale")
                     <*> (o .:? "boundingPoly")
                     <*> (o .:? "confidence")
                     <*> (o .:? "mid")
                     <*> (o .:? "locations" .!= mempty)
                     <*> (o .:? "description")
                     <*> (o .:? "properties" .!= mempty))

instance ToJSON EntityAnnotation where
        toJSON EntityAnnotation'{..}
          = object
              (catMaybes
                 [("score" .=) <$> _eaScore,
                  ("topicality" .=) <$> _eaTopicality,
                  ("locale" .=) <$> _eaLocale,
                  ("boundingPoly" .=) <$> _eaBoundingPoly,
                  ("confidence" .=) <$> _eaConfidence,
                  ("mid" .=) <$> _eaMid,
                  ("locations" .=) <$> _eaLocations,
                  ("description" .=) <$> _eaDescription,
                  ("properties" .=) <$> _eaProperties])

-- | Request for performing Google Cloud Vision API tasks over a
-- user-provided image, with user-requested features.
--
-- /See:/ 'annotateImageRequest' smart constructor.
data AnnotateImageRequest = AnnotateImageRequest'
    { _airImage :: !(Maybe Image)
    , _airFeatures :: !(Maybe [Feature])
    , _airImageContext :: !(Maybe ImageContext)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AnnotateImageRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airImage'
--
-- * 'airFeatures'
--
-- * 'airImageContext'
annotateImageRequest
    :: AnnotateImageRequest
annotateImageRequest = 
    AnnotateImageRequest'
    { _airImage = Nothing
    , _airFeatures = Nothing
    , _airImageContext = Nothing
    }

-- | The image to be processed.
airImage :: Lens' AnnotateImageRequest (Maybe Image)
airImage = lens _airImage (\ s a -> s{_airImage = a})

-- | Requested features.
airFeatures :: Lens' AnnotateImageRequest [Feature]
airFeatures
  = lens _airFeatures (\ s a -> s{_airFeatures = a}) .
      _Default
      . _Coerce

-- | Additional context that may accompany the image.
airImageContext :: Lens' AnnotateImageRequest (Maybe ImageContext)
airImageContext
  = lens _airImageContext
      (\ s a -> s{_airImageContext = a})

instance FromJSON AnnotateImageRequest where
        parseJSON
          = withObject "AnnotateImageRequest"
              (\ o ->
                 AnnotateImageRequest' <$>
                   (o .:? "image") <*> (o .:? "features" .!= mempty) <*>
                     (o .:? "imageContext"))

instance ToJSON AnnotateImageRequest where
        toJSON AnnotateImageRequest'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _airImage,
                  ("features" .=) <$> _airFeatures,
                  ("imageContext" .=) <$> _airImageContext])

-- | Detected language for a structural component.
--
-- /See:/ 'detectedLanguage' smart constructor.
data DetectedLanguage = DetectedLanguage'
    { _dlLanguageCode :: !(Maybe Text)
    , _dlConfidence :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetectedLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLanguageCode'
--
-- * 'dlConfidence'
detectedLanguage
    :: DetectedLanguage
detectedLanguage = 
    DetectedLanguage'
    { _dlLanguageCode = Nothing
    , _dlConfidence = Nothing
    }

-- | The BCP-47 language code, such as \"en-US\" or \"sr-Latn\". For more
-- information, see
-- http:\/\/www.unicode.org\/reports\/tr35\/#Unicode_locale_identifier.
dlLanguageCode :: Lens' DetectedLanguage (Maybe Text)
dlLanguageCode
  = lens _dlLanguageCode
      (\ s a -> s{_dlLanguageCode = a})

-- | Confidence of detected language. Range [0, 1].
dlConfidence :: Lens' DetectedLanguage (Maybe Double)
dlConfidence
  = lens _dlConfidence (\ s a -> s{_dlConfidence = a})
      . mapping _Coerce

instance FromJSON DetectedLanguage where
        parseJSON
          = withObject "DetectedLanguage"
              (\ o ->
                 DetectedLanguage' <$>
                   (o .:? "languageCode") <*> (o .:? "confidence"))

instance ToJSON DetectedLanguage where
        toJSON DetectedLanguage'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _dlLanguageCode,
                  ("confidence" .=) <$> _dlConfidence])

-- | Metadata for online images.
--
-- /See:/ 'webImage' smart constructor.
data WebImage = WebImage'
    { _wiScore :: !(Maybe (Textual Double))
    , _wiURL :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wiScore'
--
-- * 'wiURL'
webImage
    :: WebImage
webImage = 
    WebImage'
    { _wiScore = Nothing
    , _wiURL = Nothing
    }

-- | (Deprecated) Overall relevancy score for the image.
wiScore :: Lens' WebImage (Maybe Double)
wiScore
  = lens _wiScore (\ s a -> s{_wiScore = a}) .
      mapping _Coerce

-- | The result image URL.
wiURL :: Lens' WebImage (Maybe Text)
wiURL = lens _wiURL (\ s a -> s{_wiURL = a})

instance FromJSON WebImage where
        parseJSON
          = withObject "WebImage"
              (\ o ->
                 WebImage' <$> (o .:? "score") <*> (o .:? "url"))

instance ToJSON WebImage where
        toJSON WebImage'{..}
          = object
              (catMaybes
                 [("score" .=) <$> _wiScore, ("url" .=) <$> _wiURL])

-- | Relevant information for the image from the Internet.
--
-- /See:/ 'webDetection' smart constructor.
data WebDetection = WebDetection'
    { _wdVisuallySimilarImages :: !(Maybe [WebImage])
    , _wdBestGuessLabels :: !(Maybe [WebLabel])
    , _wdPagesWithMatchingImages :: !(Maybe [WebPage])
    , _wdPartialMatchingImages :: !(Maybe [WebImage])
    , _wdFullMatchingImages :: !(Maybe [WebImage])
    , _wdWebEntities :: !(Maybe [WebEntity])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wdVisuallySimilarImages'
--
-- * 'wdBestGuessLabels'
--
-- * 'wdPagesWithMatchingImages'
--
-- * 'wdPartialMatchingImages'
--
-- * 'wdFullMatchingImages'
--
-- * 'wdWebEntities'
webDetection
    :: WebDetection
webDetection = 
    WebDetection'
    { _wdVisuallySimilarImages = Nothing
    , _wdBestGuessLabels = Nothing
    , _wdPagesWithMatchingImages = Nothing
    , _wdPartialMatchingImages = Nothing
    , _wdFullMatchingImages = Nothing
    , _wdWebEntities = Nothing
    }

-- | The visually similar image results.
wdVisuallySimilarImages :: Lens' WebDetection [WebImage]
wdVisuallySimilarImages
  = lens _wdVisuallySimilarImages
      (\ s a -> s{_wdVisuallySimilarImages = a})
      . _Default
      . _Coerce

-- | Best guess text labels for the request image.
wdBestGuessLabels :: Lens' WebDetection [WebLabel]
wdBestGuessLabels
  = lens _wdBestGuessLabels
      (\ s a -> s{_wdBestGuessLabels = a})
      . _Default
      . _Coerce

-- | Web pages containing the matching images from the Internet.
wdPagesWithMatchingImages :: Lens' WebDetection [WebPage]
wdPagesWithMatchingImages
  = lens _wdPagesWithMatchingImages
      (\ s a -> s{_wdPagesWithMatchingImages = a})
      . _Default
      . _Coerce

-- | Partial matching images from the Internet. Those images are similar
-- enough to share some key-point features. For example an original image
-- will likely have partial matching for its crops.
wdPartialMatchingImages :: Lens' WebDetection [WebImage]
wdPartialMatchingImages
  = lens _wdPartialMatchingImages
      (\ s a -> s{_wdPartialMatchingImages = a})
      . _Default
      . _Coerce

-- | Fully matching images from the Internet. Can include resized copies of
-- the query image.
wdFullMatchingImages :: Lens' WebDetection [WebImage]
wdFullMatchingImages
  = lens _wdFullMatchingImages
      (\ s a -> s{_wdFullMatchingImages = a})
      . _Default
      . _Coerce

-- | Deduced entities from similar images on the Internet.
wdWebEntities :: Lens' WebDetection [WebEntity]
wdWebEntities
  = lens _wdWebEntities
      (\ s a -> s{_wdWebEntities = a})
      . _Default
      . _Coerce

instance FromJSON WebDetection where
        parseJSON
          = withObject "WebDetection"
              (\ o ->
                 WebDetection' <$>
                   (o .:? "visuallySimilarImages" .!= mempty) <*>
                     (o .:? "bestGuessLabels" .!= mempty)
                     <*> (o .:? "pagesWithMatchingImages" .!= mempty)
                     <*> (o .:? "partialMatchingImages" .!= mempty)
                     <*> (o .:? "fullMatchingImages" .!= mempty)
                     <*> (o .:? "webEntities" .!= mempty))

instance ToJSON WebDetection where
        toJSON WebDetection'{..}
          = object
              (catMaybes
                 [("visuallySimilarImages" .=) <$>
                    _wdVisuallySimilarImages,
                  ("bestGuessLabels" .=) <$> _wdBestGuessLabels,
                  ("pagesWithMatchingImages" .=) <$>
                    _wdPagesWithMatchingImages,
                  ("partialMatchingImages" .=) <$>
                    _wdPartialMatchingImages,
                  ("fullMatchingImages" .=) <$> _wdFullMatchingImages,
                  ("webEntities" .=) <$> _wdWebEntities])

-- | External image source (Google Cloud Storage or web URL image location).
--
-- /See:/ 'imageSource' smart constructor.
data ImageSource = ImageSource'
    { _isGcsImageURI :: !(Maybe Text)
    , _isImageURI :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isGcsImageURI'
--
-- * 'isImageURI'
imageSource
    :: ImageSource
imageSource = 
    ImageSource'
    { _isGcsImageURI = Nothing
    , _isImageURI = Nothing
    }

-- | **Use \`image_uri\` instead.** The Google Cloud Storage URI of the form
-- \`gs:\/\/bucket_name\/object_name\`. Object versioning is not supported.
-- See [Google Cloud Storage Request
-- URIs](https:\/\/cloud.google.com\/storage\/docs\/reference-uris) for
-- more info.
isGcsImageURI :: Lens' ImageSource (Maybe Text)
isGcsImageURI
  = lens _isGcsImageURI
      (\ s a -> s{_isGcsImageURI = a})

-- | The URI of the source image. Can be either: 1. A Google Cloud Storage
-- URI of the form \`gs:\/\/bucket_name\/object_name\`. Object versioning
-- is not supported. See [Google Cloud Storage Request
-- URIs](https:\/\/cloud.google.com\/storage\/docs\/reference-uris) for
-- more info. 2. A publicly-accessible image HTTP\/HTTPS URL. When fetching
-- images from HTTP\/HTTPS URLs, Google cannot guarantee that the request
-- will be completed. Your request may fail if the specified host denies
-- the request (e.g. due to request throttling or DOS prevention), or if
-- Google throttles requests to the site for abuse prevention. You should
-- not depend on externally-hosted images for production applications. When
-- both \`gcs_image_uri\` and \`image_uri\` are specified, \`image_uri\`
-- takes precedence.
isImageURI :: Lens' ImageSource (Maybe Text)
isImageURI
  = lens _isImageURI (\ s a -> s{_isImageURI = a})

instance FromJSON ImageSource where
        parseJSON
          = withObject "ImageSource"
              (\ o ->
                 ImageSource' <$>
                   (o .:? "gcsImageUri") <*> (o .:? "imageUri"))

instance ToJSON ImageSource where
        toJSON ImageSource'{..}
          = object
              (catMaybes
                 [("gcsImageUri" .=) <$> _isGcsImageURI,
                  ("imageUri" .=) <$> _isImageURI])

-- | Single crop hint that is used to generate a new crop when serving an
-- image.
--
-- /See:/ 'cropHint' smart constructor.
data CropHint = CropHint'
    { _chBoundingPoly :: !(Maybe BoundingPoly)
    , _chConfidence :: !(Maybe (Textual Double))
    , _chImportanceFraction :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CropHint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chBoundingPoly'
--
-- * 'chConfidence'
--
-- * 'chImportanceFraction'
cropHint
    :: CropHint
cropHint = 
    CropHint'
    { _chBoundingPoly = Nothing
    , _chConfidence = Nothing
    , _chImportanceFraction = Nothing
    }

-- | The bounding polygon for the crop region. The coordinates of the
-- bounding box are in the original image\'s scale, as returned in
-- \`ImageParams\`.
chBoundingPoly :: Lens' CropHint (Maybe BoundingPoly)
chBoundingPoly
  = lens _chBoundingPoly
      (\ s a -> s{_chBoundingPoly = a})

-- | Confidence of this being a salient region. Range [0, 1].
chConfidence :: Lens' CropHint (Maybe Double)
chConfidence
  = lens _chConfidence (\ s a -> s{_chConfidence = a})
      . mapping _Coerce

-- | Fraction of importance of this salient region with respect to the
-- original image.
chImportanceFraction :: Lens' CropHint (Maybe Double)
chImportanceFraction
  = lens _chImportanceFraction
      (\ s a -> s{_chImportanceFraction = a})
      . mapping _Coerce

instance FromJSON CropHint where
        parseJSON
          = withObject "CropHint"
              (\ o ->
                 CropHint' <$>
                   (o .:? "boundingPoly") <*> (o .:? "confidence") <*>
                     (o .:? "importanceFraction"))

instance ToJSON CropHint where
        toJSON CropHint'{..}
          = object
              (catMaybes
                 [("boundingPoly" .=) <$> _chBoundingPoly,
                  ("confidence" .=) <$> _chConfidence,
                  ("importanceFraction" .=) <$> _chImportanceFraction])

-- | Set of features pertaining to the image, computed by computer vision
-- methods over safe-search verticals (for example, adult, spoof, medical,
-- violence).
--
-- /See:/ 'safeSearchAnnotation' smart constructor.
data SafeSearchAnnotation = SafeSearchAnnotation'
    { _ssaSpoof :: !(Maybe SafeSearchAnnotationSpoof)
    , _ssaRacy :: !(Maybe SafeSearchAnnotationRacy)
    , _ssaAdult :: !(Maybe SafeSearchAnnotationAdult)
    , _ssaMedical :: !(Maybe SafeSearchAnnotationMedical)
    , _ssaViolence :: !(Maybe SafeSearchAnnotationViolence)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SafeSearchAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssaSpoof'
--
-- * 'ssaRacy'
--
-- * 'ssaAdult'
--
-- * 'ssaMedical'
--
-- * 'ssaViolence'
safeSearchAnnotation
    :: SafeSearchAnnotation
safeSearchAnnotation = 
    SafeSearchAnnotation'
    { _ssaSpoof = Nothing
    , _ssaRacy = Nothing
    , _ssaAdult = Nothing
    , _ssaMedical = Nothing
    , _ssaViolence = Nothing
    }

-- | Spoof likelihood. The likelihood that an modification was made to the
-- image\'s canonical version to make it appear funny or offensive.
ssaSpoof :: Lens' SafeSearchAnnotation (Maybe SafeSearchAnnotationSpoof)
ssaSpoof = lens _ssaSpoof (\ s a -> s{_ssaSpoof = a})

-- | Likelihood that the request image contains racy content. Racy content
-- may include (but is not limited to) skimpy or sheer clothing,
-- strategically covered nudity, lewd or provocative poses, or close-ups of
-- sensitive body areas.
ssaRacy :: Lens' SafeSearchAnnotation (Maybe SafeSearchAnnotationRacy)
ssaRacy = lens _ssaRacy (\ s a -> s{_ssaRacy = a})

-- | Represents the adult content likelihood for the image. Adult content may
-- contain elements such as nudity, pornographic images or cartoons, or
-- sexual activities.
ssaAdult :: Lens' SafeSearchAnnotation (Maybe SafeSearchAnnotationAdult)
ssaAdult = lens _ssaAdult (\ s a -> s{_ssaAdult = a})

-- | Likelihood that this is a medical image.
ssaMedical :: Lens' SafeSearchAnnotation (Maybe SafeSearchAnnotationMedical)
ssaMedical
  = lens _ssaMedical (\ s a -> s{_ssaMedical = a})

-- | Likelihood that this image contains violent content.
ssaViolence :: Lens' SafeSearchAnnotation (Maybe SafeSearchAnnotationViolence)
ssaViolence
  = lens _ssaViolence (\ s a -> s{_ssaViolence = a})

instance FromJSON SafeSearchAnnotation where
        parseJSON
          = withObject "SafeSearchAnnotation"
              (\ o ->
                 SafeSearchAnnotation' <$>
                   (o .:? "spoof") <*> (o .:? "racy") <*>
                     (o .:? "adult")
                     <*> (o .:? "medical")
                     <*> (o .:? "violence"))

instance ToJSON SafeSearchAnnotation where
        toJSON SafeSearchAnnotation'{..}
          = object
              (catMaybes
                 [("spoof" .=) <$> _ssaSpoof,
                  ("racy" .=) <$> _ssaRacy, ("adult" .=) <$> _ssaAdult,
                  ("medical" .=) <$> _ssaMedical,
                  ("violence" .=) <$> _ssaViolence])

-- | Image context and\/or feature-specific parameters.
--
-- /See:/ 'imageContext' smart constructor.
data ImageContext = ImageContext'
    { _icCropHintsParams :: !(Maybe CropHintsParams)
    , _icWebDetectionParams :: !(Maybe WebDetectionParams)
    , _icLanguageHints :: !(Maybe [Text])
    , _icLatLongRect :: !(Maybe LatLongRect)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icCropHintsParams'
--
-- * 'icWebDetectionParams'
--
-- * 'icLanguageHints'
--
-- * 'icLatLongRect'
imageContext
    :: ImageContext
imageContext = 
    ImageContext'
    { _icCropHintsParams = Nothing
    , _icWebDetectionParams = Nothing
    , _icLanguageHints = Nothing
    , _icLatLongRect = Nothing
    }

-- | Parameters for crop hints annotation request.
icCropHintsParams :: Lens' ImageContext (Maybe CropHintsParams)
icCropHintsParams
  = lens _icCropHintsParams
      (\ s a -> s{_icCropHintsParams = a})

-- | Parameters for web detection.
icWebDetectionParams :: Lens' ImageContext (Maybe WebDetectionParams)
icWebDetectionParams
  = lens _icWebDetectionParams
      (\ s a -> s{_icWebDetectionParams = a})

-- | List of languages to use for TEXT_DETECTION. In most cases, an empty
-- value yields the best results since it enables automatic language
-- detection. For languages based on the Latin alphabet, setting
-- \`language_hints\` is not needed. In rare cases, when the language of
-- the text in the image is known, setting a hint will help get better
-- results (although it will be a significant hindrance if the hint is
-- wrong). Text detection returns an error if one or more of the specified
-- languages is not one of the [supported
-- languages](\/vision\/docs\/languages).
icLanguageHints :: Lens' ImageContext [Text]
icLanguageHints
  = lens _icLanguageHints
      (\ s a -> s{_icLanguageHints = a})
      . _Default
      . _Coerce

-- | lat\/long rectangle that specifies the location of the image.
icLatLongRect :: Lens' ImageContext (Maybe LatLongRect)
icLatLongRect
  = lens _icLatLongRect
      (\ s a -> s{_icLatLongRect = a})

instance FromJSON ImageContext where
        parseJSON
          = withObject "ImageContext"
              (\ o ->
                 ImageContext' <$>
                   (o .:? "cropHintsParams") <*>
                     (o .:? "webDetectionParams")
                     <*> (o .:? "languageHints" .!= mempty)
                     <*> (o .:? "latLongRect"))

instance ToJSON ImageContext where
        toJSON ImageContext'{..}
          = object
              (catMaybes
                 [("cropHintsParams" .=) <$> _icCropHintsParams,
                  ("webDetectionParams" .=) <$> _icWebDetectionParams,
                  ("languageHints" .=) <$> _icLanguageHints,
                  ("latLongRect" .=) <$> _icLatLongRect])

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
--
-- /See:/ 'operationMetadata' smart constructor.
newtype OperationMetadata = OperationMetadata'
    { _omAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'omAddtional'
operationMetadata
    :: HashMap Text JSONValue -- ^ 'omAddtional'
    -> OperationMetadata
operationMetadata pOmAddtional_ = 
    OperationMetadata'
    { _omAddtional = _Coerce # pOmAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
omAddtional :: Lens' OperationMetadata (HashMap Text JSONValue)
omAddtional
  = lens _omAddtional (\ s a -> s{_omAddtional = a}) .
      _Coerce

instance FromJSON OperationMetadata where
        parseJSON
          = withObject "OperationMetadata"
              (\ o -> OperationMetadata' <$> (parseJSONObject o))

instance ToJSON OperationMetadata where
        toJSON = toJSON . _omAddtional

-- | Metadata for web pages.
--
-- /See:/ 'webPage' smart constructor.
data WebPage = WebPage'
    { _wpScore :: !(Maybe (Textual Double))
    , _wpURL :: !(Maybe Text)
    , _wpPageTitle :: !(Maybe Text)
    , _wpPartialMatchingImages :: !(Maybe [WebImage])
    , _wpFullMatchingImages :: !(Maybe [WebImage])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebPage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wpScore'
--
-- * 'wpURL'
--
-- * 'wpPageTitle'
--
-- * 'wpPartialMatchingImages'
--
-- * 'wpFullMatchingImages'
webPage
    :: WebPage
webPage = 
    WebPage'
    { _wpScore = Nothing
    , _wpURL = Nothing
    , _wpPageTitle = Nothing
    , _wpPartialMatchingImages = Nothing
    , _wpFullMatchingImages = Nothing
    }

-- | (Deprecated) Overall relevancy score for the web page.
wpScore :: Lens' WebPage (Maybe Double)
wpScore
  = lens _wpScore (\ s a -> s{_wpScore = a}) .
      mapping _Coerce

-- | The result web page URL.
wpURL :: Lens' WebPage (Maybe Text)
wpURL = lens _wpURL (\ s a -> s{_wpURL = a})

-- | Title for the web page, may contain HTML markups.
wpPageTitle :: Lens' WebPage (Maybe Text)
wpPageTitle
  = lens _wpPageTitle (\ s a -> s{_wpPageTitle = a})

-- | Partial matching images on the page. Those images are similar enough to
-- share some key-point features. For example an original image will likely
-- have partial matching for its crops.
wpPartialMatchingImages :: Lens' WebPage [WebImage]
wpPartialMatchingImages
  = lens _wpPartialMatchingImages
      (\ s a -> s{_wpPartialMatchingImages = a})
      . _Default
      . _Coerce

-- | Fully matching images on the page. Can include resized copies of the
-- query image.
wpFullMatchingImages :: Lens' WebPage [WebImage]
wpFullMatchingImages
  = lens _wpFullMatchingImages
      (\ s a -> s{_wpFullMatchingImages = a})
      . _Default
      . _Coerce

instance FromJSON WebPage where
        parseJSON
          = withObject "WebPage"
              (\ o ->
                 WebPage' <$>
                   (o .:? "score") <*> (o .:? "url") <*>
                     (o .:? "pageTitle")
                     <*> (o .:? "partialMatchingImages" .!= mempty)
                     <*> (o .:? "fullMatchingImages" .!= mempty))

instance ToJSON WebPage where
        toJSON WebPage'{..}
          = object
              (catMaybes
                 [("score" .=) <$> _wpScore, ("url" .=) <$> _wpURL,
                  ("pageTitle" .=) <$> _wpPageTitle,
                  ("partialMatchingImages" .=) <$>
                    _wpPartialMatchingImages,
                  ("fullMatchingImages" .=) <$> _wpFullMatchingImages])

-- | Set of dominant colors and their corresponding scores.
--
-- /See:/ 'dominantColorsAnnotation' smart constructor.
newtype DominantColorsAnnotation = DominantColorsAnnotation'
    { _dcaColors :: Maybe [ColorInfo]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DominantColorsAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaColors'
dominantColorsAnnotation
    :: DominantColorsAnnotation
dominantColorsAnnotation = 
    DominantColorsAnnotation'
    { _dcaColors = Nothing
    }

-- | RGB color values with their score and pixel fraction.
dcaColors :: Lens' DominantColorsAnnotation [ColorInfo]
dcaColors
  = lens _dcaColors (\ s a -> s{_dcaColors = a}) .
      _Default
      . _Coerce

instance FromJSON DominantColorsAnnotation where
        parseJSON
          = withObject "DominantColorsAnnotation"
              (\ o ->
                 DominantColorsAnnotation' <$>
                   (o .:? "colors" .!= mempty))

instance ToJSON DominantColorsAnnotation where
        toJSON DominantColorsAnnotation'{..}
          = object (catMaybes [("colors" .=) <$> _dcaColors])

-- | Rectangle determined by min and max \`LatLng\` pairs.
--
-- /See:/ 'latLongRect' smart constructor.
data LatLongRect = LatLongRect'
    { _llrMaxLatLng :: !(Maybe LatLng)
    , _llrMinLatLng :: !(Maybe LatLng)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LatLongRect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llrMaxLatLng'
--
-- * 'llrMinLatLng'
latLongRect
    :: LatLongRect
latLongRect = 
    LatLongRect'
    { _llrMaxLatLng = Nothing
    , _llrMinLatLng = Nothing
    }

-- | Max lat\/long pair.
llrMaxLatLng :: Lens' LatLongRect (Maybe LatLng)
llrMaxLatLng
  = lens _llrMaxLatLng (\ s a -> s{_llrMaxLatLng = a})

-- | Min lat\/long pair.
llrMinLatLng :: Lens' LatLongRect (Maybe LatLng)
llrMinLatLng
  = lens _llrMinLatLng (\ s a -> s{_llrMinLatLng = a})

instance FromJSON LatLongRect where
        parseJSON
          = withObject "LatLongRect"
              (\ o ->
                 LatLongRect' <$>
                   (o .:? "maxLatLng") <*> (o .:? "minLatLng"))

instance ToJSON LatLongRect where
        toJSON LatLongRect'{..}
          = object
              (catMaybes
                 [("maxLatLng" .=) <$> _llrMaxLatLng,
                  ("minLatLng" .=) <$> _llrMinLatLng])

-- | The response for a single offline file annotation request.
--
-- /See:/ 'googleCloudVisionV1p2beta1AsyncAnnotateFileResponse' smart constructor.
newtype GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse = GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse'
    { _gcvvaafrOutputConfig :: Maybe GoogleCloudVisionV1p2beta1OutputConfig
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvvaafrOutputConfig'
googleCloudVisionV1p2beta1AsyncAnnotateFileResponse
    :: GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse
googleCloudVisionV1p2beta1AsyncAnnotateFileResponse = 
    GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse'
    { _gcvvaafrOutputConfig = Nothing
    }

-- | The output location and metadata from AsyncAnnotateFileRequest.
gcvvaafrOutputConfig :: Lens' GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse (Maybe GoogleCloudVisionV1p2beta1OutputConfig)
gcvvaafrOutputConfig
  = lens _gcvvaafrOutputConfig
      (\ s a -> s{_gcvvaafrOutputConfig = a})

instance FromJSON
         GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse
         where
        parseJSON
          = withObject
              "GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse"
              (\ o ->
                 GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse'
                   <$> (o .:? "outputConfig"))

instance ToJSON
         GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse
         where
        toJSON
          GoogleCloudVisionV1p2beta1AsyncAnnotateFileResponse'{..}
          = object
              (catMaybes
                 [("outputConfig" .=) <$> _gcvvaafrOutputConfig])

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
--
-- /See:/ 'operationResponse' smart constructor.
newtype OperationResponse = OperationResponse'
    { _orAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orAddtional'
operationResponse
    :: HashMap Text JSONValue -- ^ 'orAddtional'
    -> OperationResponse
operationResponse pOrAddtional_ = 
    OperationResponse'
    { _orAddtional = _Coerce # pOrAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
orAddtional :: Lens' OperationResponse (HashMap Text JSONValue)
orAddtional
  = lens _orAddtional (\ s a -> s{_orAddtional = a}) .
      _Coerce

instance FromJSON OperationResponse where
        parseJSON
          = withObject "OperationResponse"
              (\ o -> OperationResponse' <$> (parseJSONObject o))

instance ToJSON OperationResponse where
        toJSON = toJSON . _orAddtional

-- | A word representation.
--
-- /See:/ 'word' smart constructor.
data Word = Word'
    { _wProperty :: !(Maybe TextProperty)
    , _wBoundingBox :: !(Maybe BoundingPoly)
    , _wSymbols :: !(Maybe [Symbol])
    , _wConfidence :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Word' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wProperty'
--
-- * 'wBoundingBox'
--
-- * 'wSymbols'
--
-- * 'wConfidence'
word
    :: Word
word = 
    Word'
    { _wProperty = Nothing
    , _wBoundingBox = Nothing
    , _wSymbols = Nothing
    , _wConfidence = Nothing
    }

-- | Additional information detected for the word.
wProperty :: Lens' Word (Maybe TextProperty)
wProperty
  = lens _wProperty (\ s a -> s{_wProperty = a})

-- | The bounding box for the word. The vertices are in the order of
-- top-left, top-right, bottom-right, bottom-left. When a rotation of the
-- bounding box is detected the rotation is represented as around the
-- top-left corner as defined when the text is read in the \'natural\'
-- orientation. For example: * when the text is horizontal it might look
-- like: 0----1 | | 3----2 * when it\'s rotated 180 degrees around the
-- top-left corner it becomes: 2----3 | | 1----0 and the vertice order will
-- still be (0, 1, 2, 3).
wBoundingBox :: Lens' Word (Maybe BoundingPoly)
wBoundingBox
  = lens _wBoundingBox (\ s a -> s{_wBoundingBox = a})

-- | List of symbols in the word. The order of the symbols follows the
-- natural reading order.
wSymbols :: Lens' Word [Symbol]
wSymbols
  = lens _wSymbols (\ s a -> s{_wSymbols = a}) .
      _Default
      . _Coerce

-- | Confidence of the OCR results for the word. Range [0, 1].
wConfidence :: Lens' Word (Maybe Double)
wConfidence
  = lens _wConfidence (\ s a -> s{_wConfidence = a}) .
      mapping _Coerce

instance FromJSON Word where
        parseJSON
          = withObject "Word"
              (\ o ->
                 Word' <$>
                   (o .:? "property") <*> (o .:? "boundingBox") <*>
                     (o .:? "symbols" .!= mempty)
                     <*> (o .:? "confidence"))

instance ToJSON Word where
        toJSON Word'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _wProperty,
                  ("boundingBox" .=) <$> _wBoundingBox,
                  ("symbols" .=) <$> _wSymbols,
                  ("confidence" .=) <$> _wConfidence])

-- | Response to a batch image annotation request.
--
-- /See:/ 'batchAnnotateImagesResponse' smart constructor.
newtype BatchAnnotateImagesResponse = BatchAnnotateImagesResponse'
    { _bairResponses :: Maybe [AnnotateImageResponse]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchAnnotateImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bairResponses'
batchAnnotateImagesResponse
    :: BatchAnnotateImagesResponse
batchAnnotateImagesResponse = 
    BatchAnnotateImagesResponse'
    { _bairResponses = Nothing
    }

-- | Individual responses to image annotation requests within the batch.
bairResponses :: Lens' BatchAnnotateImagesResponse [AnnotateImageResponse]
bairResponses
  = lens _bairResponses
      (\ s a -> s{_bairResponses = a})
      . _Default
      . _Coerce

instance FromJSON BatchAnnotateImagesResponse where
        parseJSON
          = withObject "BatchAnnotateImagesResponse"
              (\ o ->
                 BatchAnnotateImagesResponse' <$>
                   (o .:? "responses" .!= mempty))

instance ToJSON BatchAnnotateImagesResponse where
        toJSON BatchAnnotateImagesResponse'{..}
          = object
              (catMaybes [("responses" .=) <$> _bairResponses])

-- | Set of crop hints that are used to generate new crops when serving
-- images.
--
-- /See:/ 'cropHintsAnnotation' smart constructor.
newtype CropHintsAnnotation = CropHintsAnnotation'
    { _chaCropHints :: Maybe [CropHint]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CropHintsAnnotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaCropHints'
cropHintsAnnotation
    :: CropHintsAnnotation
cropHintsAnnotation = 
    CropHintsAnnotation'
    { _chaCropHints = Nothing
    }

-- | Crop hint results.
chaCropHints :: Lens' CropHintsAnnotation [CropHint]
chaCropHints
  = lens _chaCropHints (\ s a -> s{_chaCropHints = a})
      . _Default
      . _Coerce

instance FromJSON CropHintsAnnotation where
        parseJSON
          = withObject "CropHintsAnnotation"
              (\ o ->
                 CropHintsAnnotation' <$>
                   (o .:? "cropHints" .!= mempty))

instance ToJSON CropHintsAnnotation where
        toJSON CropHintsAnnotation'{..}
          = object
              (catMaybes [("cropHints" .=) <$> _chaCropHints])

-- | A 3D position in the image, used primarily for Face detection landmarks.
-- A valid Position must have both x and y coordinates. The position
-- coordinates are in the same scale as the original image.
--
-- /See:/ 'position' smart constructor.
data Position = Position'
    { _pZ :: !(Maybe (Textual Double))
    , _pX :: !(Maybe (Textual Double))
    , _pY :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Position' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pZ'
--
-- * 'pX'
--
-- * 'pY'
position
    :: Position
position = 
    Position'
    { _pZ = Nothing
    , _pX = Nothing
    , _pY = Nothing
    }

-- | Z coordinate (or depth).
pZ :: Lens' Position (Maybe Double)
pZ = lens _pZ (\ s a -> s{_pZ = a}) . mapping _Coerce

-- | X coordinate.
pX :: Lens' Position (Maybe Double)
pX = lens _pX (\ s a -> s{_pX = a}) . mapping _Coerce

-- | Y coordinate.
pY :: Lens' Position (Maybe Double)
pY = lens _pY (\ s a -> s{_pY = a}) . mapping _Coerce

instance FromJSON Position where
        parseJSON
          = withObject "Position"
              (\ o ->
                 Position' <$>
                   (o .:? "z") <*> (o .:? "x") <*> (o .:? "y"))

instance ToJSON Position where
        toJSON Position'{..}
          = object
              (catMaybes
                 [("z" .=) <$> _pZ, ("x" .=) <$> _pX,
                  ("y" .=) <$> _pY])

-- | The Google Cloud Storage location where the output will be written to.
--
-- /See:/ 'googleCloudVisionV1p2beta1GcsDestination' smart constructor.
newtype GoogleCloudVisionV1p2beta1GcsDestination = GoogleCloudVisionV1p2beta1GcsDestination'
    { _gcvvgdURI :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleCloudVisionV1p2beta1GcsDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvvgdURI'
googleCloudVisionV1p2beta1GcsDestination
    :: GoogleCloudVisionV1p2beta1GcsDestination
googleCloudVisionV1p2beta1GcsDestination = 
    GoogleCloudVisionV1p2beta1GcsDestination'
    { _gcvvgdURI = Nothing
    }

-- | Google Cloud Storage URI where the results will be stored. Results will
-- be in JSON format and preceded by its corresponding input URI. This
-- field can either represent a single file, or a prefix for multiple
-- outputs. Prefixes must end in a \`\/\`. Examples: * File:
-- gs:\/\/bucket-name\/filename.json * Prefix:
-- gs:\/\/bucket-name\/prefix\/here\/ * File:
-- gs:\/\/bucket-name\/prefix\/here If multiple outputs, each response is
-- still AnnotateFileResponse, each of which contains some subset of the
-- full list of AnnotateImageResponse. Multiple outputs can happen if, for
-- example, the output JSON is too large and overflows into multiple
-- sharded files.
gcvvgdURI :: Lens' GoogleCloudVisionV1p2beta1GcsDestination (Maybe Text)
gcvvgdURI
  = lens _gcvvgdURI (\ s a -> s{_gcvvgdURI = a})

instance FromJSON
         GoogleCloudVisionV1p2beta1GcsDestination where
        parseJSON
          = withObject
              "GoogleCloudVisionV1p2beta1GcsDestination"
              (\ o ->
                 GoogleCloudVisionV1p2beta1GcsDestination' <$>
                   (o .:? "uri"))

instance ToJSON
         GoogleCloudVisionV1p2beta1GcsDestination where
        toJSON GoogleCloudVisionV1p2beta1GcsDestination'{..}
          = object (catMaybes [("uri" .=) <$> _gcvvgdURI])
