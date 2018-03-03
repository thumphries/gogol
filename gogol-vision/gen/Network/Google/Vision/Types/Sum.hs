{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Vision.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Vision.Types.Sum where

import Network.Google.Prelude

-- | Under-exposed likelihood.
data FaceAnnotationUnderExposedLikelihood
    = Unknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | VeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | Unlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | Possible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | Likely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | VeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationUnderExposedLikelihood

instance FromHttpApiData FaceAnnotationUnderExposedLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right Unknown
        "VERY_UNLIKELY" -> Right VeryUnlikely
        "UNLIKELY" -> Right Unlikely
        "POSSIBLE" -> Right Possible
        "LIKELY" -> Right Likely
        "VERY_LIKELY" -> Right VeryLikely
        x -> Left ("Unable to parse FaceAnnotationUnderExposedLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationUnderExposedLikelihood where
    toQueryParam = \case
        Unknown -> "UNKNOWN"
        VeryUnlikely -> "VERY_UNLIKELY"
        Unlikely -> "UNLIKELY"
        Possible -> "POSSIBLE"
        Likely -> "LIKELY"
        VeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationUnderExposedLikelihood where
    parseJSON = parseJSONText "FaceAnnotationUnderExposedLikelihood"

instance ToJSON FaceAnnotationUnderExposedLikelihood where
    toJSON = toJSONText

-- | Current state of the batch operation.
data GoogleCloudVisionV1p2beta1OperationMetadataState
    = StateUnspecified
      -- ^ @STATE_UNSPECIFIED@
      -- Invalid.
    | Created
      -- ^ @CREATED@
      -- Request is received.
    | Running
      -- ^ @RUNNING@
      -- Request is actively being processed.
    | Done
      -- ^ @DONE@
      -- The batch processing is done.
    | Cancelled
      -- ^ @CANCELLED@
      -- The batch processing was cancelled.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable GoogleCloudVisionV1p2beta1OperationMetadataState

instance FromHttpApiData GoogleCloudVisionV1p2beta1OperationMetadataState where
    parseQueryParam = \case
        "STATE_UNSPECIFIED" -> Right StateUnspecified
        "CREATED" -> Right Created
        "RUNNING" -> Right Running
        "DONE" -> Right Done
        "CANCELLED" -> Right Cancelled
        x -> Left ("Unable to parse GoogleCloudVisionV1p2beta1OperationMetadataState from: " <> x)

instance ToHttpApiData GoogleCloudVisionV1p2beta1OperationMetadataState where
    toQueryParam = \case
        StateUnspecified -> "STATE_UNSPECIFIED"
        Created -> "CREATED"
        Running -> "RUNNING"
        Done -> "DONE"
        Cancelled -> "CANCELLED"

instance FromJSON GoogleCloudVisionV1p2beta1OperationMetadataState where
    parseJSON = parseJSONText "GoogleCloudVisionV1p2beta1OperationMetadataState"

instance ToJSON GoogleCloudVisionV1p2beta1OperationMetadataState where
    toJSON = toJSONText

-- | Headwear likelihood.
data FaceAnnotationHeadwearLikelihood
    = FAHLUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FAHLVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FAHLUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FAHLPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FAHLLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FAHLVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationHeadwearLikelihood

instance FromHttpApiData FaceAnnotationHeadwearLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FAHLUnknown
        "VERY_UNLIKELY" -> Right FAHLVeryUnlikely
        "UNLIKELY" -> Right FAHLUnlikely
        "POSSIBLE" -> Right FAHLPossible
        "LIKELY" -> Right FAHLLikely
        "VERY_LIKELY" -> Right FAHLVeryLikely
        x -> Left ("Unable to parse FaceAnnotationHeadwearLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationHeadwearLikelihood where
    toQueryParam = \case
        FAHLUnknown -> "UNKNOWN"
        FAHLVeryUnlikely -> "VERY_UNLIKELY"
        FAHLUnlikely -> "UNLIKELY"
        FAHLPossible -> "POSSIBLE"
        FAHLLikely -> "LIKELY"
        FAHLVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationHeadwearLikelihood where
    parseJSON = parseJSONText "FaceAnnotationHeadwearLikelihood"

instance ToJSON FaceAnnotationHeadwearLikelihood where
    toJSON = toJSONText

-- | Detected block type (text, image etc) for this block.
data BlockBlockType
    = BBTUnknown
      -- ^ @UNKNOWN@
      -- Unknown block type.
    | BBTText
      -- ^ @TEXT@
      -- Regular text block.
    | BBTTable
      -- ^ @TABLE@
      -- Table block.
    | BBTPicture
      -- ^ @PICTURE@
      -- Image block.
    | BBTRuler
      -- ^ @RULER@
      -- Horizontal\/vertical line box.
    | BBTBarcode
      -- ^ @BARCODE@
      -- Barcode block.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable BlockBlockType

instance FromHttpApiData BlockBlockType where
    parseQueryParam = \case
        "UNKNOWN" -> Right BBTUnknown
        "TEXT" -> Right BBTText
        "TABLE" -> Right BBTTable
        "PICTURE" -> Right BBTPicture
        "RULER" -> Right BBTRuler
        "BARCODE" -> Right BBTBarcode
        x -> Left ("Unable to parse BlockBlockType from: " <> x)

instance ToHttpApiData BlockBlockType where
    toQueryParam = \case
        BBTUnknown -> "UNKNOWN"
        BBTText -> "TEXT"
        BBTTable -> "TABLE"
        BBTPicture -> "PICTURE"
        BBTRuler -> "RULER"
        BBTBarcode -> "BARCODE"

instance FromJSON BlockBlockType where
    parseJSON = parseJSONText "BlockBlockType"

instance ToJSON BlockBlockType where
    toJSON = toJSONText

-- | Represents the adult content likelihood for the image. Adult content may
-- contain elements such as nudity, pornographic images or cartoons, or
-- sexual activities.
data SafeSearchAnnotationAdult
    = SSAAUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | SSAAVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | SSAAUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | SSAAPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | SSAALikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | SSAAVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SafeSearchAnnotationAdult

instance FromHttpApiData SafeSearchAnnotationAdult where
    parseQueryParam = \case
        "UNKNOWN" -> Right SSAAUnknown
        "VERY_UNLIKELY" -> Right SSAAVeryUnlikely
        "UNLIKELY" -> Right SSAAUnlikely
        "POSSIBLE" -> Right SSAAPossible
        "LIKELY" -> Right SSAALikely
        "VERY_LIKELY" -> Right SSAAVeryLikely
        x -> Left ("Unable to parse SafeSearchAnnotationAdult from: " <> x)

instance ToHttpApiData SafeSearchAnnotationAdult where
    toQueryParam = \case
        SSAAUnknown -> "UNKNOWN"
        SSAAVeryUnlikely -> "VERY_UNLIKELY"
        SSAAUnlikely -> "UNLIKELY"
        SSAAPossible -> "POSSIBLE"
        SSAALikely -> "LIKELY"
        SSAAVeryLikely -> "VERY_LIKELY"

instance FromJSON SafeSearchAnnotationAdult where
    parseJSON = parseJSONText "SafeSearchAnnotationAdult"

instance ToJSON SafeSearchAnnotationAdult where
    toJSON = toJSONText

-- | Anger likelihood.
data FaceAnnotationAngerLikelihood
    = FAALUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FAALVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FAALUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FAALPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FAALLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FAALVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationAngerLikelihood

instance FromHttpApiData FaceAnnotationAngerLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FAALUnknown
        "VERY_UNLIKELY" -> Right FAALVeryUnlikely
        "UNLIKELY" -> Right FAALUnlikely
        "POSSIBLE" -> Right FAALPossible
        "LIKELY" -> Right FAALLikely
        "VERY_LIKELY" -> Right FAALVeryLikely
        x -> Left ("Unable to parse FaceAnnotationAngerLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationAngerLikelihood where
    toQueryParam = \case
        FAALUnknown -> "UNKNOWN"
        FAALVeryUnlikely -> "VERY_UNLIKELY"
        FAALUnlikely -> "UNLIKELY"
        FAALPossible -> "POSSIBLE"
        FAALLikely -> "LIKELY"
        FAALVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationAngerLikelihood where
    parseJSON = parseJSONText "FaceAnnotationAngerLikelihood"

instance ToJSON FaceAnnotationAngerLikelihood where
    toJSON = toJSONText

-- | Likelihood that this is a medical image.
data SafeSearchAnnotationMedical
    = SSAMUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | SSAMVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | SSAMUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | SSAMPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | SSAMLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | SSAMVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SafeSearchAnnotationMedical

instance FromHttpApiData SafeSearchAnnotationMedical where
    parseQueryParam = \case
        "UNKNOWN" -> Right SSAMUnknown
        "VERY_UNLIKELY" -> Right SSAMVeryUnlikely
        "UNLIKELY" -> Right SSAMUnlikely
        "POSSIBLE" -> Right SSAMPossible
        "LIKELY" -> Right SSAMLikely
        "VERY_LIKELY" -> Right SSAMVeryLikely
        x -> Left ("Unable to parse SafeSearchAnnotationMedical from: " <> x)

instance ToHttpApiData SafeSearchAnnotationMedical where
    toQueryParam = \case
        SSAMUnknown -> "UNKNOWN"
        SSAMVeryUnlikely -> "VERY_UNLIKELY"
        SSAMUnlikely -> "UNLIKELY"
        SSAMPossible -> "POSSIBLE"
        SSAMLikely -> "LIKELY"
        SSAMVeryLikely -> "VERY_LIKELY"

instance FromJSON SafeSearchAnnotationMedical where
    parseJSON = parseJSONText "SafeSearchAnnotationMedical"

instance ToJSON SafeSearchAnnotationMedical where
    toJSON = toJSONText

-- | Blurred likelihood.
data FaceAnnotationBlurredLikelihood
    = FABLUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FABLVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FABLUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FABLPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FABLLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FABLVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationBlurredLikelihood

instance FromHttpApiData FaceAnnotationBlurredLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FABLUnknown
        "VERY_UNLIKELY" -> Right FABLVeryUnlikely
        "UNLIKELY" -> Right FABLUnlikely
        "POSSIBLE" -> Right FABLPossible
        "LIKELY" -> Right FABLLikely
        "VERY_LIKELY" -> Right FABLVeryLikely
        x -> Left ("Unable to parse FaceAnnotationBlurredLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationBlurredLikelihood where
    toQueryParam = \case
        FABLUnknown -> "UNKNOWN"
        FABLVeryUnlikely -> "VERY_UNLIKELY"
        FABLUnlikely -> "UNLIKELY"
        FABLPossible -> "POSSIBLE"
        FABLLikely -> "LIKELY"
        FABLVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationBlurredLikelihood where
    parseJSON = parseJSONText "FaceAnnotationBlurredLikelihood"

instance ToJSON FaceAnnotationBlurredLikelihood where
    toJSON = toJSONText

-- | Likelihood that this image contains violent content.
data SafeSearchAnnotationViolence
    = SSAVUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | SSAVVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | SSAVUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | SSAVPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | SSAVLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | SSAVVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SafeSearchAnnotationViolence

instance FromHttpApiData SafeSearchAnnotationViolence where
    parseQueryParam = \case
        "UNKNOWN" -> Right SSAVUnknown
        "VERY_UNLIKELY" -> Right SSAVVeryUnlikely
        "UNLIKELY" -> Right SSAVUnlikely
        "POSSIBLE" -> Right SSAVPossible
        "LIKELY" -> Right SSAVLikely
        "VERY_LIKELY" -> Right SSAVVeryLikely
        x -> Left ("Unable to parse SafeSearchAnnotationViolence from: " <> x)

instance ToHttpApiData SafeSearchAnnotationViolence where
    toQueryParam = \case
        SSAVUnknown -> "UNKNOWN"
        SSAVVeryUnlikely -> "VERY_UNLIKELY"
        SSAVUnlikely -> "UNLIKELY"
        SSAVPossible -> "POSSIBLE"
        SSAVLikely -> "LIKELY"
        SSAVVeryLikely -> "VERY_LIKELY"

instance FromJSON SafeSearchAnnotationViolence where
    parseJSON = parseJSONText "SafeSearchAnnotationViolence"

instance ToJSON SafeSearchAnnotationViolence where
    toJSON = toJSONText

-- | The feature type.
data FeatureType
    = TypeUnspecified
      -- ^ @TYPE_UNSPECIFIED@
      -- Unspecified feature type.
    | FaceDetection
      -- ^ @FACE_DETECTION@
      -- Run face detection.
    | LandmarkDetection
      -- ^ @LANDMARK_DETECTION@
      -- Run landmark detection.
    | LogoDetection
      -- ^ @LOGO_DETECTION@
      -- Run logo detection.
    | LabelDetection
      -- ^ @LABEL_DETECTION@
      -- Run label detection.
    | TextDetection
      -- ^ @TEXT_DETECTION@
      -- Run text detection \/ optical character recognition (OCR). Text
      -- detection is optimized for areas of text within a larger image; if the
      -- image is a document, use \`DOCUMENT_TEXT_DETECTION\` instead.
    | DocumentTextDetection
      -- ^ @DOCUMENT_TEXT_DETECTION@
      -- Run dense text document OCR. Takes precedence when both
      -- \`DOCUMENT_TEXT_DETECTION\` and \`TEXT_DETECTION\` are present.
    | SafeSearchDetection
      -- ^ @SAFE_SEARCH_DETECTION@
      -- Run Safe Search to detect potentially unsafe or undesirable content.
    | ImageProperties
      -- ^ @IMAGE_PROPERTIES@
      -- Compute a set of image properties, such as the image\'s dominant colors.
    | CropHints
      -- ^ @CROP_HINTS@
      -- Run crop hints.
    | WebDetection
      -- ^ @WEB_DETECTION@
      -- Run web detection.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FeatureType

instance FromHttpApiData FeatureType where
    parseQueryParam = \case
        "TYPE_UNSPECIFIED" -> Right TypeUnspecified
        "FACE_DETECTION" -> Right FaceDetection
        "LANDMARK_DETECTION" -> Right LandmarkDetection
        "LOGO_DETECTION" -> Right LogoDetection
        "LABEL_DETECTION" -> Right LabelDetection
        "TEXT_DETECTION" -> Right TextDetection
        "DOCUMENT_TEXT_DETECTION" -> Right DocumentTextDetection
        "SAFE_SEARCH_DETECTION" -> Right SafeSearchDetection
        "IMAGE_PROPERTIES" -> Right ImageProperties
        "CROP_HINTS" -> Right CropHints
        "WEB_DETECTION" -> Right WebDetection
        x -> Left ("Unable to parse FeatureType from: " <> x)

instance ToHttpApiData FeatureType where
    toQueryParam = \case
        TypeUnspecified -> "TYPE_UNSPECIFIED"
        FaceDetection -> "FACE_DETECTION"
        LandmarkDetection -> "LANDMARK_DETECTION"
        LogoDetection -> "LOGO_DETECTION"
        LabelDetection -> "LABEL_DETECTION"
        TextDetection -> "TEXT_DETECTION"
        DocumentTextDetection -> "DOCUMENT_TEXT_DETECTION"
        SafeSearchDetection -> "SAFE_SEARCH_DETECTION"
        ImageProperties -> "IMAGE_PROPERTIES"
        CropHints -> "CROP_HINTS"
        WebDetection -> "WEB_DETECTION"

instance FromJSON FeatureType where
    parseJSON = parseJSONText "FeatureType"

instance ToJSON FeatureType where
    toJSON = toJSONText

-- | Face landmark type.
data LandmarkType
    = UnknownLandmark
      -- ^ @UNKNOWN_LANDMARK@
      -- Unknown face landmark detected. Should not be filled.
    | LeftEye
      -- ^ @LEFT_EYE@
      -- Left eye.
    | RightEye
      -- ^ @RIGHT_EYE@
      -- Right eye.
    | LeftOfLeftEyebrow
      -- ^ @LEFT_OF_LEFT_EYEBROW@
      -- Left of left eyebrow.
    | RightOfLeftEyebrow
      -- ^ @RIGHT_OF_LEFT_EYEBROW@
      -- Right of left eyebrow.
    | LeftOfRightEyebrow
      -- ^ @LEFT_OF_RIGHT_EYEBROW@
      -- Left of right eyebrow.
    | RightOfRightEyebrow
      -- ^ @RIGHT_OF_RIGHT_EYEBROW@
      -- Right of right eyebrow.
    | MidpointBetweenEyes
      -- ^ @MIDPOINT_BETWEEN_EYES@
      -- Midpoint between eyes.
    | NoseTip
      -- ^ @NOSE_TIP@
      -- Nose tip.
    | UpperLip
      -- ^ @UPPER_LIP@
      -- Upper lip.
    | LowerLip
      -- ^ @LOWER_LIP@
      -- Lower lip.
    | MouthLeft
      -- ^ @MOUTH_LEFT@
      -- Mouth left.
    | MouthRight
      -- ^ @MOUTH_RIGHT@
      -- Mouth right.
    | MouthCenter
      -- ^ @MOUTH_CENTER@
      -- Mouth center.
    | NoseBottomRight
      -- ^ @NOSE_BOTTOM_RIGHT@
      -- Nose, bottom right.
    | NoseBottomLeft
      -- ^ @NOSE_BOTTOM_LEFT@
      -- Nose, bottom left.
    | NoseBottomCenter
      -- ^ @NOSE_BOTTOM_CENTER@
      -- Nose, bottom center.
    | LeftEyeTopBoundary
      -- ^ @LEFT_EYE_TOP_BOUNDARY@
      -- Left eye, top boundary.
    | LeftEyeRightCorner
      -- ^ @LEFT_EYE_RIGHT_CORNER@
      -- Left eye, right corner.
    | LeftEyeBottomBoundary
      -- ^ @LEFT_EYE_BOTTOM_BOUNDARY@
      -- Left eye, bottom boundary.
    | LeftEyeLeftCorner
      -- ^ @LEFT_EYE_LEFT_CORNER@
      -- Left eye, left corner.
    | RightEyeTopBoundary
      -- ^ @RIGHT_EYE_TOP_BOUNDARY@
      -- Right eye, top boundary.
    | RightEyeRightCorner
      -- ^ @RIGHT_EYE_RIGHT_CORNER@
      -- Right eye, right corner.
    | RightEyeBottomBoundary
      -- ^ @RIGHT_EYE_BOTTOM_BOUNDARY@
      -- Right eye, bottom boundary.
    | RightEyeLeftCorner
      -- ^ @RIGHT_EYE_LEFT_CORNER@
      -- Right eye, left corner.
    | LeftEyebrowUpperMidpoint
      -- ^ @LEFT_EYEBROW_UPPER_MIDPOINT@
      -- Left eyebrow, upper midpoint.
    | RightEyebrowUpperMidpoint
      -- ^ @RIGHT_EYEBROW_UPPER_MIDPOINT@
      -- Right eyebrow, upper midpoint.
    | LeftEarTragion
      -- ^ @LEFT_EAR_TRAGION@
      -- Left ear tragion.
    | RightEarTragion
      -- ^ @RIGHT_EAR_TRAGION@
      -- Right ear tragion.
    | LeftEyePupil
      -- ^ @LEFT_EYE_PUPIL@
      -- Left eye pupil.
    | RightEyePupil
      -- ^ @RIGHT_EYE_PUPIL@
      -- Right eye pupil.
    | ForeheadGlabella
      -- ^ @FOREHEAD_GLABELLA@
      -- Forehead glabella.
    | ChinGnathion
      -- ^ @CHIN_GNATHION@
      -- Chin gnathion.
    | ChinLeftGonion
      -- ^ @CHIN_LEFT_GONION@
      -- Chin left gonion.
    | ChinRightGonion
      -- ^ @CHIN_RIGHT_GONION@
      -- Chin right gonion.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable LandmarkType

instance FromHttpApiData LandmarkType where
    parseQueryParam = \case
        "UNKNOWN_LANDMARK" -> Right UnknownLandmark
        "LEFT_EYE" -> Right LeftEye
        "RIGHT_EYE" -> Right RightEye
        "LEFT_OF_LEFT_EYEBROW" -> Right LeftOfLeftEyebrow
        "RIGHT_OF_LEFT_EYEBROW" -> Right RightOfLeftEyebrow
        "LEFT_OF_RIGHT_EYEBROW" -> Right LeftOfRightEyebrow
        "RIGHT_OF_RIGHT_EYEBROW" -> Right RightOfRightEyebrow
        "MIDPOINT_BETWEEN_EYES" -> Right MidpointBetweenEyes
        "NOSE_TIP" -> Right NoseTip
        "UPPER_LIP" -> Right UpperLip
        "LOWER_LIP" -> Right LowerLip
        "MOUTH_LEFT" -> Right MouthLeft
        "MOUTH_RIGHT" -> Right MouthRight
        "MOUTH_CENTER" -> Right MouthCenter
        "NOSE_BOTTOM_RIGHT" -> Right NoseBottomRight
        "NOSE_BOTTOM_LEFT" -> Right NoseBottomLeft
        "NOSE_BOTTOM_CENTER" -> Right NoseBottomCenter
        "LEFT_EYE_TOP_BOUNDARY" -> Right LeftEyeTopBoundary
        "LEFT_EYE_RIGHT_CORNER" -> Right LeftEyeRightCorner
        "LEFT_EYE_BOTTOM_BOUNDARY" -> Right LeftEyeBottomBoundary
        "LEFT_EYE_LEFT_CORNER" -> Right LeftEyeLeftCorner
        "RIGHT_EYE_TOP_BOUNDARY" -> Right RightEyeTopBoundary
        "RIGHT_EYE_RIGHT_CORNER" -> Right RightEyeRightCorner
        "RIGHT_EYE_BOTTOM_BOUNDARY" -> Right RightEyeBottomBoundary
        "RIGHT_EYE_LEFT_CORNER" -> Right RightEyeLeftCorner
        "LEFT_EYEBROW_UPPER_MIDPOINT" -> Right LeftEyebrowUpperMidpoint
        "RIGHT_EYEBROW_UPPER_MIDPOINT" -> Right RightEyebrowUpperMidpoint
        "LEFT_EAR_TRAGION" -> Right LeftEarTragion
        "RIGHT_EAR_TRAGION" -> Right RightEarTragion
        "LEFT_EYE_PUPIL" -> Right LeftEyePupil
        "RIGHT_EYE_PUPIL" -> Right RightEyePupil
        "FOREHEAD_GLABELLA" -> Right ForeheadGlabella
        "CHIN_GNATHION" -> Right ChinGnathion
        "CHIN_LEFT_GONION" -> Right ChinLeftGonion
        "CHIN_RIGHT_GONION" -> Right ChinRightGonion
        x -> Left ("Unable to parse LandmarkType from: " <> x)

instance ToHttpApiData LandmarkType where
    toQueryParam = \case
        UnknownLandmark -> "UNKNOWN_LANDMARK"
        LeftEye -> "LEFT_EYE"
        RightEye -> "RIGHT_EYE"
        LeftOfLeftEyebrow -> "LEFT_OF_LEFT_EYEBROW"
        RightOfLeftEyebrow -> "RIGHT_OF_LEFT_EYEBROW"
        LeftOfRightEyebrow -> "LEFT_OF_RIGHT_EYEBROW"
        RightOfRightEyebrow -> "RIGHT_OF_RIGHT_EYEBROW"
        MidpointBetweenEyes -> "MIDPOINT_BETWEEN_EYES"
        NoseTip -> "NOSE_TIP"
        UpperLip -> "UPPER_LIP"
        LowerLip -> "LOWER_LIP"
        MouthLeft -> "MOUTH_LEFT"
        MouthRight -> "MOUTH_RIGHT"
        MouthCenter -> "MOUTH_CENTER"
        NoseBottomRight -> "NOSE_BOTTOM_RIGHT"
        NoseBottomLeft -> "NOSE_BOTTOM_LEFT"
        NoseBottomCenter -> "NOSE_BOTTOM_CENTER"
        LeftEyeTopBoundary -> "LEFT_EYE_TOP_BOUNDARY"
        LeftEyeRightCorner -> "LEFT_EYE_RIGHT_CORNER"
        LeftEyeBottomBoundary -> "LEFT_EYE_BOTTOM_BOUNDARY"
        LeftEyeLeftCorner -> "LEFT_EYE_LEFT_CORNER"
        RightEyeTopBoundary -> "RIGHT_EYE_TOP_BOUNDARY"
        RightEyeRightCorner -> "RIGHT_EYE_RIGHT_CORNER"
        RightEyeBottomBoundary -> "RIGHT_EYE_BOTTOM_BOUNDARY"
        RightEyeLeftCorner -> "RIGHT_EYE_LEFT_CORNER"
        LeftEyebrowUpperMidpoint -> "LEFT_EYEBROW_UPPER_MIDPOINT"
        RightEyebrowUpperMidpoint -> "RIGHT_EYEBROW_UPPER_MIDPOINT"
        LeftEarTragion -> "LEFT_EAR_TRAGION"
        RightEarTragion -> "RIGHT_EAR_TRAGION"
        LeftEyePupil -> "LEFT_EYE_PUPIL"
        RightEyePupil -> "RIGHT_EYE_PUPIL"
        ForeheadGlabella -> "FOREHEAD_GLABELLA"
        ChinGnathion -> "CHIN_GNATHION"
        ChinLeftGonion -> "CHIN_LEFT_GONION"
        ChinRightGonion -> "CHIN_RIGHT_GONION"

instance FromJSON LandmarkType where
    parseJSON = parseJSONText "LandmarkType"

instance ToJSON LandmarkType where
    toJSON = toJSONText

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | Spoof likelihood. The likelihood that an modification was made to the
-- image\'s canonical version to make it appear funny or offensive.
data SafeSearchAnnotationSpoof
    = SSASUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | SSASVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | SSASUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | SSASPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | SSASLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | SSASVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SafeSearchAnnotationSpoof

instance FromHttpApiData SafeSearchAnnotationSpoof where
    parseQueryParam = \case
        "UNKNOWN" -> Right SSASUnknown
        "VERY_UNLIKELY" -> Right SSASVeryUnlikely
        "UNLIKELY" -> Right SSASUnlikely
        "POSSIBLE" -> Right SSASPossible
        "LIKELY" -> Right SSASLikely
        "VERY_LIKELY" -> Right SSASVeryLikely
        x -> Left ("Unable to parse SafeSearchAnnotationSpoof from: " <> x)

instance ToHttpApiData SafeSearchAnnotationSpoof where
    toQueryParam = \case
        SSASUnknown -> "UNKNOWN"
        SSASVeryUnlikely -> "VERY_UNLIKELY"
        SSASUnlikely -> "UNLIKELY"
        SSASPossible -> "POSSIBLE"
        SSASLikely -> "LIKELY"
        SSASVeryLikely -> "VERY_LIKELY"

instance FromJSON SafeSearchAnnotationSpoof where
    parseJSON = parseJSONText "SafeSearchAnnotationSpoof"

instance ToJSON SafeSearchAnnotationSpoof where
    toJSON = toJSONText

-- | Surprise likelihood.
data FaceAnnotationSurpriseLikelihood
    = FASLUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FASLVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FASLUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FASLPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FASLLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FASLVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationSurpriseLikelihood

instance FromHttpApiData FaceAnnotationSurpriseLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FASLUnknown
        "VERY_UNLIKELY" -> Right FASLVeryUnlikely
        "UNLIKELY" -> Right FASLUnlikely
        "POSSIBLE" -> Right FASLPossible
        "LIKELY" -> Right FASLLikely
        "VERY_LIKELY" -> Right FASLVeryLikely
        x -> Left ("Unable to parse FaceAnnotationSurpriseLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationSurpriseLikelihood where
    toQueryParam = \case
        FASLUnknown -> "UNKNOWN"
        FASLVeryUnlikely -> "VERY_UNLIKELY"
        FASLUnlikely -> "UNLIKELY"
        FASLPossible -> "POSSIBLE"
        FASLLikely -> "LIKELY"
        FASLVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationSurpriseLikelihood where
    parseJSON = parseJSONText "FaceAnnotationSurpriseLikelihood"

instance ToJSON FaceAnnotationSurpriseLikelihood where
    toJSON = toJSONText

-- | Sorrow likelihood.
data FaceAnnotationSorrowLikelihood
    = FUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationSorrowLikelihood

instance FromHttpApiData FaceAnnotationSorrowLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FUnknown
        "VERY_UNLIKELY" -> Right FVeryUnlikely
        "UNLIKELY" -> Right FUnlikely
        "POSSIBLE" -> Right FPossible
        "LIKELY" -> Right FLikely
        "VERY_LIKELY" -> Right FVeryLikely
        x -> Left ("Unable to parse FaceAnnotationSorrowLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationSorrowLikelihood where
    toQueryParam = \case
        FUnknown -> "UNKNOWN"
        FVeryUnlikely -> "VERY_UNLIKELY"
        FUnlikely -> "UNLIKELY"
        FPossible -> "POSSIBLE"
        FLikely -> "LIKELY"
        FVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationSorrowLikelihood where
    parseJSON = parseJSONText "FaceAnnotationSorrowLikelihood"

instance ToJSON FaceAnnotationSorrowLikelihood where
    toJSON = toJSONText

-- | Joy likelihood.
data FaceAnnotationJoyLikelihood
    = FAJLUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | FAJLVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | FAJLUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | FAJLPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | FAJLLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | FAJLVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FaceAnnotationJoyLikelihood

instance FromHttpApiData FaceAnnotationJoyLikelihood where
    parseQueryParam = \case
        "UNKNOWN" -> Right FAJLUnknown
        "VERY_UNLIKELY" -> Right FAJLVeryUnlikely
        "UNLIKELY" -> Right FAJLUnlikely
        "POSSIBLE" -> Right FAJLPossible
        "LIKELY" -> Right FAJLLikely
        "VERY_LIKELY" -> Right FAJLVeryLikely
        x -> Left ("Unable to parse FaceAnnotationJoyLikelihood from: " <> x)

instance ToHttpApiData FaceAnnotationJoyLikelihood where
    toQueryParam = \case
        FAJLUnknown -> "UNKNOWN"
        FAJLVeryUnlikely -> "VERY_UNLIKELY"
        FAJLUnlikely -> "UNLIKELY"
        FAJLPossible -> "POSSIBLE"
        FAJLLikely -> "LIKELY"
        FAJLVeryLikely -> "VERY_LIKELY"

instance FromJSON FaceAnnotationJoyLikelihood where
    parseJSON = parseJSONText "FaceAnnotationJoyLikelihood"

instance ToJSON FaceAnnotationJoyLikelihood where
    toJSON = toJSONText

-- | Detected break type.
data DetectedBreakType
    = DBTUnknown
      -- ^ @UNKNOWN@
      -- Unknown break label type.
    | DBTSpace
      -- ^ @SPACE@
      -- Regular space.
    | DBTSureSpace
      -- ^ @SURE_SPACE@
      -- Sure space (very wide).
    | DBTEolSureSpace
      -- ^ @EOL_SURE_SPACE@
      -- Line-wrapping break.
    | DBTHyphen
      -- ^ @HYPHEN@
      -- End-line hyphen that is not present in text; does not co-occur with
      -- \`SPACE\`, \`LEADER_SPACE\`, or \`LINE_BREAK\`.
    | DBTLineBreak
      -- ^ @LINE_BREAK@
      -- Line break that ends a paragraph.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable DetectedBreakType

instance FromHttpApiData DetectedBreakType where
    parseQueryParam = \case
        "UNKNOWN" -> Right DBTUnknown
        "SPACE" -> Right DBTSpace
        "SURE_SPACE" -> Right DBTSureSpace
        "EOL_SURE_SPACE" -> Right DBTEolSureSpace
        "HYPHEN" -> Right DBTHyphen
        "LINE_BREAK" -> Right DBTLineBreak
        x -> Left ("Unable to parse DetectedBreakType from: " <> x)

instance ToHttpApiData DetectedBreakType where
    toQueryParam = \case
        DBTUnknown -> "UNKNOWN"
        DBTSpace -> "SPACE"
        DBTSureSpace -> "SURE_SPACE"
        DBTEolSureSpace -> "EOL_SURE_SPACE"
        DBTHyphen -> "HYPHEN"
        DBTLineBreak -> "LINE_BREAK"

instance FromJSON DetectedBreakType where
    parseJSON = parseJSONText "DetectedBreakType"

instance ToJSON DetectedBreakType where
    toJSON = toJSONText

-- | Likelihood that the request image contains racy content. Racy content
-- may include (but is not limited to) skimpy or sheer clothing,
-- strategically covered nudity, lewd or provocative poses, or close-ups of
-- sensitive body areas.
data SafeSearchAnnotationRacy
    = SSARUnknown
      -- ^ @UNKNOWN@
      -- Unknown likelihood.
    | SSARVeryUnlikely
      -- ^ @VERY_UNLIKELY@
      -- It is very unlikely that the image belongs to the specified vertical.
    | SSARUnlikely
      -- ^ @UNLIKELY@
      -- It is unlikely that the image belongs to the specified vertical.
    | SSARPossible
      -- ^ @POSSIBLE@
      -- It is possible that the image belongs to the specified vertical.
    | SSARLikely
      -- ^ @LIKELY@
      -- It is likely that the image belongs to the specified vertical.
    | SSARVeryLikely
      -- ^ @VERY_LIKELY@
      -- It is very likely that the image belongs to the specified vertical.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SafeSearchAnnotationRacy

instance FromHttpApiData SafeSearchAnnotationRacy where
    parseQueryParam = \case
        "UNKNOWN" -> Right SSARUnknown
        "VERY_UNLIKELY" -> Right SSARVeryUnlikely
        "UNLIKELY" -> Right SSARUnlikely
        "POSSIBLE" -> Right SSARPossible
        "LIKELY" -> Right SSARLikely
        "VERY_LIKELY" -> Right SSARVeryLikely
        x -> Left ("Unable to parse SafeSearchAnnotationRacy from: " <> x)

instance ToHttpApiData SafeSearchAnnotationRacy where
    toQueryParam = \case
        SSARUnknown -> "UNKNOWN"
        SSARVeryUnlikely -> "VERY_UNLIKELY"
        SSARUnlikely -> "UNLIKELY"
        SSARPossible -> "POSSIBLE"
        SSARLikely -> "LIKELY"
        SSARVeryLikely -> "VERY_LIKELY"

instance FromJSON SafeSearchAnnotationRacy where
    parseJSON = parseJSONText "SafeSearchAnnotationRacy"

instance ToJSON SafeSearchAnnotationRacy where
    toJSON = toJSONText
