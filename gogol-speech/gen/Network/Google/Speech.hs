{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Speech
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Converts audio to text by applying powerful neural network models.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference>
module Network.Google.Speech
    (
    -- * Service Configuration
      speechService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , SpeechAPI

    -- * Resources

    -- ** speech.operations.get
    , module Network.Google.Resource.Speech.Operations.Get

    -- ** speech.speech.longrunningrecognize
    , module Network.Google.Resource.Speech.Speech.Longrunningrecognize

    -- ** speech.speech.recognize
    , module Network.Google.Resource.Speech.Speech.Recognize

    -- * Types

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** SpeechContext
    , SpeechContext
    , speechContext
    , scPhrases

    -- ** RecognizeRequest
    , RecognizeRequest
    , recognizeRequest
    , rrConfig
    , rrAudio

    -- ** Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- ** SpeechRecognitionAlternative
    , SpeechRecognitionAlternative
    , speechRecognitionAlternative
    , sraConfidence
    , sraWords
    , sraTranscript

    -- ** WordInfo
    , WordInfo
    , wordInfo
    , wiStartTime
    , wiEndTime
    , wiWord

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** SpeechRecognitionResult
    , SpeechRecognitionResult
    , speechRecognitionResult
    , srrAlternatives

    -- ** RecognitionAudio
    , RecognitionAudio
    , recognitionAudio
    , raURI
    , raContent

    -- ** RecognizeResponse
    , RecognizeResponse
    , recognizeResponse
    , rrResults

    -- ** Xgafv
    , Xgafv (..)

    -- ** RecognitionConfig
    , RecognitionConfig
    , recognitionConfig
    , rcEnableWordTimeOffSets
    , rcSpeechContexts
    , rcLanguageCode
    , rcSampleRateHertz
    , rcMaxAlternatives
    , rcProfanityFilter
    , rcEncoding

    -- ** LongRunningRecognizeRequest
    , LongRunningRecognizeRequest
    , longRunningRecognizeRequest
    , lrrrConfig
    , lrrrAudio

    -- ** OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- ** RecognitionConfigEncoding
    , RecognitionConfigEncoding (..)

    -- ** OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional
    ) where

import Network.Google.Prelude
import Network.Google.Resource.Speech.Operations.Get
import Network.Google.Resource.Speech.Speech.Longrunningrecognize
import Network.Google.Resource.Speech.Speech.Recognize
import Network.Google.Speech.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Speech API service.
type SpeechAPI =
     SpeechRecognizeResource :<|>
       SpeechLongrunningrecognizeResource
       :<|> OperationsGetResource
