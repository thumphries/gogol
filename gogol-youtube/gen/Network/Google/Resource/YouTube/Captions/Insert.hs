{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.YouTube.Captions.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a caption track.
--
-- /See:/ <https://developers.google.com/youtube/v3 YouTube Data API Reference> for @youtube.captions.insert@.
module Network.Google.Resource.YouTube.Captions.Insert
    (
    -- * REST Resource
      CaptionsInsertResource

    -- * Creating a Request
    , captionsInsert
    , CaptionsInsert

    -- * Request Lenses
    , ciOnBehalfOf
    , ciPart
    , ciPayload
    , ciOnBehalfOfContentOwner
    , ciSync
    ) where

import Network.Google.Prelude
import Network.Google.YouTube.Types

-- | A resource alias for @youtube.captions.insert@ method which the
-- 'CaptionsInsert' request conforms to.
type CaptionsInsertResource =
     "youtube" :>
       "v3" :>
         "captions" :>
           QueryParam "part" Text :>
             QueryParam "onBehalfOf" Text :>
               QueryParam "onBehalfOfContentOwner" Text :>
                 QueryParam "sync" Bool :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] Caption :> Post '[JSON] Caption
       :<|>
       "upload" :>
         "youtube" :>
           "v3" :>
             "captions" :>
               QueryParam "part" Text :>
                 QueryParam "onBehalfOf" Text :>
                   QueryParam "onBehalfOfContentOwner" Text :>
                     QueryParam "sync" Bool :>
                       QueryParam "alt" AltJSON :>
                         QueryParam "uploadType" Multipart :>
                           MultipartRelated '[JSON] Caption :>
                             Post '[JSON] Caption

-- | Uploads a caption track.
--
-- /See:/ 'captionsInsert' smart constructor.
data CaptionsInsert = CaptionsInsert'
    { _ciOnBehalfOf :: !(Maybe Text)
    , _ciPart :: !Text
    , _ciPayload :: !Caption
    , _ciOnBehalfOfContentOwner :: !(Maybe Text)
    , _ciSync :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CaptionsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciOnBehalfOf'
--
-- * 'ciPart'
--
-- * 'ciPayload'
--
-- * 'ciOnBehalfOfContentOwner'
--
-- * 'ciSync'
captionsInsert
    :: Text -- ^ 'ciPart'
    -> Caption -- ^ 'ciPayload'
    -> CaptionsInsert
captionsInsert pCiPart_ pCiPayload_ = 
    CaptionsInsert'
    { _ciOnBehalfOf = Nothing
    , _ciPart = pCiPart_
    , _ciPayload = pCiPayload_
    , _ciOnBehalfOfContentOwner = Nothing
    , _ciSync = Nothing
    }

-- | ID of the Google+ Page for the channel that the request is be on behalf
-- of
ciOnBehalfOf :: Lens' CaptionsInsert (Maybe Text)
ciOnBehalfOf
  = lens _ciOnBehalfOf (\ s a -> s{_ciOnBehalfOf = a})

-- | The part parameter specifies the caption resource parts that the API
-- response will include. Set the parameter value to snippet.
ciPart :: Lens' CaptionsInsert Text
ciPart = lens _ciPart (\ s a -> s{_ciPart = a})

-- | Multipart request metadata.
ciPayload :: Lens' CaptionsInsert Caption
ciPayload
  = lens _ciPayload (\ s a -> s{_ciPayload = a})

-- | Note: This parameter is intended exclusively for YouTube content
-- partners. The onBehalfOfContentOwner parameter indicates that the
-- request\'s authorization credentials identify a YouTube CMS user who is
-- acting on behalf of the content owner specified in the parameter value.
-- This parameter is intended for YouTube content partners that own and
-- manage many different YouTube channels. It allows content owners to
-- authenticate once and get access to all their video and channel data,
-- without having to provide authentication credentials for each individual
-- channel. The actual CMS account that the user authenticates with must be
-- linked to the specified YouTube content owner.
ciOnBehalfOfContentOwner :: Lens' CaptionsInsert (Maybe Text)
ciOnBehalfOfContentOwner
  = lens _ciOnBehalfOfContentOwner
      (\ s a -> s{_ciOnBehalfOfContentOwner = a})

-- | The sync parameter indicates whether YouTube should automatically
-- synchronize the caption file with the audio track of the video. If you
-- set the value to true, YouTube will disregard any time codes that are in
-- the uploaded caption file and generate new time codes for the captions.
-- You should set the sync parameter to true if you are uploading a
-- transcript, which has no time codes, or if you suspect the time codes in
-- your file are incorrect and want YouTube to try to fix them.
ciSync :: Lens' CaptionsInsert (Maybe Bool)
ciSync = lens _ciSync (\ s a -> s{_ciSync = a})

instance GoogleRequest CaptionsInsert where
        type Rs CaptionsInsert = Caption
        type Scopes CaptionsInsert =
             '["https://www.googleapis.com/auth/youtube.force-ssl",
               "https://www.googleapis.com/auth/youtubepartner"]
        requestClient CaptionsInsert'{..}
          = go (Just _ciPart) _ciOnBehalfOf
              _ciOnBehalfOfContentOwner
              _ciSync
              (Just AltJSON)
              _ciPayload
              youTubeService
          where go :<|> _
                  = buildClient (Proxy :: Proxy CaptionsInsertResource)
                      mempty

instance GoogleRequest (MediaUpload CaptionsInsert)
         where
        type Rs (MediaUpload CaptionsInsert) = Caption
        type Scopes (MediaUpload CaptionsInsert) =
             Scopes CaptionsInsert
        requestClient (MediaUpload CaptionsInsert'{..} body)
          = go (Just _ciPart) _ciOnBehalfOf
              _ciOnBehalfOfContentOwner
              _ciSync
              (Just AltJSON)
              (Just Multipart)
              _ciPayload
              body
              youTubeService
          where _ :<|> go
                  = buildClient (Proxy :: Proxy CaptionsInsertResource)
                      mempty
