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
-- Module      : Network.Google.Resource.Genomics.Pipelines.Run
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a pipeline. **Note:** In order to use this method, the Genomics
-- Service Agent must have access to your project. This is done
-- automatically when the Genomics API is first enabled, but if you delete
-- this permission, or if you have already enabled the Genomics API prior
-- to the launch of the v2alpha1 API, you must disable and re-enable the
-- API to grant the Genomics Service Agent the required permissions.
--
-- /See:/ <https://cloud.google.com/genomics Genomics API Reference> for @genomics.pipelines.run@.
module Network.Google.Resource.Genomics.Pipelines.Run
    (
    -- * REST Resource
      PipelinesRunResource

    -- * Creating a Request
    , pipelinesRun
    , PipelinesRun

    -- * Request Lenses
    , prXgafv
    , prUploadProtocol
    , prPp
    , prAccessToken
    , prUploadType
    , prPayload
    , prBearerToken
    , prCallback
    ) where

import Network.Google.Genomics.Types
import Network.Google.Prelude

-- | A resource alias for @genomics.pipelines.run@ method which the
-- 'PipelinesRun' request conforms to.
type PipelinesRunResource =
     "v2alpha1" :>
       "pipelines:run" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] RunPipelineRequest :>
                           Post '[JSON] Operation

-- | Runs a pipeline. **Note:** In order to use this method, the Genomics
-- Service Agent must have access to your project. This is done
-- automatically when the Genomics API is first enabled, but if you delete
-- this permission, or if you have already enabled the Genomics API prior
-- to the launch of the v2alpha1 API, you must disable and re-enable the
-- API to grant the Genomics Service Agent the required permissions.
--
-- /See:/ 'pipelinesRun' smart constructor.
data PipelinesRun = PipelinesRun'
    { _prXgafv :: !(Maybe Xgafv)
    , _prUploadProtocol :: !(Maybe Text)
    , _prPp :: !Bool
    , _prAccessToken :: !(Maybe Text)
    , _prUploadType :: !(Maybe Text)
    , _prPayload :: !RunPipelineRequest
    , _prBearerToken :: !(Maybe Text)
    , _prCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PipelinesRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prXgafv'
--
-- * 'prUploadProtocol'
--
-- * 'prPp'
--
-- * 'prAccessToken'
--
-- * 'prUploadType'
--
-- * 'prPayload'
--
-- * 'prBearerToken'
--
-- * 'prCallback'
pipelinesRun
    :: RunPipelineRequest -- ^ 'prPayload'
    -> PipelinesRun
pipelinesRun pPrPayload_ = 
    PipelinesRun'
    { _prXgafv = Nothing
    , _prUploadProtocol = Nothing
    , _prPp = True
    , _prAccessToken = Nothing
    , _prUploadType = Nothing
    , _prPayload = pPrPayload_
    , _prBearerToken = Nothing
    , _prCallback = Nothing
    }

-- | V1 error format.
prXgafv :: Lens' PipelinesRun (Maybe Xgafv)
prXgafv = lens _prXgafv (\ s a -> s{_prXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prUploadProtocol :: Lens' PipelinesRun (Maybe Text)
prUploadProtocol
  = lens _prUploadProtocol
      (\ s a -> s{_prUploadProtocol = a})

-- | Pretty-print response.
prPp :: Lens' PipelinesRun Bool
prPp = lens _prPp (\ s a -> s{_prPp = a})

-- | OAuth access token.
prAccessToken :: Lens' PipelinesRun (Maybe Text)
prAccessToken
  = lens _prAccessToken
      (\ s a -> s{_prAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prUploadType :: Lens' PipelinesRun (Maybe Text)
prUploadType
  = lens _prUploadType (\ s a -> s{_prUploadType = a})

-- | Multipart request metadata.
prPayload :: Lens' PipelinesRun RunPipelineRequest
prPayload
  = lens _prPayload (\ s a -> s{_prPayload = a})

-- | OAuth bearer token.
prBearerToken :: Lens' PipelinesRun (Maybe Text)
prBearerToken
  = lens _prBearerToken
      (\ s a -> s{_prBearerToken = a})

-- | JSONP
prCallback :: Lens' PipelinesRun (Maybe Text)
prCallback
  = lens _prCallback (\ s a -> s{_prCallback = a})

instance GoogleRequest PipelinesRun where
        type Rs PipelinesRun = Operation
        type Scopes PipelinesRun =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/genomics"]
        requestClient PipelinesRun'{..}
          = go _prXgafv _prUploadProtocol (Just _prPp)
              _prAccessToken
              _prUploadType
              _prBearerToken
              _prCallback
              (Just AltJSON)
              _prPayload
              genomicsService
          where go
                  = buildClient (Proxy :: Proxy PipelinesRunResource)
                      mempty
