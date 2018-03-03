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
-- Module      : Network.Google.Resource.Content.ShippingSettings.Custombatch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves and updates the shipping settings of multiple accounts in a
-- single request.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.shippingsettings.custombatch@.
module Network.Google.Resource.Content.ShippingSettings.Custombatch
    (
    -- * REST Resource
      ShippingSettingsCustombatchResource

    -- * Creating a Request
    , shippingSettingsCustombatch
    , ShippingSettingsCustombatch

    -- * Request Lenses
    , sscPayload
    , sscDryRun
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.shippingsettings.custombatch@ method which the
-- 'ShippingSettingsCustombatch' request conforms to.
type ShippingSettingsCustombatchResource =
     "content" :>
       "v2" :>
         "shippingsettings" :>
           "batch" :>
             QueryParam "dryRun" Bool :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] ShippingSettingsCustomBatchRequest :>
                   Post '[JSON] ShippingSettingsCustomBatchResponse

-- | Retrieves and updates the shipping settings of multiple accounts in a
-- single request.
--
-- /See:/ 'shippingSettingsCustombatch' smart constructor.
data ShippingSettingsCustombatch = ShippingSettingsCustombatch'
    { _sscPayload :: !ShippingSettingsCustomBatchRequest
    , _sscDryRun :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsCustombatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscPayload'
--
-- * 'sscDryRun'
shippingSettingsCustombatch
    :: ShippingSettingsCustomBatchRequest -- ^ 'sscPayload'
    -> ShippingSettingsCustombatch
shippingSettingsCustombatch pSscPayload_ = 
    ShippingSettingsCustombatch'
    { _sscPayload = pSscPayload_
    , _sscDryRun = Nothing
    }

-- | Multipart request metadata.
sscPayload :: Lens' ShippingSettingsCustombatch ShippingSettingsCustomBatchRequest
sscPayload
  = lens _sscPayload (\ s a -> s{_sscPayload = a})

-- | Flag to run the request in dry-run mode.
sscDryRun :: Lens' ShippingSettingsCustombatch (Maybe Bool)
sscDryRun
  = lens _sscDryRun (\ s a -> s{_sscDryRun = a})

instance GoogleRequest ShippingSettingsCustombatch
         where
        type Rs ShippingSettingsCustombatch =
             ShippingSettingsCustomBatchResponse
        type Scopes ShippingSettingsCustombatch =
             '["https://www.googleapis.com/auth/content"]
        requestClient ShippingSettingsCustombatch'{..}
          = go _sscDryRun (Just AltJSON) _sscPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy ShippingSettingsCustombatchResource)
                      mempty
