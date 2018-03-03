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
-- Module      : Network.Google.Resource.Content.Datafeeds.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a datafeed configuration from your Merchant Center account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.datafeeds.get@.
module Network.Google.Resource.Content.Datafeeds.Get
    (
    -- * REST Resource
      DatafeedsGetResource

    -- * Creating a Request
    , datafeedsGet
    , DatafeedsGet

    -- * Request Lenses
    , dgMerchantId
    , dgDatafeedId
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.datafeeds.get@ method which the
-- 'DatafeedsGet' request conforms to.
type DatafeedsGetResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "datafeeds" :>
             Capture "datafeedId" (Textual Word64) :>
               QueryParam "alt" AltJSON :> Get '[JSON] Datafeed

-- | Retrieves a datafeed configuration from your Merchant Center account.
--
-- /See:/ 'datafeedsGet' smart constructor.
data DatafeedsGet = DatafeedsGet'
    { _dgMerchantId :: !(Textual Word64)
    , _dgDatafeedId :: !(Textual Word64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgMerchantId'
--
-- * 'dgDatafeedId'
datafeedsGet
    :: Word64 -- ^ 'dgMerchantId'
    -> Word64 -- ^ 'dgDatafeedId'
    -> DatafeedsGet
datafeedsGet pDgMerchantId_ pDgDatafeedId_ = 
    DatafeedsGet'
    { _dgMerchantId = _Coerce # pDgMerchantId_
    , _dgDatafeedId = _Coerce # pDgDatafeedId_
    }

-- | The ID of the account that manages the datafeed. This account cannot be
-- a multi-client account.
dgMerchantId :: Lens' DatafeedsGet Word64
dgMerchantId
  = lens _dgMerchantId (\ s a -> s{_dgMerchantId = a})
      . _Coerce

-- | The ID of the datafeed.
dgDatafeedId :: Lens' DatafeedsGet Word64
dgDatafeedId
  = lens _dgDatafeedId (\ s a -> s{_dgDatafeedId = a})
      . _Coerce

instance GoogleRequest DatafeedsGet where
        type Rs DatafeedsGet = Datafeed
        type Scopes DatafeedsGet =
             '["https://www.googleapis.com/auth/content"]
        requestClient DatafeedsGet'{..}
          = go _dgMerchantId _dgDatafeedId (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient (Proxy :: Proxy DatafeedsGetResource)
                      mempty
