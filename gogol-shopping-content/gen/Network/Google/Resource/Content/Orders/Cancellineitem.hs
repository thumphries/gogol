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
-- Module      : Network.Google.Resource.Content.Orders.Cancellineitem
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a line item, making a full refund.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.cancellineitem@.
module Network.Google.Resource.Content.Orders.Cancellineitem
    (
    -- * REST Resource
      OrdersCancellineitemResource

    -- * Creating a Request
    , ordersCancellineitem
    , OrdersCancellineitem

    -- * Request Lenses
    , occMerchantId
    , occPayload
    , occOrderId
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.cancellineitem@ method which the
-- 'OrdersCancellineitem' request conforms to.
type OrdersCancellineitemResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "orders" :>
             Capture "orderId" Text :>
               "cancelLineItem" :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] OrdersCancelLineItemRequest :>
                     Post '[JSON] OrdersCancelLineItemResponse

-- | Cancels a line item, making a full refund.
--
-- /See:/ 'ordersCancellineitem' smart constructor.
data OrdersCancellineitem = OrdersCancellineitem'
    { _occMerchantId :: !(Textual Word64)
    , _occPayload :: !OrdersCancelLineItemRequest
    , _occOrderId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancellineitem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'occMerchantId'
--
-- * 'occPayload'
--
-- * 'occOrderId'
ordersCancellineitem
    :: Word64 -- ^ 'occMerchantId'
    -> OrdersCancelLineItemRequest -- ^ 'occPayload'
    -> Text -- ^ 'occOrderId'
    -> OrdersCancellineitem
ordersCancellineitem pOccMerchantId_ pOccPayload_ pOccOrderId_ = 
    OrdersCancellineitem'
    { _occMerchantId = _Coerce # pOccMerchantId_
    , _occPayload = pOccPayload_
    , _occOrderId = pOccOrderId_
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
occMerchantId :: Lens' OrdersCancellineitem Word64
occMerchantId
  = lens _occMerchantId
      (\ s a -> s{_occMerchantId = a})
      . _Coerce

-- | Multipart request metadata.
occPayload :: Lens' OrdersCancellineitem OrdersCancelLineItemRequest
occPayload
  = lens _occPayload (\ s a -> s{_occPayload = a})

-- | The ID of the order.
occOrderId :: Lens' OrdersCancellineitem Text
occOrderId
  = lens _occOrderId (\ s a -> s{_occOrderId = a})

instance GoogleRequest OrdersCancellineitem where
        type Rs OrdersCancellineitem =
             OrdersCancelLineItemResponse
        type Scopes OrdersCancellineitem =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersCancellineitem'{..}
          = go _occMerchantId _occOrderId (Just AltJSON)
              _occPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy OrdersCancellineitemResource)
                      mempty
