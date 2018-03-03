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
-- Module      : Network.Google.Resource.Content.Orders.Returnlineitem
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a line item.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.returnlineitem@.
module Network.Google.Resource.Content.Orders.Returnlineitem
    (
    -- * REST Resource
      OrdersReturnlineitemResource

    -- * Creating a Request
    , ordersReturnlineitem
    , OrdersReturnlineitem

    -- * Request Lenses
    , ordMerchantId
    , ordPayload
    , ordOrderId
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.returnlineitem@ method which the
-- 'OrdersReturnlineitem' request conforms to.
type OrdersReturnlineitemResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "orders" :>
             Capture "orderId" Text :>
               "returnLineItem" :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] OrdersReturnLineItemRequest :>
                     Post '[JSON] OrdersReturnLineItemResponse

-- | Returns a line item.
--
-- /See:/ 'ordersReturnlineitem' smart constructor.
data OrdersReturnlineitem = OrdersReturnlineitem'
    { _ordMerchantId :: !(Textual Word64)
    , _ordPayload :: !OrdersReturnLineItemRequest
    , _ordOrderId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnlineitem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ordMerchantId'
--
-- * 'ordPayload'
--
-- * 'ordOrderId'
ordersReturnlineitem
    :: Word64 -- ^ 'ordMerchantId'
    -> OrdersReturnLineItemRequest -- ^ 'ordPayload'
    -> Text -- ^ 'ordOrderId'
    -> OrdersReturnlineitem
ordersReturnlineitem pOrdMerchantId_ pOrdPayload_ pOrdOrderId_ = 
    OrdersReturnlineitem'
    { _ordMerchantId = _Coerce # pOrdMerchantId_
    , _ordPayload = pOrdPayload_
    , _ordOrderId = pOrdOrderId_
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
ordMerchantId :: Lens' OrdersReturnlineitem Word64
ordMerchantId
  = lens _ordMerchantId
      (\ s a -> s{_ordMerchantId = a})
      . _Coerce

-- | Multipart request metadata.
ordPayload :: Lens' OrdersReturnlineitem OrdersReturnLineItemRequest
ordPayload
  = lens _ordPayload (\ s a -> s{_ordPayload = a})

-- | The ID of the order.
ordOrderId :: Lens' OrdersReturnlineitem Text
ordOrderId
  = lens _ordOrderId (\ s a -> s{_ordOrderId = a})

instance GoogleRequest OrdersReturnlineitem where
        type Rs OrdersReturnlineitem =
             OrdersReturnLineItemResponse
        type Scopes OrdersReturnlineitem =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersReturnlineitem'{..}
          = go _ordMerchantId _ordOrderId (Just AltJSON)
              _ordPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy OrdersReturnlineitemResource)
                      mempty
