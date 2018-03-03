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
-- Module      : Network.Google.Resource.Content.Orders.Cancel
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels all line items in an order, making a full refund.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.cancel@.
module Network.Google.Resource.Content.Orders.Cancel
    (
    -- * REST Resource
      OrdersCancelResource

    -- * Creating a Request
    , ordersCancel
    , OrdersCancel

    -- * Request Lenses
    , ooMerchantId
    , ooPayload
    , ooOrderId
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.cancel@ method which the
-- 'OrdersCancel' request conforms to.
type OrdersCancelResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "orders" :>
             Capture "orderId" Text :>
               "cancel" :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] OrdersCancelRequest :>
                     Post '[JSON] OrdersCancelResponse

-- | Cancels all line items in an order, making a full refund.
--
-- /See:/ 'ordersCancel' smart constructor.
data OrdersCancel = OrdersCancel'
    { _ooMerchantId :: !(Textual Word64)
    , _ooPayload :: !OrdersCancelRequest
    , _ooOrderId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ooMerchantId'
--
-- * 'ooPayload'
--
-- * 'ooOrderId'
ordersCancel
    :: Word64 -- ^ 'ooMerchantId'
    -> OrdersCancelRequest -- ^ 'ooPayload'
    -> Text -- ^ 'ooOrderId'
    -> OrdersCancel
ordersCancel pOoMerchantId_ pOoPayload_ pOoOrderId_ = 
    OrdersCancel'
    { _ooMerchantId = _Coerce # pOoMerchantId_
    , _ooPayload = pOoPayload_
    , _ooOrderId = pOoOrderId_
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
ooMerchantId :: Lens' OrdersCancel Word64
ooMerchantId
  = lens _ooMerchantId (\ s a -> s{_ooMerchantId = a})
      . _Coerce

-- | Multipart request metadata.
ooPayload :: Lens' OrdersCancel OrdersCancelRequest
ooPayload
  = lens _ooPayload (\ s a -> s{_ooPayload = a})

-- | The ID of the order to cancel.
ooOrderId :: Lens' OrdersCancel Text
ooOrderId
  = lens _ooOrderId (\ s a -> s{_ooOrderId = a})

instance GoogleRequest OrdersCancel where
        type Rs OrdersCancel = OrdersCancelResponse
        type Scopes OrdersCancel =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersCancel'{..}
          = go _ooMerchantId _ooOrderId (Just AltJSON)
              _ooPayload
              shoppingContentService
          where go
                  = buildClient (Proxy :: Proxy OrdersCancelResource)
                      mempty
