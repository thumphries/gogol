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
-- Module      : Network.Google.Resource.Content.Productstatuses.Custombatch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the statuses of multiple products in a single request.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.productstatuses.custombatch@.
module Network.Google.Resource.Content.Productstatuses.Custombatch
    (
    -- * REST Resource
      ProductstatusesCustombatchResource

    -- * Creating a Request
    , productstatusesCustombatch
    , ProductstatusesCustombatch

    -- * Request Lenses
    , proPayload
    , proIncludeAttributes
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.productstatuses.custombatch@ method which the
-- 'ProductstatusesCustombatch' request conforms to.
type ProductstatusesCustombatchResource =
     "content" :>
       "v2" :>
         "productstatuses" :>
           "batch" :>
             QueryParam "includeAttributes" Bool :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] ProductstatusesCustomBatchRequest :>
                   Post '[JSON] ProductstatusesCustomBatchResponse

-- | Gets the statuses of multiple products in a single request.
--
-- /See:/ 'productstatusesCustombatch' smart constructor.
data ProductstatusesCustombatch = ProductstatusesCustombatch'
    { _proPayload :: !ProductstatusesCustomBatchRequest
    , _proIncludeAttributes :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesCustombatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proPayload'
--
-- * 'proIncludeAttributes'
productstatusesCustombatch
    :: ProductstatusesCustomBatchRequest -- ^ 'proPayload'
    -> ProductstatusesCustombatch
productstatusesCustombatch pProPayload_ = 
    ProductstatusesCustombatch'
    { _proPayload = pProPayload_
    , _proIncludeAttributes = Nothing
    }

-- | Multipart request metadata.
proPayload :: Lens' ProductstatusesCustombatch ProductstatusesCustomBatchRequest
proPayload
  = lens _proPayload (\ s a -> s{_proPayload = a})

-- | Flag to include full product data in the results of this request. The
-- default value is false.
proIncludeAttributes :: Lens' ProductstatusesCustombatch (Maybe Bool)
proIncludeAttributes
  = lens _proIncludeAttributes
      (\ s a -> s{_proIncludeAttributes = a})

instance GoogleRequest ProductstatusesCustombatch
         where
        type Rs ProductstatusesCustombatch =
             ProductstatusesCustomBatchResponse
        type Scopes ProductstatusesCustombatch =
             '["https://www.googleapis.com/auth/content"]
        requestClient ProductstatusesCustombatch'{..}
          = go _proIncludeAttributes (Just AltJSON) _proPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy ProductstatusesCustombatchResource)
                      mempty
