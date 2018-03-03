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
-- Module      : Network.Google.Resource.Content.Pos.Custombatch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batches multiple POS-related calls in a single request.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.pos.custombatch@.
module Network.Google.Resource.Content.Pos.Custombatch
    (
    -- * REST Resource
      PosCustombatchResource

    -- * Creating a Request
    , posCustombatch
    , PosCustombatch

    -- * Request Lenses
    , pPayload
    , pDryRun
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.pos.custombatch@ method which the
-- 'PosCustombatch' request conforms to.
type PosCustombatchResource =
     "content" :>
       "v2" :>
         "pos" :>
           "batch" :>
             QueryParam "dryRun" Bool :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] PosCustomBatchRequest :>
                   Post '[JSON] PosCustomBatchResponse

-- | Batches multiple POS-related calls in a single request.
--
-- /See:/ 'posCustombatch' smart constructor.
data PosCustombatch = PosCustombatch'
    { _pPayload :: !PosCustomBatchRequest
    , _pDryRun :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PosCustombatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPayload'
--
-- * 'pDryRun'
posCustombatch
    :: PosCustomBatchRequest -- ^ 'pPayload'
    -> PosCustombatch
posCustombatch pPPayload_ = 
    PosCustombatch'
    { _pPayload = pPPayload_
    , _pDryRun = Nothing
    }

-- | Multipart request metadata.
pPayload :: Lens' PosCustombatch PosCustomBatchRequest
pPayload = lens _pPayload (\ s a -> s{_pPayload = a})

-- | Flag to run the request in dry-run mode.
pDryRun :: Lens' PosCustombatch (Maybe Bool)
pDryRun = lens _pDryRun (\ s a -> s{_pDryRun = a})

instance GoogleRequest PosCustombatch where
        type Rs PosCustombatch = PosCustomBatchResponse
        type Scopes PosCustombatch =
             '["https://www.googleapis.com/auth/content"]
        requestClient PosCustombatch'{..}
          = go _pDryRun (Just AltJSON) _pPayload
              shoppingContentService
          where go
                  = buildClient (Proxy :: Proxy PosCustombatchResource)
                      mempty
