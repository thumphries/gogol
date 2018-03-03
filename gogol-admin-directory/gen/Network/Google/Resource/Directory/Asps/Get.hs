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
-- Module      : Network.Google.Resource.Directory.Asps.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an ASP issued by a user.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.asps.get@.
module Network.Google.Resource.Directory.Asps.Get
    (
    -- * REST Resource
      AspsGetResource

    -- * Creating a Request
    , aspsGet
    , AspsGet

    -- * Request Lenses
    , agCodeId
    , agUserKey
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.asps.get@ method which the
-- 'AspsGet' request conforms to.
type AspsGetResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "users" :>
             Capture "userKey" Text :>
               "asps" :>
                 Capture "codeId" (Textual Int32) :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Asp

-- | Get information about an ASP issued by a user.
--
-- /See:/ 'aspsGet' smart constructor.
data AspsGet = AspsGet'
    { _agCodeId :: !(Textual Int32)
    , _agUserKey :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AspsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agCodeId'
--
-- * 'agUserKey'
aspsGet
    :: Int32 -- ^ 'agCodeId'
    -> Text -- ^ 'agUserKey'
    -> AspsGet
aspsGet pAgCodeId_ pAgUserKey_ = 
    AspsGet'
    { _agCodeId = _Coerce # pAgCodeId_
    , _agUserKey = pAgUserKey_
    }

-- | The unique ID of the ASP.
agCodeId :: Lens' AspsGet Int32
agCodeId
  = lens _agCodeId (\ s a -> s{_agCodeId = a}) .
      _Coerce

-- | Identifies the user in the API request. The value can be the user\'s
-- primary email address, alias email address, or unique user ID.
agUserKey :: Lens' AspsGet Text
agUserKey
  = lens _agUserKey (\ s a -> s{_agUserKey = a})

instance GoogleRequest AspsGet where
        type Rs AspsGet = Asp
        type Scopes AspsGet =
             '["https://www.googleapis.com/auth/admin.directory.user.security"]
        requestClient AspsGet'{..}
          = go _agUserKey _agCodeId (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy AspsGetResource) mempty
