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
-- Module      : Network.Google.Resource.AndroidEnterprise.Users.SetAvailableProductSet
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of products that a user is entitled to access (referred
-- to as whitelisted products). Only products that are approved or products
-- that were previously approved (products with revoked approval) can be
-- whitelisted.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.users.setAvailableProductSet@.
module Network.Google.Resource.AndroidEnterprise.Users.SetAvailableProductSet
    (
    -- * REST Resource
      UsersSetAvailableProductSetResource

    -- * Creating a Request
    , usersSetAvailableProductSet
    , UsersSetAvailableProductSet

    -- * Request Lenses
    , usapsEnterpriseId
    , usapsPayload
    , usapsUserId
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.users.setAvailableProductSet@ method which the
-- 'UsersSetAvailableProductSet' request conforms to.
type UsersSetAvailableProductSetResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "users" :>
               Capture "userId" Text :>
                 "availableProductSet" :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] ProductSet :> Put '[JSON] ProductSet

-- | Modifies the set of products that a user is entitled to access (referred
-- to as whitelisted products). Only products that are approved or products
-- that were previously approved (products with revoked approval) can be
-- whitelisted.
--
-- /See:/ 'usersSetAvailableProductSet' smart constructor.
data UsersSetAvailableProductSet = UsersSetAvailableProductSet'
    { _usapsEnterpriseId :: !Text
    , _usapsPayload :: !ProductSet
    , _usapsUserId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersSetAvailableProductSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usapsEnterpriseId'
--
-- * 'usapsPayload'
--
-- * 'usapsUserId'
usersSetAvailableProductSet
    :: Text -- ^ 'usapsEnterpriseId'
    -> ProductSet -- ^ 'usapsPayload'
    -> Text -- ^ 'usapsUserId'
    -> UsersSetAvailableProductSet
usersSetAvailableProductSet pUsapsEnterpriseId_ pUsapsPayload_ pUsapsUserId_ = 
    UsersSetAvailableProductSet'
    { _usapsEnterpriseId = pUsapsEnterpriseId_
    , _usapsPayload = pUsapsPayload_
    , _usapsUserId = pUsapsUserId_
    }

-- | The ID of the enterprise.
usapsEnterpriseId :: Lens' UsersSetAvailableProductSet Text
usapsEnterpriseId
  = lens _usapsEnterpriseId
      (\ s a -> s{_usapsEnterpriseId = a})

-- | Multipart request metadata.
usapsPayload :: Lens' UsersSetAvailableProductSet ProductSet
usapsPayload
  = lens _usapsPayload (\ s a -> s{_usapsPayload = a})

-- | The ID of the user.
usapsUserId :: Lens' UsersSetAvailableProductSet Text
usapsUserId
  = lens _usapsUserId (\ s a -> s{_usapsUserId = a})

instance GoogleRequest UsersSetAvailableProductSet
         where
        type Rs UsersSetAvailableProductSet = ProductSet
        type Scopes UsersSetAvailableProductSet =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient UsersSetAvailableProductSet'{..}
          = go _usapsEnterpriseId _usapsUserId (Just AltJSON)
              _usapsPayload
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersSetAvailableProductSetResource)
                      mempty
