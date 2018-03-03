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
-- Module      : Network.Google.Resource.Directory.Roles.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a role.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.roles.get@.
module Network.Google.Resource.Directory.Roles.Get
    (
    -- * REST Resource
      RolesGetResource

    -- * Creating a Request
    , rolesGet
    , RolesGet

    -- * Request Lenses
    , rgRoleId
    , rgCustomer
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.roles.get@ method which the
-- 'RolesGet' request conforms to.
type RolesGetResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "roles" :>
                 Capture "roleId" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Role

-- | Retrieves a role.
--
-- /See:/ 'rolesGet' smart constructor.
data RolesGet = RolesGet'
    { _rgRoleId :: !Text
    , _rgCustomer :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RolesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgRoleId'
--
-- * 'rgCustomer'
rolesGet
    :: Text -- ^ 'rgRoleId'
    -> Text -- ^ 'rgCustomer'
    -> RolesGet
rolesGet pRgRoleId_ pRgCustomer_ = 
    RolesGet'
    { _rgRoleId = pRgRoleId_
    , _rgCustomer = pRgCustomer_
    }

-- | Immutable ID of the role.
rgRoleId :: Lens' RolesGet Text
rgRoleId = lens _rgRoleId (\ s a -> s{_rgRoleId = a})

-- | Immutable ID of the G Suite account.
rgCustomer :: Lens' RolesGet Text
rgCustomer
  = lens _rgCustomer (\ s a -> s{_rgCustomer = a})

instance GoogleRequest RolesGet where
        type Rs RolesGet = Role
        type Scopes RolesGet =
             '["https://www.googleapis.com/auth/admin.directory.rolemanagement",
               "https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly"]
        requestClient RolesGet'{..}
          = go _rgCustomer _rgRoleId (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy RolesGetResource)
                      mempty
