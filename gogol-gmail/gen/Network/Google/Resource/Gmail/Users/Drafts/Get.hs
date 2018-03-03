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
-- Module      : Network.Google.Resource.Gmail.Users.Drafts.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified draft.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.drafts.get@.
module Network.Google.Resource.Gmail.Users.Drafts.Get
    (
    -- * REST Resource
      UsersDraftsGetResource

    -- * Creating a Request
    , usersDraftsGet
    , UsersDraftsGet

    -- * Request Lenses
    , udgFormat
    , udgUserId
    , udgId
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.drafts.get@ method which the
-- 'UsersDraftsGet' request conforms to.
type UsersDraftsGetResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "drafts" :>
               Capture "id" Text :>
                 QueryParam "format" UsersDraftsGetFormat :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Draft

-- | Gets the specified draft.
--
-- /See:/ 'usersDraftsGet' smart constructor.
data UsersDraftsGet = UsersDraftsGet'
    { _udgFormat :: !UsersDraftsGetFormat
    , _udgUserId :: !Text
    , _udgId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersDraftsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udgFormat'
--
-- * 'udgUserId'
--
-- * 'udgId'
usersDraftsGet
    :: Text -- ^ 'udgId'
    -> UsersDraftsGet
usersDraftsGet pUdgId_ = 
    UsersDraftsGet'
    { _udgFormat = UDGFFull
    , _udgUserId = "me"
    , _udgId = pUdgId_
    }

-- | The format to return the draft in.
udgFormat :: Lens' UsersDraftsGet UsersDraftsGetFormat
udgFormat
  = lens _udgFormat (\ s a -> s{_udgFormat = a})

-- | The user\'s email address. The special value me can be used to indicate
-- the authenticated user.
udgUserId :: Lens' UsersDraftsGet Text
udgUserId
  = lens _udgUserId (\ s a -> s{_udgUserId = a})

-- | The ID of the draft to retrieve.
udgId :: Lens' UsersDraftsGet Text
udgId = lens _udgId (\ s a -> s{_udgId = a})

instance GoogleRequest UsersDraftsGet where
        type Rs UsersDraftsGet = Draft
        type Scopes UsersDraftsGet =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.compose",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly"]
        requestClient UsersDraftsGet'{..}
          = go _udgUserId _udgId (Just _udgFormat)
              (Just AltJSON)
              gmailService
          where go
                  = buildClient (Proxy :: Proxy UsersDraftsGetResource)
                      mempty
