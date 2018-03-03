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
-- Module      : Network.Google.Resource.Gmail.Users.Labels.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all labels in the user\'s mailbox.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.labels.list@.
module Network.Google.Resource.Gmail.Users.Labels.List
    (
    -- * REST Resource
      UsersLabelsListResource

    -- * Creating a Request
    , usersLabelsList
    , UsersLabelsList

    -- * Request Lenses
    , ullUserId
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.labels.list@ method which the
-- 'UsersLabelsList' request conforms to.
type UsersLabelsListResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "labels" :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] ListLabelsResponse

-- | Lists all labels in the user\'s mailbox.
--
-- /See:/ 'usersLabelsList' smart constructor.
newtype UsersLabelsList = UsersLabelsList'
    { _ullUserId :: Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersLabelsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ullUserId'
usersLabelsList
    :: UsersLabelsList
usersLabelsList = 
    UsersLabelsList'
    { _ullUserId = "me"
    }

-- | The user\'s email address. The special value me can be used to indicate
-- the authenticated user.
ullUserId :: Lens' UsersLabelsList Text
ullUserId
  = lens _ullUserId (\ s a -> s{_ullUserId = a})

instance GoogleRequest UsersLabelsList where
        type Rs UsersLabelsList = ListLabelsResponse
        type Scopes UsersLabelsList =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.labels",
               "https://www.googleapis.com/auth/gmail.metadata",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly"]
        requestClient UsersLabelsList'{..}
          = go _ullUserId (Just AltJSON) gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersLabelsListResource)
                      mempty
