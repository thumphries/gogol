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
-- Module      : Network.Google.Resource.Gmail.Users.Messages.Send
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the specified message to the recipients in the To, Cc, and Bcc
-- headers.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.messages.send@.
module Network.Google.Resource.Gmail.Users.Messages.Send
    (
    -- * REST Resource
      UsersMessagesSendResource

    -- * Creating a Request
    , usersMessagesSend
    , UsersMessagesSend

    -- * Request Lenses
    , umsPayload
    , umsUserId
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.messages.send@ method which the
-- 'UsersMessagesSend' request conforms to.
type UsersMessagesSendResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "messages" :>
               "send" :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Message :> Post '[JSON] Message
       :<|>
       "upload" :>
         "gmail" :>
           "v1" :>
             "users" :>
               Capture "userId" Text :>
                 "messages" :>
                   "send" :>
                     QueryParam "alt" AltJSON :>
                       QueryParam "uploadType" Multipart :>
                         MultipartRelated '[JSON] Message :>
                           Post '[JSON] Message

-- | Sends the specified message to the recipients in the To, Cc, and Bcc
-- headers.
--
-- /See:/ 'usersMessagesSend' smart constructor.
data UsersMessagesSend = UsersMessagesSend'
    { _umsPayload :: !Message
    , _umsUserId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersMessagesSend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umsPayload'
--
-- * 'umsUserId'
usersMessagesSend
    :: Message -- ^ 'umsPayload'
    -> UsersMessagesSend
usersMessagesSend pUmsPayload_ = 
    UsersMessagesSend'
    { _umsPayload = pUmsPayload_
    , _umsUserId = "me"
    }

-- | Multipart request metadata.
umsPayload :: Lens' UsersMessagesSend Message
umsPayload
  = lens _umsPayload (\ s a -> s{_umsPayload = a})

-- | The user\'s email address. The special value me can be used to indicate
-- the authenticated user.
umsUserId :: Lens' UsersMessagesSend Text
umsUserId
  = lens _umsUserId (\ s a -> s{_umsUserId = a})

instance GoogleRequest UsersMessagesSend where
        type Rs UsersMessagesSend = Message
        type Scopes UsersMessagesSend =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.compose",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.send"]
        requestClient UsersMessagesSend'{..}
          = go _umsUserId (Just AltJSON) _umsPayload
              gmailService
          where go :<|> _
                  = buildClient
                      (Proxy :: Proxy UsersMessagesSendResource)
                      mempty

instance GoogleRequest
         (MediaUpload UsersMessagesSend) where
        type Rs (MediaUpload UsersMessagesSend) = Message
        type Scopes (MediaUpload UsersMessagesSend) =
             Scopes UsersMessagesSend
        requestClient
          (MediaUpload UsersMessagesSend'{..} body)
          = go _umsUserId (Just AltJSON) (Just Multipart)
              _umsPayload
              body
              gmailService
          where _ :<|> go
                  = buildClient
                      (Proxy :: Proxy UsersMessagesSendResource)
                      mempty
