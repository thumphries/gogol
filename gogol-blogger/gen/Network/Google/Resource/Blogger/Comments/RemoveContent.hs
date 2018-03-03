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
-- Module      : Network.Google.Resource.Blogger.Comments.RemoveContent
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the content of a comment.
--
-- /See:/ <https://developers.google.com/blogger/docs/3.0/getting_started Blogger API Reference> for @blogger.comments.removeContent@.
module Network.Google.Resource.Blogger.Comments.RemoveContent
    (
    -- * REST Resource
      CommentsRemoveContentResource

    -- * Creating a Request
    , commentsRemoveContent
    , CommentsRemoveContent

    -- * Request Lenses
    , crcBlogId
    , crcPostId
    , crcCommentId
    ) where

import Network.Google.Blogger.Types
import Network.Google.Prelude

-- | A resource alias for @blogger.comments.removeContent@ method which the
-- 'CommentsRemoveContent' request conforms to.
type CommentsRemoveContentResource =
     "blogger" :>
       "v3" :>
         "blogs" :>
           Capture "blogId" Text :>
             "posts" :>
               Capture "postId" Text :>
                 "comments" :>
                   Capture "commentId" Text :>
                     "removecontent" :>
                       QueryParam "alt" AltJSON :> Post '[JSON] Comment

-- | Removes the content of a comment.
--
-- /See:/ 'commentsRemoveContent' smart constructor.
data CommentsRemoveContent = CommentsRemoveContent'
    { _crcBlogId :: !Text
    , _crcPostId :: !Text
    , _crcCommentId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommentsRemoveContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crcBlogId'
--
-- * 'crcPostId'
--
-- * 'crcCommentId'
commentsRemoveContent
    :: Text -- ^ 'crcBlogId'
    -> Text -- ^ 'crcPostId'
    -> Text -- ^ 'crcCommentId'
    -> CommentsRemoveContent
commentsRemoveContent pCrcBlogId_ pCrcPostId_ pCrcCommentId_ = 
    CommentsRemoveContent'
    { _crcBlogId = pCrcBlogId_
    , _crcPostId = pCrcPostId_
    , _crcCommentId = pCrcCommentId_
    }

-- | The ID of the Blog.
crcBlogId :: Lens' CommentsRemoveContent Text
crcBlogId
  = lens _crcBlogId (\ s a -> s{_crcBlogId = a})

-- | The ID of the Post.
crcPostId :: Lens' CommentsRemoveContent Text
crcPostId
  = lens _crcPostId (\ s a -> s{_crcPostId = a})

-- | The ID of the comment to delete content from.
crcCommentId :: Lens' CommentsRemoveContent Text
crcCommentId
  = lens _crcCommentId (\ s a -> s{_crcCommentId = a})

instance GoogleRequest CommentsRemoveContent where
        type Rs CommentsRemoveContent = Comment
        type Scopes CommentsRemoveContent =
             '["https://www.googleapis.com/auth/blogger"]
        requestClient CommentsRemoveContent'{..}
          = go _crcBlogId _crcPostId _crcCommentId
              (Just AltJSON)
              bloggerService
          where go
                  = buildClient
                      (Proxy :: Proxy CommentsRemoveContentResource)
                      mempty
