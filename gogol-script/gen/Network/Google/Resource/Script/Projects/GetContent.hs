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
-- Module      : Network.Google.Resource.Script.Projects.GetContent
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the content of the script project, including the code source and
-- metadata for each script file.
--
-- /See:/ <https://developers.google.com/apps-script/api/ Google Apps Script API Reference> for @script.projects.getContent@.
module Network.Google.Resource.Script.Projects.GetContent
    (
    -- * REST Resource
      ProjectsGetContentResource

    -- * Creating a Request
    , projectsGetContent
    , ProjectsGetContent

    -- * Request Lenses
    , pgcXgafv
    , pgcUploadProtocol
    , pgcPp
    , pgcAccessToken
    , pgcUploadType
    , pgcVersionNumber
    , pgcBearerToken
    , pgcScriptId
    , pgcCallback
    ) where

import Network.Google.Prelude
import Network.Google.Script.Types

-- | A resource alias for @script.projects.getContent@ method which the
-- 'ProjectsGetContent' request conforms to.
type ProjectsGetContentResource =
     "v1" :>
       "projects" :>
         Capture "scriptId" Text :>
           "content" :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "versionNumber" (Textual Int32) :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "alt" AltJSON :> Get '[JSON] Content

-- | Gets the content of the script project, including the code source and
-- metadata for each script file.
--
-- /See:/ 'projectsGetContent' smart constructor.
data ProjectsGetContent = ProjectsGetContent'
    { _pgcXgafv :: !(Maybe Xgafv)
    , _pgcUploadProtocol :: !(Maybe Text)
    , _pgcPp :: !Bool
    , _pgcAccessToken :: !(Maybe Text)
    , _pgcUploadType :: !(Maybe Text)
    , _pgcVersionNumber :: !(Maybe (Textual Int32))
    , _pgcBearerToken :: !(Maybe Text)
    , _pgcScriptId :: !Text
    , _pgcCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGetContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgcXgafv'
--
-- * 'pgcUploadProtocol'
--
-- * 'pgcPp'
--
-- * 'pgcAccessToken'
--
-- * 'pgcUploadType'
--
-- * 'pgcVersionNumber'
--
-- * 'pgcBearerToken'
--
-- * 'pgcScriptId'
--
-- * 'pgcCallback'
projectsGetContent
    :: Text -- ^ 'pgcScriptId'
    -> ProjectsGetContent
projectsGetContent pPgcScriptId_ = 
    ProjectsGetContent'
    { _pgcXgafv = Nothing
    , _pgcUploadProtocol = Nothing
    , _pgcPp = True
    , _pgcAccessToken = Nothing
    , _pgcUploadType = Nothing
    , _pgcVersionNumber = Nothing
    , _pgcBearerToken = Nothing
    , _pgcScriptId = pPgcScriptId_
    , _pgcCallback = Nothing
    }

-- | V1 error format.
pgcXgafv :: Lens' ProjectsGetContent (Maybe Xgafv)
pgcXgafv = lens _pgcXgafv (\ s a -> s{_pgcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgcUploadProtocol :: Lens' ProjectsGetContent (Maybe Text)
pgcUploadProtocol
  = lens _pgcUploadProtocol
      (\ s a -> s{_pgcUploadProtocol = a})

-- | Pretty-print response.
pgcPp :: Lens' ProjectsGetContent Bool
pgcPp = lens _pgcPp (\ s a -> s{_pgcPp = a})

-- | OAuth access token.
pgcAccessToken :: Lens' ProjectsGetContent (Maybe Text)
pgcAccessToken
  = lens _pgcAccessToken
      (\ s a -> s{_pgcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgcUploadType :: Lens' ProjectsGetContent (Maybe Text)
pgcUploadType
  = lens _pgcUploadType
      (\ s a -> s{_pgcUploadType = a})

-- | The version number of the project to retrieve. If not provided, the
-- project\'s HEAD version is returned.
pgcVersionNumber :: Lens' ProjectsGetContent (Maybe Int32)
pgcVersionNumber
  = lens _pgcVersionNumber
      (\ s a -> s{_pgcVersionNumber = a})
      . mapping _Coerce

-- | OAuth bearer token.
pgcBearerToken :: Lens' ProjectsGetContent (Maybe Text)
pgcBearerToken
  = lens _pgcBearerToken
      (\ s a -> s{_pgcBearerToken = a})

-- | The script project\'s Drive ID.
pgcScriptId :: Lens' ProjectsGetContent Text
pgcScriptId
  = lens _pgcScriptId (\ s a -> s{_pgcScriptId = a})

-- | JSONP
pgcCallback :: Lens' ProjectsGetContent (Maybe Text)
pgcCallback
  = lens _pgcCallback (\ s a -> s{_pgcCallback = a})

instance GoogleRequest ProjectsGetContent where
        type Rs ProjectsGetContent = Content
        type Scopes ProjectsGetContent = '[]
        requestClient ProjectsGetContent'{..}
          = go _pgcScriptId _pgcXgafv _pgcUploadProtocol
              (Just _pgcPp)
              _pgcAccessToken
              _pgcUploadType
              _pgcVersionNumber
              _pgcBearerToken
              _pgcCallback
              (Just AltJSON)
              scriptService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsGetContentResource)
                      mempty
