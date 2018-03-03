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
-- Module      : Network.Google.Resource.Script.Projects.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a script project\'s metadata.
--
-- /See:/ <https://developers.google.com/apps-script/api/ Google Apps Script API Reference> for @script.projects.get@.
module Network.Google.Resource.Script.Projects.Get
    (
    -- * REST Resource
      ProjectsGetResource

    -- * Creating a Request
    , projectsGet
    , ProjectsGet

    -- * Request Lenses
    , pgXgafv
    , pgUploadProtocol
    , pgPp
    , pgAccessToken
    , pgUploadType
    , pgBearerToken
    , pgScriptId
    , pgCallback
    ) where

import Network.Google.Prelude
import Network.Google.Script.Types

-- | A resource alias for @script.projects.get@ method which the
-- 'ProjectsGet' request conforms to.
type ProjectsGetResource =
     "v1" :>
       "projects" :>
         Capture "scriptId" Text :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] Project

-- | Gets a script project\'s metadata.
--
-- /See:/ 'projectsGet' smart constructor.
data ProjectsGet = ProjectsGet'
    { _pgXgafv :: !(Maybe Xgafv)
    , _pgUploadProtocol :: !(Maybe Text)
    , _pgPp :: !Bool
    , _pgAccessToken :: !(Maybe Text)
    , _pgUploadType :: !(Maybe Text)
    , _pgBearerToken :: !(Maybe Text)
    , _pgScriptId :: !Text
    , _pgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgXgafv'
--
-- * 'pgUploadProtocol'
--
-- * 'pgPp'
--
-- * 'pgAccessToken'
--
-- * 'pgUploadType'
--
-- * 'pgBearerToken'
--
-- * 'pgScriptId'
--
-- * 'pgCallback'
projectsGet
    :: Text -- ^ 'pgScriptId'
    -> ProjectsGet
projectsGet pPgScriptId_ = 
    ProjectsGet'
    { _pgXgafv = Nothing
    , _pgUploadProtocol = Nothing
    , _pgPp = True
    , _pgAccessToken = Nothing
    , _pgUploadType = Nothing
    , _pgBearerToken = Nothing
    , _pgScriptId = pPgScriptId_
    , _pgCallback = Nothing
    }

-- | V1 error format.
pgXgafv :: Lens' ProjectsGet (Maybe Xgafv)
pgXgafv = lens _pgXgafv (\ s a -> s{_pgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgUploadProtocol :: Lens' ProjectsGet (Maybe Text)
pgUploadProtocol
  = lens _pgUploadProtocol
      (\ s a -> s{_pgUploadProtocol = a})

-- | Pretty-print response.
pgPp :: Lens' ProjectsGet Bool
pgPp = lens _pgPp (\ s a -> s{_pgPp = a})

-- | OAuth access token.
pgAccessToken :: Lens' ProjectsGet (Maybe Text)
pgAccessToken
  = lens _pgAccessToken
      (\ s a -> s{_pgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgUploadType :: Lens' ProjectsGet (Maybe Text)
pgUploadType
  = lens _pgUploadType (\ s a -> s{_pgUploadType = a})

-- | OAuth bearer token.
pgBearerToken :: Lens' ProjectsGet (Maybe Text)
pgBearerToken
  = lens _pgBearerToken
      (\ s a -> s{_pgBearerToken = a})

-- | The script project\'s Drive ID.
pgScriptId :: Lens' ProjectsGet Text
pgScriptId
  = lens _pgScriptId (\ s a -> s{_pgScriptId = a})

-- | JSONP
pgCallback :: Lens' ProjectsGet (Maybe Text)
pgCallback
  = lens _pgCallback (\ s a -> s{_pgCallback = a})

instance GoogleRequest ProjectsGet where
        type Rs ProjectsGet = Project
        type Scopes ProjectsGet = '[]
        requestClient ProjectsGet'{..}
          = go _pgScriptId _pgXgafv _pgUploadProtocol
              (Just _pgPp)
              _pgAccessToken
              _pgUploadType
              _pgBearerToken
              _pgCallback
              (Just AltJSON)
              scriptService
          where go
                  = buildClient (Proxy :: Proxy ProjectsGetResource)
                      mempty
