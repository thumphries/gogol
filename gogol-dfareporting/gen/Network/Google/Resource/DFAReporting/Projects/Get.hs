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
-- Module      : Network.Google.Resource.DFAReporting.Projects.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one project by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.projects.get@.
module Network.Google.Resource.DFAReporting.Projects.Get
    (
    -- * REST Resource
      ProjectsGetResource

    -- * Creating a Request
    , projectsGet
    , ProjectsGet

    -- * Request Lenses
    , proProFileId
    , proId
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.projects.get@ method which the
-- 'ProjectsGet' request conforms to.
type ProjectsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "projects" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Project

-- | Gets one project by ID.
--
-- /See:/ 'projectsGet' smart constructor.
data ProjectsGet = ProjectsGet'
    { _proProFileId :: !(Textual Int64)
    , _proId :: !(Textual Int64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proProFileId'
--
-- * 'proId'
projectsGet
    :: Int64 -- ^ 'proProFileId'
    -> Int64 -- ^ 'proId'
    -> ProjectsGet
projectsGet pProProFileId_ pProId_ = 
    ProjectsGet'
    { _proProFileId = _Coerce # pProProFileId_
    , _proId = _Coerce # pProId_
    }

-- | User profile ID associated with this request.
proProFileId :: Lens' ProjectsGet Int64
proProFileId
  = lens _proProFileId (\ s a -> s{_proProFileId = a})
      . _Coerce

-- | Project ID.
proId :: Lens' ProjectsGet Int64
proId
  = lens _proId (\ s a -> s{_proId = a}) . _Coerce

instance GoogleRequest ProjectsGet where
        type Rs ProjectsGet = Project
        type Scopes ProjectsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient ProjectsGet'{..}
          = go _proProFileId _proId (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy ProjectsGetResource)
                      mempty
