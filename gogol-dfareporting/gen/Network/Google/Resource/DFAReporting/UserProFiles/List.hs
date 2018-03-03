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
-- Module      : Network.Google.Resource.DFAReporting.UserProFiles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves list of user profiles for a user.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.userProfiles.list@.
module Network.Google.Resource.DFAReporting.UserProFiles.List
    (
    -- * REST Resource
      UserProFilesListResource

    -- * Creating a Request
    , userProFilesList
    , UserProFilesList

    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.userProfiles.list@ method which the
-- 'UserProFilesList' request conforms to.
type UserProFilesListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           QueryParam "alt" AltJSON :>
             Get '[JSON] UserProFileList

-- | Retrieves list of user profiles for a user.
--
-- /See:/ 'userProFilesList' smart constructor.
data UserProFilesList =
    UserProFilesList' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserProFilesList' with the minimum fields required to make a request.
--
userProFilesList
    :: UserProFilesList
userProFilesList = UserProFilesList'

instance GoogleRequest UserProFilesList where
        type Rs UserProFilesList = UserProFileList
        type Scopes UserProFilesList =
             '["https://www.googleapis.com/auth/dfareporting",
               "https://www.googleapis.com/auth/dfatrafficking"]
        requestClient UserProFilesList'{}
          = go (Just AltJSON) dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy UserProFilesListResource)
                      mempty
