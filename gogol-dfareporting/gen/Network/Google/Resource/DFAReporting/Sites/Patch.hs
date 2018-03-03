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
-- Module      : Network.Google.Resource.DFAReporting.Sites.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing site. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.sites.patch@.
module Network.Google.Resource.DFAReporting.Sites.Patch
    (
    -- * REST Resource
      SitesPatchResource

    -- * Creating a Request
    , sitesPatch
    , SitesPatch

    -- * Request Lenses
    , spProFileId
    , spPayload
    , spId
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.sites.patch@ method which the
-- 'SitesPatch' request conforms to.
type SitesPatchResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "sites" :>
               QueryParam "id" (Textual Int64) :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Site :> Patch '[JSON] Site

-- | Updates an existing site. This method supports patch semantics.
--
-- /See:/ 'sitesPatch' smart constructor.
data SitesPatch = SitesPatch'
    { _spProFileId :: !(Textual Int64)
    , _spPayload :: !Site
    , _spId :: !(Textual Int64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SitesPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spProFileId'
--
-- * 'spPayload'
--
-- * 'spId'
sitesPatch
    :: Int64 -- ^ 'spProFileId'
    -> Site -- ^ 'spPayload'
    -> Int64 -- ^ 'spId'
    -> SitesPatch
sitesPatch pSpProFileId_ pSpPayload_ pSpId_ = 
    SitesPatch'
    { _spProFileId = _Coerce # pSpProFileId_
    , _spPayload = pSpPayload_
    , _spId = _Coerce # pSpId_
    }

-- | User profile ID associated with this request.
spProFileId :: Lens' SitesPatch Int64
spProFileId
  = lens _spProFileId (\ s a -> s{_spProFileId = a}) .
      _Coerce

-- | Multipart request metadata.
spPayload :: Lens' SitesPatch Site
spPayload
  = lens _spPayload (\ s a -> s{_spPayload = a})

-- | Site ID.
spId :: Lens' SitesPatch Int64
spId = lens _spId (\ s a -> s{_spId = a}) . _Coerce

instance GoogleRequest SitesPatch where
        type Rs SitesPatch = Site
        type Scopes SitesPatch =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient SitesPatch'{..}
          = go _spProFileId (Just _spId) (Just AltJSON)
              _spPayload
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy SitesPatchResource)
                      mempty
