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
-- Module      : Network.Google.Resource.AndroidPublisher.Edits.Listings.Deleteall
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all localized listings from an edit.
--
-- /See:/ <https://developers.google.com/android-publisher Google Play Developer API Reference> for @androidpublisher.edits.listings.deleteall@.
module Network.Google.Resource.AndroidPublisher.Edits.Listings.Deleteall
    (
    -- * REST Resource
      EditsListingsDeleteallResource

    -- * Creating a Request
    , editsListingsDeleteall
    , EditsListingsDeleteall

    -- * Request Lenses
    , eldlPackageName
    , eldlEditId
    ) where

import Network.Google.AndroidPublisher.Types
import Network.Google.Prelude

-- | A resource alias for @androidpublisher.edits.listings.deleteall@ method which the
-- 'EditsListingsDeleteall' request conforms to.
type EditsListingsDeleteallResource =
     "androidpublisher" :>
       "v2" :>
         "applications" :>
           Capture "packageName" Text :>
             "edits" :>
               Capture "editId" Text :>
                 "listings" :>
                   QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes all localized listings from an edit.
--
-- /See:/ 'editsListingsDeleteall' smart constructor.
data EditsListingsDeleteall = EditsListingsDeleteall'
    { _eldlPackageName :: !Text
    , _eldlEditId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EditsListingsDeleteall' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eldlPackageName'
--
-- * 'eldlEditId'
editsListingsDeleteall
    :: Text -- ^ 'eldlPackageName'
    -> Text -- ^ 'eldlEditId'
    -> EditsListingsDeleteall
editsListingsDeleteall pEldlPackageName_ pEldlEditId_ = 
    EditsListingsDeleteall'
    { _eldlPackageName = pEldlPackageName_
    , _eldlEditId = pEldlEditId_
    }

-- | Unique identifier for the Android app that is being updated; for
-- example, \"com.spiffygame\".
eldlPackageName :: Lens' EditsListingsDeleteall Text
eldlPackageName
  = lens _eldlPackageName
      (\ s a -> s{_eldlPackageName = a})

-- | Unique identifier for this edit.
eldlEditId :: Lens' EditsListingsDeleteall Text
eldlEditId
  = lens _eldlEditId (\ s a -> s{_eldlEditId = a})

instance GoogleRequest EditsListingsDeleteall where
        type Rs EditsListingsDeleteall = ()
        type Scopes EditsListingsDeleteall =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient EditsListingsDeleteall'{..}
          = go _eldlPackageName _eldlEditId (Just AltJSON)
              androidPublisherService
          where go
                  = buildClient
                      (Proxy :: Proxy EditsListingsDeleteallResource)
                      mempty
