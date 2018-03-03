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
-- Module      : Network.Google.Resource.AndroidPublisher.Edits.APKListings.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates or creates the APK-specific localized listing for a specified
-- APK and language code.
--
-- /See:/ <https://developers.google.com/android-publisher Google Play Developer API Reference> for @androidpublisher.edits.apklistings.update@.
module Network.Google.Resource.AndroidPublisher.Edits.APKListings.Update
    (
    -- * REST Resource
      EditsAPKListingsUpdateResource

    -- * Creating a Request
    , editsAPKListingsUpdate
    , EditsAPKListingsUpdate

    -- * Request Lenses
    , eapkluPackageName
    , eapkluAPKVersionCode
    , eapkluPayload
    , eapkluLanguage
    , eapkluEditId
    ) where

import Network.Google.AndroidPublisher.Types
import Network.Google.Prelude

-- | A resource alias for @androidpublisher.edits.apklistings.update@ method which the
-- 'EditsAPKListingsUpdate' request conforms to.
type EditsAPKListingsUpdateResource =
     "androidpublisher" :>
       "v2" :>
         "applications" :>
           Capture "packageName" Text :>
             "edits" :>
               Capture "editId" Text :>
                 "apks" :>
                   Capture "apkVersionCode" (Textual Int32) :>
                     "listings" :>
                       Capture "language" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] APKListing :> Put '[JSON] APKListing

-- | Updates or creates the APK-specific localized listing for a specified
-- APK and language code.
--
-- /See:/ 'editsAPKListingsUpdate' smart constructor.
data EditsAPKListingsUpdate = EditsAPKListingsUpdate'
    { _eapkluPackageName :: !Text
    , _eapkluAPKVersionCode :: !(Textual Int32)
    , _eapkluPayload :: !APKListing
    , _eapkluLanguage :: !Text
    , _eapkluEditId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EditsAPKListingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eapkluPackageName'
--
-- * 'eapkluAPKVersionCode'
--
-- * 'eapkluPayload'
--
-- * 'eapkluLanguage'
--
-- * 'eapkluEditId'
editsAPKListingsUpdate
    :: Text -- ^ 'eapkluPackageName'
    -> Int32 -- ^ 'eapkluAPKVersionCode'
    -> APKListing -- ^ 'eapkluPayload'
    -> Text -- ^ 'eapkluLanguage'
    -> Text -- ^ 'eapkluEditId'
    -> EditsAPKListingsUpdate
editsAPKListingsUpdate pEapkluPackageName_ pEapkluAPKVersionCode_ pEapkluPayload_ pEapkluLanguage_ pEapkluEditId_ = 
    EditsAPKListingsUpdate'
    { _eapkluPackageName = pEapkluPackageName_
    , _eapkluAPKVersionCode = _Coerce # pEapkluAPKVersionCode_
    , _eapkluPayload = pEapkluPayload_
    , _eapkluLanguage = pEapkluLanguage_
    , _eapkluEditId = pEapkluEditId_
    }

-- | Unique identifier for the Android app that is being updated; for
-- example, \"com.spiffygame\".
eapkluPackageName :: Lens' EditsAPKListingsUpdate Text
eapkluPackageName
  = lens _eapkluPackageName
      (\ s a -> s{_eapkluPackageName = a})

-- | The APK version code whose APK-specific listings should be read or
-- modified.
eapkluAPKVersionCode :: Lens' EditsAPKListingsUpdate Int32
eapkluAPKVersionCode
  = lens _eapkluAPKVersionCode
      (\ s a -> s{_eapkluAPKVersionCode = a})
      . _Coerce

-- | Multipart request metadata.
eapkluPayload :: Lens' EditsAPKListingsUpdate APKListing
eapkluPayload
  = lens _eapkluPayload
      (\ s a -> s{_eapkluPayload = a})

-- | The language code (a BCP-47 language tag) of the APK-specific localized
-- listing to read or modify. For example, to select Austrian German, pass
-- \"de-AT\".
eapkluLanguage :: Lens' EditsAPKListingsUpdate Text
eapkluLanguage
  = lens _eapkluLanguage
      (\ s a -> s{_eapkluLanguage = a})

-- | Unique identifier for this edit.
eapkluEditId :: Lens' EditsAPKListingsUpdate Text
eapkluEditId
  = lens _eapkluEditId (\ s a -> s{_eapkluEditId = a})

instance GoogleRequest EditsAPKListingsUpdate where
        type Rs EditsAPKListingsUpdate = APKListing
        type Scopes EditsAPKListingsUpdate =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient EditsAPKListingsUpdate'{..}
          = go _eapkluPackageName _eapkluEditId
              _eapkluAPKVersionCode
              _eapkluLanguage
              (Just AltJSON)
              _eapkluPayload
              androidPublisherService
          where go
                  = buildClient
                      (Proxy :: Proxy EditsAPKListingsUpdateResource)
                      mempty
