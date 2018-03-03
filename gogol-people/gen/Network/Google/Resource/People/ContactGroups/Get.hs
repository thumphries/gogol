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
-- Module      : Network.Google.Resource.People.ContactGroups.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a specific contact group owned by the authenticated user by
-- specifying a contact group resource name.
--
-- /See:/ <https://developers.google.com/people/ People API Reference> for @people.contactGroups.get@.
module Network.Google.Resource.People.ContactGroups.Get
    (
    -- * REST Resource
      ContactGroupsGetResource

    -- * Creating a Request
    , contactGroupsGet
    , ContactGroupsGet

    -- * Request Lenses
    , cggXgafv
    , cggMaxMembers
    , cggUploadProtocol
    , cggResourceName
    , cggPp
    , cggAccessToken
    , cggUploadType
    , cggBearerToken
    , cggCallback
    ) where

import Network.Google.People.Types
import Network.Google.Prelude

-- | A resource alias for @people.contactGroups.get@ method which the
-- 'ContactGroupsGet' request conforms to.
type ContactGroupsGetResource =
     "v1" :>
       Capture "resourceName" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "maxMembers" (Textual Int32) :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] ContactGroup

-- | Get a specific contact group owned by the authenticated user by
-- specifying a contact group resource name.
--
-- /See:/ 'contactGroupsGet' smart constructor.
data ContactGroupsGet = ContactGroupsGet'
    { _cggXgafv :: !(Maybe Xgafv)
    , _cggMaxMembers :: !(Maybe (Textual Int32))
    , _cggUploadProtocol :: !(Maybe Text)
    , _cggResourceName :: !Text
    , _cggPp :: !Bool
    , _cggAccessToken :: !(Maybe Text)
    , _cggUploadType :: !(Maybe Text)
    , _cggBearerToken :: !(Maybe Text)
    , _cggCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactGroupsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cggXgafv'
--
-- * 'cggMaxMembers'
--
-- * 'cggUploadProtocol'
--
-- * 'cggResourceName'
--
-- * 'cggPp'
--
-- * 'cggAccessToken'
--
-- * 'cggUploadType'
--
-- * 'cggBearerToken'
--
-- * 'cggCallback'
contactGroupsGet
    :: Text -- ^ 'cggResourceName'
    -> ContactGroupsGet
contactGroupsGet pCggResourceName_ = 
    ContactGroupsGet'
    { _cggXgafv = Nothing
    , _cggMaxMembers = Nothing
    , _cggUploadProtocol = Nothing
    , _cggResourceName = pCggResourceName_
    , _cggPp = True
    , _cggAccessToken = Nothing
    , _cggUploadType = Nothing
    , _cggBearerToken = Nothing
    , _cggCallback = Nothing
    }

-- | V1 error format.
cggXgafv :: Lens' ContactGroupsGet (Maybe Xgafv)
cggXgafv = lens _cggXgafv (\ s a -> s{_cggXgafv = a})

-- | Specifies the maximum number of members to return.
cggMaxMembers :: Lens' ContactGroupsGet (Maybe Int32)
cggMaxMembers
  = lens _cggMaxMembers
      (\ s a -> s{_cggMaxMembers = a})
      . mapping _Coerce

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
cggUploadProtocol :: Lens' ContactGroupsGet (Maybe Text)
cggUploadProtocol
  = lens _cggUploadProtocol
      (\ s a -> s{_cggUploadProtocol = a})

-- | The resource name of the contact group to get.
cggResourceName :: Lens' ContactGroupsGet Text
cggResourceName
  = lens _cggResourceName
      (\ s a -> s{_cggResourceName = a})

-- | Pretty-print response.
cggPp :: Lens' ContactGroupsGet Bool
cggPp = lens _cggPp (\ s a -> s{_cggPp = a})

-- | OAuth access token.
cggAccessToken :: Lens' ContactGroupsGet (Maybe Text)
cggAccessToken
  = lens _cggAccessToken
      (\ s a -> s{_cggAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
cggUploadType :: Lens' ContactGroupsGet (Maybe Text)
cggUploadType
  = lens _cggUploadType
      (\ s a -> s{_cggUploadType = a})

-- | OAuth bearer token.
cggBearerToken :: Lens' ContactGroupsGet (Maybe Text)
cggBearerToken
  = lens _cggBearerToken
      (\ s a -> s{_cggBearerToken = a})

-- | JSONP
cggCallback :: Lens' ContactGroupsGet (Maybe Text)
cggCallback
  = lens _cggCallback (\ s a -> s{_cggCallback = a})

instance GoogleRequest ContactGroupsGet where
        type Rs ContactGroupsGet = ContactGroup
        type Scopes ContactGroupsGet =
             '["https://www.googleapis.com/auth/contacts",
               "https://www.googleapis.com/auth/contacts.readonly"]
        requestClient ContactGroupsGet'{..}
          = go _cggResourceName _cggXgafv _cggMaxMembers
              _cggUploadProtocol
              (Just _cggPp)
              _cggAccessToken
              _cggUploadType
              _cggBearerToken
              _cggCallback
              (Just AltJSON)
              peopleService
          where go
                  = buildClient
                      (Proxy :: Proxy ContactGroupsGetResource)
                      mempty
