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
-- Module      : Network.Google.Resource.DNS.Changes.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Atomically update the ResourceRecordSet collection.
--
-- /See:/ <https://developers.google.com/cloud-dns Google Cloud DNS API Reference> for @dns.changes.create@.
module Network.Google.Resource.DNS.Changes.Create
    (
    -- * REST Resource
      ChangesCreateResource

    -- * Creating a Request
    , changesCreate
    , ChangesCreate

    -- * Request Lenses
    , ccProject
    , ccPayload
    , ccManagedZone
    , ccClientOperationId
    ) where

import Network.Google.DNS.Types
import Network.Google.Prelude

-- | A resource alias for @dns.changes.create@ method which the
-- 'ChangesCreate' request conforms to.
type ChangesCreateResource =
     "dns" :>
       "v2beta1" :>
         "projects" :>
           Capture "project" Text :>
             "managedZones" :>
               Capture "managedZone" Text :>
                 "changes" :>
                   QueryParam "clientOperationId" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] Change :> Post '[JSON] Change

-- | Atomically update the ResourceRecordSet collection.
--
-- /See:/ 'changesCreate' smart constructor.
data ChangesCreate = ChangesCreate'
    { _ccProject :: !Text
    , _ccPayload :: !Change
    , _ccManagedZone :: !Text
    , _ccClientOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangesCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccProject'
--
-- * 'ccPayload'
--
-- * 'ccManagedZone'
--
-- * 'ccClientOperationId'
changesCreate
    :: Text -- ^ 'ccProject'
    -> Change -- ^ 'ccPayload'
    -> Text -- ^ 'ccManagedZone'
    -> ChangesCreate
changesCreate pCcProject_ pCcPayload_ pCcManagedZone_ = 
    ChangesCreate'
    { _ccProject = pCcProject_
    , _ccPayload = pCcPayload_
    , _ccManagedZone = pCcManagedZone_
    , _ccClientOperationId = Nothing
    }

-- | Identifies the project addressed by this request.
ccProject :: Lens' ChangesCreate Text
ccProject
  = lens _ccProject (\ s a -> s{_ccProject = a})

-- | Multipart request metadata.
ccPayload :: Lens' ChangesCreate Change
ccPayload
  = lens _ccPayload (\ s a -> s{_ccPayload = a})

-- | Identifies the managed zone addressed by this request. Can be the
-- managed zone name or id.
ccManagedZone :: Lens' ChangesCreate Text
ccManagedZone
  = lens _ccManagedZone
      (\ s a -> s{_ccManagedZone = a})

-- | For mutating operation requests only. An optional identifier specified
-- by the client. Must be unique for operation resources in the Operations
-- collection.
ccClientOperationId :: Lens' ChangesCreate (Maybe Text)
ccClientOperationId
  = lens _ccClientOperationId
      (\ s a -> s{_ccClientOperationId = a})

instance GoogleRequest ChangesCreate where
        type Rs ChangesCreate = Change
        type Scopes ChangesCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/ndev.clouddns.readwrite"]
        requestClient ChangesCreate'{..}
          = go _ccProject _ccManagedZone _ccClientOperationId
              (Just AltJSON)
              _ccPayload
              dNSService
          where go
                  = buildClient (Proxy :: Proxy ChangesCreateResource)
                      mempty
