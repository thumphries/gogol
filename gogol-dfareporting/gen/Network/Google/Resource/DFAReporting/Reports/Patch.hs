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
-- Module      : Network.Google.Resource.DFAReporting.Reports.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.reports.patch@.
module Network.Google.Resource.DFAReporting.Reports.Patch
    (
    -- * REST Resource
      ReportsPatchResource

    -- * Creating a Request
    , reportsPatch
    , ReportsPatch

    -- * Request Lenses
    , rpReportId
    , rpProFileId
    , rpPayload
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.reports.patch@ method which the
-- 'ReportsPatch' request conforms to.
type ReportsPatchResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "reports" :>
               Capture "reportId" (Textual Int64) :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Report :> Patch '[JSON] Report

-- | Updates a report. This method supports patch semantics.
--
-- /See:/ 'reportsPatch' smart constructor.
data ReportsPatch = ReportsPatch'
    { _rpReportId :: !(Textual Int64)
    , _rpProFileId :: !(Textual Int64)
    , _rpPayload :: !Report
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpReportId'
--
-- * 'rpProFileId'
--
-- * 'rpPayload'
reportsPatch
    :: Int64 -- ^ 'rpReportId'
    -> Int64 -- ^ 'rpProFileId'
    -> Report -- ^ 'rpPayload'
    -> ReportsPatch
reportsPatch pRpReportId_ pRpProFileId_ pRpPayload_ = 
    ReportsPatch'
    { _rpReportId = _Coerce # pRpReportId_
    , _rpProFileId = _Coerce # pRpProFileId_
    , _rpPayload = pRpPayload_
    }

-- | The ID of the report.
rpReportId :: Lens' ReportsPatch Int64
rpReportId
  = lens _rpReportId (\ s a -> s{_rpReportId = a}) .
      _Coerce

-- | The DFA user profile ID.
rpProFileId :: Lens' ReportsPatch Int64
rpProFileId
  = lens _rpProFileId (\ s a -> s{_rpProFileId = a}) .
      _Coerce

-- | Multipart request metadata.
rpPayload :: Lens' ReportsPatch Report
rpPayload
  = lens _rpPayload (\ s a -> s{_rpPayload = a})

instance GoogleRequest ReportsPatch where
        type Rs ReportsPatch = Report
        type Scopes ReportsPatch =
             '["https://www.googleapis.com/auth/dfareporting"]
        requestClient ReportsPatch'{..}
          = go _rpProFileId _rpReportId (Just AltJSON)
              _rpPayload
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy ReportsPatchResource)
                      mempty
