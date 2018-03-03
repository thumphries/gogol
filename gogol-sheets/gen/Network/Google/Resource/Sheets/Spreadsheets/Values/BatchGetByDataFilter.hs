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
-- Module      : Network.Google.Resource.Sheets.Spreadsheets.Values.BatchGetByDataFilter
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more ranges of values that match the specified data
-- filters. The caller must specify the spreadsheet ID and one or more
-- DataFilters. Ranges that match any of the data filters in the request
-- will be returned.
--
-- /See:/ <https://developers.google.com/sheets/ Google Sheets API Reference> for @sheets.spreadsheets.values.batchGetByDataFilter@.
module Network.Google.Resource.Sheets.Spreadsheets.Values.BatchGetByDataFilter
    (
    -- * REST Resource
      SpreadsheetsValuesBatchGetByDataFilterResource

    -- * Creating a Request
    , spreadsheetsValuesBatchGetByDataFilter
    , SpreadsheetsValuesBatchGetByDataFilter

    -- * Request Lenses
    , svbgbdfXgafv
    , svbgbdfUploadProtocol
    , svbgbdfPp
    , svbgbdfAccessToken
    , svbgbdfSpreadsheetId
    , svbgbdfUploadType
    , svbgbdfPayload
    , svbgbdfBearerToken
    , svbgbdfCallback
    ) where

import Network.Google.Prelude
import Network.Google.Sheets.Types

-- | A resource alias for @sheets.spreadsheets.values.batchGetByDataFilter@ method which the
-- 'SpreadsheetsValuesBatchGetByDataFilter' request conforms to.
type SpreadsheetsValuesBatchGetByDataFilterResource =
     "v4" :>
       "spreadsheets" :>
         Capture "spreadsheetId" Text :>
           "values:batchGetByDataFilter" :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] BatchGetValuesByDataFilterRequest
                               :>
                               Post '[JSON] BatchGetValuesByDataFilterResponse

-- | Returns one or more ranges of values that match the specified data
-- filters. The caller must specify the spreadsheet ID and one or more
-- DataFilters. Ranges that match any of the data filters in the request
-- will be returned.
--
-- /See:/ 'spreadsheetsValuesBatchGetByDataFilter' smart constructor.
data SpreadsheetsValuesBatchGetByDataFilter = SpreadsheetsValuesBatchGetByDataFilter'
    { _svbgbdfXgafv :: !(Maybe Xgafv)
    , _svbgbdfUploadProtocol :: !(Maybe Text)
    , _svbgbdfPp :: !Bool
    , _svbgbdfAccessToken :: !(Maybe Text)
    , _svbgbdfSpreadsheetId :: !Text
    , _svbgbdfUploadType :: !(Maybe Text)
    , _svbgbdfPayload :: !BatchGetValuesByDataFilterRequest
    , _svbgbdfBearerToken :: !(Maybe Text)
    , _svbgbdfCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpreadsheetsValuesBatchGetByDataFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svbgbdfXgafv'
--
-- * 'svbgbdfUploadProtocol'
--
-- * 'svbgbdfPp'
--
-- * 'svbgbdfAccessToken'
--
-- * 'svbgbdfSpreadsheetId'
--
-- * 'svbgbdfUploadType'
--
-- * 'svbgbdfPayload'
--
-- * 'svbgbdfBearerToken'
--
-- * 'svbgbdfCallback'
spreadsheetsValuesBatchGetByDataFilter
    :: Text -- ^ 'svbgbdfSpreadsheetId'
    -> BatchGetValuesByDataFilterRequest -- ^ 'svbgbdfPayload'
    -> SpreadsheetsValuesBatchGetByDataFilter
spreadsheetsValuesBatchGetByDataFilter pSvbgbdfSpreadsheetId_ pSvbgbdfPayload_ = 
    SpreadsheetsValuesBatchGetByDataFilter'
    { _svbgbdfXgafv = Nothing
    , _svbgbdfUploadProtocol = Nothing
    , _svbgbdfPp = True
    , _svbgbdfAccessToken = Nothing
    , _svbgbdfSpreadsheetId = pSvbgbdfSpreadsheetId_
    , _svbgbdfUploadType = Nothing
    , _svbgbdfPayload = pSvbgbdfPayload_
    , _svbgbdfBearerToken = Nothing
    , _svbgbdfCallback = Nothing
    }

-- | V1 error format.
svbgbdfXgafv :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Xgafv)
svbgbdfXgafv
  = lens _svbgbdfXgafv (\ s a -> s{_svbgbdfXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
svbgbdfUploadProtocol :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Text)
svbgbdfUploadProtocol
  = lens _svbgbdfUploadProtocol
      (\ s a -> s{_svbgbdfUploadProtocol = a})

-- | Pretty-print response.
svbgbdfPp :: Lens' SpreadsheetsValuesBatchGetByDataFilter Bool
svbgbdfPp
  = lens _svbgbdfPp (\ s a -> s{_svbgbdfPp = a})

-- | OAuth access token.
svbgbdfAccessToken :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Text)
svbgbdfAccessToken
  = lens _svbgbdfAccessToken
      (\ s a -> s{_svbgbdfAccessToken = a})

-- | The ID of the spreadsheet to retrieve data from.
svbgbdfSpreadsheetId :: Lens' SpreadsheetsValuesBatchGetByDataFilter Text
svbgbdfSpreadsheetId
  = lens _svbgbdfSpreadsheetId
      (\ s a -> s{_svbgbdfSpreadsheetId = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
svbgbdfUploadType :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Text)
svbgbdfUploadType
  = lens _svbgbdfUploadType
      (\ s a -> s{_svbgbdfUploadType = a})

-- | Multipart request metadata.
svbgbdfPayload :: Lens' SpreadsheetsValuesBatchGetByDataFilter BatchGetValuesByDataFilterRequest
svbgbdfPayload
  = lens _svbgbdfPayload
      (\ s a -> s{_svbgbdfPayload = a})

-- | OAuth bearer token.
svbgbdfBearerToken :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Text)
svbgbdfBearerToken
  = lens _svbgbdfBearerToken
      (\ s a -> s{_svbgbdfBearerToken = a})

-- | JSONP
svbgbdfCallback :: Lens' SpreadsheetsValuesBatchGetByDataFilter (Maybe Text)
svbgbdfCallback
  = lens _svbgbdfCallback
      (\ s a -> s{_svbgbdfCallback = a})

instance GoogleRequest
         SpreadsheetsValuesBatchGetByDataFilter where
        type Rs SpreadsheetsValuesBatchGetByDataFilter =
             BatchGetValuesByDataFilterResponse
        type Scopes SpreadsheetsValuesBatchGetByDataFilter =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.file",
               "https://www.googleapis.com/auth/spreadsheets"]
        requestClient
          SpreadsheetsValuesBatchGetByDataFilter'{..}
          = go _svbgbdfSpreadsheetId _svbgbdfXgafv
              _svbgbdfUploadProtocol
              (Just _svbgbdfPp)
              _svbgbdfAccessToken
              _svbgbdfUploadType
              _svbgbdfBearerToken
              _svbgbdfCallback
              (Just AltJSON)
              _svbgbdfPayload
              sheetsService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy SpreadsheetsValuesBatchGetByDataFilterResource)
                      mempty
