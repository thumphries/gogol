{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Drive.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Drive.Types.Sum where

import Network.Google.Prelude

-- | The source of files to list. Deprecated: use \'corpora\' instead.
data FilesListCorpus
    = FLCDomain
      -- ^ @domain@
      -- Files shared to the user\'s domain.
    | FLCUser
      -- ^ @user@
      -- Files owned by or shared to the user.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable FilesListCorpus

instance FromHttpApiData FilesListCorpus where
    parseQueryParam = \case
        "domain" -> Right FLCDomain
        "user" -> Right FLCUser
        x -> Left ("Unable to parse FilesListCorpus from: " <> x)

instance ToHttpApiData FilesListCorpus where
    toQueryParam = \case
        FLCDomain -> "domain"
        FLCUser -> "user"

instance FromJSON FilesListCorpus where
    parseJSON = parseJSONText "FilesListCorpus"

instance ToJSON FilesListCorpus where
    toJSON = toJSONText
