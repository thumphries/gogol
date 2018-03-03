{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.YouTubeAnalytics.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.YouTubeAnalytics.Types.Product where

import Network.Google.Prelude
import Network.Google.YouTubeAnalytics.Types.Sum

--
-- /See:/ 'groupContentDetails' smart constructor.
data GroupContentDetails = GroupContentDetails'
    { _gcdItemType :: !(Maybe Text)
    , _gcdItemCount :: !(Maybe (Textual Word64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupContentDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdItemType'
--
-- * 'gcdItemCount'
groupContentDetails
    :: GroupContentDetails
groupContentDetails = 
    GroupContentDetails'
    { _gcdItemType = Nothing
    , _gcdItemCount = Nothing
    }

gcdItemType :: Lens' GroupContentDetails (Maybe Text)
gcdItemType
  = lens _gcdItemType (\ s a -> s{_gcdItemType = a})

gcdItemCount :: Lens' GroupContentDetails (Maybe Word64)
gcdItemCount
  = lens _gcdItemCount (\ s a -> s{_gcdItemCount = a})
      . mapping _Coerce

instance FromJSON GroupContentDetails where
        parseJSON
          = withObject "GroupContentDetails"
              (\ o ->
                 GroupContentDetails' <$>
                   (o .:? "itemType") <*> (o .:? "itemCount"))

instance ToJSON GroupContentDetails where
        toJSON GroupContentDetails'{..}
          = object
              (catMaybes
                 [("itemType" .=) <$> _gcdItemType,
                  ("itemCount" .=) <$> _gcdItemCount])

--
-- /See:/ 'group'' smart constructor.
data Group = Group'
    { _gEtag :: !(Maybe Text)
    , _gSnippet :: !(Maybe GroupSnippet)
    , _gKind :: !Text
    , _gContentDetails :: !(Maybe GroupContentDetails)
    , _gId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gEtag'
--
-- * 'gSnippet'
--
-- * 'gKind'
--
-- * 'gContentDetails'
--
-- * 'gId'
group'
    :: Group
group' = 
    Group'
    { _gEtag = Nothing
    , _gSnippet = Nothing
    , _gKind = "youtube#group"
    , _gContentDetails = Nothing
    , _gId = Nothing
    }

gEtag :: Lens' Group (Maybe Text)
gEtag = lens _gEtag (\ s a -> s{_gEtag = a})

gSnippet :: Lens' Group (Maybe GroupSnippet)
gSnippet = lens _gSnippet (\ s a -> s{_gSnippet = a})

gKind :: Lens' Group Text
gKind = lens _gKind (\ s a -> s{_gKind = a})

gContentDetails :: Lens' Group (Maybe GroupContentDetails)
gContentDetails
  = lens _gContentDetails
      (\ s a -> s{_gContentDetails = a})

gId :: Lens' Group (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ o ->
                 Group' <$>
                   (o .:? "etag") <*> (o .:? "snippet") <*>
                     (o .:? "kind" .!= "youtube#group")
                     <*> (o .:? "contentDetails")
                     <*> (o .:? "id"))

instance ToJSON Group where
        toJSON Group'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _gEtag,
                  ("snippet" .=) <$> _gSnippet,
                  Just ("kind" .= _gKind),
                  ("contentDetails" .=) <$> _gContentDetails,
                  ("id" .=) <$> _gId])

--
-- /See:/ 'groupItemResource' smart constructor.
data GroupItemResource = GroupItemResource'
    { _girKind :: !(Maybe Text)
    , _girId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupItemResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girKind'
--
-- * 'girId'
groupItemResource
    :: GroupItemResource
groupItemResource = 
    GroupItemResource'
    { _girKind = Nothing
    , _girId = Nothing
    }

girKind :: Lens' GroupItemResource (Maybe Text)
girKind = lens _girKind (\ s a -> s{_girKind = a})

girId :: Lens' GroupItemResource (Maybe Text)
girId = lens _girId (\ s a -> s{_girId = a})

instance FromJSON GroupItemResource where
        parseJSON
          = withObject "GroupItemResource"
              (\ o ->
                 GroupItemResource' <$>
                   (o .:? "kind") <*> (o .:? "id"))

instance ToJSON GroupItemResource where
        toJSON GroupItemResource'{..}
          = object
              (catMaybes
                 [("kind" .=) <$> _girKind, ("id" .=) <$> _girId])

--
-- /See:/ 'resultTableColumnHeadersItem' smart constructor.
data ResultTableColumnHeadersItem = ResultTableColumnHeadersItem'
    { _rtchiColumnType :: !(Maybe Text)
    , _rtchiName :: !(Maybe Text)
    , _rtchiDataType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResultTableColumnHeadersItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtchiColumnType'
--
-- * 'rtchiName'
--
-- * 'rtchiDataType'
resultTableColumnHeadersItem
    :: ResultTableColumnHeadersItem
resultTableColumnHeadersItem = 
    ResultTableColumnHeadersItem'
    { _rtchiColumnType = Nothing
    , _rtchiName = Nothing
    , _rtchiDataType = Nothing
    }

-- | The type of the column (DIMENSION or METRIC).
rtchiColumnType :: Lens' ResultTableColumnHeadersItem (Maybe Text)
rtchiColumnType
  = lens _rtchiColumnType
      (\ s a -> s{_rtchiColumnType = a})

-- | The name of the dimension or metric.
rtchiName :: Lens' ResultTableColumnHeadersItem (Maybe Text)
rtchiName
  = lens _rtchiName (\ s a -> s{_rtchiName = a})

-- | The type of the data in the column (STRING, INTEGER, FLOAT, etc.).
rtchiDataType :: Lens' ResultTableColumnHeadersItem (Maybe Text)
rtchiDataType
  = lens _rtchiDataType
      (\ s a -> s{_rtchiDataType = a})

instance FromJSON ResultTableColumnHeadersItem where
        parseJSON
          = withObject "ResultTableColumnHeadersItem"
              (\ o ->
                 ResultTableColumnHeadersItem' <$>
                   (o .:? "columnType") <*> (o .:? "name") <*>
                     (o .:? "dataType"))

instance ToJSON ResultTableColumnHeadersItem where
        toJSON ResultTableColumnHeadersItem'{..}
          = object
              (catMaybes
                 [("columnType" .=) <$> _rtchiColumnType,
                  ("name" .=) <$> _rtchiName,
                  ("dataType" .=) <$> _rtchiDataType])

-- | Contains a single result table. The table is returned as an array of
-- rows that contain the values for the cells of the table. Depending on
-- the metric or dimension, the cell can contain a string (video ID,
-- country code) or a number (number of views or number of likes).
--
-- /See:/ 'resultTable' smart constructor.
data ResultTable = ResultTable'
    { _rtKind :: !Text
    , _rtRows :: !(Maybe [[JSONValue]])
    , _rtColumnHeaders :: !(Maybe [ResultTableColumnHeadersItem])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResultTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtKind'
--
-- * 'rtRows'
--
-- * 'rtColumnHeaders'
resultTable
    :: ResultTable
resultTable = 
    ResultTable'
    { _rtKind = "youtubeAnalytics#resultTable"
    , _rtRows = Nothing
    , _rtColumnHeaders = Nothing
    }

-- | This value specifies the type of data included in the API response. For
-- the query method, the kind property value will be
-- youtubeAnalytics#resultTable.
rtKind :: Lens' ResultTable Text
rtKind = lens _rtKind (\ s a -> s{_rtKind = a})

-- | The list contains all rows of the result table. Each item in the list is
-- an array that contains comma-delimited data corresponding to a single
-- row of data. The order of the comma-delimited data fields will match the
-- order of the columns listed in the columnHeaders field. If no data is
-- available for the given query, the rows element will be omitted from the
-- response. The response for a query with the day dimension will not
-- contain rows for the most recent days.
rtRows :: Lens' ResultTable [[JSONValue]]
rtRows
  = lens _rtRows (\ s a -> s{_rtRows = a}) . _Default .
      _Coerce

-- | This value specifies information about the data returned in the rows
-- fields. Each item in the columnHeaders list identifies a field returned
-- in the rows value, which contains a list of comma-delimited data. The
-- columnHeaders list will begin with the dimensions specified in the API
-- request, which will be followed by the metrics specified in the API
-- request. The order of both dimensions and metrics will match the
-- ordering in the API request. For example, if the API request contains
-- the parameters dimensions=ageGroup,gender&metrics=viewerPercentage, the
-- API response will return columns in this order:
-- ageGroup,gender,viewerPercentage.
rtColumnHeaders :: Lens' ResultTable [ResultTableColumnHeadersItem]
rtColumnHeaders
  = lens _rtColumnHeaders
      (\ s a -> s{_rtColumnHeaders = a})
      . _Default
      . _Coerce

instance FromJSON ResultTable where
        parseJSON
          = withObject "ResultTable"
              (\ o ->
                 ResultTable' <$>
                   (o .:? "kind" .!= "youtubeAnalytics#resultTable") <*>
                     (o .:? "rows" .!= mempty)
                     <*> (o .:? "columnHeaders" .!= mempty))

instance ToJSON ResultTable where
        toJSON ResultTable'{..}
          = object
              (catMaybes
                 [Just ("kind" .= _rtKind), ("rows" .=) <$> _rtRows,
                  ("columnHeaders" .=) <$> _rtColumnHeaders])

--
-- /See:/ 'groupSnippet' smart constructor.
data GroupSnippet = GroupSnippet'
    { _gsPublishedAt :: !(Maybe DateTime')
    , _gsTitle :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupSnippet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsPublishedAt'
--
-- * 'gsTitle'
groupSnippet
    :: GroupSnippet
groupSnippet = 
    GroupSnippet'
    { _gsPublishedAt = Nothing
    , _gsTitle = Nothing
    }

gsPublishedAt :: Lens' GroupSnippet (Maybe UTCTime)
gsPublishedAt
  = lens _gsPublishedAt
      (\ s a -> s{_gsPublishedAt = a})
      . mapping _DateTime

gsTitle :: Lens' GroupSnippet (Maybe Text)
gsTitle = lens _gsTitle (\ s a -> s{_gsTitle = a})

instance FromJSON GroupSnippet where
        parseJSON
          = withObject "GroupSnippet"
              (\ o ->
                 GroupSnippet' <$>
                   (o .:? "publishedAt") <*> (o .:? "title"))

instance ToJSON GroupSnippet where
        toJSON GroupSnippet'{..}
          = object
              (catMaybes
                 [("publishedAt" .=) <$> _gsPublishedAt,
                  ("title" .=) <$> _gsTitle])

--
-- /See:/ 'groupItem' smart constructor.
data GroupItem = GroupItem'
    { _giEtag :: !(Maybe Text)
    , _giKind :: !Text
    , _giResource :: !(Maybe GroupItemResource)
    , _giGroupId :: !(Maybe Text)
    , _giId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giEtag'
--
-- * 'giKind'
--
-- * 'giResource'
--
-- * 'giGroupId'
--
-- * 'giId'
groupItem
    :: GroupItem
groupItem = 
    GroupItem'
    { _giEtag = Nothing
    , _giKind = "youtube#groupItem"
    , _giResource = Nothing
    , _giGroupId = Nothing
    , _giId = Nothing
    }

giEtag :: Lens' GroupItem (Maybe Text)
giEtag = lens _giEtag (\ s a -> s{_giEtag = a})

giKind :: Lens' GroupItem Text
giKind = lens _giKind (\ s a -> s{_giKind = a})

giResource :: Lens' GroupItem (Maybe GroupItemResource)
giResource
  = lens _giResource (\ s a -> s{_giResource = a})

giGroupId :: Lens' GroupItem (Maybe Text)
giGroupId
  = lens _giGroupId (\ s a -> s{_giGroupId = a})

giId :: Lens' GroupItem (Maybe Text)
giId = lens _giId (\ s a -> s{_giId = a})

instance FromJSON GroupItem where
        parseJSON
          = withObject "GroupItem"
              (\ o ->
                 GroupItem' <$>
                   (o .:? "etag") <*>
                     (o .:? "kind" .!= "youtube#groupItem")
                     <*> (o .:? "resource")
                     <*> (o .:? "groupId")
                     <*> (o .:? "id"))

instance ToJSON GroupItem where
        toJSON GroupItem'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _giEtag, Just ("kind" .= _giKind),
                  ("resource" .=) <$> _giResource,
                  ("groupId" .=) <$> _giGroupId, ("id" .=) <$> _giId])

-- | A paginated list of grouList resources returned in response to a
-- youtubeAnalytics.groupApi.list request.
--
-- /See:/ 'groupItemListResponse' smart constructor.
data GroupItemListResponse = GroupItemListResponse'
    { _gilrEtag :: !(Maybe Text)
    , _gilrKind :: !Text
    , _gilrItems :: !(Maybe [GroupItem])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupItemListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gilrEtag'
--
-- * 'gilrKind'
--
-- * 'gilrItems'
groupItemListResponse
    :: GroupItemListResponse
groupItemListResponse = 
    GroupItemListResponse'
    { _gilrEtag = Nothing
    , _gilrKind = "youtube#groupItemListResponse"
    , _gilrItems = Nothing
    }

gilrEtag :: Lens' GroupItemListResponse (Maybe Text)
gilrEtag = lens _gilrEtag (\ s a -> s{_gilrEtag = a})

gilrKind :: Lens' GroupItemListResponse Text
gilrKind = lens _gilrKind (\ s a -> s{_gilrKind = a})

gilrItems :: Lens' GroupItemListResponse [GroupItem]
gilrItems
  = lens _gilrItems (\ s a -> s{_gilrItems = a}) .
      _Default
      . _Coerce

instance FromJSON GroupItemListResponse where
        parseJSON
          = withObject "GroupItemListResponse"
              (\ o ->
                 GroupItemListResponse' <$>
                   (o .:? "etag") <*>
                     (o .:? "kind" .!= "youtube#groupItemListResponse")
                     <*> (o .:? "items" .!= mempty))

instance ToJSON GroupItemListResponse where
        toJSON GroupItemListResponse'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _gilrEtag,
                  Just ("kind" .= _gilrKind),
                  ("items" .=) <$> _gilrItems])

-- | A paginated list of grouList resources returned in response to a
-- youtubeAnalytics.groupApi.list request.
--
-- /See:/ 'groupListResponse' smart constructor.
data GroupListResponse = GroupListResponse'
    { _glrEtag :: !(Maybe Text)
    , _glrNextPageToken :: !(Maybe Text)
    , _glrKind :: !Text
    , _glrItems :: !(Maybe [Group])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glrEtag'
--
-- * 'glrNextPageToken'
--
-- * 'glrKind'
--
-- * 'glrItems'
groupListResponse
    :: GroupListResponse
groupListResponse = 
    GroupListResponse'
    { _glrEtag = Nothing
    , _glrNextPageToken = Nothing
    , _glrKind = "youtube#groupListResponse"
    , _glrItems = Nothing
    }

glrEtag :: Lens' GroupListResponse (Maybe Text)
glrEtag = lens _glrEtag (\ s a -> s{_glrEtag = a})

glrNextPageToken :: Lens' GroupListResponse (Maybe Text)
glrNextPageToken
  = lens _glrNextPageToken
      (\ s a -> s{_glrNextPageToken = a})

glrKind :: Lens' GroupListResponse Text
glrKind = lens _glrKind (\ s a -> s{_glrKind = a})

glrItems :: Lens' GroupListResponse [Group]
glrItems
  = lens _glrItems (\ s a -> s{_glrItems = a}) .
      _Default
      . _Coerce

instance FromJSON GroupListResponse where
        parseJSON
          = withObject "GroupListResponse"
              (\ o ->
                 GroupListResponse' <$>
                   (o .:? "etag") <*> (o .:? "nextPageToken") <*>
                     (o .:? "kind" .!= "youtube#groupListResponse")
                     <*> (o .:? "items" .!= mempty))

instance ToJSON GroupListResponse where
        toJSON GroupListResponse'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _glrEtag,
                  ("nextPageToken" .=) <$> _glrNextPageToken,
                  Just ("kind" .= _glrKind),
                  ("items" .=) <$> _glrItems])
