{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Datastore.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Datastore.Types.Product where

import Network.Google.Datastore.Types.Sum
import Network.Google.Prelude

-- | An object representing a latitude\/longitude pair. This is expressed as
-- a pair of doubles representing degrees latitude and degrees longitude.
-- Unless specified otherwise, this must conform to the
-- <http://www.unoosa.org/pdf/icg/2012/template/WGS_84.pdf WGS84 standard>.
-- Values must be within normalized ranges.
--
-- /See:/ 'latLng' smart constructor.
data LatLng = LatLng'
    { _llLatitude :: !(Maybe (Textual Double))
    , _llLongitude :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LatLng' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llLatitude'
--
-- * 'llLongitude'
latLng
    :: LatLng
latLng = 
    LatLng'
    { _llLatitude = Nothing
    , _llLongitude = Nothing
    }

-- | The latitude in degrees. It must be in the range [-90.0, +90.0].
llLatitude :: Lens' LatLng (Maybe Double)
llLatitude
  = lens _llLatitude (\ s a -> s{_llLatitude = a}) .
      mapping _Coerce

-- | The longitude in degrees. It must be in the range [-180.0, +180.0].
llLongitude :: Lens' LatLng (Maybe Double)
llLongitude
  = lens _llLongitude (\ s a -> s{_llLongitude = a}) .
      mapping _Coerce

instance FromJSON LatLng where
        parseJSON
          = withObject "LatLng"
              (\ o ->
                 LatLng' <$>
                   (o .:? "latitude") <*> (o .:? "longitude"))

instance ToJSON LatLng where
        toJSON LatLng'{..}
          = object
              (catMaybes
                 [("latitude" .=) <$> _llLatitude,
                  ("longitude" .=) <$> _llLongitude])

-- | Options for beginning a new transaction. Transactions can be created
-- explicitly with calls to Datastore.BeginTransaction or implicitly by
-- setting ReadOptions.new_transaction in read requests.
--
-- /See:/ 'transactionOptions' smart constructor.
data TransactionOptions = TransactionOptions'
    { _toReadWrite :: !(Maybe ReadWrite)
    , _toReadOnly :: !(Maybe ReadOnly)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TransactionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toReadWrite'
--
-- * 'toReadOnly'
transactionOptions
    :: TransactionOptions
transactionOptions = 
    TransactionOptions'
    { _toReadWrite = Nothing
    , _toReadOnly = Nothing
    }

-- | The transaction should allow both reads and writes.
toReadWrite :: Lens' TransactionOptions (Maybe ReadWrite)
toReadWrite
  = lens _toReadWrite (\ s a -> s{_toReadWrite = a})

-- | The transaction should only allow reads.
toReadOnly :: Lens' TransactionOptions (Maybe ReadOnly)
toReadOnly
  = lens _toReadOnly (\ s a -> s{_toReadOnly = a})

instance FromJSON TransactionOptions where
        parseJSON
          = withObject "TransactionOptions"
              (\ o ->
                 TransactionOptions' <$>
                   (o .:? "readWrite") <*> (o .:? "readOnly"))

instance ToJSON TransactionOptions where
        toJSON TransactionOptions'{..}
          = object
              (catMaybes
                 [("readWrite" .=) <$> _toReadWrite,
                  ("readOnly" .=) <$> _toReadOnly])

-- | The \`Status\` type defines a logical error model that is suitable for
-- different programming environments, including REST APIs and RPC APIs. It
-- is used by [gRPC](https:\/\/github.com\/grpc). The error model is
-- designed to be: - Simple to use and understand for most users - Flexible
-- enough to meet unexpected needs # Overview The \`Status\` message
-- contains three pieces of data: error code, error message, and error
-- details. The error code should be an enum value of google.rpc.Code, but
-- it may accept additional error codes if needed. The error message should
-- be a developer-facing English message that helps developers *understand*
-- and *resolve* the error. If a localized user-facing error message is
-- needed, put the localized message in the error details or localize it in
-- the client. The optional error details may contain arbitrary information
-- about the error. There is a predefined set of error detail types in the
-- package \`google.rpc\` that can be used for common error conditions. #
-- Language mapping The \`Status\` message is the logical representation of
-- the error model, but it is not necessarily the actual wire format. When
-- the \`Status\` message is exposed in different client libraries and
-- different wire protocols, it can be mapped differently. For example, it
-- will likely be mapped to some exceptions in Java, but more likely mapped
-- to some error codes in C. # Other uses The error model and the
-- \`Status\` message can be used in a variety of environments, either with
-- or without APIs, to provide a consistent developer experience across
-- different environments. Example uses of this error model include: -
-- Partial errors. If a service needs to return partial errors to the
-- client, it may embed the \`Status\` in the normal response to indicate
-- the partial errors. - Workflow errors. A typical workflow has multiple
-- steps. Each step may have a \`Status\` message for error reporting. -
-- Batch operations. If a client uses batch request and batch response, the
-- \`Status\` message should be used directly inside batch response, one
-- for each error sub-response. - Asynchronous operations. If an API call
-- embeds asynchronous operation results in its response, the status of
-- those operations should be represented directly using the \`Status\`
-- message. - Logging. If some API errors are stored in logs, the message
-- \`Status\` could be used directly after any stripping needed for
-- security\/privacy reasons.
--
-- /See:/ 'status' smart constructor.
data Status = Status'
    { _sDetails :: !(Maybe [StatusDetailsItem])
    , _sCode :: !(Maybe (Textual Int32))
    , _sMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Status' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetails'
--
-- * 'sCode'
--
-- * 'sMessage'
status
    :: Status
status = 
    Status'
    { _sDetails = Nothing
    , _sCode = Nothing
    , _sMessage = Nothing
    }

-- | A list of messages that carry the error details. There is a common set
-- of message types for APIs to use.
sDetails :: Lens' Status [StatusDetailsItem]
sDetails
  = lens _sDetails (\ s a -> s{_sDetails = a}) .
      _Default
      . _Coerce

-- | The status code, which should be an enum value of google.rpc.Code.
sCode :: Lens' Status (Maybe Int32)
sCode
  = lens _sCode (\ s a -> s{_sCode = a}) .
      mapping _Coerce

-- | A developer-facing error message, which should be in English. Any
-- user-facing error message should be localized and sent in the
-- google.rpc.Status.details field, or localized by the client.
sMessage :: Lens' Status (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a})

instance FromJSON Status where
        parseJSON
          = withObject "Status"
              (\ o ->
                 Status' <$>
                   (o .:? "details" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON Status where
        toJSON Status'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _sDetails,
                  ("code" .=) <$> _sCode,
                  ("message" .=) <$> _sMessage])

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
--
-- /See:/ 'googleLongrunningOperationMetadata' smart constructor.
newtype GoogleLongrunningOperationMetadata = GoogleLongrunningOperationMetadata'
    { _glomAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleLongrunningOperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glomAddtional'
googleLongrunningOperationMetadata
    :: HashMap Text JSONValue -- ^ 'glomAddtional'
    -> GoogleLongrunningOperationMetadata
googleLongrunningOperationMetadata pGlomAddtional_ = 
    GoogleLongrunningOperationMetadata'
    { _glomAddtional = _Coerce # pGlomAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
glomAddtional :: Lens' GoogleLongrunningOperationMetadata (HashMap Text JSONValue)
glomAddtional
  = lens _glomAddtional
      (\ s a -> s{_glomAddtional = a})
      . _Coerce

instance FromJSON GoogleLongrunningOperationMetadata
         where
        parseJSON
          = withObject "GoogleLongrunningOperationMetadata"
              (\ o ->
                 GoogleLongrunningOperationMetadata' <$>
                   (parseJSONObject o))

instance ToJSON GoogleLongrunningOperationMetadata
         where
        toJSON = toJSON . _glomAddtional

-- | Options specific to read \/ write transactions.
--
-- /See:/ 'readWrite' smart constructor.
newtype ReadWrite = ReadWrite'
    { _rwPreviousTransaction :: Maybe Bytes
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReadWrite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwPreviousTransaction'
readWrite
    :: ReadWrite
readWrite = 
    ReadWrite'
    { _rwPreviousTransaction = Nothing
    }

-- | The transaction identifier of the transaction being retried.
rwPreviousTransaction :: Lens' ReadWrite (Maybe ByteString)
rwPreviousTransaction
  = lens _rwPreviousTransaction
      (\ s a -> s{_rwPreviousTransaction = a})
      . mapping _Bytes

instance FromJSON ReadWrite where
        parseJSON
          = withObject "ReadWrite"
              (\ o -> ReadWrite' <$> (o .:? "previousTransaction"))

instance ToJSON ReadWrite where
        toJSON ReadWrite'{..}
          = object
              (catMaybes
                 [("previousTransaction" .=) <$>
                    _rwPreviousTransaction])

-- | The response for
-- google.datastore.admin.v1beta1.DatastoreAdmin.ExportEntities.
--
-- /See:/ 'googleDatastoreAdminV1beta1ExportEntitiesResponse' smart constructor.
newtype GoogleDatastoreAdminV1beta1ExportEntitiesResponse = GoogleDatastoreAdminV1beta1ExportEntitiesResponse'
    { _gdaveerOutputURL :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1ExportEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdaveerOutputURL'
googleDatastoreAdminV1beta1ExportEntitiesResponse
    :: GoogleDatastoreAdminV1beta1ExportEntitiesResponse
googleDatastoreAdminV1beta1ExportEntitiesResponse = 
    GoogleDatastoreAdminV1beta1ExportEntitiesResponse'
    { _gdaveerOutputURL = Nothing
    }

-- | Location of the output metadata file. This can be used to begin an
-- import into Cloud Datastore (this project or another project). See
-- google.datastore.admin.v1beta1.ImportEntitiesRequest.input_url. Only
-- present if the operation completed successfully.
gdaveerOutputURL :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesResponse (Maybe Text)
gdaveerOutputURL
  = lens _gdaveerOutputURL
      (\ s a -> s{_gdaveerOutputURL = a})

instance FromJSON
         GoogleDatastoreAdminV1beta1ExportEntitiesResponse
         where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1ExportEntitiesResponse"
              (\ o ->
                 GoogleDatastoreAdminV1beta1ExportEntitiesResponse'
                   <$> (o .:? "outputUrl"))

instance ToJSON
         GoogleDatastoreAdminV1beta1ExportEntitiesResponse
         where
        toJSON
          GoogleDatastoreAdminV1beta1ExportEntitiesResponse'{..}
          = object
              (catMaybes [("outputUrl" .=) <$> _gdaveerOutputURL])

-- | The request for Datastore.Rollback.
--
-- /See:/ 'rollbackRequest' smart constructor.
newtype RollbackRequest = RollbackRequest'
    { _rrTransaction :: Maybe Bytes
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RollbackRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrTransaction'
rollbackRequest
    :: RollbackRequest
rollbackRequest = 
    RollbackRequest'
    { _rrTransaction = Nothing
    }

-- | The transaction identifier, returned by a call to
-- Datastore.BeginTransaction.
rrTransaction :: Lens' RollbackRequest (Maybe ByteString)
rrTransaction
  = lens _rrTransaction
      (\ s a -> s{_rrTransaction = a})
      . mapping _Bytes

instance FromJSON RollbackRequest where
        parseJSON
          = withObject "RollbackRequest"
              (\ o -> RollbackRequest' <$> (o .:? "transaction"))

instance ToJSON RollbackRequest where
        toJSON RollbackRequest'{..}
          = object
              (catMaybes [("transaction" .=) <$> _rrTransaction])

-- | The request for Datastore.ReserveIds.
--
-- /See:/ 'reserveIdsRequest' smart constructor.
data ReserveIdsRequest = ReserveIdsRequest'
    { _rirKeys :: !(Maybe [Key])
    , _rirDatabaseId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReserveIdsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirKeys'
--
-- * 'rirDatabaseId'
reserveIdsRequest
    :: ReserveIdsRequest
reserveIdsRequest = 
    ReserveIdsRequest'
    { _rirKeys = Nothing
    , _rirDatabaseId = Nothing
    }

-- | A list of keys with complete key paths whose numeric IDs should not be
-- auto-allocated.
rirKeys :: Lens' ReserveIdsRequest [Key]
rirKeys
  = lens _rirKeys (\ s a -> s{_rirKeys = a}) . _Default
      . _Coerce

-- | If not empty, the ID of the database against which to make the request.
rirDatabaseId :: Lens' ReserveIdsRequest (Maybe Text)
rirDatabaseId
  = lens _rirDatabaseId
      (\ s a -> s{_rirDatabaseId = a})

instance FromJSON ReserveIdsRequest where
        parseJSON
          = withObject "ReserveIdsRequest"
              (\ o ->
                 ReserveIdsRequest' <$>
                   (o .:? "keys" .!= mempty) <*> (o .:? "databaseId"))

instance ToJSON ReserveIdsRequest where
        toJSON ReserveIdsRequest'{..}
          = object
              (catMaybes
                 [("keys" .=) <$> _rirKeys,
                  ("databaseId" .=) <$> _rirDatabaseId])

-- | A partition ID identifies a grouping of entities. The grouping is always
-- by project and namespace, however the namespace ID may be empty. A
-- partition ID contains several dimensions: project ID and namespace ID.
-- Partition dimensions: - May be \`\"\"\`. - Must be valid UTF-8 bytes. -
-- Must have values that match regex \`[A-Za-z\\d\\.\\-_]{1,100}\` If the
-- value of any dimension matches regex \`__.*__\`, the partition is
-- reserved\/read-only. A reserved\/read-only partition ID is forbidden in
-- certain documented contexts. Foreign partition IDs (in which the project
-- ID does not match the context project ID ) are discouraged. Reads and
-- writes of foreign partition IDs may fail if the project is not in an
-- active state.
--
-- /See:/ 'partitionId' smart constructor.
data PartitionId = PartitionId'
    { _piNamespaceId :: !(Maybe Text)
    , _piProjectId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PartitionId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piNamespaceId'
--
-- * 'piProjectId'
partitionId
    :: PartitionId
partitionId = 
    PartitionId'
    { _piNamespaceId = Nothing
    , _piProjectId = Nothing
    }

-- | If not empty, the ID of the namespace to which the entities belong.
piNamespaceId :: Lens' PartitionId (Maybe Text)
piNamespaceId
  = lens _piNamespaceId
      (\ s a -> s{_piNamespaceId = a})

-- | The ID of the project to which the entities belong.
piProjectId :: Lens' PartitionId (Maybe Text)
piProjectId
  = lens _piProjectId (\ s a -> s{_piProjectId = a})

instance FromJSON PartitionId where
        parseJSON
          = withObject "PartitionId"
              (\ o ->
                 PartitionId' <$>
                   (o .:? "namespaceId") <*> (o .:? "projectId"))

instance ToJSON PartitionId where
        toJSON PartitionId'{..}
          = object
              (catMaybes
                 [("namespaceId" .=) <$> _piNamespaceId,
                  ("projectId" .=) <$> _piProjectId])

-- | A batch of results produced by a query.
--
-- /See:/ 'queryResultBatch' smart constructor.
data QueryResultBatch = QueryResultBatch'
    { _qrbSkippedResults :: !(Maybe (Textual Int32))
    , _qrbSkippedCursor :: !(Maybe Bytes)
    , _qrbEntityResultType :: !(Maybe QueryResultBatchEntityResultType)
    , _qrbSnapshotVersion :: !(Maybe (Textual Int64))
    , _qrbEntityResults :: !(Maybe [EntityResult])
    , _qrbMoreResults :: !(Maybe QueryResultBatchMoreResults)
    , _qrbEndCursor :: !(Maybe Bytes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QueryResultBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrbSkippedResults'
--
-- * 'qrbSkippedCursor'
--
-- * 'qrbEntityResultType'
--
-- * 'qrbSnapshotVersion'
--
-- * 'qrbEntityResults'
--
-- * 'qrbMoreResults'
--
-- * 'qrbEndCursor'
queryResultBatch
    :: QueryResultBatch
queryResultBatch = 
    QueryResultBatch'
    { _qrbSkippedResults = Nothing
    , _qrbSkippedCursor = Nothing
    , _qrbEntityResultType = Nothing
    , _qrbSnapshotVersion = Nothing
    , _qrbEntityResults = Nothing
    , _qrbMoreResults = Nothing
    , _qrbEndCursor = Nothing
    }

-- | The number of results skipped, typically because of an offset.
qrbSkippedResults :: Lens' QueryResultBatch (Maybe Int32)
qrbSkippedResults
  = lens _qrbSkippedResults
      (\ s a -> s{_qrbSkippedResults = a})
      . mapping _Coerce

-- | A cursor that points to the position after the last skipped result. Will
-- be set when \`skipped_results\` != 0.
qrbSkippedCursor :: Lens' QueryResultBatch (Maybe ByteString)
qrbSkippedCursor
  = lens _qrbSkippedCursor
      (\ s a -> s{_qrbSkippedCursor = a})
      . mapping _Bytes

-- | The result type for every entity in \`entity_results\`.
qrbEntityResultType :: Lens' QueryResultBatch (Maybe QueryResultBatchEntityResultType)
qrbEntityResultType
  = lens _qrbEntityResultType
      (\ s a -> s{_qrbEntityResultType = a})

-- | The version number of the snapshot this batch was returned from. This
-- applies to the range of results from the query\'s \`start_cursor\` (or
-- the beginning of the query if no cursor was given) to this batch\'s
-- \`end_cursor\` (not the query\'s \`end_cursor\`). In a single
-- transaction, subsequent query result batches for the same query can have
-- a greater snapshot version number. Each batch\'s snapshot version is
-- valid for all preceding batches. The value will be zero for eventually
-- consistent queries.
qrbSnapshotVersion :: Lens' QueryResultBatch (Maybe Int64)
qrbSnapshotVersion
  = lens _qrbSnapshotVersion
      (\ s a -> s{_qrbSnapshotVersion = a})
      . mapping _Coerce

-- | The results for this batch.
qrbEntityResults :: Lens' QueryResultBatch [EntityResult]
qrbEntityResults
  = lens _qrbEntityResults
      (\ s a -> s{_qrbEntityResults = a})
      . _Default
      . _Coerce

-- | The state of the query after the current batch.
qrbMoreResults :: Lens' QueryResultBatch (Maybe QueryResultBatchMoreResults)
qrbMoreResults
  = lens _qrbMoreResults
      (\ s a -> s{_qrbMoreResults = a})

-- | A cursor that points to the position after the last result in the batch.
qrbEndCursor :: Lens' QueryResultBatch (Maybe ByteString)
qrbEndCursor
  = lens _qrbEndCursor (\ s a -> s{_qrbEndCursor = a})
      . mapping _Bytes

instance FromJSON QueryResultBatch where
        parseJSON
          = withObject "QueryResultBatch"
              (\ o ->
                 QueryResultBatch' <$>
                   (o .:? "skippedResults") <*> (o .:? "skippedCursor")
                     <*> (o .:? "entityResultType")
                     <*> (o .:? "snapshotVersion")
                     <*> (o .:? "entityResults" .!= mempty)
                     <*> (o .:? "moreResults")
                     <*> (o .:? "endCursor"))

instance ToJSON QueryResultBatch where
        toJSON QueryResultBatch'{..}
          = object
              (catMaybes
                 [("skippedResults" .=) <$> _qrbSkippedResults,
                  ("skippedCursor" .=) <$> _qrbSkippedCursor,
                  ("entityResultType" .=) <$> _qrbEntityResultType,
                  ("snapshotVersion" .=) <$> _qrbSnapshotVersion,
                  ("entityResults" .=) <$> _qrbEntityResults,
                  ("moreResults" .=) <$> _qrbMoreResults,
                  ("endCursor" .=) <$> _qrbEndCursor])

-- | The entity\'s properties. The map\'s keys are property names. A property
-- name matching regex \`__.*__\` is reserved. A reserved property name is
-- forbidden in certain documented contexts. The name must not contain more
-- than 500 characters. The name cannot be \`\"\"\`.
--
-- /See:/ 'entityProperties' smart constructor.
newtype EntityProperties = EntityProperties'
    { _epAddtional :: HashMap Text Value
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EntityProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epAddtional'
entityProperties
    :: HashMap Text Value -- ^ 'epAddtional'
    -> EntityProperties
entityProperties pEpAddtional_ = 
    EntityProperties'
    { _epAddtional = _Coerce # pEpAddtional_
    }

epAddtional :: Lens' EntityProperties (HashMap Text Value)
epAddtional
  = lens _epAddtional (\ s a -> s{_epAddtional = a}) .
      _Coerce

instance FromJSON EntityProperties where
        parseJSON
          = withObject "EntityProperties"
              (\ o -> EntityProperties' <$> (parseJSONObject o))

instance ToJSON EntityProperties where
        toJSON = toJSON . _epAddtional

-- | The request for Datastore.BeginTransaction.
--
-- /See:/ 'beginTransactionRequest' smart constructor.
newtype BeginTransactionRequest = BeginTransactionRequest'
    { _btrTransactionOptions :: Maybe TransactionOptions
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BeginTransactionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btrTransactionOptions'
beginTransactionRequest
    :: BeginTransactionRequest
beginTransactionRequest = 
    BeginTransactionRequest'
    { _btrTransactionOptions = Nothing
    }

-- | Options for a new transaction.
btrTransactionOptions :: Lens' BeginTransactionRequest (Maybe TransactionOptions)
btrTransactionOptions
  = lens _btrTransactionOptions
      (\ s a -> s{_btrTransactionOptions = a})

instance FromJSON BeginTransactionRequest where
        parseJSON
          = withObject "BeginTransactionRequest"
              (\ o ->
                 BeginTransactionRequest' <$>
                   (o .:? "transactionOptions"))

instance ToJSON BeginTransactionRequest where
        toJSON BeginTransactionRequest'{..}
          = object
              (catMaybes
                 [("transactionOptions" .=) <$>
                    _btrTransactionOptions])

-- | The request for Datastore.RunQuery.
--
-- /See:/ 'runQueryRequest' smart constructor.
data RunQueryRequest = RunQueryRequest'
    { _rqrPartitionId :: !(Maybe PartitionId)
    , _rqrGqlQuery :: !(Maybe GqlQuery)
    , _rqrQuery :: !(Maybe Query)
    , _rqrReadOptions :: !(Maybe ReadOptions)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunQueryRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqrPartitionId'
--
-- * 'rqrGqlQuery'
--
-- * 'rqrQuery'
--
-- * 'rqrReadOptions'
runQueryRequest
    :: RunQueryRequest
runQueryRequest = 
    RunQueryRequest'
    { _rqrPartitionId = Nothing
    , _rqrGqlQuery = Nothing
    , _rqrQuery = Nothing
    , _rqrReadOptions = Nothing
    }

-- | Entities are partitioned into subsets, identified by a partition ID.
-- Queries are scoped to a single partition. This partition ID is
-- normalized with the standard default context partition ID.
rqrPartitionId :: Lens' RunQueryRequest (Maybe PartitionId)
rqrPartitionId
  = lens _rqrPartitionId
      (\ s a -> s{_rqrPartitionId = a})

-- | The GQL query to run.
rqrGqlQuery :: Lens' RunQueryRequest (Maybe GqlQuery)
rqrGqlQuery
  = lens _rqrGqlQuery (\ s a -> s{_rqrGqlQuery = a})

-- | The query to run.
rqrQuery :: Lens' RunQueryRequest (Maybe Query)
rqrQuery = lens _rqrQuery (\ s a -> s{_rqrQuery = a})

-- | The options for this query.
rqrReadOptions :: Lens' RunQueryRequest (Maybe ReadOptions)
rqrReadOptions
  = lens _rqrReadOptions
      (\ s a -> s{_rqrReadOptions = a})

instance FromJSON RunQueryRequest where
        parseJSON
          = withObject "RunQueryRequest"
              (\ o ->
                 RunQueryRequest' <$>
                   (o .:? "partitionId") <*> (o .:? "gqlQuery") <*>
                     (o .:? "query")
                     <*> (o .:? "readOptions"))

instance ToJSON RunQueryRequest where
        toJSON RunQueryRequest'{..}
          = object
              (catMaybes
                 [("partitionId" .=) <$> _rqrPartitionId,
                  ("gqlQuery" .=) <$> _rqrGqlQuery,
                  ("query" .=) <$> _rqrQuery,
                  ("readOptions" .=) <$> _rqrReadOptions])

-- | The request for Datastore.AllocateIds.
--
-- /See:/ 'allocateIdsRequest' smart constructor.
newtype AllocateIdsRequest = AllocateIdsRequest'
    { _airKeys :: Maybe [Key]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateIdsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airKeys'
allocateIdsRequest
    :: AllocateIdsRequest
allocateIdsRequest = 
    AllocateIdsRequest'
    { _airKeys = Nothing
    }

-- | A list of keys with incomplete key paths for which to allocate IDs. No
-- key may be reserved\/read-only.
airKeys :: Lens' AllocateIdsRequest [Key]
airKeys
  = lens _airKeys (\ s a -> s{_airKeys = a}) . _Default
      . _Coerce

instance FromJSON AllocateIdsRequest where
        parseJSON
          = withObject "AllocateIdsRequest"
              (\ o ->
                 AllocateIdsRequest' <$> (o .:? "keys" .!= mempty))

instance ToJSON AllocateIdsRequest where
        toJSON AllocateIdsRequest'{..}
          = object (catMaybes [("keys" .=) <$> _airKeys])

-- | Metadata common to all Datastore Admin operations.
--
-- /See:/ 'googleDatastoreAdminV1beta1CommonMetadata' smart constructor.
data GoogleDatastoreAdminV1beta1CommonMetadata = GoogleDatastoreAdminV1beta1CommonMetadata'
    { _gdavcmState :: !(Maybe GoogleDatastoreAdminV1beta1CommonMetadataState)
    , _gdavcmStartTime :: !(Maybe DateTime')
    , _gdavcmEndTime :: !(Maybe DateTime')
    , _gdavcmLabels :: !(Maybe GoogleDatastoreAdminV1beta1CommonMetadataLabels)
    , _gdavcmOperationType :: !(Maybe GoogleDatastoreAdminV1beta1CommonMetadataOperationType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1CommonMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdavcmState'
--
-- * 'gdavcmStartTime'
--
-- * 'gdavcmEndTime'
--
-- * 'gdavcmLabels'
--
-- * 'gdavcmOperationType'
googleDatastoreAdminV1beta1CommonMetadata
    :: GoogleDatastoreAdminV1beta1CommonMetadata
googleDatastoreAdminV1beta1CommonMetadata = 
    GoogleDatastoreAdminV1beta1CommonMetadata'
    { _gdavcmState = Nothing
    , _gdavcmStartTime = Nothing
    , _gdavcmEndTime = Nothing
    , _gdavcmLabels = Nothing
    , _gdavcmOperationType = Nothing
    }

-- | The current state of the Operation.
gdavcmState :: Lens' GoogleDatastoreAdminV1beta1CommonMetadata (Maybe GoogleDatastoreAdminV1beta1CommonMetadataState)
gdavcmState
  = lens _gdavcmState (\ s a -> s{_gdavcmState = a})

-- | The time that work began on the operation.
gdavcmStartTime :: Lens' GoogleDatastoreAdminV1beta1CommonMetadata (Maybe UTCTime)
gdavcmStartTime
  = lens _gdavcmStartTime
      (\ s a -> s{_gdavcmStartTime = a})
      . mapping _DateTime

-- | The time the operation ended, either successfully or otherwise.
gdavcmEndTime :: Lens' GoogleDatastoreAdminV1beta1CommonMetadata (Maybe UTCTime)
gdavcmEndTime
  = lens _gdavcmEndTime
      (\ s a -> s{_gdavcmEndTime = a})
      . mapping _DateTime

-- | The client-assigned labels which were provided when the operation was
-- created. May also include additional labels.
gdavcmLabels :: Lens' GoogleDatastoreAdminV1beta1CommonMetadata (Maybe GoogleDatastoreAdminV1beta1CommonMetadataLabels)
gdavcmLabels
  = lens _gdavcmLabels (\ s a -> s{_gdavcmLabels = a})

-- | The type of the operation. Can be used as a filter in
-- ListOperationsRequest.
gdavcmOperationType :: Lens' GoogleDatastoreAdminV1beta1CommonMetadata (Maybe GoogleDatastoreAdminV1beta1CommonMetadataOperationType)
gdavcmOperationType
  = lens _gdavcmOperationType
      (\ s a -> s{_gdavcmOperationType = a})

instance FromJSON
         GoogleDatastoreAdminV1beta1CommonMetadata where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1CommonMetadata"
              (\ o ->
                 GoogleDatastoreAdminV1beta1CommonMetadata' <$>
                   (o .:? "state") <*> (o .:? "startTime") <*>
                     (o .:? "endTime")
                     <*> (o .:? "labels")
                     <*> (o .:? "operationType"))

instance ToJSON
         GoogleDatastoreAdminV1beta1CommonMetadata where
        toJSON GoogleDatastoreAdminV1beta1CommonMetadata'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _gdavcmState,
                  ("startTime" .=) <$> _gdavcmStartTime,
                  ("endTime" .=) <$> _gdavcmEndTime,
                  ("labels" .=) <$> _gdavcmLabels,
                  ("operationType" .=) <$> _gdavcmOperationType])

-- | A generic empty message that you can re-use to avoid defining duplicated
-- empty messages in your APIs. A typical example is to use it as the
-- request or the response type of an API method. For instance: service Foo
-- { rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty); } The
-- JSON representation for \`Empty\` is empty JSON object \`{}\`.
--
-- /See:/ 'empty' smart constructor.
data Empty =
    Empty' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Empty' with the minimum fields required to make a request.
--
empty
    :: Empty
empty = Empty'

instance FromJSON Empty where
        parseJSON = withObject "Empty" (\ o -> pure Empty')

instance ToJSON Empty where
        toJSON = const emptyObject

-- | A filter that merges multiple other filters using the given operator.
--
-- /See:/ 'compositeFilter' smart constructor.
data CompositeFilter = CompositeFilter'
    { _cfOp :: !(Maybe CompositeFilterOp)
    , _cfFilters :: !(Maybe [Filter])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompositeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfOp'
--
-- * 'cfFilters'
compositeFilter
    :: CompositeFilter
compositeFilter = 
    CompositeFilter'
    { _cfOp = Nothing
    , _cfFilters = Nothing
    }

-- | The operator for combining multiple filters.
cfOp :: Lens' CompositeFilter (Maybe CompositeFilterOp)
cfOp = lens _cfOp (\ s a -> s{_cfOp = a})

-- | The list of filters to combine. Must contain at least one filter.
cfFilters :: Lens' CompositeFilter [Filter]
cfFilters
  = lens _cfFilters (\ s a -> s{_cfFilters = a}) .
      _Default
      . _Coerce

instance FromJSON CompositeFilter where
        parseJSON
          = withObject "CompositeFilter"
              (\ o ->
                 CompositeFilter' <$>
                   (o .:? "op") <*> (o .:? "filters" .!= mempty))

instance ToJSON CompositeFilter where
        toJSON CompositeFilter'{..}
          = object
              (catMaybes
                 [("op" .=) <$> _cfOp, ("filters" .=) <$> _cfFilters])

-- | Metadata for ImportEntities operations.
--
-- /See:/ 'googleDatastoreAdminV1beta1ImportEntitiesMetadata' smart constructor.
data GoogleDatastoreAdminV1beta1ImportEntitiesMetadata = GoogleDatastoreAdminV1beta1ImportEntitiesMetadata'
    { _gdaviemProgressBytes :: !(Maybe GoogleDatastoreAdminV1beta1Progress)
    , _gdaviemProgressEntities :: !(Maybe GoogleDatastoreAdminV1beta1Progress)
    , _gdaviemEntityFilter :: !(Maybe GoogleDatastoreAdminV1beta1EntityFilter)
    , _gdaviemInputURL :: !(Maybe Text)
    , _gdaviemCommon :: !(Maybe GoogleDatastoreAdminV1beta1CommonMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1ImportEntitiesMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdaviemProgressBytes'
--
-- * 'gdaviemProgressEntities'
--
-- * 'gdaviemEntityFilter'
--
-- * 'gdaviemInputURL'
--
-- * 'gdaviemCommon'
googleDatastoreAdminV1beta1ImportEntitiesMetadata
    :: GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
googleDatastoreAdminV1beta1ImportEntitiesMetadata = 
    GoogleDatastoreAdminV1beta1ImportEntitiesMetadata'
    { _gdaviemProgressBytes = Nothing
    , _gdaviemProgressEntities = Nothing
    , _gdaviemEntityFilter = Nothing
    , _gdaviemInputURL = Nothing
    , _gdaviemCommon = Nothing
    }

-- | An estimate of the number of bytes processed.
gdaviemProgressBytes :: Lens' GoogleDatastoreAdminV1beta1ImportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1Progress)
gdaviemProgressBytes
  = lens _gdaviemProgressBytes
      (\ s a -> s{_gdaviemProgressBytes = a})

-- | An estimate of the number of entities processed.
gdaviemProgressEntities :: Lens' GoogleDatastoreAdminV1beta1ImportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1Progress)
gdaviemProgressEntities
  = lens _gdaviemProgressEntities
      (\ s a -> s{_gdaviemProgressEntities = a})

-- | Description of which entities are being imported.
gdaviemEntityFilter :: Lens' GoogleDatastoreAdminV1beta1ImportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1EntityFilter)
gdaviemEntityFilter
  = lens _gdaviemEntityFilter
      (\ s a -> s{_gdaviemEntityFilter = a})

-- | The location of the import metadata file. This will be the same value as
-- the google.datastore.admin.v1beta1.ExportEntitiesResponse.output_url
-- field.
gdaviemInputURL :: Lens' GoogleDatastoreAdminV1beta1ImportEntitiesMetadata (Maybe Text)
gdaviemInputURL
  = lens _gdaviemInputURL
      (\ s a -> s{_gdaviemInputURL = a})

-- | Metadata common to all Datastore Admin operations.
gdaviemCommon :: Lens' GoogleDatastoreAdminV1beta1ImportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1CommonMetadata)
gdaviemCommon
  = lens _gdaviemCommon
      (\ s a -> s{_gdaviemCommon = a})

instance FromJSON
         GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
         where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1ImportEntitiesMetadata"
              (\ o ->
                 GoogleDatastoreAdminV1beta1ImportEntitiesMetadata'
                   <$>
                   (o .:? "progressBytes") <*>
                     (o .:? "progressEntities")
                     <*> (o .:? "entityFilter")
                     <*> (o .:? "inputUrl")
                     <*> (o .:? "common"))

instance ToJSON
         GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
         where
        toJSON
          GoogleDatastoreAdminV1beta1ImportEntitiesMetadata'{..}
          = object
              (catMaybes
                 [("progressBytes" .=) <$> _gdaviemProgressBytes,
                  ("progressEntities" .=) <$> _gdaviemProgressEntities,
                  ("entityFilter" .=) <$> _gdaviemEntityFilter,
                  ("inputUrl" .=) <$> _gdaviemInputURL,
                  ("common" .=) <$> _gdaviemCommon])

-- | Measures the progress of a particular metric.
--
-- /See:/ 'googleDatastoreAdminV1beta1Progress' smart constructor.
data GoogleDatastoreAdminV1beta1Progress = GoogleDatastoreAdminV1beta1Progress'
    { _gdavpWorkCompleted :: !(Maybe (Textual Int64))
    , _gdavpWorkEstimated :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1Progress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdavpWorkCompleted'
--
-- * 'gdavpWorkEstimated'
googleDatastoreAdminV1beta1Progress
    :: GoogleDatastoreAdminV1beta1Progress
googleDatastoreAdminV1beta1Progress = 
    GoogleDatastoreAdminV1beta1Progress'
    { _gdavpWorkCompleted = Nothing
    , _gdavpWorkEstimated = Nothing
    }

-- | The amount of work that has been completed. Note that this may be
-- greater than work_estimated.
gdavpWorkCompleted :: Lens' GoogleDatastoreAdminV1beta1Progress (Maybe Int64)
gdavpWorkCompleted
  = lens _gdavpWorkCompleted
      (\ s a -> s{_gdavpWorkCompleted = a})
      . mapping _Coerce

-- | An estimate of how much work needs to be performed. May be zero if the
-- work estimate is unavailable.
gdavpWorkEstimated :: Lens' GoogleDatastoreAdminV1beta1Progress (Maybe Int64)
gdavpWorkEstimated
  = lens _gdavpWorkEstimated
      (\ s a -> s{_gdavpWorkEstimated = a})
      . mapping _Coerce

instance FromJSON GoogleDatastoreAdminV1beta1Progress
         where
        parseJSON
          = withObject "GoogleDatastoreAdminV1beta1Progress"
              (\ o ->
                 GoogleDatastoreAdminV1beta1Progress' <$>
                   (o .:? "workCompleted") <*> (o .:? "workEstimated"))

instance ToJSON GoogleDatastoreAdminV1beta1Progress
         where
        toJSON GoogleDatastoreAdminV1beta1Progress'{..}
          = object
              (catMaybes
                 [("workCompleted" .=) <$> _gdavpWorkCompleted,
                  ("workEstimated" .=) <$> _gdavpWorkEstimated])

-- | The response for Datastore.BeginTransaction.
--
-- /See:/ 'beginTransactionResponse' smart constructor.
newtype BeginTransactionResponse = BeginTransactionResponse'
    { _btrTransaction :: Maybe Bytes
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BeginTransactionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btrTransaction'
beginTransactionResponse
    :: BeginTransactionResponse
beginTransactionResponse = 
    BeginTransactionResponse'
    { _btrTransaction = Nothing
    }

-- | The transaction identifier (always present).
btrTransaction :: Lens' BeginTransactionResponse (Maybe ByteString)
btrTransaction
  = lens _btrTransaction
      (\ s a -> s{_btrTransaction = a})
      . mapping _Bytes

instance FromJSON BeginTransactionResponse where
        parseJSON
          = withObject "BeginTransactionResponse"
              (\ o ->
                 BeginTransactionResponse' <$> (o .:? "transaction"))

instance ToJSON BeginTransactionResponse where
        toJSON BeginTransactionResponse'{..}
          = object
              (catMaybes [("transaction" .=) <$> _btrTransaction])

-- | The result of applying a mutation.
--
-- /See:/ 'mutationResult' smart constructor.
data MutationResult = MutationResult'
    { _mrConflictDetected :: !(Maybe Bool)
    , _mrKey :: !(Maybe Key)
    , _mrVersion :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MutationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrConflictDetected'
--
-- * 'mrKey'
--
-- * 'mrVersion'
mutationResult
    :: MutationResult
mutationResult = 
    MutationResult'
    { _mrConflictDetected = Nothing
    , _mrKey = Nothing
    , _mrVersion = Nothing
    }

-- | Whether a conflict was detected for this mutation. Always false when a
-- conflict detection strategy field is not set in the mutation.
mrConflictDetected :: Lens' MutationResult (Maybe Bool)
mrConflictDetected
  = lens _mrConflictDetected
      (\ s a -> s{_mrConflictDetected = a})

-- | The automatically allocated key. Set only when the mutation allocated a
-- key.
mrKey :: Lens' MutationResult (Maybe Key)
mrKey = lens _mrKey (\ s a -> s{_mrKey = a})

-- | The version of the entity on the server after processing the mutation.
-- If the mutation doesn\'t change anything on the server, then the version
-- will be the version of the current entity or, if no entity is present, a
-- version that is strictly greater than the version of any previous entity
-- and less than the version of any possible future entity.
mrVersion :: Lens' MutationResult (Maybe Int64)
mrVersion
  = lens _mrVersion (\ s a -> s{_mrVersion = a}) .
      mapping _Coerce

instance FromJSON MutationResult where
        parseJSON
          = withObject "MutationResult"
              (\ o ->
                 MutationResult' <$>
                   (o .:? "conflictDetected") <*> (o .:? "key") <*>
                     (o .:? "version"))

instance ToJSON MutationResult where
        toJSON MutationResult'{..}
          = object
              (catMaybes
                 [("conflictDetected" .=) <$> _mrConflictDetected,
                  ("key" .=) <$> _mrKey,
                  ("version" .=) <$> _mrVersion])

-- | The response for Datastore.AllocateIds.
--
-- /See:/ 'allocateIdsResponse' smart constructor.
newtype AllocateIdsResponse = AllocateIdsResponse'
    { _aKeys :: Maybe [Key]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateIdsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aKeys'
allocateIdsResponse
    :: AllocateIdsResponse
allocateIdsResponse = 
    AllocateIdsResponse'
    { _aKeys = Nothing
    }

-- | The keys specified in the request (in the same order), each with its key
-- path completed with a newly allocated ID.
aKeys :: Lens' AllocateIdsResponse [Key]
aKeys
  = lens _aKeys (\ s a -> s{_aKeys = a}) . _Default .
      _Coerce

instance FromJSON AllocateIdsResponse where
        parseJSON
          = withObject "AllocateIdsResponse"
              (\ o ->
                 AllocateIdsResponse' <$> (o .:? "keys" .!= mempty))

instance ToJSON AllocateIdsResponse where
        toJSON AllocateIdsResponse'{..}
          = object (catMaybes [("keys" .=) <$> _aKeys])

-- | A [GQL
-- query](https:\/\/cloud.google.com\/datastore\/docs\/apis\/gql\/gql_reference).
--
-- /See:/ 'gqlQuery' smart constructor.
data GqlQuery = GqlQuery'
    { _gqPositionalBindings :: !(Maybe [GqlQueryParameter])
    , _gqNamedBindings :: !(Maybe GqlQueryNamedBindings)
    , _gqQueryString :: !(Maybe Text)
    , _gqAllowLiterals :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GqlQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqPositionalBindings'
--
-- * 'gqNamedBindings'
--
-- * 'gqQueryString'
--
-- * 'gqAllowLiterals'
gqlQuery
    :: GqlQuery
gqlQuery = 
    GqlQuery'
    { _gqPositionalBindings = Nothing
    , _gqNamedBindings = Nothing
    , _gqQueryString = Nothing
    , _gqAllowLiterals = Nothing
    }

-- | Numbered binding site \'1 references the first numbered parameter,
-- effectively using 1-based indexing, rather than the usual 0. For each
-- binding site numbered i in \`query_string\`, there must be an i-th
-- numbered parameter. The inverse must also be true.
gqPositionalBindings :: Lens' GqlQuery [GqlQueryParameter]
gqPositionalBindings
  = lens _gqPositionalBindings
      (\ s a -> s{_gqPositionalBindings = a})
      . _Default
      . _Coerce

-- | For each non-reserved named binding site in the query string, there must
-- be a named parameter with that name, but not necessarily the inverse.
-- Key must match regex \`A-Za-z_$*\`, must not match regex \`__.*__\`, and
-- must not be \`\"\"\`.
gqNamedBindings :: Lens' GqlQuery (Maybe GqlQueryNamedBindings)
gqNamedBindings
  = lens _gqNamedBindings
      (\ s a -> s{_gqNamedBindings = a})

-- | A string of the format described
-- [here](https:\/\/cloud.google.com\/datastore\/docs\/apis\/gql\/gql_reference).
gqQueryString :: Lens' GqlQuery (Maybe Text)
gqQueryString
  = lens _gqQueryString
      (\ s a -> s{_gqQueryString = a})

-- | When false, the query string must not contain any literals and instead
-- must bind all values. For example, \`SELECT * FROM Kind WHERE a =
-- \'string literal\'\` is not allowed, while \`SELECT * FROM Kind WHERE a
-- = \'value\` is.
gqAllowLiterals :: Lens' GqlQuery (Maybe Bool)
gqAllowLiterals
  = lens _gqAllowLiterals
      (\ s a -> s{_gqAllowLiterals = a})

instance FromJSON GqlQuery where
        parseJSON
          = withObject "GqlQuery"
              (\ o ->
                 GqlQuery' <$>
                   (o .:? "positionalBindings" .!= mempty) <*>
                     (o .:? "namedBindings")
                     <*> (o .:? "queryString")
                     <*> (o .:? "allowLiterals"))

instance ToJSON GqlQuery where
        toJSON GqlQuery'{..}
          = object
              (catMaybes
                 [("positionalBindings" .=) <$> _gqPositionalBindings,
                  ("namedBindings" .=) <$> _gqNamedBindings,
                  ("queryString" .=) <$> _gqQueryString,
                  ("allowLiterals" .=) <$> _gqAllowLiterals])

-- | The response for Datastore.RunQuery.
--
-- /See:/ 'runQueryResponse' smart constructor.
data RunQueryResponse = RunQueryResponse'
    { _rBatch :: !(Maybe QueryResultBatch)
    , _rQuery :: !(Maybe Query)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rBatch'
--
-- * 'rQuery'
runQueryResponse
    :: RunQueryResponse
runQueryResponse = 
    RunQueryResponse'
    { _rBatch = Nothing
    , _rQuery = Nothing
    }

-- | A batch of query results (always present).
rBatch :: Lens' RunQueryResponse (Maybe QueryResultBatch)
rBatch = lens _rBatch (\ s a -> s{_rBatch = a})

-- | The parsed form of the \`GqlQuery\` from the request, if it was set.
rQuery :: Lens' RunQueryResponse (Maybe Query)
rQuery = lens _rQuery (\ s a -> s{_rQuery = a})

instance FromJSON RunQueryResponse where
        parseJSON
          = withObject "RunQueryResponse"
              (\ o ->
                 RunQueryResponse' <$>
                   (o .:? "batch") <*> (o .:? "query"))

instance ToJSON RunQueryResponse where
        toJSON RunQueryResponse'{..}
          = object
              (catMaybes
                 [("batch" .=) <$> _rBatch, ("query" .=) <$> _rQuery])

-- | A message that can hold any of the supported value types and associated
-- metadata.
--
-- /See:/ 'value' smart constructor.
data Value = Value'
    { _vKeyValue :: !(Maybe Key)
    , _vGeoPointValue :: !(Maybe LatLng)
    , _vIntegerValue :: !(Maybe (Textual Int64))
    , _vTimestampValue :: !(Maybe DateTime')
    , _vEntityValue :: !(Maybe Entity)
    , _vExcludeFromIndexes :: !(Maybe Bool)
    , _vDoubleValue :: !(Maybe (Textual Double))
    , _vStringValue :: !(Maybe Text)
    , _vBooleanValue :: !(Maybe Bool)
    , _vMeaning :: !(Maybe (Textual Int32))
    , _vArrayValue :: !(Maybe ArrayValue)
    , _vNullValue :: !(Maybe ValueNullValue)
    , _vBlobValue :: !(Maybe Bytes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Value' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vKeyValue'
--
-- * 'vGeoPointValue'
--
-- * 'vIntegerValue'
--
-- * 'vTimestampValue'
--
-- * 'vEntityValue'
--
-- * 'vExcludeFromIndexes'
--
-- * 'vDoubleValue'
--
-- * 'vStringValue'
--
-- * 'vBooleanValue'
--
-- * 'vMeaning'
--
-- * 'vArrayValue'
--
-- * 'vNullValue'
--
-- * 'vBlobValue'
value
    :: Value
value = 
    Value'
    { _vKeyValue = Nothing
    , _vGeoPointValue = Nothing
    , _vIntegerValue = Nothing
    , _vTimestampValue = Nothing
    , _vEntityValue = Nothing
    , _vExcludeFromIndexes = Nothing
    , _vDoubleValue = Nothing
    , _vStringValue = Nothing
    , _vBooleanValue = Nothing
    , _vMeaning = Nothing
    , _vArrayValue = Nothing
    , _vNullValue = Nothing
    , _vBlobValue = Nothing
    }

-- | A key value.
vKeyValue :: Lens' Value (Maybe Key)
vKeyValue
  = lens _vKeyValue (\ s a -> s{_vKeyValue = a})

-- | A geo point value representing a point on the surface of Earth.
vGeoPointValue :: Lens' Value (Maybe LatLng)
vGeoPointValue
  = lens _vGeoPointValue
      (\ s a -> s{_vGeoPointValue = a})

-- | An integer value.
vIntegerValue :: Lens' Value (Maybe Int64)
vIntegerValue
  = lens _vIntegerValue
      (\ s a -> s{_vIntegerValue = a})
      . mapping _Coerce

-- | A timestamp value. When stored in the Datastore, precise only to
-- microseconds; any additional precision is rounded down.
vTimestampValue :: Lens' Value (Maybe UTCTime)
vTimestampValue
  = lens _vTimestampValue
      (\ s a -> s{_vTimestampValue = a})
      . mapping _DateTime

-- | An entity value. - May have no key. - May have a key with an incomplete
-- key path. - May have a reserved\/read-only key.
vEntityValue :: Lens' Value (Maybe Entity)
vEntityValue
  = lens _vEntityValue (\ s a -> s{_vEntityValue = a})

-- | If the value should be excluded from all indexes including those defined
-- explicitly.
vExcludeFromIndexes :: Lens' Value (Maybe Bool)
vExcludeFromIndexes
  = lens _vExcludeFromIndexes
      (\ s a -> s{_vExcludeFromIndexes = a})

-- | A double value.
vDoubleValue :: Lens' Value (Maybe Double)
vDoubleValue
  = lens _vDoubleValue (\ s a -> s{_vDoubleValue = a})
      . mapping _Coerce

-- | A UTF-8 encoded string value. When \`exclude_from_indexes\` is false (it
-- is indexed) , may have at most 1500 bytes. Otherwise, may be set to at
-- least 1,000,000 bytes.
vStringValue :: Lens' Value (Maybe Text)
vStringValue
  = lens _vStringValue (\ s a -> s{_vStringValue = a})

-- | A boolean value.
vBooleanValue :: Lens' Value (Maybe Bool)
vBooleanValue
  = lens _vBooleanValue
      (\ s a -> s{_vBooleanValue = a})

-- | The \`meaning\` field should only be populated for backwards
-- compatibility.
vMeaning :: Lens' Value (Maybe Int32)
vMeaning
  = lens _vMeaning (\ s a -> s{_vMeaning = a}) .
      mapping _Coerce

-- | An array value. Cannot contain another array value. A \`Value\` instance
-- that sets field \`array_value\` must not set fields \`meaning\` or
-- \`exclude_from_indexes\`.
vArrayValue :: Lens' Value (Maybe ArrayValue)
vArrayValue
  = lens _vArrayValue (\ s a -> s{_vArrayValue = a})

-- | A null value.
vNullValue :: Lens' Value (Maybe ValueNullValue)
vNullValue
  = lens _vNullValue (\ s a -> s{_vNullValue = a})

-- | A blob value. May have at most 1,000,000 bytes. When
-- \`exclude_from_indexes\` is false, may have at most 1500 bytes. In JSON
-- requests, must be base64-encoded.
vBlobValue :: Lens' Value (Maybe ByteString)
vBlobValue
  = lens _vBlobValue (\ s a -> s{_vBlobValue = a}) .
      mapping _Bytes

instance FromJSON Value where
        parseJSON
          = withObject "Value"
              (\ o ->
                 Value' <$>
                   (o .:? "keyValue") <*> (o .:? "geoPointValue") <*>
                     (o .:? "integerValue")
                     <*> (o .:? "timestampValue")
                     <*> (o .:? "entityValue")
                     <*> (o .:? "excludeFromIndexes")
                     <*> (o .:? "doubleValue")
                     <*> (o .:? "stringValue")
                     <*> (o .:? "booleanValue")
                     <*> (o .:? "meaning")
                     <*> (o .:? "arrayValue")
                     <*> (o .:? "nullValue")
                     <*> (o .:? "blobValue"))

instance ToJSON Value where
        toJSON Value'{..}
          = object
              (catMaybes
                 [("keyValue" .=) <$> _vKeyValue,
                  ("geoPointValue" .=) <$> _vGeoPointValue,
                  ("integerValue" .=) <$> _vIntegerValue,
                  ("timestampValue" .=) <$> _vTimestampValue,
                  ("entityValue" .=) <$> _vEntityValue,
                  ("excludeFromIndexes" .=) <$> _vExcludeFromIndexes,
                  ("doubleValue" .=) <$> _vDoubleValue,
                  ("stringValue" .=) <$> _vStringValue,
                  ("booleanValue" .=) <$> _vBooleanValue,
                  ("meaning" .=) <$> _vMeaning,
                  ("arrayValue" .=) <$> _vArrayValue,
                  ("nullValue" .=) <$> _vNullValue,
                  ("blobValue" .=) <$> _vBlobValue])

--
-- /See:/ 'statusDetailsItem' smart constructor.
newtype StatusDetailsItem = StatusDetailsItem'
    { _sdiAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiAddtional'
statusDetailsItem
    :: HashMap Text JSONValue -- ^ 'sdiAddtional'
    -> StatusDetailsItem
statusDetailsItem pSdiAddtional_ = 
    StatusDetailsItem'
    { _sdiAddtional = _Coerce # pSdiAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
sdiAddtional :: Lens' StatusDetailsItem (HashMap Text JSONValue)
sdiAddtional
  = lens _sdiAddtional (\ s a -> s{_sdiAddtional = a})
      . _Coerce

instance FromJSON StatusDetailsItem where
        parseJSON
          = withObject "StatusDetailsItem"
              (\ o -> StatusDetailsItem' <$> (parseJSONObject o))

instance ToJSON StatusDetailsItem where
        toJSON = toJSON . _sdiAddtional

-- | The request for Datastore.Lookup.
--
-- /See:/ 'lookupRequest' smart constructor.
data LookupRequest = LookupRequest'
    { _lrKeys :: !(Maybe [Key])
    , _lrReadOptions :: !(Maybe ReadOptions)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LookupRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrKeys'
--
-- * 'lrReadOptions'
lookupRequest
    :: LookupRequest
lookupRequest = 
    LookupRequest'
    { _lrKeys = Nothing
    , _lrReadOptions = Nothing
    }

-- | Keys of entities to look up.
lrKeys :: Lens' LookupRequest [Key]
lrKeys
  = lens _lrKeys (\ s a -> s{_lrKeys = a}) . _Default .
      _Coerce

-- | The options for this lookup request.
lrReadOptions :: Lens' LookupRequest (Maybe ReadOptions)
lrReadOptions
  = lens _lrReadOptions
      (\ s a -> s{_lrReadOptions = a})

instance FromJSON LookupRequest where
        parseJSON
          = withObject "LookupRequest"
              (\ o ->
                 LookupRequest' <$>
                   (o .:? "keys" .!= mempty) <*> (o .:? "readOptions"))

instance ToJSON LookupRequest where
        toJSON LookupRequest'{..}
          = object
              (catMaybes
                 [("keys" .=) <$> _lrKeys,
                  ("readOptions" .=) <$> _lrReadOptions])

-- | A mutation to apply to an entity.
--
-- /See:/ 'mutation' smart constructor.
data Mutation = Mutation'
    { _mBaseVersion :: !(Maybe (Textual Int64))
    , _mInsert :: !(Maybe Entity)
    , _mUpsert :: !(Maybe Entity)
    , _mDelete :: !(Maybe Key)
    , _mUpdate :: !(Maybe Entity)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Mutation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mBaseVersion'
--
-- * 'mInsert'
--
-- * 'mUpsert'
--
-- * 'mDelete'
--
-- * 'mUpdate'
mutation
    :: Mutation
mutation = 
    Mutation'
    { _mBaseVersion = Nothing
    , _mInsert = Nothing
    , _mUpsert = Nothing
    , _mDelete = Nothing
    , _mUpdate = Nothing
    }

-- | The version of the entity that this mutation is being applied to. If
-- this does not match the current version on the server, the mutation
-- conflicts.
mBaseVersion :: Lens' Mutation (Maybe Int64)
mBaseVersion
  = lens _mBaseVersion (\ s a -> s{_mBaseVersion = a})
      . mapping _Coerce

-- | The entity to insert. The entity must not already exist. The entity
-- key\'s final path element may be incomplete.
mInsert :: Lens' Mutation (Maybe Entity)
mInsert = lens _mInsert (\ s a -> s{_mInsert = a})

-- | The entity to upsert. The entity may or may not already exist. The
-- entity key\'s final path element may be incomplete.
mUpsert :: Lens' Mutation (Maybe Entity)
mUpsert = lens _mUpsert (\ s a -> s{_mUpsert = a})

-- | The key of the entity to delete. The entity may or may not already
-- exist. Must have a complete key path and must not be
-- reserved\/read-only.
mDelete :: Lens' Mutation (Maybe Key)
mDelete = lens _mDelete (\ s a -> s{_mDelete = a})

-- | The entity to update. The entity must already exist. Must have a
-- complete key path.
mUpdate :: Lens' Mutation (Maybe Entity)
mUpdate = lens _mUpdate (\ s a -> s{_mUpdate = a})

instance FromJSON Mutation where
        parseJSON
          = withObject "Mutation"
              (\ o ->
                 Mutation' <$>
                   (o .:? "baseVersion") <*> (o .:? "insert") <*>
                     (o .:? "upsert")
                     <*> (o .:? "delete")
                     <*> (o .:? "update"))

instance ToJSON Mutation where
        toJSON Mutation'{..}
          = object
              (catMaybes
                 [("baseVersion" .=) <$> _mBaseVersion,
                  ("insert" .=) <$> _mInsert,
                  ("upsert" .=) <$> _mUpsert,
                  ("delete" .=) <$> _mDelete,
                  ("update" .=) <$> _mUpdate])

-- | For each non-reserved named binding site in the query string, there must
-- be a named parameter with that name, but not necessarily the inverse.
-- Key must match regex \`A-Za-z_$*\`, must not match regex \`__.*__\`, and
-- must not be \`\"\"\`.
--
-- /See:/ 'gqlQueryNamedBindings' smart constructor.
newtype GqlQueryNamedBindings = GqlQueryNamedBindings'
    { _gqnbAddtional :: HashMap Text GqlQueryParameter
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GqlQueryNamedBindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqnbAddtional'
gqlQueryNamedBindings
    :: HashMap Text GqlQueryParameter -- ^ 'gqnbAddtional'
    -> GqlQueryNamedBindings
gqlQueryNamedBindings pGqnbAddtional_ = 
    GqlQueryNamedBindings'
    { _gqnbAddtional = _Coerce # pGqnbAddtional_
    }

gqnbAddtional :: Lens' GqlQueryNamedBindings (HashMap Text GqlQueryParameter)
gqnbAddtional
  = lens _gqnbAddtional
      (\ s a -> s{_gqnbAddtional = a})
      . _Coerce

instance FromJSON GqlQueryNamedBindings where
        parseJSON
          = withObject "GqlQueryNamedBindings"
              (\ o ->
                 GqlQueryNamedBindings' <$> (parseJSONObject o))

instance ToJSON GqlQueryNamedBindings where
        toJSON = toJSON . _gqnbAddtional

-- | A reference to a property relative to the kind expressions.
--
-- /See:/ 'propertyReference' smart constructor.
newtype PropertyReference = PropertyReference'
    { _prName :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PropertyReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prName'
propertyReference
    :: PropertyReference
propertyReference = 
    PropertyReference'
    { _prName = Nothing
    }

-- | The name of the property. If name includes \".\"s, it may be interpreted
-- as a property name path.
prName :: Lens' PropertyReference (Maybe Text)
prName = lens _prName (\ s a -> s{_prName = a})

instance FromJSON PropertyReference where
        parseJSON
          = withObject "PropertyReference"
              (\ o -> PropertyReference' <$> (o .:? "name"))

instance ToJSON PropertyReference where
        toJSON PropertyReference'{..}
          = object (catMaybes [("name" .=) <$> _prName])

-- | A unique identifier for an entity. If a key\'s partition ID or any of
-- its path kinds or names are reserved\/read-only, the key is
-- reserved\/read-only. A reserved\/read-only key is forbidden in certain
-- documented contexts.
--
-- /See:/ 'key' smart constructor.
data Key = Key'
    { _kPartitionId :: !(Maybe PartitionId)
    , _kPath :: !(Maybe [PathElement])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Key' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kPartitionId'
--
-- * 'kPath'
key
    :: Key
key = 
    Key'
    { _kPartitionId = Nothing
    , _kPath = Nothing
    }

-- | Entities are partitioned into subsets, currently identified by a project
-- ID and namespace ID. Queries are scoped to a single partition.
kPartitionId :: Lens' Key (Maybe PartitionId)
kPartitionId
  = lens _kPartitionId (\ s a -> s{_kPartitionId = a})

-- | The entity path. An entity path consists of one or more elements
-- composed of a kind and a string or numerical identifier, which identify
-- entities. The first element identifies a _root entity_, the second
-- element identifies a _child_ of the root entity, the third element
-- identifies a child of the second entity, and so forth. The entities
-- identified by all prefixes of the path are called the element\'s
-- _ancestors_. An entity path is always fully complete: *all* of the
-- entity\'s ancestors are required to be in the path along with the entity
-- identifier itself. The only exception is that in some documented cases,
-- the identifier in the last path element (for the entity) itself may be
-- omitted. For example, the last path element of the key of
-- \`Mutation.insert\` may have no identifier. A path can never be empty,
-- and a path can have at most 100 elements.
kPath :: Lens' Key [PathElement]
kPath
  = lens _kPath (\ s a -> s{_kPath = a}) . _Default .
      _Coerce

instance FromJSON Key where
        parseJSON
          = withObject "Key"
              (\ o ->
                 Key' <$>
                   (o .:? "partitionId") <*> (o .:? "path" .!= mempty))

instance ToJSON Key where
        toJSON Key'{..}
          = object
              (catMaybes
                 [("partitionId" .=) <$> _kPartitionId,
                  ("path" .=) <$> _kPath])

-- | A filter on a specific property.
--
-- /See:/ 'propertyFilter' smart constructor.
data PropertyFilter = PropertyFilter'
    { _pfProperty :: !(Maybe PropertyReference)
    , _pfOp :: !(Maybe PropertyFilterOp)
    , _pfValue :: !(Maybe Value)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PropertyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfProperty'
--
-- * 'pfOp'
--
-- * 'pfValue'
propertyFilter
    :: PropertyFilter
propertyFilter = 
    PropertyFilter'
    { _pfProperty = Nothing
    , _pfOp = Nothing
    , _pfValue = Nothing
    }

-- | The property to filter by.
pfProperty :: Lens' PropertyFilter (Maybe PropertyReference)
pfProperty
  = lens _pfProperty (\ s a -> s{_pfProperty = a})

-- | The operator to filter by.
pfOp :: Lens' PropertyFilter (Maybe PropertyFilterOp)
pfOp = lens _pfOp (\ s a -> s{_pfOp = a})

-- | The value to compare the property to.
pfValue :: Lens' PropertyFilter (Maybe Value)
pfValue = lens _pfValue (\ s a -> s{_pfValue = a})

instance FromJSON PropertyFilter where
        parseJSON
          = withObject "PropertyFilter"
              (\ o ->
                 PropertyFilter' <$>
                   (o .:? "property") <*> (o .:? "op") <*>
                     (o .:? "value"))

instance ToJSON PropertyFilter where
        toJSON PropertyFilter'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _pfProperty,
                  ("op" .=) <$> _pfOp, ("value" .=) <$> _pfValue])

-- | A query for entities.
--
-- /See:/ 'query' smart constructor.
data Query = Query'
    { _qStartCursor :: !(Maybe Bytes)
    , _qOffSet :: !(Maybe (Textual Int32))
    , _qKind :: !(Maybe [KindExpression])
    , _qDistinctOn :: !(Maybe [PropertyReference])
    , _qEndCursor :: !(Maybe Bytes)
    , _qLimit :: !(Maybe (Textual Int32))
    , _qProjection :: !(Maybe [Projection])
    , _qFilter :: !(Maybe Filter)
    , _qOrder :: !(Maybe [PropertyOrder])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Query' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qStartCursor'
--
-- * 'qOffSet'
--
-- * 'qKind'
--
-- * 'qDistinctOn'
--
-- * 'qEndCursor'
--
-- * 'qLimit'
--
-- * 'qProjection'
--
-- * 'qFilter'
--
-- * 'qOrder'
query
    :: Query
query = 
    Query'
    { _qStartCursor = Nothing
    , _qOffSet = Nothing
    , _qKind = Nothing
    , _qDistinctOn = Nothing
    , _qEndCursor = Nothing
    , _qLimit = Nothing
    , _qProjection = Nothing
    , _qFilter = Nothing
    , _qOrder = Nothing
    }

-- | A starting point for the query results. Query cursors are returned in
-- query result batches and [can only be used to continue the same
-- query](https:\/\/cloud.google.com\/datastore\/docs\/concepts\/queries#cursors_limits_and_offsets).
qStartCursor :: Lens' Query (Maybe ByteString)
qStartCursor
  = lens _qStartCursor (\ s a -> s{_qStartCursor = a})
      . mapping _Bytes

-- | The number of results to skip. Applies before limit, but after all other
-- constraints. Optional. Must be >= 0 if specified.
qOffSet :: Lens' Query (Maybe Int32)
qOffSet
  = lens _qOffSet (\ s a -> s{_qOffSet = a}) .
      mapping _Coerce

-- | The kinds to query (if empty, returns entities of all kinds). Currently
-- at most 1 kind may be specified.
qKind :: Lens' Query [KindExpression]
qKind
  = lens _qKind (\ s a -> s{_qKind = a}) . _Default .
      _Coerce

-- | The properties to make distinct. The query results will contain the
-- first result for each distinct combination of values for the given
-- properties (if empty, all results are returned).
qDistinctOn :: Lens' Query [PropertyReference]
qDistinctOn
  = lens _qDistinctOn (\ s a -> s{_qDistinctOn = a}) .
      _Default
      . _Coerce

-- | An ending point for the query results. Query cursors are returned in
-- query result batches and [can only be used to limit the same
-- query](https:\/\/cloud.google.com\/datastore\/docs\/concepts\/queries#cursors_limits_and_offsets).
qEndCursor :: Lens' Query (Maybe ByteString)
qEndCursor
  = lens _qEndCursor (\ s a -> s{_qEndCursor = a}) .
      mapping _Bytes

-- | The maximum number of results to return. Applies after all other
-- constraints. Optional. Unspecified is interpreted as no limit. Must be
-- >= 0 if specified.
qLimit :: Lens' Query (Maybe Int32)
qLimit
  = lens _qLimit (\ s a -> s{_qLimit = a}) .
      mapping _Coerce

-- | The projection to return. Defaults to returning all properties.
qProjection :: Lens' Query [Projection]
qProjection
  = lens _qProjection (\ s a -> s{_qProjection = a}) .
      _Default
      . _Coerce

-- | The filter to apply.
qFilter :: Lens' Query (Maybe Filter)
qFilter = lens _qFilter (\ s a -> s{_qFilter = a})

-- | The order to apply to the query results (if empty, order is
-- unspecified).
qOrder :: Lens' Query [PropertyOrder]
qOrder
  = lens _qOrder (\ s a -> s{_qOrder = a}) . _Default .
      _Coerce

instance FromJSON Query where
        parseJSON
          = withObject "Query"
              (\ o ->
                 Query' <$>
                   (o .:? "startCursor") <*> (o .:? "offset") <*>
                     (o .:? "kind" .!= mempty)
                     <*> (o .:? "distinctOn" .!= mempty)
                     <*> (o .:? "endCursor")
                     <*> (o .:? "limit")
                     <*> (o .:? "projection" .!= mempty)
                     <*> (o .:? "filter")
                     <*> (o .:? "order" .!= mempty))

instance ToJSON Query where
        toJSON Query'{..}
          = object
              (catMaybes
                 [("startCursor" .=) <$> _qStartCursor,
                  ("offset" .=) <$> _qOffSet, ("kind" .=) <$> _qKind,
                  ("distinctOn" .=) <$> _qDistinctOn,
                  ("endCursor" .=) <$> _qEndCursor,
                  ("limit" .=) <$> _qLimit,
                  ("projection" .=) <$> _qProjection,
                  ("filter" .=) <$> _qFilter,
                  ("order" .=) <$> _qOrder])

-- | An array value.
--
-- /See:/ 'arrayValue' smart constructor.
newtype ArrayValue = ArrayValue'
    { _avValues :: Maybe [Value]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ArrayValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avValues'
arrayValue
    :: ArrayValue
arrayValue = 
    ArrayValue'
    { _avValues = Nothing
    }

-- | Values in the array. The order of this array may not be preserved if it
-- contains a mix of indexed and unindexed values.
avValues :: Lens' ArrayValue [Value]
avValues
  = lens _avValues (\ s a -> s{_avValues = a}) .
      _Default
      . _Coerce

instance FromJSON ArrayValue where
        parseJSON
          = withObject "ArrayValue"
              (\ o -> ArrayValue' <$> (o .:? "values" .!= mempty))

instance ToJSON ArrayValue where
        toJSON ArrayValue'{..}
          = object (catMaybes [("values" .=) <$> _avValues])

-- | The result of fetching an entity from Datastore.
--
-- /See:/ 'entityResult' smart constructor.
data EntityResult = EntityResult'
    { _erCursor :: !(Maybe Bytes)
    , _erVersion :: !(Maybe (Textual Int64))
    , _erEntity :: !(Maybe Entity)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EntityResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erCursor'
--
-- * 'erVersion'
--
-- * 'erEntity'
entityResult
    :: EntityResult
entityResult = 
    EntityResult'
    { _erCursor = Nothing
    , _erVersion = Nothing
    , _erEntity = Nothing
    }

-- | A cursor that points to the position after the result entity. Set only
-- when the \`EntityResult\` is part of a \`QueryResultBatch\` message.
erCursor :: Lens' EntityResult (Maybe ByteString)
erCursor
  = lens _erCursor (\ s a -> s{_erCursor = a}) .
      mapping _Bytes

-- | The version of the entity, a strictly positive number that monotonically
-- increases with changes to the entity. This field is set for \`FULL\`
-- entity results. For missing entities in \`LookupResponse\`, this is the
-- version of the snapshot that was used to look up the entity, and it is
-- always set except for eventually consistent reads.
erVersion :: Lens' EntityResult (Maybe Int64)
erVersion
  = lens _erVersion (\ s a -> s{_erVersion = a}) .
      mapping _Coerce

-- | The resulting entity.
erEntity :: Lens' EntityResult (Maybe Entity)
erEntity = lens _erEntity (\ s a -> s{_erEntity = a})

instance FromJSON EntityResult where
        parseJSON
          = withObject "EntityResult"
              (\ o ->
                 EntityResult' <$>
                   (o .:? "cursor") <*> (o .:? "version") <*>
                     (o .:? "entity"))

instance ToJSON EntityResult where
        toJSON EntityResult'{..}
          = object
              (catMaybes
                 [("cursor" .=) <$> _erCursor,
                  ("version" .=) <$> _erVersion,
                  ("entity" .=) <$> _erEntity])

-- | The response for Datastore.Commit.
--
-- /See:/ 'commitResponse' smart constructor.
data CommitResponse = CommitResponse'
    { _crIndexUpdates :: !(Maybe (Textual Int32))
    , _crMutationResults :: !(Maybe [MutationResult])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crIndexUpdates'
--
-- * 'crMutationResults'
commitResponse
    :: CommitResponse
commitResponse = 
    CommitResponse'
    { _crIndexUpdates = Nothing
    , _crMutationResults = Nothing
    }

-- | The number of index entries updated during the commit, or zero if none
-- were updated.
crIndexUpdates :: Lens' CommitResponse (Maybe Int32)
crIndexUpdates
  = lens _crIndexUpdates
      (\ s a -> s{_crIndexUpdates = a})
      . mapping _Coerce

-- | The result of performing the mutations. The i-th mutation result
-- corresponds to the i-th mutation in the request.
crMutationResults :: Lens' CommitResponse [MutationResult]
crMutationResults
  = lens _crMutationResults
      (\ s a -> s{_crMutationResults = a})
      . _Default
      . _Coerce

instance FromJSON CommitResponse where
        parseJSON
          = withObject "CommitResponse"
              (\ o ->
                 CommitResponse' <$>
                   (o .:? "indexUpdates") <*>
                     (o .:? "mutationResults" .!= mempty))

instance ToJSON CommitResponse where
        toJSON CommitResponse'{..}
          = object
              (catMaybes
                 [("indexUpdates" .=) <$> _crIndexUpdates,
                  ("mutationResults" .=) <$> _crMutationResults])

-- | A representation of a kind.
--
-- /See:/ 'kindExpression' smart constructor.
newtype KindExpression = KindExpression'
    { _keName :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'KindExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'keName'
kindExpression
    :: KindExpression
kindExpression = 
    KindExpression'
    { _keName = Nothing
    }

-- | The name of the kind.
keName :: Lens' KindExpression (Maybe Text)
keName = lens _keName (\ s a -> s{_keName = a})

instance FromJSON KindExpression where
        parseJSON
          = withObject "KindExpression"
              (\ o -> KindExpression' <$> (o .:? "name"))

instance ToJSON KindExpression where
        toJSON KindExpression'{..}
          = object (catMaybes [("name" .=) <$> _keName])

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
--
-- /See:/ 'googleLongrunningOperationResponse' smart constructor.
newtype GoogleLongrunningOperationResponse = GoogleLongrunningOperationResponse'
    { _glorAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleLongrunningOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glorAddtional'
googleLongrunningOperationResponse
    :: HashMap Text JSONValue -- ^ 'glorAddtional'
    -> GoogleLongrunningOperationResponse
googleLongrunningOperationResponse pGlorAddtional_ = 
    GoogleLongrunningOperationResponse'
    { _glorAddtional = _Coerce # pGlorAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
glorAddtional :: Lens' GoogleLongrunningOperationResponse (HashMap Text JSONValue)
glorAddtional
  = lens _glorAddtional
      (\ s a -> s{_glorAddtional = a})
      . _Coerce

instance FromJSON GoogleLongrunningOperationResponse
         where
        parseJSON
          = withObject "GoogleLongrunningOperationResponse"
              (\ o ->
                 GoogleLongrunningOperationResponse' <$>
                   (parseJSONObject o))

instance ToJSON GoogleLongrunningOperationResponse
         where
        toJSON = toJSON . _glorAddtional

-- | The options shared by read requests.
--
-- /See:/ 'readOptions' smart constructor.
data ReadOptions = ReadOptions'
    { _roReadConsistency :: !(Maybe ReadOptionsReadConsistency)
    , _roTransaction :: !(Maybe Bytes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReadOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roReadConsistency'
--
-- * 'roTransaction'
readOptions
    :: ReadOptions
readOptions = 
    ReadOptions'
    { _roReadConsistency = Nothing
    , _roTransaction = Nothing
    }

-- | The non-transactional read consistency to use. Cannot be set to
-- \`STRONG\` for global queries.
roReadConsistency :: Lens' ReadOptions (Maybe ReadOptionsReadConsistency)
roReadConsistency
  = lens _roReadConsistency
      (\ s a -> s{_roReadConsistency = a})

-- | The identifier of the transaction in which to read. A transaction
-- identifier is returned by a call to Datastore.BeginTransaction.
roTransaction :: Lens' ReadOptions (Maybe ByteString)
roTransaction
  = lens _roTransaction
      (\ s a -> s{_roTransaction = a})
      . mapping _Bytes

instance FromJSON ReadOptions where
        parseJSON
          = withObject "ReadOptions"
              (\ o ->
                 ReadOptions' <$>
                   (o .:? "readConsistency") <*> (o .:? "transaction"))

instance ToJSON ReadOptions where
        toJSON ReadOptions'{..}
          = object
              (catMaybes
                 [("readConsistency" .=) <$> _roReadConsistency,
                  ("transaction" .=) <$> _roTransaction])

-- | The response for Datastore.Rollback. (an empty message).
--
-- /See:/ 'rollbackResponse' smart constructor.
data RollbackResponse =
    RollbackResponse' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RollbackResponse' with the minimum fields required to make a request.
--
rollbackResponse
    :: RollbackResponse
rollbackResponse = RollbackResponse'

instance FromJSON RollbackResponse where
        parseJSON
          = withObject "RollbackResponse"
              (\ o -> pure RollbackResponse')

instance ToJSON RollbackResponse where
        toJSON = const emptyObject

-- | A representation of a property in a projection.
--
-- /See:/ 'projection' smart constructor.
newtype Projection = Projection'
    { _pProperty :: Maybe PropertyReference
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Projection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pProperty'
projection
    :: Projection
projection = 
    Projection'
    { _pProperty = Nothing
    }

-- | The property to project.
pProperty :: Lens' Projection (Maybe PropertyReference)
pProperty
  = lens _pProperty (\ s a -> s{_pProperty = a})

instance FromJSON Projection where
        parseJSON
          = withObject "Projection"
              (\ o -> Projection' <$> (o .:? "property"))

instance ToJSON Projection where
        toJSON Projection'{..}
          = object (catMaybes [("property" .=) <$> _pProperty])

-- | The response for Datastore.ReserveIds.
--
-- /See:/ 'reserveIdsResponse' smart constructor.
data ReserveIdsResponse =
    ReserveIdsResponse' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReserveIdsResponse' with the minimum fields required to make a request.
--
reserveIdsResponse
    :: ReserveIdsResponse
reserveIdsResponse = ReserveIdsResponse'

instance FromJSON ReserveIdsResponse where
        parseJSON
          = withObject "ReserveIdsResponse"
              (\ o -> pure ReserveIdsResponse')

instance ToJSON ReserveIdsResponse where
        toJSON = const emptyObject

-- | A holder for any type of filter.
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
    { _fCompositeFilter :: !(Maybe CompositeFilter)
    , _fPropertyFilter :: !(Maybe PropertyFilter)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fCompositeFilter'
--
-- * 'fPropertyFilter'
filter'
    :: Filter
filter' = 
    Filter'
    { _fCompositeFilter = Nothing
    , _fPropertyFilter = Nothing
    }

-- | A composite filter.
fCompositeFilter :: Lens' Filter (Maybe CompositeFilter)
fCompositeFilter
  = lens _fCompositeFilter
      (\ s a -> s{_fCompositeFilter = a})

-- | A filter on a property.
fPropertyFilter :: Lens' Filter (Maybe PropertyFilter)
fPropertyFilter
  = lens _fPropertyFilter
      (\ s a -> s{_fPropertyFilter = a})

instance FromJSON Filter where
        parseJSON
          = withObject "Filter"
              (\ o ->
                 Filter' <$>
                   (o .:? "compositeFilter") <*>
                     (o .:? "propertyFilter"))

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [("compositeFilter" .=) <$> _fCompositeFilter,
                  ("propertyFilter" .=) <$> _fPropertyFilter])

-- | The client-assigned labels which were provided when the operation was
-- created. May also include additional labels.
--
-- /See:/ 'googleDatastoreAdminV1beta1CommonMetadataLabels' smart constructor.
newtype GoogleDatastoreAdminV1beta1CommonMetadataLabels = GoogleDatastoreAdminV1beta1CommonMetadataLabels'
    { _gdavcmlAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1CommonMetadataLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdavcmlAddtional'
googleDatastoreAdminV1beta1CommonMetadataLabels
    :: HashMap Text Text -- ^ 'gdavcmlAddtional'
    -> GoogleDatastoreAdminV1beta1CommonMetadataLabels
googleDatastoreAdminV1beta1CommonMetadataLabels pGdavcmlAddtional_ = 
    GoogleDatastoreAdminV1beta1CommonMetadataLabels'
    { _gdavcmlAddtional = _Coerce # pGdavcmlAddtional_
    }

gdavcmlAddtional :: Lens' GoogleDatastoreAdminV1beta1CommonMetadataLabels (HashMap Text Text)
gdavcmlAddtional
  = lens _gdavcmlAddtional
      (\ s a -> s{_gdavcmlAddtional = a})
      . _Coerce

instance FromJSON
         GoogleDatastoreAdminV1beta1CommonMetadataLabels where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1CommonMetadataLabels"
              (\ o ->
                 GoogleDatastoreAdminV1beta1CommonMetadataLabels' <$>
                   (parseJSONObject o))

instance ToJSON
         GoogleDatastoreAdminV1beta1CommonMetadataLabels where
        toJSON = toJSON . _gdavcmlAddtional

-- | The request for Datastore.Commit.
--
-- /See:/ 'commitRequest' smart constructor.
data CommitRequest = CommitRequest'
    { _crMutations :: !(Maybe [Mutation])
    , _crMode :: !(Maybe CommitRequestMode)
    , _crTransaction :: !(Maybe Bytes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommitRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crMutations'
--
-- * 'crMode'
--
-- * 'crTransaction'
commitRequest
    :: CommitRequest
commitRequest = 
    CommitRequest'
    { _crMutations = Nothing
    , _crMode = Nothing
    , _crTransaction = Nothing
    }

-- | The mutations to perform. When mode is \`TRANSACTIONAL\`, mutations
-- affecting a single entity are applied in order. The following sequences
-- of mutations affecting a single entity are not permitted in a single
-- \`Commit\` request: - \`insert\` followed by \`insert\` - \`update\`
-- followed by \`insert\` - \`upsert\` followed by \`insert\` - \`delete\`
-- followed by \`update\` When mode is \`NON_TRANSACTIONAL\`, no two
-- mutations may affect a single entity.
crMutations :: Lens' CommitRequest [Mutation]
crMutations
  = lens _crMutations (\ s a -> s{_crMutations = a}) .
      _Default
      . _Coerce

-- | The type of commit to perform. Defaults to \`TRANSACTIONAL\`.
crMode :: Lens' CommitRequest (Maybe CommitRequestMode)
crMode = lens _crMode (\ s a -> s{_crMode = a})

-- | The identifier of the transaction associated with the commit. A
-- transaction identifier is returned by a call to
-- Datastore.BeginTransaction.
crTransaction :: Lens' CommitRequest (Maybe ByteString)
crTransaction
  = lens _crTransaction
      (\ s a -> s{_crTransaction = a})
      . mapping _Bytes

instance FromJSON CommitRequest where
        parseJSON
          = withObject "CommitRequest"
              (\ o ->
                 CommitRequest' <$>
                   (o .:? "mutations" .!= mempty) <*> (o .:? "mode") <*>
                     (o .:? "transaction"))

instance ToJSON CommitRequest where
        toJSON CommitRequest'{..}
          = object
              (catMaybes
                 [("mutations" .=) <$> _crMutations,
                  ("mode" .=) <$> _crMode,
                  ("transaction" .=) <$> _crTransaction])

-- | The response message for Operations.ListOperations.
--
-- /See:/ 'googleLongrunningListOperationsResponse' smart constructor.
data GoogleLongrunningListOperationsResponse = GoogleLongrunningListOperationsResponse'
    { _gllorNextPageToken :: !(Maybe Text)
    , _gllorOperations :: !(Maybe [GoogleLongrunningOperation])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleLongrunningListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gllorNextPageToken'
--
-- * 'gllorOperations'
googleLongrunningListOperationsResponse
    :: GoogleLongrunningListOperationsResponse
googleLongrunningListOperationsResponse = 
    GoogleLongrunningListOperationsResponse'
    { _gllorNextPageToken = Nothing
    , _gllorOperations = Nothing
    }

-- | The standard List next-page token.
gllorNextPageToken :: Lens' GoogleLongrunningListOperationsResponse (Maybe Text)
gllorNextPageToken
  = lens _gllorNextPageToken
      (\ s a -> s{_gllorNextPageToken = a})

-- | A list of operations that matches the specified filter in the request.
gllorOperations :: Lens' GoogleLongrunningListOperationsResponse [GoogleLongrunningOperation]
gllorOperations
  = lens _gllorOperations
      (\ s a -> s{_gllorOperations = a})
      . _Default
      . _Coerce

instance FromJSON
         GoogleLongrunningListOperationsResponse where
        parseJSON
          = withObject
              "GoogleLongrunningListOperationsResponse"
              (\ o ->
                 GoogleLongrunningListOperationsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "operations" .!= mempty))

instance ToJSON
         GoogleLongrunningListOperationsResponse where
        toJSON GoogleLongrunningListOperationsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gllorNextPageToken,
                  ("operations" .=) <$> _gllorOperations])

-- | A (kind, ID\/name) pair used to construct a key path. If either name or
-- ID is set, the element is complete. If neither is set, the element is
-- incomplete.
--
-- /See:/ 'pathElement' smart constructor.
data PathElement = PathElement'
    { _peKind :: !(Maybe Text)
    , _peName :: !(Maybe Text)
    , _peId :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PathElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peKind'
--
-- * 'peName'
--
-- * 'peId'
pathElement
    :: PathElement
pathElement = 
    PathElement'
    { _peKind = Nothing
    , _peName = Nothing
    , _peId = Nothing
    }

-- | The kind of the entity. A kind matching regex \`__.*__\` is
-- reserved\/read-only. A kind must not contain more than 1500 bytes when
-- UTF-8 encoded. Cannot be \`\"\"\`.
peKind :: Lens' PathElement (Maybe Text)
peKind = lens _peKind (\ s a -> s{_peKind = a})

-- | The name of the entity. A name matching regex \`__.*__\` is
-- reserved\/read-only. A name must not be more than 1500 bytes when UTF-8
-- encoded. Cannot be \`\"\"\`.
peName :: Lens' PathElement (Maybe Text)
peName = lens _peName (\ s a -> s{_peName = a})

-- | The auto-allocated ID of the entity. Never equal to zero. Values less
-- than zero are discouraged and may not be supported in the future.
peId :: Lens' PathElement (Maybe Int64)
peId
  = lens _peId (\ s a -> s{_peId = a}) .
      mapping _Coerce

instance FromJSON PathElement where
        parseJSON
          = withObject "PathElement"
              (\ o ->
                 PathElement' <$>
                   (o .:? "kind") <*> (o .:? "name") <*> (o .:? "id"))

instance ToJSON PathElement where
        toJSON PathElement'{..}
          = object
              (catMaybes
                 [("kind" .=) <$> _peKind, ("name" .=) <$> _peName,
                  ("id" .=) <$> _peId])

-- | A Datastore data object. An entity is limited to 1 megabyte when stored.
-- That _roughly_ corresponds to a limit of 1 megabyte for the serialized
-- form of this message.
--
-- /See:/ 'entity' smart constructor.
data Entity = Entity'
    { _eKey :: !(Maybe Key)
    , _eProperties :: !(Maybe EntityProperties)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Entity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eKey'
--
-- * 'eProperties'
entity
    :: Entity
entity = 
    Entity'
    { _eKey = Nothing
    , _eProperties = Nothing
    }

-- | The entity\'s key. An entity must have a key, unless otherwise
-- documented (for example, an entity in \`Value.entity_value\` may have no
-- key). An entity\'s kind is its key path\'s last element\'s kind, or null
-- if it has no key.
eKey :: Lens' Entity (Maybe Key)
eKey = lens _eKey (\ s a -> s{_eKey = a})

-- | The entity\'s properties. The map\'s keys are property names. A property
-- name matching regex \`__.*__\` is reserved. A reserved property name is
-- forbidden in certain documented contexts. The name must not contain more
-- than 500 characters. The name cannot be \`\"\"\`.
eProperties :: Lens' Entity (Maybe EntityProperties)
eProperties
  = lens _eProperties (\ s a -> s{_eProperties = a})

instance FromJSON Entity where
        parseJSON
          = withObject "Entity"
              (\ o ->
                 Entity' <$> (o .:? "key") <*> (o .:? "properties"))

instance ToJSON Entity where
        toJSON Entity'{..}
          = object
              (catMaybes
                 [("key" .=) <$> _eKey,
                  ("properties" .=) <$> _eProperties])

-- | Identifies a subset of entities in a project. This is specified as
-- combinations of kinds and namespaces (either or both of which may be
-- all, as described in the following examples). Example usage: Entire
-- project: kinds=[], namespace_ids=[] Kinds Foo and Bar in all namespaces:
-- kinds=[\'Foo\', \'Bar\'], namespace_ids=[] Kinds Foo and Bar only in the
-- default namespace: kinds=[\'Foo\', \'Bar\'], namespace_ids=[\'\'] Kinds
-- Foo and Bar in both the default and Baz namespaces: kinds=[\'Foo\',
-- \'Bar\'], namespace_ids=[\'\', \'Baz\'] The entire Baz namespace:
-- kinds=[], namespace_ids=[\'Baz\']
--
-- /See:/ 'googleDatastoreAdminV1beta1EntityFilter' smart constructor.
data GoogleDatastoreAdminV1beta1EntityFilter = GoogleDatastoreAdminV1beta1EntityFilter'
    { _gdavefNamespaceIds :: !(Maybe [Text])
    , _gdavefKinds :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1EntityFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdavefNamespaceIds'
--
-- * 'gdavefKinds'
googleDatastoreAdminV1beta1EntityFilter
    :: GoogleDatastoreAdminV1beta1EntityFilter
googleDatastoreAdminV1beta1EntityFilter = 
    GoogleDatastoreAdminV1beta1EntityFilter'
    { _gdavefNamespaceIds = Nothing
    , _gdavefKinds = Nothing
    }

-- | An empty list represents all namespaces. This is the preferred usage for
-- projects that don\'t use namespaces. An empty string element represents
-- the default namespace. This should be used if the project has data in
-- non-default namespaces, but doesn\'t want to include them. Each
-- namespace in this list must be unique.
gdavefNamespaceIds :: Lens' GoogleDatastoreAdminV1beta1EntityFilter [Text]
gdavefNamespaceIds
  = lens _gdavefNamespaceIds
      (\ s a -> s{_gdavefNamespaceIds = a})
      . _Default
      . _Coerce

-- | If empty, then this represents all kinds.
gdavefKinds :: Lens' GoogleDatastoreAdminV1beta1EntityFilter [Text]
gdavefKinds
  = lens _gdavefKinds (\ s a -> s{_gdavefKinds = a}) .
      _Default
      . _Coerce

instance FromJSON
         GoogleDatastoreAdminV1beta1EntityFilter where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1EntityFilter"
              (\ o ->
                 GoogleDatastoreAdminV1beta1EntityFilter' <$>
                   (o .:? "namespaceIds" .!= mempty) <*>
                     (o .:? "kinds" .!= mempty))

instance ToJSON
         GoogleDatastoreAdminV1beta1EntityFilter where
        toJSON GoogleDatastoreAdminV1beta1EntityFilter'{..}
          = object
              (catMaybes
                 [("namespaceIds" .=) <$> _gdavefNamespaceIds,
                  ("kinds" .=) <$> _gdavefKinds])

-- | Options specific to read-only transactions.
--
-- /See:/ 'readOnly' smart constructor.
data ReadOnly =
    ReadOnly' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReadOnly' with the minimum fields required to make a request.
--
readOnly
    :: ReadOnly
readOnly = ReadOnly'

instance FromJSON ReadOnly where
        parseJSON
          = withObject "ReadOnly" (\ o -> pure ReadOnly')

instance ToJSON ReadOnly where
        toJSON = const emptyObject

-- | The response for Datastore.Lookup.
--
-- /See:/ 'lookupResponse' smart constructor.
data LookupResponse = LookupResponse'
    { _lrDeferred :: !(Maybe [Key])
    , _lrFound :: !(Maybe [EntityResult])
    , _lrMissing :: !(Maybe [EntityResult])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LookupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrDeferred'
--
-- * 'lrFound'
--
-- * 'lrMissing'
lookupResponse
    :: LookupResponse
lookupResponse = 
    LookupResponse'
    { _lrDeferred = Nothing
    , _lrFound = Nothing
    , _lrMissing = Nothing
    }

-- | A list of keys that were not looked up due to resource constraints. The
-- order of results in this field is undefined and has no relation to the
-- order of the keys in the input.
lrDeferred :: Lens' LookupResponse [Key]
lrDeferred
  = lens _lrDeferred (\ s a -> s{_lrDeferred = a}) .
      _Default
      . _Coerce

-- | Entities found as \`ResultType.FULL\` entities. The order of results in
-- this field is undefined and has no relation to the order of the keys in
-- the input.
lrFound :: Lens' LookupResponse [EntityResult]
lrFound
  = lens _lrFound (\ s a -> s{_lrFound = a}) . _Default
      . _Coerce

-- | Entities not found as \`ResultType.KEY_ONLY\` entities. The order of
-- results in this field is undefined and has no relation to the order of
-- the keys in the input.
lrMissing :: Lens' LookupResponse [EntityResult]
lrMissing
  = lens _lrMissing (\ s a -> s{_lrMissing = a}) .
      _Default
      . _Coerce

instance FromJSON LookupResponse where
        parseJSON
          = withObject "LookupResponse"
              (\ o ->
                 LookupResponse' <$>
                   (o .:? "deferred" .!= mempty) <*>
                     (o .:? "found" .!= mempty)
                     <*> (o .:? "missing" .!= mempty))

instance ToJSON LookupResponse where
        toJSON LookupResponse'{..}
          = object
              (catMaybes
                 [("deferred" .=) <$> _lrDeferred,
                  ("found" .=) <$> _lrFound,
                  ("missing" .=) <$> _lrMissing])

-- | This resource represents a long-running operation that is the result of
-- a network API call.
--
-- /See:/ 'googleLongrunningOperation' smart constructor.
data GoogleLongrunningOperation = GoogleLongrunningOperation'
    { _gloDone :: !(Maybe Bool)
    , _gloError :: !(Maybe Status)
    , _gloResponse :: !(Maybe GoogleLongrunningOperationResponse)
    , _gloName :: !(Maybe Text)
    , _gloMetadata :: !(Maybe GoogleLongrunningOperationMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleLongrunningOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gloDone'
--
-- * 'gloError'
--
-- * 'gloResponse'
--
-- * 'gloName'
--
-- * 'gloMetadata'
googleLongrunningOperation
    :: GoogleLongrunningOperation
googleLongrunningOperation = 
    GoogleLongrunningOperation'
    { _gloDone = Nothing
    , _gloError = Nothing
    , _gloResponse = Nothing
    , _gloName = Nothing
    , _gloMetadata = Nothing
    }

-- | If the value is \`false\`, it means the operation is still in progress.
-- If \`true\`, the operation is completed, and either \`error\` or
-- \`response\` is available.
gloDone :: Lens' GoogleLongrunningOperation (Maybe Bool)
gloDone = lens _gloDone (\ s a -> s{_gloDone = a})

-- | The error result of the operation in case of failure or cancellation.
gloError :: Lens' GoogleLongrunningOperation (Maybe Status)
gloError = lens _gloError (\ s a -> s{_gloError = a})

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
gloResponse :: Lens' GoogleLongrunningOperation (Maybe GoogleLongrunningOperationResponse)
gloResponse
  = lens _gloResponse (\ s a -> s{_gloResponse = a})

-- | The server-assigned name, which is only unique within the same service
-- that originally returns it. If you use the default HTTP mapping, the
-- \`name\` should have the format of \`operations\/some\/unique\/name\`.
gloName :: Lens' GoogleLongrunningOperation (Maybe Text)
gloName = lens _gloName (\ s a -> s{_gloName = a})

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
gloMetadata :: Lens' GoogleLongrunningOperation (Maybe GoogleLongrunningOperationMetadata)
gloMetadata
  = lens _gloMetadata (\ s a -> s{_gloMetadata = a})

instance FromJSON GoogleLongrunningOperation where
        parseJSON
          = withObject "GoogleLongrunningOperation"
              (\ o ->
                 GoogleLongrunningOperation' <$>
                   (o .:? "done") <*> (o .:? "error") <*>
                     (o .:? "response")
                     <*> (o .:? "name")
                     <*> (o .:? "metadata"))

instance ToJSON GoogleLongrunningOperation where
        toJSON GoogleLongrunningOperation'{..}
          = object
              (catMaybes
                 [("done" .=) <$> _gloDone,
                  ("error" .=) <$> _gloError,
                  ("response" .=) <$> _gloResponse,
                  ("name" .=) <$> _gloName,
                  ("metadata" .=) <$> _gloMetadata])

-- | The desired order for a specific property.
--
-- /See:/ 'propertyOrder' smart constructor.
data PropertyOrder = PropertyOrder'
    { _poProperty :: !(Maybe PropertyReference)
    , _poDirection :: !(Maybe PropertyOrderDirection)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PropertyOrder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poProperty'
--
-- * 'poDirection'
propertyOrder
    :: PropertyOrder
propertyOrder = 
    PropertyOrder'
    { _poProperty = Nothing
    , _poDirection = Nothing
    }

-- | The property to order by.
poProperty :: Lens' PropertyOrder (Maybe PropertyReference)
poProperty
  = lens _poProperty (\ s a -> s{_poProperty = a})

-- | The direction to order by. Defaults to \`ASCENDING\`.
poDirection :: Lens' PropertyOrder (Maybe PropertyOrderDirection)
poDirection
  = lens _poDirection (\ s a -> s{_poDirection = a})

instance FromJSON PropertyOrder where
        parseJSON
          = withObject "PropertyOrder"
              (\ o ->
                 PropertyOrder' <$>
                   (o .:? "property") <*> (o .:? "direction"))

instance ToJSON PropertyOrder where
        toJSON PropertyOrder'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _poProperty,
                  ("direction" .=) <$> _poDirection])

-- | Metadata for ExportEntities operations.
--
-- /See:/ 'googleDatastoreAdminV1beta1ExportEntitiesMetadata' smart constructor.
data GoogleDatastoreAdminV1beta1ExportEntitiesMetadata = GoogleDatastoreAdminV1beta1ExportEntitiesMetadata'
    { _gdaveemProgressBytes :: !(Maybe GoogleDatastoreAdminV1beta1Progress)
    , _gdaveemOutputURLPrefix :: !(Maybe Text)
    , _gdaveemProgressEntities :: !(Maybe GoogleDatastoreAdminV1beta1Progress)
    , _gdaveemEntityFilter :: !(Maybe GoogleDatastoreAdminV1beta1EntityFilter)
    , _gdaveemCommon :: !(Maybe GoogleDatastoreAdminV1beta1CommonMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GoogleDatastoreAdminV1beta1ExportEntitiesMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdaveemProgressBytes'
--
-- * 'gdaveemOutputURLPrefix'
--
-- * 'gdaveemProgressEntities'
--
-- * 'gdaveemEntityFilter'
--
-- * 'gdaveemCommon'
googleDatastoreAdminV1beta1ExportEntitiesMetadata
    :: GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
googleDatastoreAdminV1beta1ExportEntitiesMetadata = 
    GoogleDatastoreAdminV1beta1ExportEntitiesMetadata'
    { _gdaveemProgressBytes = Nothing
    , _gdaveemOutputURLPrefix = Nothing
    , _gdaveemProgressEntities = Nothing
    , _gdaveemEntityFilter = Nothing
    , _gdaveemCommon = Nothing
    }

-- | An estimate of the number of bytes processed.
gdaveemProgressBytes :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1Progress)
gdaveemProgressBytes
  = lens _gdaveemProgressBytes
      (\ s a -> s{_gdaveemProgressBytes = a})

-- | Location for the export metadata and data files. This will be the same
-- value as the
-- google.datastore.admin.v1beta1.ExportEntitiesRequest.output_url_prefix
-- field. The final output location is provided in
-- google.datastore.admin.v1beta1.ExportEntitiesResponse.output_url.
gdaveemOutputURLPrefix :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesMetadata (Maybe Text)
gdaveemOutputURLPrefix
  = lens _gdaveemOutputURLPrefix
      (\ s a -> s{_gdaveemOutputURLPrefix = a})

-- | An estimate of the number of entities processed.
gdaveemProgressEntities :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1Progress)
gdaveemProgressEntities
  = lens _gdaveemProgressEntities
      (\ s a -> s{_gdaveemProgressEntities = a})

-- | Description of which entities are being exported.
gdaveemEntityFilter :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1EntityFilter)
gdaveemEntityFilter
  = lens _gdaveemEntityFilter
      (\ s a -> s{_gdaveemEntityFilter = a})

-- | Metadata common to all Datastore Admin operations.
gdaveemCommon :: Lens' GoogleDatastoreAdminV1beta1ExportEntitiesMetadata (Maybe GoogleDatastoreAdminV1beta1CommonMetadata)
gdaveemCommon
  = lens _gdaveemCommon
      (\ s a -> s{_gdaveemCommon = a})

instance FromJSON
         GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
         where
        parseJSON
          = withObject
              "GoogleDatastoreAdminV1beta1ExportEntitiesMetadata"
              (\ o ->
                 GoogleDatastoreAdminV1beta1ExportEntitiesMetadata'
                   <$>
                   (o .:? "progressBytes") <*> (o .:? "outputUrlPrefix")
                     <*> (o .:? "progressEntities")
                     <*> (o .:? "entityFilter")
                     <*> (o .:? "common"))

instance ToJSON
         GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
         where
        toJSON
          GoogleDatastoreAdminV1beta1ExportEntitiesMetadata'{..}
          = object
              (catMaybes
                 [("progressBytes" .=) <$> _gdaveemProgressBytes,
                  ("outputUrlPrefix" .=) <$> _gdaveemOutputURLPrefix,
                  ("progressEntities" .=) <$> _gdaveemProgressEntities,
                  ("entityFilter" .=) <$> _gdaveemEntityFilter,
                  ("common" .=) <$> _gdaveemCommon])

-- | A binding parameter for a GQL query.
--
-- /See:/ 'gqlQueryParameter' smart constructor.
data GqlQueryParameter = GqlQueryParameter'
    { _gqpCursor :: !(Maybe Bytes)
    , _gqpValue :: !(Maybe Value)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GqlQueryParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqpCursor'
--
-- * 'gqpValue'
gqlQueryParameter
    :: GqlQueryParameter
gqlQueryParameter = 
    GqlQueryParameter'
    { _gqpCursor = Nothing
    , _gqpValue = Nothing
    }

-- | A query cursor. Query cursors are returned in query result batches.
gqpCursor :: Lens' GqlQueryParameter (Maybe ByteString)
gqpCursor
  = lens _gqpCursor (\ s a -> s{_gqpCursor = a}) .
      mapping _Bytes

-- | A value parameter.
gqpValue :: Lens' GqlQueryParameter (Maybe Value)
gqpValue = lens _gqpValue (\ s a -> s{_gqpValue = a})

instance FromJSON GqlQueryParameter where
        parseJSON
          = withObject "GqlQueryParameter"
              (\ o ->
                 GqlQueryParameter' <$>
                   (o .:? "cursor") <*> (o .:? "value"))

instance ToJSON GqlQueryParameter where
        toJSON GqlQueryParameter'{..}
          = object
              (catMaybes
                 [("cursor" .=) <$> _gqpCursor,
                  ("value" .=) <$> _gqpValue])
