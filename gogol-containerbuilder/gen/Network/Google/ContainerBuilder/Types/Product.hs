{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ContainerBuilder.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ContainerBuilder.Types.Product where

import Network.Google.ContainerBuilder.Types.Sum
import Network.Google.Prelude

-- | A step in the build pipeline.
--
-- /See:/ 'buildStep' smart constructor.
data BuildStep = BuildStep'
    { _bsStatus :: !(Maybe BuildStepStatus)
    , _bsDir :: !(Maybe Text)
    , _bsArgs :: !(Maybe [Text])
    , _bsEnv :: !(Maybe [Text])
    , _bsEntrypoint :: !(Maybe Text)
    , _bsWaitFor :: !(Maybe [Text])
    , _bsName :: !(Maybe Text)
    , _bsId :: !(Maybe Text)
    , _bsTiming :: !(Maybe TimeSpan)
    , _bsSecretEnv :: !(Maybe [Text])
    , _bsTimeout :: !(Maybe Duration)
    , _bsVolumes :: !(Maybe [Volume])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildStep' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsStatus'
--
-- * 'bsDir'
--
-- * 'bsArgs'
--
-- * 'bsEnv'
--
-- * 'bsEntrypoint'
--
-- * 'bsWaitFor'
--
-- * 'bsName'
--
-- * 'bsId'
--
-- * 'bsTiming'
--
-- * 'bsSecretEnv'
--
-- * 'bsTimeout'
--
-- * 'bsVolumes'
buildStep
    :: BuildStep
buildStep = 
    BuildStep'
    { _bsStatus = Nothing
    , _bsDir = Nothing
    , _bsArgs = Nothing
    , _bsEnv = Nothing
    , _bsEntrypoint = Nothing
    , _bsWaitFor = Nothing
    , _bsName = Nothing
    , _bsId = Nothing
    , _bsTiming = Nothing
    , _bsSecretEnv = Nothing
    , _bsTimeout = Nothing
    , _bsVolumes = Nothing
    }

-- | Status of the build step. At this time, build step status is only
-- updated on build completion; step status is not updated in real-time as
-- the build progresses. \'OutputOnly
bsStatus :: Lens' BuildStep (Maybe BuildStepStatus)
bsStatus = lens _bsStatus (\ s a -> s{_bsStatus = a})

-- | Working directory to use when running this step\'s container. If this
-- value is a relative path, it is relative to the build\'s working
-- directory. If this value is absolute, it may be outside the build\'s
-- working directory, in which case the contents of the path may not be
-- persisted across build step executions, unless a \`volume\` for that
-- path is specified. If the build specifies a \`RepoSource\` with \`dir\`
-- and a step with a \`dir\`, which specifies an absolute path, the
-- \`RepoSource\` \`dir\` is ignored for the step\'s execution.
bsDir :: Lens' BuildStep (Maybe Text)
bsDir = lens _bsDir (\ s a -> s{_bsDir = a})

-- | A list of arguments that will be presented to the step when it is
-- started. If the image used to run the step\'s container has an
-- entrypoint, the \`args\` are used as arguments to that entrypoint. If
-- the image does not define an entrypoint, the first element in args is
-- used as the entrypoint, and the remainder will be used as arguments.
bsArgs :: Lens' BuildStep [Text]
bsArgs
  = lens _bsArgs (\ s a -> s{_bsArgs = a}) . _Default .
      _Coerce

-- | A list of environment variable definitions to be used when running a
-- step. The elements are of the form \"KEY=VALUE\" for the environment
-- variable \"KEY\" being given the value \"VALUE\".
bsEnv :: Lens' BuildStep [Text]
bsEnv
  = lens _bsEnv (\ s a -> s{_bsEnv = a}) . _Default .
      _Coerce

-- | Entrypoint to be used instead of the build step image\'s default
-- entrypoint. If unset, the image\'s default entrypoint is used.
bsEntrypoint :: Lens' BuildStep (Maybe Text)
bsEntrypoint
  = lens _bsEntrypoint (\ s a -> s{_bsEntrypoint = a})

-- | The ID(s) of the step(s) that this build step depends on. This build
-- step will not start until all the build steps in \`wait_for\` have
-- completed successfully. If \`wait_for\` is empty, this build step will
-- start when all previous build steps in the \`Build.Steps\` list have
-- completed successfully.
bsWaitFor :: Lens' BuildStep [Text]
bsWaitFor
  = lens _bsWaitFor (\ s a -> s{_bsWaitFor = a}) .
      _Default
      . _Coerce

-- | Required. The name of the container image that will run this particular
-- build step. If the image is available in the host\'s Docker daemon\'s
-- cache, it will be run directly. If not, the host will attempt to pull
-- the image first, using the builder service account\'s credentials if
-- necessary. The Docker daemon\'s cache will already have the latest
-- versions of all of the officially supported build steps
-- ([https:\/\/github.com\/GoogleCloudPlatform\/cloud-builders](https:\/\/github.com\/GoogleCloudPlatform\/cloud-builders)).
-- The Docker daemon will also have cached many of the layers for some
-- popular images, like \"ubuntu\", \"debian\", but they will be refreshed
-- at the time you attempt to use them. If you built an image in a previous
-- build step, it will be stored in the host\'s Docker daemon\'s cache and
-- is available to use as the name for a later build step.
bsName :: Lens' BuildStep (Maybe Text)
bsName = lens _bsName (\ s a -> s{_bsName = a})

-- | Unique identifier for this build step, used in \`wait_for\` to reference
-- this build step as a dependency.
bsId :: Lens' BuildStep (Maybe Text)
bsId = lens _bsId (\ s a -> s{_bsId = a})

-- | Stores timing information for executing this build step. \'OutputOnly
bsTiming :: Lens' BuildStep (Maybe TimeSpan)
bsTiming = lens _bsTiming (\ s a -> s{_bsTiming = a})

-- | A list of environment variables which are encrypted using a Cloud Key
-- Management Service crypto key. These values must be specified in the
-- build\'s \`Secret\`.
bsSecretEnv :: Lens' BuildStep [Text]
bsSecretEnv
  = lens _bsSecretEnv (\ s a -> s{_bsSecretEnv = a}) .
      _Default
      . _Coerce

-- | Time limit for executing this build step. If not defined, the step has
-- no time limit and will be allowed to continue to run until either it
-- completes or the build itself times out.
bsTimeout :: Lens' BuildStep (Maybe Scientific)
bsTimeout
  = lens _bsTimeout (\ s a -> s{_bsTimeout = a}) .
      mapping _Duration

-- | List of volumes to mount into the build step. Each volume will be
-- created as an empty volume prior to execution of the build step. Upon
-- completion of the build, volumes and their contents will be discarded.
-- Using a named volume in only one step is not valid as it is indicative
-- of a mis-configured build request.
bsVolumes :: Lens' BuildStep [Volume]
bsVolumes
  = lens _bsVolumes (\ s a -> s{_bsVolumes = a}) .
      _Default
      . _Coerce

instance FromJSON BuildStep where
        parseJSON
          = withObject "BuildStep"
              (\ o ->
                 BuildStep' <$>
                   (o .:? "status") <*> (o .:? "dir") <*>
                     (o .:? "args" .!= mempty)
                     <*> (o .:? "env" .!= mempty)
                     <*> (o .:? "entrypoint")
                     <*> (o .:? "waitFor" .!= mempty)
                     <*> (o .:? "name")
                     <*> (o .:? "id")
                     <*> (o .:? "timing")
                     <*> (o .:? "secretEnv" .!= mempty)
                     <*> (o .:? "timeout")
                     <*> (o .:? "volumes" .!= mempty))

instance ToJSON BuildStep where
        toJSON BuildStep'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _bsStatus, ("dir" .=) <$> _bsDir,
                  ("args" .=) <$> _bsArgs, ("env" .=) <$> _bsEnv,
                  ("entrypoint" .=) <$> _bsEntrypoint,
                  ("waitFor" .=) <$> _bsWaitFor,
                  ("name" .=) <$> _bsName, ("id" .=) <$> _bsId,
                  ("timing" .=) <$> _bsTiming,
                  ("secretEnv" .=) <$> _bsSecretEnv,
                  ("timeout" .=) <$> _bsTimeout,
                  ("volumes" .=) <$> _bsVolumes])

-- | Provenance of the source. Ways to find the original source, or verify
-- that some source was used for this build.
--
-- /See:/ 'sourceProvenance' smart constructor.
data SourceProvenance = SourceProvenance'
    { _spResolvedRepoSource :: !(Maybe RepoSource)
    , _spResolvedStorageSource :: !(Maybe StorageSource)
    , _spFileHashes :: !(Maybe SourceProvenanceFileHashes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceProvenance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spResolvedRepoSource'
--
-- * 'spResolvedStorageSource'
--
-- * 'spFileHashes'
sourceProvenance
    :: SourceProvenance
sourceProvenance = 
    SourceProvenance'
    { _spResolvedRepoSource = Nothing
    , _spResolvedStorageSource = Nothing
    , _spFileHashes = Nothing
    }

-- | A copy of the build\'s \`source.repo_source\`, if exists, with any
-- revisions resolved.
spResolvedRepoSource :: Lens' SourceProvenance (Maybe RepoSource)
spResolvedRepoSource
  = lens _spResolvedRepoSource
      (\ s a -> s{_spResolvedRepoSource = a})

-- | A copy of the build\'s \`source.storage_source\`, if exists, with any
-- generations resolved.
spResolvedStorageSource :: Lens' SourceProvenance (Maybe StorageSource)
spResolvedStorageSource
  = lens _spResolvedStorageSource
      (\ s a -> s{_spResolvedStorageSource = a})

-- | Hash(es) of the build source, which can be used to verify that the
-- original source integrity was maintained in the build. Note that
-- \`FileHashes\` will only be populated if \`BuildOptions\` has requested
-- a \`SourceProvenanceHash\`. The keys to this map are file paths used as
-- build source and the values contain the hash values for those files. If
-- the build source came in a single package such as a gzipped tarfile
-- (\`.tar.gz\`), the \`FileHash\` will be for the single path to that
-- file. \'OutputOnly
spFileHashes :: Lens' SourceProvenance (Maybe SourceProvenanceFileHashes)
spFileHashes
  = lens _spFileHashes (\ s a -> s{_spFileHashes = a})

instance FromJSON SourceProvenance where
        parseJSON
          = withObject "SourceProvenance"
              (\ o ->
                 SourceProvenance' <$>
                   (o .:? "resolvedRepoSource") <*>
                     (o .:? "resolvedStorageSource")
                     <*> (o .:? "fileHashes"))

instance ToJSON SourceProvenance where
        toJSON SourceProvenance'{..}
          = object
              (catMaybes
                 [("resolvedRepoSource" .=) <$> _spResolvedRepoSource,
                  ("resolvedStorageSource" .=) <$>
                    _spResolvedStorageSource,
                  ("fileHashes" .=) <$> _spFileHashes])

-- | Response including listed builds.
--
-- /See:/ 'listBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
    { _lbrNextPageToken :: !(Maybe Text)
    , _lbrBuilds :: !(Maybe [Build])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBuildsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrNextPageToken'
--
-- * 'lbrBuilds'
listBuildsResponse
    :: ListBuildsResponse
listBuildsResponse = 
    ListBuildsResponse'
    { _lbrNextPageToken = Nothing
    , _lbrBuilds = Nothing
    }

-- | Token to receive the next page of results.
lbrNextPageToken :: Lens' ListBuildsResponse (Maybe Text)
lbrNextPageToken
  = lens _lbrNextPageToken
      (\ s a -> s{_lbrNextPageToken = a})

-- | Builds will be sorted by \`create_time\`, descending.
lbrBuilds :: Lens' ListBuildsResponse [Build]
lbrBuilds
  = lens _lbrBuilds (\ s a -> s{_lbrBuilds = a}) .
      _Default
      . _Coerce

instance FromJSON ListBuildsResponse where
        parseJSON
          = withObject "ListBuildsResponse"
              (\ o ->
                 ListBuildsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "builds" .!= mempty))

instance ToJSON ListBuildsResponse where
        toJSON ListBuildsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lbrNextPageToken,
                  ("builds" .=) <$> _lbrBuilds])

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

-- | Specifies a build to retry.
--
-- /See:/ 'retryBuildRequest' smart constructor.
data RetryBuildRequest =
    RetryBuildRequest' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetryBuildRequest' with the minimum fields required to make a request.
--
retryBuildRequest
    :: RetryBuildRequest
retryBuildRequest = RetryBuildRequest'

instance FromJSON RetryBuildRequest where
        parseJSON
          = withObject "RetryBuildRequest"
              (\ o -> pure RetryBuildRequest')

instance ToJSON RetryBuildRequest where
        toJSON = const emptyObject

-- | The response message for Operations.ListOperations.
--
-- /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
    { _lorNextPageToken :: !(Maybe Text)
    , _lorOperations :: !(Maybe [Operation])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorNextPageToken'
--
-- * 'lorOperations'
listOperationsResponse
    :: ListOperationsResponse
listOperationsResponse = 
    ListOperationsResponse'
    { _lorNextPageToken = Nothing
    , _lorOperations = Nothing
    }

-- | The standard List next-page token.
lorNextPageToken :: Lens' ListOperationsResponse (Maybe Text)
lorNextPageToken
  = lens _lorNextPageToken
      (\ s a -> s{_lorNextPageToken = a})

-- | A list of operations that matches the specified filter in the request.
lorOperations :: Lens' ListOperationsResponse [Operation]
lorOperations
  = lens _lorOperations
      (\ s a -> s{_lorOperations = a})
      . _Default
      . _Coerce

instance FromJSON ListOperationsResponse where
        parseJSON
          = withObject "ListOperationsResponse"
              (\ o ->
                 ListOperationsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "operations" .!= mempty))

instance ToJSON ListOperationsResponse where
        toJSON ListOperationsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lorNextPageToken,
                  ("operations" .=) <$> _lorOperations])

-- | The request message for Operations.CancelOperation.
--
-- /See:/ 'cancelOperationRequest' smart constructor.
data CancelOperationRequest =
    CancelOperationRequest' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelOperationRequest' with the minimum fields required to make a request.
--
cancelOperationRequest
    :: CancelOperationRequest
cancelOperationRequest = CancelOperationRequest'

instance FromJSON CancelOperationRequest where
        parseJSON
          = withObject "CancelOperationRequest"
              (\ o -> pure CancelOperationRequest')

instance ToJSON CancelOperationRequest where
        toJSON = const emptyObject

-- | Container message for hash values.
--
-- /See:/ 'hash' smart constructor.
data Hash = Hash'
    { _hValue :: !(Maybe Bytes)
    , _hType :: !(Maybe HashType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Hash' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hValue'
--
-- * 'hType'
hash
    :: Hash
hash = 
    Hash'
    { _hValue = Nothing
    , _hType = Nothing
    }

-- | The hash value.
hValue :: Lens' Hash (Maybe ByteString)
hValue
  = lens _hValue (\ s a -> s{_hValue = a}) .
      mapping _Bytes

-- | The type of hash that was performed.
hType :: Lens' Hash (Maybe HashType)
hType = lens _hType (\ s a -> s{_hType = a})

instance FromJSON Hash where
        parseJSON
          = withObject "Hash"
              (\ o -> Hash' <$> (o .:? "value") <*> (o .:? "type"))

instance ToJSON Hash where
        toJSON Hash'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _hValue, ("type" .=) <$> _hType])

-- | Artifacts created by the build pipeline.
--
-- /See:/ 'results' smart constructor.
data Results = Results'
    { _rImages :: !(Maybe [BuiltImage])
    , _rBuildStepImages :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Results' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rImages'
--
-- * 'rBuildStepImages'
results
    :: Results
results = 
    Results'
    { _rImages = Nothing
    , _rBuildStepImages = Nothing
    }

-- | Container images that were built as a part of the build.
rImages :: Lens' Results [BuiltImage]
rImages
  = lens _rImages (\ s a -> s{_rImages = a}) . _Default
      . _Coerce

-- | List of build step digests, in the order corresponding to build step
-- indices.
rBuildStepImages :: Lens' Results [Text]
rBuildStepImages
  = lens _rBuildStepImages
      (\ s a -> s{_rBuildStepImages = a})
      . _Default
      . _Coerce

instance FromJSON Results where
        parseJSON
          = withObject "Results"
              (\ o ->
                 Results' <$>
                   (o .:? "images" .!= mempty) <*>
                     (o .:? "buildStepImages" .!= mempty))

instance ToJSON Results where
        toJSON Results'{..}
          = object
              (catMaybes
                 [("images" .=) <$> _rImages,
                  ("buildStepImages" .=) <$> _rBuildStepImages])

-- | Substitutions data for Build resource.
--
-- /See:/ 'buildTriggerSubstitutions' smart constructor.
newtype BuildTriggerSubstitutions = BuildTriggerSubstitutions'
    { _btsAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildTriggerSubstitutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btsAddtional'
buildTriggerSubstitutions
    :: HashMap Text Text -- ^ 'btsAddtional'
    -> BuildTriggerSubstitutions
buildTriggerSubstitutions pBtsAddtional_ = 
    BuildTriggerSubstitutions'
    { _btsAddtional = _Coerce # pBtsAddtional_
    }

btsAddtional :: Lens' BuildTriggerSubstitutions (HashMap Text Text)
btsAddtional
  = lens _btsAddtional (\ s a -> s{_btsAddtional = a})
      . _Coerce

instance FromJSON BuildTriggerSubstitutions where
        parseJSON
          = withObject "BuildTriggerSubstitutions"
              (\ o ->
                 BuildTriggerSubstitutions' <$> (parseJSONObject o))

instance ToJSON BuildTriggerSubstitutions where
        toJSON = toJSON . _btsAddtional

-- | Location of the source in a Google Cloud Source Repository.
--
-- /See:/ 'repoSource' smart constructor.
data RepoSource = RepoSource'
    { _rsRepoName :: !(Maybe Text)
    , _rsDir :: !(Maybe Text)
    , _rsCommitSha :: !(Maybe Text)
    , _rsBranchName :: !(Maybe Text)
    , _rsTagName :: !(Maybe Text)
    , _rsProjectId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RepoSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRepoName'
--
-- * 'rsDir'
--
-- * 'rsCommitSha'
--
-- * 'rsBranchName'
--
-- * 'rsTagName'
--
-- * 'rsProjectId'
repoSource
    :: RepoSource
repoSource = 
    RepoSource'
    { _rsRepoName = Nothing
    , _rsDir = Nothing
    , _rsCommitSha = Nothing
    , _rsBranchName = Nothing
    , _rsTagName = Nothing
    , _rsProjectId = Nothing
    }

-- | Name of the Cloud Source Repository. If omitted, the name \"default\" is
-- assumed.
rsRepoName :: Lens' RepoSource (Maybe Text)
rsRepoName
  = lens _rsRepoName (\ s a -> s{_rsRepoName = a})

-- | Directory, relative to the source root, in which to run the build. This
-- must be a relative path. If a step\'s \`dir\` is specified and is an
-- absolute path, this value is ignored for that step\'s execution.
rsDir :: Lens' RepoSource (Maybe Text)
rsDir = lens _rsDir (\ s a -> s{_rsDir = a})

-- | Explicit commit SHA to build.
rsCommitSha :: Lens' RepoSource (Maybe Text)
rsCommitSha
  = lens _rsCommitSha (\ s a -> s{_rsCommitSha = a})

-- | Name of the branch to build.
rsBranchName :: Lens' RepoSource (Maybe Text)
rsBranchName
  = lens _rsBranchName (\ s a -> s{_rsBranchName = a})

-- | Name of the tag to build.
rsTagName :: Lens' RepoSource (Maybe Text)
rsTagName
  = lens _rsTagName (\ s a -> s{_rsTagName = a})

-- | ID of the project that owns the Cloud Source Repository. If omitted, the
-- project ID requesting the build is assumed.
rsProjectId :: Lens' RepoSource (Maybe Text)
rsProjectId
  = lens _rsProjectId (\ s a -> s{_rsProjectId = a})

instance FromJSON RepoSource where
        parseJSON
          = withObject "RepoSource"
              (\ o ->
                 RepoSource' <$>
                   (o .:? "repoName") <*> (o .:? "dir") <*>
                     (o .:? "commitSha")
                     <*> (o .:? "branchName")
                     <*> (o .:? "tagName")
                     <*> (o .:? "projectId"))

instance ToJSON RepoSource where
        toJSON RepoSource'{..}
          = object
              (catMaybes
                 [("repoName" .=) <$> _rsRepoName,
                  ("dir" .=) <$> _rsDir,
                  ("commitSha" .=) <$> _rsCommitSha,
                  ("branchName" .=) <$> _rsBranchName,
                  ("tagName" .=) <$> _rsTagName,
                  ("projectId" .=) <$> _rsProjectId])

-- | This resource represents a long-running operation that is the result of
-- a network API call.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oDone :: !(Maybe Bool)
    , _oError :: !(Maybe Status)
    , _oResponse :: !(Maybe OperationResponse)
    , _oName :: !(Maybe Text)
    , _oMetadata :: !(Maybe OperationMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDone'
--
-- * 'oError'
--
-- * 'oResponse'
--
-- * 'oName'
--
-- * 'oMetadata'
operation
    :: Operation
operation = 
    Operation'
    { _oDone = Nothing
    , _oError = Nothing
    , _oResponse = Nothing
    , _oName = Nothing
    , _oMetadata = Nothing
    }

-- | If the value is \`false\`, it means the operation is still in progress.
-- If \`true\`, the operation is completed, and either \`error\` or
-- \`response\` is available.
oDone :: Lens' Operation (Maybe Bool)
oDone = lens _oDone (\ s a -> s{_oDone = a})

-- | The error result of the operation in case of failure or cancellation.
oError :: Lens' Operation (Maybe Status)
oError = lens _oError (\ s a -> s{_oError = a})

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
oResponse :: Lens' Operation (Maybe OperationResponse)
oResponse
  = lens _oResponse (\ s a -> s{_oResponse = a})

-- | The server-assigned name, which is only unique within the same service
-- that originally returns it. If you use the default HTTP mapping, the
-- \`name\` should have the format of \`operations\/some\/unique\/name\`.
oName :: Lens' Operation (Maybe Text)
oName = lens _oName (\ s a -> s{_oName = a})

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
oMetadata :: Lens' Operation (Maybe OperationMetadata)
oMetadata
  = lens _oMetadata (\ s a -> s{_oMetadata = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "done") <*> (o .:? "error") <*>
                     (o .:? "response")
                     <*> (o .:? "name")
                     <*> (o .:? "metadata"))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("done" .=) <$> _oDone, ("error" .=) <$> _oError,
                  ("response" .=) <$> _oResponse,
                  ("name" .=) <$> _oName,
                  ("metadata" .=) <$> _oMetadata])

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

-- | Map of environment variable name to its encrypted value. Secret
-- environment variables must be unique across all of a build\'s secrets,
-- and must be used by at least one build step. Values can be at most 1 KB
-- in size. There can be at most ten secret values across all of a build\'s
-- secrets.
--
-- /See:/ 'secretSecretEnv' smart constructor.
newtype SecretSecretEnv = SecretSecretEnv'
    { _sseAddtional :: HashMap Text Bytes
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SecretSecretEnv' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseAddtional'
secretSecretEnv
    :: HashMap Text ByteString -- ^ 'sseAddtional'
    -> SecretSecretEnv
secretSecretEnv pSseAddtional_ = 
    SecretSecretEnv'
    { _sseAddtional = _Coerce # pSseAddtional_
    }

sseAddtional :: Lens' SecretSecretEnv (HashMap Text ByteString)
sseAddtional
  = lens _sseAddtional (\ s a -> s{_sseAddtional = a})
      . _Coerce

instance FromJSON SecretSecretEnv where
        parseJSON
          = withObject "SecretSecretEnv"
              (\ o -> SecretSecretEnv' <$> (parseJSONObject o))

instance ToJSON SecretSecretEnv where
        toJSON = toJSON . _sseAddtional

-- | Volume describes a Docker container volume which is mounted into build
-- steps in order to persist files across build step execution.
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
    { _vPath :: !(Maybe Text)
    , _vName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vPath'
--
-- * 'vName'
volume
    :: Volume
volume = 
    Volume'
    { _vPath = Nothing
    , _vName = Nothing
    }

-- | Path at which to mount the volume. Paths must be absolute and cannot
-- conflict with other volume paths on the same build step or with certain
-- reserved volume paths.
vPath :: Lens' Volume (Maybe Text)
vPath = lens _vPath (\ s a -> s{_vPath = a})

-- | Name of the volume to mount. Volume names must be unique per build step
-- and must be valid names for Docker volumes. Each named volume must be
-- used by at least two build steps.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a})

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ o ->
                 Volume' <$> (o .:? "path") <*> (o .:? "name"))

instance ToJSON Volume where
        toJSON Volume'{..}
          = object
              (catMaybes
                 [("path" .=) <$> _vPath, ("name" .=) <$> _vName])

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

-- | A build resource in the Container Builder API. At a high level, a
-- \`Build\` describes where to find source code, how to build it (for
-- example, the builder image to run on the source), and where to store the
-- built artifacts. Fields can include the following variables, which will
-- be expanded when the build is created: - $PROJECT_ID: the project ID of
-- the build. - $BUILD_ID: the autogenerated ID of the build. - $REPO_NAME:
-- the source repository name specified by RepoSource. - $BRANCH_NAME: the
-- branch name specified by RepoSource. - $TAG_NAME: the tag name specified
-- by RepoSource. - $REVISION_ID or $COMMIT_SHA: the commit SHA specified
-- by RepoSource or resolved from the specified branch or tag. -
-- $SHORT_SHA: first 7 characters of $REVISION_ID or $COMMIT_SHA.
--
-- /See:/ 'build' smart constructor.
data Build = Build'
    { _bImages :: !(Maybe [Text])
    , _bStatus :: !(Maybe BuildStatus)
    , _bSourceProvenance :: !(Maybe SourceProvenance)
    , _bSubstitutions :: !(Maybe BuildSubstitutions)
    , _bLogURL :: !(Maybe Text)
    , _bResults :: !(Maybe Results)
    , _bSecrets :: !(Maybe [Secret])
    , _bStartTime :: !(Maybe DateTime')
    , _bLogsBucket :: !(Maybe Text)
    , _bSteps :: !(Maybe [BuildStep])
    , _bStatusDetail :: !(Maybe Text)
    , _bSource :: !(Maybe Source)
    , _bId :: !(Maybe Text)
    , _bOptions :: !(Maybe BuildOptions)
    , _bProjectId :: !(Maybe Text)
    , _bTiming :: !(Maybe BuildTiming)
    , _bBuildTriggerId :: !(Maybe Text)
    , _bTimeout :: !(Maybe Duration)
    , _bFinishTime :: !(Maybe DateTime')
    , _bCreateTime :: !(Maybe DateTime')
    , _bTags :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bImages'
--
-- * 'bStatus'
--
-- * 'bSourceProvenance'
--
-- * 'bSubstitutions'
--
-- * 'bLogURL'
--
-- * 'bResults'
--
-- * 'bSecrets'
--
-- * 'bStartTime'
--
-- * 'bLogsBucket'
--
-- * 'bSteps'
--
-- * 'bStatusDetail'
--
-- * 'bSource'
--
-- * 'bId'
--
-- * 'bOptions'
--
-- * 'bProjectId'
--
-- * 'bTiming'
--
-- * 'bBuildTriggerId'
--
-- * 'bTimeout'
--
-- * 'bFinishTime'
--
-- * 'bCreateTime'
--
-- * 'bTags'
build
    :: Build
build = 
    Build'
    { _bImages = Nothing
    , _bStatus = Nothing
    , _bSourceProvenance = Nothing
    , _bSubstitutions = Nothing
    , _bLogURL = Nothing
    , _bResults = Nothing
    , _bSecrets = Nothing
    , _bStartTime = Nothing
    , _bLogsBucket = Nothing
    , _bSteps = Nothing
    , _bStatusDetail = Nothing
    , _bSource = Nothing
    , _bId = Nothing
    , _bOptions = Nothing
    , _bProjectId = Nothing
    , _bTiming = Nothing
    , _bBuildTriggerId = Nothing
    , _bTimeout = Nothing
    , _bFinishTime = Nothing
    , _bCreateTime = Nothing
    , _bTags = Nothing
    }

-- | A list of images to be pushed upon the successful completion of all
-- build steps. The images are pushed using the builder service account\'s
-- credentials. The digests of the pushed images will be stored in the
-- \`Build\` resource\'s results field. If any of the images fail to be
-- pushed, the build status is marked \`FAILURE\`.
bImages :: Lens' Build [Text]
bImages
  = lens _bImages (\ s a -> s{_bImages = a}) . _Default
      . _Coerce

-- | Status of the build. \'OutputOnly
bStatus :: Lens' Build (Maybe BuildStatus)
bStatus = lens _bStatus (\ s a -> s{_bStatus = a})

-- | A permanent fixed identifier for source. \'OutputOnly
bSourceProvenance :: Lens' Build (Maybe SourceProvenance)
bSourceProvenance
  = lens _bSourceProvenance
      (\ s a -> s{_bSourceProvenance = a})

-- | Substitutions data for \`Build\` resource.
bSubstitutions :: Lens' Build (Maybe BuildSubstitutions)
bSubstitutions
  = lens _bSubstitutions
      (\ s a -> s{_bSubstitutions = a})

-- | URL to logs for this build in Google Cloud Console. \'OutputOnly
bLogURL :: Lens' Build (Maybe Text)
bLogURL = lens _bLogURL (\ s a -> s{_bLogURL = a})

-- | Results of the build. \'OutputOnly
bResults :: Lens' Build (Maybe Results)
bResults = lens _bResults (\ s a -> s{_bResults = a})

-- | Secrets to decrypt using Cloud Key Management Service.
bSecrets :: Lens' Build [Secret]
bSecrets
  = lens _bSecrets (\ s a -> s{_bSecrets = a}) .
      _Default
      . _Coerce

-- | Time at which execution of the build was started. \'OutputOnly
bStartTime :: Lens' Build (Maybe UTCTime)
bStartTime
  = lens _bStartTime (\ s a -> s{_bStartTime = a}) .
      mapping _DateTime

-- | Google Cloud Storage bucket where logs should be written (see [Bucket
-- Name
-- Requirements](https:\/\/cloud.google.com\/storage\/docs\/bucket-naming#requirements)).
-- Logs file names will be of the format
-- \`/l//o//g//s//b//u//c//k//e//t/\//l//o//g/−{build_id}.txt\`.
bLogsBucket :: Lens' Build (Maybe Text)
bLogsBucket
  = lens _bLogsBucket (\ s a -> s{_bLogsBucket = a})

-- | Required. The operations to be performed on the workspace.
bSteps :: Lens' Build [BuildStep]
bSteps
  = lens _bSteps (\ s a -> s{_bSteps = a}) . _Default .
      _Coerce

-- | Customer-readable message about the current status. \'OutputOnly
bStatusDetail :: Lens' Build (Maybe Text)
bStatusDetail
  = lens _bStatusDetail
      (\ s a -> s{_bStatusDetail = a})

-- | The location of the source files to build.
bSource :: Lens' Build (Maybe Source)
bSource = lens _bSource (\ s a -> s{_bSource = a})

-- | Unique identifier of the build. \'OutputOnly
bId :: Lens' Build (Maybe Text)
bId = lens _bId (\ s a -> s{_bId = a})

-- | Special options for this build.
bOptions :: Lens' Build (Maybe BuildOptions)
bOptions = lens _bOptions (\ s a -> s{_bOptions = a})

-- | ID of the project. \'OutputOnly.
bProjectId :: Lens' Build (Maybe Text)
bProjectId
  = lens _bProjectId (\ s a -> s{_bProjectId = a})

-- | Stores timing information for phases of the build. Valid keys are: *
-- BUILD: time to execute all build steps * PUSH: time to push all
-- specified images. * FETCHSOURCE: time to fetch source. If the build does
-- not specify source or images, these keys will not be included.
-- \'OutputOnly
bTiming :: Lens' Build (Maybe BuildTiming)
bTiming = lens _bTiming (\ s a -> s{_bTiming = a})

-- | The ID of the \`BuildTrigger\` that triggered this build, if it was
-- triggered automatically. \'OutputOnly
bBuildTriggerId :: Lens' Build (Maybe Text)
bBuildTriggerId
  = lens _bBuildTriggerId
      (\ s a -> s{_bBuildTriggerId = a})

-- | Amount of time that this build should be allowed to run, to second
-- granularity. If this amount of time elapses, work on the build will
-- cease and the build status will be \`TIMEOUT\`. Default time is ten
-- minutes.
bTimeout :: Lens' Build (Maybe Scientific)
bTimeout
  = lens _bTimeout (\ s a -> s{_bTimeout = a}) .
      mapping _Duration

-- | Time at which execution of the build was finished. The difference
-- between finish_time and start_time is the duration of the build\'s
-- execution. \'OutputOnly
bFinishTime :: Lens' Build (Maybe UTCTime)
bFinishTime
  = lens _bFinishTime (\ s a -> s{_bFinishTime = a}) .
      mapping _DateTime

-- | Time at which the request to create the build was received. \'OutputOnly
bCreateTime :: Lens' Build (Maybe UTCTime)
bCreateTime
  = lens _bCreateTime (\ s a -> s{_bCreateTime = a}) .
      mapping _DateTime

-- | Tags for annotation of a \`Build\`. These are not docker tags.
bTags :: Lens' Build [Text]
bTags
  = lens _bTags (\ s a -> s{_bTags = a}) . _Default .
      _Coerce

instance FromJSON Build where
        parseJSON
          = withObject "Build"
              (\ o ->
                 Build' <$>
                   (o .:? "images" .!= mempty) <*> (o .:? "status") <*>
                     (o .:? "sourceProvenance")
                     <*> (o .:? "substitutions")
                     <*> (o .:? "logUrl")
                     <*> (o .:? "results")
                     <*> (o .:? "secrets" .!= mempty)
                     <*> (o .:? "startTime")
                     <*> (o .:? "logsBucket")
                     <*> (o .:? "steps" .!= mempty)
                     <*> (o .:? "statusDetail")
                     <*> (o .:? "source")
                     <*> (o .:? "id")
                     <*> (o .:? "options")
                     <*> (o .:? "projectId")
                     <*> (o .:? "timing")
                     <*> (o .:? "buildTriggerId")
                     <*> (o .:? "timeout")
                     <*> (o .:? "finishTime")
                     <*> (o .:? "createTime")
                     <*> (o .:? "tags" .!= mempty))

instance ToJSON Build where
        toJSON Build'{..}
          = object
              (catMaybes
                 [("images" .=) <$> _bImages,
                  ("status" .=) <$> _bStatus,
                  ("sourceProvenance" .=) <$> _bSourceProvenance,
                  ("substitutions" .=) <$> _bSubstitutions,
                  ("logUrl" .=) <$> _bLogURL,
                  ("results" .=) <$> _bResults,
                  ("secrets" .=) <$> _bSecrets,
                  ("startTime" .=) <$> _bStartTime,
                  ("logsBucket" .=) <$> _bLogsBucket,
                  ("steps" .=) <$> _bSteps,
                  ("statusDetail" .=) <$> _bStatusDetail,
                  ("source" .=) <$> _bSource, ("id" .=) <$> _bId,
                  ("options" .=) <$> _bOptions,
                  ("projectId" .=) <$> _bProjectId,
                  ("timing" .=) <$> _bTiming,
                  ("buildTriggerId" .=) <$> _bBuildTriggerId,
                  ("timeout" .=) <$> _bTimeout,
                  ("finishTime" .=) <$> _bFinishTime,
                  ("createTime" .=) <$> _bCreateTime,
                  ("tags" .=) <$> _bTags])

-- | Hash(es) of the build source, which can be used to verify that the
-- original source integrity was maintained in the build. Note that
-- \`FileHashes\` will only be populated if \`BuildOptions\` has requested
-- a \`SourceProvenanceHash\`. The keys to this map are file paths used as
-- build source and the values contain the hash values for those files. If
-- the build source came in a single package such as a gzipped tarfile
-- (\`.tar.gz\`), the \`FileHash\` will be for the single path to that
-- file. \'OutputOnly
--
-- /See:/ 'sourceProvenanceFileHashes' smart constructor.
newtype SourceProvenanceFileHashes = SourceProvenanceFileHashes'
    { _spfhAddtional :: HashMap Text FileHashes
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceProvenanceFileHashes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spfhAddtional'
sourceProvenanceFileHashes
    :: HashMap Text FileHashes -- ^ 'spfhAddtional'
    -> SourceProvenanceFileHashes
sourceProvenanceFileHashes pSpfhAddtional_ = 
    SourceProvenanceFileHashes'
    { _spfhAddtional = _Coerce # pSpfhAddtional_
    }

spfhAddtional :: Lens' SourceProvenanceFileHashes (HashMap Text FileHashes)
spfhAddtional
  = lens _spfhAddtional
      (\ s a -> s{_spfhAddtional = a})
      . _Coerce

instance FromJSON SourceProvenanceFileHashes where
        parseJSON
          = withObject "SourceProvenanceFileHashes"
              (\ o ->
                 SourceProvenanceFileHashes' <$> (parseJSONObject o))

instance ToJSON SourceProvenanceFileHashes where
        toJSON = toJSON . _spfhAddtional

-- | Pairs a set of secret environment variables containing encrypted values
-- with the Cloud KMS key to use to decrypt the value.
--
-- /See:/ 'secret' smart constructor.
data Secret = Secret'
    { _sKmsKeyName :: !(Maybe Text)
    , _sSecretEnv :: !(Maybe SecretSecretEnv)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Secret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sKmsKeyName'
--
-- * 'sSecretEnv'
secret
    :: Secret
secret = 
    Secret'
    { _sKmsKeyName = Nothing
    , _sSecretEnv = Nothing
    }

-- | Cloud KMS key name to use to decrypt these envs.
sKmsKeyName :: Lens' Secret (Maybe Text)
sKmsKeyName
  = lens _sKmsKeyName (\ s a -> s{_sKmsKeyName = a})

-- | Map of environment variable name to its encrypted value. Secret
-- environment variables must be unique across all of a build\'s secrets,
-- and must be used by at least one build step. Values can be at most 1 KB
-- in size. There can be at most ten secret values across all of a build\'s
-- secrets.
sSecretEnv :: Lens' Secret (Maybe SecretSecretEnv)
sSecretEnv
  = lens _sSecretEnv (\ s a -> s{_sSecretEnv = a})

instance FromJSON Secret where
        parseJSON
          = withObject "Secret"
              (\ o ->
                 Secret' <$>
                   (o .:? "kmsKeyName") <*> (o .:? "secretEnv"))

instance ToJSON Secret where
        toJSON Secret'{..}
          = object
              (catMaybes
                 [("kmsKeyName" .=) <$> _sKmsKeyName,
                  ("secretEnv" .=) <$> _sSecretEnv])

-- | Request to cancel an ongoing build.
--
-- /See:/ 'cancelBuildRequest' smart constructor.
data CancelBuildRequest =
    CancelBuildRequest' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelBuildRequest' with the minimum fields required to make a request.
--
cancelBuildRequest
    :: CancelBuildRequest
cancelBuildRequest = CancelBuildRequest'

instance FromJSON CancelBuildRequest where
        parseJSON
          = withObject "CancelBuildRequest"
              (\ o -> pure CancelBuildRequest')

instance ToJSON CancelBuildRequest where
        toJSON = const emptyObject

-- | Start and end times for a build execution phase.
--
-- /See:/ 'timeSpan' smart constructor.
data TimeSpan = TimeSpan'
    { _tsStartTime :: !(Maybe DateTime')
    , _tsEndTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimeSpan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsStartTime'
--
-- * 'tsEndTime'
timeSpan
    :: TimeSpan
timeSpan = 
    TimeSpan'
    { _tsStartTime = Nothing
    , _tsEndTime = Nothing
    }

-- | Start of time span.
tsStartTime :: Lens' TimeSpan (Maybe UTCTime)
tsStartTime
  = lens _tsStartTime (\ s a -> s{_tsStartTime = a}) .
      mapping _DateTime

-- | End of time span.
tsEndTime :: Lens' TimeSpan (Maybe UTCTime)
tsEndTime
  = lens _tsEndTime (\ s a -> s{_tsEndTime = a}) .
      mapping _DateTime

instance FromJSON TimeSpan where
        parseJSON
          = withObject "TimeSpan"
              (\ o ->
                 TimeSpan' <$>
                   (o .:? "startTime") <*> (o .:? "endTime"))

instance ToJSON TimeSpan where
        toJSON TimeSpan'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _tsStartTime,
                  ("endTime" .=) <$> _tsEndTime])

-- | Location of the source in an archive file in Google Cloud Storage.
--
-- /See:/ 'storageSource' smart constructor.
data StorageSource = StorageSource'
    { _ssBucket :: !(Maybe Text)
    , _ssObject :: !(Maybe Text)
    , _ssGeneration :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StorageSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssBucket'
--
-- * 'ssObject'
--
-- * 'ssGeneration'
storageSource
    :: StorageSource
storageSource = 
    StorageSource'
    { _ssBucket = Nothing
    , _ssObject = Nothing
    , _ssGeneration = Nothing
    }

-- | Google Cloud Storage bucket containing the source (see [Bucket Name
-- Requirements](https:\/\/cloud.google.com\/storage\/docs\/bucket-naming#requirements)).
ssBucket :: Lens' StorageSource (Maybe Text)
ssBucket = lens _ssBucket (\ s a -> s{_ssBucket = a})

-- | Google Cloud Storage object containing the source. This object must be a
-- gzipped archive file (\`.tar.gz\`) containing source to build.
ssObject :: Lens' StorageSource (Maybe Text)
ssObject = lens _ssObject (\ s a -> s{_ssObject = a})

-- | Google Cloud Storage generation for the object. If the generation is
-- omitted, the latest generation will be used.
ssGeneration :: Lens' StorageSource (Maybe Int64)
ssGeneration
  = lens _ssGeneration (\ s a -> s{_ssGeneration = a})
      . mapping _Coerce

instance FromJSON StorageSource where
        parseJSON
          = withObject "StorageSource"
              (\ o ->
                 StorageSource' <$>
                   (o .:? "bucket") <*> (o .:? "object") <*>
                     (o .:? "generation"))

instance ToJSON StorageSource where
        toJSON StorageSource'{..}
          = object
              (catMaybes
                 [("bucket" .=) <$> _ssBucket,
                  ("object" .=) <$> _ssObject,
                  ("generation" .=) <$> _ssGeneration])

-- | Response containing existing \`BuildTriggers\`.
--
-- /See:/ 'listBuildTriggersResponse' smart constructor.
newtype ListBuildTriggersResponse = ListBuildTriggersResponse'
    { _lbtrTriggers :: Maybe [BuildTrigger]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListBuildTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtrTriggers'
listBuildTriggersResponse
    :: ListBuildTriggersResponse
listBuildTriggersResponse = 
    ListBuildTriggersResponse'
    { _lbtrTriggers = Nothing
    }

-- | \`BuildTriggers\` for the project, sorted by \`create_time\` descending.
lbtrTriggers :: Lens' ListBuildTriggersResponse [BuildTrigger]
lbtrTriggers
  = lens _lbtrTriggers (\ s a -> s{_lbtrTriggers = a})
      . _Default
      . _Coerce

instance FromJSON ListBuildTriggersResponse where
        parseJSON
          = withObject "ListBuildTriggersResponse"
              (\ o ->
                 ListBuildTriggersResponse' <$>
                   (o .:? "triggers" .!= mempty))

instance ToJSON ListBuildTriggersResponse where
        toJSON ListBuildTriggersResponse'{..}
          = object
              (catMaybes [("triggers" .=) <$> _lbtrTriggers])

-- | Container message for hashes of byte content of files, used in
-- SourceProvenance messages to verify integrity of source input to the
-- build.
--
-- /See:/ 'fileHashes' smart constructor.
newtype FileHashes = FileHashes'
    { _fhFileHash :: Maybe [Hash]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FileHashes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fhFileHash'
fileHashes
    :: FileHashes
fileHashes = 
    FileHashes'
    { _fhFileHash = Nothing
    }

-- | Collection of file hashes.
fhFileHash :: Lens' FileHashes [Hash]
fhFileHash
  = lens _fhFileHash (\ s a -> s{_fhFileHash = a}) .
      _Default
      . _Coerce

instance FromJSON FileHashes where
        parseJSON
          = withObject "FileHashes"
              (\ o ->
                 FileHashes' <$> (o .:? "fileHash" .!= mempty))

instance ToJSON FileHashes where
        toJSON FileHashes'{..}
          = object
              (catMaybes [("fileHash" .=) <$> _fhFileHash])

-- | Substitutions data for \`Build\` resource.
--
-- /See:/ 'buildSubstitutions' smart constructor.
newtype BuildSubstitutions = BuildSubstitutions'
    { _bsAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildSubstitutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsAddtional'
buildSubstitutions
    :: HashMap Text Text -- ^ 'bsAddtional'
    -> BuildSubstitutions
buildSubstitutions pBsAddtional_ = 
    BuildSubstitutions'
    { _bsAddtional = _Coerce # pBsAddtional_
    }

bsAddtional :: Lens' BuildSubstitutions (HashMap Text Text)
bsAddtional
  = lens _bsAddtional (\ s a -> s{_bsAddtional = a}) .
      _Coerce

instance FromJSON BuildSubstitutions where
        parseJSON
          = withObject "BuildSubstitutions"
              (\ o -> BuildSubstitutions' <$> (parseJSONObject o))

instance ToJSON BuildSubstitutions where
        toJSON = toJSON . _bsAddtional

-- | Location of the source in a supported storage service.
--
-- /See:/ 'source' smart constructor.
data Source = Source'
    { _sRepoSource :: !(Maybe RepoSource)
    , _sStorageSource :: !(Maybe StorageSource)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sRepoSource'
--
-- * 'sStorageSource'
source
    :: Source
source = 
    Source'
    { _sRepoSource = Nothing
    , _sStorageSource = Nothing
    }

-- | If provided, get the source from this location in a Cloud Source
-- Repository.
sRepoSource :: Lens' Source (Maybe RepoSource)
sRepoSource
  = lens _sRepoSource (\ s a -> s{_sRepoSource = a})

-- | If provided, get the source from this location in Google Cloud Storage.
sStorageSource :: Lens' Source (Maybe StorageSource)
sStorageSource
  = lens _sStorageSource
      (\ s a -> s{_sStorageSource = a})

instance FromJSON Source where
        parseJSON
          = withObject "Source"
              (\ o ->
                 Source' <$>
                   (o .:? "repoSource") <*> (o .:? "storageSource"))

instance ToJSON Source where
        toJSON Source'{..}
          = object
              (catMaybes
                 [("repoSource" .=) <$> _sRepoSource,
                  ("storageSource" .=) <$> _sStorageSource])

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
--
-- /See:/ 'operationMetadata' smart constructor.
newtype OperationMetadata = OperationMetadata'
    { _omAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'omAddtional'
operationMetadata
    :: HashMap Text JSONValue -- ^ 'omAddtional'
    -> OperationMetadata
operationMetadata pOmAddtional_ = 
    OperationMetadata'
    { _omAddtional = _Coerce # pOmAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
omAddtional :: Lens' OperationMetadata (HashMap Text JSONValue)
omAddtional
  = lens _omAddtional (\ s a -> s{_omAddtional = a}) .
      _Coerce

instance FromJSON OperationMetadata where
        parseJSON
          = withObject "OperationMetadata"
              (\ o -> OperationMetadata' <$> (parseJSONObject o))

instance ToJSON OperationMetadata where
        toJSON = toJSON . _omAddtional

-- | Stores timing information for phases of the build. Valid keys are: *
-- BUILD: time to execute all build steps * PUSH: time to push all
-- specified images. * FETCHSOURCE: time to fetch source. If the build does
-- not specify source or images, these keys will not be included.
-- \'OutputOnly
--
-- /See:/ 'buildTiming' smart constructor.
newtype BuildTiming = BuildTiming'
    { _btAddtional :: HashMap Text TimeSpan
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildTiming' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btAddtional'
buildTiming
    :: HashMap Text TimeSpan -- ^ 'btAddtional'
    -> BuildTiming
buildTiming pBtAddtional_ = 
    BuildTiming'
    { _btAddtional = _Coerce # pBtAddtional_
    }

btAddtional :: Lens' BuildTiming (HashMap Text TimeSpan)
btAddtional
  = lens _btAddtional (\ s a -> s{_btAddtional = a}) .
      _Coerce

instance FromJSON BuildTiming where
        parseJSON
          = withObject "BuildTiming"
              (\ o -> BuildTiming' <$> (parseJSONObject o))

instance ToJSON BuildTiming where
        toJSON = toJSON . _btAddtional

-- | Metadata for build operations.
--
-- /See:/ 'buildOperationMetadata' smart constructor.
newtype BuildOperationMetadata = BuildOperationMetadata'
    { _bomBuild :: Maybe Build
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildOperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bomBuild'
buildOperationMetadata
    :: BuildOperationMetadata
buildOperationMetadata = 
    BuildOperationMetadata'
    { _bomBuild = Nothing
    }

-- | The build that the operation is tracking.
bomBuild :: Lens' BuildOperationMetadata (Maybe Build)
bomBuild = lens _bomBuild (\ s a -> s{_bomBuild = a})

instance FromJSON BuildOperationMetadata where
        parseJSON
          = withObject "BuildOperationMetadata"
              (\ o -> BuildOperationMetadata' <$> (o .:? "build"))

instance ToJSON BuildOperationMetadata where
        toJSON BuildOperationMetadata'{..}
          = object (catMaybes [("build" .=) <$> _bomBuild])

-- | Optional arguments to enable specific features of builds.
--
-- /See:/ 'buildOptions' smart constructor.
data BuildOptions = BuildOptions'
    { _boDiskSizeGb :: !(Maybe (Textual Int64))
    , _boSubstitutionOption :: !(Maybe BuildOptionsSubstitutionOption)
    , _boRequestedVerifyOption :: !(Maybe BuildOptionsRequestedVerifyOption)
    , _boMachineType :: !(Maybe BuildOptionsMachineType)
    , _boLogStreamingOption :: !(Maybe BuildOptionsLogStreamingOption)
    , _boSourceProvenanceHash :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'boDiskSizeGb'
--
-- * 'boSubstitutionOption'
--
-- * 'boRequestedVerifyOption'
--
-- * 'boMachineType'
--
-- * 'boLogStreamingOption'
--
-- * 'boSourceProvenanceHash'
buildOptions
    :: BuildOptions
buildOptions = 
    BuildOptions'
    { _boDiskSizeGb = Nothing
    , _boSubstitutionOption = Nothing
    , _boRequestedVerifyOption = Nothing
    , _boMachineType = Nothing
    , _boLogStreamingOption = Nothing
    , _boSourceProvenanceHash = Nothing
    }

-- | Requested disk size for the VM that runs the build. Note that this is
-- *NOT* \"disk free\"; some of the space will be used by the operating
-- system and build utilities. Also note that this is the minimum disk size
-- that will be allocated for the build -- the build may run with a larger
-- disk than requested. At present, the maximum disk size is 1000GB; builds
-- that request more than the maximum are rejected with an error.
boDiskSizeGb :: Lens' BuildOptions (Maybe Int64)
boDiskSizeGb
  = lens _boDiskSizeGb (\ s a -> s{_boDiskSizeGb = a})
      . mapping _Coerce

-- | Option to specify behavior when there is an error in the substitution
-- checks.
boSubstitutionOption :: Lens' BuildOptions (Maybe BuildOptionsSubstitutionOption)
boSubstitutionOption
  = lens _boSubstitutionOption
      (\ s a -> s{_boSubstitutionOption = a})

-- | Requested verifiability options.
boRequestedVerifyOption :: Lens' BuildOptions (Maybe BuildOptionsRequestedVerifyOption)
boRequestedVerifyOption
  = lens _boRequestedVerifyOption
      (\ s a -> s{_boRequestedVerifyOption = a})

-- | Compute Engine machine type on which to run the build.
boMachineType :: Lens' BuildOptions (Maybe BuildOptionsMachineType)
boMachineType
  = lens _boMachineType
      (\ s a -> s{_boMachineType = a})

-- | Option to define build log streaming behavior to Google Cloud Storage.
boLogStreamingOption :: Lens' BuildOptions (Maybe BuildOptionsLogStreamingOption)
boLogStreamingOption
  = lens _boLogStreamingOption
      (\ s a -> s{_boLogStreamingOption = a})

-- | Requested hash for SourceProvenance.
boSourceProvenanceHash :: Lens' BuildOptions [Text]
boSourceProvenanceHash
  = lens _boSourceProvenanceHash
      (\ s a -> s{_boSourceProvenanceHash = a})
      . _Default
      . _Coerce

instance FromJSON BuildOptions where
        parseJSON
          = withObject "BuildOptions"
              (\ o ->
                 BuildOptions' <$>
                   (o .:? "diskSizeGb") <*> (o .:? "substitutionOption")
                     <*> (o .:? "requestedVerifyOption")
                     <*> (o .:? "machineType")
                     <*> (o .:? "logStreamingOption")
                     <*> (o .:? "sourceProvenanceHash" .!= mempty))

instance ToJSON BuildOptions where
        toJSON BuildOptions'{..}
          = object
              (catMaybes
                 [("diskSizeGb" .=) <$> _boDiskSizeGb,
                  ("substitutionOption" .=) <$> _boSubstitutionOption,
                  ("requestedVerifyOption" .=) <$>
                    _boRequestedVerifyOption,
                  ("machineType" .=) <$> _boMachineType,
                  ("logStreamingOption" .=) <$> _boLogStreamingOption,
                  ("sourceProvenanceHash" .=) <$>
                    _boSourceProvenanceHash])

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
--
-- /See:/ 'operationResponse' smart constructor.
newtype OperationResponse = OperationResponse'
    { _orAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orAddtional'
operationResponse
    :: HashMap Text JSONValue -- ^ 'orAddtional'
    -> OperationResponse
operationResponse pOrAddtional_ = 
    OperationResponse'
    { _orAddtional = _Coerce # pOrAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
orAddtional :: Lens' OperationResponse (HashMap Text JSONValue)
orAddtional
  = lens _orAddtional (\ s a -> s{_orAddtional = a}) .
      _Coerce

instance FromJSON OperationResponse where
        parseJSON
          = withObject "OperationResponse"
              (\ o -> OperationResponse' <$> (parseJSONObject o))

instance ToJSON OperationResponse where
        toJSON = toJSON . _orAddtional

-- | Configuration for an automated build in response to source repository
-- changes.
--
-- /See:/ 'buildTrigger' smart constructor.
data BuildTrigger = BuildTrigger'
    { _btSubstitutions :: !(Maybe BuildTriggerSubstitutions)
    , _btDisabled :: !(Maybe Bool)
    , _btTriggerTemplate :: !(Maybe RepoSource)
    , _btBuild :: !(Maybe Build)
    , _btId :: !(Maybe Text)
    , _btDescription :: !(Maybe Text)
    , _btFilename :: !(Maybe Text)
    , _btCreateTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btSubstitutions'
--
-- * 'btDisabled'
--
-- * 'btTriggerTemplate'
--
-- * 'btBuild'
--
-- * 'btId'
--
-- * 'btDescription'
--
-- * 'btFilename'
--
-- * 'btCreateTime'
buildTrigger
    :: BuildTrigger
buildTrigger = 
    BuildTrigger'
    { _btSubstitutions = Nothing
    , _btDisabled = Nothing
    , _btTriggerTemplate = Nothing
    , _btBuild = Nothing
    , _btId = Nothing
    , _btDescription = Nothing
    , _btFilename = Nothing
    , _btCreateTime = Nothing
    }

-- | Substitutions data for Build resource.
btSubstitutions :: Lens' BuildTrigger (Maybe BuildTriggerSubstitutions)
btSubstitutions
  = lens _btSubstitutions
      (\ s a -> s{_btSubstitutions = a})

-- | If true, the trigger will never result in a build.
btDisabled :: Lens' BuildTrigger (Maybe Bool)
btDisabled
  = lens _btDisabled (\ s a -> s{_btDisabled = a})

-- | Template describing the types of source changes to trigger a build.
-- Branch and tag names in trigger templates are interpreted as regular
-- expressions. Any branch or tag change that matches that regular
-- expression will trigger a build.
btTriggerTemplate :: Lens' BuildTrigger (Maybe RepoSource)
btTriggerTemplate
  = lens _btTriggerTemplate
      (\ s a -> s{_btTriggerTemplate = a})

-- | Contents of the build template.
btBuild :: Lens' BuildTrigger (Maybe Build)
btBuild = lens _btBuild (\ s a -> s{_btBuild = a})

-- | Unique identifier of the trigger. \'OutputOnly
btId :: Lens' BuildTrigger (Maybe Text)
btId = lens _btId (\ s a -> s{_btId = a})

-- | Human-readable description of this trigger.
btDescription :: Lens' BuildTrigger (Maybe Text)
btDescription
  = lens _btDescription
      (\ s a -> s{_btDescription = a})

-- | Path, from the source root, to a file whose contents is used for the
-- template.
btFilename :: Lens' BuildTrigger (Maybe Text)
btFilename
  = lens _btFilename (\ s a -> s{_btFilename = a})

-- | Time when the trigger was created. \'OutputOnly
btCreateTime :: Lens' BuildTrigger (Maybe UTCTime)
btCreateTime
  = lens _btCreateTime (\ s a -> s{_btCreateTime = a})
      . mapping _DateTime

instance FromJSON BuildTrigger where
        parseJSON
          = withObject "BuildTrigger"
              (\ o ->
                 BuildTrigger' <$>
                   (o .:? "substitutions") <*> (o .:? "disabled") <*>
                     (o .:? "triggerTemplate")
                     <*> (o .:? "build")
                     <*> (o .:? "id")
                     <*> (o .:? "description")
                     <*> (o .:? "filename")
                     <*> (o .:? "createTime"))

instance ToJSON BuildTrigger where
        toJSON BuildTrigger'{..}
          = object
              (catMaybes
                 [("substitutions" .=) <$> _btSubstitutions,
                  ("disabled" .=) <$> _btDisabled,
                  ("triggerTemplate" .=) <$> _btTriggerTemplate,
                  ("build" .=) <$> _btBuild, ("id" .=) <$> _btId,
                  ("description" .=) <$> _btDescription,
                  ("filename" .=) <$> _btFilename,
                  ("createTime" .=) <$> _btCreateTime])

-- | An image built by the pipeline.
--
-- /See:/ 'builtImage' smart constructor.
data BuiltImage = BuiltImage'
    { _biPushTiming :: !(Maybe TimeSpan)
    , _biName :: !(Maybe Text)
    , _biDigest :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuiltImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biPushTiming'
--
-- * 'biName'
--
-- * 'biDigest'
builtImage
    :: BuiltImage
builtImage = 
    BuiltImage'
    { _biPushTiming = Nothing
    , _biName = Nothing
    , _biDigest = Nothing
    }

-- | Stores timing information for pushing the specified image. \'OutputOnly
biPushTiming :: Lens' BuiltImage (Maybe TimeSpan)
biPushTiming
  = lens _biPushTiming (\ s a -> s{_biPushTiming = a})

-- | Name used to push the container image to Google Container Registry, as
-- presented to \`docker push\`.
biName :: Lens' BuiltImage (Maybe Text)
biName = lens _biName (\ s a -> s{_biName = a})

-- | Docker Registry 2.0 digest.
biDigest :: Lens' BuiltImage (Maybe Text)
biDigest = lens _biDigest (\ s a -> s{_biDigest = a})

instance FromJSON BuiltImage where
        parseJSON
          = withObject "BuiltImage"
              (\ o ->
                 BuiltImage' <$>
                   (o .:? "pushTiming") <*> (o .:? "name") <*>
                     (o .:? "digest"))

instance ToJSON BuiltImage where
        toJSON BuiltImage'{..}
          = object
              (catMaybes
                 [("pushTiming" .=) <$> _biPushTiming,
                  ("name" .=) <$> _biName,
                  ("digest" .=) <$> _biDigest])
