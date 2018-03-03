{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.PubSub.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.PubSub.Types.Product where

import Network.Google.Prelude
import Network.Google.PubSub.Types.Sum

-- | Configuration for a push delivery endpoint.
--
-- /See:/ 'pushConfig' smart constructor.
data PushConfig = PushConfig'
    { _pcAttributes :: !(Maybe PushConfigAttributes)
    , _pcPushEndpoint :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PushConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAttributes'
--
-- * 'pcPushEndpoint'
pushConfig
    :: PushConfig
pushConfig = 
    PushConfig'
    { _pcAttributes = Nothing
    , _pcPushEndpoint = Nothing
    }

-- | Endpoint configuration attributes. Every endpoint has a set of API
-- supported attributes that can be used to control different aspects of
-- the message delivery. The currently supported attribute is
-- \`x-goog-version\`, which you can use to change the format of the pushed
-- message. This attribute indicates the version of the data expected by
-- the endpoint. This controls the shape of the pushed message (i.e., its
-- fields and metadata). The endpoint version is based on the version of
-- the Pub\/Sub API. If not present during the \`CreateSubscription\` call,
-- it will default to the version of the API used to make such call. If not
-- present during a \`ModifyPushConfig\` call, its value will not be
-- changed. \`GetSubscription\` calls will always return a valid version,
-- even if the subscription was created without this attribute. The
-- possible values for this attribute are: * \`v1beta1\`: uses the push
-- format defined in the v1beta1 Pub\/Sub API. * \`v1\` or \`v1beta2\`:
-- uses the push format defined in the v1 Pub\/Sub API.
pcAttributes :: Lens' PushConfig (Maybe PushConfigAttributes)
pcAttributes
  = lens _pcAttributes (\ s a -> s{_pcAttributes = a})

-- | A URL locating the endpoint to which messages should be pushed. For
-- example, a Webhook endpoint might use \"https:\/\/example.com\/push\".
pcPushEndpoint :: Lens' PushConfig (Maybe Text)
pcPushEndpoint
  = lens _pcPushEndpoint
      (\ s a -> s{_pcPushEndpoint = a})

instance FromJSON PushConfig where
        parseJSON
          = withObject "PushConfig"
              (\ o ->
                 PushConfig' <$>
                   (o .:? "attributes") <*> (o .:? "pushEndpoint"))

instance ToJSON PushConfig where
        toJSON PushConfig'{..}
          = object
              (catMaybes
                 [("attributes" .=) <$> _pcAttributes,
                  ("pushEndpoint" .=) <$> _pcPushEndpoint])

-- | A message and its corresponding acknowledgment ID.
--
-- /See:/ 'receivedMessage' smart constructor.
data ReceivedMessage = ReceivedMessage'
    { _rmAckId :: !(Maybe Text)
    , _rmMessage :: !(Maybe PubsubMessage)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReceivedMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmAckId'
--
-- * 'rmMessage'
receivedMessage
    :: ReceivedMessage
receivedMessage = 
    ReceivedMessage'
    { _rmAckId = Nothing
    , _rmMessage = Nothing
    }

-- | This ID can be used to acknowledge the received message.
rmAckId :: Lens' ReceivedMessage (Maybe Text)
rmAckId = lens _rmAckId (\ s a -> s{_rmAckId = a})

-- | The message.
rmMessage :: Lens' ReceivedMessage (Maybe PubsubMessage)
rmMessage
  = lens _rmMessage (\ s a -> s{_rmMessage = a})

instance FromJSON ReceivedMessage where
        parseJSON
          = withObject "ReceivedMessage"
              (\ o ->
                 ReceivedMessage' <$>
                   (o .:? "ackId") <*> (o .:? "message"))

instance ToJSON ReceivedMessage where
        toJSON ReceivedMessage'{..}
          = object
              (catMaybes
                 [("ackId" .=) <$> _rmAckId,
                  ("message" .=) <$> _rmMessage])

-- | A snapshot resource.
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
    { _sTopic :: !(Maybe Text)
    , _sName :: !(Maybe Text)
    , _sExpireTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTopic'
--
-- * 'sName'
--
-- * 'sExpireTime'
snapshot
    :: Snapshot
snapshot = 
    Snapshot'
    { _sTopic = Nothing
    , _sName = Nothing
    , _sExpireTime = Nothing
    }

-- | The name of the topic from which this snapshot is retaining messages.
sTopic :: Lens' Snapshot (Maybe Text)
sTopic = lens _sTopic (\ s a -> s{_sTopic = a})

-- | The name of the snapshot.
sName :: Lens' Snapshot (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The snapshot is guaranteed to exist up until this time. A newly-created
-- snapshot expires no later than 7 days from the time of its creation. Its
-- exact lifetime is determined at creation by the existing backlog in the
-- source subscription. Specifically, the lifetime of the snapshot is \`7
-- days - (age of oldest unacked message in the subscription)\`. For
-- example, consider a subscription whose oldest unacked message is 3 days
-- old. If a snapshot is created from this subscription, the snapshot --
-- which will always capture this 3-day-old backlog as long as the snapshot
-- exists -- will expire in 4 days. The service will refuse to create a
-- snapshot that would expire in less than 1 hour after creation.
sExpireTime :: Lens' Snapshot (Maybe UTCTime)
sExpireTime
  = lens _sExpireTime (\ s a -> s{_sExpireTime = a}) .
      mapping _DateTime

instance FromJSON Snapshot where
        parseJSON
          = withObject "Snapshot"
              (\ o ->
                 Snapshot' <$>
                   (o .:? "topic") <*> (o .:? "name") <*>
                     (o .:? "expireTime"))

instance ToJSON Snapshot where
        toJSON Snapshot'{..}
          = object
              (catMaybes
                 [("topic" .=) <$> _sTopic, ("name" .=) <$> _sName,
                  ("expireTime" .=) <$> _sExpireTime])

-- | Response for the \`ListTopicSnapshots\` method.
--
-- /See:/ 'listTopicSnapshotsResponse' smart constructor.
data ListTopicSnapshotsResponse = ListTopicSnapshotsResponse'
    { _ltsrNextPageToken :: !(Maybe Text)
    , _ltsrSnapshots :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTopicSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsrNextPageToken'
--
-- * 'ltsrSnapshots'
listTopicSnapshotsResponse
    :: ListTopicSnapshotsResponse
listTopicSnapshotsResponse = 
    ListTopicSnapshotsResponse'
    { _ltsrNextPageToken = Nothing
    , _ltsrSnapshots = Nothing
    }

-- | If not empty, indicates that there may be more snapshots that match the
-- request; this value should be passed in a new
-- \`ListTopicSnapshotsRequest\` to get more snapshots.
ltsrNextPageToken :: Lens' ListTopicSnapshotsResponse (Maybe Text)
ltsrNextPageToken
  = lens _ltsrNextPageToken
      (\ s a -> s{_ltsrNextPageToken = a})

-- | The names of the snapshots that match the request.
ltsrSnapshots :: Lens' ListTopicSnapshotsResponse [Text]
ltsrSnapshots
  = lens _ltsrSnapshots
      (\ s a -> s{_ltsrSnapshots = a})
      . _Default
      . _Coerce

instance FromJSON ListTopicSnapshotsResponse where
        parseJSON
          = withObject "ListTopicSnapshotsResponse"
              (\ o ->
                 ListTopicSnapshotsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "snapshots" .!= mempty))

instance ToJSON ListTopicSnapshotsResponse where
        toJSON ListTopicSnapshotsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _ltsrNextPageToken,
                  ("snapshots" .=) <$> _ltsrSnapshots])

-- | Request for the ModifyAckDeadline method.
--
-- /See:/ 'modifyAckDeadlineRequest' smart constructor.
data ModifyAckDeadlineRequest = ModifyAckDeadlineRequest'
    { _madrAckIds :: !(Maybe [Text])
    , _madrAckDeadlineSeconds :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyAckDeadlineRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'madrAckIds'
--
-- * 'madrAckDeadlineSeconds'
modifyAckDeadlineRequest
    :: ModifyAckDeadlineRequest
modifyAckDeadlineRequest = 
    ModifyAckDeadlineRequest'
    { _madrAckIds = Nothing
    , _madrAckDeadlineSeconds = Nothing
    }

-- | List of acknowledgment IDs.
madrAckIds :: Lens' ModifyAckDeadlineRequest [Text]
madrAckIds
  = lens _madrAckIds (\ s a -> s{_madrAckIds = a}) .
      _Default
      . _Coerce

-- | The new ack deadline with respect to the time this request was sent to
-- the Pub\/Sub system. For example, if the value is 10, the new ack
-- deadline will expire 10 seconds after the \`ModifyAckDeadline\` call was
-- made. Specifying zero may immediately make the message available for
-- another pull request. The minimum deadline you can specify is 0 seconds.
-- The maximum deadline you can specify is 600 seconds (10 minutes).
madrAckDeadlineSeconds :: Lens' ModifyAckDeadlineRequest (Maybe Int32)
madrAckDeadlineSeconds
  = lens _madrAckDeadlineSeconds
      (\ s a -> s{_madrAckDeadlineSeconds = a})
      . mapping _Coerce

instance FromJSON ModifyAckDeadlineRequest where
        parseJSON
          = withObject "ModifyAckDeadlineRequest"
              (\ o ->
                 ModifyAckDeadlineRequest' <$>
                   (o .:? "ackIds" .!= mempty) <*>
                     (o .:? "ackDeadlineSeconds"))

instance ToJSON ModifyAckDeadlineRequest where
        toJSON ModifyAckDeadlineRequest'{..}
          = object
              (catMaybes
                 [("ackIds" .=) <$> _madrAckIds,
                  ("ackDeadlineSeconds" .=) <$>
                    _madrAckDeadlineSeconds])

-- | Request for the ModifyPushConfig method.
--
-- /See:/ 'modifyPushConfigRequest' smart constructor.
newtype ModifyPushConfigRequest = ModifyPushConfigRequest'
    { _mpcrPushConfig :: Maybe PushConfig
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyPushConfigRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpcrPushConfig'
modifyPushConfigRequest
    :: ModifyPushConfigRequest
modifyPushConfigRequest = 
    ModifyPushConfigRequest'
    { _mpcrPushConfig = Nothing
    }

-- | The push configuration for future deliveries. An empty \`pushConfig\`
-- indicates that the Pub\/Sub system should stop pushing messages from the
-- given subscription and allow messages to be pulled and acknowledged -
-- effectively pausing the subscription if \`Pull\` or \`StreamingPull\` is
-- not called.
mpcrPushConfig :: Lens' ModifyPushConfigRequest (Maybe PushConfig)
mpcrPushConfig
  = lens _mpcrPushConfig
      (\ s a -> s{_mpcrPushConfig = a})

instance FromJSON ModifyPushConfigRequest where
        parseJSON
          = withObject "ModifyPushConfigRequest"
              (\ o ->
                 ModifyPushConfigRequest' <$> (o .:? "pushConfig"))

instance ToJSON ModifyPushConfigRequest where
        toJSON ModifyPushConfigRequest'{..}
          = object
              (catMaybes [("pushConfig" .=) <$> _mpcrPushConfig])

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

-- | A message data and its attributes. The message payload must not be
-- empty; it must contain either a non-empty data field, or at least one
-- attribute.
--
-- /See:/ 'pubsubMessage' smart constructor.
data PubsubMessage = PubsubMessage'
    { _pmData :: !(Maybe Bytes)
    , _pmPublishTime :: !(Maybe DateTime')
    , _pmAttributes :: !(Maybe PubsubMessageAttributes)
    , _pmMessageId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PubsubMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmData'
--
-- * 'pmPublishTime'
--
-- * 'pmAttributes'
--
-- * 'pmMessageId'
pubsubMessage
    :: PubsubMessage
pubsubMessage = 
    PubsubMessage'
    { _pmData = Nothing
    , _pmPublishTime = Nothing
    , _pmAttributes = Nothing
    , _pmMessageId = Nothing
    }

-- | The message payload.
pmData :: Lens' PubsubMessage (Maybe ByteString)
pmData
  = lens _pmData (\ s a -> s{_pmData = a}) .
      mapping _Bytes

-- | The time at which the message was published, populated by the server
-- when it receives the \`Publish\` call. It must not be populated by the
-- publisher in a \`Publish\` call.
pmPublishTime :: Lens' PubsubMessage (Maybe UTCTime)
pmPublishTime
  = lens _pmPublishTime
      (\ s a -> s{_pmPublishTime = a})
      . mapping _DateTime

-- | Optional attributes for this message.
pmAttributes :: Lens' PubsubMessage (Maybe PubsubMessageAttributes)
pmAttributes
  = lens _pmAttributes (\ s a -> s{_pmAttributes = a})

-- | ID of this message, assigned by the server when the message is
-- published. Guaranteed to be unique within the topic. This value may be
-- read by a subscriber that receives a \`PubsubMessage\` via a \`Pull\`
-- call or a push delivery. It must not be populated by the publisher in a
-- \`Publish\` call.
pmMessageId :: Lens' PubsubMessage (Maybe Text)
pmMessageId
  = lens _pmMessageId (\ s a -> s{_pmMessageId = a})

instance FromJSON PubsubMessage where
        parseJSON
          = withObject "PubsubMessage"
              (\ o ->
                 PubsubMessage' <$>
                   (o .:? "data") <*> (o .:? "publishTime") <*>
                     (o .:? "attributes")
                     <*> (o .:? "messageId"))

instance ToJSON PubsubMessage where
        toJSON PubsubMessage'{..}
          = object
              (catMaybes
                 [("data" .=) <$> _pmData,
                  ("publishTime" .=) <$> _pmPublishTime,
                  ("attributes" .=) <$> _pmAttributes,
                  ("messageId" .=) <$> _pmMessageId])

-- | Response for the \`ListTopicSubscriptions\` method.
--
-- /See:/ 'listTopicSubscriptionsResponse' smart constructor.
data ListTopicSubscriptionsResponse = ListTopicSubscriptionsResponse'
    { _lNextPageToken :: !(Maybe Text)
    , _lSubscriptions :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTopicSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextPageToken'
--
-- * 'lSubscriptions'
listTopicSubscriptionsResponse
    :: ListTopicSubscriptionsResponse
listTopicSubscriptionsResponse = 
    ListTopicSubscriptionsResponse'
    { _lNextPageToken = Nothing
    , _lSubscriptions = Nothing
    }

-- | If not empty, indicates that there may be more subscriptions that match
-- the request; this value should be passed in a new
-- \`ListTopicSubscriptionsRequest\` to get more subscriptions.
lNextPageToken :: Lens' ListTopicSubscriptionsResponse (Maybe Text)
lNextPageToken
  = lens _lNextPageToken
      (\ s a -> s{_lNextPageToken = a})

-- | The names of the subscriptions that match the request.
lSubscriptions :: Lens' ListTopicSubscriptionsResponse [Text]
lSubscriptions
  = lens _lSubscriptions
      (\ s a -> s{_lSubscriptions = a})
      . _Default
      . _Coerce

instance FromJSON ListTopicSubscriptionsResponse
         where
        parseJSON
          = withObject "ListTopicSubscriptionsResponse"
              (\ o ->
                 ListTopicSubscriptionsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "subscriptions" .!= mempty))

instance ToJSON ListTopicSubscriptionsResponse where
        toJSON ListTopicSubscriptionsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lNextPageToken,
                  ("subscriptions" .=) <$> _lSubscriptions])

-- | Response for the \`ListTopics\` method.
--
-- /See:/ 'listTopicsResponse' smart constructor.
data ListTopicsResponse = ListTopicsResponse'
    { _ltrNextPageToken :: !(Maybe Text)
    , _ltrTopics :: !(Maybe [Topic])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTopicsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrNextPageToken'
--
-- * 'ltrTopics'
listTopicsResponse
    :: ListTopicsResponse
listTopicsResponse = 
    ListTopicsResponse'
    { _ltrNextPageToken = Nothing
    , _ltrTopics = Nothing
    }

-- | If not empty, indicates that there may be more topics that match the
-- request; this value should be passed in a new \`ListTopicsRequest\`.
ltrNextPageToken :: Lens' ListTopicsResponse (Maybe Text)
ltrNextPageToken
  = lens _ltrNextPageToken
      (\ s a -> s{_ltrNextPageToken = a})

-- | The resulting topics.
ltrTopics :: Lens' ListTopicsResponse [Topic]
ltrTopics
  = lens _ltrTopics (\ s a -> s{_ltrTopics = a}) .
      _Default
      . _Coerce

instance FromJSON ListTopicsResponse where
        parseJSON
          = withObject "ListTopicsResponse"
              (\ o ->
                 ListTopicsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "topics" .!= mempty))

instance ToJSON ListTopicsResponse where
        toJSON ListTopicsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _ltrNextPageToken,
                  ("topics" .=) <$> _ltrTopics])

-- | Response for the \`Pull\` method.
--
-- /See:/ 'pullResponse' smart constructor.
newtype PullResponse = PullResponse'
    { _prReceivedMessages :: Maybe [ReceivedMessage]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PullResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prReceivedMessages'
pullResponse
    :: PullResponse
pullResponse = 
    PullResponse'
    { _prReceivedMessages = Nothing
    }

-- | Received Pub\/Sub messages. The Pub\/Sub system will return zero
-- messages if there are no more available in the backlog. The Pub\/Sub
-- system may return fewer than the \`maxMessages\` requested even if there
-- are more messages available in the backlog.
prReceivedMessages :: Lens' PullResponse [ReceivedMessage]
prReceivedMessages
  = lens _prReceivedMessages
      (\ s a -> s{_prReceivedMessages = a})
      . _Default
      . _Coerce

instance FromJSON PullResponse where
        parseJSON
          = withObject "PullResponse"
              (\ o ->
                 PullResponse' <$>
                   (o .:? "receivedMessages" .!= mempty))

instance ToJSON PullResponse where
        toJSON PullResponse'{..}
          = object
              (catMaybes
                 [("receivedMessages" .=) <$> _prReceivedMessages])

-- | Response for the \`ListSnapshots\` method.
--
-- /See:/ 'listSnapshotsResponse' smart constructor.
data ListSnapshotsResponse = ListSnapshotsResponse'
    { _lsrNextPageToken :: !(Maybe Text)
    , _lsrSnapshots :: !(Maybe [Snapshot])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrNextPageToken'
--
-- * 'lsrSnapshots'
listSnapshotsResponse
    :: ListSnapshotsResponse
listSnapshotsResponse = 
    ListSnapshotsResponse'
    { _lsrNextPageToken = Nothing
    , _lsrSnapshots = Nothing
    }

-- | If not empty, indicates that there may be more snapshot that match the
-- request; this value should be passed in a new \`ListSnapshotsRequest\`.
lsrNextPageToken :: Lens' ListSnapshotsResponse (Maybe Text)
lsrNextPageToken
  = lens _lsrNextPageToken
      (\ s a -> s{_lsrNextPageToken = a})

-- | The resulting snapshots.
lsrSnapshots :: Lens' ListSnapshotsResponse [Snapshot]
lsrSnapshots
  = lens _lsrSnapshots (\ s a -> s{_lsrSnapshots = a})
      . _Default
      . _Coerce

instance FromJSON ListSnapshotsResponse where
        parseJSON
          = withObject "ListSnapshotsResponse"
              (\ o ->
                 ListSnapshotsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "snapshots" .!= mempty))

instance ToJSON ListSnapshotsResponse where
        toJSON ListSnapshotsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lsrNextPageToken,
                  ("snapshots" .=) <$> _lsrSnapshots])

-- | Request message for \`SetIamPolicy\` method.
--
-- /See:/ 'setIAMPolicyRequest' smart constructor.
newtype SetIAMPolicyRequest = SetIAMPolicyRequest'
    { _siprPolicy :: Maybe Policy
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetIAMPolicyRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siprPolicy'
setIAMPolicyRequest
    :: SetIAMPolicyRequest
setIAMPolicyRequest = 
    SetIAMPolicyRequest'
    { _siprPolicy = Nothing
    }

-- | REQUIRED: The complete policy to be applied to the \`resource\`. The
-- size of the policy is limited to a few 10s of KB. An empty policy is a
-- valid policy but certain Cloud Platform services (such as Projects)
-- might reject them.
siprPolicy :: Lens' SetIAMPolicyRequest (Maybe Policy)
siprPolicy
  = lens _siprPolicy (\ s a -> s{_siprPolicy = a})

instance FromJSON SetIAMPolicyRequest where
        parseJSON
          = withObject "SetIAMPolicyRequest"
              (\ o -> SetIAMPolicyRequest' <$> (o .:? "policy"))

instance ToJSON SetIAMPolicyRequest where
        toJSON SetIAMPolicyRequest'{..}
          = object (catMaybes [("policy" .=) <$> _siprPolicy])

-- | Request for the \`CreateSnapshot\` method.
--
-- /See:/ 'createSnapshotRequest' smart constructor.
newtype CreateSnapshotRequest = CreateSnapshotRequest'
    { _csrSubscription :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSnapshotRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrSubscription'
createSnapshotRequest
    :: CreateSnapshotRequest
createSnapshotRequest = 
    CreateSnapshotRequest'
    { _csrSubscription = Nothing
    }

-- | The subscription whose backlog the snapshot retains. Specifically, the
-- created snapshot is guaranteed to retain: (a) The existing backlog on
-- the subscription. More precisely, this is defined as the messages in the
-- subscription\'s backlog that are unacknowledged upon the successful
-- completion of the \`CreateSnapshot\` request; as well as: (b) Any
-- messages published to the subscription\'s topic following the successful
-- completion of the CreateSnapshot request. Format is
-- \`projects\/{project}\/subscriptions\/{sub}\`.
csrSubscription :: Lens' CreateSnapshotRequest (Maybe Text)
csrSubscription
  = lens _csrSubscription
      (\ s a -> s{_csrSubscription = a})

instance FromJSON CreateSnapshotRequest where
        parseJSON
          = withObject "CreateSnapshotRequest"
              (\ o ->
                 CreateSnapshotRequest' <$> (o .:? "subscription"))

instance ToJSON CreateSnapshotRequest where
        toJSON CreateSnapshotRequest'{..}
          = object
              (catMaybes
                 [("subscription" .=) <$> _csrSubscription])

-- | Request for the \`Seek\` method.
--
-- /See:/ 'seekRequest' smart constructor.
data SeekRequest = SeekRequest'
    { _srSnapshot :: !(Maybe Text)
    , _srTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SeekRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srSnapshot'
--
-- * 'srTime'
seekRequest
    :: SeekRequest
seekRequest = 
    SeekRequest'
    { _srSnapshot = Nothing
    , _srTime = Nothing
    }

-- | The snapshot to seek to. The snapshot\'s topic must be the same as that
-- of the provided subscription. Format is
-- \`projects\/{project}\/snapshots\/{snap}\`.
srSnapshot :: Lens' SeekRequest (Maybe Text)
srSnapshot
  = lens _srSnapshot (\ s a -> s{_srSnapshot = a})

-- | The time to seek to. Messages retained in the subscription that were
-- published before this time are marked as acknowledged, and messages
-- retained in the subscription that were published after this time are
-- marked as unacknowledged. Note that this operation affects only those
-- messages retained in the subscription (configured by the combination of
-- \`message_retention_duration\` and \`retain_acked_messages\`). For
-- example, if \`time\` corresponds to a point before the message retention
-- window (or to a point before the system\'s notion of the subscription
-- creation time), only retained messages will be marked as unacknowledged,
-- and already-expunged messages will not be restored.
srTime :: Lens' SeekRequest (Maybe UTCTime)
srTime
  = lens _srTime (\ s a -> s{_srTime = a}) .
      mapping _DateTime

instance FromJSON SeekRequest where
        parseJSON
          = withObject "SeekRequest"
              (\ o ->
                 SeekRequest' <$>
                   (o .:? "snapshot") <*> (o .:? "time"))

instance ToJSON SeekRequest where
        toJSON SeekRequest'{..}
          = object
              (catMaybes
                 [("snapshot" .=) <$> _srSnapshot,
                  ("time" .=) <$> _srTime])

-- | A topic resource.
--
-- /See:/ 'topic' smart constructor.
newtype Topic = Topic'
    { _tName :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Topic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tName'
topic
    :: Topic
topic = 
    Topic'
    { _tName = Nothing
    }

-- | The name of the topic. It must have the format
-- \`\"projects\/{project}\/topics\/{topic}\"\`. \`{topic}\` must start
-- with a letter, and contain only letters (\`[A-Za-z]\`), numbers
-- (\`[0-9]\`), dashes (\`-\`), underscores (\`_\`), periods (\`.\`),
-- tildes (\`~\`), plus (\`+\`) or percent signs (\`%\`). It must be
-- between 3 and 255 characters in length, and it must not start with
-- \`\"goog\"\`.
tName :: Lens' Topic (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

instance FromJSON Topic where
        parseJSON
          = withObject "Topic"
              (\ o -> Topic' <$> (o .:? "name"))

instance ToJSON Topic where
        toJSON Topic'{..}
          = object (catMaybes [("name" .=) <$> _tName])

-- | Request for the UpdateSnapshot method.
--
-- /See:/ 'updateSnapshotRequest' smart constructor.
data UpdateSnapshotRequest = UpdateSnapshotRequest'
    { _usrSnapshot :: !(Maybe Snapshot)
    , _usrUpdateMask :: !(Maybe FieldMask)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateSnapshotRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrSnapshot'
--
-- * 'usrUpdateMask'
updateSnapshotRequest
    :: UpdateSnapshotRequest
updateSnapshotRequest = 
    UpdateSnapshotRequest'
    { _usrSnapshot = Nothing
    , _usrUpdateMask = Nothing
    }

-- | The updated snpashot object.
usrSnapshot :: Lens' UpdateSnapshotRequest (Maybe Snapshot)
usrSnapshot
  = lens _usrSnapshot (\ s a -> s{_usrSnapshot = a})

-- | Indicates which fields in the provided snapshot to update. Must be
-- specified and non-empty.
usrUpdateMask :: Lens' UpdateSnapshotRequest (Maybe FieldMask)
usrUpdateMask
  = lens _usrUpdateMask
      (\ s a -> s{_usrUpdateMask = a})

instance FromJSON UpdateSnapshotRequest where
        parseJSON
          = withObject "UpdateSnapshotRequest"
              (\ o ->
                 UpdateSnapshotRequest' <$>
                   (o .:? "snapshot") <*> (o .:? "updateMask"))

instance ToJSON UpdateSnapshotRequest where
        toJSON UpdateSnapshotRequest'{..}
          = object
              (catMaybes
                 [("snapshot" .=) <$> _usrSnapshot,
                  ("updateMask" .=) <$> _usrUpdateMask])

-- | Request for the \`Pull\` method.
--
-- /See:/ 'pullRequest' smart constructor.
data PullRequest = PullRequest'
    { _prMaxMessages :: !(Maybe (Textual Int32))
    , _prReturnImmediately :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prMaxMessages'
--
-- * 'prReturnImmediately'
pullRequest
    :: PullRequest
pullRequest = 
    PullRequest'
    { _prMaxMessages = Nothing
    , _prReturnImmediately = Nothing
    }

-- | The maximum number of messages returned for this request. The Pub\/Sub
-- system may return fewer than the number specified.
prMaxMessages :: Lens' PullRequest (Maybe Int32)
prMaxMessages
  = lens _prMaxMessages
      (\ s a -> s{_prMaxMessages = a})
      . mapping _Coerce

-- | If this field set to true, the system will respond immediately even if
-- it there are no messages available to return in the \`Pull\` response.
-- Otherwise, the system may wait (for a bounded amount of time) until at
-- least one message is available, rather than returning no messages. The
-- client may cancel the request if it does not wish to wait any longer for
-- the response.
prReturnImmediately :: Lens' PullRequest (Maybe Bool)
prReturnImmediately
  = lens _prReturnImmediately
      (\ s a -> s{_prReturnImmediately = a})

instance FromJSON PullRequest where
        parseJSON
          = withObject "PullRequest"
              (\ o ->
                 PullRequest' <$>
                   (o .:? "maxMessages") <*>
                     (o .:? "returnImmediately"))

instance ToJSON PullRequest where
        toJSON PullRequest'{..}
          = object
              (catMaybes
                 [("maxMessages" .=) <$> _prMaxMessages,
                  ("returnImmediately" .=) <$> _prReturnImmediately])

-- | Optional attributes for this message.
--
-- /See:/ 'pubsubMessageAttributes' smart constructor.
newtype PubsubMessageAttributes = PubsubMessageAttributes'
    { _pmaAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PubsubMessageAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAddtional'
pubsubMessageAttributes
    :: HashMap Text Text -- ^ 'pmaAddtional'
    -> PubsubMessageAttributes
pubsubMessageAttributes pPmaAddtional_ = 
    PubsubMessageAttributes'
    { _pmaAddtional = _Coerce # pPmaAddtional_
    }

pmaAddtional :: Lens' PubsubMessageAttributes (HashMap Text Text)
pmaAddtional
  = lens _pmaAddtional (\ s a -> s{_pmaAddtional = a})
      . _Coerce

instance FromJSON PubsubMessageAttributes where
        parseJSON
          = withObject "PubsubMessageAttributes"
              (\ o ->
                 PubsubMessageAttributes' <$> (parseJSONObject o))

instance ToJSON PubsubMessageAttributes where
        toJSON = toJSON . _pmaAddtional

-- | Request message for \`TestIamPermissions\` method.
--
-- /See:/ 'testIAMPermissionsRequest' smart constructor.
newtype TestIAMPermissionsRequest = TestIAMPermissionsRequest'
    { _tiprPermissions :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestIAMPermissionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiprPermissions'
testIAMPermissionsRequest
    :: TestIAMPermissionsRequest
testIAMPermissionsRequest = 
    TestIAMPermissionsRequest'
    { _tiprPermissions = Nothing
    }

-- | The set of permissions to check for the \`resource\`. Permissions with
-- wildcards (such as \'*\' or \'storage.*\') are not allowed. For more
-- information see [IAM
-- Overview](https:\/\/cloud.google.com\/iam\/docs\/overview#permissions).
tiprPermissions :: Lens' TestIAMPermissionsRequest [Text]
tiprPermissions
  = lens _tiprPermissions
      (\ s a -> s{_tiprPermissions = a})
      . _Default
      . _Coerce

instance FromJSON TestIAMPermissionsRequest where
        parseJSON
          = withObject "TestIAMPermissionsRequest"
              (\ o ->
                 TestIAMPermissionsRequest' <$>
                   (o .:? "permissions" .!= mempty))

instance ToJSON TestIAMPermissionsRequest where
        toJSON TestIAMPermissionsRequest'{..}
          = object
              (catMaybes [("permissions" .=) <$> _tiprPermissions])

-- | Response for the \`Publish\` method.
--
-- /See:/ 'publishResponse' smart constructor.
newtype PublishResponse = PublishResponse'
    { _prMessageIds :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prMessageIds'
publishResponse
    :: PublishResponse
publishResponse = 
    PublishResponse'
    { _prMessageIds = Nothing
    }

-- | The server-assigned ID of each published message, in the same order as
-- the messages in the request. IDs are guaranteed to be unique within the
-- topic.
prMessageIds :: Lens' PublishResponse [Text]
prMessageIds
  = lens _prMessageIds (\ s a -> s{_prMessageIds = a})
      . _Default
      . _Coerce

instance FromJSON PublishResponse where
        parseJSON
          = withObject "PublishResponse"
              (\ o ->
                 PublishResponse' <$> (o .:? "messageIds" .!= mempty))

instance ToJSON PublishResponse where
        toJSON PublishResponse'{..}
          = object
              (catMaybes [("messageIds" .=) <$> _prMessageIds])

-- | Request for the Publish method.
--
-- /See:/ 'publishRequest' smart constructor.
newtype PublishRequest = PublishRequest'
    { _prMessages :: Maybe [PubsubMessage]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublishRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prMessages'
publishRequest
    :: PublishRequest
publishRequest = 
    PublishRequest'
    { _prMessages = Nothing
    }

-- | The messages to publish.
prMessages :: Lens' PublishRequest [PubsubMessage]
prMessages
  = lens _prMessages (\ s a -> s{_prMessages = a}) .
      _Default
      . _Coerce

instance FromJSON PublishRequest where
        parseJSON
          = withObject "PublishRequest"
              (\ o ->
                 PublishRequest' <$> (o .:? "messages" .!= mempty))

instance ToJSON PublishRequest where
        toJSON PublishRequest'{..}
          = object
              (catMaybes [("messages" .=) <$> _prMessages])

-- | Response message for \`TestIamPermissions\` method.
--
-- /See:/ 'testIAMPermissionsResponse' smart constructor.
newtype TestIAMPermissionsResponse = TestIAMPermissionsResponse'
    { _tiamprPermissions :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestIAMPermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiamprPermissions'
testIAMPermissionsResponse
    :: TestIAMPermissionsResponse
testIAMPermissionsResponse = 
    TestIAMPermissionsResponse'
    { _tiamprPermissions = Nothing
    }

-- | A subset of \`TestPermissionsRequest.permissions\` that the caller is
-- allowed.
tiamprPermissions :: Lens' TestIAMPermissionsResponse [Text]
tiamprPermissions
  = lens _tiamprPermissions
      (\ s a -> s{_tiamprPermissions = a})
      . _Default
      . _Coerce

instance FromJSON TestIAMPermissionsResponse where
        parseJSON
          = withObject "TestIAMPermissionsResponse"
              (\ o ->
                 TestIAMPermissionsResponse' <$>
                   (o .:? "permissions" .!= mempty))

instance ToJSON TestIAMPermissionsResponse where
        toJSON TestIAMPermissionsResponse'{..}
          = object
              (catMaybes
                 [("permissions" .=) <$> _tiamprPermissions])

-- | Defines an Identity and Access Management (IAM) policy. It is used to
-- specify access control policies for Cloud Platform resources. A
-- \`Policy\` consists of a list of \`bindings\`. A \`Binding\` binds a
-- list of \`members\` to a \`role\`, where the members can be user
-- accounts, Google groups, Google domains, and service accounts. A
-- \`role\` is a named list of permissions defined by IAM. **Example** {
-- \"bindings\": [ { \"role\": \"roles\/owner\", \"members\": [
-- \"user:mike\'example.com\", \"group:admins\'example.com\",
-- \"domain:google.com\",
-- \"serviceAccount:my-other-app\'appspot.gserviceaccount.com\", ] }, {
-- \"role\": \"roles\/viewer\", \"members\": [\"user:sean\'example.com\"] }
-- ] } For a description of IAM and its features, see the [IAM developer\'s
-- guide](https:\/\/cloud.google.com\/iam\/docs).
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
    { _pEtag :: !(Maybe Bytes)
    , _pVersion :: !(Maybe (Textual Int32))
    , _pBindings :: !(Maybe [Binding])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pEtag'
--
-- * 'pVersion'
--
-- * 'pBindings'
policy
    :: Policy
policy = 
    Policy'
    { _pEtag = Nothing
    , _pVersion = Nothing
    , _pBindings = Nothing
    }

-- | \`etag\` is used for optimistic concurrency control as a way to help
-- prevent simultaneous updates of a policy from overwriting each other. It
-- is strongly suggested that systems make use of the \`etag\` in the
-- read-modify-write cycle to perform policy updates in order to avoid race
-- conditions: An \`etag\` is returned in the response to \`getIamPolicy\`,
-- and systems are expected to put that etag in the request to
-- \`setIamPolicy\` to ensure that their change will be applied to the same
-- version of the policy. If no \`etag\` is provided in the call to
-- \`setIamPolicy\`, then the existing policy is overwritten blindly.
pEtag :: Lens' Policy (Maybe ByteString)
pEtag
  = lens _pEtag (\ s a -> s{_pEtag = a}) .
      mapping _Bytes

-- | Deprecated.
pVersion :: Lens' Policy (Maybe Int32)
pVersion
  = lens _pVersion (\ s a -> s{_pVersion = a}) .
      mapping _Coerce

-- | Associates a list of \`members\` to a \`role\`. \`bindings\` with no
-- members will result in an error.
pBindings :: Lens' Policy [Binding]
pBindings
  = lens _pBindings (\ s a -> s{_pBindings = a}) .
      _Default
      . _Coerce

instance FromJSON Policy where
        parseJSON
          = withObject "Policy"
              (\ o ->
                 Policy' <$>
                   (o .:? "etag") <*> (o .:? "version") <*>
                     (o .:? "bindings" .!= mempty))

instance ToJSON Policy where
        toJSON Policy'{..}
          = object
              (catMaybes
                 [("etag" .=) <$> _pEtag,
                  ("version" .=) <$> _pVersion,
                  ("bindings" .=) <$> _pBindings])

--
-- /See:/ 'seekResponse' smart constructor.
data SeekResponse =
    SeekResponse' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SeekResponse' with the minimum fields required to make a request.
--
seekResponse
    :: SeekResponse
seekResponse = SeekResponse'

instance FromJSON SeekResponse where
        parseJSON
          = withObject "SeekResponse"
              (\ o -> pure SeekResponse')

instance ToJSON SeekResponse where
        toJSON = const emptyObject

-- | Endpoint configuration attributes. Every endpoint has a set of API
-- supported attributes that can be used to control different aspects of
-- the message delivery. The currently supported attribute is
-- \`x-goog-version\`, which you can use to change the format of the pushed
-- message. This attribute indicates the version of the data expected by
-- the endpoint. This controls the shape of the pushed message (i.e., its
-- fields and metadata). The endpoint version is based on the version of
-- the Pub\/Sub API. If not present during the \`CreateSubscription\` call,
-- it will default to the version of the API used to make such call. If not
-- present during a \`ModifyPushConfig\` call, its value will not be
-- changed. \`GetSubscription\` calls will always return a valid version,
-- even if the subscription was created without this attribute. The
-- possible values for this attribute are: * \`v1beta1\`: uses the push
-- format defined in the v1beta1 Pub\/Sub API. * \`v1\` or \`v1beta2\`:
-- uses the push format defined in the v1 Pub\/Sub API.
--
-- /See:/ 'pushConfigAttributes' smart constructor.
newtype PushConfigAttributes = PushConfigAttributes'
    { _pcaAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PushConfigAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcaAddtional'
pushConfigAttributes
    :: HashMap Text Text -- ^ 'pcaAddtional'
    -> PushConfigAttributes
pushConfigAttributes pPcaAddtional_ = 
    PushConfigAttributes'
    { _pcaAddtional = _Coerce # pPcaAddtional_
    }

pcaAddtional :: Lens' PushConfigAttributes (HashMap Text Text)
pcaAddtional
  = lens _pcaAddtional (\ s a -> s{_pcaAddtional = a})
      . _Coerce

instance FromJSON PushConfigAttributes where
        parseJSON
          = withObject "PushConfigAttributes"
              (\ o ->
                 PushConfigAttributes' <$> (parseJSONObject o))

instance ToJSON PushConfigAttributes where
        toJSON = toJSON . _pcaAddtional

-- | A subscription resource.
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
    { _subPushConfig :: !(Maybe PushConfig)
    , _subMessageRetentionDuration :: !(Maybe Duration)
    , _subTopic :: !(Maybe Text)
    , _subName :: !(Maybe Text)
    , _subRetainAckedMessages :: !(Maybe Bool)
    , _subAckDeadlineSeconds :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'subPushConfig'
--
-- * 'subMessageRetentionDuration'
--
-- * 'subTopic'
--
-- * 'subName'
--
-- * 'subRetainAckedMessages'
--
-- * 'subAckDeadlineSeconds'
subscription
    :: Subscription
subscription = 
    Subscription'
    { _subPushConfig = Nothing
    , _subMessageRetentionDuration = Nothing
    , _subTopic = Nothing
    , _subName = Nothing
    , _subRetainAckedMessages = Nothing
    , _subAckDeadlineSeconds = Nothing
    }

-- | If push delivery is used with this subscription, this field is used to
-- configure it. An empty \`pushConfig\` signifies that the subscriber will
-- pull and ack messages using API methods.
subPushConfig :: Lens' Subscription (Maybe PushConfig)
subPushConfig
  = lens _subPushConfig
      (\ s a -> s{_subPushConfig = a})

-- | How long to retain unacknowledged messages in the subscription\'s
-- backlog, from the moment a message is published. If
-- \`retain_acked_messages\` is true, then this also configures the
-- retention of acknowledged messages, and thus configures how far back in
-- time a \`Seek\` can be done. Defaults to 7 days. Cannot be more than 7
-- days or less than 10 minutes.
subMessageRetentionDuration :: Lens' Subscription (Maybe Scientific)
subMessageRetentionDuration
  = lens _subMessageRetentionDuration
      (\ s a -> s{_subMessageRetentionDuration = a})
      . mapping _Duration

-- | The name of the topic from which this subscription is receiving
-- messages. Format is \`projects\/{project}\/topics\/{topic}\`. The value
-- of this field will be \`_deleted-topic_\` if the topic has been deleted.
subTopic :: Lens' Subscription (Maybe Text)
subTopic = lens _subTopic (\ s a -> s{_subTopic = a})

-- | The name of the subscription. It must have the format
-- \`\"projects\/{project}\/subscriptions\/{subscription}\"\`.
-- \`{subscription}\` must start with a letter, and contain only letters
-- (\`[A-Za-z]\`), numbers (\`[0-9]\`), dashes (\`-\`), underscores
-- (\`_\`), periods (\`.\`), tildes (\`~\`), plus (\`+\`) or percent signs
-- (\`%\`). It must be between 3 and 255 characters in length, and it must
-- not start with \`\"goog\"\`.
subName :: Lens' Subscription (Maybe Text)
subName = lens _subName (\ s a -> s{_subName = a})

-- | Indicates whether to retain acknowledged messages. If true, then
-- messages are not expunged from the subscription\'s backlog, even if they
-- are acknowledged, until they fall out of the
-- \`message_retention_duration\` window.
subRetainAckedMessages :: Lens' Subscription (Maybe Bool)
subRetainAckedMessages
  = lens _subRetainAckedMessages
      (\ s a -> s{_subRetainAckedMessages = a})

-- | This value is the maximum time after a subscriber receives a message
-- before the subscriber should acknowledge the message. After message
-- delivery but before the ack deadline expires and before the message is
-- acknowledged, it is an outstanding message and will not be delivered
-- again during that time (on a best-effort basis). For pull subscriptions,
-- this value is used as the initial value for the ack deadline. To
-- override this value for a given message, call \`ModifyAckDeadline\` with
-- the corresponding \`ack_id\` if using non-streaming pull or send the
-- \`ack_id\` in a \`StreamingModifyAckDeadlineRequest\` if using streaming
-- pull. The minimum custom deadline you can specify is 10 seconds. The
-- maximum custom deadline you can specify is 600 seconds (10 minutes). If
-- this parameter is 0, a default value of 10 seconds is used. For push
-- delivery, this value is also used to set the request timeout for the
-- call to the push endpoint. If the subscriber never acknowledges the
-- message, the Pub\/Sub system will eventually redeliver the message.
subAckDeadlineSeconds :: Lens' Subscription (Maybe Int32)
subAckDeadlineSeconds
  = lens _subAckDeadlineSeconds
      (\ s a -> s{_subAckDeadlineSeconds = a})
      . mapping _Coerce

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ o ->
                 Subscription' <$>
                   (o .:? "pushConfig") <*>
                     (o .:? "messageRetentionDuration")
                     <*> (o .:? "topic")
                     <*> (o .:? "name")
                     <*> (o .:? "retainAckedMessages")
                     <*> (o .:? "ackDeadlineSeconds"))

instance ToJSON Subscription where
        toJSON Subscription'{..}
          = object
              (catMaybes
                 [("pushConfig" .=) <$> _subPushConfig,
                  ("messageRetentionDuration" .=) <$>
                    _subMessageRetentionDuration,
                  ("topic" .=) <$> _subTopic, ("name" .=) <$> _subName,
                  ("retainAckedMessages" .=) <$>
                    _subRetainAckedMessages,
                  ("ackDeadlineSeconds" .=) <$>
                    _subAckDeadlineSeconds])

-- | Request for the UpdateSubscription method.
--
-- /See:/ 'updateSubscriptionRequest' smart constructor.
data UpdateSubscriptionRequest = UpdateSubscriptionRequest'
    { _uUpdateMask :: !(Maybe FieldMask)
    , _uSubscription :: !(Maybe Subscription)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateSubscriptionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uUpdateMask'
--
-- * 'uSubscription'
updateSubscriptionRequest
    :: UpdateSubscriptionRequest
updateSubscriptionRequest = 
    UpdateSubscriptionRequest'
    { _uUpdateMask = Nothing
    , _uSubscription = Nothing
    }

-- | Indicates which fields in the provided subscription to update. Must be
-- specified and non-empty.
uUpdateMask :: Lens' UpdateSubscriptionRequest (Maybe FieldMask)
uUpdateMask
  = lens _uUpdateMask (\ s a -> s{_uUpdateMask = a})

-- | The updated subscription object.
uSubscription :: Lens' UpdateSubscriptionRequest (Maybe Subscription)
uSubscription
  = lens _uSubscription
      (\ s a -> s{_uSubscription = a})

instance FromJSON UpdateSubscriptionRequest where
        parseJSON
          = withObject "UpdateSubscriptionRequest"
              (\ o ->
                 UpdateSubscriptionRequest' <$>
                   (o .:? "updateMask") <*> (o .:? "subscription"))

instance ToJSON UpdateSubscriptionRequest where
        toJSON UpdateSubscriptionRequest'{..}
          = object
              (catMaybes
                 [("updateMask" .=) <$> _uUpdateMask,
                  ("subscription" .=) <$> _uSubscription])

-- | Response for the \`ListSubscriptions\` method.
--
-- /See:/ 'listSubscriptionsResponse' smart constructor.
data ListSubscriptionsResponse = ListSubscriptionsResponse'
    { _lisNextPageToken :: !(Maybe Text)
    , _lisSubscriptions :: !(Maybe [Subscription])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisNextPageToken'
--
-- * 'lisSubscriptions'
listSubscriptionsResponse
    :: ListSubscriptionsResponse
listSubscriptionsResponse = 
    ListSubscriptionsResponse'
    { _lisNextPageToken = Nothing
    , _lisSubscriptions = Nothing
    }

-- | If not empty, indicates that there may be more subscriptions that match
-- the request; this value should be passed in a new
-- \`ListSubscriptionsRequest\` to get more subscriptions.
lisNextPageToken :: Lens' ListSubscriptionsResponse (Maybe Text)
lisNextPageToken
  = lens _lisNextPageToken
      (\ s a -> s{_lisNextPageToken = a})

-- | The subscriptions that match the request.
lisSubscriptions :: Lens' ListSubscriptionsResponse [Subscription]
lisSubscriptions
  = lens _lisSubscriptions
      (\ s a -> s{_lisSubscriptions = a})
      . _Default
      . _Coerce

instance FromJSON ListSubscriptionsResponse where
        parseJSON
          = withObject "ListSubscriptionsResponse"
              (\ o ->
                 ListSubscriptionsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "subscriptions" .!= mempty))

instance ToJSON ListSubscriptionsResponse where
        toJSON ListSubscriptionsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lisNextPageToken,
                  ("subscriptions" .=) <$> _lisSubscriptions])

-- | Associates \`members\` with a \`role\`.
--
-- /See:/ 'binding' smart constructor.
data Binding = Binding'
    { _bMembers :: !(Maybe [Text])
    , _bRole :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Binding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bMembers'
--
-- * 'bRole'
binding
    :: Binding
binding = 
    Binding'
    { _bMembers = Nothing
    , _bRole = Nothing
    }

-- | Specifies the identities requesting access for a Cloud Platform
-- resource. \`members\` can have the following values: * \`allUsers\`: A
-- special identifier that represents anyone who is on the internet; with
-- or without a Google account. * \`allAuthenticatedUsers\`: A special
-- identifier that represents anyone who is authenticated with a Google
-- account or a service account. * \`user:{emailid}\`: An email address
-- that represents a specific Google account. For example,
-- \`alice\'gmail.com\` or \`joe\'example.com\`. *
-- \`serviceAccount:{emailid}\`: An email address that represents a service
-- account. For example, \`my-other-app\'appspot.gserviceaccount.com\`. *
-- \`group:{emailid}\`: An email address that represents a Google group.
-- For example, \`admins\'example.com\`. * \`domain:{domain}\`: A Google
-- Apps domain name that represents all the users of that domain. For
-- example, \`google.com\` or \`example.com\`.
bMembers :: Lens' Binding [Text]
bMembers
  = lens _bMembers (\ s a -> s{_bMembers = a}) .
      _Default
      . _Coerce

-- | Role that is assigned to \`members\`. For example, \`roles\/viewer\`,
-- \`roles\/editor\`, or \`roles\/owner\`. Required
bRole :: Lens' Binding (Maybe Text)
bRole = lens _bRole (\ s a -> s{_bRole = a})

instance FromJSON Binding where
        parseJSON
          = withObject "Binding"
              (\ o ->
                 Binding' <$>
                   (o .:? "members" .!= mempty) <*> (o .:? "role"))

instance ToJSON Binding where
        toJSON Binding'{..}
          = object
              (catMaybes
                 [("members" .=) <$> _bMembers,
                  ("role" .=) <$> _bRole])

-- | Request for the Acknowledge method.
--
-- /See:/ 'acknowledgeRequest' smart constructor.
newtype AcknowledgeRequest = AcknowledgeRequest'
    { _arAckIds :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AcknowledgeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arAckIds'
acknowledgeRequest
    :: AcknowledgeRequest
acknowledgeRequest = 
    AcknowledgeRequest'
    { _arAckIds = Nothing
    }

-- | The acknowledgment ID for the messages being acknowledged that was
-- returned by the Pub\/Sub system in the \`Pull\` response. Must not be
-- empty.
arAckIds :: Lens' AcknowledgeRequest [Text]
arAckIds
  = lens _arAckIds (\ s a -> s{_arAckIds = a}) .
      _Default
      . _Coerce

instance FromJSON AcknowledgeRequest where
        parseJSON
          = withObject "AcknowledgeRequest"
              (\ o ->
                 AcknowledgeRequest' <$> (o .:? "ackIds" .!= mempty))

instance ToJSON AcknowledgeRequest where
        toJSON AcknowledgeRequest'{..}
          = object (catMaybes [("ackIds" .=) <$> _arAckIds])
