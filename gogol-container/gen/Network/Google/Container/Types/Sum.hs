{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Container.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Container.Types.Sum where

import Network.Google.Prelude

-- | The operation type.
data OperationOperationType
    = TypeUnspecified
      -- ^ @TYPE_UNSPECIFIED@
      -- Not set.
    | CreateCluster
      -- ^ @CREATE_CLUSTER@
      -- Cluster create.
    | DeleteCluster
      -- ^ @DELETE_CLUSTER@
      -- Cluster delete.
    | UpgradeMaster
      -- ^ @UPGRADE_MASTER@
      -- A master upgrade.
    | UpgradeNodes
      -- ^ @UPGRADE_NODES@
      -- A node upgrade.
    | RepairCluster
      -- ^ @REPAIR_CLUSTER@
      -- Cluster repair.
    | UpdateCluster
      -- ^ @UPDATE_CLUSTER@
      -- Cluster update.
    | CreateNodePool
      -- ^ @CREATE_NODE_POOL@
      -- Node pool create.
    | DeleteNodePool
      -- ^ @DELETE_NODE_POOL@
      -- Node pool delete.
    | SetNodePoolManagement
      -- ^ @SET_NODE_POOL_MANAGEMENT@
      -- Set node pool management.
    | AutoRepairNodes
      -- ^ @AUTO_REPAIR_NODES@
      -- Automatic node pool repair.
    | AutoUpgradeNodes
      -- ^ @AUTO_UPGRADE_NODES@
      -- Automatic node upgrade.
    | SetLabels
      -- ^ @SET_LABELS@
      -- Set labels.
    | SetMasterAuth
      -- ^ @SET_MASTER_AUTH@
      -- Set\/generate master auth materials
    | SetNodePoolSize
      -- ^ @SET_NODE_POOL_SIZE@
      -- Set node pool size.
    | SetNetworkPolicy
      -- ^ @SET_NETWORK_POLICY@
      -- Updates network policy for a cluster.
    | SetMaintenancePolicy
      -- ^ @SET_MAINTENANCE_POLICY@
      -- Set the maintenance policy.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable OperationOperationType

instance FromHttpApiData OperationOperationType where
    parseQueryParam = \case
        "TYPE_UNSPECIFIED" -> Right TypeUnspecified
        "CREATE_CLUSTER" -> Right CreateCluster
        "DELETE_CLUSTER" -> Right DeleteCluster
        "UPGRADE_MASTER" -> Right UpgradeMaster
        "UPGRADE_NODES" -> Right UpgradeNodes
        "REPAIR_CLUSTER" -> Right RepairCluster
        "UPDATE_CLUSTER" -> Right UpdateCluster
        "CREATE_NODE_POOL" -> Right CreateNodePool
        "DELETE_NODE_POOL" -> Right DeleteNodePool
        "SET_NODE_POOL_MANAGEMENT" -> Right SetNodePoolManagement
        "AUTO_REPAIR_NODES" -> Right AutoRepairNodes
        "AUTO_UPGRADE_NODES" -> Right AutoUpgradeNodes
        "SET_LABELS" -> Right SetLabels
        "SET_MASTER_AUTH" -> Right SetMasterAuth
        "SET_NODE_POOL_SIZE" -> Right SetNodePoolSize
        "SET_NETWORK_POLICY" -> Right SetNetworkPolicy
        "SET_MAINTENANCE_POLICY" -> Right SetMaintenancePolicy
        x -> Left ("Unable to parse OperationOperationType from: " <> x)

instance ToHttpApiData OperationOperationType where
    toQueryParam = \case
        TypeUnspecified -> "TYPE_UNSPECIFIED"
        CreateCluster -> "CREATE_CLUSTER"
        DeleteCluster -> "DELETE_CLUSTER"
        UpgradeMaster -> "UPGRADE_MASTER"
        UpgradeNodes -> "UPGRADE_NODES"
        RepairCluster -> "REPAIR_CLUSTER"
        UpdateCluster -> "UPDATE_CLUSTER"
        CreateNodePool -> "CREATE_NODE_POOL"
        DeleteNodePool -> "DELETE_NODE_POOL"
        SetNodePoolManagement -> "SET_NODE_POOL_MANAGEMENT"
        AutoRepairNodes -> "AUTO_REPAIR_NODES"
        AutoUpgradeNodes -> "AUTO_UPGRADE_NODES"
        SetLabels -> "SET_LABELS"
        SetMasterAuth -> "SET_MASTER_AUTH"
        SetNodePoolSize -> "SET_NODE_POOL_SIZE"
        SetNetworkPolicy -> "SET_NETWORK_POLICY"
        SetMaintenancePolicy -> "SET_MAINTENANCE_POLICY"

instance FromJSON OperationOperationType where
    parseJSON = parseJSONText "OperationOperationType"

instance ToJSON OperationOperationType where
    toJSON = toJSONText

-- | The current status of the operation.
data OperationStatus
    = StatusUnspecified
      -- ^ @STATUS_UNSPECIFIED@
      -- Not set.
    | Pending
      -- ^ @PENDING@
      -- The operation has been created.
    | Running
      -- ^ @RUNNING@
      -- The operation is currently running.
    | Done
      -- ^ @DONE@
      -- The operation is done, either cancelled or completed.
    | Aborting
      -- ^ @ABORTING@
      -- The operation is aborting.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable OperationStatus

instance FromHttpApiData OperationStatus where
    parseQueryParam = \case
        "STATUS_UNSPECIFIED" -> Right StatusUnspecified
        "PENDING" -> Right Pending
        "RUNNING" -> Right Running
        "DONE" -> Right Done
        "ABORTING" -> Right Aborting
        x -> Left ("Unable to parse OperationStatus from: " <> x)

instance ToHttpApiData OperationStatus where
    toQueryParam = \case
        StatusUnspecified -> "STATUS_UNSPECIFIED"
        Pending -> "PENDING"
        Running -> "RUNNING"
        Done -> "DONE"
        Aborting -> "ABORTING"

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

instance ToJSON OperationStatus where
    toJSON = toJSONText

-- | The selected network policy provider.
data NetworkPolicyProvider
    = ProviderUnspecified
      -- ^ @PROVIDER_UNSPECIFIED@
      -- Not set
    | Calico
      -- ^ @CALICO@
      -- Tigera (Calico Felix).
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable NetworkPolicyProvider

instance FromHttpApiData NetworkPolicyProvider where
    parseQueryParam = \case
        "PROVIDER_UNSPECIFIED" -> Right ProviderUnspecified
        "CALICO" -> Right Calico
        x -> Left ("Unable to parse NetworkPolicyProvider from: " <> x)

instance ToHttpApiData NetworkPolicyProvider where
    toQueryParam = \case
        ProviderUnspecified -> "PROVIDER_UNSPECIFIED"
        Calico -> "CALICO"

instance FromJSON NetworkPolicyProvider where
    parseJSON = parseJSONText "NetworkPolicyProvider"

instance ToJSON NetworkPolicyProvider where
    toJSON = toJSONText

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | [Output only] The status of the nodes in this pool instance.
data NodePoolStatus
    = NPSStatusUnspecified
      -- ^ @STATUS_UNSPECIFIED@
      -- Not set.
    | NPSProvisioning
      -- ^ @PROVISIONING@
      -- The PROVISIONING state indicates the node pool is being created.
    | NPSRunning
      -- ^ @RUNNING@
      -- The RUNNING state indicates the node pool has been created and is fully
      -- usable.
    | NPSRunningWithError
      -- ^ @RUNNING_WITH_ERROR@
      -- The RUNNING_WITH_ERROR state indicates the node pool has been created
      -- and is partially usable. Some error state has occurred and some
      -- functionality may be impaired. Customer may need to reissue a request or
      -- trigger a new update.
    | NPSReconciling
      -- ^ @RECONCILING@
      -- The RECONCILING state indicates that some work is actively being done on
      -- the node pool, such as upgrading node software. Details can be found in
      -- the \`statusMessage\` field.
    | NPSStopping
      -- ^ @STOPPING@
      -- The STOPPING state indicates the node pool is being deleted.
    | NPSError'
      -- ^ @ERROR@
      -- The ERROR state indicates the node pool may be unusable. Details can be
      -- found in the \`statusMessage\` field.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable NodePoolStatus

instance FromHttpApiData NodePoolStatus where
    parseQueryParam = \case
        "STATUS_UNSPECIFIED" -> Right NPSStatusUnspecified
        "PROVISIONING" -> Right NPSProvisioning
        "RUNNING" -> Right NPSRunning
        "RUNNING_WITH_ERROR" -> Right NPSRunningWithError
        "RECONCILING" -> Right NPSReconciling
        "STOPPING" -> Right NPSStopping
        "ERROR" -> Right NPSError'
        x -> Left ("Unable to parse NodePoolStatus from: " <> x)

instance ToHttpApiData NodePoolStatus where
    toQueryParam = \case
        NPSStatusUnspecified -> "STATUS_UNSPECIFIED"
        NPSProvisioning -> "PROVISIONING"
        NPSRunning -> "RUNNING"
        NPSRunningWithError -> "RUNNING_WITH_ERROR"
        NPSReconciling -> "RECONCILING"
        NPSStopping -> "STOPPING"
        NPSError' -> "ERROR"

instance FromJSON NodePoolStatus where
    parseJSON = parseJSONText "NodePoolStatus"

instance ToJSON NodePoolStatus where
    toJSON = toJSONText

-- | [Output only] The current status of this cluster.
data ClusterStatus
    = CSStatusUnspecified
      -- ^ @STATUS_UNSPECIFIED@
      -- Not set.
    | CSProvisioning
      -- ^ @PROVISIONING@
      -- The PROVISIONING state indicates the cluster is being created.
    | CSRunning
      -- ^ @RUNNING@
      -- The RUNNING state indicates the cluster has been created and is fully
      -- usable.
    | CSReconciling
      -- ^ @RECONCILING@
      -- The RECONCILING state indicates that some work is actively being done on
      -- the cluster, such as upgrading the master or node software. Details can
      -- be found in the \`statusMessage\` field.
    | CSStopping
      -- ^ @STOPPING@
      -- The STOPPING state indicates the cluster is being deleted.
    | CSError'
      -- ^ @ERROR@
      -- The ERROR state indicates the cluster may be unusable. Details can be
      -- found in the \`statusMessage\` field.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable ClusterStatus

instance FromHttpApiData ClusterStatus where
    parseQueryParam = \case
        "STATUS_UNSPECIFIED" -> Right CSStatusUnspecified
        "PROVISIONING" -> Right CSProvisioning
        "RUNNING" -> Right CSRunning
        "RECONCILING" -> Right CSReconciling
        "STOPPING" -> Right CSStopping
        "ERROR" -> Right CSError'
        x -> Left ("Unable to parse ClusterStatus from: " <> x)

instance ToHttpApiData ClusterStatus where
    toQueryParam = \case
        CSStatusUnspecified -> "STATUS_UNSPECIFIED"
        CSProvisioning -> "PROVISIONING"
        CSRunning -> "RUNNING"
        CSReconciling -> "RECONCILING"
        CSStopping -> "STOPPING"
        CSError' -> "ERROR"

instance FromJSON ClusterStatus where
    parseJSON = parseJSONText "ClusterStatus"

instance ToJSON ClusterStatus where
    toJSON = toJSONText

-- | The exact form of action to be taken on the master auth.
data SetMasterAuthRequestAction
    = Unknown
      -- ^ @UNKNOWN@
      -- Operation is unknown and will error out.
    | SetPassword
      -- ^ @SET_PASSWORD@
      -- Set the password to a user generated value.
    | GeneratePassword
      -- ^ @GENERATE_PASSWORD@
      -- Generate a new password and set it to that.
    | SetUsername
      -- ^ @SET_USERNAME@
      -- Set the username. If an empty username is provided, basic authentication
      -- is disabled for the cluster. If a non-empty username is provided, basic
      -- authentication is enabled, with either a provided password or a
      -- generated one.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable SetMasterAuthRequestAction

instance FromHttpApiData SetMasterAuthRequestAction where
    parseQueryParam = \case
        "UNKNOWN" -> Right Unknown
        "SET_PASSWORD" -> Right SetPassword
        "GENERATE_PASSWORD" -> Right GeneratePassword
        "SET_USERNAME" -> Right SetUsername
        x -> Left ("Unable to parse SetMasterAuthRequestAction from: " <> x)

instance ToHttpApiData SetMasterAuthRequestAction where
    toQueryParam = \case
        Unknown -> "UNKNOWN"
        SetPassword -> "SET_PASSWORD"
        GeneratePassword -> "GENERATE_PASSWORD"
        SetUsername -> "SET_USERNAME"

instance FromJSON SetMasterAuthRequestAction where
    parseJSON = parseJSONText "SetMasterAuthRequestAction"

instance ToJSON SetMasterAuthRequestAction where
    toJSON = toJSONText
