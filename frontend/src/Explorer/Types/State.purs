module Explorer.Types.State where

import Control.Monad.Eff.Exception (Error)
import Control.SocketIO.Client (Socket)
import Data.DateTime (DateTime)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Explorer.Api.Types (SocketSubscription, SocketSubscriptionData)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Util.Config (SyncAction)
import Network.RemoteData (RemoteData)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockEntry, CBlockSummary, CTxBrief, CTxEntry, CTxSummary)
import Prelude (class Eq, class Ord, class Show)

-- Add all State types here to generate lenses from it

type State =
    { lang :: Language
    , route :: Route
    , socket :: SocketState
    , syncAction :: SyncAction
    , viewStates :: ViewStates
    , latestBlocks :: RemoteData Error CBlockEntries
    , totalBlocks :: RemoteData Error Int
    , currentBlockSummary :: Maybe CBlockSummary
    , currentBlockTxs :: Maybe CTxBriefs
    , currentTxSummary :: RemoteData Error CTxSummary
    , latestTransactions :: RemoteData Error CTxEntries
    , currentCAddress :: CAddress
    , currentAddressSummary :: RemoteData Error CAddressSummary
    , currentBlocksResult :: RemoteData Error CBlockEntries
    , errors :: Errors
    , loading :: Boolean
    , now :: DateTime
    }

data Search
    = SearchAddress
    | SearchTx
    | SearchTime

derive instance gSearch :: Generic Search
instance showSearch :: Show Search where
    show = gShow
derive instance eqSearch :: Eq Search

type SearchEpochSlotQuery = Tuple (Maybe Int) (Maybe Int)

type SocketState =
    { connected :: Boolean
    , connection :: Maybe Socket
    , subscriptions :: SocketSubscriptionItems
    }

type SocketSubscriptionItems = Array SocketSubscriptionItem

newtype SocketSubscriptionItem = SocketSubscriptionItem
    { socketSub :: SocketSubscription
    , socketSubData :: SocketSubscriptionData
    }

derive instance gSocketSubscriptionItem :: Generic SocketSubscriptionItem
derive instance ntSocketSubscriptionItem :: Newtype SocketSubscriptionItem _
derive instance eqSocketSubscriptionItem :: Eq SocketSubscriptionItem

data DashboardAPICode = Curl | Node | JQuery
derive instance eqDashboardAPICode :: Eq DashboardAPICode
derive instance ordDashboardAPICode :: Ord DashboardAPICode

type CBlockEntries = Array CBlockEntry

type CTxEntries = Array CTxEntry
type CTxBriefs = Array CTxBrief

type Errors = Array String

type ViewStates =
    { globalViewState :: GlobalViewState
    , dashboard :: DashboardViewState
    , addressDetail :: AddressDetailViewState
    , blockDetail :: BlockDetailViewState
    , blocksViewState :: BlocksViewState
    }

type GlobalViewState =
    { gViewMobileMenuOpenend :: Boolean
    , gViewTitle :: String
    , gViewSearchInputFocused :: Boolean
    , gViewSelectedSearch :: Search
    , gViewSearchQuery :: String
    , gViewSearchTimeQuery :: SearchEpochSlotQuery
    }

type DashboardViewState =
    { dbViewBlocksExpanded :: Boolean
    , dbViewBlockPagination :: Int
    , dbViewNextBlockPagination :: Int
    , dbViewLoadingBlockPagination :: Boolean
    , dbViewLoadingTotalBlocks :: Boolean
    , dbViewBlockPaginationEditable :: Boolean
    , dbViewTxsExpanded :: Boolean
    , dbViewSelectedApiCode :: DashboardAPICode
    }

type BlockDetailViewState =
    { blockTxPagination :: Int
    , blockTxPaginationEditable :: Boolean
    }

type AddressDetailViewState =
    { addressTxPagination :: Int
    , addressTxPaginationEditable :: Boolean
    }

type BlocksViewState =
    { blsViewPagination :: Int
    , blsViewPaginationEditable :: Boolean
    }

-- TODO (jk) CCurrency should be generated by purescript-bridge later
data CCurrency
    = ADA
    | BTC
    | USD
