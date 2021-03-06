-- | READ queries on the HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
--
-- TODO: We need to think about which layer will have the responsibility for
-- filtering and sorting. If we want the 'IxSet' stuff to be local to the
-- "Kernel.DB" namespace (which would be a good thing), then filtering and
-- sorting (and maybe even pagination) will need to happen here.
module Cardano.Wallet.Kernel.DB.HdWallet.Read (
    -- | * Infrastructure
    HdQuery
  , HdQueryErr
    -- | * Derived balance
  , hdRootBalance
  , hdAccountBalance
    -- | Accumulate all accounts/addresses
  , readAllHdRoots
  , readAllHdAccounts
  , readAllHdAddresses
    -- | All wallets/accounts/addresses
  , readAccountsByRootId
  , readAddressesByRootId
  , readAddressesByAccountId
    -- | Single wallets/accounts/addresses
  , readHdRoot
  , readHdAccount
  , readHdAccountCurrentCheckpoint
  , readHdAddress
  , readHdAddressByCardanoAddress
  ) where

import           Universum hiding (toList)

import           Control.Lens (at)
import           Data.Foldable (toList)

import           Pos.Core (Address, Coin, sumCoins)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet,
                     ixedIndexed)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-# ANN module ("HLint: ignore Unnecessary hiding" :: Text) #-}

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | Query on a HD wallet
type HdQuery a = HdWallets -> a

-- | Like 'HdQuery', but with the possibility of errors
type HdQueryErr e a = HdQuery (Either e a)

-- | Like '(>>=)' for queries
using :: HdQueryErr e a -> (a -> HdQueryErr e b) -> HdQueryErr e b
using f g wallets =
    case f wallets of
      Left  e -> Left e
      Right a -> g a wallets

-- | Variation on 'using' where the second query cannot throw errors
using' :: HdQueryErr e a -> (a -> HdQuery b) -> HdQueryErr e b
using' f g = using f ((Right .) . g)

-- | Variation on 'using'' where the result of the first query is ignored
--
-- Useful when the first query is merely a sanity check.
check :: HdQueryErr e a -> HdQuery b -> HdQueryErr e b
check f g = using' f (const g)

{-------------------------------------------------------------------------------
  Computed balances information
-------------------------------------------------------------------------------}

hdRootBalance :: HdRootId -> HdQuery Integer
hdRootBalance rootId = sumCoins
                     . map hdAccountBalance
                     . Data.Foldable.toList
                     . IxSet.getEQ rootId
                     . view hdWalletsAccounts

-- | Current balance of an account
hdAccountBalance :: HdAccount -> Coin
hdAccountBalance = view $ hdAccountState
                        . hdAccountStateCurrent
                        . pcheckpointUtxoBalance
                        . fromDb

{-------------------------------------------------------------------------------
  Accumulate across wallets/accounts
-------------------------------------------------------------------------------}

-- | Meta-information of /all wallets
readAllHdRoots :: HdQuery (IxSet HdRoot)
readAllHdRoots = view hdWalletsRoots

-- | Meta-information of /all/ accounts
readAllHdAccounts :: HdQuery (IxSet HdAccount)
readAllHdAccounts = view hdWalletsAccounts

-- | Meta-information and total balance of /all/ addresses
readAllHdAddresses :: HdQuery (IxSet (Indexed HdAddress))
readAllHdAddresses = view hdWalletsAddresses

{-------------------------------------------------------------------------------
  Information about all wallets/accounts/addresses
-------------------------------------------------------------------------------}

-- | All accounts in the given wallet
readAccountsByRootId :: HdRootId  -> HdQueryErr UnknownHdRoot (IxSet HdAccount)
readAccountsByRootId rootId =
      check (readHdRoot rootId)
    $ IxSet.getEQ rootId . readAllHdAccounts

-- | All addresses in the given wallet
readAddressesByRootId :: HdRootId
                      -> HdQueryErr UnknownHdRoot (IxSet (Indexed HdAddress))
readAddressesByRootId rootId =
      check (readHdRoot rootId)
    $ IxSet.getEQ rootId . readAllHdAddresses

-- | All addresses in the given account
readAddressesByAccountId :: HdAccountId
                         -> HdQueryErr UnknownHdAccount (IxSet (Indexed HdAddress))
readAddressesByAccountId accId =
      check (readHdAccount accId)
    $ IxSet.getEQ accId . readAllHdAddresses

{-------------------------------------------------------------------------------
  Information about a single wallet/address/account
-------------------------------------------------------------------------------}

-- | Look up the specified wallet
readHdRoot :: HdRootId -> HdQueryErr UnknownHdRoot HdRoot
readHdRoot rootId = aux . view (at rootId) . readAllHdRoots
  where
    aux :: Maybe a -> Either UnknownHdRoot a
    aux = maybe (Left (UnknownHdRoot rootId)) Right

-- | Look up the specified account
readHdAccount :: HdAccountId -> HdQueryErr UnknownHdAccount HdAccount
readHdAccount accId = do
    res <- view (at accId) . readAllHdAccounts
    case res of
         Just account -> return (Right account)
         Nothing -> do
             let rootId = accId ^. hdAccountIdParent
             -- Offer a better diagnostic on what went wrong.
             either (\_ -> Left (UnknownHdAccountRoot rootId))
                    (\_ -> Left (UnknownHdAccount accId))
                   <$> readHdRoot rootId

-- | Look up the specified account and return the current checkpoint
readHdAccountCurrentCheckpoint :: HdAccountId
                               -> HdQueryErr UnknownHdAccount PartialCheckpoint
readHdAccountCurrentCheckpoint accId db =
    view (hdAccountState . hdAccountStateCurrent) <$> readHdAccount accId db

-- | Look up the specified address
readHdAddress :: HdAddressId -> HdQueryErr UnknownHdAddress HdAddress
readHdAddress addrId = aux . view (at addrId) . readAllHdAddresses
  where
    aux :: Maybe (Indexed a) -> Either UnknownHdAddress a
    aux = maybe (Left (UnknownHdAddress addrId)) (Right . view ixedIndexed)

-- | Look up the specified address by its associated Cardano's 'Address'.
readHdAddressByCardanoAddress :: Address -> HdQueryErr UnknownHdAddress HdAddress
readHdAddressByCardanoAddress cardanoAddr = do
    aux . IxSet.getEQ cardanoAddr . readAllHdAddresses
  where
    aux :: IxSet (Indexed HdAddress) -> Either UnknownHdAddress HdAddress
    aux ixset = case IxSet.getOne ixset of
                     Just x  -> Right (view ixedIndexed x)
                     Nothing -> Left (UnknownHdCardanoAddress cardanoAddr)
