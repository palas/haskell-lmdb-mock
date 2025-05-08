{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | FFI bindings for LMDB.
--
-- This module is only defines the essential bindings for LMDB without
-- abstractions on top.
module Database.LMDB.FFI (
    -- * Version Macros
    mdbVersionMajor, mdbVersionMinor, mdbVersionPatch
    -- * Environment flags
  , mdbFixedmap, mdbNosubdir, mdbNosync, mdbRdonly, mdbNometasync
  , mdbWritemap, mdbMapasync, mdbNotls, mdbNolock, mdbNordahead, mdbNomeminit
    -- * Database flags
  , mdbReversekey, mdbDupsort, mdbIntegerkey, mdbDupfixed, mdbIntegerdup
  , mdbReversedup, mdbCreate
    -- * Write flags
  , mdbNooverwrite, mdbNodupdata, mdbCurrent, mdbReserve
  , mdbAppend, mdbAppenddup, mdbMultiple
    -- * Cursor get operations
  , MDB_cursor_op (..)
  , mdbFirst, mdbFirstDup, mdbGetBoth, mdbGetBothRange, mdbGetCurrent
  , mdbGetMultiple, mdbLast, mdbLastDup, mdbNext, mdbNextDup, mdbNextMultiple
  , mdbNextNodup, mdbPrev, mdbPrevDup, mdbPrevNodup, mdbSet, mdbSetKey
  , mdbSetRange
    -- * Return codes
  , mdbSuccess, mdbKeyexist, mdbNotfound, mdbPageNotfound, mdbCorrupted, mdbPanic
  , mdbVersionMismatch, mdbInvalid, mdbMapFull, mdbDbsFull, mdbReadersFull
  , mdbTlsFull, mdbTxnFull, mdbCursorFull, mdbPageFull, mdbMapResized
  , mdbIncompatible, mdbBadRslot, mdbBadTxn, mdbBadValsize, mdbBadDbi
  , mdbLastErrcode
    -- * Data structures
  , MDB_val (..)
  , MDB_stat (..)
  , MDB_envinfo (..)
    -- ** Opaque
  , MDB_env
  , MDB_txn
  , MDB_cursor
    -- ** Newtypes (typedefs)
  , MDB_mode_t (..)
  , MDB_dbi (..)
    -- * Functions
  , mdb_version
  , mdb_strerror
  , mdb_env_create
  , mdb_env_open
  , mdb_env_copy
  , mdb_env_stat
  , mdb_env_info
  , mdb_env_sync
  , mdb_env_close
  , mdb_env_set_flags
  , mdb_env_get_flags
  , mdb_env_get_path
  , mdb_env_set_mapsize
  , mdb_env_set_maxreaders
  , mdb_env_get_maxreaders
  , mdb_env_set_maxdbs
  , mdb_env_get_maxkeysize
  , mdb_txn_begin
  , mdb_txn_commit
  , mdb_txn_abort
  , mdb_dbi_open
  , mdb_stat
  , mdb_dbi_flags
  , mdb_dbi_close
  , mdb_drop
  , mdb_set_compare
  , mdb_set_dupsort
  , mdb_cmp
  , mdb_dcmp
  , mdb_cmp'
  , mdb_dcmp'
  , mdb_get
  , mdb_put
  , mdb_del
  , mdb_get'
  , mdb_put'
  , mdb_del'
  , mdb_cursor_open
  , mdb_cursor_close
  , mdb_cursor_get
  , mdb_cursor_put
  , mdb_cursor_del
  , mdb_cursor_count
  , mdb_cursor_open'
  , mdb_cursor_close'
  , mdb_cursor_get'
  , mdb_cursor_put'
  , mdb_cursor_del'
  , mdb_cursor_count'
  , mdb_txn_reset
  , mdb_txn_renew
  , mdb_reader_list
  , mdb_reader_check
  , MDB_cmp_func, wrapCmpFn
  , MDB_msg_func, wrapMsgFunc
  ) where

import Foreign
import Foreign.C


{-------------------------------------------------------------------------------
  Version Macros
--------------------------------------------------------------------------------}

mdbVersionMajor :: CLong
mdbVersionMajor = 0

mdbVersionMinor :: CLong
mdbVersionMinor = 9

mdbVersionPatch :: CLong
mdbVersionPatch = 70

{-------------------------------------------------------------------------------
  Environment flags
--------------------------------------------------------------------------------}

mdbFixedmap :: CUInt
mdbFixedmap = 0x01

mdbNosubdir :: CUInt
mdbNosubdir = 0x4000

mdbNosync :: CUInt
mdbNosync = 0x10000

mdbRdonly :: CUInt
mdbRdonly = 0x20000

mdbNometasync :: CUInt
mdbNometasync = 0x40000

mdbWritemap :: CUInt
mdbWritemap = 0x80000

mdbMapasync :: CUInt
mdbMapasync = 0x100000

mdbNotls :: CUInt
mdbNotls = 0x200000

mdbNolock :: CUInt
mdbNolock = 0x400000

mdbNordahead :: CUInt
mdbNordahead = 0x800000

mdbNomeminit :: CUInt
mdbNomeminit = 0x1000000

{-------------------------------------------------------------------------------
  Database flags
--------------------------------------------------------------------------------}

mdbReversekey :: CUInt
mdbReversekey = 0x02

mdbDupsort :: CUInt
mdbDupsort = 0x04

mdbIntegerkey :: CUInt
mdbIntegerkey = 0x08

mdbDupfixed :: CUInt
mdbDupfixed = 0x10

mdbIntegerdup :: CUInt
mdbIntegerdup = 0x20

mdbReversedup :: CUInt
mdbReversedup = 0x40

mdbCreate :: CUInt
mdbCreate = 0x40000

{-------------------------------------------------------------------------------
  Write flags
--------------------------------------------------------------------------------}

mdbNooverwrite :: CUInt
mdbNooverwrite = 0x10

mdbNodupdata :: CUInt
mdbNodupdata = 0x20

mdbCurrent :: CUInt
mdbCurrent = 0x40

mdbReserve :: CUInt
mdbReserve = 0x10000

mdbAppend :: CUInt
mdbAppend = 0x20000

mdbAppenddup :: CUInt
mdbAppenddup = 0x40000

mdbMultiple :: CUInt
mdbMultiple = 0x80000

{-------------------------------------------------------------------------------
  Cursor get operations
-------------------------------------------------------------------------------}

data MDB_cursor_op =
    MDB_FIRST
  | MDB_FIRST_DUP
  | MDB_GET_BOTH
  | MDB_GET_BOTH_RANGE
  | MDB_GET_CURRENT
  | MDB_GET_MULTIPLE
  | MDB_LAST
  | MDB_LAST_DUP
  | MDB_NEXT
  | MDB_NEXT_DUP
  | MDB_NEXT_MULTIPLE
  | MDB_NEXT_NODUP
  | MDB_PREV
  | MDB_PREV_DUP
  | MDB_PREV_NODUP
  | MDB_SET
  | MDB_SET_KEY
  | MDB_SET_RANGE

mdbFirst :: MDB_cursor_op
mdbFirst = MDB_FIRST

mdbFirstDup :: MDB_cursor_op
mdbFirstDup = MDB_FIRST_DUP

mdbGetBoth :: MDB_cursor_op
mdbGetBoth = MDB_GET_BOTH

mdbGetBothRange :: MDB_cursor_op
mdbGetBothRange = MDB_GET_BOTH_RANGE

mdbGetCurrent :: MDB_cursor_op
mdbGetCurrent = MDB_GET_CURRENT

mdbGetMultiple :: MDB_cursor_op
mdbGetMultiple = MDB_GET_MULTIPLE

mdbLast :: MDB_cursor_op
mdbLast = MDB_LAST

mdbLastDup :: MDB_cursor_op
mdbLastDup = MDB_LAST_DUP

mdbNext :: MDB_cursor_op
mdbNext = MDB_NEXT

mdbNextDup :: MDB_cursor_op
mdbNextDup = MDB_NEXT_DUP

mdbNextMultiple :: MDB_cursor_op
mdbNextMultiple = MDB_NEXT_MULTIPLE

mdbNextNodup :: MDB_cursor_op
mdbNextNodup = MDB_NEXT_NODUP

mdbPrev :: MDB_cursor_op
mdbPrev = MDB_PREV

mdbPrevDup :: MDB_cursor_op
mdbPrevDup = MDB_PREV_DUP

mdbPrevNodup :: MDB_cursor_op
mdbPrevNodup = MDB_PREV_NODUP

mdbSet :: MDB_cursor_op
mdbSet = MDB_SET

mdbSetKey :: MDB_cursor_op
mdbSetKey = MDB_SET_KEY

mdbSetRange :: MDB_cursor_op
mdbSetRange = MDB_SET_RANGE

instance Bounded MDB_cursor_op where
  minBound = MDB_FIRST
  maxBound = MDB_SET_RANGE

{-------------------------------------------------------------------------------
  Return codes
--------------------------------------------------------------------------------}

mdbSuccess :: CInt
mdbSuccess = 0

mdbKeyexist :: CInt
mdbKeyexist = (-30799)

mdbNotfound :: CInt
mdbNotfound = (-30798)

mdbPageNotfound :: CInt
mdbPageNotfound = (-30797)

mdbCorrupted :: CInt
mdbCorrupted = (-30796)

mdbPanic :: CInt
mdbPanic = (-30795)

mdbVersionMismatch :: CInt
mdbVersionMismatch = (-30794)

mdbInvalid :: CInt
mdbInvalid = (-30793)

mdbMapFull :: CInt
mdbMapFull = (-30792)

mdbDbsFull :: CInt
mdbDbsFull = (-30791)

mdbReadersFull :: CInt
mdbReadersFull = (-30790)

mdbTlsFull :: CInt
mdbTlsFull = (-30789)

mdbTxnFull :: CInt
mdbTxnFull = (-30788)

mdbCursorFull :: CInt
mdbCursorFull = (-30787)

mdbPageFull :: CInt
mdbPageFull = (-30786)

mdbMapResized :: CInt
mdbMapResized = (-30785)

mdbIncompatible :: CInt
mdbIncompatible = (-30784)

mdbBadRslot :: CInt
mdbBadRslot = (-30783)

mdbBadTxn :: CInt
mdbBadTxn = (-30782)

mdbBadValsize :: CInt
mdbBadValsize = (-30781)

mdbBadDbi :: CInt
mdbBadDbi = (-30780)

mdbLastErrcode :: CInt
mdbLastErrcode = (-30779)

{-------------------------------------------------------------------------------
  Data structures
-------------------------------------------------------------------------------}

data MDB_stat = MDB_stat {
      ms_psize :: {-# UNPACK #-} !CUInt
    , ms_depth :: {-# UNPACK #-} !CUInt
    , ms_branch_pages :: {-# UNPACK #-} !CSize
    , ms_leaf_pages :: {-# UNPACK #-} !CSize
    , ms_overflow_pages :: {-# UNPACK #-} !CSize
    , ms_entries :: {-# UNPACK #-} !CSize
    }
    deriving (Show, Eq)

instance Storable MDB_stat where
    sizeOf _ = 0
    alignment _ = 0
    peek _ =
        return $! MDB_stat
            { ms_psize = 0
            , ms_depth = 0
            , ms_branch_pages = 0
            , ms_leaf_pages = 0
            , ms_overflow_pages = 0
            , ms_entries = 0
            }
    poke _ _ = return ()

data  {-# CTYPE "struct MDB_envinfo" #-} MDB_envinfo = MDB_envinfo
    { me_mapaddr :: {-# UNPACK #-} !(Ptr ())
    , me_mapsize :: {-# UNPACK #-} !CSize
    , me_last_pgno :: {-# UNPACK #-} !CSize
    , me_last_txnid :: {-# UNPACK #-} !CSize
    , me_maxreaders :: {-# UNPACK #-} !CUInt
    , me_numreaders :: {-# UNPACK #-} !CUInt
    } deriving (Eq, Show)

instance Storable MDB_envinfo where
    sizeOf _ = 0
    alignment _ = 0
    peek _ =
        return $! MDB_envinfo
            { me_mapaddr = nullPtr
            , me_mapsize = 0
            , me_last_pgno = 0
            , me_last_txnid = 0
            , me_maxreaders = 0
            , me_numreaders = 0
            }
    poke _ _ = return ()

-- | A value stored in the database. Be cautious; committing the
-- transaction that obtained a value should also invalidate it;
-- avoid capturing MDB_val in a lazy value. A safe interface
-- similar to STRef could be provided by another module.
data MDB_val = MDB_val {
      mv_size :: {-# UNPACK #-} !CSize
    , mv_data :: {-# UNPACK #-} !(Ptr ())
    }

instance Storable MDB_val where
  sizeOf _ = 0
  alignment _ = 0
  peek _ = pure $! MDB_val { mv_size = 0, mv_data = nullPtr}
  poke _ _ = return ()

-- opaque types

data MDB_env

data MDB_txn

data MDB_cursor

-- typedefs

newtype MDB_mode_t = MDB_mode_t CInt
  deriving (Show, Eq, Num, Storable)

newtype MDB_dbi = MDB_dbi CUInt
  deriving (Show, Eq, Num, Storable)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- FFI
--  'safe': higher overhead, thread juggling, allows callbacks into Haskell
--  'unsafe': lower overhead, reduced concurrency, no callbacks into Haskell
mdb_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CString
mdb_version _ _ _ = error "mdb_version: not implemented"

mdb_strerror :: CInt -> CString
mdb_strerror _ = error "mdb_strerror: not implemented"

mdb_env_create :: Ptr (Ptr MDB_env) -> IO CInt
mdb_env_create _ = error "mdb_env_create: not implemented"

mdb_env_open :: Ptr MDB_env -> CString -> CUInt -> MDB_mode_t -> IO CInt
mdb_env_open _ _ _ _ = error "mdb_env_open: not implemented"

mdb_env_copy :: Ptr MDB_env -> CString -> IO CInt
mdb_env_copy _ _ = error "mdb_env_copy: not implemented"

mdb_env_stat :: Ptr MDB_env -> Ptr MDB_stat -> IO CInt
mdb_env_stat _ _ = error "mdb_env_stat: not implemented"

mdb_env_info :: Ptr MDB_env -> Ptr MDB_envinfo -> IO CInt
mdb_env_info _ _ = error "mdb_env_info: not implemented"

mdb_env_sync :: Ptr MDB_env -> CInt -> IO CInt
mdb_env_sync _ _ = error "mdb_env_sync: not implemented"

mdb_env_close :: Ptr MDB_env -> IO ()
mdb_env_close _ = error "mdb_env_close: not implemented"

mdb_env_set_flags :: Ptr MDB_env -> CUInt -> CInt -> IO CInt
mdb_env_set_flags _ _ _ = error "mdb_env_set_flags: not implemented"

mdb_env_get_flags :: Ptr MDB_env -> Ptr CUInt -> IO CInt
mdb_env_get_flags _ _ = error "mdb_env_get_flags: not implemented"

mdb_env_get_path :: Ptr MDB_env -> Ptr (Ptr CChar) -> IO CInt
mdb_env_get_path _ _ = error "mdb_env_get_path: not implemented"

mdb_env_set_mapsize :: Ptr MDB_env -> CSize -> IO CInt
mdb_env_set_mapsize _ _ = error "mdb_env_set_mapsize: not implemented"

mdb_env_set_maxreaders :: Ptr MDB_env -> CUInt -> IO CInt
mdb_env_set_maxreaders _ _ = error "mdb_env_set_maxreaders: not implemented"

mdb_env_get_maxreaders :: Ptr MDB_env -> Ptr CUInt -> IO CInt
mdb_env_get_maxreaders _ _ = error "mdb_env_get_maxreaders: not implemented"

mdb_env_set_maxdbs :: Ptr MDB_env -> MDB_dbi -> IO CInt
mdb_env_set_maxdbs _ _ = error "mdb_env_set_maxdbs: not implemented"

mdb_env_get_maxkeysize :: Ptr MDB_env -> IO CInt
mdb_env_get_maxkeysize _ = error "mdb_env_get_maxkeysize: not implemented"


mdb_txn_begin :: Ptr MDB_env -> Ptr MDB_txn -> CUInt -> Ptr (Ptr MDB_txn) -> IO CInt
mdb_txn_begin _ _ _ _ = error "mdb_txn_begin: not implemented"

mdb_txn_commit :: Ptr MDB_txn -> IO CInt
mdb_txn_commit _ = error "mdb_txn_commit: not implemented"

mdb_txn_abort :: Ptr MDB_txn -> IO ()
mdb_txn_abort _ = error "mdb_txn_abort: not implemented"


mdb_dbi_open :: Ptr MDB_txn -> CString -> CUInt -> Ptr MDB_dbi -> IO CInt
mdb_dbi_open _ _ _ _ = error "mdb_dbi_open: not implemented"

mdb_stat :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_stat -> IO CInt
mdb_stat _ _ _ = error "mdb_stat: not implemented"

mdb_dbi_flags :: Ptr MDB_txn -> MDB_dbi -> Ptr CUInt -> IO CInt
mdb_dbi_flags _ _ _ = error "mdb_dbi_flags: not implemented"

mdb_dbi_close :: Ptr MDB_env -> MDB_dbi -> IO ()
mdb_dbi_close _ _ = error "mdb_dbi_close: not implemented"

mdb_drop :: Ptr MDB_txn -> MDB_dbi -> CInt -> IO CInt
mdb_drop _ _ _ = error "mdb_drop: not implemented"

-- comparisons may only be configured for a 'safe' MDB_dbi.
mdb_set_compare :: Ptr MDB_txn -> MDB_dbi -> FunPtr MDB_cmp_func -> IO CInt
mdb_set_compare _ _ _ = error "mdb_set_compare: not implemented"

mdb_set_dupsort :: Ptr MDB_txn -> MDB_dbi -> FunPtr MDB_cmp_func -> IO CInt
mdb_set_dupsort _ _ _ = error "mdb_set_dupsort: not implemented"

mdb_cmp :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_cmp _ _ _ _ = error "mdb_cmp: not implemented"

mdb_dcmp :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_dcmp _ _ _ _ = error "mdb_dcmp: not implemented"


mdb_cmp' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_cmp' _ _ _ _ = error "mdb_cmp': not implemented"

mdb_dcmp' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_dcmp' _ _ _ _ = error "mdb_dcmp': not implemented"

mdb_get :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_get _ _ _ _ = error "mdb_get: not implemented"

mdb_put :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
mdb_put _ _ _ _ _ = error "mdb_put: not implemented"

mdb_del :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_del _ _ _ _ = error "mdb_del: not implemented"

mdb_get' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_get' _ _ _ _ = error "mdb_get': not implemented"

mdb_put' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
mdb_put' _ _ _ _ _ = error "mdb_put': not implemented"

mdb_del' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
mdb_del' _ _ _ _ = error "mdb_del': not implemented"

-- I dislike LMDB's cursor interface: one 'get' function with 18 special cases.
-- Seems like it should be 18 functions.
mdb_cursor_open :: Ptr MDB_txn -> MDB_dbi -> Ptr (Ptr MDB_cursor) -> IO CInt
mdb_cursor_open _ _ _ = error "mdb_cursor_open: not implemented"

mdb_cursor_close :: Ptr MDB_cursor -> IO ()
mdb_cursor_close _ = error "mdb_cursor_close: not implemented"

mdb_cursor_get :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> MDB_cursor_op -> IO CInt
mdb_cursor_get _ _ _ _ = error "mdb_cursor_get: not implemented"

mdb_cursor_put :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
mdb_cursor_put _ _ _ _ = error "mdb_cursor_put: not implemented"

mdb_cursor_del :: Ptr MDB_cursor -> CUInt -> IO CInt
mdb_cursor_del _ _ = error "mdb_cursor_del: not implemented"

mdb_cursor_count :: Ptr MDB_cursor -> Ptr CSize -> IO CInt
mdb_cursor_count _ _ = error "mdb_cursor_count: not implemented"


mdb_cursor_open' :: Ptr MDB_txn -> MDB_dbi -> Ptr (Ptr MDB_cursor) -> IO CInt
mdb_cursor_open' _ _ _ = error "mdb_cursor_open': not implemented"

mdb_cursor_close' :: Ptr MDB_cursor -> IO ()
mdb_cursor_close' _ = error "mdb_cursor_close': not implemented"

mdb_cursor_get' :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> MDB_cursor_op -> IO CInt
mdb_cursor_get' _ _ _ _ = error "mdb_cursor_get': not implemented"

mdb_cursor_put' :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
mdb_cursor_put' _ _ _ _ = error "mdb_cursor_put': not implemented"

mdb_cursor_del' :: Ptr MDB_cursor -> CUInt -> IO CInt
mdb_cursor_del' _ _ = error "mdb_cursor_del': not implemented"

mdb_cursor_count' :: Ptr MDB_cursor -> Ptr CSize -> IO CInt
mdb_cursor_count' _ _ = error "mdb_cursor_count': not implemented"


mdb_txn_reset :: Ptr MDB_txn -> IO ()
mdb_txn_reset _ = error "mdb_txn_reset: not implemented"

mdb_txn_renew :: Ptr MDB_txn -> IO CInt
mdb_txn_renew _ = error "mdb_txn_renew: not implemented"


mdb_reader_list :: Ptr MDB_env -> FunPtr MDB_msg_func -> Ptr () -> IO CInt
mdb_reader_list _ _ _ = error "mdb_reader_list: not implemented"

mdb_reader_check :: Ptr MDB_env -> Ptr CInt -> IO CInt
mdb_reader_check _ _ = error "mdb_reader_check: not implemented"

-- | User-defined comparison functions for keys.
type MDB_cmp_func = Ptr MDB_val -> Ptr MDB_val -> IO CInt
wrapCmpFn :: MDB_cmp_func -> IO (FunPtr MDB_cmp_func)
wrapCmpFn _ = error "wrapCmpFn: not implemented"

-- callback function for reader list (used internally to this binding)
type MDB_msg_func = CString -> Ptr () -> IO CInt
wrapMsgFunc :: MDB_msg_func -> IO (FunPtr MDB_msg_func)
wrapMsgFunc _ = error "wrapMsgFunc: not implemented"
