{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TorrentContext where

import Data.Either (fromRight)
import InlineTypes
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import Language.C.Types.Parse (cIdentifierFromString)
import TorrentTypes

data Sha1Hash

data StdMutex

torrentContext :: C.Context
torrentContext =
  C.cppCtx
    <> C.cppTypePairs
      [ (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::string", [t|StdString|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::vector", [t|StdVector|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::shared_ptr", [t|StdSharedPtr|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "boost::shared_array", [t|BoostSharedArray|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::mutex", [t|StdMutex|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::session", [t|CTorrentSession|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::add_torrent_params", [t|CAddTorrentParams|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::torrent_info", [t|CTorrentInfo|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::torrent_handle", [t|CTorrentHandle|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::alert", [t|LibTorrentAlert|]),
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::info_hash_t", [t|InfoHash|]),
        -- ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::sha1_hash", [t| Sha1Hash |])
        (fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString False "torrent_files_info", [t|TorrentInfo|])
      ]
    <> C.bsCtx
