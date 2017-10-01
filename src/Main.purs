module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)
import Data.Show (show)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry street city book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street && entry.address.city == city

printEntry street city book = map showEntry (findEntry street city book)

doesEntryExist :: String -> String -> AddressBook -> Boolean
doesEntryExist firstName lastName book = null $ filter filterAddressBook book
  where
    filterAddressBook :: Entry -> Boolean
    filterAddressBook entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName book = nubBy (\x y -> x.firstName == firstName || x.lastName == lastName ) book
