module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.List (head, filter, nubByEq, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

-- Exercise 2
findEntryByStreet :: String -> AddressBook -> Maybe Entry
--findEntryByStreet street = head <<< filter filterEntryByStreet
-- Exercise 3
findEntryByStreet street = head <<< filter (_.address.street >>> eq street)
--  where
--  filterEntryByStreet :: Entry -> Boolean
--  filterEntryByStreet entry = entry.address.street == street

-- Exercise 4
isInBook :: String -> String -> AddressBook -> Boolean
--isInBook firstName lastName book = null (filter filterEntry book)
isInBook firstName lastName = not null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Exercise 5
removeDuplicates :: AddressBook -> AddressBook
--removeDuplicates = nubByEq entriesAreEqual
--  where
--  entriesAreEqual :: Entry -> Entry -> Boolean
--  entriesAreEqual a b = a.firstName == b.firstName && a.lastName == b.lastName
--removeDuplicates book = nubByEq (\a b -> a.firstName == b.firstName && a.lastName == b.lastName) book
removeDuplicates = nubByEq \a b -> a.firstName == b.firstName && a.lastName == b.lastName
