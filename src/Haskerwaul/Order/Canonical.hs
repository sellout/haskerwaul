module Haskerwaul.Order.Canonical where

newtype Canonical a = Canonical { decanonicalize :: a }
