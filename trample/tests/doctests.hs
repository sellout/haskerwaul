{-# LANGUAGE Unsafe #-}

module Main (main) where

import "doctest" Test.DocTest (doctest)
import safe "haskerwaul-trample" Data.Function (($))
import safe "haskerwaul-trample" Data.Semigroup ((<>))
import safe "haskerwaul-trample" System.IO (IO)
import "this" Build_doctests (flags, module_sources, pkgs)

main :: IO ()
main = doctest $ flags <> pkgs <> module_sources
