/*
NIXPKGS version

Any archive of nixpkgs can be used.

The simplest update solution is to look at
http://github.com/NixOS/nixpkgs-channels and pick the latest commit for
nixpkgs-unstable. The archive can then be fetched at:

https://github.com/NixOS/nixpkgs-channels/archive/COMMIT_NUMBER.tar.gz;

and the control sum computed using `sha256`.
*/

let
  # nixpkgs-unstable 2020-01-30
  sha256 = "0p7amn7raw5rahyxw3jq21378n7i7ld4122hm5dddlbcvmpw60p0";
  rev = "690dd986b2349d2c9cd6437e820954ed400f37f7";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
