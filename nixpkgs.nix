/*
NIXPKGS version

Any archive of nixpkgs can be used.

The simplest update solution is to look at http://github.com/NixOS/nixpkgs and
pick the latest commit for nixpkgs-unstable. The archive can then be fetched at:

https://github.com/NixOS/nixpkgs-channels/archive/COMMIT_NUMBER.tar.gz;

and the control sum computed using `sha256`.
*/

let
  # nixpkgs-unstable 2020-12-18
  sha256 = "12mjfar2ir561jxa1xvw6b1whbqs1rq59byc87icql399zal5z4a";
  rev = "00941cd747e9bc1c3326d1362dbc7e9cfe18cf53";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
