{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  overrides =
    final: prev:
    rec {
    } // extraOverrides final prev;
in
{
  haskell =
    super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" =
          super.haskell.packages."${compiler}".override { inherit overrides; };
      };
    };
}
