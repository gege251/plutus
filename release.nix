{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  # Passed in by Hydra depending on the configuration, contains the revision and the out path
, plutus ? null
, rootsOnly ? false
}:
let
  traceNames = prefix: builtins.mapAttrs (n: v:
    if builtins.isAttrs v
    then if v ? type && v.type == "derivation"
    then __trace ("found job " + prefix + n) v
    else __trace ("looking in " + prefix + n) traceNames (prefix + n + ".") v
    else v);
  inherit (import ./nix/lib/ci.nix) stripAttrsForHydra filterDerivations derivationAggregate;

  ci = import ./ci.nix { inherit supportedSystems rootsOnly; };
  # ci.nix is a set of attributes that work fine as jobs (albeit in a slightly different structure, the platform comes
  # first), but we mainly just need to get rid of some extra attributes.
  ciJobsets = stripAttrsForHydra (filterDerivations ci);
  # Don't filter anyting out of required for now
  requiredJobsets = ciJobsets;
in
traceNames "" (ciJobsets // { required = derivationAggregate "required-plutus" requiredJobsets; })
