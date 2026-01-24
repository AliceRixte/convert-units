# Changelog for `convert-units`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1 - 2026-01-24

### Added

- #4 : Imperial and UsCustomary units for Area and Length  (thanks to Aster89)
- Use integer singletons for exponentiating units:

``` haskell
  >>> Meter 2 .^. pos @10
  quantity @(Meter.^+10) 1024.0
```

### Renamed

This is strongly against backward compatibility, but this is still very early stages of this library. Sorry !

- `NonStd` -> `NonSI` (more accurate in terms of physics)
- `Base` -> `Core` (avoid confusion with base units)

### Removed

- The file `Data.Type.Int.Proxy` was deleted. `zero, pos1, pos2, pos3, pos4, neg1, neg2, neg3 , neg4` are now exported by `Data.Type.Int`.


## 0 - 2025-09-10

### Added

- Units
- Dimensions
- Quantities
