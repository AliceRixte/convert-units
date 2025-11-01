# Changelog for `convert-units`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0 - 2025-09-10

* First release :
  * Units
  * Dimensions
  * Quantities

## 0.1 - TODO

* Use integer singletons for exponentiating units:

``` haskell
  >>> Meter 2 .^. pos @10
  quantity @(Meter.^+10) 1024.0
```

As a consequence, `Data.Type.Int.Proxy` was deleted. `zero, pos1, pos2, pos3, pos4, neg1, neg2, neg3 , neg4` are now exported by `Data.Type.Int`.
