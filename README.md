# convert-units

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/AliceRixte/convert-units/LICENSE) [![Hackage](https://img.shields.io/hackage/v/convert-units.svg)](https://hackage.haskell.org/package/convert-units) [![Nightly](https://www.stackage.org/package/convert-units/badge/nightly)](https://www.stackage.org/nightly/package/convert-units) [![LTS](https://www.stackage.org/package/convert-units/badge/lts)](https://www.stackage.org/lts/package/convert-units)

A Haskell library to convert between units, that will statically check the dimensions of the units being converted.

Conversions are usually as fast as manual multiplication by a conversion factor, thanks to heavy use of inlining.

## Usage

You will need the `TypeApplications` extension:

``` haskell
>>> :set -XTypeApplications
```

### Convert between units

You can use `to` or  `fromTo` for conversions:
``` haskell
>>> t = Hour 8
>>> to @Minute t
Minute 480.0

>>> fromTo @Hour @Minute 8
Minute 480.0
```

User-friendly static errors when  trying to convert between incompatible dimensions:

```haskell
>>> fromTo @Minute @Meter 1
• Cannot convert unit ‘min’ to unit ‘m’ because their dimensions do not match.
      Dimension of ‘min’ is: T
      Dimension of ‘m’ is: L
```

There are two sorts of unit conversions:

1. The regular ones
``` haskell
>>> fromTo @Celsius @Kelvin 0
Kelvin 273.15
```

2. Conversion that only takes the conversion factor into account (and not potential offsets):
``` haskell
>>> fromTo' @Celsius @Kelvin 0
Kelvin 0.0
```

### Pretty printing

``` haskell
>>> putQuantity (Celsius 25)
25 °C
>>> putQuantity (quantity @(Kilo Meter ./. Hour) 130)
130 km⋅hr⁻¹
```

Get info about some unit:

``` haskell
>>> putInfoU @Newton
Unit:       Newton
 abbr:      N
Dimension:  Mass .*. Length .*. Time.^-2
 abbr:      M⋅L⋅T⁻²
Normalized: Kilo Gram .*. Meter .*. Second.^-2
 abbr:      kg⋅m⋅s⁻²
```

### Unit arithmetics

Multiplication by a scalar:

``` haskell
>>> 2 * Meter 4
Meter 8
```

You can multiply or divide two units:

``` haskell
>>> putQuantity $ Newton 1 .*. Meter 2
2 N⋅m
```

``` haskell
>>> v = Kilo (Meter 10) ./. Hour 2
>>> putQuantity v
5.0 km⋅hr⁻¹
>>> putQuantity $ to @(Meter ./. Second) v
1.3888888888888888 m⋅s⁻¹
```

Automatically simplify units by converting the right unit :
``` haskell
>>> putQuantity $ Kilo (Meter 2) .*~  Meter 3
6.0e-3 km²
```

or the left unit:

``` haskell
>>> putQuantity $ Kilo (Meter 2) ~*.  Meter 3
6000.0 m²
```

### Convert to and from SI base units

``` haskell
>>> v = toNormalUnit (quantity @(Kilo Meter ./. Hour) 36)
>>> putQuantity v
10.0 m⋅s⁻¹
```

### Make your own units, prefixes and dimensions

Make a new dimension with its associated base unit:

``` haskell
$(mkDim "Angle" "A" 1000)
$(mkBaseUnit "Radian" "rad" ''Angle)
```

Make a new unit convertible by multiplying with some  factor:
``` haskell
$(mkUnit "Minute" "min" ''Time 60)
```

Make a new prefix:

``` haskell
$(mkPrefix "Micro" "µ" 1e-6)
```

Make a new unit with special conversion:

``` haskell
$(mkUnitNoFactor "Fahrenheit" "°F" ''Temperature)

instance Fractional a => ConversionFactor Fahrenheit a where
  factor = 5 / 9
  {-# INLINE factor #-}

instance Fractional a => ConvertibleUnit Fahrenheit a where
  toNormalUnit (Fahrenheit x) = Kelvin ((x + 459.67) * 5 / 9)
  {-# INLINE toNormalUnit #-}

  fromNormalUnit (Kelvin x) = Fahrenheit (x * 9 / 5 - 459.67)
  {-# INLINE fromNormalUnit #-}
```

## Comparison with other Haskell unit libraries

There are other excellent units libraries out there, the two most used being:
- [dimensional](https://hackage.haskell.org/package/dimensional)
- [units](https://hackage.haskell.org/package/units)

Compared to these two libraries, `convert-units` offers

* Greater flexibility for conversions that do not use conversion factors, for instance for logarithmic units (see logarithmic pitch units in `Data.Unit.NonStd.Frequency` for instance)
* The possibility to add dimensions, such as `Angle`, `Information` (not yet implemented, see this [wikipedia article](https://en.wikipedia.org/wiki/Quantities_of_information)), and so on ...

| Feature                              | convert-units | dimensional | units |
|---------------------------------------|:------------:|:-----------:|:-----:|
| Static dimension checking             |      ✅     |     ✅      |  ✅   |
| Custom unit                           |      ✅     |     ✅      |  ✅   |
| Custom prefixes                       |      ✅     |     ✅      |  ✅   |
| Custom dimensions                     |      ✅     |     ❌      |  ❌   |
| Pretty-printing units                 |      ✅     |     ✅      |  ✅   |
| Offset-aware conversions (e.g. °C/K)  |      ✅     |     ✅      |  ❌   |
| Any conversion                        |      ✅     |     ❌      |  ❌   |
