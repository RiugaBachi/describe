# Revision history for describe

##0.3.1.1 -- 2020-01-09

* Added flist combinator

##0.3.1.0 -- 2020-01-09

* Added NText combinator

##0.3.0.1 -- 2020-01-09

* Added unexposed combinator modules

##0.3.0.0 -- 2020-01-09

* Reorganized module structure
* Added new combinators: FText, ByteEnum, FList, LPList, Const

##0.2.0.6 -- 2019-10-08

* Added `isolate` combinator.

## 0.2.0.5 -- 2019-10-07

* Added the `Remaining` combinator.
* Added `deserializeEx`.

## 0.2.0.4 -- 2019-10-06

* Made Vec instance for Equals overlapping. 

## 0.2.0.3 -- 2019-10-06

* Added constraint on `Optional` to prevent overlapping instances on the supplied predicate.

## 0.2.0.2 -- 2019-10-06

* Exposed `Equals`.

## 0.2.0.1 -- 2019-10-06

* Exposed hidden instances.

## 0.2.0.0 -- 2019-10-06

* Added `Describe` type class.
* Made `Describe` generically-derivable.
* Refactored module structure.
* Added type- and value-level combinators to be used with `Describe`.

## 0.1.2.1 -- 2019-09-08

* Fixed unwrapPut / serialize.

## 0.1.2.0 -- 2019-09-08

* Fixed Monad instance.

## 0.1.1.0 -- 2019-09-05

* Added Monad instance for Descriptor.

## 0.1.0.0 -- 2019-06-03

* Initial release.
