type size_t = int;

type t('a);

// Abandon hope all ye who enter here

// No range - always returns the same value
let singleton: 'a => t('a);

let constant_from: ('a, 'a, 'a) => t('a);

let constant: ('a, 'a) => t('a);

/// Construct a range which is unaffected by the size parameter using the
/// full range of a data type.
let constant_bounded: (module Numeric.Number with type t = 'a) => t('a);

let lower_bound: (size_t, t('a)) => 'a;

let upper_bound: (size_t, t('a)) => 'a;

let bounds: (t('a), size_t) => ('a, 'a);

let origin: t('a) => 'a;

//
// Combinators - Linear
//

/// Truncate a value so it stays within some range.
// let clamp (x : 'a) (y : 'a) (n : 'a) : 'a =
//     if x > y then
//         min x (max y n)
//     else
//         min y (max x n)

let clamp: ('a, 'a, 'a) => 'a;

// TODO: Does this need to be exposed?
module type Scalable = {
  type st;
  let zero: st;
  let min_value: st;
  let max_value: st;

  // -- | Scale an integral linearly with the size parameter.
  // --
  // /// Scale an integral linearly with the size parameter.
  // let inline scaleLinear (sz0 : Size) (z0 : 'a) (n0 : 'a) : 'a =
  //     let sz =
  //         max 0 (min 99 sz0)
  //     let z =
  //         toBigInt z0
  //     let n =
  //         toBigInt n0
  //     let diff =
  //         ((n - z) * bigint sz) / (bigint 99)
  //     fromBigInt (z + diff)
  let scale_linear: (size_t, st, st) => st;

  // /// Scale an integral exponentially with the size parameter.
  // let inline scaleExponential (sz0 : Size) (z0 : 'a) (n0 : 'a) : 'a =
  //     let sz =
  //         clamp 0 99 sz0
  //     let z =
  //         toBigInt z0
  //     let n =
  //         toBigInt n0
  //     let diff =
  // (
  //   (
  //     (float (abs (n - z) + 1I))
  //     **
  //     (float sz / 99.0)
  //   ) - 1.0
  // )
  // *
  // float (sign (n - z))
  //     fromBigInt (bigint (round (float z + diff)))
  let scale_exponential: (size_t, st, st) => st;
};

module type WithRange = {
  type st;

  // /// Construct a range which scales the second bound relative to the size
  // /// parameter.
  // [<CompiledName("Linear")>]
  // let inline linear (x : 'a) (y : 'a) : Range<'a> =
  //   linearFrom x x y
  let linear: (st, st) => t(st);

  // /// Construct a range which scales the bounds relative to the size
  // /// parameter.
  // [<CompiledName("LinearFrom")>]
  // let inline linearFrom (z : 'a) (x : 'a) (y : 'a) : Range<'a> =
  //     Range (z, fun sz ->
  //         let x_sized =
  //             clamp x y (scaleLinear sz z x)
  //         let y_sized =
  //             clamp x y (scaleLinear sz z y)
  //         x_sized, y_sized)
  let linear_from: (st, st, st) => t(st);

  // /// Construct a range which is scaled relative to the size parameter and
  // /// uses the full range of a data type.
  // [<CompiledName("LinearBounded")>]
  // let inline linearBounded () : Range<'a> =
  //     let lo = minValue ()
  //     let hi = maxValue ()
  //     let zero = LanguagePrimitives.GenericZero
  //     linearFrom zero lo hi
  let linear_bounded: unit => t(st);

  // /// Construct a range which scales the second bound exponentially relative
  // /// to the size parameter.
  // [<CompiledName("Exponential")>]
  // let inline exponential (x : 'a) (y : 'a) : Range<'a> =
  //     exponentialFrom x x y
  let exponential: (st, st) => t(st);

  // /// Construct a range which scales the bounds exponentially relative to the
  // /// size parameter.
  // [<CompiledName("ExponentialFrom")>]
  // let inline exponentialFrom (z : 'a) (x : 'a) (y : 'a) : Range<'a> =
  //     Range (z, fun sz ->
  //         let x_sized =
  //             clamp x y (scaleExponential sz z x)
  //         let y_sized =
  //             clamp x y (scaleExponential sz z y)
  //         x_sized, y_sized)
  let exponential_from: (st, st, st) => t(st);

  // /// Construct a range which is scaled exponentially relative to the size
  // /// parameter and uses the full range of a data type.
  // [<CompiledName("ExponentialBounded")>]
  // let inline exponentialBounded () : Range<'a> =
  //     let lo = minValue ()
  //     let hi = maxValue ()
  //     let zero = LanguagePrimitives.GenericZero
  //     exponentialFrom zero lo hi
  let exponential_bounded: unit => t(st);
};

module MakeScalable: (B: Numeric.Number) => Scalable with type st = B.t;

module MakeWithRange: (S: Scalable) => WithRange with type st = S.st;

module IntRanges: WithRange with type st = int;
module FloatRanges: WithRange with type st = float;
module Int64Ranges: WithRange with type st = Int64.t;
