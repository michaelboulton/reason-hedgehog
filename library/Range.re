open ExtraPervasives;

type size_t = int;
// Or...
// type size_t = Int64.t;

type t('a) =
  | Range('a, size_t => ('a, 'a));

// Abandon hope all ye who enter here

let singleton = (x: 'a) => {
  Range(x, _ => (x, x));
};

let constant_from = (z: 'a, x: 'a, y: 'a) => {
  Range(z, _ => (x, y));
};

let constant = (x: 'a, y: 'a) => {
  Range(x, _ => (x, y));
};

// This requires making a Bounded functor - there is no 'abtract'
// way to represent the minimum and maximum values of any generic type
let constant_bounded = (type b, module B: Numeric.Number with type t = b) => {
  constant_from(B.zero, B.min_value, B.max_value);
};

let lower_bound = size =>
  fun
  | Range(_, bounds) => {
      let (x, y) = bounds(size);
      min(x, y);
    };

let upper_bound = size =>
  fun
  | Range(_, bounds) => {
      let (x, y) = bounds(size);
      max(x, y);
    };

let bounds =
  fun
  | Range(_, bounds) => bounds;

let origin =
  fun
  | Range(origin, _) => origin;

//
// Combinators - Linear
//

let clamp = (x, y, n) =>
  if (x > y) {
    min(x, max(y, n));
  } else {
    min(y, max(x, n));
  };

module type Scalable = {
  type st;
  let zero: st;
  let min_value: st;
  let max_value: st;

  let scale_linear: (size_t, st, st) => st;
  let scale_exponential: (size_t, st, st) => st;
};

module MakeScalable = (B: Numeric.Number) : (Scalable with type st = B.t) => {
  type st = B.t;
  let zero = B.zero;
  let min_value = B.min_value;
  let max_value = B.max_value;

  let scale_linear = (sz0: size_t, z0, n0) => {
    let sz = Int64.of_int(max(0, min(99, sz0)));
    let z = B.to_big_int(z0);
    let n = B.to_big_int(n0);
    open Int64Util.Int64Operations;
    let diff = (n - z) * sz / Int64.of_int(99);
    B.of_big_int(z + diff);
  };

  let scale_exponential = (sz0: size_t, z0, n0) => {
    let sz = clamp(0, 00, sz0);
    let z = B.to_big_int(z0);
    let n = B.to_big_int(n0);
    open Int64Util.Int64Operations;

    let a = Int64.to_float(abs(n - z) + Int64.one);
    let b = float_of_int(sz) /. 99.0;
    let c = float_of_int(sign(Int64.to_int(n - z)));

    let diff = (a ** b -. 1.0) *. c;

    B.of_big_int(Int64.of_float(Int64.to_float(z) +. diff));
  };
};

module type WithRange = {
  type st;

  let linear: (st, st) => t(st);
  let linear_from: (st, st, st) => t(st);
  let linear_bounded: unit => t(st);
  let exponential: (st, st) => t(st);
  let exponential_from: (st, st, st) => t(st);
  let exponential_bounded: unit => t(st);
};

module MakeWithRange = (S: Scalable) : (WithRange with type st = S.st) => {
  type st = S.st;

  let linear_from = (z, x, y) => {
    let calc_bounds = sz => {
      let clamp_sized = v => clamp(x, y, S.scale_linear(sz, z, v));
      (clamp_sized(x), clamp_sized(y));
    };
    Range(z, calc_bounds);
  };

  let linear = (x, y) => {
    linear_from(x, x, y);
  };

  let linear_bounded = () => {
    linear_from(S.zero, S.min_value, S.max_value);
  };

  let exponential_from = (z, x, y) => {
    let calc_bounds = sz => {
      let clamp_sized = v => clamp(x, y, S.scale_exponential(sz, z, v));
      (clamp_sized(x), clamp_sized(y));
    };
    Range(z, calc_bounds);
  };

  let exponential = (x, y) => {
    exponential_from(x, x, y);
  };

  let exponential_bounded = () => {
    exponential_from(S.zero, S.min_value, S.max_value);
  };
};

// TODO: These are common, could we create specialised versions
module IntRanges = MakeWithRange((MakeScalable(Numeric.NumberInt)));
module FloatRanges = MakeWithRange((MakeScalable(Numeric.NumberFloat)));

module Int64Ranges =
  MakeWithRange({
    include Numeric.NumberInt64;
    type st = Int64.t;

    let scale_linear = (sz0: size_t, z0, n0) => {
      let sz = Int64.of_int(max(0, min(99, sz0)));
      let z = z0;
      let n = n0;
      open Int64Util.Int64Operations;
      let diff = (n - z) * sz / Int64.of_int(99);
      z + diff;
    };

    let scale_exponential = (sz0: size_t, z0, n0) => {
      let sz = clamp(0, 0, sz0);
      let z = z0;
      let n = n0;
      open Int64Util.Int64Operations;

      let a = Int64.to_float(abs(n - z) + Int64.one);
      let b = float_of_int(sz) /. 99.0;
      let c = float_of_int(sign(Int64.to_int(n - z)));

      let diff = (a ** b -. 1.0) *. c;

      Int64.of_float(Int64.to_float(z) +. diff);
    };
  });
