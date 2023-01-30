open ExtraPervasives;

module type BigIntConvertable = {
  type t;
  let of_big_int: Int64.t => t;
  let to_big_int: t => Int64.t;
};

// TODO: split out min/max/zero from other things
// Makes it easier to use in Ranges
module type Number = {
  include BigIntConvertable;
  let min_value: t;
  let max_value: t;
  let zero: t;
  let (+): (t, t) => t;
  let ( * ): (t, t) => t;
  let (/): (t, t) => t;
  let (-): (t, t) => t;

  let next_random: (SplittableRandom.t, t, t) => t;
};

// TODO: Could be bad because we ignore the returned state...
let wrap_ignore_state = func => {
  let impl = (state, lo, hi) => {
    let (_, draw) = func(state, lo, hi);
    draw;
  };
  impl;
};

module NumberInt: Number with type t = int = {
  type t = int;
  let min_value = min_int;
  let max_value = max_int;
  let zero = 0;
  let of_big_int = Int64.to_int;
  let to_big_int = Int64.of_int;

  let (+) = (+);
  let (/) = (/);
  let (-) = (-);
  let ( * ) = ( * );

  let next_random = wrap_ignore_state(SplittableRandom.next_int);
};

module NumberFloat: Number with type t = float = {
  type t = float;
  let min_value = min_float;
  let max_value = max_float;
  let zero = 0.;
  let of_big_int = Int64.float_of_bits;
  let to_big_int = Int64.bits_of_float;

  let (+) = (+.);
  let (/) = (/.);
  let (-) = (-.);
  let ( * ) = ( *. );

  let next_random = wrap_ignore_state(SplittableRandom.next_double);
};

module NumberInt64: Number with type t = Int64.t = {
  include Int64Util.Int64Operations;

  type t = Int64.t;
  let min_value = Int64.min_int;
  let max_value = Int64.max_int;
  let zero = 0L;
  let of_big_int = id;
  let to_big_int = id;

  let next_random = wrap_ignore_state(SplittableRandom.next_int64);
};
