module type BigIntConvertable = {
  type t;
  let of_big_int: Int64.t => t;
  let to_big_int: t => Int64.t;
};

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

module NumberInt: Number with type t = int;

module NumberFloat: Number with type t = float;

module NumberInt64: Number with type t = Int64.t;
