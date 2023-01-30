// Code from the jane street splittable random (which the haskell
// hedgehog refers to) implemented in reasonml
// https://github.com/janestreet/splittable_random/blob/master/src/splittable_random.mli

open ExtraPervasives;

module type StateI = {
  type t;

  let create: Random.State.t => t;
  let of_int: int => t;
  let copy: t => t;
  let split: t => (t, t);
  let next_state: t => t;
  let next_state_with_int64: t => (t, Int64.t);
};

let is_odd = x => Int64.logor(x, 1L) == x;

// https://github.com/janestreet/base/blob/master/src/bitcount.ml
let bitcount = x => {
  open Int64Util.Int64Operations;

  let m1 = 0x5555555555555555L;
  let m2 = 0x3333333333333333L;
  let m4 = 0x0f0f0f0f0f0f0f0fL;
  let h01 = 0x0101010101010101L;

  let x = x - land_(lsr_(x, 1), m1);
  let x = land_(x, m2) + land_(lsr_(x, 2), m2);
  let x = land_(x + lsr_(x, 4), m4);

  Int64.to_int(lsr_(x * h01, 56));
};

module State: StateI = {

  type t = {
    value: Int64.t,
    odd_gamma: Int64.t,
  };

  /********************** */

  let golden_gamma = 0x9e37_79b9_7f4a_7c15L;

  let mix_bits = (z, n) => {
    Int64.
      (logxor(z, shift_left(z, n)));
      // z lxor z lsr n;
  };

  let mix64 = (z: Int64.t) => {
    open Int64Util.Int64Operations;
    let z = mix_bits(z, 33) * 0xff51_afd7_ed55_8ccdL;
    let z = mix_bits(z, 33) * 0xc4ce_b9fe_1a85_ec53L;
    mix_bits(z, 33);
  };

  let mix64_variant13 = (z: Int64.t) => {
    open Int64Util.Int64Operations;
    let z = mix_bits(z, 30) * 0xbf58_476d_1ce4_e5b9L;
    let z = mix_bits(z, 27) * 0x94d0_49bb_1331_11ebL;
    mix_bits(z, 31);
  };

  let mix_odd_gamma = (z: Int64.t) => {
    open Int64Util.Int64Operations;

    let z = lor_(mix64_variant13(z), 1L);
    let n = bitcount(lxor_(z, lsr_(z, 1)));

    if (n < 24) {
      lxor_(z, 0xaaaa_aaaa_aaaa_aaaaL);
    } else {
      z;
    };
  };

  let next_state = t => {
    open Int64Util.Int64Operations;
    let next_value = t.value + t.odd_gamma;
    {value: next_value, odd_gamma: t.odd_gamma};
  };

  let next_state_with_int64 = t => {
    let next = next_state(t);
    (next, mix64(next.value));
  };

  let of_seed_and_gamma = (value, gamma) => {
    let next_value = mix64(value);
    let odd_gamma = mix_odd_gamma(gamma);
    {value: next_value, odd_gamma};
  };

  let random_int64 = state => {
    // Fixed here
    let is_negative = Random.bits() mod 2;
    let random_int = Random.State.int64(state, Int64.max_int);
    Int64Util.Int64Operations.(random_int * (is_negative == 0 ? (-1L) : 1L));
  };

  /********************** */

  let create = state => {
    let value = random_int64(state);
    let odd_gamma = random_int64(state);
    of_seed_and_gamma(value, odd_gamma);
  };

  let split = t => {
    let t1 = next_state(t);
    let t2 = next_state(t1);
    (t1, t2);
  };

  let of_int = seed => {value: Int64.of_int(seed), odd_gamma: golden_gamma};

  let copy = ({value, odd_gamma}) => {value, odd_gamma};
};

include State;

let remainder_is_unbiased = (draw, remainder, draw_maximum, remainder_maximum) => {
  Int64Util.Int64Operations.(
    draw - remainder <= draw_maximum - remainder_maximum
  );
};

let next_int64_ = (state: State.t, lo: Int64.t, hi: Int64.t) => {
  open Int64Util.Int64Operations;

  let max_val = () => {
    let (next_state, draw) = next_state_with_int64(state);
    let p = land_(draw, Int64.max_int) + lo;

    (next_state, p);
  };

  let rec between = (state, lo, hi) => {
    let (next_state, draw) = next_state_with_int64(state);

    if (lo <= draw && draw <= hi) {
      (next_state, draw);
    } else {
      between(next_state, lo, hi);
    };
  };

  let rec non_negative_up_to = (state, maximum) => {
    let (next_state, draw) = next_state_with_int64(state);
    let draw = land_(draw, Int64.max_int);
    let remainder = Int64.rem(draw, Int64.succ(maximum));
    if (remainder_is_unbiased(draw, remainder, Int64.max_int, maximum)) {
      (next_state, remainder);
    } else {
      non_negative_up_to(next_state, maximum);
    };
  };

  let diff = hi - lo;

  if (diff == Int64.max_int) {
    max_val();
  } else if (diff >= 0L) {
    let (next_state, draw) = non_negative_up_to(state, diff);
    (next_state, draw + lo);
  } else {
    between(state, lo, hi);
  };
};

let next_int64 = (state: State.t, lo, hi) =>
  if (lo > hi) {
    next_int64_(state, hi, lo);
  } else {
    next_int64_(state, lo, hi);
  };

/*
 let double_ulp = 2. **. -53.

 let%test_unit "double_ulp" =
   let open Float.O in
   match Word_size.word_size with
   | W64 ->
     assert (1.0 -.  double_ulp         < 1.0);
     assert (1.0 -. (double_ulp /. 2.0) = 1.0)
   | W32 ->
     (* 32-bit OCaml uses a 64-bit float representation but 80-bit float instructions, so
        rounding works differently due to the conversion back and forth. *)
     assert (1.0 -.  double_ulp         <  1.0);
     assert (1.0 -. (double_ulp /. 2.0) <= 1.0)

 let unit_float_from_int64 int64 =
 (Int64.to_float (int64 lsr 11)) *. double_ulp
 */


let is_odd = x => Int64.logxor(x, 1L) == x;

let format_bits = (int64: Int64.t): string => {
  let rec impl = acc =>
    fun
    | (-1) => String.concat("", acc)
    | n => {
        let bit = Int64.shift_left(1L, n) |> Int64.logand(_, int64);
        let is_odd = bit != 0L;
        let formatted = is_odd ? "1" : "0";
        impl(acc @ [formatted], n - 1);
      };

  impl([], 63);
};

let double_ulp = 2.0 ** (-53.0);

// Scale the int64 from 0 < x < 1
let unit_float = int64 =>
  Int64.to_float(Int64Util.Int64Operations.lsr_(int64, 11)) *. double_ulp;

let bool = state => {
  let (next_state, draw) = next_state_with_int64(state);
  (next_state, is_odd(draw));
};

let finite_float = (state, draw) => {
  let rec impl = (state, lo, hi) => {
    let range = hi -. lo;

    if (is_invalid_float(range)) {
      let mid = (hi +. lo) /. 2.0;
      let (next_state, as_bool) = bool(state);

      if (as_bool) {
        impl(next_state, lo, mid);
      } else {
        impl(next_state, mid, hi);
      };
    } else {
      let (next_state, draw) = next_state_with_int64(state);
      (next_state, lo +. unit_float(draw) *. range);
    };
  };

  impl(state);
};

// NOTE: ocaml/reason flaots are already 64 bit
let next_double_ = (state, lo, hi) => {
  let (next_state, big_int) =
    next_int64(state, Int64.min_int, Int64.max_int);

  let (next_state, scaled) = finite_float(next_state, big_int, lo, hi);

  (next_state, scaled);
};

let next_double = (state, lo, hi) =>
  if (lo > hi) {
    next_double_(state, hi, lo);
  } else {
    next_double_(state, lo, hi);
  };

let next_int_ = (state, lo, hi) => {
  let lo = Int64.of_int(lo);
  let hi = Int64.of_int(hi);

  let (next_state, big_int) = next_int64(state, lo, hi);

  (next_state, Int64.to_int(big_int));
};

let next_int = (state, lo, hi) =>
  if (lo > hi) {
    next_int_(state, hi, lo);
  } else {
    next_int_(state, lo, hi);
  };

// /// Create a new random 'Seed'.
// let random () : Seed =
//     from (uint64 System.DateTimeOffset.UtcNow.Ticks + 2UL * goldenGamma)
let random = () => {
  Random.bits() |> of_int;
};
