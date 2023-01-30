type t;

let create: Random.State.t => t;
let of_int: int => t;
let copy: t => t;
let split: t => (t, t);
let next_state: t => t;
let next_state_with_int64: t => (t, Int64.t);

let bool: t => (t, bool);
let next_int64: (t, Int64.t, Int64.t) => (t, Int64.t);
let next_double: (t, float, float) => (t, float);
let next_int: (t, int, int) => (t, int);

let random: unit => t;

let unit_float: Int64.t => float;
