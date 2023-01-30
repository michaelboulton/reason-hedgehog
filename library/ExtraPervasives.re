// Useful utilities

// Identity function
let id = x => x;

// Left to right composition
let (%>) = (f1: 'a => 'x, f2: 'x => 'b, x: 'a): 'b => f2(f1(x));

let to_some = x => Some(x);

let is_some =
  fun
  | Some(_) => true
  | None => false;

let is_invalid_float = List.mem(_, [nan, infinity, neg_infinity]);

let sign =
  fun
  | 0 => 0
  | x when x < 0 => (-1)
  | _ => 1;
