module Int64Operations = {
  // Magic operations
  let (+) = Int64.add;
  let ( * ) = Int64.mul;
  let (-) = Int64.sub;
  let (/) = Int64.div;
  let abs = Int64.abs;
  let lor_ = Int64.logor;
  let lsr_ = Int64.shift_right_logical;
  let land_ = Int64.logand;
  let lxor_ = Int64.logxor;
  let mod_ = (a, n) => {
    a - n * (a / n);
  };
};

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
