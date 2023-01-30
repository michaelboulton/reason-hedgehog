type t('a) =
  | Random((SplittableRandom.t, Range.size_t) => 'a);

let run = (state, size) =>
  fun
  | Random(func) => func(state, size);

let delay = (f: unit => t('a)) => {
  Random((state, size) => run(state, size, f()));
};

let constant = value => {
  Random((_, _) => value);
};

let map = (f: 'a => 'b, r: t('a)) => {
  let mapper = (state, size) => {
    r |> run(state, size, _) |> f;
  };

  Random(mapper);
};

let bind = (r: t('a), f: 'a => t('b)): t('b) => {
  let binder = (state, size) => {
    let (state', state'') = SplittableRandom.split(state);
    // NOTE: This looks like map, but the type is different
    r |> run(state', size, _) |> f |> run(state'', size, _);
  };

  Random(binder);
};

module RandomMonad =
  CoolMonad.MakeMonad({
    type nonrec t('a) = t('a);
    let bind = bind;
    let return = constant;
    let fmap = map;
  });

let replicate = (times: int, r: t('a)): t(list('a)) => {
  let replicated = (initial_state, size) => {
    let rec loop = (state, acc) =>
      fun
      | k when k <= 0 => acc
      | k => {
          let (state', state'') = SplittableRandom.split(state);
          let x = run(state', size, r);
          loop(state'', [x, ...acc], k - 1);
        };

    loop(initial_state, [], times);
  };

  Random(replicated);
};

let sized = (f: Range.size_t => t('a)) => {
  Random((state, size) => run(state, size, f(size)));
};

let resize = (new_size, r) => {
  Random((state, _) => run(state, new_size, r));
};

// FIXME: Want to make this a module type as well, but running into issues
module NextNumber = (B: Numeric.Number) => {
  let next_number = v => {
    let bounds = Range.bounds(v);
    let calc_next = (state, size) => {
      let (lo, hi) = bounds(size);
      B.next_random(state, lo, hi);
    };
    Random(calc_next);
  };
};
