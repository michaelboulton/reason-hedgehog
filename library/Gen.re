open ExtraPervasives;

type random_tree_t('a) = RandomValues.t(Tree.t('a));

type t('a) =
  | Gen(random_tree_t('a));

let of_random = r => Gen(r);
let to_random =
  fun
  | Gen(r) => r;

let delay = f => {
  RandomValues.delay(f %> to_random) |> of_random;
};

let create = (shrinker: 'a => LazyList.t('a), r) => {
  RandomValues.map(Tree.unfold(id, shrinker), r) |> of_random;
};

let constant = x => {
  Tree.singleton(x) |> RandomValues.constant |> of_random;
};

let run_random = (this_state, size, r: RandomValues.t('x)): 'x => {
  RandomValues.run(this_state, size, r);
};
let bind_random =
    (m: random_tree_t('a), func: 'a => random_tree_t('b))
    : random_tree_t('b) => {
  let binder = (state, size) => {
    let (state', state'') = SplittableRandom.split(state);

    Tree.bind(
      run_random(state', size, m),
      func %> run_random(state'', size, _),
    );
  };

  RandomValues.Random(binder);
};

let map_random = (func, g: t('a)): t('b) => {
  to_random(g) |> func |> of_random;
};

let map_tree = (func, g: t('a)): t('b) => {
  map_random(RandomValues.map(func, _), g);
};

let bind = (m0: t('a), k0: 'a => t('b)): t('b) => {
  bind_random(to_random(m0), k0 %> to_random) |> of_random;
};

let map = (func: 'a => 'b, gx: t('a)): t('b) => {
  map_tree(Tree.map(func, _), gx);
};

module GenMonad =
  CoolMonad.MakeMonad({
    type nonrec t('a) = t('a);
    let bind = bind;
    let return = constant;
    let fmap = map;
  });

let map2 = (func: ('a, 'b) => 'c, gx: t('a), gy: t('b)): t('c) => {
  bind(gx, x => bind(gy, y => constant(func(x, y))));
};

let zip = (gx: t('a), gy: t('b)): t(('a, 'b)) => {
  let to_pair = (x: 'a, y: 'b): 'c => (x, y);
  map2(to_pair, gx, gy);
};

let tuple = (g: t('a)): t(('a, 'a)) => {
  zip(g, g);
};

// Just group these up
module ShrinkCombinators = {
  let no_shrink = g => {
    let drop =
      fun
      | Tree.Node(x, _) => Tree.Node(x, LazyList.empty);

    map_tree(drop, g);
  };

  let shrink_lazy = (func, g) => {
    map_tree(Tree.expand(func, _), g);
  };

  let shrink = (func, g) => {
    shrink_lazy(func(_) %> LazyList.tolazy, g);
  };
};

let sized = (func: Range.size_t => t('a)) => {
  RandomValues.sized(func(_) %> to_random) |> of_random;
};

let resize = (n, g: t('a)) => {
  map_random(RandomValues.resize(n, _), g);
};

let scale = (func: Range.size_t => Range.size_t, g: t('a)) => {
  let transform_size = n => resize(func(n), g);
  sized(transform_size);
};

module type GenNumber = {
  type gt;
  let integral: Range.t(gt) => t(gt);
};

module MakeGenNumber = (B: Numeric.Number) : (GenNumber with type gt = B.t) => {
  module ShrinkB = Shrink.MakeShrinkNumber(B);
  module RandomB = RandomValues.NextNumber(B);

  type gt = B.t;

  //
  // Combinators - Numeric
  //
  /// Generates a random number in the given inclusive range.
  // [<CompiledName("Integral")>]
  // let inline integral (range : Range<'a>) : Gen<'a> =
  //     create (Shrink.towards <| Range.origin range) (Random.integral range)
  let integral = (range: Range.t('a)): t('a) => {
    create(
      Range.origin(range) |> ShrinkB.shrink,
      RandomB.next_number(range),
    );
  };
};

module GenIntegers = MakeGenNumber(Numeric.NumberInt);
module GenBigints = MakeGenNumber(Numeric.NumberInt64);
module GenFloats = MakeGenNumber(Numeric.NumberFloat);

exception Invalid_argument(string);

// let private crashEmpty (arg : string) : 'b =
//     invalidArg arg (sprintf "'%s' must have at least one element" arg)
let throw_empty = arg_name => {
  raise(
    Invalid_argument(Printf.sprintf("'%s' must not be empty", arg_name)),
  );
};

module type GenSequences = {
  type st('a);
  let item: st('a) => t('a);
  let frequency: st((Numeric.NumberInt.t, 'a)) => t('a);
  let choice: st(t('a)) => t('a);
  let choice_rec: (st(t('a)), st(t('a))) => t('a);
  let try_filter_random:
    ('a => bool, RandomValues.t(Tree.t('a))) =>
    RandomValues.t(option(Tree.t('a)));
  let filter: ('a => bool, t('a)) => t('a);
  let try_filter: ('a => bool, t('a)) => t(option('a));

  let some: t(option('a)) => t('a);
  let option: t('a) => t(t(option('a)));
  let seq: (Range.t(int), t('a)) => t(st('a));
};

module MakeGenSequences =
       (S: Sequence.Seq)
       : (GenSequences with type st('a) = S.t('a)) => {
  type st('a) = S.t('a);

  // Seems like this should always be an int
  module IntGenerator = MakeGenNumber(Numeric.NumberInt);
  module RandomB = RandomValues.NextNumber(Numeric.NumberInt);
  module SeqShrink = Shrink.MakeShrinkSeq(S);

  let item = (xs: S.t('a)): t('a) => {
    switch (S.length(xs)) {
    | 0 => throw_empty("xs")
    | length =>
      let length = length - 1;
      let ix = Range.constant(0, length) |> IntGenerator.integral;

      GenMonad.(ix >>| S.at(xs, _));
    };
  };

  let frequency = xs0 => {
    // xs0 is a t() of a pair of weights and values
    let total = S.map(fst, xs0) |> S.fold_left((+), 0, _);

    let rec pick = n => {
      fun
      | el when el == S.mempty => throw_empty("xs")
      | l => {
          let (k, y) = S.hd(l);
          if (n <= k) {
            y;
          } else {
            pick(n - k, S.tl(l));
          };
        };
    };

    let n = Range.constant(1, total) |> IntGenerator.integral;

    GenMonad.(n >>| pick(_, xs0));
  };

  let choice: S.t(t('a)) => t('a) =
    fun
    | el when el == S.mempty => throw_empty("xs")
    | xs => {
        let ix =
          Range.constant(0, S.length(xs) - 1) |> IntGenerator.integral;

        GenMonad.(ix >>= S.at(xs, _));
      };

  let choice_rec = (nonrecs: S.t(t('a)), recs: S.t(t('a))): t('a) => {
    let sizer = n =>
      if (n <= 1) {
        choice(nonrecs);
      } else {
        let halve = x => x / 2;
        scale(halve, _) |> S.map(_, recs) |> S.mappend(_, nonrecs) |> choice;
      };

    sized(sizer);
  };

  let try_filter_random = (func: 'a => bool, r0) => {
    let rec try_n = k => {
      fun
      | 0 => RandomValues.constant(None)
      | n => {
          let r = RandomValues.resize(2 * k + n, r0);
          let binder = x =>
            if (Tree.outcome(x) |> func) {
              Some(Tree.filter(func, x)) |> RandomValues.constant;
            } else {
              try_n(k + 1, n - 1);
            };
          RandomValues.bind(r, binder);
        };
    };

    RandomValues.sized(max(1, _) %> try_n(0, _));
  };

  let filter = (func: 'a => bool, g) => {
    let rec loop = () => {
      let binder =
        fun
        | None => {
            let resize_delay = n => {
              RandomValues.resize(n + 1, RandomValues.delay(loop));
            };
            RandomValues.sized(resize_delay);
          }
        | Some(x) => RandomValues.constant(x);

      RandomValues.bind(to_random(g) |> try_filter_random(func, _), binder);
    };

    loop() |> of_random;
  };

  let try_filter = (func: 'a => bool, g) => {
    let binder =
      fun
      | None => Tree.singleton(None) |> RandomValues.constant
      | Some(x) => Tree.map(to_some, x) |> RandomValues.constant;

    let bound =
      RandomValues.bind(
        to_random(g) |> try_filter_random(func, _),
        binder(_),
      );

    bound |> of_random;
  };

  let some = g => {
    let binder =
      fun
      | Some(x) => constant(x)
      | None => failwith("internal error");

    filter(is_some, g) |> bind(_, binder);
  };

  let option = g => {
    let gen_options = n =>
      frequency(
        S.mappend(
          S.pure((2, constant(None))),
          S.pure((1 + n, map(to_some, g))),
        ),
      );

    sized(gen_options);
  };

  // let private atLeast (n : int) (xs : List<'a>) : bool =
  //     n = 0 || not (List.isEmpty (List.skip (n - 1) xs))
  let at_least = (n, xs) => {
    n == 0 || S.skip(n - 1, xs) |> S.is_empty |> (!);
  };

  let seq = (range: Range.t(int), g: t('a)): t(S.t('a)) => {
    let sizer = size => {
      let k = RandomB.next_number(range);
      let random_g = to_random(g);

      let filter_at_least = Range.lower_bound(size, range) |> at_least;

      // TODO: Goes to a list and back here, could remove but would need
      // to modularize random values as well...
      let r =
        RandomValues.(
          RandomValues.RandomMonad.(
            k
            >>= replicate(_, random_g)
            >>| S.of_list
            >>| SeqShrink.sequence_seq
            >>| Tree.filter(filter_at_least, _)
          )
        );
      r;
    };

    RandomValues.sized(sizer) |> of_random;
  };
};

module GenLists = MakeGenSequences(Sequence.ListSequence);
module GenArrays = MakeGenSequences(Sequence.ArraySequence);

module GenCharacters = {
  let char = (lo: char, hi: char) => {
    Range.constant(Char.code(lo), Char.code(hi))
    |> GenIntegers.integral
    |> map(Char.chr);
  };

  // /// Generates a Unicode character, including invalid standalone surrogates:
  // /// '\000'..'\65535'
  // [<CompiledName("UnicodeAll")>]
  // NOTE: no unicode unless we include a library
  // let unicode_all = {
  //   let lo = Char.chr(0);
  //   let hi = Char.chr(255);
  //   char(lo, hi);
  // };
  // TODO:

  let digit = char('0', '9');

  let lower = char('a', 'z');

  let upper = char('A', 'Z');

  let ascii = char(Char.chr(0), Char.chr(127));

  let latin = char(Char.chr(0), Char.chr(255));

  let alpha = GenLists.choice([lower, upper]);

  let alphanum = GenLists.choice([lower, upper, digit]);

  // /// Generates a Unicode character, excluding noncharacters
  // /// ('\65534', '\65535') and invalid standalone surrogates
  // /// ('\000'..'\65535' excluding '\55296'..'\57343').
  // [<CompiledName("Unicode")>]
  // let unicode : Gen<char> =
  //     let isNoncharacter x =
  //            x = Operators.char 65534
  //         || x = Operators.char 65535
  //     unicodeAll
  //     |> filter (not << isNoncharacter)
  //     |> filter (not << System.Char.IsSurrogate)
  // TODO:
  // let unicode;

  let string = (range, g: t(char)) => {
    let string_of_char_array = (as_array: array(char)) =>
      Array.fold_left((acc, c) => acc ++ String.make(1, c), "", as_array);

    // FIXME: This is ignored in f sharp version for some reason - needs fixing
    let sizer = size =>
      g
      |> resize(size)
      |> GenArrays.seq(range)
      |> map(string_of_char_array, _);

    sizer |> sized;
  };
};

module GenPrimitives = {
  let bool = GenLists.item([true, false]);

  // TODO:
  // Lots of missing ones here, see original. can be filled in eventually

  let int = (range: Range.t(int)): t(int) => {
    GenIntegers.integral(range);
  };
  let int64 = (range: Range.t(Int64.t)): t(Int64.t) => {
    GenBigints.integral(range);
  };
  let float = (range: Range.t(float)): t(float) => {
    GenFloats.integral(range);
  };
};

module GenSamples = {
  let sample_tree = (size: Range.size_t, count: int, g: t('a)) => {
    let seed = SplittableRandom.random();
    to_random(g)
    |> RandomValues.replicate(count, _)
    |> RandomValues.run(seed, size, _);
  };

  let sample = (size: Range.size_t, count: int, g) => {
    sample_tree(size, count, g) |> List.map(Tree.outcome);
  };

  let generate_tree = g => {
    let seed = SplittableRandom.random();
    to_random(g) |> RandomValues.run(seed, 30, _);
  };

  let polyprint_ = item => {
    let objectPrinter = ObjectPrinter.base;

    objectPrinter.polymorphicPrint(objectPrinter, item) |> print_endline;
  };

  let print_sample = g => {
    let forest = sample_tree(10, 5, g);

    let print_shrink = shrink => {
      Tree.outcome(shrink) |> polyprint_;
    };

    let print_tree = tree => {
      print_endline("=== Outcome ===");
      Tree.outcome(tree) |> polyprint_;
      print_endline("=== Shrinks ===");

      List.iter(print_shrink, Tree.shrinks(tree) |> LazyList.forceall);
      print_endline("---");
    };

    List.iter(print_tree, forest);
  };
};
