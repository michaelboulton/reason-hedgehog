open ExtraPervasives;

type journal_t =
  | Journal(LazyList.t(string));

type result_t('a) =
  | Failure
  | Discard
  | Success('a);

type gen_journal_result_t('a) = Gen.t((journal_t, result_t('a)));
type property_t('a) =
  | Property(gen_journal_result_t('a));

type tests = int;
type discards = int;
type shrinks = int;

type status_t =
  | Failed(shrinks, journal_t)
  | GaveUp
  // Note: uppercase to not conflict with 'Ok'
  | OK;

type report_t = {
  tests,
  discards,
  status: status_t,
};

let ft_fst = (func: 'a => 'c, (x, y): ('a, 'b)): ('c, 'b) => {
  (func(x), y);
};

let ft_snd = (func: 'b => 'c, (x, y): ('a, 'b)): ('a, 'c) => {
  (x, func(y));
};

module Journal = {
  let of_list = xs => {
    Journal(xs);
  };

  let to_list =
    fun
    | Journal(xs) => LazyList.forceall(xs);

  let empty = LazyList.empty |> of_list;

  let singleton = x => LazyList.singleton(x) |> of_list;

  let delayed_singleton = (x: unit => string) => {
    let do_delay = LazyList.delayed(() => LazyList.singleton(x()));
    do_delay |> of_list;
  };

  // Could be done with destructuring arg labels too
  let append = (xsj, ysj) =>
    switch (xsj, ysj) {
    | (Journal(xs), Journal(ys)) => LazyList.append(xs, ys) |> of_list
    };
};

module Result = {
  let map: ('a => 'b, result_t('a)) => result_t('b) =
    func =>
      fun
      | Failure => Failure
      | Discard => Discard
      | Success(x) => Success(func(x));

  let filter: ('a => bool, result_t('a)) => result_t('b) =
    func =>
      fun
      | Failure => Failure
      | Discard => Discard
      | Success(x) when func(x) => Success(x)
      | Success(_) => Discard;

  let is_failure: result_t('a) => bool =
    fun
    | Failure => true
    | _ => false;
};

module PrettyPrint = {
  open Printf;

  let render_tests: tests => string =
    fun
    | 1 => "1 test"
    | n => sprintf("%d tests", n);

  let render_discards: discards => string =
    fun
    | 1 => "1 discard"
    | n => sprintf("%d discards", n);

  let render_and_discards: discards => string =
    fun
    | 0 => ""
    | 1 => " and 1 discard"
    | n => sprintf(" and %d discards", n);

  let render_and_shrinks: shrinks => string =
    fun
    | 0 => ""
    | 1 => " and 1 shrink"
    | n => sprintf(" and %d shrinks", n);

  let append = (s: string, msg: string) => s ++ msg;

  let render_ok = (n_tests: tests): string =>
    sprintf("+++ OK, passed %s", render_tests(n_tests));

  let render_gaveup = (n_tests: tests, n_discards: discards): string =>
    sprintf(
      "!!! Gave up after %s, passed %s.",
      render_discards(n_discards),
      render_tests(n_tests),
    );

  let render_failed =
      (
        n_tests: tests,
        n_discards: discards,
        n_shrinks: shrinks,
        journal: journal_t,
      )
      : string => {
    let header =
      sprintf(
        "*** Failed - falsifiable (after %s%s%s)",
        render_tests(n_tests),
        render_and_discards(n_discards),
        render_and_shrinks(n_shrinks),
      );

    let add_line = (acc, v) => acc ++ "\n" ++ v;

    let total_message =
      Journal.to_list(journal) |> List.fold_left(add_line, header, _);

    total_message;
  };
};

exception HedgehogException(string);
exception GaveUpException(tests, discards);
exception FailedException(tests, discards, shrinks, journal_t);

module Report = {
  open PrettyPrint;

  let render = ({status, tests, discards}: report_t) => {
    switch (status) {
    | OK => render_ok(tests)
    | GaveUp => render_gaveup(tests, discards)
    | Failed(n_shrinks, journal) =>
      render_failed(tests, discards, n_shrinks, journal)
    };
  };

  let try_raise = ({status, tests, discards}: report_t) => {
    switch (status) {
    | OK => ()
    | GaveUp => GaveUpException(tests, discards) |> raise
    | Failed(n_shrinks, journal) =>
      FailedException(tests, discards, n_shrinks, journal) |> raise
    };
  };
};

module Property = {
  let of_gen = (x: gen_journal_result_t('a)): property_t('a) =>
    Property(x);

  // TODO: consistent destructuring of args...probably nicer to do with function arg destructuring
  let to_gen: property_t('a) => gen_journal_result_t('a) =
    fun
    | Property(x) => x;

  // [<CompiledName("Delay")>]
  // let delay (f : unit -> Property<'a>) : Property<'a> =
  //     Gen.delay (toGen << f) |> ofGen
  let delay = (func: unit => property_t('a)) => {
    Gen.delay(func %> to_gen) |> of_gen;
  };

  // [<CompiledName("Filter")>]
  // let filter (p : 'a -> bool) (m : Property<'a>) : Property<'a> =
  //     Gen.map (second <| Result.filter p) (toGen m) |> ofGen
  let filter = (pred: 'a => bool, m: property_t('a)): property_t('a) => {
    let filter_results = Result.filter(pred, _) |> ft_snd;
    let mapped = Gen.map(filter_results, to_gen(m));
    mapped |> of_gen;
  };

  // [<CompiledName("OfResult")>]
  // let ofResult (x : Result<'a>) : Property<'a> =
  //     (Journal.empty, x) |> Gen.constant |> ofGen
  let of_result = (x: result_t('a)): property_t('a) => {
    Gen.constant((Journal.empty, x)) |> of_gen;
  };

  // [<CompiledName("Failure")>]
  // let failure : Property<unit> =
  //     Failure |> ofResult
  let failure = of_result(Failure);

  // [<CompiledName("Discard")>]
  // let discard : Property<unit> =
  //     Discard |> ofResult
  let discard = of_result(Discard);

  // [<CompiledName("Success")>]
  // let success (x : 'a) : Property<'a> =
  //     Success x |> ofResult
  let success = x => {
    of_result(Success(x));
  };

  // [<CompiledName("OfBool")>]
  // let ofBool (x : bool) : Property<unit> =
  //     if x then
  //         success ()
  //     else
  //         failure
  let of_bool = (b: bool): property_t(unit) =>
    if (b) {
      success();
    } else {
      failure;
    };

  // [<CompiledName("CounterExample")>]
  // let counterexample (msg : unit -> string) : Property<unit> =
  //     Gen.constant (Journal.delayedSingleton msg, Success ()) |> ofGen
  let counterexample = (msg: unit => string): property_t(unit) => {
    // Gen.constant(Journal.delayed_singleton)
    (Journal.delayed_singleton(msg), Success()) |> Gen.constant |> of_gen;
  };

  // let private mapGen
  //         (f : Gen<Journal * Result<'a>> -> Gen<Journal * Result<'b>>)
  //         (x : Property<'a>) : Property<'b> =
  //     toGen x |> f |> ofGen
  let map_gen =
      (
        func: gen_journal_result_t('a) => gen_journal_result_t('b),
        x: property_t('a),
      )
      : property_t('b) => {
    to_gen(x) |> func |> of_gen;
  };

  // [<CompiledName("Map")>]
  // let map (f : 'a -> 'b) (x : Property<'a>) : Property<'b> =
  //     (mapGen << Gen.map << second << Result.map) f x
  let map = (func: 'a => 'b, x: property_t('a)): property_t('b) => {
    let chain = Result.map %> ft_snd %> Gen.map %> map_gen;
    chain(func, x);
  };

  // let private bindGen
  //         (m : Gen<Journal * Result<'a>>)
  //         (k : 'a -> Gen<Journal * Result<'b>>) : Gen<Journal * Result<'b>> =
  //     Gen.bind m <| fun (journal, result) ->
  //         match result with
  //         | Failure ->
  //             Gen.constant (journal, Failure)
  //         | Discard ->
  //             Gen.constant (journal, Discard)
  //         | Success x ->
  //             Gen.map (first (Journal.append journal)) (k x)
  let bind_gen =
      (m: gen_journal_result_t('a), func: 'a => gen_journal_result_t('b))
      : gen_journal_result_t('b) => {
    let match_result = ((journal, result)) => {
      switch (result) {
      | Success(x) =>
        let append_journal = Journal.append(journal, _) |> ft_fst;
        Gen.map(append_journal, func(x));
      | Failure => Gen.constant((journal, Failure))
      | Discard => Gen.constant((journal, Discard))
      };
    };
    Gen.bind(m, match_result);
  };

  // [<CompiledName("Bind")>]
  // let bind (m : Property<'a>) (k : 'a -> Property<'b>) : Property<'b> =
  //     bindGen (toGen m) (toGen << k) |> ofGen
  let bind:
    type ta tb. (property_t(ta), ta => property_t(tb)) => property_t(tb) =
    (m, func) => {
      bind_gen(to_gen(m), func %> to_gen) |> of_gen;
    };

  // [<CompiledName("ForAll")>]
  // let forAll (gen : Gen<'a>) (k : 'a -> Property<'b>) : Property<'b> =
  //     let handle (e : exn) =
  //         Gen.constant (Journal.singleton (string e), Failure) |> ofGen
  //     let prepend (x : 'a) =
  //         bind (counterexample (fun () -> sprintf "%A" x)) (fun _ -> try k x with e -> handle e) |> toGen
  //     Gen.bind gen prepend |> ofGen
  let for_all:
    type gt pt. (Gen.t(gt), gt => property_t(pt)) => property_t(pt) =
    (gen, func) => {
      let handle = (e: exn) => {
        let as_string = Printexc.to_string(e) |> Journal.singleton;
        Gen.constant((as_string, Failure)) |> of_gen;
      };

      let objectPrinter = ObjectPrinter.base;

      let prepend = (x: gt) => {
        let c =
          counterexample(() =>
            objectPrinter.polymorphicPrint(objectPrinter, x)
          );
        let binder = _ =>
          try (func(x)) {
          | n => handle(n)
          };

        bind(c, binder) |> to_gen;
      };

      Gen.bind(gen, prepend) |> of_gen;
    };

  // [<CompiledName("ForAll")>]
  // let forAll' (gen : Gen<'a>) : Property<'a> =
  //     forAll gen success
  // TODO: Not used?
  // let for_all_ = gen => for_all(gen, success);

  //
  // Runner
  //

  // let rec private takeSmallest
  //         (Node ((journal, x), xs) : Tree<Journal * Result<'a>>)
  //         (nshrinks : int<shrinks>) : Status =
  //     match x with
  //     | Failure ->
  //         match LazyList.tryFind (Result.isFailure << snd << Tree.outcome) xs with
  //         | None ->
  //             Failed (nshrinks, journal)
  //         | Some tree ->
  //             takeSmallest tree (nshrinks + 1<shrinks>)
  //     | Discard ->
  //         GaveUp
  //     | Success _ ->
  //         OK
  let rec take_smallest =
          (~tree as Tree.Node((journal, x), xs), n_shrinks: shrinks)
          : status_t => {
    switch (x) {
    | Success(x) => OK
    | Discard => GaveUp
    | Failure =>
      let chain = Tree.outcome %> snd %> Result.is_failure;
      let maybe_found = LazyList.try_find(chain, xs) |> Lazy.force;
      switch (maybe_found) {
      | None => Failed(n_shrinks, journal)
      | Some(tree) => take_smallest(tree, n_shrinks + 1)
      };
    };
  };

  // [<CompiledName("Report")>]
  // let report' (n : int<tests>) (p : Property<unit>) : Report =
  //     let random = toGen p |> Gen.toRandom
  //     let nextSize size =
  //         if size >= 100 then
  //             1
  //         else
  //             size + 1
  //     let rec loop seed size tests discards =
  //         if tests = n then
  //             { Tests = tests
  //               Discards = discards
  //               Status = OK }
  //         elif discards >= 100<discards> then
  //             { Tests = tests
  //               Discards = discards
  //               Status = GaveUp }
  //         else
  //             let seed1, seed2 = Seed.split seed
  //             let result = Random.run seed1 size random
  //             match snd (Tree.outcome result) with
  //             | Failure ->
  //                 { Tests = tests + 1<tests>
  //                   Discards = discards
  //                   Status = takeSmallest result 0<shrinks> }
  //             | Success () ->
  //                 loop seed2 (nextSize size) (tests + 1<tests>) discards
  //             | Discard ->
  //                 loop seed2 (nextSize size) tests (discards + 1<discards>)
  //     let seed = Seed.random ()
  //     loop seed 1 0<tests> 0<discards>
  let report_ = (n: tests, p: property_t(unit)): report_t => {
    let as_random = to_gen(p) |> Gen.to_random;

    let next_size = size =>
      if (size >= 100) {
        1;
      } else {
        size + 1;
      };

    let rec loop = (state, size, n_tests, n_discards) =>
      if (n_tests == n) {
        {tests: n_tests, discards: n_discards, status: OK};
      } else if (n_discards >= 100) {
        {tests: n_tests, discards: n_discards, status: GaveUp};
      } else {
        let (state', state'') = SplittableRandom.split(state);
        let result = RandomValues.run(state', size, as_random);

        let outcome = Tree.outcome(result) |> snd;

        switch (outcome) {
        | Failure => {
            tests: n_tests + 1,
            discards: n_discards,
            status: take_smallest(result, 0),
          }
        | Discard => loop(state'', next_size(size), n_tests, n_discards + 1)
        | Success(_) =>
          loop(state'', next_size(size), n_tests + 1, n_discards)
        };
      };

    let state = SplittableRandom.random();
    loop(state, 1, 0, 0);
  };

  // [<CompiledName("Report")>]
  // let report (p : Property<unit>) : Report =
  //     report' 100<tests> p
  let report = (p: property_t(unit)): report_t => {
    report_(100, p);
  };

  // [<CompiledName("Check")>]
  // let check' (n : int<tests>) (p : Property<unit>) : unit =
  //     report' n p
  //     |> Report.tryRaise
  let check_ = (n: tests, p: property_t(unit)) => {
    report_(n, p) |> Report.try_raise;
  };

  // [<CompiledName("Check")>]
  // let check (p : Property<unit>) : unit =
  //     report p
  //     |> Report.tryRaise
  let check = (p: property_t(unit)) => {
    report(p) |> Report.try_raise;
  };
  // TODO: not needed?
  // // Overload for ease-of-use from C#
  // [<CompiledName("Check")>]
  // let checkBool (g : Property<bool>) : unit =
  //     bind g ofBool |> check
  // // Overload for ease-of-use from C#
  // [<CompiledName("Check")>]
  // let checkBool' (n : int<tests>) (g : Property<bool>) : unit =
  //     bind g ofBool |> check' n
  // FIXME: 'return'?
  // include CoolMonad.MakeMonad({
  //   type t('a) = property_t('a);
  //   let bind = bind;
  //   let return = of_bool;
  //   let fmap = map;
  // });
};

// module PropertyI
