open LazyList;
open ExtraPervasives;

module type ShrinkMin = {
  type t('a);

  let halves: int => LazyList.t(int);
  let shrink: t('a) => LazyList.t(t('a));
};

module type ShrinkSeq = {
  include ShrinkMin;

  let removes: (int, t('a)) => LazyList.t(t('a));
  let shrink_elems: ('a => LazyList.t('a), t('a)) => LazyList.t(t('a));
  let sequence:
    (t(Tree.t('a)) => LazyList.t(t(Tree.t('a))), t(Tree.t('a))) =>
    Tree.t(t('a));
  let sequence_seq: t(Tree.t('a)) => Tree.t(t('a));
  let sequence_elems: t(Tree.t('a)) => Tree.t(t('a));
};

module MakeShrinkSeq =
       (S: Sequence.Seq)
       : (ShrinkSeq with type t('a) = S.t('a)) => {
  type t('a) = S.t('a);

  let halves = n => {
    let impl =
      fun
      | 0 => None
      | x => Some((x, x / 2));
    unfold(impl, n);
  };

  let rec removes_impl_ =
          (k: int, n: int, xs: S.t('a)): LazyList.t(S.t('a)) =>
    if (k > n) {
      empty;
    } else {
      switch (S.split_at(k, xs)) {
      | (_, e) when e == S.mempty => singleton(S.mempty)
      | (head, tail) =>
        let mapped =
          map(x => S.mappend(head, x), removes_impl_(k, n - k, tail));
        // append(mapped, singleton(tail));
        append(singleton(tail), mapped);
      };
    };

  let removes = (k0: int, xs0: S.t('a)): LazyList.t(S.t('a)) => {
    removes_impl_(k0, S.length(xs0), xs0);
  };

  let shrink = (xs: S.t('a)) => {
    let remove_perms = removes(_, xs);
    let halved_input = S.length(xs) |> halves;

    concat_map(remove_perms, halved_input);
  };

  let rec shrink_elems = (shrink: 'a => LazyList.t('a)) =>
    fun
    | e when e == S.mempty => LazyList.empty
    | s => {
        // [x, ...xs]
        let x = S.hd(s);
        let xs = S.tl(s);
        let ys = LazyList.map(y => S.mappend(S.pure(y), xs), shrink(x));
        let zs =
          LazyList.map(
            zs => S.mappend(S.pure(x), zs),
            shrink_elems(shrink, xs),
          );
        LazyList.append(ys, zs);
      };

  let rec sequence = (merge, xs) => {
    let y = S.map(Tree.outcome, xs);
    let ys = LazyList.map(sequence(merge, _), merge(xs));
    Tree.Node(y, ys);
  };

  let sequence_seq = (xs0: S.t(Tree.t('a))): Tree.t(S.t('a)) => {
    let sl = xs => {
      LazyList.append(shrink(xs), shrink_elems(Tree.shrinks, xs));
    };

    sequence(sl, xs0);
  };

  let sequence_elems = (xs0: S.t(Tree.t('a))): Tree.t(S.t('a)) => {
    let sl = xs => shrink_elems(Tree.shrinks, xs);

    sequence(sl, xs0);
  };
};

module ShrinkList = MakeShrinkSeq(Sequence.ListSequence);
module ShrinkArray = MakeShrinkSeq(Sequence.ArraySequence);

module type ShrinkNumber = {
  type t;
  let halves: t => LazyList.t(t);
  /// Shrink a number by edging towards a destination.
  let shrink: (t, t) => LazyList.t(t);
};

module MakeShrinkNumber =
       (B: Numeric.Number)
       : (ShrinkNumber with type t = B.t) => {
  type t = B.t;

  let halves = n => {
    let impl = x => {
      switch (B.to_big_int(x)) {
      | 0L => None
      | n =>
        let divided_by_2 = B.of_big_int(Int64.div(n, 2L));
        Some((x, divided_by_2));
      };
    };
    unfold(impl, n);
  };

  let shrink = (to_, from) =>
    if (from == to_) {
      empty;
    } else {
      // Note form haskell version - dividing first prevents overflow
      let diff = B.(from / of_big_int(2L) - to_ / of_big_int(2L));

      // eg shrink(100, 50) => [50, 75, 87.5, ...]
      let move_towards = LazyList.map(y => B.(from - y), halves(diff));

      // Maybe add destination, it might already be there though
      // depending on how big the operands are
      let with_destination = cons_nub(to_, move_towards);

      with_destination;
    };
};

module ShrinkInt: ShrinkNumber with type t = int = {
  type t = int;

  let halves = n => {
    let impl =
      fun
      | 0 => None
      | n => {
          let divided_by_2 = n / 2;
          Some((n, divided_by_2));
        };

    unfold(impl, n);
  };

  let shrink = (to_, from) =>
    if (from == to_) {
      empty;
    } else {
      let diff = from / 2 - to_ / 2;

      let move_towards = LazyList.map(y => from - y, halves(diff));
      let with_destination = cons_nub(to_, move_towards);

      with_destination;
    };
};

module ShrinkInt64: ShrinkNumber with type t = Int64.t = {
  module U = Int64Util.Int64Operations;

  type t = Int64.t;

  let halves = n => {
    let impl =
      fun
      | 0L => None
      | n => {
          let divided_by_2 = U.(n / 2L);
          Some((n, divided_by_2));
        };

    unfold(impl, n);
  };

  let shrink = (to_, from) =>
    if (from == to_) {
      empty;
    } else {
      let diff = U.(from / 2L - to_ / 2L);

      let move_towards = LazyList.map(y => U.(from - y), halves(diff));
      let with_destination = cons_nub(to_, move_towards);

      with_destination;
    };
};

module ShrinkFloat: ShrinkNumber with type t = float = {
  type t = float;

  let halves = _ => failwith("Not implemented for float");

  let shrink = (to_, from) =>
    // Will very rarely match
    if (from == to_) {
      empty;
    } else {
      let diff = from -. to_;

      let check = n =>
        switch (from -. n) {
        | y when is_invalid_float(y) => None
        | y => Some((y, n /. 2.0))
        };

      unfold(check, diff);
    };
};
