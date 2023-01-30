open ExtraPervasives;

// The things it has to _implement_ - 'private'?
module type SequenceBase = {
  type t('a);
  let length: t('a) => int;
  let at: (t('a), int) => 'a;

  let map: ('a => 'b, t('a)) => t('b);
  let bind: (t('a), 'a => t('b)) => t('b);
  let pure: 'a => t('a);

  let hd: t('a) => 'a;
  let tl: t('a) => t('a);

  let mempty: t('a);
  let mappend: (t('a), t('a)) => t('a);
  let of_list: list('a) => t('a);

  let split_at: (int, t('a)) => (t('a), t('a));

  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
};

// The type that should actually be exposed - with mconcat
module type Seq = {
  include SequenceBase;

  let skip: (int, t('a)) => t('a);
  let mconcat: t(t('a)) => t('a);
  let is_empty: t('a) => bool;
};

module MakeSequence = (S: SequenceBase) : (Seq with type t('a) = S.t('a)) => {
  // breaks syntax highlighting
  include (S: SequenceBase with type t('a) = S.t('a));

  // Generated
  let mconcat = fold_left(mappend, mempty, _);
  let is_empty = s => length(s) == 0;

  let rec skip = (n, l) => {
    switch (n, l) {
    | (i, _) when i < 0 => failwith("skip called with negative number")
    | (_, e) when e == S.mempty => l
    | (0, _) => l
    | (_, xs) => skip(n - 1, tl(xs))
    };
  };

  // FIXME: How to deal with 'shadowed' type names, even if it's the same?
  // include CoolMonad.MakeMonad(
  //           (
  //             {
  //               // type t('a) = S.t('a);
  //               let fmap = S.map;
  //               let bind = S.bind;
  //               let return = S.pure;
  //             }
  //             // : CoolMonad.Monad with type t('a) = S.t('a)
  //           ),
  //         );
};

module ListSequence =
  MakeSequence({
    type t('a) = list('a);
    let length = List.length;
    let at = List.nth;
    let map = List.map;
    let hd = List.hd;
    let tl = List.tl;
    let pure = x => [x];
    let mempty = [];
    let mappend = (@);
    let fold_left = List.fold_left;
    let of_list = id;
    let bind = (l, f) => List.map(f, l) |> List.concat;

    let split_at = (num: int, l: list('a)) => {
      let rec impl = (n_to_take: int, remaining: list('a), acc: list('a)) =>
        if (n_to_take <= 0) {
          (acc, remaining);
        } else {
          switch (remaining) {
          | [] => (acc, remaining)
          | [x, ...xs] => impl(n_to_take - 1, xs, acc @ [x])
          };
        };

      switch (num) {
      | 0 => ([], l)
      | n when List.length(l) < n => (l, [])
      | _ => impl(num, l, [])
      };
    };
  });

module ArraySequence =
  MakeSequence({
    type t('a) = array('a);
    let length = Array.length;
    let at = Array.get;
    let map = Array.map;
    let hd = x => at(x, 0);
    let tl = x => Array.sub(x, 0, length(x) - 1);
    let pure = x => [|x|];
    let mempty = [||];
    let mappend = Array.append;
    let fold_left = Array.fold_left;
    let of_list = Array.of_list;
    let bind = (l, f) => Array.map(f, l) |> Array.to_list |> Array.concat;

    let split_at = (num: int, a) => {
      switch (num) {
      | 0 => ([||], a)
      | n when Array.length(a) < n => (a, [||])
      | _ =>
        let head = Array.sub(a, 0, num);
        let tail = Array.sub(a, num, Array.length(a) - num);

        (head, tail);
      };
    };
  });
