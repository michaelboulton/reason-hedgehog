open LazyList;

/*
 This would be a bit nicer but it breaks syntax highlighting
 type lazy_tree_t('a) = LazyList.t(t('a))
 and t('a) =
   | Node('a, LazyList.t(t('a)));
 */

type shrinks('a) = LazyList.t(t('a))
and t('a) =
  | Node('a, shrinks('a));

/**
  * Cool
 */
let outcome =
  fun
  | Node(x, _) => x;

let shrinks =
  fun
  | Node(_, xs) => xs;

let singleton = (value): t('a) => Node(value, empty);

let rec map = (func: 'a => 'b, tree: t('a)) => {
  Node(
    outcome(tree) |> func,
    shrinks(tree) |> LazyList.map(map(func, _), _),
  );
};

let rec bind = (tree: t('a), func: 'a => t('b)): t('b) => {
  switch (tree) {
  | Node(x, xs) =>
    switch (func(x)) {
    | Node(y, ys) =>
      let new_xs = LazyList.map(m => bind(m, func), xs);
      Node(y, LazyList.append(new_xs, ys));
    }
  };
};

// ???
// let join (xss : Tree<Tree<'a>>) : Tree<'a> =
//     bind xss id
// let join = (xss: t(t('a))): t('a) => {
//   bind(x => x, xss);
// };

let fold = (func: ('a, 'x) => 'b, fold_lazy_list: LazyList.t('b) => 'x) => {
  let rec fold_tree = (tree: t('a)): 'b => {
    func(outcome(tree), fold_shrinks(shrinks(tree)));
  }
  and fold_shrinks = (shrinks: LazyList.t('b)): 'x => {
    LazyList.map(fold_tree(_), shrinks) |> fold_lazy_list;
  };

  fold_tree;
};

let rec unfold =
        (func: 'b => 'a, unfold_lazy_list: 'b => LazyList.t('b), x: 'b) => {
  Node(func(x), unfold_shrinks(func, unfold_lazy_list, x));
}
and unfold_shrinks =
    (func: 'b => 'a, unfold_lazy_list: 'b => LazyList.t('b), xs: 'b) => {
  unfold_lazy_list(xs) |> LazyList.map(unfold(func, unfold_lazy_list, _), _);
};
// let unfold = (func: 'b => 'a, unfold_lazy_list: 'b => LazyList.t('b)) => {
//   let rec unfold_tree = (x: 'b) => {
//     Node(func(x), unfold_shrinks(x));
//   }
//   and unfold_shrinks = (xs: 'b) => {
//     unfold_lazy_list(xs) |> LazyList.map(unfold_tree, _);
//   };
//   unfold_tree;
// };

let filter = (func: 'a => bool) => {
  let filter_by_outcome = (a: t('a)): bool => outcome(a) |> func;

  let rec filter_tree =
    fun
    | Node(x, xs) => Node(x, filter_forest(xs))
  and filter_forest = (ys: shrinks('a)) => {
    LazyList.filter(filter_by_outcome, ys) |> LazyList.map(filter_tree);
  };

  filter_tree;
};

// Recurses infinitely
let rec expand = (func: 'a => LazyList.t('a)) =>
  fun
  | Node(x, xs) => {
      let ys = LazyList.map(expand(func, _), xs);
      let zs = unfold_shrinks(x => x, func, x);

      let expanded = LazyList.append(ys, zs);

      Node(x, expanded);
    };

/************* Utilities */

type forced_tree_t('a) =
  | FNode(forced_t('a))
and forced_t('a) = {
  outcome: 'a,
  shrinks: list(forced_tree_t('a)),
};

let rec force_tree =
  fun
  | Node(x, xs) => FNode({outcome: x, shrinks: force_shrinks(xs)})
and force_shrinks = shrinks => {
  LazyList.forceall(shrinks) |> List.map(force_tree, _);
};

module TreeMonad =
  CoolMonad.MakeMonad({
    type nonrec t('a) = t('a);
    let bind = bind;
    let return = singleton;
    let fmap = map;
  });
