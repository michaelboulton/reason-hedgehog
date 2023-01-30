type shrinks('a) = LazyList.t(t('a))
and t('a) =
  | Node('a, shrinks('a));

let outcome: t('a) => 'a;

let shrinks: t('a) => shrinks('a);

let singleton: 'a => t('a);

let map: ('a => 'b, t('a)) => t('b);

let bind: (t('a), 'a => t('b)) => t('b);

// /// Fold over a tree.
// let rec fold (f : 'a -> 'x -> 'b) (g : LazyList<'b> -> 'x) (Node (x, xs) : Tree<'a>) : 'b =
//     f x (foldForest f g xs)
// /// Fold over a list of trees.
// and foldForest (f : 'a -> 'x -> 'b) (g : LazyList<'b> -> 'x) (xs : LazyList<Tree<'a>>) : 'x =
//     LazyList.map (fold f g) xs |> g
let fold: (('a, 'x) => t('a), LazyList.t(t('a)) => 'x, t('a)) => t('a);

// /// Build a tree from an unfolding function and a seed value.
// let rec unfold (f : 'b -> 'a) (g : 'b -> LazyList<'b>) (x : 'b) : Tree<'a> =
//     Node (f x, unfoldForest f g x)
// /// Build a list of trees from an unfolding function and a seed value.
// and unfoldForest (f : 'b -> 'a) (g : 'b -> LazyList<'b>) (x : 'b) : LazyList<Tree<'a>> =
//     g x |> LazyList.map (unfold f g)
let unfold: ('b => 'a, 'b => LazyList.t('b), 'b) => t('a);

// /// Recursively discard any shrinks whose outcome does not pass the predicate.
// /// Note that the root outcome can never be discarded.
// let rec filter (f : 'a -> bool) (Node (x, xs) : Tree<'a>) : Tree<'a> =
//     Node (x, filterForest f xs)
// /// Recursively discard any trees whose outcome does not pass the predicate.
// and filterForest (f : 'a -> bool) (xs : LazyList<Tree<'a>>) : LazyList<Tree<'a>> =
//     LazyList.filter (f << outcome) xs
//     |> LazyList.map (filter f)
let filter: ('a => bool, t('a)) => t('a);

// /// Apply an additional unfolding function to an existing tree.
// ///
// /// The root outcome remains intact, only the shrinks are affected, this
// /// applies recursively, so shrinks can only ever be added using this
// /// function.
// ///
// /// If you want to replace the shrinks altogether, try:
// ///
// /// Tree.unfold f (outcome oldTree)
// ///
// let rec expand (f : 'a -> LazyList<'a>) (Node (x, xs) : Tree<'a>) : Tree<'a> =
//     //
//     // Ideally we could put the 'unfoldForest' nodes before the 'map expandTree'
//     // nodes, so that we're culling from the top down and we would be able to
//     // terminate our search faster, but this prevents minimal shrinking.
//     //
//     // We'd need some kind of tree transpose to do this properly.
//     //
//     let ys = LazyList.map (expand f) xs
//     let zs = unfoldForest id f x
//     Node (x, LazyList.append ys zs)
let expand: ('a => LazyList.t('a), t('a)) => t('a);

module TreeMonad: CoolMonad.Monad with type t('a) = t('a);
