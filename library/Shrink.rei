module type ShrinkMin = {
  type t('a);

  // /// Produce a list containing the progressive halving of an integral.
  // let inline halves (n : ^a) : LazyList<'a> =
  //     let go x =
  //         let zero : ^a = LanguagePrimitives.GenericZero
  //         if x = zero then
  //             None
  //         else
  //             let one : ^a = LanguagePrimitives.GenericOne
  //             let two : ^a = one + one
  //             let x' = x / two
  //             Some (x, x')
  //     LazyList.unfold go n
  let halves: int => LazyList.t(int);

  // /// Shrink a list by edging towards the empty list.
  // /// Note we always try the empty list first, as that is the optimal shrink.
  // let list (xs : List<'a>) : LazyList<List<'a>> =
  //     LazyList.concatMap (fun k -> removes k xs) (halves <| List.length xs)
  let shrink: t('a) => LazyList.t(t('a));
};

module type ShrinkSeq = {
  include ShrinkMin;

  // /// Produce all permutations of removing 'k' elements from a list.
  // let removes (k0 : int) (xs0 : List<'a>) : LazyList<List<'a>> =
  //     let rec loop (k : int) (n : int) (xs : List<'a>) : LazyList<List<'a>> =
  //         let hd = List.take k xs
  //         let tl = List.skip k xs
  //         if k > n then
  //             LazyList.empty
  //         elif List.isEmpty tl then
  //             LazyList.singleton List.empty
  //         else
  //             LazyList.consDelayed tl <| fun _ ->
  //                 LazyList.map (fun x -> List.append hd x) (loop k (n - k) tl)
  //     loop k0 (List.length xs0) xs0
  let removes: (int, t('a)) => LazyList.t(t('a));

  // /// Shrink each of the elements in input list using the supplied shrinking
  // /// function.
  // let rec elems (shrink : 'a -> LazyList<'a>) (xs00 : List<'a>) : LazyList<List<'a>> =
  //     match xs00 with
  //     | [] ->
  //         LazyList.empty
  //     | x0 :: xs0 ->
  //         let ys = LazyList.map (fun x1 -> x1 :: xs0) (shrink x0)
  //         let zs = LazyList.map (fun xs1 -> x0 :: xs1) (elems shrink xs0)
  //         LazyList.append ys zs
  let shrink_elems: ('a => LazyList.t('a), t('a)) => LazyList.t(t('a));

  // /// Turn a list of trees in to a tree of lists, using the supplied function to
  // /// merge shrinking options.
  // let rec sequence (merge : List<Tree<'a>> -> LazyList<List<Tree<'a>>>) (xs : List<Tree<'a>>) : Tree<List<'a>> =
  //     let y = List.map Tree.outcome xs
  //     let ys = LazyList.map (sequence merge) (merge xs)
  //     Node (y, ys)
  let sequence:
    (t(Tree.t('a)) => LazyList.t(t(Tree.t('a))), t(Tree.t('a))) =>
    Tree.t(t('a));

  // /// Turn a list of trees in to a tree of lists, opting to shrink both the list
  // /// itself and the elements in the list during traversal.
  // let sequenceList (xs0 : List<Tree<'a>>) : Tree<List<'a>> =
  //     sequence (fun xs ->
  //         LazyList.append (list xs) (elems Tree.shrinks xs)) xs0
  let sequence_seq: t(Tree.t('a)) => Tree.t(t('a));

  // /// Turn a list of trees in to a tree of lists, opting to shrink only the
  // /// elements of the list (i.e. the size of the list will always be the same).
  // let sequenceElems (xs0 : List<Tree<'a>>) : Tree<List<'a>> =
  //     sequence (fun xs ->
  //         elems Tree.shrinks xs) xs0
  let sequence_elems: t(Tree.t('a)) => Tree.t(t('a));
};

module MakeShrinkSeq:
  (S: Sequence.Seq) => ShrinkSeq with type t('a) = S.t('a);

module ShrinkArray: ShrinkSeq with type t('a) = array('a);
module ShrinkList: ShrinkSeq with type t('a) = list('a);

module type ShrinkNumber = {
  type t;
  let halves: t => LazyList.t(t);
  /// Shrink a number by edging towards a destination.
  let shrink: (t, t) => LazyList.t(t);
};

module MakeShrinkNumber:
  (B: Numeric.Number) => ShrinkNumber with type t = B.t;

module ShrinkInt: ShrinkNumber with type t = int;

module ShrinkInt64: ShrinkNumber with type t = Int64.t;

module ShrinkFloat: ShrinkNumber with type t = float;
