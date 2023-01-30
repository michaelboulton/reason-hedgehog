type random_tree_t('a) = RandomValues.t(Tree.t('a));
type t('a) =
  | Gen(random_tree_t('a));

// [<CompiledName("OfRandom")>]
// let ofRandom (r : Random<Tree<'a>>) : Gen<'a> =
//     Gen r
let of_random: random_tree_t('a) => t('a);

// [<CompiledName("ToRandom")>]
// let toRandom (Gen r : Gen<'a>) : Random<Tree<'a>> =
//     r
let to_random: t('a) => random_tree_t('a);
let delay: (unit => t('a)) => t('a);
let create: ('a => LazyList.t('a), RandomValues.t('a)) => t('a);
let constant: 'a => t('a);

// let private bindRandom (m : Random<Tree<'a>>) (k : 'a -> Random<Tree<'b>>) : Random<Tree<'b>> =
//     Random <| fun seed0 size ->
//       let seed1, seed2 =
//           Seed.split seed0
//       let run (seed : Seed) (random : Random<'x>) : 'x =
//           Random.run seed size random
//       Tree.bind (run seed1 m) (run seed2 << k)
let bind_random:
  (random_tree_t('a), 'a => random_tree_t('b)) => random_tree_t('b);

// [<CompiledName("MapRandom")>]
// let mapRandom (f : Random<Tree<'a>> -> Random<Tree<'b>>) (g : Gen<'a>) : Gen<'b> =
//     toRandom g |> f |> ofRandom
let map_random: (random_tree_t('a) => random_tree_t('b), t('a)) => t('b);

// [<CompiledName("MapTree")>]
// let mapTree (f : Tree<'a> -> Tree<'b>) (g : Gen<'a>) : Gen<'b> =
//     mapRandom (Random.map f) g
let map_tree: (Tree.t('a) => Tree.t('b), t('a)) => t('b);

// [<CompiledName("Bind")>]
// let bind (m0 : Gen<'a>) (k0 : 'a -> Gen<'b>) : Gen<'b> =
//     bindRandom (toRandom m0) (toRandom << k0) |> ofRandom
let bind: (t('a), 'a => t('b)) => t('b);

// [<CompiledName("Map")>]
// let map (f : 'a -> 'b) (g : Gen<'a>) : Gen<'b> =
//     mapTree (Tree.map f) g
let map: ('a => 'b, t('a)) => t('b);

let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);

// [<CompiledName("Zip")>]
// let zip (gx : Gen<'a>) (gy : Gen<'b>) : Gen<'a * 'b> =
//     map2 (fun x y -> x, y) gx gy
let zip: (t('a), t('b)) => t(('a, 'b));

let tuple: t('a) => t(('a, 'a));

module ShrinkCombinators: {
  // //
  // // Combinators - Shrinking
  // //

  // /// Prevent a 'Gen' from shrinking.
  // [<CompiledName("NoShrink")>]
  // let noShrink (g : Gen<'a>) : Gen<'a> =
  //     let drop (Node (x, _)) =
  //         Node (x, LazyList.empty)
  //     mapTree drop g
  let no_shrink: t('a) => t('a);

  // /// Apply an additional shrinker to all generated trees.
  // [<CompiledName("ShrinkLazy")>]
  // let shrinkLazy (f : 'a -> LazyList<'a>) (g : Gen<'a>) : Gen<'a> =
  //     mapTree (Tree.expand f) g
  let shrink_lazy: ('a => LazyList.t('a), t('a)) => t('a);

  // /// Apply an additional shrinker to all generated trees.
  // [<CompiledName("Shrink")>]
  // let shrink (f : 'a -> List<'a>) (g : Gen<'a>) : Gen<'a>  =
  //     shrinkLazy (LazyList.ofList << f) g
  let shrink: ('a => list('a), t('a)) => t('a);
};

//
// Combinators - Size
//

/// Used to construct generators that depend on the size parameter.
// [<CompiledName("Sized")>]
// let sized (f : Size -> Gen<'a>) : Gen<'a> =
//     Random.sized (toRandom << f) |> ofRandom
let sized: (Range.size_t => t('a)) => t('a);

/// Overrides the size parameter. Returns a generator which uses the
/// given size instead of the runtime-size parameter.
// [<CompiledName("Resize")>]
// let resize (n : int) (g : Gen<'a>) : Gen<'a> =
//     mapRandom (Random.resize n) g
let resize: (Range.size_t, t('a)) => t('a);

/// Adjust the size parameter, by transforming it with the given
/// function.
// [<CompiledName("Scale")>]
// let scale (f : int -> int) (g : Gen<'a>) : Gen<'a> =
//     sized <| fun n ->
//         resize (f n) g
let scale: (Range.size_t => Range.size_t, t('a)) => t('a);

module type GenNumber = {
  type gt;
  let integral: Range.t(gt) => t(gt);
};
module MakeGenNumber: (B: Numeric.Number) => GenNumber with type gt = B.t;

module GenIntegers: GenNumber with type gt = int;
module GenBigints: GenNumber with type gt = Int64.t;
module GenFloats: GenNumber with type gt = float;

exception Invalid_argument(string);
let throw_empty: string => 'a;

module type GenSequences = {
  type st('a);

  /// Randomly selects one of the values in the list.
  /// <i>The input list must be non-empty.</i>
  // [<CompiledName("Item")>]
  // let item (xs0 : seq<'a>) : Gen<'a> = gen {
  //     let xs = Array.ofSeq xs0
  //     if Array.isEmpty xs then
  //         return crashEmpty "xs"
  //     else
  //         let! ix = integral <| Range.constant 0 (Array.length xs - 1)
  //         return Array.item ix xs
  // }
  let item: st('a) => t('a);

  // /// Uses a weighted distribution to randomly select one of the gens in the list.
  // /// <i>The input list must be non-empty.</i>
  // [<CompiledName("Frequency")>]
  // let frequency (xs0 : seq<int * Gen<'a>>) : Gen<'a> = gen {
  //     let xs =
  //         List.ofSeq xs0
  //     let total =
  //         List.sum (List.map fst xs)
  //     let rec pick n = function
  //         | [] ->
  //             crashEmpty "xs"
  //         | (k, y) :: ys ->
  //             if n <= k then
  //                 y
  //             else
  //                 pick (n - k) ys
  //     let! n = integral <| Range.constant 1 total
  //     return! pick n xs
  // }
  let frequency: st((Numeric.NumberInt.t, 'a)) => t('a);

  // /// Randomly selects one of the gens in the list.
  // /// <i>The input list must be non-empty.</i>
  // [<CompiledName("Choice")>]
  // let choice (xs0 : seq<Gen<'a>>) : Gen<'a> = gen {
  //     let xs = Array.ofSeq xs0
  //     if Array.isEmpty xs then
  //         return crashEmpty "xs" xs
  //     else
  //         let! ix = integral <| Range.constant 0 (Array.length xs - 1)
  //         return! Array.item ix xs
  // }
  let choice: st(t('a)) => t('a);

  // /// Randomly selects from one of the gens in either the non-recursive or the
  // /// recursive list. When a selection is made from the recursive list, the size
  // /// is halved. When the size gets to one or less, selections are no longer made
  // /// from the recursive list.
  // /// <i>The first argument (i.e. the non-recursive input list) must be non-empty.</i>
  // [<CompiledName("ChoiceRecursive")>]
  // let choiceRec (nonrecs : seq<Gen<'a>>) (recs : seq<Gen<'a>>) : Gen<'a> =
  //     sized <| fun n ->
  //         if n <= 1 then
  //             choice nonrecs
  //         else
  //             let halve x = x / 2
  //             choice <| Seq.append nonrecs (Seq.map (scale halve) recs)
  let choice_rec: (st(t('a)), st(t('a))) => t('a);

  //
  // Combinators - Conditional
  //

  // /// More or less the same logic as suchThatMaybe from QuickCheck, except
  // /// modified to ensure that the shrinks also obey the predicate.
  // let private tryFilterRandom (p : 'a -> bool) (r0 : Random<Tree<'a>>) : Random<Option<Tree<'a>>> =
  //     let rec tryN k = function
  //         | 0 ->
  //             Random.constant None
  //         | n ->
  //             let r = Random.resize (2 * k + n) r0
  //             Random.bind r <| fun x ->
  //                 if p (Tree.outcome x) then
  //                     Tree.filter p x |> Some |> Random.constant
  //                 else
  //                     tryN (k + 1) (n - 1)
  //     Random.sized (tryN 0 << max 1)
  let try_filter_random:
    ('a => bool, RandomValues.t(Tree.t('a))) =>
    RandomValues.t(option(Tree.t('a)));

  // /// Generates a value that satisfies a predicate.
  // [<CompiledName("Filter")>]
  // let filter (p : 'a -> bool) (g : Gen<'a>) : Gen<'a> =
  //     let rec loop () =
  //         Random.bind (toRandom g |> tryFilterRandom p) <| function
  //             | None ->
  //                 Random.sized <| fun n ->
  //                     Random.resize (n + 1) (Random.delay loop)
  //             | Some x ->
  //                 Random.constant x
  //     loop ()
  //     |> ofRandom
  let filter: ('a => bool, t('a)) => t('a);

  // /// Tries to generate a value that satisfies a predicate.
  // [<CompiledName("TryFilter")>]
  // let tryFilter (p : 'a -> bool) (g : Gen<'a>) : Gen<'a option> =
  //     ofRandom << Random.bind (toRandom g |> tryFilterRandom p) <| function
  //         | None ->
  //             None |> Tree.singleton |> Random.constant
  //         | Some x ->
  //             Tree.map Some x |> Random.constant
  let try_filter: ('a => bool, t('a)) => t(option('a));

  // /// Runs an option generator until it produces a 'Some'.
  // [<CompiledName("Some")>]
  // let some (g : Gen<'a option>) : Gen<'a> =
  //     bind (filter Option.isSome g) <| function
  //     | Some x ->
  //         constant x
  //     | None ->
  //         invalidOp "internal error, unexpected None"
  let some: t(option('a)) => t('a);

  // //
  // // Combinators - Collections
  // //
  // /// Generates a 'None' part of the time.
  // [<CompiledName("Option")>]
  // let option (g : Gen<'a>) : Gen<'a option> =
  //     sized <| fun n ->
  //         frequency [
  //             2, constant None
  //             1 + n, map Some g
  //         ]
  let option: t('a) => t(t(option('a)));

  // /// Generates a list using a 'Range' to determine the length.
  // [<CompiledName("List")>]
  // let list (range : Range<int>) (g : Gen<'a>) : Gen<List<'a>> =
  //     ofRandom
  //     <| (Random.sized
  //     <| fun size -> random {
  //            let! k = Random.integral range
  //            let! xs = Random.replicate k (toRandom g)
  //            return Shrink.sequenceList xs
  //                |> Tree.filter (atLeast (Range.lowerBound size range))
  //        })
  // NOTE: We are generating just a t('a) here, not array/list/etc
  let seq: (Range.t(int), t('a)) => t(st('a));
};

module MakeGenSequences:
  (S: Sequence.Seq) => GenSequences with type st('a) = S.t('a);

module GenLists: GenSequences with type st('a) = list('a);
module GenArrays: GenSequences with type st('a) = array('a);

module GenCharacters: {
  // [<CompiledName("Char")>]
  // // Generates a random character in the specified range.
  let char: (char, char) => t(char);

  // // Generates a random digit.
  // [<CompiledName("Digit")>]
  // let digit : Gen<char> =
  //     char '0' '9'
  let digit: t(char);

  // // Generates a random lowercase character.
  // [<CompiledName("Lower")>]
  // let lower : Gen<char> =
  //     char 'a' 'z'
  let lower: t(char);

  // // Generates a random uppercase character.
  // [<CompiledName("Upper")>]
  // let upper : Gen<char> =
  //     char 'A' 'Z'
  let upper: t(char);

  // /// Generates an ASCII character: '\000'..'\127'
  // [<CompiledName("Ascii")>]
  // let ascii : Gen<char> =
  //     char '\000' '\127'
  let ascii: t(char);

  // /// Generates a Latin-1 character: '\000'..'\255'
  // [<CompiledName("Latin1")>]
  // let latin1 : Gen<char> =
  //     char '\000' '\255'
  let latin: t(char);

  // // Generates a random alpha character.
  // [<CompiledName("Alpha")>]
  // let alpha : Gen<char> =
  //     choice [lower; upper]
  let alpha: t(char);

  // [<CompiledName("AlphaNum")>]
  // // Generates a random alpha-numeric character.
  // let alphaNum : Gen<char> =
  //     choice [lower; upper; digit]
  let alphanum: t(char);

  // ------ fsharp
  // /// Generates a random string using 'Range' to determine the length and the
  // /// specified character generator.
  // [<CompiledName("String")>]
  // let string (range : Range<int>) (g : Gen<char>) : Gen<string> =
  //     sized <| fun size ->
  //         g |> array range
  //     |> map System.String
  // ------ haskell
  //   list :: MonadGen m => Range Int -> m a -> m [a]
  // list range gen =
  //   sized $ \size ->
  //     (traverse snd =<<) .
  //     ensure (atLeast $ Range.lowerBound size range) .
  //     shrink Shrink.list $ do
  //       k <- integral_ range
  //       replicateM k (freeze gen)
  let string: (Range.t(int), t(char)) => t(string);
};

module GenPrimitives: {
  //
  // Combinators - Primitives
  //

  // /// Generates a random boolean.
  // [<CompiledName("Bool")>]
  // let bool : Gen<bool> =
  //     item [false; true]

  let bool: t(bool);
  let int: Range.t(int) => t(int);
  let int64: Range.t(Int64.t) => t(Int64.t);
  let float: Range.t(float) => t(float);
};

module GenSamples: {
  //
  // Sampling
  //

  // [<CompiledName("SampleTree")>]
  // let sampleTree (size : Size) (count : int) (g : Gen<'a>) : List<Tree<'a>> =
  //     let seed = Seed.random ()
  //     toRandom g
  //     |> Random.replicate count
  //     |> Random.run seed size
  let sample_tree: (Range.size_t, int, t('a)) => list(Tree.t('a));

  // [<CompiledName("Sample")>]
  // let sample (size : Size) (count : int) (g : Gen<'a>) : List<'a> =
  //     sampleTree size count g
  //     |> List.map Tree.outcome
  let sample: (Range.size_t, int, t('a)) => list('a);

  // /// Run a generator. The size passed to the generator is always 30;
  // /// if you want another size then you should explicitly use 'resize'.
  // [<CompiledName("GenerateTree")>]
  // let generateTree (g : Gen<'a>) : Tree<'a> =
  //     let seed = Seed.random ()
  //     toRandom g
  //     |> Random.run seed 30
  let generate_tree: t('a) => Tree.t('a);

  // [<CompiledName("PrintSample")>]
  // let printSample (g : Gen<'a>) : unit =
  //     let forest = sampleTree 10 5 g
  //     for tree in forest do
  //         printfn "=== Outcome ==="
  //         printfn "%A" <| Tree.outcome tree
  //         printfn "=== Shrinks ==="
  //         for shrink in Tree.shrinks tree do
  //             printfn "%A" <| Tree.outcome shrink
  //         printfn "."
  let print_sample: t('a) => unit;
};

module GenMonad: CoolMonad.Monad with type t('a) = t('a);
