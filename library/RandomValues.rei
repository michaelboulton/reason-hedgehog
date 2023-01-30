type t('a) =
  | Random((SplittableRandom.t, Range.size_t) => 'a);

// let run (seed : Seed) (size : Size) (r : Random<'a>) : 'a =
//     unsafeRun seed (max 1 size) r
// Generate random value with the given random state and size
let run: (SplittableRandom.t, Range.size_t, t('a)) => 'a;

// Wrap the given function in another level of indirection
let delay: (unit => t('a)) => t('a);

// 'random' Function that always returns the given value
let constant: 'a => t('a);

// let map (f : 'a -> 'b) (r : Random<'a>) : Random<'b> =
//     Random <| fun seed size ->
//         r
//         |> unsafeRun seed size
//         |> f

// Run given function on the result of the random generation
let map: ('a => 'b, t('a)) => t('b);

// let bind (r : Random<'a>) (k : 'a -> Random<'b>) : Random<'b> =
//     Random <| fun seed size ->
//         let seed1, seed2 = Seed.split seed
//         r
//         |> unsafeRun seed1 size
//         |> k
//         |> unsafeRun seed2 size

// Compose map result with new random generation step
let bind: (t('a), 'a => t('b)) => t('b);

// let replicate (times : int) (r : Random<'a>) : Random<List<'a>> =
//     Random <| fun seed0 size ->
//         let rec loop seed k acc =
//             if k <= 0 then
//                 acc
//             else
//                 let seed1, seed2 = Seed.split seed
//                 let x = unsafeRun seed1 size r
//                 loop seed2 (k - 1) (x :: acc)
//         loop seed0 times []
// Generate list of random values
let replicate: (int, t('a)) => t(list('a));

// /// Used to construct generators that depend on the size parameter.
// let sized (f : Size -> Random<'a>) : Random<'a> =
//     Random <| fun seed size ->
//         unsafeRun seed size (f size)
let sized: (Range.size_t => t('a)) => t('a);

// /// Overrides the size parameter. Returns a generator which uses the
// /// given size instead of the runtime-size parameter.
// let resize (newSize : Size) (r : Random<'a>) : Random<'a> =
//     Random <| fun seed _ ->
//       run seed newSize r
let resize: (Range.size_t, t('a)) => t('a);

module NextNumber:
  (B: Numeric.Number) =>
   {
    /// Generates a random integral number in the given inclusive range.
    // let inline integral (range : Range<'a>) : Random<'a> =
    //     Random <| fun seed size ->
    //         let (lo, hi) = Range.bounds size range
    //         let x, _ = Seed.nextBigInt (toBigInt lo) (toBigInt hi) seed
    //         fromBigInt x
    // NOTE: We generate _any_ number, not just an integral
    let next_number: Range.t(B.t) => t(B.t);
  };

module RandomMonad: CoolMonad.Monad with type t('a) = t('a);
