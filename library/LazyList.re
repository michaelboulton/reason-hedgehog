// TODO: See if all these can be made tail recursive

// TODO: Make a 'sequnce' type out of lazy list?

type t(+'a) = Lazy.t(node('a))
and node('a) =
  | Nil
  | Cons('a, t('a));

let empty = lazy Nil;

let singleton = item => lazy (Cons(item, empty));

let rec map = (func: 'a => 'b, l) =>
  lazy (
    switch (l) {
    | lazy Nil => Nil
    | lazy (Cons(x, xs)) => Cons(func(x), map(func, xs))
    }
  );

let rec append = (item: t('a), l: t('a)): t('a) =>
  lazy (
    switch (l) {
    | lazy Nil => Lazy.force(item)
    | lazy (Cons(x, xs)) => Cons(x, append(xs, item))
    }
  );

let rec fold = (func: ('b, 'a) => 'b, acc: 'b, l: t('a)): Lazy.t('b) => {
  switch (l) {
  | lazy Nil => lazy acc
  | lazy (Cons(x, xs)) => fold(func, func(acc, x), xs)
  };
};

// 'func' takes an element and either returns None to say that
// the unfolding is done, or returns a tuple in a Some where
// the first element is the current value and the second element
// is the next element to pass to the function
let rec unfold = (func: 'a => option(('a, 'a)), acc: 'a) => {
  lazy (
    switch (func(acc)) {
    | None => Nil
    | Some((this, next)) => Cons(this, unfold(func, next))
    }
  );
};

let replicate = item => {
  let rec impl = (l, times) => {
    switch (times) {
    | 0 => lazy l
    | _ => append(impl(l, times - 1), singleton(item))
    };
  };
  impl(Nil);
};

let filter = (predicate, l) => {
  let rec impl =
    fun
    | lazy Nil => Nil
    | lazy (Cons(x, xs)) =>
      predicate(x) ? Cons(x, lazy (impl(xs))) : impl(xs);

  lazy (impl(l));
};

// NOTE: Order of items output is not defined well
let rec concat =
  fun
  | lazy Nil => empty
  | lazy (Cons(x, xs)) => append(concat(xs), x);

let concat_map = (func: 'a => t('b), l: t('a)) => {
  map(func, l) |> concat;
};

// Adds alement to front of list unless it's already there
let cons_nub = (y, l) => {
  switch (l) {
  | lazy Nil => singleton(y)
  | lazy (Cons(x, _)) =>
    if (x == y) {
      l;
    } else {
      lazy (Cons(y, l));
    }
  };
};

let take = (num, l): t('a) => {
  let rec impl = (n_to_take, remaining): t('a) =>
    if (n_to_take <= 0) {
      empty;
    } else {
      switch (remaining) {
      | lazy Nil => empty
      | lazy (Cons(x, xs)) => lazy (Cons(x, impl(n_to_take - 1, xs)))
      };
    };

  switch (num) {
  | x when x <= 0 => empty
  | _ => impl(num, l)
  };
};

// let take = (num, l): t('a) => {
//   let rec impl = (n_to_take, remaining, acc): t('a) =>
//     if (n_to_take <= 0) {
//       acc;
//     } else {
//       switch (remaining) {
//       | lazy Nil => acc
//       // | lazy (Cons(x, xs)) => impl(n_to_take - 1, xs, lazy Cons(acc, x))
//       | lazy (Cons(x, xs)) => impl(n_to_take - 1, xs, lazy Cons(acc, x))
//       };
//     };

//   switch (num) {
//   | x when x <= 0 => empty
//   | _ => impl(num, l, empty)
//   };
// };

// Takes a function that creates a lazy list and wraps it in lazy indirection
let delayed = (func: unit => t('a)) => {
  lazy (func() |> Lazy.force);
};

// let rec tryFind p s1 =
//     match getCell s1 with
//     | CellCons(a,b) -> if p a then Some a else tryFind p b
//     | CellEmpty -> None
// Try to find a variable that satisfies the predicate in the list, return None if not found
let rec try_find = (pred: 'a => bool, l: t('a)) => {
  switch (l) {
  | lazy Nil => lazy None
  | lazy (Cons(x, xs)) =>
    if (pred(x)) {
      lazy (Some(x));
    } else {
      try_find(pred, xs);
    }
  };
};

module LazyListMonad =
  CoolMonad.MakeMonad({
    type nonrec t('a) = t('a);
    let bind = (v, f) => concat_map(f, v);
    let return = singleton;
    let fmap = map;
  });

/************* Utilities */

let rec forceall = l => {
  switch (l) {
  | lazy Nil => []
  | lazy (Cons(x, xs)) => [x, ...xs |> forceall]
  };
};

let rec forceall_array = l => {
  switch (l) {
  | lazy Nil => [||]
  | lazy (Cons(x, xs)) => Array.append([|x|], forceall_array(xs))
  };
};

let rec tolazy = (l: list('a)): t('a) => {
  switch (l) {
  | [] => empty
  | [x, ...xs] => append(tolazy(xs), singleton(x))
  };
};
