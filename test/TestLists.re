open Framework;
open ReasonmlHedgehog;

describe("Test lazy lists", tst => {
  open LazyList;

  let l =
    singleton("a")
    |> append(singleton("b"))
    |> append(singleton("b"))
    |> append(singleton("b"));

  tst.test("Test basic list appending", ({expect, _}) => {
    let z = forceall(l);

    expect.list(z).toEqual(["a", "b", "b", "b"]);
  });

  tst.test("Test forcing array", ({expect, _}) => {
    let a = forceall_array(l);

    expect.array(a).toEqual([|"a", "b", "b", "b"|]);
  });

  tst.test("Test basic list mapping", ({expect, _}) => {
    let func = a => a ++ "1";

    let mapped = map(func, l);

    let z = forceall(mapped);

    expect.list(z).toEqual(["a1", "b1", "b1", "b1"]);
  });

  tst.test("Test creating a lazy list", ({expect, _}) => {
    let original = [1, 2, 3];

    let as_lazy = tolazy(original);

    let forced = forceall(as_lazy);

    expect.list(forced).toEqual(original);
  });

  tst.test("Test folding a list", ({expect, _}) => {
    let lazy_list = [1, 2, 3] |> tolazy;

    let sum = (a, b) => a + b;

    let lazy_fold = fold(sum, 0, lazy_list);

    let forced = Lazy.force(lazy_fold);

    expect.int(forced).toBe(6);
  });

  tst.test("Test unfolding integers", ({expect, _}) => {
    let half = x => {
      switch (x) {
      | 0 => None
      | _ => Some((x, x / 2))
      };
    };

    let unfolded = unfold(half, 100);

    let sum = (a, b) => a + b;

    let lazy_fold = fold(sum, 0, unfolded);

    let forced = Lazy.force(lazy_fold);

    expect.int(forced).toBe(197);
  });

  tst.test("Test unfolding strings", ({expect, _}) => {
    let char_subtract = x => {
      switch (x) {
      | 'a' => None
      | _ => Some((x, Char.(chr(code(x) - 1))))
      };
    };

    let unfolded = unfold(char_subtract, 'z');

    let sum = (acc, next) => acc ++ String.make(1, next);

    let lazy_fold = fold(sum, "", unfolded);

    let forced = Lazy.force(lazy_fold);

    expect.string(forced).toEqual("zyxwvutsrqponmlkjihgfedcb");
  });

  tst.test("Test replicating item", ({expect, _}) => {
    let lazy_rep = replicate("a", 5);

    let forced = forceall(lazy_rep);

    expect.list(forced).toEqual(["a", "a", "a", "a", "a"]);
  });

  tst.test("Test filter list", ({expect, _}) => {
    let lazy_list = [1, 2, 3, 4, 5] |> tolazy;

    let filtered = filter(x => x > 3, lazy_list);

    let forced = forceall(filtered);

    expect.list(forced).toEqual([4, 5]);
  });

  tst.test("Test concat lists", ({expect, _}) => {
    // Two lazy lists, of lists
    let lazy_rep_a = replicate("a", 5) |> singleton;
    let lazy_rep_b = replicate("b", 5) |> singleton;

    let joined = lazy_rep_a |> append(lazy_rep_b);

    let concated = concat(joined);

    let forced = forceall(concated);

    expect.list(forced).toEqual([
      "a",
      "b",
      "a",
      "b",
      "a",
      "b",
      "a",
      "b",
      "a",
      "b",
    ]);
  });

  tst.test("Test concat map lists", ({expect, _}) => {
    let lazy_rep_a = ["a", "b", "c"] |> tolazy |> singleton;

    let concatmapped = concat_map(replicate(_, 3), lazy_rep_a);

    let forced = forceall(concatmapped) |> List.map(forceall);

    expect.list(forced).toEqual([
      ["a", "b", "c"],
      ["a", "b", "c"],
      ["a", "b", "c"],
    ]);

    let forced_lazy = map(forceall, concatmapped) |> forceall;

    expect.list(forced_lazy).toEqual(forced);
  });

  tst.test("Test cons nub noop", ({expect, _}) => {
    let str_list = ["a", "b", "c", "d"];
    let lazy_list = str_list |> tolazy;

    let consed = cons_nub("a", lazy_list);

    let forced = forceall(consed);

    expect.list(forced).toEqual(str_list);
  });

  tst.test("Test cons nub", ({expect, _}) => {
    let str_list = ["a", "b", "c", "d"];
    let lazy_list = str_list |> tolazy;

    let consed = cons_nub("z", lazy_list);

    let forced = forceall(consed);

    expect.list(forced).toEqual(["z", ...str_list]);
  });

  tst.test("Test taking", ({expect, _}) => {
    let lazy_list = ["a", "b", "c", "d", "e"] |> tolazy;

    let taken = take(3, lazy_list);
    let forced = forceall(taken);

    expect.list(forced).toEqual(["a", "b", "c"]);
  });

  tst.test("Test taking none", ({expect, _}) => {
    let lazy_list = ["a", "b", "c", "d", "e"] |> tolazy;

    let taken = take(0, lazy_list);
    let forced = forceall(taken);

    expect.list(forced).toEqual([]);
  });

  tst.test("Test lazy finding", ({expect, _}) => {
    let lazy_list = ["a", "b", "c", "d", "e"] |> tolazy;

    let to_find = "d";

    let found = try_find(x => x == to_find, lazy_list) |> Lazy.force;

    expect.option(found).toBe(Some(to_find));
  });

  tst.test("Test lazy finding", ({expect, _}) => {
    let lazy_list = ["a", "b", "c", "d", "e"] |> tolazy;

    let to_find = "hh";

    let found = try_find(x => x == to_find, lazy_list) |> Lazy.force;

    expect.option(found).toBe(None);
  });
});
