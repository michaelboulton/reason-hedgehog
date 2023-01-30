open Framework;
open ReasonmlHedgehog;

describe("Test shrinking functions", tst => {
  open Shrink;
  open LazyList;

  tst.test("Test create shrinks", ({expect, _}) => {
    let n = 3;
    let xs = [1, 2, 3, 4, 5, 6, 7];

    let result = ShrinkList.removes(n, xs);

    let forced = forceall(result);

    expect.list(forced).toEqual([[1, 2, 3, 7], [4, 5, 6, 7]]);
  });

  tst.test("Test create no elements from shrink", ({expect, _}) => {
    let n = 3;
    let xs = [1, 2];

    let result = ShrinkList.removes(n, xs);

    let forced = forceall(result);

    // No way to remove 3 elements from a 2 element list
    expect.list(forced).toEqual([]);
  });

  tst.test("Test create one element from shrink", ({expect, _}) => {
    let n = 3;
    let xs = [1, 2, 3, 4];

    let result = ShrinkList.removes(n, xs);

    let forced = forceall(result);

    expect.list(forced).toEqual([[4]]);
  });

  tst.test("Test create empty list from shrink", ({expect, _}) => {
    let n = 3;
    let xs = [1, 2, 3];

    let result = ShrinkList.removes(n, xs);

    let forced = forceall(result);

    expect.list(forced).toEqual([[]]);
  });

  tst.test("Test shrink list", ({expect, _}) => {
    let xs = ["a", "b", "c", "d"];

    let result = ShrinkList.shrink(xs);

    let forced = forceall(result);

    let expected = [
      [],
      ["a", "b"],
      ["a", "b", "c"],
      ["c", "d"],
      ["b", "c", "d"],
      ["a", "c", "d"],
      ["a", "b", "d"],
    ];

    expect.list(forced).toEqual(expected);
  });

  tst.test("Test halves", ({expect, _}) => {
    let actual = ShrinkInt.halves(100) |> LazyList.forceall;
    let expected = [100, 50, 25, 12, 6, 3, 1];

    expect.list(actual).toEqual(expected);
  });

  // TODO: move arrays and lists into common code
  tst.test("Test shrink array", ({expect, _}) => {
    let xs = [|"a", "b", "c", "d"|];

    let result = ShrinkArray.shrink(xs);

    let forced = forceall(result);

    let expected = [
      [||],
      [|"a", "b"|],
      [|"a", "b", "c"|],
      [|"c", "d"|],
      [|"b", "c", "d"|],
      [|"a", "c", "d"|],
      [|"a", "b", "d"|],
    ];

    expect.list(forced).toEqual(expected);
  });

  tst.test("Test Shrinking ints with no shrink", ({expect, _}) => {
    let from = 100;
    let to_ = 100;

    let shrinks = ShrinkInt.shrink(from, to_);

    let first_5 = take(4, shrinks);
    let forced = forceall(first_5);

    expect.list(forced).toEqual([]);
  });

  tst.test("Test Shrinking ints", ({expect, _}) => {
    let from = 0;
    let to_ = 100;

    let shrinks = ShrinkInt.shrink(from, to_);

    let first_5 = take(5, shrinks);
    let forced = forceall(first_5);

    expect.list(forced).toEqual([0, 50, 75, 88, 94]);
  });

  tst.test("Test Shrinking floats with no shrink", ({expect, _}) => {
    let from = 100.0;
    let to_ = 100.0;

    let shrinks = ShrinkFloat.shrink(from, to_);

    let first_5 = take(4, shrinks);
    let forced = forceall(first_5);

    expect.list(forced).toEqual([]);
  });

  tst.test("Test Shrinking floats", ({expect, _}) => {
    let from = 0.0;
    let to_ = 100.0;

    let shrinks = ShrinkFloat.shrink(from, to_);

    let first_5 = take(5, shrinks);
    let forced = forceall(first_5);

    expect.list(forced).toEqual([0.0, 50.0, 75.0, 87.5, 93.75]);
  });
});
