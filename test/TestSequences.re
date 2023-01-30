open Framework;
open ReasonmlHedgehog;

describe("Test list functions", tst => {
  open Sequence;

  tst.test("Test split", ({expect, _}) => {
    let l = [1, 2, 3, 4];

    let (hd, tl) = ListSequence.split_at(2, l);

    expect.list(hd).toEqual([1, 2]);
    expect.list(tl).toEqual([3, 4]);
  })
});

describe("Test array functions", tst => {
  open Sequence;

  tst.test("Test split", ({expect, _}) => {
    let l = [|1, 2, 3, 4|];

    let (hd, tl) = ArraySequence.split_at(2, l);

    expect.array(hd).toEqual([|1, 2|]);
    expect.array(tl).toEqual([|3, 4|]);
  })
});
