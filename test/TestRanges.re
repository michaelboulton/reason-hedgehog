open Framework;
open ReasonmlHedgehog;

describe("Test ranges", tst => {
  open Range;

  // A range from -50 to 50
  let basic_range = constant_from(0, -50, 50);

  tst.test("Test origin", ({expect, _}) =>
    expect.int(origin(basic_range)).toBe(0)
  );

  tst.test("Test lb", ({expect, _}) =>
    expect.int(lower_bound(20, basic_range)).toBe(-50)
  );

  tst.test("Test lb oob", ({expect, _}) =>
    expect.int(lower_bound(-20, basic_range)).toBe(-50)
  );

  tst.test("Test ub", ({expect, _}) =>
    expect.int(upper_bound(-20, basic_range)).toBe(50)
  );

  tst.test("Test ub oob", ({expect, _}) =>
    expect.int(upper_bound(20, basic_range)).toBe(50)
  );
});

describe("Test generated ranges", tst => {
  open Range;

  tst.test("Test int range", ({expect, _}) => {
    let int_range = IntRanges.linear(0, 50);

    expect.int(origin(int_range)).toBe(0);
    expect.int(lower_bound(20, int_range)).toBe(0);
  });

  tst.test("Test float range", ({expect, _}) => {
    let int_range = FloatRanges.linear(0.0, 50.0);

    expect.float(origin(int_range)).toBeCloseTo(0.0);
    expect.float(lower_bound(20, int_range)).toBeCloseTo(0.0);
  });
});
