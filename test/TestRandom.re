open Framework;
open ReasonmlHedgehog;

Random.self_init();

// TODO: Can we test this using the framework itself

describe("Test generate random integers", tst => {
  open SplittableRandom;

  let state = ref(of_int(Random.bits()));

  tst.test("Test greater than 0", ({expect, _}) => {
    let lo = 5L;
    let hi = max_int - 1 |> Int64.of_int;

    for (_ in 1 to 100) {
      let (next_state, draw) = next_int64(state^, lo, hi);
      state := next_state;

      expect.bool(draw <= hi).toBeTrue();
      expect.bool(draw >= lo).toBeTrue();
    };
  });

  tst.test("Test stradlling 0", ({expect, _}) => {
    let lo = (-1000L);
    let hi = max_int - 1 |> Int64.of_int;

    for (_ in 1 to 100) {
      let (next_state, draw) = next_int64(state^, lo, hi);
      state := next_state;

      expect.bool(draw <= hi).toBeTrue();
      expect.bool(draw >= lo).toBeTrue();
    };
  });

  tst.test("Test less than 0", ({expect, _}) => {
    let lo = (-1000L);
    let hi = (-1L);

    for (_ in 1 to 100) {
      let (next_state, draw) = next_int64(state^, lo, hi);
      state := next_state;

      expect.bool(draw <= hi).toBeTrue();
      expect.bool(draw >= lo).toBeTrue();
    };
  });
});

describe("Test Clamping float", tst => {
  open SplittableRandom;

  tst.test("Test clamps 0", ({expect, _}) => {
    let clamp_min = unit_float(0L);
    expect.float(clamp_min).toBeCloseTo(0.0);
  });

  tst.test("Test clamps 0xffff... to 1", ({expect, _}) => {
    let clamp_min = unit_float(-1L);
    expect.float(clamp_min).toBeCloseTo(1.0);
  });

  tst.test("Test clamps in between bigint", ({expect, _}) => {
    let clamp_min = unit_float(12444343345L);
    expect.bool(clamp_min >= 0.0).toBeTrue();
    expect.bool(clamp_min <= 1.0).toBeTrue();
  });
});

describe("Test generate random floats", tst => {
  open SplittableRandom;
  let state = ref(of_int(Random.bits()));

  tst.test("Test greater than 0", ({expect, _}) => {
    let lo = 5.0;
    let hi = max_float /. 1000000000.0;

    for (_ in 1 to 100) {
      let (next_state, draw) = next_double(state^, lo, hi);
      state := next_state;

      if (draw > hi || draw < lo) {
        Console.debug(draw);
      };

      expect.bool(draw < hi).toBeTrue();
      expect.bool(draw > lo).toBeTrue();
    };
  });

  tst.test("Test stradlling 0", ({expect, _}) => {
    let lo = (-100000.0);
    let hi = max_int - 1 |> Int64.of_int |> Int64.to_float;

    for (_ in 1 to 100) {
      let (next_state, draw) = next_double(state^, lo, hi);
      state := next_state;

      if (draw > hi || draw < lo) {
        Console.debug(draw);
      };

      expect.bool(draw < hi).toBeTrue();
      expect.bool(draw > lo).toBeTrue();
    };
  });

  tst.test("Test less than 0", ({expect, _}) => {
    let lo = (-100000.0);
    let hi = (-1.0);

    for (_ in 1 to 100) {
      let (next_state, draw) = next_double(state^, lo, hi);
      state := next_state;

      if (draw > hi || draw < lo) {
        Console.debug(draw);
      };

      expect.bool(draw < hi).toBeTrue();
      expect.bool(draw > lo).toBeTrue();
    };
  });
});
