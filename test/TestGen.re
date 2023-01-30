open Framework;
open ReasonmlHedgehog;

// TODO: custom matchers for all in list
// TODO: Also possibly integrate these into properties
describe("Test generating numbers", tst => {
  open Range;

  tst.test("Test integer range", ({expect, _}) => {
    let basic_range = constant_from(0, -50, 50);
    let sizes = Gen.GenIntegers.integral(basic_range);

    let samples = Gen.GenSamples.sample(100, 100, sizes);

    let expect_in_range = x => {
      expect.bool(x >= (-50) && x <= 50).toBeTrue();
    };

    List.iter(expect_in_range, samples);
  });

  tst.test("Test float range", ({expect, _}) => {
    let basic_range = constant_from(0.0, -50.0, 50.0);
    let sizes = Gen.GenFloats.integral(basic_range);

    let samples = Gen.GenSamples.sample(100, 100, sizes);

    let expect_in_range = x => {
      if (!(x >= (-50.0) && x <= 50.0)) {
        Console.debug(x);
      };
      expect.bool(x >= (-50.0) && x <= 50.0).toBeTrue();
    };

    List.iter(expect_in_range, samples);
  });
});

describe("Generating sequences", tst => {
  tst.test("Test Generating a list", ({expect, _}) => {
    let magnitude_range = Range.constant(10, 100);
    let gen_integers = Gen.GenIntegers.integral(magnitude_range);

    let length_range = Range.constant(10, 50);
    let gen_lists = Gen.GenLists.seq(length_range, gen_integers);

    let samples = Gen.GenSamples.sample(100, 100, gen_lists);

    let expect_in_range = l => {
      let len = List.length(l);
      expect.bool(len >= 10 && len <= 50).toBeTrue();
    };

    List.iter(expect_in_range, samples);
  });

  tst.test("Test Generating a array", ({expect, _}) => {
    let magnitude_range = Range.constant(10, 100);
    let gen_integers = Gen.GenIntegers.integral(magnitude_range);

    let length_range = Range.constant(10, 50);
    let gen_lists = Gen.GenArrays.seq(length_range, gen_integers);

    let samples = Gen.GenSamples.sample(100, 100, gen_lists);

    let expect_in_range = l => {
      let len = Array.length(l);
      expect.bool(len >= 10 && len <= 50).toBeTrue();
    };

    List.iter(expect_in_range, samples);
  });
});
