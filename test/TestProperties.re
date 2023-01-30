// open TestReasonmlHedgehog__Framework;
open Framework;
open ReasonmlHedgehog;
open ExtraPervasives;

describe("Test lazy lists", tst => {
  open Property;

  tst.test("Test reverse property", ({expect, _}) => {
    // reverse (reverse xs) = xs, ∀xs :: [α]

    // property {
    //     let! xs = Gen.list (Range.linear 0 100) <| Gen.int (Range.constant 0 1000)
    //     return List.rev (List.rev xs) = xs
    //     }

    let size_range = Range.constant(0, 100);
    let sizes = Gen.GenIntegers.integral(size_range);

    let length_range = Range.constant(0, 100);

    let gen_lists = Gen.GenLists.seq(length_range, sizes);

    let check_reverse = xs => xs == List.rev(List.rev(xs));

    let matches = Gen.(map(check_reverse, gen_lists));

    let prop = Property.for_all(matches, Property.of_bool);

    // if (true) {
    //   raise(HedgehogException("sfdi"));
    // };
    let report = Property.report(prop);

    expect.string(Report.render(report)).toEqual(
      "+++ OK, passed 100 tests",
    );
  });

  tst.test("Test bad reverse property", ({expect, _}) => {
    // reverse (reverse xs) = xs, ∀xs :: [α]

    // property {
    //     let! xs = Gen.list (Range.linear 0 100) <| Gen.int (Range.constant 0 1000)
    //     return List.rev (List.rev xs) = xs
    //     }

    // Set rng to a preset number so the test below always shrinks to the same value
    Random.init(1);

    let size_range = Range.constant(10, 100);
    let sizes = Gen.GenIntegers.integral(size_range);

    let length_range = Range.constant(10, 20);
    let gen_lists = Gen.GenLists.seq(length_range, sizes);

    let check_reverse = xs => {
      xs == List.rev(xs);
    };

    let prop = Property.for_all(gen_lists, check_reverse %> Property.of_bool);

    let report = Property.report(prop);
    let rendered = Report.render(report);

    expect.string(rendered).toEqual(
      "*** Failed - falsifiable (after 1 test and 14 shrinks)
[11, 10, 10, 10, 10, 10, 10, 10, 10, 10]",
    );
  });
});
