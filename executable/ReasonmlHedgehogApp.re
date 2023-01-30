open ReasonmlHedgehog;
open ReasonmlHedgehog__Property;

Printexc.record_backtrace(true);
Random.self_init();

let numbers_range = Range.constant(-1999999.0, 100000.0);
let sizes = Gen.GenFloats.integral(numbers_range);

let length_range = Range.constant(2, 6);
let gen_lists = Gen.GenLists.seq(length_range, sizes);

// Gen.GenSamples.print_sample(sizes);
Gen.GenSamples.print_sample(gen_lists);

let format_genned_list = Format.pp_print_list(Format.pp_print_float);
// let ffff = Gen.show_gen_t(format_genned_list, gen_lists);

/* Console.log(ffff); */

let check_reverse = xs => xs == List.rev((xs));

include Gen.GenMonad;

let matches = Gen.(gen_lists >>| check_reverse);

let prop = Property.for_all(matches, Property.of_bool);

// if (true) {
//   raise(HedgehogException("sfdi"));
// };
let report = Property.report(prop);

Console.log(report);

Report.render(report)|>print_endline;
