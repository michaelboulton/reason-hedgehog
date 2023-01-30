open Framework;
open ReasonmlHedgehog;

describe("Test trees", tst => {
  open Tree;
  open LazyList;

  tst.test("Test making a tree", ({expect, _}) => {
    let tree = Node(1, empty);

    let outcome = outcome(tree);
    expect.int(outcome).toBe(1);

    let shrunk = shrinks(tree);
    let evaluated = forceall(shrunk);
    expect.list(evaluated).toEqual([]);
  });

  tst.test("Test making a tree", ({expect, _}) => {
    let l1 = singleton(Node(1, empty));
    let l2 = append(singleton(Node(1, empty)), l1);
    let tree_3 = Node(1, l2);

    let value = outcome(tree_3);
    expect.int(value).toBe(1);
    // Console.log(tree_1);
    // Console.log(l2);
    // Console.log(tree_3);
    // let shrunk = shrinks(tree_3);
    // let second_value = LazyList.map(shrinks, shrunk);
    // let shrinks_2 = map(shrinks, shrunk);
    // let evaluated_2 = forceall(shrinks_2);
    // expect.list(evaluated_2).toEqual([2]);
  });
});

describe("Test tree functions", tst => {
  open Tree;

  let l = List.map(singleton, [10, 20, 30]) |> LazyList.tolazy;

  let tree = Node(1, l);

  tst.test("Test bind function", ({expect, _}) => {
    let bound = bind(tree, a => Node(a + 1, LazyList.empty));

    let value = outcome(bound);
    expect.int(value).toBe(2);
  });

  tst.test("Test map function", ({expect, _}) => {
    let mapped = map(x => x + 1, tree);

    let value = outcome(mapped);
    expect.int(value).toBe(2);

    let shrunk = LazyList.(shrinks(mapped) |> forceall);
    let shrunk_values = List.map(outcome, shrunk);

    expect.list(shrunk_values).toEqual([11, 21, 31]);
  });

  // tst.test("Test folding", ({expect, _}) => {
  //   let sum = (acc, next) => acc + next;
  //   let sum_to_tree = (acc, next) => Node(acc, lazy next);
  //   let sum_shrinks = LazyList.fold(sum, 0, _);
  //   let folded = fold(sum_to_tree, sum_shrinks, tree);
  //   ();
  // });

  /*
   tst.test("Test unfolding", ({expect, _}) => {
     let half = x => outcome(x) / 2;

     let to_option = a => {
       switch (half(a)) {
       | 0
       | 1 => None
       | n => Some((singleton(n), Node(n, LazyList.replicate(a, 4))))
       };
     };

     let gen_shrinks = LazyList.unfold(to_option, _);

     let tree = unfold(half, gen_shrinks, singleton(50));

     let shrunk = shrinks(tree);
     let evaluated = LazyList.forceall(shrunk);

     Console.log("");
     List.map(shrinks, evaluated)
     |> List.map(LazyList.forceall)
     |> Console.log;
     // expect.list(evaluated).toEqual([]);
   });
   */

  tst.test("Test filtering", ({expect, _}) => {
    let original_shrinks =
      shrinks(tree) |> LazyList.forceall |> List.map(outcome);
    expect.list(original_shrinks).toEqual([10, 20, 30]);

    let is_gt_25 =
      fun
      | x when x < 25 => false
      | _ => true;

    let filtered = filter(is_gt_25, tree);

    let new_shrinks =
      shrinks(filtered) |> LazyList.forceall |> List.map(outcome);

    expect.list(new_shrinks).toEqual([30]);
  });
});
