open Revery.UI;

open ReasonmlHedgehog.LazyList;

let l =
  singleton("a")
  |> append(singleton("b"))
  |> append(singleton("b"))
  |> append(singleton("b"));

Console.log(l |> forceall);

<View> <Text text= "sdf"/> </View>;
