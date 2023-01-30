/*
  Each monad, or computation type, provides means, subject to Monad Laws, to
 (a) create a description of a computation that will produce (a.k.a. "return") a
 given Haskell value, and
 (b) combine (a.k.a. "bind") a computation
 description with a reaction to it, – a pure Haskell function that is set to
 receive a computation-produced value (when and if that happens) and return
 another computation description, using or dependent on that value if need be, –
 creating a description of a combined computation that will feed the original
 computation's output through the reaction while automatically taking care of the
 particulars of the computational process itself.
 */
module type MonadI = {
  type t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let return: 'a => t('a);
  let fmap: ('a => 'b, t('a)) => t('b);
};

module type Monad = {
  include MonadI;
  let (>>=): (t('a), 'a => t('b)) => t('b);
  let (>>|): (t('a), 'a => 'b) => t('b);
};

module MakeMonad = (M: MonadI) : (Monad with type t('a) = M.t('a)) => {
  type t('a) = M.t('a);

  let return = M.return;

  let bind = M.bind;
  let (>>=) = bind;

  //   let pure = return;
  //   let (<*>) = apply;

  // let fmap = (f, m) => m >>= (a => f(a) |> return);
  let fmap = M.fmap;
  let (>>|) = (m, f) => fmap(f, m);
};
