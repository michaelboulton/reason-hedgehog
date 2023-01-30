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

module MakeMonad: (M: MonadI) => Monad with type t('a) = M.t('a);
