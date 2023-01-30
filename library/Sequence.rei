// The things it has to _implement_ - 'private'?
module type SequenceBase = {
  type t('a);
  let length: t('a) => int;
  let at: (t('a), int) => 'a;

  let map: ('a => 'b, t('a)) => t('b);
  let bind: (t('a), 'a => t('b)) => t('b);
  let pure: 'a => t('a);

  let hd: t('a) => 'a;
  let tl: t('a) => t('a);

  let mempty: t('a);
  let mappend: (t('a), t('a)) => t('a);
  let of_list: list('a) => t('a);

  let split_at: (int, t('a)) => (t('a), t('a));

  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
};

// The type that should actually be exposed - with mconcat
module type Seq = {
  include SequenceBase;

  let skip: (int, t('a)) => t('a);
  let mconcat: t(t('a)) => t('a);
  let is_empty: t('a) => bool;
};

module MakeSequence: (S: SequenceBase) => (Seq with type t('a) = S.t('a));

module ListSequence : (Seq with type t('a) = list('a));
module ArraySequence : (Seq with type t('a) = array('a));
