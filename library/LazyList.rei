type t(+'a);

let empty: t('a);

let singleton: 'a => t('a);

let map: ('a => 'b, t('a)) => t('b);

let append: (t('a), t('a)) => t('a);

let fold: (('b, 'a) => 'b, 'b, t('a)) => Lazy.t('b);

let unfold: ('a => option(('a, 'a)), 'a) => t('a);

let replicate: ('a, int) => t('a);

let filter: ('a => bool, t('a)) => t('a);

let concat: t(t('a)) => t('a);

let concat_map: ('a => t('b), t('a)) => t('b);

let cons_nub: ('a, t('a)) => t('a);

let take: (int, t('a)) => t('a);

let delayed: (unit => t('a)) => t('a);

let try_find: ('a => bool, t('a)) => lazy_t(option('a));

/************* Utilities */

let forceall: t('a) => list('a);

let forceall_array: t('a) => array('a);

let tolazy: list('a) => t('a);
