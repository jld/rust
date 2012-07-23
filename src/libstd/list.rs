//! A standard linked list

import core::option;
import option::*;
import option::{some, none};

enum list<T> {
    cons(T, @list<T>),
    nil,
}

/// Create a list from a vector
pure fn from_vec<T: copy>(v: &[T]) -> @list<T> {
    vec::foldr(v, @nil::<T>, |h, t| @cons(h, t))
}

/// Create a list from a vector in reverse order
pure fn from_vec_rev<T: copy>(v: &[T]) -> @list<T> {
    vec::foldl(@nil::<T>, v, |t, h| @cons(h, t))
}

/// Create a vector from a list
pure fn to_vec<T: copy>(l: @list<T>) -> ~[T] {
    let mut acc = ~[];
    do iter(l) |elt| {
        unchecked { vec::push(acc, elt); }
    }
}

/// Create a vector from a list in reverse order
pure fn to_vec_rev<T: copy>(l: @list<T>) -> ~[T] {
    // Copies each element twice but uses only O(sizeof(T)) temporary storage
    let v = to_vec(l);
    vec::as_mut(v, |mv| unchecked { vec::reverse(mv) })
}


/**
 * Return a vector of pointers to each `cons` in a list.  Useful for iterating
 * over a list backwards, without allocating stack frames or making
 * unnecessary copies of elements.
 */
pure fn conses<T>(l: @list<T>) -> ~[@list<T>] {
    let mut l = l, acc = ~[];
    loop {
        alt *l {
          nil { ret acc }
          cons (_, t) { 
            unchecked { vec::push(acc, l) }
            l = t
          }
        }
    }
}


/// Return a deep copy of a list
pure fn deep_copy<T: copy>(l: @list<T>) -> @list<T> {
    append(l, @nil)
}


/**
 * Left fold
 *
 * Applies `f` to `z` and the first element in the list, then applies `f` to
 * the result of the previous call and the second element, and so on,
 * returning the accumulated result.
 *
 * # Arguments
 *
 * * ls - The list to fold
 * * z - The initial value
 * * f - The function to apply
 */
pure fn foldl<T, U>(+z: T, ls: @list<U>, f: fn(T, U) -> T) -> T {
    let mut accum: T = z;
    do iter(ls) |elt| { accum = f(accum, elt);}
    accum
}

/**
 * Right fold
 *
 * Similar to left fold, but reversed: applies `f` to the last element of the
 * list and `z`, then applies `f` to the penultimate element and the result of
 * the previous call, and so on, returning the accumulated result.
 */
pure fn foldr<T, U>(ls: @list<T>, +z: U, f: fn(T, U) -> U) -> U {
    let mut accum: U = z;
    do riter(ls) |elt| { accum = f(elt, accum); }
    accum
}

/**
 * Search for an element that matches a given predicate
 *
 * Apply function `f` to each element of `v`, starting from the first.
 * When function `f` returns true then an option containing the element
 * is returned. If `f` matches no elements then none is returned.
 */
pure fn find<T: copy>(ls: @list<T>, f: fn(T) -> bool) -> option<T> {
    let mut ls = ls;
    loop {
        ls = alt *ls {
          cons(hd, tl) {
            if f(hd) { ret some(hd); }
            tl
          }
          nil { ret none; }
        }
    };
}

/// Returns true if a list contains an element with the given value
pure fn has<T>(ls: @list<T>, elt: T) -> bool {
    for each(ls) |e| {
        if e == elt { ret true; }
    }
    ret false;
}

/// Returns true if the list is empty
pure fn is_empty<T>(ls: @list<T>) -> bool {
    alt *ls {
        nil { true }
        _ { false }
    }
}

/// Returns true if the list is not empty
pure fn is_not_empty<T>(ls: @list<T>) -> bool {
    ret !is_empty(ls);
}

/// Returns the length of a list
pure fn len<T>(ls: @list<T>) -> uint {
    let mut count = 0u;
    iter(ls, |_e| count += 1u);
    count
}

/// Returns all but the first element of a list
pure fn tail<T: copy>(ls: @list<T>) -> @list<T> {
    alt *ls {
        cons(_, tl) { ret tl; }
        nil { fail ~"list empty" }
    }
}

/*
 * Returns all but the first `n` elements of a list, or
 * the empty list if there are fewer than `n`.
 */
pure fn tailn<T>(l: @list<T>, n: uint) -> @list<T> {
    let mut l = l, n = n;
    while n > 0 {
        alt *l {
          cons(_, tl) { l = tl; n -= 1 }
          nil { break }
        }
    }
    ret l;
}

/// Returns the first element of a list
pure fn head<T: copy>(ls: @list<T>) -> T {
    alt head_opt(ls) {
      some(x) { x }
      none { fail ~"head: List is empty" }
    }
}

/**
 * Returns `some(x)` where `x` is the first element of the list,
 * or `none` if the list is empty.
 */
pure fn head_opt<T: copy>(ls: @list<T>) -> option<T> {
    alt *ls {
      cons(x, _) { some(x) }
      nil { none }
    }
}

/// Returns the `n`th element of a list, or fails if the list is too short.
pure fn nth<T: copy>(l: @list<T>, n: uint) -> T {
    alt nth_opt(l, n) {
      some(x) { x }
      none { fail ~"nth: Too few elements in list" }
    }
}

/**
 * Returns `some(x)` where `x` is the `n`th element of a list,
 * or `none` if the list is too short.
 */
pure fn nth_opt<T: copy>(l: @list<T>, n: uint) -> option<T> {
    head_opt(tailn(l, n))
}

/**
 * Returns the first `n` elements of a list, or the entire list if there are
 * fewer than `n`.  In both cases the list elements are copied.
 */
pure fn headn<T: copy>(l: @list<T>, n: uint) -> @list<T> {
    let (pfx, _sfx) = split_at(l, n);
    pfx
}

/* Returns a copy of the elements from [`start`..`end`) from `l`.
 * Will return fewer elements if the list ends before `end` (or none if
 * before `start`).
 */
pure fn slice<T: copy>(l: @list<T>, start: uint, end: uint) -> @list<T> {
    assert(start <= end);
    headn(tailn(l, start), end - start)
}

/**
 * Returns a tuple of the first `n` elements of a list and the remainder of
 * that list; if the list has fewer than `n` elements, returns the entire list
 * and the empty list.  See also `headn` and `tailn`; as with `head`, the list
 * prefix is copied.
 */
pure fn split_at<T: copy>(l: @list<T>, n: uint) -> (@list<T>, @list<T>) {
    let mut pfxc = ~[], sfx = l, n = n;
    while n > 0 {
        alt *sfx {
          nil { break }
          cons(_hd, tl) {
            unchecked { vec::push(pfxc, sfx) }
            sfx = tl;
            n -= 1;
          }
        }
    }
    (vec::foldr(pfxc, @nil, |c, t| @cons(head(c), t)), sfx)
}

/// Appends one list to another; the first list's elements are copied.
pure fn append<T: copy>(l: @list<T>, m: @list<T>) -> @list<T> {
    let mut acc = m;
    do riter(l) |elt| {
        acc = @cons(elt, acc)
    } 
    acc
}

/// Map a function over a list
pure fn map<T, U>(l: @list<T>, f: fn(T) -> U) -> @list<U> {
    let mut acc = @nil;
    do riter(l) |elt| {
        acc = @cons(f(elt), acc)
    }
    acc
}

/// Push an element to the front of a list
fn push<T>(&l: list<T>, +v: T) {
    let mut ll = nil;
    ll <-> l;
    l = cons(v, @ll);
}

/// Iterate over a list
pure fn iter<T>(l: @list<T>, f: fn(T)) {
    let mut cur = l;
    loop {
        cur = alt *cur {
          cons(hd, tl) {
            f(hd);
            tl
          }
          nil { break; }
        }
    }
}

/// Iterate over a list, with option to break
pure fn each<T>(l: @list<T>, f: fn(T) -> bool) {
    let mut cur = l;
    loop {
        cur = alt *cur {
          cons(hd, tl) {
            if !f(hd) { ret; }
            tl
          }
          nil { break; }
        }
    }
}

/// Iterate over a list, backwards
pure fn riter<T>(l: @list<T>, f: fn(T)) {
    do vec::riter(conses(l)) |c| {
        alt check *c {
          cons(h, _) { f(h) }
        }
    }
}

/// Iterate over a list, backwards, with option to break
pure fn reach<T>(l: @list<T>, f: fn(T) -> bool) {
    do vec::reach(conses(l)) |c| { 
        alt check *c {
          cons(h, _) { f(h) }
        }
    }
}



#[cfg(test)]
mod tests {
    fn assert_eq<T: copy>(l1: @list<T>, l2: @list<T>) {
        #debug("assert_eq(%?, %?)", l1, l2);
        alt check (*l1, *l2) {
          (nil, nil) { }
          (cons(h1, t1), cons(h2, t2)) if h1 == h2 { assert_eq(t1, t2) }
        }
    }

    #[test]
    fn test_is_empty() {
        let empty : @list::list<int> = from_vec(~[]);
        let full1 = from_vec(~[1]);
        let full2 = from_vec(~['r', 'u']);

        assert is_empty(empty);
        assert !is_empty(full1);
        assert !is_empty(full2);

        assert !is_not_empty(empty);
        assert is_not_empty(full1);
        assert is_not_empty(full2);
    }

    #[test]
    fn test_from_vec() {
        let l = from_vec(~[0, 1, 2]);

        assert (head(l) == 0);

        let tail_l = tail(l);
        assert (head(tail_l) == 1);

        let tail_tail_l = tail(tail_l);
        assert (head(tail_tail_l) == 2);
    }

    #[test]
    fn test_from_vec_empty() {
        let empty : @list::list<int> = from_vec(~[]);
        assert (empty == @list::nil::<int>);
    }

    #[test]
    fn test_from_vec_rev() {
        let l = from_vec_rev(~[0, 1, 2]);

        assert (head(l) == 2);

        let tail_l = tail(l);
        assert (head(tail_l) == 1);

        let tail_tail_l = tail(tail_l);
        assert (head(tail_tail_l) == 0);
    }

    #[test]
    fn test_from_vec_rev_empty() {
        let empty : @list::list<int> = from_vec_rev(~[]);
        assert (empty == @list::nil::<int>);
    }

    #[test]
    fn test_reversed() {
        let l = reversed(from_vec(~[0, 1, 2]));

        assert (head(l) == 2);

        let tail_l = tail(l);
        assert (head(tail_l) == 1);

        let tail_tail_l = tail(tail_l);
        assert (head(tail_tail_l) == 0);
    }

    #[test]
    fn test_foldl() {
        fn add(&&a: uint, &&b: int) -> uint { ret a + (b as uint); }
        let l = from_vec(~[0, 1, 2, 3, 4]);
        let empty = @list::nil::<int>;
        assert (list::foldl(0u, l, add) == 10u);
        assert (list::foldl(0u, empty, add) == 0u);
    }

    #[test]
    fn test_foldl2() {
        fn sub(&&a: int, &&b: int) -> int {
            a - b
        }
        let l = from_vec(~[1, 2, 3, 4]);
        assert (list::foldl(0, l, sub) == -10);
    }

    #[test]
    fn test_find_success() {
        fn match(&&i: int) -> bool { ret i == 2; }
        let l = from_vec(~[0, 1, 2]);
        assert (list::find(l, match) == option::some(2));
    }

    #[test]
    fn test_find_fail() {
        fn match(&&_i: int) -> bool { ret false; }
        let l = from_vec(~[0, 1, 2]);
        let empty = @list::nil::<int>;
        assert (list::find(l, match) == option::none::<int>);
        assert (list::find(empty, match) == option::none::<int>);
    }

    #[test]
    fn test_has() {
        let l = from_vec(~[5, 8, 6]);
        let empty = @list::nil::<int>;
        assert (list::has(l, 5));
        assert (!list::has(l, 7));
        assert (list::has(l, 8));
        assert (!list::has(empty, 5));
    }

    #[test]
    fn test_len() {
        let l = from_vec(~[0, 1, 2]);
        let empty = @list::nil::<int>;
        assert (list::len(l) == 3u);
        assert (list::len(empty) == 0u);
    }

    #[test]
    fn test_head() {
        let l = from_vec(~[0, 1, 2]);
        assert(head(l) == 0);
    }

    #[test]
    fn test_head_opt() {
        let l = from_vec(~[0, 1, 2]);
        let empty = @nil::<int>;
        assert(head_opt(l) == some(0));
        assert(head_opt(empty) == none);
    }

    #[test]
    fn test_nth() {
        let l = from_vec(~[4, 5, 6]);
        assert(nth(l, 0) == 4);
        assert(nth(l, 1) == 5);
        assert(nth(l, 2) == 6);
    }

    #[test]
    fn test_nth_opt() {
        let l = from_vec(~[4, 5, 6]);
        let empty = @nil::<int>;
        assert(nth_opt(l, 0) == some(4));
        assert(nth_opt(l, 1) == some(5));
        assert(nth_opt(l, 2) == some(6));
        assert(nth_opt(l, 3) == none);
        assert(nth_opt(empty, 0) == none);
        assert(nth_opt(empty, 23) == none);
    }

    #[test]
    fn test_headn() {
        let l = from_vec(~[4, 5, 6]);
        let empty = @nil::<int>;
        assert_eq(headn(l, 0), from_vec(~[]));
        assert_eq(headn(l, 1), from_vec(~[4]));
        assert_eq(headn(l, 2), from_vec(~[4, 5]));
        assert_eq(headn(l, 3), from_vec(~[4, 5, 6]));
        assert_eq(headn(l, 4), from_vec(~[4, 5, 6]));
        assert_eq(headn(empty, 0), empty);
        assert_eq(headn(empty, 23), empty);
    }

    #[test]
    fn test_tailn() {
        let l = from_vec(~[4, 5, 6]);
        let empty = @nil::<int>;
        assert_eq(tailn(l, 0), from_vec(~[4, 5, 6]));
        assert_eq(tailn(l, 1), from_vec(~[5, 6]));
        assert_eq(tailn(l, 2), from_vec(~[6]));
        assert_eq(tailn(l, 3), from_vec(~[]));
        assert_eq(tailn(l, 4), from_vec(~[]));
        assert_eq(tailn(empty, 0), empty);
        assert_eq(tailn(empty, 23), empty);
    }

    #[test]
    fn test_slice() {
        // Mostly covered by headn/tailn tests, but just in case:
        let l = from_vec(~[4, 5, 6]);
        assert_eq(slice(l, 1, 2), from_vec(~[5]));
        assert_eq(slice(l, 2, 2), from_vec(~[]));
        assert_eq(slice(l, 1, 9), from_vec(~[5, 6]));
        assert_eq(slice(l, 6, 8), from_vec(~[]));
    }

    fn assert_eq2<T,U>(+ls: (@list<T>, @list<U>),
                       l2: @list<T>, l3: @list<U>) {
        let (l0, l1) <- ls;
        assert_eq(l0, l2);
        assert_eq(l1, l3);
    }

    #[test]
    fn test_split_at() {
        let l = from_vec(~[4, 5, 6]);
        let empty = @nil::<int>;
        assert_eq2(split_at(l, 0), from_vec(~[]), from_vec(~[4, 5, 6]));
        assert_eq2(split_at(l, 1), from_vec(~[4]), from_vec(~[5, 6]));
        assert_eq2(split_at(l, 2), from_vec(~[4, 5]), from_vec(~[6]));
        assert_eq2(split_at(l, 3), from_vec(~[4, 5, 6]), from_vec(~[]));
        assert_eq2(split_at(l, 4), from_vec(~[4, 5, 6]), from_vec(~[]));
        assert_eq2(split_at(empty, 0), empty, empty);
        assert_eq2(split_at(empty, 23), empty, empty);
    }
}

// Local Variables:
// mode: rust;
// fill-column: 78;
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
