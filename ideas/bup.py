
from sibilant import cons, nil, is_pair, is_proper


def bup(*items):
    if not items:
        return nil

    coll = list()
    colle = coll.extend

    for item in items:
        if item is nil:
            pass
        elif is_pair(item):
            colle(item.unpack())
        else:
            colle(item)

    if not coll:
        return nil

    elif not is_pair(item):
        coll.append(nil)

    elif is_proper(item):
        coll.append(nil)

    return cons(*coll)


# in essence, bup combines lists and pairs into a new pair list. If
# the last item is proper, then the result is also proper. If the
# whole thing is empty, then it's nil

assert bup([1, 2, 3]) == cons(1, 2, 3, nil)
assert bup([1, 2, 3], []) == cons(1, 2, 3, nil)
assert bup([1, 2, 3], [4]) == cons(1, 2, 3, 4, nil)
assert bup([1, 2, 3], [], [4]) == cons(1, 2, 3, 4, nil)
assert bup([1, 2, 3], [], [1, 2]) == cons(1, 2, 3, 1, 2, nil)
assert bup(cons(1, nil), nil, [1, 2]) == cons(1, 1, 2, nil)
assert bup([1, 2, 3], nil) == cons(1, 2, 3, nil)
assert bup([1, 2, 3], [nil]) == cons(1, 2, 3, nil, nil)
assert bup([1, 2, 3], [], nil) == cons(1, 2, 3, nil)
assert bup([1, 2, 3], [nil], nil) == cons(1, 2, 3, nil, nil)
assert bup([1, 2, 3], [], cons(4, nil)) == cons(1, 2, 3, 4, nil)
assert bup([1]) == cons(1, nil)

# only way to get an improper result is if the last argument is an
# improper cons list. Non-pair sequences are considered proper.
assert bup([1, 2, 3], [], cons(1, 2)) == cons(1, 2, 3, 1, 2)
assert bup(cons(1, 2)) == cons(1, 2)

assert bup(cons(1, 2, nil), nil) == cons(1, 2, nil)
assert bup(cons(1, 2), nil) == cons(1, 2, nil)
assert bup(nil, cons(1, 2)) == cons(1, 2)

# these are nil
assert bup([], nil) == nil
assert bup(nil, []) == nil

# bup behaves a lot like cons in this case.
