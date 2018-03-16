

class values(object):

    def __init__(self, *args, **kwds):
        self.__args = args
        self.__iter__ = args.__iter__

        self.__kwds = kwds
        self.keys = kwds.keys


    def __getitem__(self, key):
        if isinstance(key, (slice, int)):
            return self.__args[key]
        else:
            return self.__kwds[key]


    def __repr__(self):
        return "values(*%r, **%r)" % (self.__args, self.__kwds)


    def __call__(self, function):
        return function(*self.__args, **self.__kwds)


    def unpack(self, positionals, variadic=None, kwonly=(),
               keywords=(), defaults={}, kwvariadic=None):

        lpos = len(positionals)
        largs = len(self.__args)
        kwvar = dict(self.__kwds)

        if lpos == largs:
            args = list(self.__args)
            var = () if variadic else None
        elif lpos < largs:
            if variadic:
                args = list(self.__args[:lpos])
                var = self.__args[lpos:]
            else:
                raise TypeError("too many positional arguments")
        else:
            args = list(self.__args)
            var = () if variadic else None
            try:
                for a in positionals[largs:]:
                    args.append(kwvar.pop(a))
            except KeyError:
                raise TypeError("missing required argument %s" % a)

        kwds = {}
        try:
            for a in kwonly:
                kwds[a] = kwvar.pop(a)
        except KeyError:
            raise TypeError("missing required keyword-only argument %s" % a)

        for a in keywords:
            kwds[a] = kwvar.pop(a, defaults[a])

        if kwvar and not kwvariadic:
            raise TypeError("unexpected arguments, %r" % list(kwvar.keys()))

        return args, var, kwds, (kwvar if kwvariadic else None)


    def unpack_fun(self, function):
        code = function.__code__

        ac = code.co_argcount + code.co_kwonlyargcount
        if code.co_flags & 4:
            ac += 1
        if code.co_flags & 8:
            ac += 1

        positionals = list(code.co_varnames[:ac])
        kwvariadic = positionals.pop() if code.co_flags & 8 else None
        variadic = positionals.pop() if code.co_flags & 4 else None

        kwoac = code.co_kwonlyargcount
        if kwoac:
            kwonly = positionals[-kwoac:]
            positionals = positionals[:-kwoac]
        else:
            kwonly = ()

        defaults = function.__kwdefaults__
        if defaults:
            kwac = len(defaults)
            if kwonly:
                keywords = kwonly[-kwac:]
                kwonly = kwonly[:-kwac]
            else:
                keywords = positionals[-kwac:]
                positionals = positionals[:-kwac]
        else:
            defaults = ()
            keywords = ()

        return self.unpack(positionals, variadic, kwonly,
                           keywords, defaults, kwvariadic)


if __name__ == '__main__':

    v = values(1, 2, 3, foo=77, bar=88)
    print("v is", repr(v))


    def test_1(a, b, c, foo, **kwds):
        return [a, b, c, foo], None, {}, kwds

    print()
    print("v(test_1) :", v(test_1))
    print("test_1(*v, **v) :", test_1(*v, **v))
    print("v.unpack_fun(test_1) :", v.unpack_fun(test_1))

    def test_2(a, *rest, foo, bar, baz=None):
        return [a], rest, dict(foo=foo, bar=bar, baz=baz), None

    print()
    print("v(test_2) :", v(test_2))
    print("test_2(*v, **v) :", test_2(*v, **v))
    print("v.unpack_fun(test_2) :", v.unpack_fun(test_2))

    def test_3(a, b, c, *, foo, bar=100, baz=200, **kwds):
        return [a, b, c], None, dict(foo=foo, bar=bar, baz=baz), kwds

    print()
    print("v(test_3) :", v(test_3))
    print("test_3(*v, **v) :", test_3(*v, **v))
    print("v.unpack_fun(test_3) :", v.unpack_fun(test_3))


# The end.
