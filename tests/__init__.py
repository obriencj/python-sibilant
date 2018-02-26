# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see
# <http://www.gnu.org/licenses/>.


from functools import partial

from sibilant.module import (
    fake_module_from_env, init_module, load_module_1, run_time,
)

from sibilant.parse import source_str


class Object(object):
    pass


def compile_expr(src_str, builtins=None, **base):
    mod = fake_module_from_env(base)
    init_module(mod, source_str(src_str, "<unittest>"), builtins)

    partial_run_time = partial(partial, run_time)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


def compile_expr_no_tco(src_str, **base):
    mod = fake_module_from_env(base)

    params = {"tco_enabled": False}

    init_module(mod, source_str(src_str, "<unittest>"), None,
                compiler_factory_params=params)

    partial_run_time = partial(partial, run_time)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


def compile_dis_expr(src_str, **base):
    mod = fake_module_from_env(base)
    init_module(mod, source_str(src_str, "<unittest>"), None)

    code_objs = []
    def partial_run_time(module, code_obj):

        dis.show_code(code_obj)
        print("Disassembly:")
        dis.dis(code_obj)

        code_objs.append(code_obj)
        return partial(run_time, module, code_obj)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


def make_accumulator():
    accu = list()

    def accumulate(x):
        accu.append(x)
        return x

    return accu, accumulate


def make_raise_accumulator(excclass=Exception):
    accu = list()

    def accumulate(x):
        accu.append(x)
        raise excclass(x)

    return accu, accumulate


def make_manager():
    accumulator = list()

    def accu(val):
        accumulator.append(val)
        return val

    class Manager():
        def __init__(self, initial, enter, leave):
            self.enter = enter
            self.leave = leave
            accumulator.append(initial)

        def __enter__(self):
            accumulator.append(self.enter)
            return accu

        def __exit__(self, _a, _b, _c):
            accumulator.append(self.leave)
            return True

    return accumulator, Manager


#
# The end.
