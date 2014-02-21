"""
"""


# we can replace the class supplying number functions here
numbers = float


def number_op_k(opf):
    return lambda k,*a: k(reduce(opf, a))


add_k = number_op_k(numbers.__add__)
sub_k = number_op_k(numbers.__sub__)
mult_k = number_op_k(numbers.__mult__)
divide_k = number_op_k(numbers.__div__)
mod_k = number_op_k(numbers.__mod__)
pow_k = number_op_k(numbers.__pow__)


class cons(object):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr


null = cons(None, None)

def nullp(k, list):
    return k(not (list.car or list.cdr))

def cons_k(k, item, list):
    return k(cons(item, list))

def car_k(k, list):
    return k(list.car)

def cdr_k(k, list):
    return k(list.cdr)

def list_k(k, *a):
    return k(reduce(lambda x,y: cons(y,x), a[::-1], null))


class symbol(object):
    def __init__(self, s):
        self.sym = s
        

#
# The end.
