

def sample_1(parent=object):
    class Sample(parent):
        def __init__(self, val=None):
            self.value = val

    return Sample

def sample_2(parent=object):
    class Sample(parent):
        def __init__(self, val=None):
            super().__init__(self)
            self.value = val

    return Sample


from dis import dis, show_code
from types import CodeType


def disassemble(obj, verbose=False):
    if verbose:
        show_code(obj)
        print("Disassembly:")
    dis(obj)



def find_code_const(obj, name):
    obj = getattr(obj, "__code__", obj)

    for c in obj.co_consts:
        if isinstance(c, CodeType):
            if c.co_name == name:
                return c
    else:
        return None


# Sample1 = sample_1(object)
Sample1_class_code = find_code_const(sample_1, "Sample")
Sample1_init_code = find_code_const(Sample1_class_code, "__init__")

# Sample2 = sample_2(object)
Sample2_class_code = find_code_const(sample_2, "Sample")
Sample2_init_code = find_code_const(Sample2_class_code, "__init__")


if __name__ == "__main__":

    print("\n === Sample1 class code ===")
    disassemble(Sample1_class_code, True)

    print("\n === Sample1 init code ===")
    disassemble(Sample1_init_code, True)

    print("\n === Sample2 class code ===")
    disassemble(Sample2_class_code, True)

    print("\n === Sample2 init code ===")
    disassemble(Sample2_init_code, True)


# yuck.
