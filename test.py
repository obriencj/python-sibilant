#
#

from sibilant import eval_from_str


def kfoo(k):
	print "k FOO"
	return (k, True)


def foo():
	print "FOO"
	return True


def kbar(k, b):
	print "k BAR", b
	return (k, not b)


def bar(b):
	print "BAR", b
	return not b



#src = "(if (if False (foo) (bar (foo))) 1 2)"
#src = """
#(begin
#  (foo)
#  (bar True)
#  (if (bar False)
#    1 2))
#"""
#src = """
#((lambda (W) (bar W)) (foo))
#"""
#src = """(if (and (foo) (not (not (foo)))) 88 99)"""
src = """
(begin
  ((py-fun foo))
  ((py-fun bar) (kfoo)))
"""


#eval_from_str(src, globals())


from sibilant.test import main
main()


#
# The end.
