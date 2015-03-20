

# TODO: supporting methods and bound methods with this type of
# marking may require a metaclass to decorate the bound methods as they
# are created. The following is too naive.

# def k_method_wrap(meth):
#     if getattr(meth, "__k_style", False):
#         return meth
#
#     else:
#         wrapped = getattr(meth, "__k_adapt", None)
#         if not wrapped:
#             def wrapped(self, k, *p, **kw):
#                 return k(func(self, *p, **kw))
#             wrapped.__k_style = True
#             wraps(wrapped, func)
#         return wrapped


# def k_method_adapt(wrapped=None):
#     def decorator(func):
#         func.__k_adapt = wrapped if wrapped else k_method_wrap(func)
#         return func
#
#     return decorator


#
# The end.
