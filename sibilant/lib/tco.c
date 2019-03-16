/*
  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation; either version 3 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, see
  <http://www.gnu.org/licenses/>.
*/


/**
   Part of sibilant.lib.ctypes

   Native implementation of the TCO trampoline and state for sibilant.

   author: Christopher O'Brien  <obriencj@gmail.com>
   license: LGPL v.3
*/


#include "types.h"

#include <cellobject.h>
#include <frameobject.h>


#define DOCSTR "Native TCO Implementation for Sibilant"


/* === util === */


static PyObject *__get__ = NULL;
static PyObject *_tco_enable = NULL;
static PyObject *_tco_original = NULL;


static Tailcall *tailcall_free_list = NULL;


static PyObject *_getattro(PyObject *inst, PyObject *name) {
  /* Like PyObject_GetAttr except without some of the checks */

  PyTypeObject *tp = Py_TYPE(inst);
  PyObject *res = NULL;

  if (tp->tp_getattro) {
    res = tp->tp_getattro(inst, name);

  } else if (tp->tp_getattr) {
    // note, per docs AsUTF8 is cached internally, no need to free
    res = tp->tp_getattr(inst, (char *) PyUnicode_AsUTF8(name));

  } else {
    return NULL;
  }

  if (! res)
    PyErr_Clear();

  return res;
}


/* === SibTailcallType === */


static PyObject *do_tc_full(PyObject *work, PyObject *args, PyObject *kwds) {

  if (SibTrampoline_Check(work)) {
    // it's a trampoline, so we'll tailcall using the original
    // function if it allows it.
    work = ((Trampoline *) work)->tco_original;
    Py_INCREF(work);

  } else {
    PyObject *tmp = _getattro(work, _tco_enable);
    if (tmp && (PyObject_IsTrue(tmp))) {
      Py_DECREF(tmp);

      tmp = _getattro(work, _tco_original);
      if (tmp) {
	work = tmp;
      } else {
	Py_INCREF(work);
      }

    } else {
      Py_XDECREF(tmp);
      return PyObject_Call(work, args, kwds);
    }
  }

  Tailcall *result = (Tailcall *) SibTailcall_New(NULL);
  result->work = work; // steal
  result->args = args; Py_INCREF(args);
  result->kwds = kwds; Py_XINCREF(kwds);

  return (PyObject *) result;
}


#define GETLOCAL(index) (fastlocals[index])

#define SETLOCAL(index, value) {		\
    PyObject *tmp = GETLOCAL(index);		\
    GETLOCAL(index) = value;			\
    Py_XDECREF(tmp);				\
  }


static void
format_missing(const char *kind, PyCodeObject *co, PyObject *names)
{
  /* directly copied from cpython, for use from apply_frame_vars */

    int err;
    Py_ssize_t len = PyList_GET_SIZE(names);
    PyObject *name_str, *comma, *tail, *tmp;

    assert(PyList_CheckExact(names));
    assert(len >= 1);
    /* Deal with the joys of natural language. */
    switch (len) {
    case 1:
        name_str = PyList_GET_ITEM(names, 0);
        Py_INCREF(name_str);
        break;
    case 2:
        name_str = PyUnicode_FromFormat("%U and %U",
                                        PyList_GET_ITEM(names, len - 2),
                                        PyList_GET_ITEM(names, len - 1));
        break;
    default:
        tail = PyUnicode_FromFormat(", %U, and %U",
                                    PyList_GET_ITEM(names, len - 2),
                                    PyList_GET_ITEM(names, len - 1));
        if (tail == NULL)
            return;
        /* Chop off the last two objects in the list. This shouldn't actually
           fail, but we can't be too careful. */
        err = PyList_SetSlice(names, len - 2, len, NULL);
        if (err == -1) {
            Py_DECREF(tail);
            return;
        }
        /* Stitch everything up into a nice comma-separated list. */
        comma = PyUnicode_FromString(", ");
        if (comma == NULL) {
            Py_DECREF(tail);
            return;
        }
        tmp = PyUnicode_Join(comma, names);
        Py_DECREF(comma);
        if (tmp == NULL) {
            Py_DECREF(tail);
            return;
        }
        name_str = PyUnicode_Concat(tmp, tail);
        Py_DECREF(tmp);
        Py_DECREF(tail);
        break;
    }
    if (name_str == NULL)
        return;
    PyErr_Format(PyExc_TypeError,
                 "%U() missing %i required %s argument%s: %U",
                 co->co_name,
                 len,
                 kind,
                 len == 1 ? "" : "s",
                 name_str);
    Py_DECREF(name_str);
}

static void
missing_arguments(PyCodeObject *co, Py_ssize_t missing, Py_ssize_t defcount,
                  PyObject **fastlocals)
{
    /* directly copied from cpython, for use from apply_frame_vars */

    Py_ssize_t i, j = 0;
    Py_ssize_t start, end;
    int positional = (defcount != -1);
    const char *kind = positional ? "positional" : "keyword-only";
    PyObject *missing_names;

    /* Compute the names of the arguments that are missing. */
    missing_names = PyList_New(missing);
    if (missing_names == NULL)
        return;
    if (positional) {
        start = 0;
        end = co->co_argcount - defcount;
    }
    else {
        start = co->co_argcount;
        end = start + co->co_kwonlyargcount;
    }
    for (i = start; i < end; i++) {
        if (GETLOCAL(i) == NULL) {
            PyObject *raw = PyTuple_GET_ITEM(co->co_varnames, i);
            PyObject *name = PyObject_Repr(raw);
            if (name == NULL) {
                Py_DECREF(missing_names);
                return;
            }
            PyList_SET_ITEM(missing_names, j++, name);
        }
    }
    assert(j == missing);
    format_missing(kind, co, missing_names);
    Py_DECREF(missing_names);
}

static void
too_many_positional(PyCodeObject *co, Py_ssize_t given, Py_ssize_t defcount,
                    PyObject **fastlocals)
{
    /* directly copied from cpython, for use from apply_frame_vars */

    int plural;
    Py_ssize_t kwonly_given = 0;
    Py_ssize_t i;
    PyObject *sig, *kwonly_sig;
    Py_ssize_t co_argcount = co->co_argcount;

    assert((co->co_flags & CO_VARARGS) == 0);
    /* Count missing keyword-only args. */
    for (i = co_argcount; i < co_argcount + co->co_kwonlyargcount; i++) {
        if (GETLOCAL(i) != NULL) {
            kwonly_given++;
        }
    }
    if (defcount) {
        Py_ssize_t atleast = co_argcount - defcount;
        plural = 1;
        sig = PyUnicode_FromFormat("from %zd to %zd", atleast, co_argcount);
    }
    else {
        plural = (co_argcount != 1);
        sig = PyUnicode_FromFormat("%zd", co_argcount);
    }
    if (sig == NULL)
        return;
    if (kwonly_given) {
        const char *format = " positional argument%s (and %zd keyword-only argument%s)";
        kwonly_sig = PyUnicode_FromFormat(format,
                                          given != 1 ? "s" : "",
                                          kwonly_given,
                                          kwonly_given != 1 ? "s" : "");
        if (kwonly_sig == NULL) {
            Py_DECREF(sig);
            return;
        }
    }
    else {
        /* This will not fail. */
        kwonly_sig = PyUnicode_FromString("");
        assert(kwonly_sig != NULL);
    }
    PyErr_Format(PyExc_TypeError,
                 "%U() takes %U positional argument%s but %zd%U %s given",
                 co->co_name,
                 sig,
                 plural ? "s" : "",
                 given,
                 kwonly_sig,
                 given == 1 && !kwonly_given ? "was" : "were");
    Py_DECREF(sig);
    Py_DECREF(kwonly_sig);
}


static int apply_frame_vars(PyFrameObject *frame,
			    PyObject *func,
			    PyObject **args, Py_ssize_t argcount,
			    PyObject *kwds_dict) {

  /* adapted from cpython, this is a way for us to reset a frame's
     local and cell vars similarly to how they would be initialized
     during function calls. */

  PyCodeObject *co = frame->f_code;
  PyObject **fastlocals = frame->f_localsplus;

  const Py_ssize_t n_locals = co->co_nlocals;
  const Py_ssize_t n_cellvars = PyTuple_GET_SIZE(co->co_cellvars);

  const Py_ssize_t total_args = co->co_argcount + co->co_kwonlyargcount;
  Py_ssize_t i, n;

  PyObject *x, *u;

  // reset the frame's locals and cell vars. free vars remain
  // untouched.
  for (i = (n_locals + n_cellvars); i--; ) {
    Py_CLEAR(fastlocals[i]);
  }

  PyObject **defs;
  Py_ssize_t defcount;

  u = PyFunction_GET_DEFAULTS(func);
  if (u && PyTuple_Check(u)) {
    defs = &PyTuple_GET_ITEM(u, 0);
    defcount = PyTuple_GET_SIZE(u);
  } else {
    defs = NULL;
    defcount = 0;
  }

  PyObject *kwdefs = PyFunction_GET_KW_DEFAULTS(func);

  /*
    the rest of this is a nearly identical copy of what CPython's
    _PyEval_EvalCodeWithName does to copy args/kwds into the frame.
    Some aspects were adopted to work with the slightly different
    structure of arguments we deal with.
  */

  // Create a dictionary for keyword parameters (**kwags)
  PyObject *kwdict;

  if (co->co_flags & CO_VARKEYWORDS) {
    kwdict = PyDict_New();
    if (kwdict == NULL) {
      goto fail;
    }
    i = total_args;
    if (co->co_flags & CO_VARARGS) {
      i++;
    }
    SETLOCAL(i, kwdict);

  } else {
    kwdict = NULL;
  }

  // Copy positional arguments into locals
  n = (argcount > co->co_argcount)? co->co_argcount: argcount;

  for (i = n; i--; ) {
    x = args[i];
    Py_INCREF(x);
    SETLOCAL(i, x);
  }

  // Pack other positional arguments into the *args argument
  if (co->co_flags & CO_VARARGS) {
    u = PyTuple_New(argcount - n);
    if (u == NULL) {
      goto fail;
    }
    SETLOCAL(total_args, u);
    for (i = n; i < argcount; i++) {
      x = args[i];
      Py_INCREF(x);
      PyTuple_SET_ITEM(u, i-n, x);
    }
  }

  // Handle keyword arguments passed as two strided arrays
  if (kwds_dict && (PyDict_Size(kwds_dict))) {
    Py_ssize_t pos = 0;
    PyObject *keyword = NULL;
    PyObject *value = NULL;

    while (PyDict_Next(kwds_dict, &pos, &keyword, &value)) {

      PyObject **co_varnames;
      Py_ssize_t j;

      if (keyword == NULL || !PyUnicode_Check(keyword)) {
	PyErr_Format(PyExc_TypeError,
		     "%U() keywords must be strings",
		     co->co_name);
	goto fail;
      }

      /* Speed hack: do raw pointer compares. As names are
	 normally interned this should almost always hit. */
      co_varnames = ((PyTupleObject *)(co->co_varnames))->ob_item;
      for (j = total_args; j--; ) {
	PyObject *name = co_varnames[j];
	if (name == keyword) {
	  goto kw_found;
	}
      }

      /* Slow fallback, just in case */
      for (j = total_args; j--; ) {
	PyObject *name = co_varnames[j];
	int cmp = PyObject_RichCompareBool(keyword, name, Py_EQ);
	if (cmp > 0) {
	  goto kw_found;
	} else if (cmp < 0) {
	  goto fail;
	}
      }

      if (kwdict == NULL) {
	PyErr_Format(PyExc_TypeError,
		     "%U() got an unexpected keyword argument '%S'",
		     co->co_name, keyword);
	goto fail;

      } else if (PyDict_SetItem(kwdict, keyword, value) == -1) {
	goto fail;

      } else {
	continue;
      }

    kw_found:
      if (GETLOCAL(j)) {
	PyErr_Format(PyExc_TypeError,
		     "%U() got multiple values for argument '%S'",
		     co->co_name, keyword);
	goto fail;
      }

      Py_INCREF(value);
      SETLOCAL(j, value);
    }
  }

  // Check the number of positional arguments
  if (argcount > co->co_argcount && !(co->co_flags & CO_VARARGS)) {
    too_many_positional(co, argcount, defcount, fastlocals);
    goto fail;
  }

  // Add missing positional arguments (copy default values from defs)
  if (argcount < co->co_argcount) {
    Py_ssize_t m = co->co_argcount - defcount;
    Py_ssize_t missing = 0;

    for (i = argcount; i < m; i++) {
      if (GETLOCAL(i) == NULL) {
	missing++;
      }
    }

    if (missing) {
      missing_arguments(co, missing, defcount, fastlocals);
      goto fail;
    }

    i = (n > m)? (n - m): 0;

    for (; i < defcount; i++) {
      if (GETLOCAL(m+i) == NULL) {
	PyObject *def = defs[i];
	Py_INCREF(def);
	SETLOCAL(m+i, def);
      }
    }
  }

  // Add missing keyword arguments (copy default values from kwdefs)
  if (co->co_kwonlyargcount > 0) {
    Py_ssize_t missing = 0;

    for (i = co->co_argcount; i < total_args; i++) {
      PyObject *name;

      if (GETLOCAL(i) != NULL)
	continue;

      name = PyTuple_GET_ITEM(co->co_varnames, i);

      if (kwdefs != NULL) {
	PyObject *def = PyDict_GetItem(kwdefs, name);

	if (def) {
	  Py_INCREF(def);
	  SETLOCAL(i, def);
	  continue;
	}
      }
      missing++;
    }

    if (missing) {
      missing_arguments(co, missing, -1, fastlocals);
      goto fail;
    }
  }

  // Allocate and initialize storage for cell vars
  for (i = 0; i < n_cellvars; ++i) {
    PyObject *c;
    int arg;

    /* Possibly account for the cell variable being an argument. */
    if (co->co_cell2arg != NULL &&
	(arg = co->co_cell2arg[i]) != CO_CELL_NOT_AN_ARG) {

      c = PyCell_New(GETLOCAL(arg));
      SETLOCAL(arg, NULL);

    } else {
      c = PyCell_New(NULL);
    }

    SETLOCAL(co->co_nlocals + i, c);
  }

  return 0;

 fail:
  return -1;
}


static PyObject *m_tcr_frame_vars(PyObject *mod,
				  PyObject *args, PyObject *kwds) {

  PyObject *result;

  Py_ssize_t argc = args? PyTuple_GET_SIZE(args): 0;
  if (argc < 2) {
    return NULL;
  }

  PyObject *selfref = PyTuple_GET_ITEM(args, 0);
  PyObject *work = PyTuple_GET_ITEM(args, 1);

  // note, this won't TCR on methods. We might make adapt that to work
  // later on (moving self ref over into args)

  if (work == selfref) {
    PyObject **argptr = (argc > 2)? &PyTuple_GET_ITEM(args, 2): NULL;

    if (apply_frame_vars(PyEval_GetFrame(), work, argptr, argc - 2, kwds)) {
      result = NULL;

    } else {
      Py_INCREF(Py_False);
      result = Py_False;
    }

  } else {

    // this isn't the same function, so we won't be doing the jump
    // 0. Instead, we'll be using tailcall_full to either wrap up the
    // function or invoke it inline if it's not TCO enabled.

    args = PyTuple_GetSlice(args, 2, PyTuple_GET_SIZE(args));
    PyObject *tc = do_tc_full(work, args, kwds);
    Py_DECREF(args);

    if (tc) {
      result = PyTuple_New(1);
      PyTuple_SET_ITEM(result, 0, tc);

    } else {
      result = NULL;
    }
  }

  /*
    result is one of the following:
     - NULL to signal an exception
     - False if a JUMP 0 is appropriate
     - 1-tuple of Tailcall instance for the trampoline to bounce
     - 1-tuple of inline function call result for trampoline to return
  */
  return result;
}


static PyObject *m_tailcall_full(PyObject *mod,
				 PyObject *args, PyObject *kwds) {

  // checked

  PyObject *work = NULL;

  if (PyTuple_GET_SIZE(args) <= 1) {
    if (unlikely(! PyArg_ParseTuple(args, "O", &work))) {
      return NULL;
    }
    args = PyTuple_New(0);

  } else {
    work = PyTuple_GET_ITEM(args, 0);
    args = PyTuple_GetSlice(args, 1, PyTuple_GET_SIZE(args));
  }

  PyObject *result = do_tc_full(work, args, kwds);
  Py_DECREF(args);

  return result;
}


static PyObject *tailcall_new(PyTypeObject *type,
			      PyObject *args, PyObject *kwds) {

  // checked

  PyObject *work = NULL;
  PyObject *tmp = NULL;

  if (kwds && PyDict_Size(kwds)) {
    PyErr_SetString(PyExc_TypeError, "tailcall takes no named arguments");
    return NULL;
  }

  if (unlikely(! PyArg_ParseTuple(args, "O:tailcall", &work))) {
    return NULL;
  }

  if (SibTrampoline_Check(work)) {
    // it's a trampoline, so we'll tailcall using the original
    // function if it allows it.
    work = ((Trampoline *) work)->tco_original;
    Py_INCREF(work);

  } else {
    tmp = _getattro(work, _tco_enable);
    if (tmp && (PyObject_IsTrue(tmp))) {
      Py_DECREF(tmp);

      tmp = _getattro(work, _tco_original);
      if (tmp) {
	work = tmp;
      } else {
	Py_INCREF(work);
      }

    } else {
      Py_XDECREF(tmp);
      Py_INCREF(work);
      return work;
    }
  }

  // work is ref'd

  tmp = SibTailcall_New(work);
  Py_DECREF(work);
  return tmp;
}


static void tailcall_dealloc(PyObject *self) {

  // checked

  Py_CLEAR(((Tailcall *) self)->work);
  Py_CLEAR(((Tailcall *) self)->args);
  Py_CLEAR(((Tailcall *) self)->kwds);

  if (tailcall_free_list) {
    Py_TYPE(self)->tp_free(self);
  } else {
    tailcall_free_list = (Tailcall *) self;
  }
}


static PyObject *tailcall_call(PyObject *self,
			       PyObject *args, PyObject *kwds) {

  // checked

  Py_ASSIGN(((Tailcall *) self)->args, args);
  Py_ASSIGN(((Tailcall *) self)->kwds, kwds);

  Py_INCREF(self);
  return self;
}


PyTypeObject SibTailcallType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.Tailcall",
    sizeof(Tailcall),
    0,

    .tp_new = tailcall_new,
    .tp_dealloc = tailcall_dealloc,
    .tp_call = tailcall_call,
    .tp_flags = Py_TPFLAGS_DEFAULT,
};


PyObject *SibTailcall_New(PyObject *work) {

  // checked

  Tailcall *tc = NULL;

  if (tailcall_free_list) {
    // printf("using existing Tailcall instance\n");

    tc = tailcall_free_list;
    tailcall_free_list = NULL;
    Py_INCREF(tc);

  } else {
    // printf("no existing Tailcall instance, allocating\n");

    tc = PyObject_New(Tailcall, &SibTailcallType);
    if (unlikely(! tc))
      return NULL;

    tc->args = NULL;
    tc->kwds = NULL;
  }

  Py_XINCREF(work);
  tc->work = work;

  return (PyObject *) tc;
}


/* === SibTrampolineType === */


static void trampoline_dealloc(PyObject *self) {

  // checked

  Py_CLEAR(((Trampoline *) self)->tco_original);

  Py_TYPE(self)->tp_free(self);
}


#define STEAL(dest, orig) {			\
    dest = (orig);				\
    orig = NULL;				\
  }



static PyObject *trampoline_call(PyObject *self,
				 PyObject *args, PyObject *kwds) {

  // checked

  PyObject *work = ((Trampoline *) self)->tco_original;
  PyObject *result = NULL;

  if (unlikely(! work)) {
    PyErr_SetString(PyExc_ValueError, "trampoline invoked with no function");
    return NULL;
  }

  // initial call using the given arguments
  result = PyObject_Call(work, args, kwds);

  while (SibTailcall_Check(result)) {
    // each bounce comes with its own work and arguments

    STEAL(work, ((Tailcall *) result)->work);
    STEAL(args, ((Tailcall *) result)->args);
    STEAL(kwds, ((Tailcall *) result)->kwds);

    // free up the Tailcall early so it can be reused. This also sets
    // result to a NULL in case we're in an error state.
    Py_CLEAR(result);

    if (likely(work && args)) {
      result = PyObject_Call(work, args, kwds);

    } else if (! work) {
      PyErr_SetString(PyExc_ValueError, "tailcall bounced with no function");

    } else if (! args) {
      PyErr_SetString(PyExc_ValueError, "tailcall bounced with no args");
    }

    Py_XDECREF(work);
    Py_XDECREF(args);
    Py_XDECREF(kwds);
  }

  return result;
}


static PyObject *trampoline_repr(PyObject *self) {

  // checked

  PyObject *original = ((Trampoline *) self)->tco_original;
  return PyUnicode_FromFormat("<trampoline for %R>", original);
}


static PyObject *trampoline_tco_original(PyObject *self) {

  // checked

  PyObject *tco_original = ((Trampoline *) self)->tco_original;

  if (unlikely(! tco_original)) {
    PyErr_SetString(PyExc_ValueError, "trampoline has no original");
    return NULL;

  } else {
    Py_INCREF(tco_original);
    return tco_original;
  }
}


static PyObject *trampoline_tco_enable(PyObject *self) {

  // checked

  Py_INCREF(Py_True);
  return Py_True;
}


static PyObject *get_original(PyObject *self, char *name) {

  // checked

  PyObject *tco_original = ((Trampoline *) self)->tco_original;

  if (unlikely(! tco_original)) {
    PyErr_SetString(PyExc_ValueError, "trampoline has no original");
    return NULL;

  } else {
    return PyObject_GetAttrString(tco_original, name);
  }
}


static int set_original(PyObject *self, PyObject *val, char *name) {

  // checked

  PyObject *tco_original = ((Trampoline *) self)->tco_original;

  if (unlikely(! tco_original)) {
    PyErr_SetString(PyExc_ValueError, "trampoline has no original");
    return -1;

  } else {
    return PyObject_SetAttrString(tco_original, name, val);
  }
}


static PyGetSetDef trampoline_func_getset[] = {
  { "_tco_original",
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { "_tco_enable",
    (getter) trampoline_tco_enable, NULL,
    "", NULL },

  { "__doc__",
    (getter) get_original, (setter) set_original,
    "", "__doc__" },

  { NULL },
};


static PyGetSetDef trampoline_meth_getset[] = {
  { "_tco_original",
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { "_tco_enable",
    (getter) trampoline_tco_enable, NULL,
    "", NULL },

  { "__doc__",
    (getter) get_original, (setter) set_original,
    "", "__doc__" },

  { NULL },
};


static PyObject *descr_get(PyObject *self,
			   PyObject *inst, PyObject *owner) {

  // checked

  /**
     When a function trampoline member is retrieved from an object
     instance, this will be called to enable the function trampoline
     to wrap itself as a method trampoline.

     We do that by invoking the same __get__ call on our function
     trampoline's underlying callable, and then allocating a method
     trampoline to wrap it. The method trampoline only differs from a
     function trampoline in that it will not re-wrap in this fashion.
   */

  PyObject *orig, *bound;

  if (unlikely((! inst) || inst == Py_None)) {
    Py_INCREF(self);
    return self;
  }

  orig = ((Trampoline *) self)->tco_original;

  // TODO: try to detect the descr interface rather than calling
  // __get__ directly like this.

  bound = PyObject_CallMethodObjArgs(orig, __get__, inst, owner, NULL);
  if (unlikely (! bound)) {
    return NULL;

  } else if (bound == orig) {
    // descriptor returned same as original, we'll return self
    Py_DECREF(bound);
    Py_INCREF(self);
    return self;

  } else {
    // wrap the descriptor result up in a method trampoline
    Trampoline *tramp = PyObject_New(Trampoline, &MethodTrampolineType);
    if (unlikely(! tramp)) {
      Py_DECREF(bound);
      return NULL;
    }
    tramp->tco_original = bound;
    return (PyObject *) tramp;
  }
}


static PyObject *trampoline_getattr(PyObject *self, PyObject *name) {

  PyObject *orig = ((Trampoline *) self)->tco_original;
  PyObject **odp = orig? _PyObject_GetDictPtr(orig): NULL;
  PyObject *od = odp? *odp: NULL;

  PyObject *res = _PyObject_GenericGetAttrWithDict(self, name, od, 1);

  if(res == NULL) {
    // wasn't found directly on the trampoline, pass through to the
    // underlying function instead.

    res = PyObject_GetAttr(orig, name);
  }

  return res;
}


static int trampoline_setattr(PyObject *self, PyObject *name,
			      PyObject *value) {

  PyObject *orig = ((Trampoline *) self)->tco_original;
  PyObject **odp = orig? _PyObject_GetDictPtr(orig): NULL;
  PyObject *od = odp? *odp: NULL;

  int res = _PyObject_GenericSetAttrWithDict(self, name, value, od);

  if(res != 0) {
    // wasn't found directly on the trampoline, pass through to the
    // underlying function instead.
    PyErr_Clear();
    res = PyObject_SetAttr(orig, name, value);
  }

  return res;
}


PyTypeObject FunctionTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.FunctionTrampoline",
    sizeof(Trampoline),
    0,

    // .tp_new = PyType_GenericNew,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = descr_get,
    .tp_getset = trampoline_func_getset,
    .tp_call = trampoline_call,
    .tp_repr = trampoline_repr,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_getattro = trampoline_getattr,
    .tp_setattro = trampoline_setattr,
};


PyTypeObject MethodTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.MethodTrampoline",
    sizeof(Trampoline),
    0,

    // .tp_new = PyType_GenericNew,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = NULL,
    .tp_getset = trampoline_meth_getset,
    .tp_call = trampoline_call,
    .tp_repr = trampoline_repr,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_getattro = trampoline_getattr,
    .tp_setattro = trampoline_setattr,
};


static PyObject *m_trampoline(PyObject *self, PyObject *args) {

  // checked

  PyObject *fun = NULL;
  PyObject *tmp = NULL;
  Trampoline *tramp = NULL;

  if (unlikely(! PyArg_ParseTuple(args, "O", &fun))) {
    return NULL;
  }

  tramp = PyObject_New(Trampoline, &FunctionTrampolineType);
  if (unlikely(! tramp))
    return NULL;

  tmp = _getattro(fun, _tco_original);
  if (tmp) {
    fun = tmp;
  } else {
    Py_INCREF(fun);
  }

  tramp->tco_original = fun;
  return (PyObject *) tramp;
}


static PyObject *m_trampoline_check(PyObject *self, PyObject *obj) {
  return PyBool_FromLong(SibTrampoline_Check(obj));
}


static PyMethodDef methods[] = {

  { "tailcall_full", (PyCFunction) m_tailcall_full,
    METH_VARARGS|METH_KEYWORDS,
    "tailcall_full(function, *args, **kwds) ->"
    " tailcall(function)(*args, **kwds)" },

  { "trampoline", m_trampoline, METH_VARARGS,
    "wraps a callable in a trampoline. A trampoline will catch returned"
    " tailcall instances and invoke their function and arguments"
    " in-place. The trampoline will continue catching and bouncing until"
    " a non-tailcall instance is returned or an exception is raised." },

  { "is_trampoline", m_trampoline_check, METH_O,
    "True if an object is a trampoline." },

  { "tcr_frame_vars", (PyCFunction) m_tcr_frame_vars,
    METH_VARARGS|METH_KEYWORDS,
    "resets local and cell variables for the current frame from args and"
    " kwargs if selfref and work are identical. Returns a tuple indicating"
    " whether a jump 0 is acceptable or not" },

  { NULL, NULL, 0, NULL },
};


#define STR_CONST(which, val) {			\
    if (! (which))				\
      which = PyUnicode_FromString(val);	\
  }


int sib_types_tco_init(PyObject *mod) {
  if (! mod)
    return -1;

  if (PyType_Ready(&SibTailcallType))
    return -1;

  if (PyType_Ready(&FunctionTrampolineType))
    return -1;

  if (PyType_Ready(&MethodTrampolineType))
    return -1;

  STR_CONST(__get__, "__get__");
  STR_CONST(_tco_original, "_tco_original");
  STR_CONST(_tco_enable, "_tco_enable");

  PyObject *dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "tailcall", (PyObject *) &SibTailcallType);

  return PyModule_AddFunctions(mod, methods);
}


/* The end. */
