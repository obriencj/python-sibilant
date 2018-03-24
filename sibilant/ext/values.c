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
   sibilant._values

   Sibilant's values type.

   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
 */


#include "sibilant.h"


#define DOCSTR "Sibilant's values type"


#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif


#if 1
#define DEBUGMSG(msg, obj) {					\
    printf("** " msg " ");					\
    (obj) && PyObject_Print(((PyObject *) (obj)), stdout, 0);	\
    printf("\n");						\
  }
#else
#define DEBUGMSG(msg, obj) {}
#endif


#if (defined(__GNUC__) &&					\
     (__GNUC__ > 2 || (__GNUC__ == 2 && (__GNUC_MINOR__ > 95))))
  #define likely(x)   __builtin_expect(!!(x), 1)
  #define unlikely(x) __builtin_expect(!!(x), 0)
#else
  #define likely(x)   (x)
  #define unlikely(x) (x)
#endif


/* === util === */

static PyObject *_str_close_paren = NULL;
static PyObject *_str_colon = NULL;
static PyObject *_str_comma_space = NULL;
static PyObject *_str_cons_paren = NULL;
static PyObject *_str_dot_space = NULL;
static PyObject *_str_elipsis = NULL;
static PyObject *_str_empty = NULL;
static PyObject *_str_equals = NULL;
static PyObject *_str_esc_quote = NULL;
static PyObject *_str_nil = NULL;
static PyObject *_str_open_paren = NULL;
static PyObject *_str_quote = NULL;
static PyObject *_str_recursive = NULL;
static PyObject *_str_recursive_true = NULL;
static PyObject *_str_rsplit = NULL;
static PyObject *_str_space = NULL;
static PyObject *_str_space_dot_space = NULL;
static PyObject *_str_space_elipsis = NULL;
static PyObject *_str_split = NULL;
static PyObject *_str_star = NULL;
static PyObject *_str_starstar = NULL;
static PyObject *_str_strip = NULL;
static PyObject *_str_values_paren = NULL;


static PyObject *quoted(PyObject *u) {
  PyObject *tmp, *result;

  tmp = PyUnicode_Replace(u, _str_quote, _str_esc_quote, -1);
  result = PyUnicode_FromFormat("\"%U\"", tmp);
  Py_DECREF(tmp);

  return result;
}


/* === ValuesType === */


static PyObject *values_new(PyTypeObject *type,
			    PyObject *args, PyObject *kwds) {

  return sib_values(args, kwds);
}


static void values_dealloc(PyObject *self) {
  SibValues *s = (SibValues *) self;

  if (s->weakrefs != NULL)
    PyObject_ClearWeakRefs(self);

  Py_XDECREF(s->args);
  Py_XDECREF(s->kwds);

  Py_TYPE(self)->tp_free(self);
}


static int values_traverse(PyObject *self, visitproc visit, void *arg) {
  SibValues *s = (SibValues *) self;
  Py_VISIT(s->args);
  if (s->kwds)
    Py_VISIT(s->kwds);
  return 0;
}


static int values_clear(PyObject *self) {
  SibValues *s = (SibValues *) self;
  Py_CLEAR(s->args);
  if (s->kwds)
    Py_CLEAR(s->kwds);
  return 0;
}


static PyObject *values_iter(PyObject *self) {
  SibValues *s = (SibValues *) self;
  return PyObject_GetIter(s->args);
}


static PyObject *values_args_getitem(PyObject *self, Py_ssize_t index) {
  SibValues *s = (SibValues *) self;
  return PySequence_GetItem(s->args, index);
}


static Py_ssize_t values_kwds_length(PyObject *self) {
  SibValues *s = (SibValues *) self;

  if (s->kwds) {
    return PyDict_Size(s->kwds);
  } else {
    return 0;
  }
}


static PyObject *values_kwds_getitem(PyObject *self, PyObject *key) {
  SibValues *s = (SibValues *) self;

  if (PyLong_CheckExact(key)) {
    return PySequence_GetItem(s->args, PyLong_AsSsize_t(key));

  } else {
    PyObject *result = NULL;

    if (s->kwds) {
      result = PyDict_GetItem(s->kwds, key);
    }

    if (result) {
      Py_INCREF(result);

    } else {
      // we do our own error handling because result could be NULL
      // either because there was a NULL keyword dict, or because of
      // an actual NULL result from GetItem. in either of those cases,
      // we want to emit the same KeyError
      PyErr_SetObject(PyExc_KeyError, quoted(key));
    }

    return result;
  }
}


static PyObject *values_call(PyObject *self,
			     PyObject *args, PyObject *kwds) {

  SibValues *s = (SibValues *) self;
  PyObject *call_args, *call_kwds;
  PyObject *work = NULL, *tmp = NULL;

  if (unlikely(! PyTuple_GET_SIZE(args))) {
    PyErr_SetString(PyExc_TypeError, "values objects must be called with at"
		    "least one argument, the function to apply");
    return NULL;
  }

  work = PyTuple_GET_ITEM(args, 0);

  if (PyTuple_GET_SIZE(args) > 1) {
    // if we have more positionals beyond just the callable work item,
    // we'll need to add those the invocation of work

    tmp = PySequence_GetSlice(args, 1, PyTuple_GET_SIZE(args));

    if (PyTuple_GET_SIZE(s->args)) {
      // merge the existing positionals with the invocation ones
      call_args = PySequence_Concat(s->args, tmp);
      Py_DECREF(tmp);

    } else {
      // there were no positionals in the values, so just use the
      // invocation ones
      call_args = tmp;
    }

  } else {
    // no additional positionals given at invocation, so we'll just be
    // using our existing ones.
    call_args = s->args;
    Py_INCREF(call_args);
  }

  if (kwds && PyDict_Size(kwds)) {
    // if keyword arguments were supplied, we'll need to add those to
    // the work invocation.

    if (s->kwds && PyDict_Size(s->kwds)) {
      // if the values already had keywords, we'll need to create a
      // new dict and merge these two sets of keyword arguments
      // together

      call_kwds = PyDict_New();
      PyDict_Update(call_kwds, s->kwds);
      PyDict_Update(call_kwds, kwds);

    } else {
      // the values had no keywords, so let's just use the ones supplied
      // by the invocation

      call_kwds = kwds;
      Py_INCREF(call_kwds);
    }

  } else {
    // no extra keyword arguments were supplied, so we only need to
    // use the ones from the values
    call_kwds = s->kwds;
    Py_XINCREF(call_kwds);
  }

  tmp = PyObject_Call(work, call_args, call_kwds);
  Py_DECREF(call_args);
  Py_XDECREF(call_kwds);

  return tmp;
}


static PyObject *values_repr(PyObject *self) {
  SibValues *s = (SibValues *) self;
  PyObject *col = PyList_New(0);
  PyObject *tmp = NULL;
  Py_ssize_t count = 0, limit = 0;
  PyObject *key = NULL, *value = NULL;

  // "values()"
  // "values(1, 2, 3)"
  // "values(foo=4, bar=5)"
  // "values(1, 2, 3, foo=4, bar=5)"

  PyList_Append(col, _str_values_paren);

  limit = PyTuple_GET_SIZE(s->args);
  for (count = 0; count < limit; count++) {
    tmp = PyObject_Repr(PyTuple_GET_ITEM(s->args, count));
    PyList_Append(col, tmp);
    PyList_Append(col, _str_comma_space);
    Py_DECREF(tmp);
  }

  count = 0;
  if (s->kwds && PyDict_Size(s->kwds)) {
    while (PyDict_Next(s->kwds, &count, &key, &value)) {
      tmp = PyObject_Repr(value);
      PyList_Append(col, key);
      PyList_Append(col, _str_equals);
      PyList_Append(col, tmp);
      PyList_Append(col, _str_comma_space);
      Py_DECREF(tmp);
    }
  }

  if (limit || count) {
    // we'll have a trailing _str_comma_space if we added
    // anything. Re-use its index for the close paren
    Py_INCREF(_str_close_paren);
    PyList_SetItem(col, PyList_GET_SIZE(col) - 1, _str_close_paren);
  } else {
    // otherwise, just close off as "values()"
    PyList_Append(col, _str_close_paren);
  }

  tmp = PyUnicode_Join(_str_empty, col);
  Py_DECREF(col);

  return tmp;
}


static Py_hash_t values_hash(PyObject *self) {
  SibValues *s = (SibValues *) self;
  Py_uhash_t result = s->hashed, khash;
  PyObject *tmp, *frozen;

  if (result == 0) {
    result = PyObject_Hash(s->args);
    if (result == (Py_uhash_t) -1)
      return -1;

    if (s->kwds && PyDict_Size(s->kwds)) {
      tmp = _PyDictView_New(s->kwds, &PyDictItems_Type);
      frozen = PyFrozenSet_New(tmp);
      Py_DECREF(tmp);

      if (! frozen)
	return -1;

      khash = PyObject_Hash(frozen);
      Py_DECREF(frozen);

      if (khash == (Py_uhash_t) -1)
	return -1;

      // I stole these magic numbers from tuplehash
      result = (result ^ khash) * _PyHASH_MULTIPLIER;
      result += 97531UL;

      if (result == (Py_uhash_t) -1)
	result = -2;
    }

    s->hashed = result;
  }

  return result;
}


static long values_eq(PyObject *self, PyObject *other) {
  SibValues *s = (SibValues *) self;
  long answer = 0;

  if (self == other) {
    // identity is equality, yes
    answer = 1;

  } else if (SibValues_CheckExact(other)) {
    SibValues *o = (SibValues *) other;

    // when comparing two values against each other, we'll just
    // compare their positionals and keywords. We'll actually do the
    // keywords check first, because it has a quick NULL-check

    if (s->kwds && o->kwds) {
      answer = PyObject_RichCompareBool(s->kwds, o->kwds, Py_EQ);
    } else {
      answer = (s->kwds == o->kwds);
    }

    answer = answer && \
      PyObject_RichCompareBool(s->args, o->args, Py_EQ);

  } else if (PyTuple_CheckExact(other)) {
    // comparing against a tuple is fine, so long as keywords either
    // are NULL or empty.

    answer = ((! s->kwds) || (! PyDict_Size(s->kwds))) &&	\
      PyObject_RichCompareBool(s->args, other, Py_EQ);

  } else if (PyDict_CheckExact(other)) {
    // comparing against a dict is fine, so long as positionals is
    // empty.

    if (s->kwds) {
      answer = (! PyTuple_GET_SIZE(s->args)) &&			\
	PyObject_RichCompareBool(s->kwds, other, Py_EQ);

    } else {
      // we'll say a NULL keywords is equal to an empty dict
      answer = (! PyTuple_GET_SIZE(s->args)) && \
	(! PyDict_Size(other));
    }
  }

  return answer;
}


static PyObject *values_richcomp(PyObject *self, PyObject *other, int op) {
  if (op == Py_EQ) {
    return PyBool_FromLong(values_eq(self, other));

  } else if (op == Py_NE) {
    return PyBool_FromLong(! values_eq(self, other));

  } else {
    PyErr_SetString(PyExc_TypeError, "unsupported values comparison");
    return NULL;
  }
}


static int values_bool(PyObject *self) {
  SibValues *s = (SibValues *) self;

  return !!((s->kwds && PyDict_Size(s->kwds)) || PyTuple_GET_SIZE(s->args));
}


static PyObject *values_keys(PyObject *self, PyObject *_noargs) {
  SibValues *s = (SibValues *) self;
  PyObject *result = NULL, *tmp;

  if (s->kwds) {
    // this is what the default keys() impl on dict does. The
    // PyDict_Keys API creates a list, which we don't want to do.
    result = _PyDictView_New(s->kwds, &PyDictKeys_Type);

  } else {
    // a cheap empty iterator
    tmp = PyTuple_New(0);
    result = PyObject_GetIter(tmp);
    Py_DECREF(tmp);
  }

  return result;
}


static PyMethodDef values_methods[] = {
  { "keys", (PyCFunction) values_keys, METH_NOARGS,
    "V.keys()" },

  { NULL, NULL, 0, NULL },
};


static PyNumberMethods values_as_number = {
  .nb_bool = (inquiry) values_bool,
};


static PySequenceMethods values_as_sequence = {
  .sq_item = values_args_getitem,
};


static PyMappingMethods values_as_mapping = {
  .mp_length = values_kwds_length,
  .mp_subscript = values_kwds_getitem,
};


PyTypeObject SibValuesType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "values",
  sizeof(SibValues),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_HAVE_GC,
  .tp_methods = values_methods,
  .tp_new = values_new,
  .tp_dealloc = values_dealloc,
  .tp_weaklistoffset = offsetof(SibValues, weakrefs),
  .tp_traverse = values_traverse,
  .tp_clear = values_clear,

  .tp_iter = values_iter,
  .tp_hash = values_hash,
  .tp_as_number = &values_as_number,
  .tp_as_sequence = &values_as_sequence,
  .tp_as_mapping = &values_as_mapping,

  .tp_call = values_call,
  .tp_repr = values_repr,
  .tp_richcompare = values_richcomp,
};


PyObject *sib_values(PyObject *args, PyObject *kwds) {
  SibValues *self = NULL;

  if (! args) {
    PyErr_SetString(PyExc_TypeError, "values require arguments");
    return NULL;
  }

  self = PyObject_GC_New(SibValues, &SibValuesType);
  if (unlikely(! self))
    return NULL;

  Py_INCREF(args);
  self->args = args;
  self->kwds = kwds? PyDict_Copy(kwds): NULL;
  self->weakrefs = NULL;
  self->hashed = 0;

  PyObject_GC_Track((PyObject *) self);
  return (PyObject *) self;
}


/* === module === */


static struct PyModuleDef ext_values = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.ext.values",
  .m_doc = DOCSTR,
  .m_size = -1,
  .m_methods = NULL,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


#define STR_CONST(which, val) {			\
    if (! (which))				\
      which = PyUnicode_FromString(val);	\
  }


PyMODINIT_FUNC PyInit_values(void) {

  PyObject *mod, *dict;

  if (PyType_Ready(&SibValuesType) < 0)
    return NULL;

  STR_CONST(_str_close_paren, ")");
  STR_CONST(_str_colon, ":");
  STR_CONST(_str_comma_space, ", ");
  STR_CONST(_str_cons_paren, "cons(");
  STR_CONST(_str_dot_space, ". ");
  STR_CONST(_str_elipsis, "...");
  STR_CONST(_str_empty, "");
  STR_CONST(_str_equals, "=");
  STR_CONST(_str_esc_quote, "\\\"");
  STR_CONST(_str_nil, "nil");
  STR_CONST(_str_open_paren, "(");
  STR_CONST(_str_quote, "\"");
  STR_CONST(_str_recursive, "recursive");
  STR_CONST(_str_recursive_true, "recursive=True");
  STR_CONST(_str_rsplit, "rsplit");
  STR_CONST(_str_space, " ");
  STR_CONST(_str_space_dot_space, " . ");
  STR_CONST(_str_space_elipsis, " ...");
  STR_CONST(_str_split, "split");
  STR_CONST(_str_star, "*");
  STR_CONST(_str_starstar, "**");
  STR_CONST(_str_strip, "strip");
  STR_CONST(_str_values_paren, "values(");

  mod = PyModule_Create(&ext_values);
  if (! mod)
    return NULL;

  dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "values", (PyObject *) &SibValuesType);

  return mod;
}


/* The end. */
