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
   sibilant._util

   Sibilant utility functions

   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
 */


#include "sibilant.h"


#define DOCSTR "Native Sibilant core types and functions"


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


static PyObject *Sib_Nil = NULL;


static PyObject *m_reapply(PyObject *mod, PyObject *args, PyObject *kwds) {
  PyObject *fun, *data, *result;
  long count = 0;

  static char *keywords[] = { "fun", "data", "count", NULL };

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOl:reapply", keywords,
				    &fun, &data, &count))
    return NULL;

  Py_INCREF(data);
  result = data;

  while (count-- > 0) {
    result = PyObject_CallFunctionObjArgs(fun, data);
    Py_DECREF(data);

    if (! result)
      break;

    data = result;
  }

  return result;
}


static PyObject *m_build_unpack_pair(PyObject *mod, PyObject *seqs) {
  Py_ssize_t count = PyTuple_GET_SIZE(seqs);
  Py_ssize_t index = 0;
  PyObject *coll = NULL;
  PyObject *work = NULL;
  PyObject *result = NULL;

  if (count == 0) {
    Py_INCREF(Sib_Nil);
    return Sib_Nil;
  }

  coll = PyList_New(0);

  DEBUGMSG("seqs is", seqs);

  while (index < count) {
    work = PyTuple_GET_ITEM(seqs, index++);

    if (Sib_Nilp(work)) {
      continue;

    } else if (SibPair_CheckExact(work)) {
      work = SibPair_Unpack((SibPair *) work);

    } else {
      // todo: check for lists and tuples explicitly, and see if
      // they're zero-length. If so, avoid creating an iterator, just
      // continue to the next item.
      work = PyObject_GetIter(work);
    }

    if (! work) {
      Py_DECREF(coll);
      return NULL;
    }

    // returns a new reference to None
    result = _PyList_Extend((PyListObject *) coll, work);
    Py_CLEAR(work);

    if (! result) {
      Py_DECREF(coll);
      return NULL;

    } else {
      Py_CLEAR(result);
    }
  }

  if (! PyList_GET_SIZE(coll)) {
    // The collection didn't net any actual elements, so let's skip
    // out early and just return nil

    Py_DECREF(coll);
    Py_INCREF(Sib_Nil);
    return Sib_Nil;
  }

  // I wonder if there isn't a better way to do this. We end up
  // traversing the last cons pair twice, which means allocating a set
  // in order to avoid recursion. Is that too expensive?
  work = PyTuple_GET_ITEM(seqs, count - 1);
  if (SibPair_CheckExact(work) && SibPair_IsProper((SibPair *) work)) {
    PyList_Append(coll, Sib_Nil);
  }

  DEBUGMSG("coll is", coll);

  // assemble coll into a pair
  count = PyList_GET_SIZE(coll);
  if (count) {
    result = sib_pair(PyList_GET_ITEM(coll, 0), Sib_Nil);
    DEBUGMSG("bla is", PyList_GET_ITEM(coll, 0));
    DEBUGMSG("nil is", Sib_Nil);
    DEBUGMSG("result is", result);

    if(! result) {
      Py_DECREF(coll);
      return NULL;
    }

  } else {
    Py_INCREF(Sib_Nil);
    result = Sib_Nil;
  }

  DEBUGMSG("result is", result);

  if (count > 1) {
    work = PyList_GET_ITEM(coll, --count);
    DEBUGMSG("work is", work);
    while (--count) {
      work = sib_pair(PyList_GET_ITEM(coll, count), work);
      DEBUGMSG("work is", work);
    }
    SETCDR(result, work);
    DEBUGMSG("result is", result);
  }

  Py_DECREF(coll);
  return result;
}


static PyObject *m_build_tuple(PyObject *mod, PyObject *values) {
  Py_INCREF(values);
  return values;
}


static PyObject *m_build_list(PyObject *mod, PyObject *values) {
  return PySequence_List(values);
}


static PyObject *m_build_set(PyObject *mod, PyObject *values) {
  return PySet_New(values);
}


static PyObject *m_build_dict(PyObject *mod, PyObject *values) {
  PyObject *collect;
  PyObject *result;
  PyObject *item;

  int count = PyTuple_GET_SIZE(values);

  result = PyDict_New();
  if (! count)
    return result;

  /* I duplicate values into a new tuple because I'm not entirely sure
     just how safe it is to modify that in-place. */
  collect = PyTuple_New(count);

  while (count--) {
    /* because we support items as either an arbitrary iterable with
       len 2, or a pair (which may be proper), we need to potentially
       convert pairs via unpack */
    item = PyTuple_GET_ITEM(values, count);

    if (SibPair_CheckExact(item) && SibPair_Check(CDR(item))) {
      /* pair of more than one link, convert to iterator */
      PyTuple_SET_ITEM(collect, count, SibPair_Unpack((SibPair *)item));

    } else {
      Py_INCREF(item);
      PyTuple_SET_ITEM(collect, count, item);
    }
  }

  if (PyDict_MergeFromSeq2(result, collect, 1)) {
    Py_DECREF(result);
    result = NULL;
  }

  Py_DECREF(collect);
  return result;
}


static PyMethodDef methods[] = {
  { "reapply", (PyCFunction) m_reapply, METH_VARARGS|METH_KEYWORDS,
    "reapply(func, data, count) -> result data\n"
    "Calls `data = func(data)` count times (or until an exception is\n"
    "raised), and returns the final data value." },

  { "build_unpack_pair", (PyCFunction) m_build_unpack_pair, METH_VARARGS,
    "build_unpack_pair(*pair_or_seq) -> new pair\n"
    "Creates a new sibilant pair list from a collection of pair or\n"
    "non-pair sequences." },

  { "build_tuple", (PyCFunction) m_build_tuple, METH_VARARGS,
    "build_tuple(*args) -> args" },

  { "build_list", (PyCFunction) m_build_list, METH_VARARGS,
    "build_list(*args) -> list(args)" },

  { "build_set", (PyCFunction) m_build_set, METH_VARARGS,
    "build_set(*args) -> set(args)" },

  { "build_dict",  (PyCFunction) m_build_dict, METH_VARARGS,
    "build_dict(*items) -> dict(items)" },

  { NULL, NULL, 0, NULL },
};


static struct PyModuleDef ext_util = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.ext.util",
  .m_doc = DOCSTR,
  .m_size = -1,
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


PyMODINIT_FUNC PyInit_util(void) {
  PyObject *tmp = PyImport_Import("sibilant.ext.pair");
  if (! tmp)
    return NULL;

  Sib_Nil = PyObject_GetAttrString(tmp, "nil");
  Py_DECREF(tmp);

  if (! Sib_Nil)
    return NULL;

  printf("\nSib_Nil in util is %p\n", Sib_Nil);

  return PyModule_Create(&ext_util);
}


/* The end. */
