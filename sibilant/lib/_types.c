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
   sibilant.lib._types

   Native sibilant core types and helper functions. These function and
   types will be re-exported from the sibilant module.

   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
*/


#include "types.h"


#define DOCSTR "Native Sibilant core types and functions"


/* === util === */


PyObject *_str_close_paren = NULL;
PyObject *_str_colon = NULL;
PyObject *_str_comma_space = NULL;
PyObject *_str_cons_paren = NULL;
PyObject *_str_dot_space = NULL;
PyObject *_str_elipsis = NULL;
PyObject *_str_empty = NULL;
PyObject *_str_equals = NULL;
PyObject *_str_esc_quote = NULL;
PyObject *_str_nil = NULL;
PyObject *_str_open_paren = NULL;
PyObject *_str_quote = NULL;
PyObject *_str_recursive = NULL;
PyObject *_str_recursive_true = NULL;
PyObject *_str_rsplit = NULL;
PyObject *_str_space = NULL;
PyObject *_str_space_dot_space = NULL;
PyObject *_str_space_elipsis = NULL;
PyObject *_str_split = NULL;
PyObject *_str_star = NULL;
PyObject *_str_starstar = NULL;
PyObject *_str_strip = NULL;
PyObject *_str_values_paren = NULL;


PyObject *sib_quoted(PyObject *u) {

  // checked

  PyObject *tmp = PyUnicode_Replace(u, _str_quote, _str_esc_quote, -1);
  if (! tmp)
    return NULL;

  PyObject *result = PyUnicode_FromFormat("\"%U\"", tmp);
  Py_DECREF(tmp);
  return result;
}


/* === module === */


static PyObject *m_getderef(PyObject *mod, PyObject *cell) {

  // checked

  PyObject *result = NULL;

  if (! PyCell_Check(cell)) {
    PyErr_SetString(PyExc_TypeError, "getderef argument 1 must be a cell");

  } else {
    result = PyCell_Get(cell);
    if (! result) {
      PyErr_SetString(PyExc_ValueError, "cell is empty");
    }
  }

  return result;
}


static PyObject *m_setderef(PyObject *mod, PyObject *args) {

  // checked

  PyObject *cell = NULL, *value = NULL;

  if (! PyArg_ParseTuple(args, "O!O", &PyCell_Type, &cell, &value))
    return NULL;

  if (PyCell_Set(cell, value)) {
    return NULL;

  } else {
    Py_RETURN_NONE;
  }
}


static PyObject *m_clearderef(PyObject *mod, PyObject *cell) {

  // checked

  if (! PyCell_Check(cell)) {
    PyErr_SetString(PyExc_TypeError, "clearderef argument 1 must be a cell");
    return NULL;

  } else if (PyCell_Set(cell, NULL)) {
    return NULL;

  } else {
    Py_RETURN_NONE;
  }
}


static PyObject *m_reapply(PyObject *mod, PyObject *args, PyObject *kwds) {

  // checked

  PyObject *work = NULL, *result = NULL;
  long count = 0;

  static char *keywords[] = { "work", "data", "count", NULL };

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOl", keywords,
				    &work, &result, &count))
    return NULL;

  PyObject *data = result;
  Py_INCREF(data);

  while (count-- > 0) {
    result = PyObject_CallFunctionObjArgs(work, data, NULL);
    Py_DECREF(data);

    if (! result)
      break;

    data = result;
  }

  return result;
}


static PyObject *m_pass(PyObject *mod, PyObject *args, PyObject *kwds) {
  Py_RETURN_NONE;
}


static PyObject *m_build_tuple(PyObject *mod, PyObject *values) {

  // checked

  return PySequence_Tuple(values);
}


static PyObject *m_build_list(PyObject *mod, PyObject *values) {

  // checked

  return PySequence_List(values);
}


static PyObject *m_build_set(PyObject *mod, PyObject *values) {

  // checked

  return PySet_New(values);
}


static PyObject *m_build_dict(PyObject *mod, PyObject *values,
			      PyObject *kwds) {

  // checked

  PyObject *collect;
  PyObject *result;
  PyObject *item;

  int count = values? PyTuple_GET_SIZE(values): 0;

  result = PyDict_New();
  if (! count)
    return result;

  /* I duplicate values into a new tuple because I'm not entirely sure
     just how safe it is to modify that in-place. */
  collect = PyTuple_New(count);

  while (count--) {
    item = PyTuple_GET_ITEM(values, count);

    /* because we support items as either an arbitrary iterable with
       len 2, or a pair (which may be proper), we need to potentially
       convert pairs via unpack */

    if (SibPair_CheckExact(item) && SibPair_Check(SibPair_CDR(item))) {
      /* pair of more than one link, convert to iterator */
      item = SibPair_Unpack(item);
    } else {
      Py_INCREF(item);
    }

    PyTuple_SET_ITEM(collect, count, item);
  }

  if (unlikely(PyDict_MergeFromSeq2(result, collect, 1))) {
    Py_DECREF(collect);
    Py_DECREF(result);
    return NULL;

  } else {
    Py_DECREF(collect);
  }

  if (unlikely(PyDict_Merge(result, kwds, 1))) {
    Py_DECREF(result);
    return NULL;
  }

  return result;
}


static PyMethodDef methods[] = {

  { "reapply", (PyCFunction) m_reapply, METH_VARARGS|METH_KEYWORDS,
    "reapply(func, data, count) -> result data\n"
    "Calls `data = func(data)` count times (or until an exception is\n"
    "raised), and returns the final data value." },

  { "_pass", (PyCFunction) m_pass, METH_VARARGS|METH_KEYWORDS,
    "pass(*args, **kwds) -> None\n" },

  { "build_tuple", (PyCFunction) m_build_tuple, METH_VARARGS,
    "build_tuple(*args) -> args" },

  { "build_list", (PyCFunction) m_build_list, METH_VARARGS,
    "build_list(*args) -> list(args)" },

  { "build_set", (PyCFunction) m_build_set, METH_VARARGS,
    "build_set(*args) -> set(args)" },

  { "build_dict",  (PyCFunction) m_build_dict, METH_VARARGS|METH_KEYWORDS,
    "build_dict(*items) -> dict(items)" },

  { "getderef", (PyCFunction) m_getderef, METH_O,
    "getderef(cell) -> object\n"
    "Returns the object contained in a cell, or raises a ValueError if the\n"
    "cell is empty." },

  { "setderef", (PyCFunction) m_setderef, METH_VARARGS,
    "setderef(cell, value) -> None\n"
    "Assigns value to a cell." },

  { "clearderef", (PyCFunction) m_clearderef, METH_O,
    "clearderef(cell) -> None\n"
    "Clears a cell." },

  { NULL, NULL, 0, NULL },
};


static struct PyModuleDef ctypes = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.lib._types",
  .m_doc = DOCSTR,
  .m_size = -1,
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


#define STR_CONST(which, val) {			\
    if (! (which))				\
      which = PyUnicode_FromString(val);	\
  }


PyMODINIT_FUNC PyInit__types(void) {

  // checked

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

  PyObject *mod = PyModule_Create(&ctypes);
  if (! mod)
    return NULL;

  if (sib_types_atom_init(mod) ||
      sib_types_pair_init(mod) ||
      sib_types_tco_init(mod) ||
      sib_types_values_init(mod)) {

    Py_DECREF(mod);
    return NULL;
  }

  return mod;
}


/* The end. */
