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
   This is a native implementation of the TCO module for sibilant

   author: Christopher O'Brien  <obriencj@gmail.com>
   license: LGPL v.3
*/


#include <Python.h>


#define TCO_ENABLE "_tco_enable"
#define TCO_ORIGINAL "_tco_original"


static PyObject *tailcall(PyObject *self, PyObject *args) {
  PyObject *partial = NULL;
  PyObject *TailCall = NULL;
  PyObject *fun = NULL;

  PyObject *bounce = NULL;
  PyObject *tmp = NULL;

  if (! PyArg_ParseTuple(args, "OOO", &partial, &TailCall, &fun)) {
    return NULL;
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ENABLE);
  if (tmp == NULL) {
    PyErr_Clear();
    return fun;

  } else if (tmp == Py_False) {
    Py_DECREF(tmp);
    return fun;

  } else {
    Py_DECREF(tmp);
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ORIGINAL);
  if (tmp == None) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  bounce = PyObject_CallObject(partial, "OO", TailCall, fun);
  PyObject_SetAttrStr(bounce, TCO_ORIGINAL, fun);

  Py_DECREF(fun);

  return bounce;
}


static PyObject *trampoline(PyObject *self, PyObject *args) {
  PyObject *partial = NULL;
  PyObject *TailCall = NULL;
  PyObject *bounce = NULL;
  PyObject *fun = NULL;

  PyObject *tmp = NULL;

  if (! PyArg_ParseTuple(args, "OOOO", &partial, &TailCall, &bounce, &fun)) {
    return NULL;
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ORIGINAL);
  if (tmp == None) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  result = PyObject_CallObject(partial, "OOO", bounce, TailCall, fun);


  return fun;
}


static PyMethodDef methods[] = {
  { "ctailcall", tailcall, METH_VARARGS,
    "" },

  { "ctrampoline", trampoline, METH_VARARGS,
    "" },

  { "ctco_bounce", tco_bounce, METH_VARARGS,
    "" },

  { NULL, NULL, 0, NULL },
};


PyMODINIT_FUNC init_tco() {
  Py_InitModule("sibilant.compiler._tco", methods);
}


/* The end. */
