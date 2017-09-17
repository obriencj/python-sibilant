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


static PyObject *c_tailcall(PyObject *self, PyObject *args) {
  PyObject *partial = NULL;
  PyObject *TailCall = NULL;
  PyObject *fun = NULL;

  PyObject *bounce = NULL;
  PyObject *tmp = NULL;

  if (! PyArg_ParseTuple(args, "OOO", &partial, &TailCall, &fun)) {
    return NULL;
  }

  tmp = PyObject_GetAttrStr(fun, "_tco_enable");
  if (tmp == None) {
    PyErr_Clear();
    return fun;

  } else if (tmp == Py_False) {
    Py_DECREF(tmp);
    return fun;

  } else {
    Py_DECREF(tmp);
  }

  tmp = PyObject_GetAttrStr(fun, "_tco_original");
  if (tmp == None) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  bounce = PyObject_CallObject(partial, "OO", TailCall, fun);
  PyObject_SetAttrStr(bounce, "_tco_original", fun);

  return bounce;
}


static PyObject *cell_set_value(PyObject *self, PyObject *args) {
  PyObject *cell = NULL;
  PyObject *val = NULL;

  if (! PyArg_ParseTuple(args, "O!O", &PyCell_Type, &cell, &val))
    return NULL;

  PyCell_Set(cell, val);

  Py_RETURN_NONE;
}


static PyMethodDef methods[] = {
  { "ctailcall", c_tailcall, METH_VARARGS,
    "" },

  { "ctrampoline", c_trampoline, METH_VARARGS,
    "" },

  { NULL, NULL, 0, NULL },
};


PyMODINIT_FUNC init_frame() {
  Py_InitModule("sibilant.compiler._tco", methods);
}


/* The end. */
