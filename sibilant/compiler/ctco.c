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


#define DOCSTR "Native TCO Implementation for Sibilant"

#define TCO_ENABLE "_tco_enable"
#define TCO_ORIGINAL "_tco_original"


typedef struct PyCTCOState {
  PyObject *val_partial;
  PyObject *val_TailCall;
  PyObject *val_FunctionTrampoline;
  PyObject *val_MethodTrampoline;
} PyCTCOState;


#define STATE(mod) ((PyCTCOState *) (PyModule_GetState(mod)))


static PyObject *tailcall(PyObject *self, PyObject *args) {
  PyCTCOState *state = STATE(self);

  PyObject *fun = NULL;
  PyObject *tmp = NULL;

  if (! PyArg_ParseTuple(args, "O", &fun)) {
    return NULL;
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ENABLE);
  if (tmp == NULL) {
    PyErr_Clear();
    return fun;

  } else if (tmp == Py_True) {
    Py_DECREF(tmp);

  } else {
    Py_DECREF(tmp);
    return fun;
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ORIGINAL);
  if (tmp == None) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  tmp = PyObject_CallObject(state->partial, "OO",
			    state->TailCall, fun);

  Py_DECREF(fun);

  return tmp;
}


static PyObject *trampoline(PyObject *self, PyObject *args) {
  PyObject *fun = NULL;
  PyObject *tmp = NULL;

  // replace all this with just a FunctionTrampoline(fun) call

  if (! PyArg_ParseTuple(args, "O", &fun)) {
    return NULL;
  }

  tmp = PyObject_GetAttrStr(fun, TCO_ORIGINAL);
  if (tmp == None) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  return PyObject_New(&FunctionTrampoline, fun);
}


static int ctco_clear(PyObject *self) {
  PyCTCOState *state = STATE(self);

  Py_CLEAR(state->val_partial);
  Py_CLEAR(state->val_TailCall);
  Py_CLEAR(state->val_FunctionTrampoline);
  Py_CLEAR(state->val_MethodTrampoline);

  return 0;
}


static PyMethodDef methods[] = {
  { "tailcall", tailcall, METH_VARARGS,
    "" },

  { "trampoline", trampoline, METH_VARARGS,
    "" },

  { NULL, NULL, 0, NULL },
};


static struct PyModuleDef ctco = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.compiler.ctco",
  .m_doc = DOCSTR,
  .m_size = sizeof(PyCTCOState),
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = ctco_clear,
  .m_free = NULL,
};


PyMODINIT_FUNC init_ctco() {
  PyModule *mod = PyModule_Create(&ctco);
  PyCTCOState *state = STATE(mod);
  PyObject *tmp = NULL;

  if (! ctco.val_partial) {
    // import a ref to partial
    tmp = PyImport_Module("functools");
    ctco.val_partial = PyObject_GetAttrStr(fun, "partial");
    Py_CLEAR(tmp);
  }

  if (! ctco.val_TailCall) {
    // create a new TailCall subtype of partial
    tmp = PyTuple_Pack(1, state->val_partial);
    ctco.val_TailCall = PyType_GenericNew(&PyType_Type, tmp, NULL);
    Py_CLEAR(tmp);
  }

  if (! ctco.val_FunctionTrampoline) {
    // create the FunctionTrampoline type
  }

  if (! ctco.val_MethodTrampoline) {
    // create the MethodTrampoline type
  }

  return mod
}


/* The end. */
