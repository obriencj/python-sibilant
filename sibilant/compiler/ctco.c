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
#include <object.h>


#define DOCSTR "Native TCO Implementation for Sibilant"

#define TCO_ENABLE "_tco_enable"
#define TCO_ORIGINAL "_tco_original"


#if (defined(__GNUC__) &&					\
     (__GNUC__ > 2 || (__GNUC__ == 2 && (__GNUC_MINOR__ > 95))))
  #define likely(x)   __builtin_expect(!!(x), 1)
  #define unlikely(x) __builtin_expect(!!(x), 0)
#else
  #define likely(x)   (x)
  #define unlikely(x) (x)
#endif


static PyTypeObject FunctionTrampolineType;
static PyTypeObject MethodTrampolineType;
static PyTypeObject TailCallType;

static PyObject *__get__ = NULL;
static PyObject *_tco_enable = NULL;
static PyObject *_tco_original = NULL;


typedef struct {
  PyObject_HEAD

  PyObject *fun;
  PyObject *args;
  PyObject *kwds;
} TailCall;


static int tailcall_init(PyObject *self, PyObject *args, PyObject *kwds) {

  PyObject *fun = NULL;
  static char *kwlist[] = { "fun", NULL };

  if (unlikely(! PyArg_ParseTupleAndKeywords(args, kwds, "O", kwlist, &fun)))
    return -1;

  Py_INCREF(fun);
  ((TailCall *) self)->fun = fun;
  ((TailCall *) self)->args = NULL;
  ((TailCall *) self)->kwds = NULL;

  return 0;
}


static void tailcall_dealloc(PyObject *self) {
  TailCall *s = (TailCall *) self;

  Py_XDECREF(s->fun);
  Py_XDECREF(s->args);
  Py_XDECREF(s->kwds);

  Py_TYPE(self)->tp_free(self);
}


static PyObject *tailcall_call(PyObject *self,
			       PyObject *args, PyObject *kwds) {

  TailCall *s = (TailCall *) self;

  Py_INCREF(args);
  s->args = args;

  Py_XINCREF(kwds);
  s->kwds = kwds;

  Py_INCREF(self);
  return self;
}


static PyTypeObject TailCallType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.compiler.ctco.TailCall",
    sizeof(TailCall),
    0,

    .tp_init = tailcall_init,
    .tp_dealloc = tailcall_dealloc,
    .tp_call = tailcall_call,
    .tp_flags = Py_TPFLAGS_DEFAULT,

    .tp_print = NULL,
    .tp_getattr = NULL,
    .tp_setattr = NULL,
    .tp_as_async = NULL,
    .tp_descr_get = NULL,
    .tp_getset = NULL,
    .tp_repr = NULL,
    .tp_as_number = NULL,
    .tp_as_sequence = NULL,
    .tp_as_mapping = NULL,
    .tp_hash = NULL,
    .tp_str = NULL,
    .tp_getattro = NULL,
    .tp_setattro = NULL,
    .tp_as_buffer = NULL,
};


typedef struct {
  PyObject_HEAD
  PyObject *tco_original;
} Trampoline;


static int trampoline_init(PyObject *self,
			   PyObject *args, PyObject *kwds) {

  PyObject *fun = NULL;
  static char *kwlist[] = { "fun", NULL };

  if (unlikely(! PyArg_ParseTupleAndKeywords(args, kwds, "O", kwlist, &fun)))
    return -1;

  Py_INCREF(fun);
  ((Trampoline *) self)->tco_original = fun;

  return 0;
}


static void trampoline_dealloc(PyObject *self) {

  Py_CLEAR(((Trampoline *) self)->tco_original);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *trampoline_call(PyObject *self,
				 PyObject *args, PyObject *kwds) {

  PyObject *work = ((Trampoline *) self)->tco_original;
  TailCall *res = NULL;

  work = PyObject_Call(work, args, kwds);

  while (likely(work) && work->ob_type == &TailCallType) {
    res = (TailCall *) work;
    work = PyObject_Call(res->fun, res->args, res->kwds);
    Py_DECREF(res);
  }

  return work;
}


static PyObject *trampoline_tco_original(PyObject *self) {
  PyObject *tco_original = ((Trampoline *)self)->tco_original;

  if (unlikely(! tco_original)) {
    return NULL;

  } else {
    Py_INCREF(tco_original);
    return tco_original;
  }
}


static PyObject *trampoline_tco_enable(PyObject *self) {
  PyObject *t = Py_True;
  Py_INCREF(t);
  return t;
}


static PyObject *get_original(PyObject *self, char *name) {
  PyObject *tco_original = ((Trampoline *)self)->tco_original;

  if (unlikely(! tco_original)) {
    return NULL;

  } else {
    return PyObject_GetAttrString(tco_original, name);
  }
}


static int set_original(PyObject *self, PyObject *val, char *name) {
  PyObject *tco_original = ((Trampoline *)self)->tco_original;
  return PyObject_SetAttrString(tco_original, name, val);
}


static PyGetSetDef trampoline_getset[] = {
  { TCO_ORIGINAL,
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { TCO_ENABLE,
    (getter) trampoline_tco_enable, NULL,
    "", NULL },

  { "__name__",
    (getter) get_original, (setter) set_original,
    "", "__name__" },

  { "__qualname__",
    (getter) get_original, (setter) set_original,
    "", "__qualname__" },

  { "__doc__",
    (getter) get_original, (setter) set_original,
    "", "__doc__" },

  { NULL },
};


static PyObject *descr_get(PyObject *self,
			   PyObject *inst, PyObject *owner) {
  PyObject *tmp = NULL;

  if (unlikely((! inst) || inst == Py_None)) {
    Py_INCREF(self);
    return self;

  } else {
    tmp = ((Trampoline *) self)->tco_original;
    tmp = PyObject_CallMethodObjArgs(tmp, __get__, inst, owner, NULL);
    if (unlikely (! tmp))
      return NULL;

    self = PyObject_CallFunctionObjArgs((PyObject *) &MethodTrampolineType,
					tmp, NULL);
    Py_DECREF(tmp);
    return self;
  }
}


static PyTypeObject FunctionTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.compiler.ctco.FunctionTrampoline",
    sizeof(Trampoline),
    0,

    .tp_init = trampoline_init,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = descr_get,
    .tp_getset = trampoline_getset,
    .tp_call = trampoline_call,
    .tp_flags = Py_TPFLAGS_DEFAULT,

    .tp_print = NULL,
    .tp_getattr = NULL,
    .tp_setattr = NULL,
    .tp_as_async = NULL,
    .tp_repr = NULL,
    .tp_as_number = NULL,
    .tp_as_sequence = NULL,
    .tp_as_mapping = NULL,
    .tp_hash = NULL,
    .tp_str = NULL,
    .tp_getattro = NULL,
    .tp_setattro = NULL,
    .tp_as_buffer = NULL,
};


static PyTypeObject MethodTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.compiler.ctco.MethodTrampoline",
    sizeof(Trampoline),
    0,

    .tp_init = trampoline_init,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = NULL,
    .tp_getset = trampoline_getset,
    .tp_call = trampoline_call,
    .tp_flags = Py_TPFLAGS_DEFAULT,

    .tp_print = NULL,
    .tp_getattr = NULL,
    .tp_setattr = NULL,
    .tp_as_async = NULL,
    .tp_repr = NULL,
    .tp_as_number = NULL,
    .tp_as_sequence = NULL,
    .tp_as_mapping = NULL,
    .tp_hash = NULL,
    .tp_str = NULL,
    .tp_getattro = NULL,
    .tp_setattro = NULL,
    .tp_as_buffer = NULL,
};


static PyObject *tailcall(PyObject *self, PyObject *args) {
  PyObject *fun = NULL;
  PyObject *tmp = NULL;

  if (unlikely(! PyArg_ParseTuple(args, "O", &fun))) {
    return NULL;
  }

  Py_INCREF(fun);

  tmp = PyObject_GetAttr(fun, _tco_enable);
  if (tmp == NULL) {
    PyErr_Clear();
    return fun;

  } else if (PyObject_IsTrue(tmp)) {
    Py_DECREF(tmp);

  } else {
    Py_DECREF(tmp);
    return fun;
  }

  tmp = PyObject_GetAttr(fun, _tco_original);
  if (tmp == NULL) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  tmp = PyObject_CallFunctionObjArgs((PyObject *) &TailCallType,
				     fun, NULL);
  Py_DECREF(fun);

  return tmp;
}


static PyObject *trampoline(PyObject *self, PyObject *args) {
  PyObject *fun = NULL;
  PyObject *tmp = NULL;

  // replace all this with just a FunctionTrampoline(fun) call

  if (unlikely(! PyArg_ParseTuple(args, "O", &fun))) {
    return NULL;
  }

  Py_INCREF(fun);

  tmp = PyObject_GetAttr(fun, _tco_original);
  if (likely(! tmp)) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  tmp = PyObject_CallFunctionObjArgs((PyObject *) &FunctionTrampolineType,
				     fun, NULL);
  Py_DECREF(fun);

  return tmp;
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
  .m_size = -1,
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


PyMODINIT_FUNC PyInit_ctco() {
  if (! __get__) {
    __get__ = PyUnicode_FromString("__get__");
  }
  if (! _tco_original) {
    _tco_original = PyUnicode_FromString(TCO_ORIGINAL);
  }
  if (! _tco_enable) {
    _tco_enable = PyUnicode_FromString(TCO_ENABLE);
  }

  TailCallType.tp_new = PyType_GenericNew;
  TailCallType.tp_getattro = PyObject_GenericGetAttr;
  if (PyType_Ready(&TailCallType) < 0)
    return NULL;
  Py_INCREF(&TailCallType);

  FunctionTrampolineType.tp_new = PyType_GenericNew;
  FunctionTrampolineType.tp_getattro = PyObject_GenericGetAttr;
  if (PyType_Ready(&FunctionTrampolineType) < 0)
    return NULL;
  Py_INCREF(&FunctionTrampolineType);

  MethodTrampolineType.tp_new = PyType_GenericNew;
  MethodTrampolineType.tp_getattro = PyObject_GenericGetAttr;
  if (PyType_Ready(&MethodTrampolineType) < 0)
    return NULL;
  Py_INCREF(&MethodTrampolineType);

  return PyModule_Create(&ctco);
}


/* The end. */
