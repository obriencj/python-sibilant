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
   sibilant._tco

   Native implementation of the TCO trampoline and state for
   sibilant. These functions are re-exported from the sibilant.tco
   module.

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

    "sibilant.TailCall",
    sizeof(TailCall),
    0,

    .tp_new = PyType_GenericNew,
    .tp_dealloc = tailcall_dealloc,
    .tp_call = tailcall_call,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_getattro = PyObject_GenericGetAttr,
};


typedef struct {
  PyObject_HEAD
  PyObject *tco_original;
} Trampoline;


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


static PyObject *trampoline_repr(PyObject *self) {
  PyObject *original = ((Trampoline *)self)->tco_original;
  return PyUnicode_FromFormat("<trampoline for %R>", original);
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


static PyGetSetDef trampoline_func_getset[] = {
  { TCO_ORIGINAL,
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { TCO_ENABLE,
    (getter) trampoline_tco_enable, NULL,
    "", NULL },

  { "__code__",
    (getter) get_original, (setter) set_original,
    "", "__code__" },

  { "__defaults__",
    (getter) get_original, (setter) set_original,
    "", "__defaults__" },

  { "__kwdefaults__",
    (getter) get_original, (setter) set_original,
    "", "__kwdefaults__" },

  { "__annotations__",
    (getter) get_original, (setter) set_original,
    "", "__annotations__" },

  { "__dict__",
    (getter) get_original, (setter) set_original,
    "", "__dict__" },

  { "__name__",
    (getter) get_original, (setter) set_original,
    "", "__name__" },

  { "__qualname__",
    (getter) get_original, (setter) set_original,
    "", "__qualname__" },

  { NULL },
};


static PyGetSetDef trampoline_meth_getset[] = {
  { TCO_ORIGINAL,
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { TCO_ENABLE,
    (getter) trampoline_tco_enable, NULL,
    "", NULL },

  { "__dict__",
    (getter) get_original, (setter) set_original,
    "", "__dict__" },

  { "__doc__",
    (getter) get_original, (setter) set_original,
    "", "__doc__" },

  { "__name__",
    (getter) get_original, (setter) set_original,
    "", "__name__" },

  { "__qualname__",
    (getter) get_original, (setter) set_original,
    "", "__qualname__" },

  { "__self__",
    (getter) get_original, (setter) set_original,
    "", "__self__" },

  { "__func__",
    (getter) get_original, (setter) set_original,
    "", "__func__" },

  { "__text_signature__",
    (getter) get_original, (setter) set_original,
    "", "__text_signature__" },

  { NULL },
};


static PyObject *descr_get(PyObject *self,
			   PyObject *inst, PyObject *owner) {

  /**
     When a function trampoline member is retrieved from an object
     instance, this will be called to enable the function trampoline
     to wrap itself as a method trampoline.

     We do that by invoking the same __get__ call on our function
     trampoline's underlying callable, and then allocating a method
     trampoline to wrap it. The method trampoline only differs from a
     function trampoline in that it will not re-wrap in this fashion.
   */

  PyObject *tmp = NULL;
  Trampoline *tramp = NULL;

  if (unlikely((! inst) || inst == Py_None)) {
    Py_INCREF(self);
    return self;

  } else {
    // tmp = self._tco_original.__get__(inst, owner)
    tmp = ((Trampoline *) self)->tco_original;
    tmp = PyObject_CallMethodObjArgs(tmp, __get__, inst, owner, NULL);
    if (unlikely (! tmp))
      return NULL;

    tramp = PyObject_New(Trampoline, &MethodTrampolineType);
    if (unlikely(! tramp)) {
      return NULL;
    }

    tramp->tco_original = tmp;
    return (PyObject *) tramp;
  }
}


static PyTypeObject FunctionTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.FunctionTrampoline",
    sizeof(Trampoline),
    0,

    .tp_new = PyType_GenericNew,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = descr_get,
    .tp_getset = trampoline_func_getset,
    .tp_call = trampoline_call,
    .tp_repr = trampoline_repr,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_getattro = PyObject_GenericGetAttr,
};


static PyTypeObject MethodTrampolineType = {
    PyVarObject_HEAD_INIT(NULL, 0)

    "sibilant.MethodTrampoline",
    sizeof(Trampoline),
    0,

    .tp_new = PyType_GenericNew,
    .tp_dealloc = trampoline_dealloc,
    .tp_descr_get = NULL,
    .tp_getset = trampoline_meth_getset,
    .tp_call = trampoline_call,
    .tp_repr = trampoline_repr,
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_getattro = PyObject_GenericGetAttr,
};


static PyObject *tailcall(PyObject *self, PyObject *args) {
  PyObject *fun = NULL;
  PyObject *tmp = NULL;

  if (unlikely(! PyArg_ParseTuple(args, "O", &fun))) {
    return NULL;
  }

  tmp = (PyObject *) fun->ob_type;
  if ((tmp == (PyObject *) &FunctionTrampolineType) ||
      (tmp == (PyObject *) &MethodTrampolineType)) {

    tmp = ((Trampoline *) fun)->tco_original;
    Py_INCREF(tmp);
    fun = tmp;

  } else {
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
  }

  tmp = (PyObject *) PyObject_New(TailCall, &TailCallType);
  if (unlikely(! tmp))
    return NULL;

  ((TailCall *) tmp)->fun = fun;
  return tmp;
}


static PyObject *trampoline(PyObject *self, PyObject *args) {
  PyObject *fun = NULL;
  PyObject *tmp = NULL;
  Trampoline *tramp = NULL;

  if (unlikely(! PyArg_ParseTuple(args, "O", &fun))) {
    return NULL;
  }

  Py_INCREF(fun);

  // tmp = getattr(fun, "_tco_original", fun)
  tmp = PyObject_GetAttr(fun, _tco_original);
  if (likely(! tmp)) {
    PyErr_Clear();

  } else {
    Py_DECREF(fun);
    fun = tmp;
  }

  tramp = PyObject_New(Trampoline, &FunctionTrampolineType);
  if (unlikely(! tramp)) {
    return NULL;
  }

  tramp->tco_original = fun;
  return (PyObject *) tramp;
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
  .m_name = "sibilant._tco",
  .m_doc = DOCSTR,
  .m_size = -1,
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


PyMODINIT_FUNC PyInit__tco(void) {
  if (! __get__) {
    __get__ = PyUnicode_FromString("__get__");
  }
  if (! _tco_original) {
    _tco_original = PyUnicode_FromString(TCO_ORIGINAL);
  }
  if (! _tco_enable) {
    _tco_enable = PyUnicode_FromString(TCO_ENABLE);
  }

  if (PyType_Ready(&TailCallType) < 0)
    return NULL;

  if (PyType_Ready(&FunctionTrampolineType) < 0)
    return NULL;

  if (PyType_Ready(&MethodTrampolineType) < 0)
    return NULL;

  return PyModule_Create(&ctco);
}


/* The end. */
