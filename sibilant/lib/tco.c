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


static PyObject *m_tailcall_full(PyObject *self,
				 PyObject *args, PyObject *kwds) {

  // checked

  PyObject *work = NULL;
  PyObject *tmp = NULL;

  if (PyTuple_GET_SIZE(args) <= 1) {
    if (unlikely(! PyArg_ParseTuple(args, "O", &work))) {
      return NULL;
    }
    args = PyTuple_New(0);

  } else {
    work = PyTuple_GET_ITEM(args, 0);
    args = PyTuple_GetSlice(args, 1, PyTuple_GET_SIZE(args));
  }

  // work and kwds are borrowed, args is ref'd

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
      // not tailcall enabled, so invoke directly. work is still
      // borrowed at this point, and args are ref'd
      Py_XDECREF(tmp);
      tmp = PyObject_Call(work, args, kwds);
      Py_DECREF(args);
      return tmp;
    }
  }

  // kwds are borrowed, work and args are ref'd, let the new tailcall
  // steal those references. kwds needs to be incref'd

  tmp = SibTailcall_New(work);
  ((Tailcall *) tmp)->args = args;
  ((Tailcall *) tmp)->kwds = kwds;
  Py_XINCREF(kwds);
  return tmp;
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

    Py_CLEAR(work);
    Py_CLEAR(args);
    Py_CLEAR(kwds);
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
  { "_tco_original",
    (getter) trampoline_tco_original, NULL,
    "", NULL},

  { "_tco_enable",
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
    .tp_getattro = PyObject_GenericGetAttr,
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
    .tp_getattro = PyObject_GenericGetAttr,
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


static PyMethodDef methods[] = {

  { "tailcall_full", (PyCFunction) m_tailcall_full, METH_VARARGS|METH_KEYWORDS,
    "tailcall_full(function, *args, **kwds) ->"
    " tailcall(function)(*args, **kwds)" },

  { "trampoline", m_trampoline, METH_VARARGS,
    "wraps a callable in a trampoline. A trampoline will catch returned"
    " tailcall instances and invoke their function and arguments"
    " in-place. The trampoline will continue catching and bouncing until"
    " a non-tailcall instance is returned or an exception is raised." },

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
