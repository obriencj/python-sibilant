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


#include "ctypes.h"


#define DOCSTR "Native Sibilant core types"


#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif


#if 0
#define DEBUGMSG(msg, obj) { \
  printf("** " msg " "); \
  PyObject_Print(((PyObject *) (obj)), stdout, 0);	\
  printf("\n"); \
  }
#else
#define DEBUGMSG(msg, obj) {}
#endif


static PyObject *intern_syms = NULL;
static PyObject *intern_kwds = NULL;


static void pair_dealloc(PyObject *self) {
  SibPair *s = (SibPair *) self;

  if (s->weakrefs != NULL)
    PyObject_ClearWeakRefs(self);

  Py_DECREF(s->head);
  Py_DECREF(s->tail);
  Py_XDECREF(s->position);

  Py_TYPE(self)->tp_free(self);
}


static Py_ssize_t pair_length(PyObject *n) {
  return 2;
}


static PySequenceMethods pair_as_sequence = {
  .sq_length = pair_length,
};


PyTypeObject SibPairType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.pair",
  sizeof(SibPair),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_dealloc = pair_dealloc,
  .tp_weaklistoffset = offsetof(SibPair, weakrefs),

  //.tp_iter = pair_iter,
  .tp_as_sequence = &pair_as_sequence,

  // .tp_print = pair_print,
  // .tp_repr = pair_repr,
};


static void symbol_dealloc(PyObject *self) {
  SibInternedAtom *s = (SibInternedAtom *) self;

  if (s->weakrefs != NULL)
    PyObject_ClearWeakRefs(self);

  PyDict_DelItem(intern_syms, s->name);

  Py_DECREF(s->name);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *symbol_new(PyTypeObject *type,
			    PyObject *args, PyObject *kwds) {

  PyObject *name = NULL;

  if (! PyArg_ParseTuple(args, "O", &name))
    return NULL;

  return sib_symbol(name);
}


PyTypeObject SibSymbolType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.symbol",
  sizeof(SibInternedAtom),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_new = symbol_new,
  .tp_dealloc = symbol_dealloc,
  .tp_weaklistoffset = offsetof(SibInternedAtom, weakrefs),

  // .tp_print = atom_print,
  // .tp_repr = atom_repr,
};


static void keyword_dealloc(PyObject *self) {
  SibInternedAtom *s = (SibInternedAtom *) self;

  if (s->weakrefs != NULL)
    PyObject_ClearWeakRefs(self);

  PyDict_DelItem(intern_kwds, s->name);

  Py_DECREF(s->name);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *keyword_new(PyTypeObject *type,
			     PyObject *args, PyObject *kwds) {

  PyObject *name = NULL;

  if (! PyArg_ParseTuple(args, "O", &name))
    return NULL;

  return sib_keyword(name);
}


PyTypeObject SibKeywordType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.keyword",
  sizeof(SibInternedAtom),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_new = keyword_new,
  .tp_dealloc = keyword_dealloc,
  .tp_weaklistoffset = offsetof(SibInternedAtom, weakrefs),

  // .tp_print = atom_print,
  // .tp_repr = atom_repr,
};


static void nil_dealloc(PyObject *self) {
  ;
}


static PyObject *nil_new(PyTypeObject *type,
			 PyObject *args, PyObject *kwds) {

  if (PyTuple_Size(args) || (kwds && PyDict_Size(kwds))) {
    PyErr_SetString(PyExc_TypeError, "NilType takes no arguments");
    return NULL;
  }

  Py_INCREF(Sib_Nil);
  return Sib_Nil;
}


static Py_ssize_t nil_length(PyObject *n) {
  return 0;
}


static PySequenceMethods nil_as_sequence = {
  .sq_length = nil_length,
};


static int nil_bool(PyObject *v) {
  return 0;
}


static PyNumberMethods nil_as_number = {
  .nb_bool = (inquiry) nil_bool,
};


PyTypeObject SibNilType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.nil",
  0,
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_new = nil_new,
  .tp_dealloc = nil_dealloc,
  //.tp_repr = nil_repr,
  //.tp_iter = nil_iter,
  .tp_as_number = &nil_as_number,
  .tp_as_sequence = &nil_as_sequence,
};


PyObject _SibNil = {
  _PyObject_EXTRA_INIT
  1, &SibNilType
};


inline PyObject *sib_symbol(PyObject *name) {
  SibInternedAtom *n = NULL;

  if(! name) {
    PyErr_SetString(PyExc_TypeError, "symbol requires a name");
    return NULL;

  } else if (! PyUnicode_CheckExact(name)) {
    name = PyObject_Str(name);

  } else{
    Py_INCREF(name);
  }

  n = (SibInternedAtom *) PyDict_GetItem(intern_syms, name);

  if (! n) {
    n = (SibInternedAtom *) PyObject_New(SibInternedAtom, &SibSymbolType);
    n->name = name;
    n->weakrefs = NULL;

    DEBUGMSG("allocating and interning new symbol", n);

    PyDict_SetItem(intern_syms, name, (PyObject *) n);

    // make the intern a borrowed ref. This allows refcount to drop to
    // zero, at which point the deallocation of the symbol will clear
    // it from the intern dict.
    Py_DECREF(n);

  } else {
    DEBUGMSG("returning previously interned symbol", n);

    Py_INCREF(n);
    Py_DECREF(name);
  }

  return (PyObject *) n;
}


inline PyObject *sib_keyword(PyObject *name) {
  SibInternedAtom *n = NULL;

  if (! n) {
    n = PyObject_New(SibInternedAtom, &SibKeywordType);

    Py_INCREF(name);
    n->name = name;
  }

  return (PyObject *) n;
}


inline PyObject *sib_pair(PyObject *head, PyObject *tail) {
  SibPair *self = NULL;

  self = PyObject_New(SibPair, &SibPairType);

  Py_INCREF(head);
  self->head = head;

  Py_INCREF(tail);
  self->tail = tail;

  return (PyObject *) self;
}


static PyMethodDef methods[] = {

  { NULL, NULL, 0, NULL },
};


static struct PyModuleDef ctypes = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.ctypes",
  .m_doc = DOCSTR,
  .m_size = -1,
  .m_methods = methods,
  .m_slots = NULL,
  .m_traverse = NULL,
  .m_clear = NULL,
  .m_free = NULL,
};


PyMODINIT_FUNC PyInit_ctypes(void) {

  PyObject *mod, *dict;

  SibPairType.tp_new = PyType_GenericNew;
  if (PyType_Ready(&SibPairType) < 0)
    return NULL;

  if (PyType_Ready(&SibNilType) < 0)
    return NULL;

  if (PyType_Ready(&SibKeywordType) < 0)
    return NULL;

  if (PyType_Ready(&SibSymbolType) < 0)
    return NULL;

  mod = PyModule_Create(&ctypes);
  if (! mod)
    return NULL;

  if (! intern_syms) {
    intern_syms = PyDict_New();
    if (! intern_syms)
      return NULL;
  }

  if (! intern_kwds) {
    intern_kwds = PyDict_New();
    if (! intern_kwds)
      return NULL;
  }

  dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "nil", Sib_Nil);
  PyDict_SetItemString(dict, "pair", (PyObject *) &SibPairType);
  PyDict_SetItemString(dict, "symbol", (PyObject *) &SibSymbolType);
  PyDict_SetItemString(dict, "keyword", (PyObject *) &SibKeywordType);

  return mod;
}


/* The end. */
