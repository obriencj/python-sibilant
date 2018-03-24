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
   sibilant._atoms

   Sibilant's keyword and symbol types.

   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
 */


#include "sibilant.h"


#define DOCSTR "Sibilant's keyword and symbol types"


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


/* === interned atom === */


#define ATOM_NAME(o) (((SibInternedAtom *)self)->name)


static PyObject *atom_new(PyObject *name,
			  PyObject *intern,
			  PyTypeObject *type) {

  SibInternedAtom *n = NULL;

  if(! name) {
    PyErr_SetString(PyExc_TypeError, "interned atom requires a name");
    return NULL;

  } else if (! PyUnicode_CheckExact(name)) {
    name = PyObject_Str(name);

  } else{
    Py_INCREF(name);
  }

  n = (SibInternedAtom *) PyDict_GetItem(intern, name);

  if (! n) {
    n = PyObject_New(SibInternedAtom, type);
    n->name = name;
    n->weakrefs = NULL;

    // DEBUGMSG("allocating and interning new atom", n);

    PyDict_SetItem(intern, name, (PyObject *) n);

    // make the intern a borrowed ref. This allows refcount to drop to
    // zero, at which point the deallocation of the symbol will clear
    // it from the intern dict.
    Py_DECREF(n);

  } else {
    // DEBUGMSG("returning previously interned atom", n);

    Py_INCREF(n);
    Py_DECREF(name);
  }

  return (PyObject *) n;
}


static PyObject *atom_repr(PyObject *self) {
  return PyUnicode_FromFormat("<%s %R>",
			      self->ob_type->tp_name,
			      ATOM_NAME(self));
}


static PyObject *atom_str(PyObject *self) {
  PyObject *name = ATOM_NAME(self);
  Py_INCREF(name);
  return name;
}


static void atom_rewrap(PyObject *vals, unaryfunc conv) {
  for (int index = PyList_Size(vals); index--; ) {
    PyList_SetItem(vals, index, conv(PyList_GET_ITEM(vals, index)));
  }
}


/* === symbol === */


static PyObject *intern_syms = NULL;


PyObject *sib_symbol(PyObject *name) {
  return atom_new(name, intern_syms, &SibSymbolType);
}


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

  if (kwds && PyDict_Size(kwds)) {
    PyErr_SetString(PyExc_TypeError, "symbol takes no named arguments");
    return NULL;
  }

  if (! PyArg_ParseTuple(args, "O:symbol", &name))
    return NULL;

  return sib_symbol(name);
}


static PyObject *symbol_split(PyObject *self,
			      PyObject *args, PyObject *kwds) {

  PyObject *name = ATOM_NAME(self);
  PyObject *method = NULL;
  PyObject *result = NULL;

  method = PyObject_GetAttr(name, _str_split);
  if (! method)
    return NULL;

  result = PyObject_Call(method, args, kwds);
  Py_CLEAR(method);
  if (! result)
    return NULL;

  atom_rewrap(result, sib_symbol);
  return result;
}


static PyObject *symbol_rsplit(PyObject *self,
			       PyObject *args, PyObject *kwds) {

  PyObject *name = ATOM_NAME(self);
  PyObject *method = NULL;
  PyObject *result = NULL;

  method = PyObject_GetAttr(name, _str_rsplit);
  if (! method)
    return NULL;

  result = PyObject_Call(method, args, kwds);
  Py_CLEAR(method);
  if (! result)
    return NULL;

  atom_rewrap(result, sib_symbol);
  return result;
}


static PyMethodDef symbol_methods[] = {
  { "split", (PyCFunction) symbol_split, METH_VARARGS|METH_KEYWORDS,
    "S.split(sep=None, maxsplit=-1) -> list of symbols" },

  { "rsplit", (PyCFunction) symbol_rsplit, METH_VARARGS|METH_KEYWORDS,
    "S.rsplit(sep=None, maxsplit=-1) -> list of symbols" },

  { NULL, NULL, 0, NULL },
};


PyTypeObject SibSymbolType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "symbol",
  sizeof(SibInternedAtom),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_methods = symbol_methods,
  .tp_new = symbol_new,
  .tp_dealloc = symbol_dealloc,
  .tp_weaklistoffset = offsetof(SibInternedAtom, weakrefs),

  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


/* === keyword === */


static PyObject *intern_kwds = NULL;


PyObject *sib_keyword(PyObject *name) {
  PyObject *clean;
  PyObject *result;

  if(! name) {
    PyErr_SetString(PyExc_TypeError, "keywrod requires a name");
    return NULL;

  } else if (! PyUnicode_CheckExact(name)) {
    name = PyObject_Str(name);

  } else{
    Py_INCREF(name);
  }

  clean = PyObject_CallMethodObjArgs(name, _str_strip, _str_colon, NULL);
  result = atom_new(clean, intern_kwds, &SibKeywordType);

  Py_DECREF(name);
  Py_DECREF(clean);

  return result;
}


static PyObject *keyword_new(PyTypeObject *type,
			     PyObject *args, PyObject *kwds) {

  PyObject *name = NULL;

  if (kwds && PyDict_Size(kwds)) {
    PyErr_SetString(PyExc_TypeError, "keyword takes no named arguments");
    return NULL;
  }

  if (! PyArg_ParseTuple(args, "O:keyword", &name))
    return NULL;

  return sib_keyword(name);
}


static void keyword_dealloc(PyObject *self) {
  SibInternedAtom *s = (SibInternedAtom *) self;

  if (s->weakrefs != NULL)
    PyObject_ClearWeakRefs(self);

  PyDict_DelItem(intern_kwds, s->name);

  Py_DECREF(s->name);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *keyword_split(PyObject *self,
			       PyObject *args, PyObject *kwds) {

  PyObject *name = ATOM_NAME(self);
  PyObject *method = NULL;
  PyObject *result = NULL;

  method = PyObject_GetAttr(name, _str_split);
  if (! method)
    return NULL;

  result = PyObject_Call(method, args, kwds);
  Py_CLEAR(method);
  if (! result)
    return NULL;

  atom_rewrap(result, sib_keyword);
  return result;
}


static PyObject *keyword_rsplit(PyObject *self,
				PyObject *args, PyObject *kwds) {

  PyObject *name = ATOM_NAME(self);
  PyObject *method = NULL;
  PyObject *result = NULL;

  method = PyObject_GetAttr(name, _str_rsplit);
  if (! method)
    return NULL;

  result = PyObject_Call(method, args, kwds);
  Py_CLEAR(method);
  if (! result)
    return NULL;

  atom_rewrap(result, sib_keyword);
  return result;
}


static PyMethodDef keyword_methods[] = {
  { "split", (PyCFunction) keyword_split, METH_VARARGS|METH_KEYWORDS,
    "K.split(sep=None, maxsplit=-1) -> list of keywords" },

  { "rsplit", (PyCFunction) keyword_rsplit, METH_VARARGS|METH_KEYWORDS,
    "K.rsplit(sep=None, maxsplit=-1) -> list of keywords" },

  { NULL, NULL, 0, NULL },
};


PyTypeObject SibKeywordType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "keyword",
  sizeof(SibInternedAtom),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_methods = keyword_methods,
  .tp_new = keyword_new,
  .tp_dealloc = keyword_dealloc,
  .tp_weaklistoffset = offsetof(SibInternedAtom, weakrefs),

  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


/* === module === */


static struct PyModuleDef ext_atom = {
  .m_base = PyModuleDef_HEAD_INIT,
  .m_name = "sibilant.ext.atom",
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


PyMODINIT_FUNC PyInit_atom(void) {

  PyObject *mod, *dict;

  if (PyType_Ready(&SibKeywordType) < 0)
    return NULL;

  if (PyType_Ready(&SibSymbolType) < 0)
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

  mod = PyModule_Create(&ext_atom);
  if (! mod)
    return NULL;

  dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "symbol", (PyObject *) &SibSymbolType);
  PyDict_SetItemString(dict, "keyword", (PyObject *) &SibKeywordType);

  return mod;
}


/* The end. */
