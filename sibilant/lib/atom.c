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


#include "types.h"


/* === interned atom === */


#define ATOM_NAME(o) (((SibInternedAtom *)self)->name)


static PyObject *atom_new(PyObject *name,
			  PyObject *intern,
			  PyTypeObject *type) {

  // checked

  if(! name) {
    PyErr_SetString(PyExc_TypeError, "interned atom requires a name");
    return NULL;

  } else if (! PyUnicode_CheckExact(name)) {
    name = PyObject_Str(name);

  } else{
    Py_INCREF(name);
  }

  // name is ref'd

  SibInternedAtom *atom = (SibInternedAtom *) PyDict_GetItem(intern, name);
  if (! atom) {
    atom = PyObject_New(SibInternedAtom, type);
    if(! atom) {
      Py_DECREF(name);
      return NULL;
    }

    atom->name = name;

    // DEBUGMSG("allocating and interning new atom", n);

    PyDict_SetItem(intern, name, (PyObject *) atom);

    // make the intern a borrowed ref. This allows refcount to drop to
    // zero, at which point the deallocation of the symbol will clear
    // it from the intern dict.
    Py_DECREF(atom);

  } else {
    // DEBUGMSG("returning previously interned atom", n);

    Py_INCREF(atom);
    Py_DECREF(name);
  }

  // DEBUGMSG("here is an atom", atom);
  // printf("**  refcount: %zi\n", ((PyObject *) atom)->ob_refcnt);

  return (PyObject *) atom;
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


static Py_ssize_t atom_len(PyObject *self) {
  PyObject *name = ATOM_NAME(self);
  return PyUnicode_GET_LENGTH(name);
}


static void atom_rewrap(PyObject *vals, unaryfunc conv) {

  // checked

  for (int index = PyList_Size(vals); index--; ) {
    PyList_SetItem(vals, index, conv(PyList_GET_ITEM(vals, index)));
  }
}


static PySequenceMethods atom_as_sequence = {
  .sq_length = atom_len,
};


/* === symbol === */


static PyObject *intern_syms = NULL;


PyObject *SibSymbol_FromString(PyObject *name) {

  // checked

  return atom_new(name, intern_syms, &SibSymbolType);
}


static void symbol_dealloc(PyObject *self) {

  // checked

  if (intern_syms) {
    Py_REFCNT(self) = 3;
    PyDict_DelItem(intern_syms, ((SibInternedAtom *) self)->name);
  }

  Py_CLEAR(((SibInternedAtom *) self)->name);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *symbol_new(PyTypeObject *type,
			    PyObject *args, PyObject *kwds) {

  // checked

  PyObject *name = NULL;

  if (kwds && PyDict_Size(kwds)) {
    PyErr_SetString(PyExc_TypeError, "symbol takes no named arguments");
    return NULL;
  }

  if (! PyArg_ParseTuple(args, "O:symbol", &name))
    return NULL;

  return SibSymbol_FromString(name);
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

  atom_rewrap(result, SibSymbol_FromString);
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

  atom_rewrap(result, SibSymbol_FromString);
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

  .tp_as_sequence = &atom_as_sequence,
  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


/* === gensym === */


static Py_uhash_t gen_counter = 97531UL;


PyObject *SibGensym_FromString(PyObject *name, PyObject *predicate) {

  // checked

  while (1) {

    // generate a name
    PyObject *maybe_name;
    if (name) {
      maybe_name = PyUnicode_FromFormat("%S#%x", name, gen_counter, NULL);
    } else {
      maybe_name = PyUnicode_FromFormat("<gensym>#%x", gen_counter, NULL);
    }
    if (! maybe_name)
      return NULL;

    // I don't know, some silliness to make the index number bounce
    // all over the damned place
    gen_counter += (Py_uhash_t) maybe_name;
    gen_counter *= _PyHASH_MULTIPLIER;
    if (! gen_counter)
      gen_counter = 97531UL;

    // check whether a symbol with that name is already allocated. If
    // it is, then this is not a unique symbol, try again.
    if (PyDict_GetItem(intern_syms, maybe_name)) {
      Py_CLEAR(maybe_name);
      continue;
    }

    // reserves the symbol for this gensym attempt
    PyObject *maybe_symbol = SibSymbol_FromString(maybe_name);
    Py_CLEAR(maybe_name);

    if (predicate) {
      PyObject *maybe = PyObject_CallFunctionObjArgs(predicate,
						     maybe_symbol, NULL);
      if (! maybe) {
	return NULL;

      } else if (PyObject_IsTrue(maybe)) {
	Py_DECREF(maybe);
	return maybe_symbol;

      } else {
	Py_DECREF(maybe);
	Py_CLEAR(maybe_symbol);
	continue;
      }

    } else {
      return maybe_symbol;
    }
  }
}


static PyObject *m_gensym(PyObject *mod, PyObject *args) {

  // checked

  PyObject *name = NULL;
  PyObject *predicate = NULL;

  if (! PyArg_ParseTuple(args, "|OO:gensym", &name, &predicate))
    return NULL;

  if (predicate == Py_None)
    predicate = NULL;

  return SibGensym_FromString(name, predicate);
}


/* === keyword === */


static PyObject *intern_kwds = NULL;


PyObject *SibKeyword_FromString(PyObject *name) {
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

  return SibKeyword_FromString(name);
}


static void keyword_dealloc(PyObject *self) {

  // checked

  if (intern_kwds) {
    Py_REFCNT(self) = 3;
    PyDict_DelItem(intern_kwds, ((SibInternedAtom *) self)->name);
  }

  Py_CLEAR(((SibInternedAtom *) self)->name);
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

  atom_rewrap(result, SibKeyword_FromString);
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

  atom_rewrap(result, SibKeyword_FromString);
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

  .tp_as_sequence = &atom_as_sequence,
  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


static PyMethodDef methods[] = {

  { "gensym", (PyCFunction) m_gensym, METH_VARARGS,
    "gensym(name, predicate=None -> generate a symbol" },

  { NULL, NULL, 0, NULL },
};


int sib_types_atom_init(PyObject *mod) {

  if (! mod)
    return -1;

  if (! intern_syms) {
    intern_syms = PyDict_New();
    if (! intern_syms)
      return -1;
  }

  if (! intern_kwds) {
    intern_kwds = PyDict_New();
    if (! intern_kwds)
      return -1;
  }

  if (PyType_Ready(&SibKeywordType))
    return -1;

  if (PyType_Ready(&SibSymbolType))
    return -1;

  PyObject *dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "symbol", (PyObject *) &SibSymbolType);
  PyDict_SetItemString(dict, "keyword", (PyObject *) &SibKeywordType);

  return PyModule_AddFunctions(mod, methods);
}


/* The end. */
