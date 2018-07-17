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
   sibilant._types

   Native sibilant core types and helper functions. These function and
   types will be re-exported from the sibilant module.

   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
 */


#include "_types.h"
#include "_pair.h"


#define DOCSTR "Native Sibilant core types and functions"


#if 1
#define DEBUGMSG(msg, obj) {					\
    printf("** " msg " ");					\
    if (obj) PyObject_Print(((PyObject *) (obj)), stdout, 0);	\
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


#define Py_ASSIGN(dest, value) {		\
    Py_XDECREF(dest);				\
    dest = value;				\
    Py_XINCREF(dest);				\
  }


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


PyObject *quoted(PyObject *u) {

  // checked

  PyObject *tmp = PyUnicode_Replace(u, _str_quote, _str_esc_quote, -1);
  if (! tmp)
    return NULL;

  PyObject *result = PyUnicode_FromFormat("\"%U\"", tmp);
  Py_DECREF(tmp);
  return result;
}


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


static void atom_rewrap(PyObject *vals, unaryfunc conv) {

  // checked

  for (int index = PyList_Size(vals); index--; ) {
    PyList_SetItem(vals, index, conv(PyList_GET_ITEM(vals, index)));
  }
}


/* === symbol === */


static PyObject *intern_syms = NULL;


PyObject *sib_symbol(PyObject *name) {

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

  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


/* === gensym === */


static Py_uhash_t gen_counter = 97531UL;


PyObject *sib_gensym(PyObject *name, PyObject *predicate) {

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
    PyObject *maybe_symbol = sib_symbol(maybe_name);
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

  return sib_gensym(name, predicate);
}


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

  .tp_repr = atom_repr,
  .tp_str = atom_str,
};


/* === ValuesType === */


static PyObject *values_new(PyTypeObject *type,
			    PyObject *args, PyObject *kwds) {

  // checked

  return sib_values(args, kwds);
}


static void values_dealloc(PyObject *self) {

  // checked

  PyObject_GC_UnTrack(self);
  Py_TRASHCAN_SAFE_BEGIN(self);

  Py_CLEAR(((SibValues *) self)->args);
  Py_CLEAR(((SibValues *) self)->kwds);

  // Py_TYPE(self)->tp_free(self);

  PyObject_GC_Del(self);
  Py_TRASHCAN_SAFE_END(self);
}


static int values_traverse(PyObject *self, visitproc visit, void *arg) {

  // checked

  Py_VISIT(((SibValues *) self)->args);
  Py_VISIT(((SibValues *) self)->kwds);
  return 0;
}


static int values_clear(PyObject *self) {

  // checked

  Py_CLEAR(((SibValues *) self)->args);
  Py_CLEAR(((SibValues *) self)->kwds);
  return 0;
}


static PyObject *values_iter(PyObject *self) {

  // checked

  return PyObject_GetIter(((SibValues *) self)->args);
}


static Py_ssize_t values_args_length(PyObject *self) {

  // checked

  return PyTuple_GET_SIZE(((SibValues *) self)->args);
}


static PyObject *values_args_getitem(PyObject *self, Py_ssize_t index) {

  // checked

  return PySequence_GetItem(((SibValues *) self)->args, index);
}


static Py_ssize_t values_kwds_length(PyObject *self) {

  // checked

  PyObject *kwds = ((SibValues *) self)->kwds;
  return kwds? PyDict_Size(kwds): 0;
}


static PyObject *values_kwds_getitem(PyObject *self, PyObject *key) {

  // checked

  if (PyLong_CheckExact(key)) {
    return PySequence_GetItem(((SibValues *) self)->args,
			      PyLong_AsSsize_t(key));

  } else {
    PyObject *kwds = ((SibValues *) self)->kwds;
    PyObject *result = kwds? PyDict_GetItem(kwds, key): NULL;

    if (result) {
      Py_INCREF(result);
      return result;

    } else {
      // we do our own error handling because result could be NULL
      // either because there was a NULL keyword dict, or because of
      // an actual NULL result from GetItem. in either of those cases,
      // we want to emit the same KeyError
      PyErr_SetObject(PyExc_KeyError, quoted(key));
      return NULL;
    }
  }
}


static PyObject *values_call(PyObject *self,
			     PyObject *args, PyObject *kwds) {

  // checked

  SibValues *s = (SibValues *) self;
  PyObject *call_args, *call_kwds;
  PyObject *work = NULL, *tmp = NULL;

  if (unlikely(! PyTuple_GET_SIZE(args))) {
    PyErr_SetString(PyExc_TypeError, "values objects must be called with at"
		    " least one argument, the function to apply");
    return NULL;
  }

  work = PyTuple_GET_ITEM(args, 0);

  if (PyTuple_GET_SIZE(args) > 1) {
    // if we have more positionals beyond just the callable work item,
    // we'll need to add those the invocation of work

    tmp = PySequence_GetSlice(args, 1, PyTuple_GET_SIZE(args));

    if (PyTuple_GET_SIZE(s->args)) {
      // merge the existing positionals with the invocation ones
      call_args = PySequence_Concat(s->args, tmp);
      Py_DECREF(tmp);

    } else {
      // there were no positionals in the values, so just use the
      // invocation ones
      call_args = tmp;
    }

  } else {
    // no additional positionals given at invocation, so we'll just be
    // using our existing ones.
    call_args = s->args;
    Py_INCREF(call_args);
  }

  if (kwds && PyDict_Size(kwds)) {
    // if keyword arguments were supplied, we'll need to add those to
    // the work invocation.

    if (s->kwds && PyDict_Size(s->kwds)) {
      // if the values already had keywords, we'll need to create a
      // new dict and merge these two sets of keyword arguments
      // together

      call_kwds = PyDict_Copy(s->kwds);
      PyDict_Update(call_kwds, kwds);

    } else {
      // the values had no keywords, so let's just use the ones supplied
      // by the invocation

      call_kwds = kwds;
      Py_INCREF(call_kwds);
    }

  } else {
    // no extra keyword arguments were supplied, so we only need to
    // use the ones from the values
    call_kwds = s->kwds;
    Py_XINCREF(call_kwds);
  }

  tmp = PyObject_Call(work, call_args, call_kwds);
  Py_DECREF(call_args);
  Py_XDECREF(call_kwds);

  return tmp;
}


static PyObject *values_repr(PyObject *self) {

  // checked

  SibValues *s = (SibValues *) self;
  PyObject *tmp = NULL;
  Py_ssize_t count = 0, limit = 0;
  PyObject *key = NULL, *value = NULL;

  // "values()"
  // "values(1, 2, 3)"
  // "values(foo=4, bar=5)"
  // "values(1, 2, 3, foo=4, bar=5)"

  PyObject *col = PyList_New(0);
  PyList_Append(col, _str_values_paren);

  limit = PyTuple_GET_SIZE(s->args);
  for (count = 0; count < limit; count++) {
    tmp = PyObject_Repr(PyTuple_GET_ITEM(s->args, count));
    PyList_Append(col, tmp);
    PyList_Append(col, _str_comma_space);
    Py_DECREF(tmp);
  }

  count = 0;
  if (s->kwds && PyDict_Size(s->kwds)) {
    while (PyDict_Next(s->kwds, &count, &key, &value)) {
      tmp = PyObject_Repr(value);
      PyList_Append(col, key);
      PyList_Append(col, _str_equals);
      PyList_Append(col, tmp);
      PyList_Append(col, _str_comma_space);
      Py_DECREF(tmp);
    }
  }

  if (limit || count) {
    // we'll have a trailing _str_comma_space if we added
    // anything. Re-use its index for the close paren
    Py_INCREF(_str_close_paren);
    PyList_SetItem(col, PyList_GET_SIZE(col) - 1, _str_close_paren);
  } else {
    // otherwise, just close off as "values()"
    PyList_Append(col, _str_close_paren);
  }

  tmp = PyUnicode_Join(_str_empty, col);
  Py_DECREF(col);

  return tmp;
}


static Py_hash_t values_hash(PyObject *self) {

  // checked

  SibValues *s = (SibValues *) self;
  Py_uhash_t result = s->hashed, khash;
  PyObject *tmp, *frozen;

  if (result == 0) {
    result = PyObject_Hash(s->args);
    if (result == (Py_uhash_t) -1)
      return -1;

    if (s->kwds && PyDict_Size(s->kwds)) {
      tmp = _PyDictView_New(s->kwds, &PyDictItems_Type);
      frozen = PyFrozenSet_New(tmp);
      Py_DECREF(tmp);

      if (! frozen)
	return -1;

      khash = PyObject_Hash(frozen);
      Py_DECREF(frozen);

      if (khash == (Py_uhash_t) -1)
	return -1;

      // I stole these magic numbers from tuplehash
      result = (result ^ khash) * _PyHASH_MULTIPLIER;
      result += 97531UL;

      if (result == (Py_uhash_t) -1)
	result = -2;
    }

    s->hashed = result;
  }

  return result;
}


static long values_eq(PyObject *self, PyObject *other) {
  SibValues *s = (SibValues *) self;
  long answer = 0;

  if (self == other) {
    // identity is equality, yes
    answer = 1;

  } else if (SibValues_CheckExact(other)) {
    SibValues *o = (SibValues *) other;

    // when comparing two values against each other, we'll just
    // compare their positionals and keywords. We'll actually do the
    // keywords check first, because it has a quick NULL-check

    if (s->kwds && o->kwds) {
      answer = PyObject_RichCompareBool(s->kwds, o->kwds, Py_EQ);
    } else {
      answer = (s->kwds == o->kwds);
    }

    answer = answer && \
      PyObject_RichCompareBool(s->args, o->args, Py_EQ);

  } else if (PyTuple_CheckExact(other)) {
    // comparing against a tuple is fine, so long as keywords either
    // are NULL or empty.

    answer = ((! s->kwds) || (! PyDict_Size(s->kwds))) &&	\
      PyObject_RichCompareBool(s->args, other, Py_EQ);

  } else if (PyDict_CheckExact(other)) {
    // comparing against a dict is fine, so long as positionals is
    // empty.

    if (s->kwds) {
      answer = (! PyTuple_GET_SIZE(s->args)) &&			\
	PyObject_RichCompareBool(s->kwds, other, Py_EQ);

    } else {
      // we'll say a NULL keywords is equal to an empty dict
      answer = (! PyTuple_GET_SIZE(s->args)) && \
	(! PyDict_Size(other));
    }
  }

  return answer;
}


static PyObject *values_richcomp(PyObject *self, PyObject *other, int op) {

  // checked

  if (op == Py_EQ) {
    return PyBool_FromLong(values_eq(self, other));

  } else if (op == Py_NE) {
    return PyBool_FromLong(! values_eq(self, other));

  } else {
    PyErr_SetString(PyExc_TypeError, "unsupported values comparison");
    return NULL;
  }
}


static int values_bool(PyObject *self) {

  // checked

  PyObject *args = ((SibValues *) self)->args;
  PyObject *kwds = ((SibValues *) self)->kwds;

  return !!((kwds && PyDict_Size(kwds)) || (args && PyTuple_GET_SIZE(args)));
}


static PyObject *values_add(PyObject *left, PyObject *right) {
  SibValues *result = NULL;
  PyObject *args = NULL, *kwds = NULL, *tmp;

  if (SibValues_CheckExact(left)) {
    SibValues *s = (SibValues *) left;

    if (SibValues_CheckExact(right)) {
      SibValues *o = (SibValues *) right;

      args = PySequence_Concat(s->args, o->args);
      if (! args)
	return NULL;

      if (o->kwds) {
	kwds = s->kwds? PyDict_Copy(s->kwds): PyDict_New();
	PyDict_Update(kwds, o->kwds);
      } else {
	kwds = s->kwds;
	Py_XINCREF(kwds);
      }

    } else if (PyDict_Check(right)) {
      args = s->args;
      Py_INCREF(args);

      kwds = s->kwds? PyDict_Copy(s->kwds): PyDict_New();
      PyDict_Update(kwds, right);

    } else {
      tmp = PySequence_Tuple(right);
      if (! tmp)
	return NULL;

      args = PySequence_Concat(s->args, tmp);
      Py_DECREF(tmp);

      if (! args)
	return NULL;

      kwds = s->kwds;
      Py_XINCREF(kwds);
    }

  } else if (SibValues_CheckExact(right)) {
    SibValues *s = (SibValues *) right;

    if(PyDict_Check(left)) {
      args = s->args;
      Py_INCREF(args);

      if (s->kwds) {
	kwds = PyDict_Copy(left);
	PyDict_Update(kwds, s->kwds);

      } else {
	kwds = PyDict_Copy(left);
      }

    } else {
      tmp = PySequence_Tuple(left);
      if (! tmp)
	return NULL;

      args = PySequence_Concat(tmp, s->args);
      Py_DECREF(tmp);

      if (! args)
	return NULL;

      kwds = s->kwds;
      Py_XINCREF(kwds);
    }

  } else {
    PyErr_SetString(PyExc_TypeError, "values_add invoked with no values");
    return NULL;
  }

  result = (SibValues *) sib_values(args, NULL);
  if (result)
    result->kwds = kwds;  // just to avoid another copy
  Py_DECREF(args);

  return (PyObject *) result;
}


static PyObject *values_keys(PyObject *self, PyObject *_noargs) {
  SibValues *s = (SibValues *) self;
  PyObject *tmp, *result = NULL;

  if (s->kwds) {
    // this is what the default keys() impl on dict does. The
    // PyDict_Keys API creates a list, which we don't want to do.
    result = _PyDictView_New(s->kwds, &PyDictKeys_Type);

  } else {
    // a cheap empty iterator
    tmp = PyTuple_New(0);
    result = PyObject_GetIter(tmp);
    Py_DECREF(tmp);
  }

  return result;
}


static PyMethodDef values_methods[] = {
  { "keys", (PyCFunction) values_keys, METH_NOARGS,
    "V.keys()" },

  { NULL, NULL, 0, NULL },
};


static PyNumberMethods values_as_number = {
  .nb_bool = (inquiry) values_bool,
  .nb_add = values_add,
};


static PySequenceMethods values_as_sequence = {
  .sq_length = values_args_length,
  .sq_item = values_args_getitem,
};


static PyMappingMethods values_as_mapping = {
  .mp_length = values_kwds_length,
  .mp_subscript = values_kwds_getitem,
};


PyTypeObject SibValuesType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "values",
  sizeof(SibValues),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_HAVE_GC,
  .tp_methods = values_methods,
  .tp_new = values_new,
  .tp_dealloc = values_dealloc,
  .tp_traverse = values_traverse,
  .tp_clear = values_clear,

  .tp_iter = values_iter,
  .tp_hash = values_hash,
  .tp_as_number = &values_as_number,
  .tp_as_sequence = &values_as_sequence,
  .tp_as_mapping = &values_as_mapping,

  .tp_call = values_call,
  .tp_repr = values_repr,
  .tp_richcompare = values_richcomp,
};


PyObject *sib_values(PyObject *args, PyObject *kwds) {

  // checked

  SibValues *self = NULL;

  if (! args) {
    PyErr_SetString(PyExc_TypeError, "values require arguments");
    return NULL;
  }

  self = PyObject_GC_New(SibValues, &SibValuesType);
  if (unlikely(! self))
    return NULL;

  self->args = PySequence_Tuple(args);
  self->kwds = kwds? PyDict_Copy(kwds): NULL;
  self->hashed = 0;

  PyObject_GC_Track((PyObject *) self);
  return (PyObject *) self;
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


static PyObject *m_build_dict(PyObject *mod, PyObject *values) {

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

    if (SibPair_CheckExact(item) && SibPair_Check(CDR(item))) {
      /* pair of more than one link, convert to iterator */
      item = pair_unpack(item, NULL);
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
    return result;
  }
}


static PyMethodDef methods[] = {
  { "gensym", (PyCFunction) m_gensym, METH_VARARGS,
    "gensym(name, predicate=None -> generate a symbol" },

  { "cons", (PyCFunction) m_cons, METH_VARARGS|METH_KEYWORDS,
    "cons(head, *tail, recursive=Fasle) -> new pair\n"
    "If no tail is specified, and recursive is False (the default),\n"
    "then a nil will be presumed." },

  { "car", m_car, METH_O,
    "car(P) -> object\n"
    "Returns the head element of a sibilant pair instance P."},

  { "cdr", m_cdr, METH_O,
    "cdr(P) -> object\n"
    "Returns the tail element of a sibilant pair instance P." },

  { "setcar", m_setcar, METH_VARARGS,
    "setcar(P, obj) -> None\n"
    "Assigns the head element of a sibilant pair instance P to obj." },

  { "setcdr", m_setcdr, METH_VARARGS,
    "setcdr(P, obj) -> None\n"
    "Assigns the tail element of a sibilant pair instance P to obj." },

  { "reapply", (PyCFunction) m_reapply, METH_VARARGS|METH_KEYWORDS,
    "reapply(func, data, count) -> result data\n"
    "Calls `data = func(data)` count times (or until an exception is\n"
    "raised), and returns the final data value." },

  { "build_unpack_pair", (PyCFunction) m_build_unpack_pair, METH_VARARGS,
    "build_unpack_pair(*pair_or_seq) -> new pair\n"
    "Creates a new sibilant pair list from a collection of pair or\n"
    "non-pair sequences." },

  { "build_tuple", (PyCFunction) m_build_tuple, METH_VARARGS,
    "build_tuple(*args) -> args" },

  { "build_list", (PyCFunction) m_build_list, METH_VARARGS,
    "build_list(*args) -> list(args)" },

  { "build_set", (PyCFunction) m_build_set, METH_VARARGS,
    "build_set(*args) -> set(args)" },

  { "build_dict",  (PyCFunction) m_build_dict, METH_VARARGS,
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
  .m_name = "sibilant._types",
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

  if (PyType_Ready(&SibPairType) < 0)
    return NULL;

  if (PyType_Ready(&SibPairIteratorType) < 0)
    return NULL;

  if (PyType_Ready(&SibPairFollowerType) < 0)
    return NULL;

  if (PyType_Ready(&SibNilType) < 0)
    return NULL;

  if (PyType_Ready(&SibKeywordType) < 0)
    return NULL;

  if (PyType_Ready(&SibSymbolType) < 0)
    return NULL;

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

  PyObject *mod = PyModule_Create(&ctypes);
  if (! mod)
    return NULL;

  PyObject *dict = PyModule_GetDict(mod);
  PyDict_SetItemString(dict, "nil", Sib_Nil);
  PyDict_SetItemString(dict, "pair", (PyObject *) &SibPairType);
  PyDict_SetItemString(dict, "symbol", (PyObject *) &SibSymbolType);
  PyDict_SetItemString(dict, "keyword", (PyObject *) &SibKeywordType);
  PyDict_SetItemString(dict, "values", (PyObject *) &SibValuesType);

  return mod;
}


/* The end. */
