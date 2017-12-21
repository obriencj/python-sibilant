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


static PyTypeObject PairType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.Pair",
  sizeof(Pair),
  0,

  .tp_dealloc = pair_dealloc,
  .tp_flags = Py_TPFLAGS_DEFAULT,

  .tp_descr_get = NULL,
  .tp_getset = NULL,
  .tp_call = NULL,
  .tp_init = NULL,
  .tp_print = pair_print,
  .tp_getattr = NULL,
  .tp_setattr = NULL,
  .tp_as_async = NULL,
  .tp_repr = pair_repr,
  .tp_as_number = NULL,
  .tp_as_sequence = NULL,
  .tp_as_mapping = NULL,
  .tp_hash = NULL,
  .tp_str = NULL,
  .tp_getattro = NULL,
  .tp_setattro = NULL,
  .tp_as_buffer = NULL,
};


static PyTypeObject SymbolType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.Symbol",
  sizeof(InternedAtom),
  0,

  .tp_dealloc = pair_dealloc,
  .tp_flags = Py_TPFLAGS_DEFAULT,

  .tp_descr_get = NULL,
  .tp_getset = NULL,
  .tp_call = NULL,
  .tp_init = NULL,
  .tp_print = pair_print,
  .tp_getattr = NULL,
  .tp_setattr = NULL,
  .tp_as_async = NULL,
  .tp_repr = pair_repr,
  .tp_as_number = NULL,
  .tp_as_sequence = NULL,
  .tp_as_mapping = NULL,
  .tp_hash = NULL,
  .tp_str = NULL,
  .tp_getattro = NULL,
  .tp_setattro = NULL,
  .tp_as_buffer = NULL,
};


static PyTypeObject KeywordType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "sibilant.Keyword",
  sizeof(InternedAtom),
  0,

  .tp_dealloc = pair_dealloc,
  .tp_flags = Py_TPFLAGS_DEFAULT,

  .tp_descr_get = NULL,
  .tp_getset = NULL,
  .tp_call = NULL,
  .tp_init = NULL,
  .tp_print = pair_print,
  .tp_getattr = NULL,
  .tp_setattr = NULL,
  .tp_as_async = NULL,
  .tp_repr = pair_repr,
  .tp_as_number = NULL,
  .tp_as_sequence = NULL,
  .tp_as_mapping = NULL,
  .tp_hash = NULL,
  .tp_str = NULL,
  .tp_getattro = NULL,
  .tp_setattro = NULL,
  .tp_as_buffer = NULL,
};


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

  //

  return PyModule_Create(&ctypes);
}


/* The end. */
