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


#ifndef SIBILANT_TYPES_H
#define SIBILANT_TYPES_H

#include <Python.h>


typedef struct SibInternedAtom {
  PyObject_HEAD

  PyObject *name;
} SibInternedAtom;


typedef struct SibValues {
  PyObject_HEAD

  PyObject *args;
  PyObject *kwds;
  Py_uhash_t hashed;
} SibValues;


extern PyTypeObject SibSymbolType;
extern PyTypeObject SibKeywordType;
extern PyTypeObject SibValuesType;


#define SibSymbol_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibSymbolType))

#define SibSymbol_CheckExact(obj)		\
  ((obj) && ((obj)->ob_type == &SibSymbolType))

#define SibKeyword_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibKeywordType))

#define SibKeyword_CheckExact(obj)			\
  ((obj) && ((obj)->ob_type == &SibKeywordType))

#define SibValues_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibValuesType))

#define SibValues_CheckExact(obj)		\
  ((obj) && ((obj)->ob_type == &SibValuesType))


PyObject *sib_symbol(PyObject *name);

PyObject *sib_keyword(PyObject *name);

PyObject *sib_values(PyObject *args, PyObject *kwds);


#endif


/* The end. */
