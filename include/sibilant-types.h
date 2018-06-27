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


#ifndef SIBILANT_CTYPES_H
#define SIBILANT_CTYPES_H

#include <Python.h>


typedef struct SibPair {
  PyObject_HEAD

  PyObject *head;
  PyObject *tail;
  PyObject *position;
} SibPair;


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


PyTypeObject SibPairType;
PyTypeObject SibNilType;
PyTypeObject SibSymbolType;
PyTypeObject SibKeywordType;
PyTypeObject SibValuesType;


#define SibPair_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibPairType))

#define SibPair_CheckExact(obj)			\
  ((obj) && ((obj)->ob_type == &SibPairType))

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

#define SibValues_CheckExact(obj)			\
  ((obj) && ((obj)->ob_type == &SibValuesType))


long SibPair_is_proper(PyObject *self);

long SibPair_is_recursive(PyObject *self);


PyObject *sib_symbol(PyObject *name);

PyObject *sib_keyword(PyObject *name);

PyObject *sib_pair(PyObject *head, PyObject *tail);

PyObject *sib_values(PyObject *args, PyObject *kwds);


#define CAR(p) (((SibPair *) (p))->head)

#define CDR(p) (((SibPair *) (p))->tail)

#define SETCAR(p, v) {				\
    Py_XDECREF(CAR(p));				\
    Py_XINCREF(v);				\
    CAR(p) = (v);				\
  }

#define SETCDR(p, v) {				\
    Py_XDECREF(CDR(p));				\
    Py_XINCREF(v);				\
    CDR(p) = (v);				\
  }


SibPair _SibNil;

#define Sib_Nil ((PyObject *) &_SibNil)

#define Sib_Nilp(val) (((void *) val) == (void *) &_SibNil)


#endif


/* The end. */
