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
  PyObject *weakrefs;
} SibPair;


typedef struct SibInternedAtom {
  PyObject_HEAD

  PyObject *name;
  PyObject *weakrefs;
} SibInternedAtom;


PyTypeObject SibPairType;
PyTypeObject SibNilType;
PyTypeObject SibSymbolType;
PyTypeObject SibKeywordType;


PyObject *sib_symbol(PyObject *name);

PyObject *sib_keyword(PyObject *name);

PyObject *sib_pair(PyObject *head, PyObject *tail);


PyObject _SibNil;

#define Sib_Nil ((PyObject *) &_SibNil)


#endif


/* The end. */
