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
#define SIBILANT_CTYPES_H 1

#include <Python.h>


typedef struct SibPair {
  PyObject_HEAD

  PyObject *head;
  PyObject *tail;
  PyObject *position;
  PyObject *refs;
} Pair;


typedef struct SibInternedAtom {
  PyObject_HEAD

  PyObject *name;
  PyObject *refs;
} InternedAtom;


static PyTypeObject SibPairType;
static PyTypeObject SibNilType;
static PyTypeObject SibSymbolType;
static PyTypeObject SibKeywordType;


static PyObject *SibSymbol_New(PyObject *name);

static PyObject *SibKeyword_New(PyObject *name);

static PyObject *SibPair_New(PyObject *head, PyObject *tail);

static PyObject *SibNil_Get();


#endif


/* The end. */
