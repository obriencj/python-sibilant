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


#ifndef SIBILANT_CTCO_H
#define SIBILANT_CTCO_H

#include <Python.h>


PyTypeObject SibTailCallType;
PyTypeObject FunctionTrampolineType;
PyTypeObject MethodTrampolineType;


typedef struct {
  PyObject_HEAD

  PyObject *work;
  PyObject *args;
  PyObject *kwds;
} TailCall;


typedef struct {
  PyObject_HEAD
  PyObject *tco_original;
} Trampoline;


#define SibTailCall_Check(obj)				\
  (likely(obj) && ((obj)->ob_type == &SibTailCallType))


#define SibTrampoline_Check(obj)			\
  (likely(obj) &&					\
   (((obj)->ob_type == &FunctionTrampolineType) ||	\
    ((obj)->ob_type == &MethodTrampolineType)))


PyObject *sib_tailcall(PyObject *work, PyObject *args, PyObject *kwds);


#endif


/* The end. */
