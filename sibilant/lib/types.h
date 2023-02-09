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


typedef struct SibPair {
  PyObject_HEAD

  PyObject *head;
  PyObject *tail;
  PyObject *position;
} SibPair;


typedef struct SibValues {
  PyObject_HEAD

  PyObject *args;
  PyObject *kwds;
  Py_uhash_t hashed;
} SibValues;


typedef struct {
  PyObject_HEAD

  PyObject *work;
  PyObject *args;
  PyObject *kwds;
} Tailcall;


typedef struct {
  PyObject_HEAD
  PyObject *tco_original;
} Trampoline;


extern PyTypeObject SibSymbolType;
extern PyTypeObject SibKeywordType;
extern PyTypeObject SibPairType;
extern PyTypeObject SibPairIteratorType;
extern PyTypeObject SibPairFollowerType;
extern PyTypeObject SibNilType;
extern PyTypeObject SibValuesType;
extern PyTypeObject SibTailcallType;
extern PyTypeObject FunctionTrampolineType;
extern PyTypeObject MethodTrampolineType;


extern SibPair _SibNil;


PyObject *SibSymbol_FromString(PyObject *name);

PyObject *SibKeyword_FromString(PyObject *name);

PyObject *SibGensym_FromString(PyObject *name, PyObject *predicate);

long SibPair_IsProper(PyObject *self);

long SibPair_IsRecursive(PyObject *self);

PyObject *SibPair_New(PyObject *head, PyObject *tail);

PyObject *SibPair_Unpack(PyObject *self);

PyObject *SibPair_Cons(PyObject *sequence, int recursive);

PyObject *SibValues_New(PyObject *args, PyObject *kwds);

PyObject *SibTailcall_New(PyObject *work);

PyObject *SibTrampoline_New(PyObject *self, PyObject *args);

PyObject *sib_quoted(PyObject *u);


#define SibSymbol_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibSymbolType))

#define SibSymbol_CheckExact(obj)		\
  ((obj) && ((obj)->ob_type == &SibSymbolType))

#define SibKeyword_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibKeywordType))

#define SibKeyword_CheckExact(obj)			\
  ((obj) && ((obj)->ob_type == &SibKeywordType))


#define SibPair_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibPairType))

#define SibPair_CheckExact(obj)			\
  ((obj) && ((obj)->ob_type == &SibPairType))


#define SibNil ((PyObject *) &_SibNil)

#define SibNil_Check(val) (((void *) val) == (void *) &_SibNil)


#define SibPair_CAR(p) (((SibPair *) (p))->head)

#define SibPair_CDR(p) (((SibPair *) (p))->tail)

#define SibPair_SETCAR(p, v) {				\
    Py_XDECREF(SibPair_CAR(p));				\
    SibPair_CAR(p) = (v);				\
    Py_XINCREF(SibPair_CAR(p));				\
  }

#define SibPair_SETCDR(p, v) {				\
    Py_XDECREF(SibPair_CDR(p));				\
    SibPair_CDR(p) = (v);				\
    Py_XINCREF(SibPair_CDR(p));				\
  }


#define SibValues_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibValuesType))

#define SibValues_CheckExact(obj)		\
  ((obj) && ((obj)->ob_type == &SibValuesType))


#define SibTailcall_Check(obj)				\
  (likely(obj) && ((obj)->ob_type == &SibTailcallType))


#define SibTrampoline_Check(obj)			\
  (likely(obj) &&					\
   (((obj)->ob_type == &FunctionTrampolineType) ||	\
    ((obj)->ob_type == &MethodTrampolineType)))


#if 1
#define DEBUGMSG(msg, obj) {					\
    printf("** " msg " ");					\
    if (obj) PyObject_Print(((PyObject *) (obj)), stdout, 0);	\
    printf("\n");						\
  }
#else
#define DEBUGMSG(msg, obj) {}
#endif


#define Py_ASSIGN(dest, value) {		\
    Py_XDECREF(dest);				\
    dest = value;				\
    Py_XINCREF(dest);				\
  }


extern PyObject *_str_close_paren;
extern PyObject *_str_colon;
extern PyObject *_str_comma_space;
extern PyObject *_str_cons_paren;
extern PyObject *_str_dot_space;
extern PyObject *_str_elipsis;
extern PyObject *_str_empty;
extern PyObject *_str_equals;
extern PyObject *_str_esc_quote;
extern PyObject *_str_nil;
extern PyObject *_str_open_paren;
extern PyObject *_str_quote;
extern PyObject *_str_recursive;
extern PyObject *_str_recursive_true;
extern PyObject *_str_rsplit;
extern PyObject *_str_space;
extern PyObject *_str_space_dot_space;
extern PyObject *_str_space_elipsis;
extern PyObject *_str_split;
extern PyObject *_str_star;
extern PyObject *_str_starstar;
extern PyObject *_str_strip;
extern PyObject *_str_values_paren;


int sib_types_atom_init(PyObject *module);
int sib_types_pair_init(PyObject *module);
int sib_types_tco_init(PyObject *module);
int sib_types_values_init(PyObject *module);


#if (defined(__GNUC__) &&						\
     (__GNUC__ > 2 || (__GNUC__ == 2 && (__GNUC_MINOR__ > 95))))
  #define likely(x)   __builtin_expect(!!(x), 1)
  #define unlikely(x) __builtin_expect(!!(x), 0)
#else
  #define likely(x)   (x)
  #define unlikely(x) (x)
#endif


#if PY_MAJOR_VERSION >= 3 && PY_MINOR_VERSION < 8
#  define CPy_TRASHCAN_BEGIN(op, dealloc) Py_TRASHCAN_SAFE_BEGIN(op)
#  define CPy_TRASHCAN_END(op) Py_TRASHCAN_SAFE_END(op)
#else
#  define CPy_TRASHCAN_BEGIN(op, dealloc) Py_TRASHCAN_BEGIN(op, dealloc)
#  define CPy_TRASHCAN_END(op) Py_TRASHCAN_END
#endif


#endif


/* The end. */
