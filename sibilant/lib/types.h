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
} TailCall;


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
extern PyTypeObject SibTailCallType;
extern PyTypeObject FunctionTrampolineType;
extern PyTypeObject MethodTrampolineType;


extern SibPair _SibNil;


PyObject *sib_symbol(PyObject *name);

PyObject *sib_keyword(PyObject *name);

PyObject *sib_gensym(PyObject *name, PyObject *predicate);

PyObject *m_gensym(PyObject *mod, PyObject *args);

PyObject *pair_unpack(PyObject *self, PyObject *_noargs);

PyObject *m_cons(PyObject *mod, PyObject *args, PyObject *kwds);

PyObject *m_car(PyObject *mod, PyObject *pair);

PyObject *m_cdr(PyObject *mod, PyObject *pair);

PyObject *m_setcar(PyObject *mod, PyObject *args);

PyObject *m_setcdr(PyObject *mod, PyObject *args);

PyObject *m_build_unpack_pair(PyObject *mod, PyObject *seqs);

long SibPair_is_proper(PyObject *self);

long SibPair_is_recursive(PyObject *self);

PyObject *sib_pair(PyObject *head, PyObject *tail);

PyObject *sib_cons(PyObject *sequence, int recursive);

PyObject *sib_values(PyObject *args, PyObject *kwds);

PyObject *m_quoted(PyObject *u);

PyObject *sib_tailcall(PyObject *work, PyObject *args, PyObject *kwds);

PyObject *m_tailcall_full(PyObject *self, PyObject *args, PyObject *kwds);

PyObject *m_trampoline(PyObject *self, PyObject *args);


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


#define Sib_Nil ((PyObject *) &_SibNil)

#define Sib_Nilp(val) (((void *) val) == (void *) &_SibNil)


#define CAR(p) (((SibPair *) (p))->head)

#define CDR(p) (((SibPair *) (p))->tail)

#define SETCAR(p, v) {				\
    Py_XDECREF(CAR(p));				\
    CAR(p) = (v);				\
    Py_XINCREF(CAR(p));				\
  }

#define SETCDR(p, v) {				\
    Py_XDECREF(CDR(p));				\
    CDR(p) = (v);				\
    Py_XINCREF(CDR(p));				\
  }


#define SibValues_Check(obj)					\
  ((obj) && PyType_IsSubtype((obj)->ob_type, &SibValuesType))

#define SibValues_CheckExact(obj)		\
  ((obj) && ((obj)->ob_type == &SibValuesType))


#define SibTailCall_Check(obj)				\
  (likely(obj) && ((obj)->ob_type == &SibTailCallType))


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

extern PyObject *__get__;
extern PyObject *_tco_enable;
extern PyObject *_tco_original;


extern PyObject *intern_syms;
extern PyObject *intern_kwds;


#if (defined(__GNUC__) &&						\
     (__GNUC__ > 2 || (__GNUC__ == 2 && (__GNUC_MINOR__ > 95))))
  #define likely(x)   __builtin_expect(!!(x), 1)
  #define unlikely(x) __builtin_expect(!!(x), 0)
#else
  #define likely(x)   (x)
  #define unlikely(x) (x)
#endif


#endif


/* The end. */
