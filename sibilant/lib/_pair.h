

#include <Python.h>

#ifndef SIBILANT_PAIR_H
#define SIBILANT_PAIR_H


typedef struct SibPair {
  PyObject_HEAD

  PyObject *head;
  PyObject *tail;
  PyObject *position;
} SibPair;


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


extern PyTypeObject SibPairType;
extern PyTypeObject SibPairIteratorType;
extern PyTypeObject SibPairFollowerType;
extern PyTypeObject SibNilType;


extern SibPair _SibNil;


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


#endif

/* The end. */
