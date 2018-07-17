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
   author: Christopher O'Brien <obriencj@gmail.com>
   license: LGPL v.3
*/


#include "types.h"


PyObject *quoted(PyObject *u);


/* === PairIteratorType === */


typedef struct {
  PyObject_HEAD
  PyObject *pair;
  int index;
} SibPairIterator;


static void piter_dealloc(PyObject *self) {
  Py_CLEAR(((SibPairIterator *) self)->pair);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *piter_iternext(PyObject *self) {
  SibPairIterator *s = (SibPairIterator *) self;
  PyObject *pair = s->pair;
  PyObject *result = NULL;

  if (pair) {
    switch(s->index) {
    case 0:
      result = CAR(pair);
      Py_INCREF(result);
      s->index = 1;
      break;

    case 1:
      result = CDR(pair);
      Py_INCREF(result);
      s->index = 2;
      Py_CLEAR(s->pair);
      break;

    default:
      result = NULL;
    }
  }

  return result;
}


PyTypeObject SibPairIteratorType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "pair_iterator",
  sizeof(SibPairIterator),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_dealloc = piter_dealloc,
  .tp_iter = PyObject_SelfIter,
  .tp_iternext = piter_iternext,
};


/* === PairFollowerType === */


typedef struct {
  PyObject_HEAD
  PyObject *current;
  PyObject *seen;
  long just_items;
} SibPairFollower;


static void pfoll_dealloc(PyObject *self) {

  // checked

  Py_CLEAR(((SibPairFollower *) self)->current);
  Py_CLEAR(((SibPairFollower *) self)->seen);
  Py_TYPE(self)->tp_free(self);
}


static PyObject *pfoll_iternext(PyObject *self) {
  SibPairFollower *s = (SibPairFollower *) self;
  PyObject *current;
  PyObject *curr_id;
  PyObject *result = NULL;

  current = s->current;

  if (! current) {
    /* then it's done */
    return NULL;
  }

  if (s->seen) {
    /* we're set up to keep a record of what pairs we've already
       seen, to prevent recursion */
    curr_id = PyLong_FromVoidPtr(current);

    if (PySet_Contains(s->seen, curr_id)) {
      /* if we've already seen this one, we're done */
      Py_DECREF(curr_id);
      Py_CLEAR(s->current);
      return NULL;

    } else {
      /* otherwise mark it as seen, and continue */
      PySet_Add(s->seen, curr_id);
      Py_DECREF(curr_id);
    }
  }

  /* get ready for the next */
  if (SibPair_CheckExact(current)) {
    s->current = CDR(current);
    Py_INCREF(s->current);

  } else {
    s->current = NULL;
  }

  /* at this point, we've stolen the old s->current ref as current,
     and s->current has been modified to point to something else */

  if (s->just_items) {
    /* if we're in items mode, then we want the CAR if current is a
       pair, otherwise if current is non-nil we return it. */
    if (SibPair_CheckExact(current)) {
      result = CAR(current);
      Py_INCREF(result);
      Py_DECREF(current);
    } else if (Sib_Nilp(current)) {
      result = NULL;
      Py_DECREF(current);
    } else {
      result = current;
    }
  } else {
    result = current;
  }

  return result;
}


PyTypeObject SibPairFollowerType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "pair_follower",
  sizeof(SibPairFollower),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_dealloc = pfoll_dealloc,
  .tp_iter = PyObject_SelfIter,
  .tp_iternext = pfoll_iternext,
};


/* === pair === */


static PyObject *pair_new(PyTypeObject *type,
			  PyObject *args, PyObject *kwds) {

  // checked

  PyObject *head = NULL;
  PyObject *tail = NULL;

  if (kwds && PyDict_Size(kwds)) {
    PyErr_SetString(PyExc_TypeError, "pair takes no named arguments");
    return NULL;
  }

  if (! PyArg_ParseTuple(args, "OO:pair", &head, &tail))
    return NULL;

  return sib_pair(head, tail);
}


static Py_ssize_t pair_len(PyObject *self) {

  // checked

  return 2;
}


static PyObject *pair_getitem(PyObject *self, Py_ssize_t index) {

  // checked

  PyObject *result = NULL;

  switch(index) {
  case 0:
    result = CAR(self);
    Py_XINCREF(result);
    break;

  case 1:
    result = CDR(self);
    Py_XINCREF(result);
    break;

  default:
    PyErr_SetString(PyExc_IndexError, "pair index out of range");
  }

  return result;
}


static int pair_setitem(PyObject *self, Py_ssize_t index, PyObject *val) {

  // checked

  int result = 0;

  switch(index) {
  case 0:
    SETCAR(self, val);
    break;

  case 1:
    SETCDR(self, val);
    break;

  default:
    PyErr_SetString(PyExc_IndexError, "pair index out of range");
    result = -1;
  }

  return result;
}


static PyObject *pair_iter(PyObject *self) {

  // checked

  SibPairIterator *i = PyObject_New(SibPairIterator, &SibPairIteratorType);
  if(! i)
    return NULL;

  Py_INCREF(self);
  i->pair = self;
  i->index = 0;

  return (PyObject *) i;
}


static PyObject *pair_repr(PyObject *self) {
  PyObject *tmp = NULL;
  PyObject *col = PyList_New(0);
  PyObject *found = PyDict_New();
  size_t index = 0;

  PyObject *rest = self;
  PyObject *rest_id;

  PyList_Append(col, _str_cons_paren);

  while (rest->ob_type == &SibPairType) {
    rest_id = PyLong_FromVoidPtr(rest);

    if (PyDict_Contains(found, rest_id)) {
      /* recursive pair detected */
      PyList_Append(col, _str_recursive_true);

      if (rest != self) {
	tmp = PyDict_GetItem(found, rest_id);
	PyList_Insert(col, PyLong_AsSize_t(tmp) - 1, _str_cons_paren);
	PyList_Append(col, _str_close_paren);
      }
      Py_DECREF(rest_id);
      rest = NULL;
      break;

    } else {
      index += 2;

      tmp = PyLong_FromSize_t(index);
      PyDict_SetItem(found, rest_id, tmp);
      Py_DECREF(tmp);

      tmp = PyObject_Repr(CAR(rest));
      PyList_Append(col, tmp);
      PyList_Append(col, _str_comma_space);
      Py_DECREF(tmp);

      rest = CDR(rest);
      Py_DECREF(rest_id);
    }
  }

  if (rest) {
    PyList_Append(col, PyObject_Repr(rest));
  }

  PyList_Append(col, _str_close_paren);

  tmp = PyUnicode_Join(_str_empty, col);
  Py_DECREF(col);
  Py_DECREF(found);

  return tmp;
}


static PyObject *pair_str(PyObject *self) {
  PyObject *tmp = NULL;
  PyObject *col = PyList_New(0);
  PyObject *found = PyDict_New();
  size_t index = 0;

  PyObject *rest = self;
  PyObject *rest_id;

  PyList_Append(col, _str_open_paren);

  for (rest = self; SibPair_CheckExact(rest); rest = CDR(rest)) {
    rest_id = PyLong_FromVoidPtr(rest);

    if (PyDict_Contains(found, rest_id)) {
      /* recursive pair detected */
      PyList_Append(col, _str_elipsis);
      PyList_Append(col, _str_close_paren);

      if (rest != self) {
	tmp = PyDict_GetItem(found, rest_id);
	PyList_Insert(col, PyLong_AsSize_t(tmp) - 1, _str_open_paren);
	PyList_Insert(col, PyLong_AsSize_t(tmp) - 1, _str_dot_space);
	PyList_Append(col, _str_close_paren);
      }
      Py_DECREF(rest_id);

      /* set rest to NULL so we don't try to close it like we would a
	 normal proper or improper list */
      rest = NULL;
      break;

    } else {
      index += 2;

      tmp = PyLong_FromSize_t(index);
      PyDict_SetItem(found, rest_id, tmp);
      Py_DECREF(tmp);
      Py_DECREF(rest_id);

      tmp = CAR(rest);
      if (PyUnicode_CheckExact(tmp)) {
	tmp = quoted(tmp);
      } else {
	tmp = PyObject_Str(tmp);
      }

      PyList_Append(col, tmp);
      Py_DECREF(tmp);

      PyList_Append(col, _str_space);
    }
  }

  if(Sib_Nilp(rest)) {
    /* end of a proper list */

    Py_INCREF(_str_close_paren);
    PyList_SetItem(col, index, _str_close_paren);

  } else if (rest) {
    /* end of an improper list */

    tmp = rest;
    if (PyUnicode_CheckExact(tmp)) {
      tmp = quoted(tmp);
    } else {
      tmp = PyObject_Str(tmp);
    }
    PyList_Append(col, _str_dot_space);
    PyList_Append(col, tmp);
    PyList_Append(col, _str_close_paren);
    Py_DECREF(tmp);
  }

  tmp = PyUnicode_Join(_str_empty, col);
  Py_DECREF(col);
  Py_DECREF(found);

  return tmp;
}


static long pair_eq(PyObject *left, PyObject *right) {
  PyObject *seen = NULL;
  PyObject *left_id, *right_id;
  long answer = 1;
  void *left_seen, *right_seen;

  if (left == right)
    return 1;

  seen = PyDict_New();

  while (left != right) {
    if (Sib_Nilp(left) || Sib_Nilp(right)) {
      // one of our comparisons is a nil, and nil can only equal
      // itself, and left and right aren't the same object, so
      // therefore... nope
      answer = 0;
      break;

    } else if (left->ob_type != &SibPairType) {
      // left is not a pair, use its comparison function instead of
      // ours
      answer = PyObject_RichCompareBool(left, right, Py_EQ);
      break;

    } else if (right->ob_type != &SibPairType) {
      // left is a pair, but right is not, and pairs can only be
      // equivalent to other pairs, so nope.
      answer = 0;
      break;
    }

    // if we reach this point, left and right are both SibPairType,
    // and they do not refer to the same memory space, so we can test
    // their CAR equivs and their CDR equivs

    left_id = PyLong_FromVoidPtr(left);
    right_id = PyLong_FromVoidPtr(right);

    left_seen = PyDict_GetItem(seen, left_id);
    right_seen = PyDict_GetItem(seen, right_id);

    if ((left_seen && PyLong_AsVoidPtr(left_seen) == right) ||
        (right_seen && PyLong_AsVoidPtr(right_seen) == left)) {

      /* we've already compared these two against one another */
      Py_DECREF(left_id);
      Py_DECREF(right_id);
      break;

    } else if (! PyObject_RichCompareBool(CAR(left), CAR(right), Py_EQ)) {
      /* haven't been compared before, but they're not equal */
      answer = 0;
      Py_DECREF(left_id);
      Py_DECREF(right_id);
      break;

    } else {
      /* equal, so make note that we've compared these to each other */
      PyDict_SetItem(seen, left_id, right_id);
      PyDict_SetItem(seen, right_id, left_id);
      Py_DECREF(left_id);
      Py_DECREF(right_id);
    }

    left = CDR(left);
    right = CDR(right);
  }

  Py_DECREF(seen);
  return answer;
}


static PyObject *pair_richcomp(PyObject *self, PyObject *other, int op) {
  if (op == Py_EQ) {
    return PyBool_FromLong(pair_eq(self, other));

  } else if (op == Py_NE) {
    return PyBool_FromLong(! pair_eq(self, other));

  } else {
    PyErr_SetString(PyExc_TypeError, "unsupported pair comparison");
    return NULL;
  }
}


static void pair_dealloc(PyObject *self) {

  // checked

  PyObject_GC_UnTrack(self);
  Py_TRASHCAN_SAFE_BEGIN(self);

  Py_CLEAR(CAR(self));
  Py_CLEAR(CDR(self));
  Py_CLEAR(((SibPair *) self)->position);

  // Py_TYPE(self)->tp_free(self);
  PyObject_GC_Del(self);
  Py_TRASHCAN_SAFE_END(self);
}


static int pair_traverse(PyObject *self, visitproc visit, void *arg) {

  // checked

  Py_VISIT(CAR(self));
  Py_VISIT(CDR(self));
  Py_VISIT(((SibPair *) self)->position);
  return 0;
}


static int pair_clear(PyObject *self) {

  // checked

  Py_CLEAR(CAR(self));
  Py_CLEAR(CDR(self));
  Py_CLEAR(((SibPair *) self)->position);
  return 0;
}


static PyObject *pair_copy(PyObject *self, PyObject *_noargs) {

  // checked

  PyObject *result = NULL;
  PyObject *tmp = NULL;
  PyObject *last = NULL;

  if (Sib_Nilp(self)) {
    Py_INCREF(self);
    return self;
  }

  // records of the IDs of originals to instances of the new copies
  PyObject *seen = PyDict_New();

  for (; SibPair_CheckExact(self); self = CDR(self)) {

    PyObject *self_id = PyLong_FromVoidPtr(self);

    tmp = PyDict_GetItem(seen, self_id);
    if (tmp) {
      // we've seen this one before, so we're recursive. Make the new
      // copy recursive as well and break
      Py_DECREF(self_id);
      SETCDR(last, tmp);
      last = NULL; // this signals that we made a recursive link
      break;
    }

    // make a new pair, associate it with current ID
    tmp = sib_pair(CAR(self), Sib_Nil);
    Py_ASSIGN(((SibPair *) tmp)->position, ((SibPair *) self)->position);

    PyDict_SetItem(seen, self_id, tmp);
    Py_DECREF(self_id);

    // if this is our first pair copied, it's the result
    if (! result) {
      result = tmp;
      Py_INCREF(result);
    }

    // if we have a previous, assign this new pair to the prev's CDR
    // slot. then decref it and make current into the new prev
    if (last) {
      SETCDR(last, tmp);
    }
    last = tmp;
  }

  // done with loop, don't need the seen dict anymore.
  Py_DECREF(seen);

  // either nil or a non-pair leftover is in self, otherwise last
  // would have been cleared
  if (last)
    SETCDR(last, self);

  if (! result)
    PyErr_SetString(PyExc_TypeError, "expected pair");

  return result;
}


static PyObject *pair_length(PyObject *self, PyObject *_noargs) {
  PyObject *seen;
  PyObject *tmp;
  long length = 0;

  if (Sib_Nilp(self)) {
    return PyLong_FromLong(length);
  }

  seen = PySet_New(NULL);

  for(; SibPair_CheckExact(self); self = CDR(self)) {

    tmp = PyLong_FromVoidPtr(self);

    if (PySet_Contains(seen, tmp)) {
      Py_DECREF(tmp);
      self = NULL;
      break;

    } else {
      PySet_Add(seen, tmp);
      Py_DECREF(tmp);
      length++;
    }
  }

  if (self && ! Sib_Nilp(self))
    length++;

  Py_DECREF(seen);
  return PyLong_FromLong(length);
}


static PyObject *pair_follow(PyObject *self, PyObject *_noargs) {

  // checked

  SibPairFollower *i = PyObject_New(SibPairFollower, &SibPairFollowerType);
  if(! i)
    return NULL;

  Py_INCREF(self);
  i->current = self;
  i->seen = PySet_New(NULL);
  i->just_items = 0;

  return (PyObject *) i;
}


PyObject *pair_unpack(PyObject *self, PyObject *_noargs) {

  // checked

  SibPairFollower *i = PyObject_New(SibPairFollower, &SibPairFollowerType);
  if(! i)
    return NULL;

  Py_INCREF(self);
  i->current = self;
  i->seen = PySet_New(NULL);
  i->just_items = 1;

  return (PyObject *) i;
}


/*
static PyObject *pair_to_list(PyObject *self) {
  PyObject *result;
  SibPairFollower *i = NULL;

  i = PyObject_New(SibPairFollower, &SibPairFollowerType);
  Py_INCREF(self);
  i->current = self;
  i->seen = PySet_New(NULL);
  i->just_items = 1;

  result = PySequence_List((PyObject *) i);
  Py_DECREF(i);

  return result;
}
*/


long SibPair_is_proper(PyObject *self) {

  // checked

  if (Sib_Nilp(self))
    return 1;

  PyObject *seen = PySet_New(NULL);
  PyObject *pair_id;
  long result = 0;

  for ( ; SibPair_CheckExact(self); self = CDR(self)) {
    pair_id = PyLong_FromVoidPtr(self);

    if (PySet_Contains(seen, pair_id)) {
      /* seen it, therefore recursive */
      Py_DECREF(pair_id);
      result = 1;
      break;

    } else {
      PySet_Add(seen, pair_id);
      Py_DECREF(pair_id);
    }
  }

  /* it's either recursive and thus proper, or the last item needs to
     have been a nil, or it's improper */
  Py_DECREF(seen);
  return result || Sib_Nilp(self);
}


static PyObject *pair_is_proper(PyObject *self, PyObject *_noargs) {

  // checked

  return PyBool_FromLong(SibPair_is_proper(self));
}


long SibPair_is_recursive(PyObject *self) {

  // checked

  if (Sib_Nilp(self))
    return 0;

  PyObject *seen = PySet_New(NULL);
  PyObject *pair_id;
  long result = 0;

  for ( ; SibPair_CheckExact(self); self = CDR(self)) {
    pair_id = PyLong_FromVoidPtr(self);

    if (PySet_Contains(seen, pair_id)) {
      /* seen it, therefore recursive */
      Py_DECREF(pair_id);
      result = 1;
      break;

    } else {
      PySet_Add(seen, pair_id);
      Py_DECREF(pair_id);
    }
  }

  Py_DECREF(seen);
  return result;
}


static PyObject *pair_is_recursive(PyObject *self, PyObject *_noargs) {

  // checked

  return PyBool_FromLong(SibPair_is_recursive(self));
}


static void pwalk_setpos(PyObject *pair, PyObject *seen, PyObject *pos) {

  // checked

  PyObject *pair_id;
  SibPair *sp;

  for (; SibPair_CheckExact(pair); pair = CDR(pair)) {
    sp = (SibPair *) pair;

    pair_id = PyLong_FromVoidPtr(pair);

    if (PySet_Contains(seen, pair_id)) {
      Py_DECREF(pair_id);
      break;

    } else {
      PySet_Add(seen, pair_id);
      Py_DECREF(pair_id);
    }

    Py_ASSIGN(sp->position, pos);

    if (SibPair_CheckExact(CAR(sp))) {
      pwalk_setpos(CAR(sp), seen, pos);
    }
  }
}


static void pwalk_fillpos(PyObject *pair, PyObject *seen, PyObject *pos) {

  // checked

  PyObject *pair_id;
  SibPair *sp;

  for (; SibPair_CheckExact(pair); pair = CDR(pair)) {
    sp = (SibPair *) pair;

    pair_id = PyLong_FromVoidPtr(pair);

    if (PySet_Contains(seen, pair_id)) {
      Py_DECREF(pair_id);
      break;

    } else {
      PySet_Add(seen, pair_id);
      Py_DECREF(pair_id);
    }

    if (sp->position) {
      pos = sp->position;

    } else {
      Py_INCREF(pos);
      sp->position = pos;
    }

    if (SibPair_CheckExact(CAR(sp))) {
      pwalk_fillpos(CAR(sp), seen, pos);
    }
  }
}


static PyObject *pair_clear_position(PyObject *self,
				     PyObject *args, PyObject *kwds) {

  static char *keywords[] = { "follow", NULL };
  int follow = 0;

  if (Sib_Nilp(self))
    Py_RETURN_NONE;

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|p", keywords, &follow))
    return NULL;

  if (follow) {
    PyObject *seen = PySet_New(NULL);
    pwalk_setpos(self, seen, NULL);
    Py_DECREF(seen);

  } else {
    Py_CLEAR(((SibPair *) self)->position);
  }

  Py_RETURN_NONE;
}


static PyObject *pair_set_position(PyObject *self,
				   PyObject *args, PyObject *kwds) {

  // checked

  static char *keywords[] = { "postition", "follow", NULL };
  PyObject *position = NULL;
  int follow = 0;

  if (Sib_Nilp(self))
    Py_RETURN_NONE;

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "O|p", keywords,
				    &position, &follow))
    return NULL;

  if (follow) {
    PyObject *seen = PySet_New(NULL);
    pwalk_setpos(self, seen, position);
    Py_DECREF(seen);

  } else {
    Py_ASSIGN(((SibPair *) self)->position, position);
  }

  Py_RETURN_NONE;
}


static PyObject *pair_fill_position(PyObject *self,
				    PyObject *args, PyObject *kwds) {

  // checked

  static char *keywords[] = { "position", "follow", NULL };
  PyObject *position = NULL;
  int follow = 0;

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "O|p", keywords,
				    &position, &follow))
    return NULL;

  if (follow) {
    PyObject *seen = PySet_New(NULL);
    pwalk_fillpos(self, seen, position);
    Py_DECREF(seen);

  } else {
    if (! ((SibPair *) self)->position) {
      Py_INCREF(position);
      ((SibPair *) self)->position = position;
    }
  }

  Py_RETURN_NONE;
}


static PyObject *pair_get_position(PyObject *self, PyObject *_noargs) {

  // checked

  PyObject *position = ((SibPair *) self)->position;

  if (position) {
    Py_INCREF(position);
    return position;

  } else {
    Py_RETURN_NONE;
  }
}


static PyMethodDef pair_methods[] = {
  { "__copy__", (PyCFunction) pair_copy, METH_NOARGS,
    "P.__copy__()" },

  { "length", (PyCFunction) pair_length, METH_NOARGS,
    "P.length()" },

  { "follow", (PyCFunction) pair_follow, METH_NOARGS,
    "P.follow()" },

  { "unpack", (PyCFunction) pair_unpack, METH_NOARGS,
    "P.unpack()" },

  { "is_proper", (PyCFunction) pair_is_proper, METH_NOARGS,
    "P.is_proper()" },

  { "is_recursive", (PyCFunction) pair_is_recursive, METH_NOARGS,
    "P.is_recursive()" },

  { "clear_position", (PyCFunction) pair_clear_position,
    METH_VARARGS|METH_KEYWORDS,
    "P.clear_position(follow=True)" },

  { "set_position", (PyCFunction) pair_set_position,
    METH_VARARGS|METH_KEYWORDS,
    "P.set_position(follow=True)" },

  { "fill_position", (PyCFunction) pair_fill_position,
    METH_VARARGS|METH_KEYWORDS,
    "P.fill_position(follow=True)" },

  { "get_position", (PyCFunction) pair_get_position, METH_NOARGS,
    "P.get_position()" },

  { NULL, NULL, 0, NULL },
};


static PySequenceMethods pair_as_sequence = {
  .sq_length = pair_len,
  .sq_item = pair_getitem,
  .sq_ass_item = pair_setitem,
};


PyTypeObject SibPairType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "pair",
  sizeof(SibPair),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_HAVE_GC,
  .tp_methods = pair_methods,
  .tp_new = pair_new,
  .tp_dealloc = pair_dealloc,
  .tp_traverse = pair_traverse,
  .tp_clear = pair_clear,

  .tp_iter = pair_iter,
  .tp_as_sequence = &pair_as_sequence,

  .tp_repr = pair_repr,
  .tp_str = pair_str,
  .tp_richcompare = pair_richcomp,
};


PyObject *sib_pair(PyObject *head, PyObject *tail) {

  // checked

  SibPair *self = NULL;

  if (! (head && tail)) {
    PyErr_SetString(PyExc_TypeError, "pair requires a head and a tail");
    return NULL;
  }

  self = PyObject_GC_New(SibPair, &SibPairType);
  self->position = NULL;

  Py_INCREF(head);
  self->head = head;

  Py_INCREF(tail);
  self->tail = tail;

  PyObject_GC_Track((PyObject *) self);
  return (PyObject *) self;
}


/* === NilType === */


static PyObject *nil_new(PyTypeObject *type,
			 PyObject *args, PyObject *kwds) {

  if (PyTuple_Size(args) || (kwds && PyDict_Size(kwds))) {
    PyErr_SetString(PyExc_TypeError, "NilType takes no arguments");
    return NULL;
  }

  Py_INCREF(Sib_Nil);
  return Sib_Nil;
}


static void nil_dealloc(PyObject *self) {
  ;  // nil is statically allocated, do not free it
}


static PyObject *nil_repr(PyObject *self) {

  // checked

  Py_INCREF(_str_nil);
  return _str_nil;
}


static PyObject *nil_iter(PyObject *self) {

  // checked

  SibPairIterator *i = PyObject_New(SibPairIterator, &SibPairIteratorType);
  if(! i)
    return NULL;

  i->index = 0;
  i->pair = NULL;

  return (PyObject *) i;
}


static Py_ssize_t nil_length(PyObject *selfs) {
  return 0;
}


static PyObject *nil_getitem(PyObject *self, Py_ssize_t index) {
  PyErr_SetString(PyExc_IndexError, "nil has no items");
  return NULL;
}


static int nil_setitem(PyObject *self, Py_ssize_t index, PyObject *val) {
  PyErr_SetString(PyExc_IndexError, "nil has no items");
  return -1;
}


static PySequenceMethods nil_as_sequence = {
  .sq_length = nil_length,
  .sq_item = nil_getitem,
  .sq_ass_item = nil_setitem,
};


static int nil_bool(PyObject *v) {
  return 0;
}


static PyNumberMethods nil_as_number = {
  .nb_bool = (inquiry) nil_bool,
};


PyTypeObject SibNilType = {
  PyVarObject_HEAD_INIT(NULL, 0)

  "NilType",
  sizeof(SibPair),
  0,

  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_base = &SibPairType,
  .tp_new = nil_new,
  .tp_dealloc = nil_dealloc,

  .tp_traverse = NULL,
  .tp_clear = NULL,

  .tp_repr = nil_repr,
  .tp_str = nil_repr,
  .tp_iter = nil_iter,
  .tp_as_number = &nil_as_number,
  .tp_as_sequence = &nil_as_sequence,
};


SibPair _SibNil = {
  {
    _PyObject_EXTRA_INIT
    1, &SibNilType,
  },
  .head = NULL,
  .tail = NULL,
  .position = NULL,
};



PyObject *sib_cons(PyObject *members, int recursive) {

  // checked

  PyObject *seq = PySequence_Fast(members,
				  "cons members object must be a sequence");
  if(! seq) {
    return NULL;
  }

  int count = PySequence_Fast_GET_SIZE(seq);
  if (! count) {
    Py_DECREF(seq);
    Py_INCREF(Sib_Nil);
    return Sib_Nil;
  }

  PyObject *result = sib_pair(PySequence_Fast_GET_ITEM(seq, 0), Sib_Nil);

  if (count == 1) {
    if (recursive) {
      SETCDR(result, result);
    }

  } else {
    PyObject *work = recursive? result: PySequence_Fast_GET_ITEM(seq, --count);
    Py_INCREF(work);

    while (--count) {
      PyObject *tmp = sib_pair(PySequence_Fast_GET_ITEM(seq, count), work);
      Py_DECREF(work);
      work = tmp;
    }

    SETCDR(result, work);
    Py_DECREF(work);
  }

  Py_DECREF(seq);
  return result;
}


PyObject *m_cons(PyObject *mod, PyObject *args, PyObject *kwds) {

  // checked

  int recursive = 0;

  if (kwds) {
    PyObject *tmp = PyDict_GetItem(kwds, _str_recursive);
    // tmp is a borrowed ref

    if (tmp) {
      if (PyDict_Size(kwds) > 1) {
	PyErr_SetString(PyExc_TypeError,
			"cons accepts one keyword argument: recursive");
	return NULL;
      }

      recursive = PyObject_IsTrue(tmp);
      if (recursive < 0)
	return NULL;

    } else {
      if (PyDict_Size(kwds) > 0) {
	PyErr_SetString(PyExc_TypeError,
			"cons accepts one keyword argument: recursive");
	return NULL;
      }
    }
  }

  return sib_cons(args, recursive);
}


PyObject *m_car(PyObject *mod, PyObject *pair) {

  // checked

  PyObject *result = NULL;

  if (! SibPair_Check(pair)) {
    PyErr_SetString(PyExc_TypeError, "car argument must be pair");

  } else if (Sib_Nilp(pair)) {
    PyErr_SetString(PyExc_TypeError, "cannot get car of nil");

  } else {
    result = CAR(pair);
    Py_INCREF(result);
  }

  return result;
}


PyObject *m_cdr(PyObject *mod, PyObject *pair) {

  // checked

  PyObject *result = NULL;

  if (! SibPair_Check(pair)) {
    PyErr_SetString(PyExc_TypeError, "cdr argument must be pair");

  } else if (Sib_Nilp(pair)) {
    PyErr_SetString(PyExc_TypeError, "cannot get cdr of nil");

  } else {
    result = CDR(pair);
    Py_INCREF(result);
  }

  return result;
}


PyObject *m_setcar(PyObject *mod, PyObject *args) {

  // checked

  PyObject *pair = NULL;
  PyObject *val = NULL;

  if (! PyArg_ParseTuple(args, "O!O", &SibPairType, &pair, &val)) {
    return NULL;

  } else if (Sib_Nilp(pair)) {
    PyErr_SetString(PyExc_TypeError, "cannot set car of nil");
    return NULL;

 } else {
    SETCAR(pair, val);
    Py_RETURN_NONE;
  }
}


PyObject *m_setcdr(PyObject *mod, PyObject *args) {

  // checked

  PyObject *pair = NULL;
  PyObject *val = NULL;

  if (! PyArg_ParseTuple(args, "O!O", &SibPairType, &pair, &val)) {
    return NULL;

  } else if (Sib_Nilp(pair)) {
    PyErr_SetString(PyExc_TypeError, "cannot set cdr of nil");
    return NULL;

  } else {
    SETCDR(pair, val);
    Py_RETURN_NONE;
  }
}



PyObject *m_build_unpack_pair(PyObject *mod, PyObject *seqs) {

  // checked

  Py_ssize_t count = seqs? PyTuple_GET_SIZE(seqs): 0;
  Py_ssize_t index = 0;
  PyObject *tmp;

  if (! count) {
    Py_INCREF(Sib_Nil);
    return Sib_Nil;
  }

  PyObject *coll = PyList_New(0);

  while (index < count) {
    PyObject *item = PyTuple_GET_ITEM(seqs, index++);
    PyObject *work;

    if (Sib_Nilp(item)) {
      // skip nil
      continue;

    } else if (SibPair_CheckExact(item)) {
      work = pair_unpack(item, NULL);

    } else {
      // todo: check for lists and tuples explicitly, and see if
      // they're zero-length. If so, avoid creating an iterator, just
      // continue to the next item.
      work = PyObject_GetIter(item);
    }

    if (! work) {
      Py_DECREF(coll);
      return NULL;
    }

    // why the everloving fuck isn't this a regular call?  tmp will be
    // NULL for exception, or a new PyNone reference if successful.
    tmp = _PyList_Extend((PyListObject *) coll, work);
    Py_CLEAR(work);

    if (! tmp) {
      Py_DECREF(coll);
      return NULL;

    } else {
      Py_CLEAR(tmp);
    }
  }

  if (! PyList_GET_SIZE(coll)) {
    // The collection didn't net any actual elements, so let's skip
    // out early and just return nil

    Py_DECREF(coll);
    Py_INCREF(Sib_Nil);
    return Sib_Nil;
  }

  // I wonder if there isn't a better way to do this. We end up
  // traversing the last cons pair twice, which means allocating a set
  // in order to avoid recursion. Is that too expensive?
  tmp = PyTuple_GET_ITEM(seqs, count - 1);
  if (SibPair_is_proper(tmp)) {
    if(PyList_Append(coll, Sib_Nil)) {
      Py_DECREF(coll);
      return NULL;
    }
  }

  PyObject *result = sib_cons(coll, 0);
  Py_DECREF(coll);
  return result;
}


/* The end. */
