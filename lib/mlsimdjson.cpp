/*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
   --------------------------------------------------------------------------*/

#include <simdjson.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/threads.h>

#define PRIM extern "C" CAMLprim

using namespace simdjson;

#define Parser_val(v) (*((dom::parser **) Data_custom_val(v)))
#define Element_val(v) (*((dom::element *) Data_custom_val(v)))
#define Array_val(v) (*((dom::array *) Data_custom_val(v)))
#define Obj_val(v) (*((dom::object *) Data_custom_val(v)))
#define Array_iterator_val(v) (*((dom::array::iterator *) Data_custom_val(v)))
#define Obj_iterator_val(v) (*((dom::object::iterator *) Data_custom_val(v)))
#define Doc_stream_val(v) (*((dom::document_stream **) Data_custom_val(v)))
#define Doc_stream_iter_val(v) (*((dom::document_stream::iterator **) Data_custom_val(v)))

void dom_document_stream_iter_finalize (value x) {
    auto *i = Doc_stream_iter_val(x);
    delete i;
}

static struct custom_operations dom_parser_ops = {
  "simdjson.dom.parser",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_element_ops = {
  "simdjson.dom.element",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_object_ops = {
  "simdjson.dom.object",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_object_iterator_ops = {
  "simdjson.dom.object.iterator",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_array_ops = {
  "simdjson.dom.array",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_array_iterator_ops = {
  "simdjson.dom.array.iterator",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_document_stream_ops = {
  "simdjson.dom.document_stream",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations dom_document_stream_iter_ops = {
  "simdjson.dom.document_stream.iterator",
  dom_document_stream_iter_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

PRIM value createParser_stubs (value unit) {
    CAMLparam1 (unit);
    CAMLlocal1(x);
    auto *p = new dom::parser;
    x = caml_alloc_custom_mem(&dom_parser_ops,
                              sizeof (dom::parser *),
                              sizeof (dom::parser));
    Parser_val(x) = p;
    CAMLreturn(x);
}

extern "C" value freeParser_stubs (value parser) {
    delete Parser_val(parser);
    return Val_unit;
}

extern "C" value freeDs_stubs (value ds) {
    delete Doc_stream_val(ds);
    return Val_unit;
}

PRIM value threadsEnabled (value unit) {
#ifdef SIMDJSON_THREADS_ENABLED
    return Val_true;
#else
    return Val_false;
#endif
}

PRIM value parseString_stubs (value parser, value buf, value len) {
    CAMLparam3 (parser, buf, len);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    auto *p = Parser_val(parser);
    auto error = p->parse((const uint8_t*) String_val(buf),
                          Long_val(len),false).get(Element_val(x));
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    CAMLreturn (x);
}

PRIM value parse_stubs (value parser, value buf, value len) {
    CAMLparam3 (parser, buf, len);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    auto *p = Parser_val(parser);
    auto error = p->parse((const uint8_t*) Caml_ba_data_val(buf),
                          Long_val(len),false).get(Element_val(x));
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    CAMLreturn (x);
}

PRIM value parseMany_stubs (value parser, value buf, value len, value batchSize) {
    CAMLparam4(parser, buf, len, batchSize);
    CAMLlocal1(x);
    auto *p = Parser_val(parser);
    auto *ds = new dom::document_stream;
    x = caml_alloc_custom_mem(&dom_document_stream_ops,
                              sizeof (dom::document_stream),
                              sizeof(dom::document_stream));
    caml_release_runtime_system();
    auto error = p->parse_many((const uint8_t*) Caml_ba_data_val(buf),
                               Long_val(len),
                               Long_val(batchSize)
                               ).get(*ds);
    caml_acquire_runtime_system();
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    Doc_stream_val(x) = ds;
    CAMLreturn (x);
}

PRIM value load_stubs (value parser, value fn) {
    CAMLparam2 (parser, fn);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    auto *p = Parser_val(parser);
    auto error = p->load(String_val(fn)).get(Element_val(x));
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    CAMLreturn (x);
}

PRIM value loadMany_stubs (value parser, value fn, value batchSize) {
    CAMLparam3(parser, fn, batchSize);
    CAMLlocal1(x);
    auto *p = Parser_val(parser);
    auto *ds = new dom::document_stream;
    x = caml_alloc_custom(&dom_document_stream_ops,
                          sizeof (dom::document_stream), 0, 1);
    auto error = p->load_many(String_val(fn), Long_val(batchSize)).get(*ds);
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    Doc_stream_val(x) = ds;
    CAMLreturn (x);
}


extern "C" value arraySize_stubs (value arr) {
    return Val_int(Array_val(arr).size());
}

extern "C" value objSize_stubs (value obj) {
    return Val_int(Obj_val(obj).size());
}

PRIM value arrayIterator_stubs (value arr) {
    CAMLparam1(arr);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_array_iterator_ops,
                          sizeof (dom::array::iterator),
                          0,1);
    Array_iterator_val(x) = Array_val(arr).begin();
    CAMLreturn (x);
}

PRIM value objIterator_stubs (value obj) {
    CAMLparam1(obj);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_object_iterator_ops,
                          sizeof (dom::object::iterator),
                          0,1);
    Obj_iterator_val(x) = Obj_val(obj).begin();
    CAMLreturn (x);
}

PRIM value docStreamIteratorBegin_stubs (value ds) {
    CAMLparam1(ds);
    CAMLlocal1(x);
    auto *i = new dom::document_stream::iterator(Doc_stream_val(ds)->begin());
    x = caml_alloc_custom_mem(&dom_document_stream_iter_ops,
                              sizeof (dom::document_stream::iterator *),
                              sizeof (dom::document_stream::iterator));
    Doc_stream_iter_val(x) = i;
    CAMLreturn (x);
}

PRIM value docStreamIteratorEnd_stubs (value ds) {
    CAMLparam1(ds);
    CAMLlocal1(x);
    auto *i = new dom::document_stream::iterator(Doc_stream_val(ds)->end());
    x = caml_alloc_custom_mem(&dom_document_stream_iter_ops,
                              sizeof (dom::document_stream::iterator *),
                              sizeof (dom::document_stream::iterator));
    Doc_stream_iter_val(x) = i;
    CAMLreturn (x);
}

extern "C" value docStreamIteratorCompare_stubs (value x, value y) {
    auto *a = Doc_stream_iter_val(x), *b = Doc_stream_iter_val(y);
    return Val_bool(*a != *b);
}

PRIM value docStreamIteratorGet_stubs (value iter) {
    CAMLparam1(iter);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    auto *i = Doc_stream_iter_val(iter);
    auto error = (*(*i)).get(Element_val(x));
    if (error)
        caml_invalid_argument(error_message(error));
    CAMLreturn(x);
}

PRIM value arrayIteratorGet_stubs (value iter) {
    CAMLparam1(iter);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    Element_val(x) = *(Array_iterator_val(iter));
    CAMLreturn(x);
}

PRIM value objIteratorGet_stubs (value iter) {
    CAMLparam1(iter);
    CAMLlocal3(block, k, v);
    block = caml_alloc_tuple(2);
    k = caml_copy_string(Obj_iterator_val(iter).key_c_str());
    v = caml_alloc_custom(&dom_element_ops,
                          sizeof (dom::element),
                          0,1);
    Element_val(v) = Obj_iterator_val(iter).value();
    Store_field(block, 0, k);
    Store_field(block, 1, v);
    CAMLreturn(block);
}

extern "C" value arrayIteratorNext_stubs(value iter) {
    ++(Array_iterator_val(iter));
    return Val_unit;
}

extern "C" value objIteratorNext_stubs(value iter) {
    ++(Obj_iterator_val(iter));
    return Val_unit;
}

extern "C" value docStreamIteratorNext_stubs(value iter) {
    auto *i = Doc_stream_iter_val(iter);
    ++(*i);
    return Val_unit;
}

extern "C" value getBool_stubs(value elt) {
    return Val_bool(bool(Element_val(elt)));
}

PRIM value getInt64_stubs(value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    int64_t i64;
    auto error = Element_val(elt).get(i64);
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    x = caml_copy_int64(i64);
    CAMLreturn(x);
}

PRIM value getUint64_stubs(value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    uint64_t u64;
    auto error = Element_val(elt).get(u64);
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    x = caml_copy_int64(u64);
    CAMLreturn(x);
}

PRIM value getDouble_stubs(value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    double dbl;
    auto error = Element_val(elt).get(dbl);
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    x = caml_copy_double(dbl);
    CAMLreturn(x);
}

PRIM value getString_stubs(value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    std::string_view str;
    auto error = Element_val(elt).get(str);
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    x = caml_alloc_initialized_string(str.length(), str.data());
    CAMLreturn(x);
}

PRIM value getArray_stubs (value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_array_ops,
                          sizeof (dom::array),
                          0,1);
    auto error = Element_val(elt).get(Array_val(x));
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    CAMLreturn (x);
}

PRIM value getObject_stubs (value elt) {
    CAMLparam1(elt);
    CAMLlocal1(x);
    x = caml_alloc_custom(&dom_object_ops,
                          sizeof (dom::object),
                          0,1);
    auto error = Element_val(elt).get(Obj_val(x));
    if (error) {
        caml_invalid_argument(error_message(error));
    }
    CAMLreturn (x);
}

extern "C" value elementType_stubs (value elt) {
    return Val_int(Element_val(elt).type());
}

extern "C" value currentIndex_stubs(value iter) {
    dom::document_stream::iterator *i = Doc_stream_iter_val(iter);
    return Val_long(i->current_index());
}

/*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
   --------------------------------------------------------------------------*/
