package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Numbers struct {
    values *_goml_vec_int32
}

type FnIterator__int32 struct {
    next_fn func() Option__int32
}

type closure_env_inherent_Vec_Vec_T_iter_T_int32_0 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    var values__3 *_goml_vec_int32
    var inline281 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline281
    var inline278 int32 = 10
    vec_push__Vec_5int32(values__3, inline278)
    var inline275 int32 = 20
    vec_push__Vec_5int32(values__3, inline275)
    var inline272 int32 = 30
    vec_push__Vec_5int32(values__3, inline272)
    var t201 Numbers = Numbers{
        values: values__3,
    }
    var t202 int32 = count__B_Numbers(t201)
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
    _goml_runtime_core_string_println(inline269)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline298 int32 = 0
    var inline299 *ref_int32_x = ref__Ref_5int32(inline298)
    total__2 = inline299
    var t213 *_goml_vec_int32
    var inline296 *_goml_vec_int32 = batch__1.values
    t213 = inline296
    var for_iter187 FnIterator__int32
    var inline294 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t213)
    for_iter187 = inline294
    Loop_loop216:
    for {
        var for_next188 Option__int32
        var inline289 func() Option__int32 = for_iter187.next_fn
        var inline290 Option__int32 = inline289()
        for_next188 = inline290
        switch for_next188.(type) {
        case None:
            break Loop_loop216
        case Some:
            var t218 int32
            var inline287 int32 = ref_get__Ref_5int32(total__2)
            t218 = inline287
            var t219 int32 = t218 + 1
            ref_set__Ref_5int32(total__2, t219)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline292 int32 = ref_get__Ref_5int32(total__2)
    return inline292
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__70)
    return t222
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__230 *_goml_vec_int32) FnIterator__int32 {
    var index__231 *ref_int_x = ref__Ref_3int(0)
    var len__232 int
    var inline309 int = vec_len__Vec_5int32(self__230)
    len__232 = inline309
    var t240 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__231,
        len_1: len__232,
        self_2: self__230,
    }
    var t241 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t240)
    }
    var inline307 FnIterator__int32 = FnIterator__int32{
        next_fn: t241,
    }
    return inline307
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env196 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__231 *ref_int_x = env196.index_0
    var len__232 int = env196.len_1
    var self__230 *_goml_vec_int32 = env196.self_2
    var current__233 int = ref_get__Ref_3int(index__231)
    var t265 bool = current__233 < len__232
    if t265 {
        var value__234 int32 = vec_get__Vec_5int32(self__230, current__233)
        var t266 int = current__233 + 1
        ref_set__Ref_3int(index__231, t266)
        var t267 Option__int32 = Some{
            _0: value__234,
        }
        return t267
    } else {
        return None{}
    }
}

func main() {
    main0()
}
