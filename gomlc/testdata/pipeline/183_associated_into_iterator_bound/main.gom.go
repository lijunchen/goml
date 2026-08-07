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
    var inline265 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline265
    var inline262 int32 = 10
    vec_push__Vec_5int32(values__3, inline262)
    var inline259 int32 = 20
    vec_push__Vec_5int32(values__3, inline259)
    var inline256 int32 = 30
    vec_push__Vec_5int32(values__3, inline256)
    var t186 Numbers = Numbers{
        values: values__3,
    }
    var t187 int32 = count__B_Numbers(t186)
    var inline253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t187)
    _goml_runtime_core_string_println(inline253)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline282 int32 = 0
    var inline283 *ref_int32_x = ref__Ref_5int32(inline282)
    total__2 = inline283
    var t198 *_goml_vec_int32
    var inline280 *_goml_vec_int32 = batch__1.values
    t198 = inline280
    var for_iter172 FnIterator__int32
    var inline278 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t198)
    for_iter172 = inline278
    Loop_loop201:
    for {
        var for_next173 Option__int32
        var inline273 func() Option__int32 = for_iter172.next_fn
        var inline274 Option__int32 = inline273()
        for_next173 = inline274
        switch for_next173.(type) {
        case None:
            break Loop_loop201
        case Some:
            var t203 int32
            var inline271 int32 = ref_get__Ref_5int32(total__2)
            t203 = inline271
            var t204 int32 = t203 + 1
            ref_set__Ref_5int32(total__2, t204)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline276 int32 = ref_get__Ref_5int32(total__2)
    return inline276
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__72)
    return t207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__228 *_goml_vec_int32) FnIterator__int32 {
    var index__229 *ref_int_x = ref__Ref_3int(0)
    var len__230 int
    var inline290 int = vec_len__Vec_5int32(self__228)
    len__230 = inline290
    var t225 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__229,
        len_1: len__230,
        self_2: self__228,
    }
    var t226 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t225)
    })
    return t226
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__172 func() Option__int32) FnIterator__int32 {
    var t232 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__172,
    }
    return t232
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env181 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__229 *ref_int_x = env181.index_0
    var len__230 int = env181.len_1
    var self__228 *_goml_vec_int32 = env181.self_2
    var current__231 int = ref_get__Ref_3int(index__229)
    var t249 bool = current__231 < len__230
    if t249 {
        var value__232 int32 = vec_get__Vec_5int32(self__228, current__231)
        var t250 int = current__231 + 1
        ref_set__Ref_3int(index__229, t250)
        var t251 Option__int32 = Some{
            _0: value__232,
        }
        return t251
    } else {
        return None{}
    }
}

func main() {
    main0()
}
