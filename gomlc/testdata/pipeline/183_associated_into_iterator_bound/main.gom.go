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
    var inline229 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline229
    var inline226 int32 = 10
    vec_push__Vec_5int32(values__3, inline226)
    var inline223 int32 = 20
    vec_push__Vec_5int32(values__3, inline223)
    var inline220 int32 = 30
    vec_push__Vec_5int32(values__3, inline220)
    var t150 Numbers = Numbers{
        values: values__3,
    }
    var t151 int32 = count__B_Numbers(t150)
    var inline217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t151)
    _goml_runtime_core_string_println(inline217)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline246 int32 = 0
    var inline247 *ref_int32_x = ref__Ref_5int32(inline246)
    total__2 = inline247
    var t162 *_goml_vec_int32
    var inline244 *_goml_vec_int32 = batch__1.values
    t162 = inline244
    var for_iter136 FnIterator__int32
    var inline242 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t162)
    for_iter136 = inline242
    Loop_loop165:
    for {
        var for_next137 Option__int32
        var inline237 func() Option__int32 = for_iter136.next_fn
        var inline238 Option__int32 = inline237()
        for_next137 = inline238
        switch for_next137.(type) {
        case None:
            break Loop_loop165
        case Some:
            var t167 int32
            var inline235 int32 = ref_get__Ref_5int32(total__2)
            t167 = inline235
            var t168 int32 = t167 + 1
            ref_set__Ref_5int32(total__2, t168)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline240 int32 = ref_get__Ref_5int32(total__2)
    return inline240
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__72)
    return t171
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__203 *_goml_vec_int32) FnIterator__int32 {
    var index__204 *ref_int_x = ref__Ref_3int(0)
    var len__205 int
    var inline254 int = vec_len__Vec_5int32(self__203)
    len__205 = inline254
    var t189 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__204,
        len_1: len__205,
        self_2: self__203,
    }
    var t190 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t189)
    })
    return t190
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__147 func() Option__int32) FnIterator__int32 {
    var t196 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__147,
    }
    return t196
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env145 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__204 *ref_int_x = env145.index_0
    var len__205 int = env145.len_1
    var self__203 *_goml_vec_int32 = env145.self_2
    var current__206 int = ref_get__Ref_3int(index__204)
    var t213 bool = current__206 < len__205
    if t213 {
        var value__207 int32 = vec_get__Vec_5int32(self__203, current__206)
        var t214 int = current__206 + 1
        ref_set__Ref_3int(index__204, t214)
        var t215 Option__int32 = Some{
            _0: value__207,
        }
        return t215
    } else {
        return None{}
    }
}

func main() {
    main0()
}
