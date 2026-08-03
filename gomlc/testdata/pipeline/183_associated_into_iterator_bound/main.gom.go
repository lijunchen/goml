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
    var inline270 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline270
    var inline267 int32 = 10
    vec_push__Vec_5int32(values__3, inline267)
    var inline264 int32 = 20
    vec_push__Vec_5int32(values__3, inline264)
    var inline261 int32 = 30
    vec_push__Vec_5int32(values__3, inline261)
    var t191 Numbers = Numbers{
        values: values__3,
    }
    var t192 int32 = count__B_Numbers(t191)
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t192)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline287 int32 = 0
    var inline288 *ref_int32_x = ref__Ref_5int32(inline287)
    total__2 = inline288
    var t203 *_goml_vec_int32
    var inline285 *_goml_vec_int32 = batch__1.values
    t203 = inline285
    var for_iter177 FnIterator__int32
    var inline283 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t203)
    for_iter177 = inline283
    Loop_loop206:
    for {
        var for_next178 Option__int32
        var inline278 func() Option__int32 = for_iter177.next_fn
        var inline279 Option__int32 = inline278()
        for_next178 = inline279
        switch for_next178.(type) {
        case None:
            break Loop_loop206
        case Some:
            var t208 int32
            var inline276 int32 = ref_get__Ref_5int32(total__2)
            t208 = inline276
            var t209 int32 = t208 + 1
            ref_set__Ref_5int32(total__2, t209)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline281 int32 = ref_get__Ref_5int32(total__2)
    return inline281
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t212 string = _goml_runtime_core_int32_to_string(self__72)
    return t212
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__207 *_goml_vec_int32) FnIterator__int32 {
    var index__208 *ref_int_x = ref__Ref_3int(0)
    var len__209 int
    var inline295 int = vec_len__Vec_5int32(self__207)
    len__209 = inline295
    var t230 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__208,
        len_1: len__209,
        self_2: self__207,
    }
    var t231 FnIterator__int32 = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t230)
    })
    return t231
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int32(next_fn__130 func() Option__int32) FnIterator__int32 {
    var t237 FnIterator__int32 = FnIterator__int32{
        next_fn: next_fn__130,
    }
    return t237
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env186 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__208 *ref_int_x = env186.index_0
    var len__209 int = env186.len_1
    var self__207 *_goml_vec_int32 = env186.self_2
    var current__210 int = ref_get__Ref_3int(index__208)
    var t254 bool = current__210 < len__209
    if t254 {
        var value__211 int32 = vec_get__Vec_5int32(self__207, current__210)
        var t255 int = current__210 + 1
        ref_set__Ref_3int(index__208, t255)
        var t256 Option__int32 = Some{
            _0: value__211,
        }
        return t256
    } else {
        return None{}
    }
}

func main() {
    main0()
}
