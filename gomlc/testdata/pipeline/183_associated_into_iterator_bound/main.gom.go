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
    var inline276 *_goml_vec_int32 = vec_new__Vec_5int32()
    values__3 = inline276
    var inline273 int32 = 10
    vec_push__Vec_5int32(values__3, inline273)
    var inline270 int32 = 20
    vec_push__Vec_5int32(values__3, inline270)
    var inline267 int32 = 30
    vec_push__Vec_5int32(values__3, inline267)
    var t196 Numbers = Numbers{
        values: values__3,
    }
    var t197 int32 = count__B_Numbers(t196)
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func count__B_Numbers(batch__1 Numbers) int32 {
    var total__2 *ref_int32_x
    var inline293 int32 = 0
    var inline294 *ref_int32_x = ref__Ref_5int32(inline293)
    total__2 = inline294
    var t208 *_goml_vec_int32
    var inline291 *_goml_vec_int32 = batch__1.values
    t208 = inline291
    var for_iter182 FnIterator__int32
    var inline289 FnIterator__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(t208)
    for_iter182 = inline289
    Loop_loop211:
    for {
        var for_next183 Option__int32
        var inline284 func() Option__int32 = for_iter182.next_fn
        var inline285 Option__int32 = inline284()
        for_next183 = inline285
        switch for_next183.(type) {
        case None:
            break Loop_loop211
        case Some:
            var t213 int32
            var inline282 int32 = ref_get__Ref_5int32(total__2)
            t213 = inline282
            var t214 int32 = t213 + 1
            ref_set__Ref_5int32(total__2, t214)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var inline287 int32 = ref_get__Ref_5int32(total__2)
    return inline287
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t217 string = _goml_runtime_core_int32_to_string(self__70)
    return t217
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__int32(self__230 *_goml_vec_int32) FnIterator__int32 {
    var index__231 *ref_int_x = ref__Ref_3int(0)
    var len__232 int
    var inline304 int = vec_len__Vec_5int32(self__230)
    len__232 = inline304
    var t235 closure_env_inherent_Vec_Vec_T_iter_T_int32_0 = closure_env_inherent_Vec_Vec_T_iter_T_int32_0{
        index_0: index__231,
        len_1: len__232,
        self_2: self__230,
    }
    var t236 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(t235)
    }
    var inline302 FnIterator__int32 = FnIterator__int32{
        next_fn: t236,
    }
    return inline302
}

func _goml_m_inherent_i_closure__en_hc2f7b05843f81f2ab8cf844432967a9e_nt32__0_i_apply(env191 closure_env_inherent_Vec_Vec_T_iter_T_int32_0) Option__int32 {
    var index__231 *ref_int_x = env191.index_0
    var len__232 int = env191.len_1
    var self__230 *_goml_vec_int32 = env191.self_2
    var current__233 int = ref_get__Ref_3int(index__231)
    var t260 bool = current__233 < len__232
    if t260 {
        var value__234 int32 = vec_get__Vec_5int32(self__230, current__233)
        var t261 int = current__233 + 1
        ref_set__Ref_3int(index__231, t261)
        var t262 Option__int32 = Some{
            _0: value__234,
        }
        return t262
    } else {
        return None{}
    }
}

func main() {
    main0()
}
