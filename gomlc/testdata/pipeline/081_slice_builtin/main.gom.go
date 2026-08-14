package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func main0() struct{} {
    var v__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var inline293 int32 = 10
    vec_push__Vec_5int32(v__0, inline293)
    var inline290 int32 = 20
    vec_push__Vec_5int32(v__0, inline290)
    var inline287 int32 = 30
    vec_push__Vec_5int32(v__0, inline287)
    var inline284 int32 = 40
    vec_push__Vec_5int32(v__0, inline284)
    var s__1 []int32
    var inline280 int = 1
    var inline281 int = 4
    var inline282 []int32 = v__0.items[inline280:inline281]
    s__1 = inline282
    var t199 int
    var inline278 int = len(s__1)
    t199 = inline278
    var inline275 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t199)
    _goml_runtime_core_string_println(inline275)
    var t200 int32
    var inline272 int = 0
    var inline273 int32 = s__1[inline272]
    t200 = inline273
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t200)
    _goml_runtime_core_string_println(inline269)
    var t201 int32
    var inline266 int = 1
    var inline267 int32 = s__1[inline266]
    t201 = inline267
    var inline263 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t201)
    _goml_runtime_core_string_println(inline263)
    var t202 int32
    var inline260 int = 2
    var inline261 int32 = s__1[inline260]
    t202 = inline261
    var inline257 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
    _goml_runtime_core_string_println(inline257)
    var t__2 []int32
    var inline253 int = 1
    var inline254 int = 3
    var inline255 []int32 = s__1[inline253:inline254]
    t__2 = inline255
    var t203 int
    var inline251 int = len(t__2)
    t203 = inline251
    var inline248 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline248)
    var t204 int32
    var inline245 int = 0
    var inline246 int32 = t__2[inline245]
    t204 = inline246
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline242)
    var t205 int32
    var inline239 int = 1
    var inline240 int32 = t__2[inline239]
    t205 = inline240
    var inline236 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t208 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t208
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t231 string = _goml_runtime_core_int_to_string(self__67)
    return t231
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t234 string = _goml_runtime_core_int32_to_string(self__70)
    return t234
}

func main() {
    main0()
}
