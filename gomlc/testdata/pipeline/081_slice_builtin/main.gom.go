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
    var inline283 int32 = 10
    vec_push__Vec_5int32(v__0, inline283)
    var inline280 int32 = 20
    vec_push__Vec_5int32(v__0, inline280)
    var inline277 int32 = 30
    vec_push__Vec_5int32(v__0, inline277)
    var inline274 int32 = 40
    vec_push__Vec_5int32(v__0, inline274)
    var s__1 []int32
    var inline270 int = 1
    var inline271 int = 4
    var inline272 []int32 = v__0.items[inline270:inline271]
    s__1 = inline272
    var t189 int
    var inline268 int = len(s__1)
    t189 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline265)
    var t190 int32
    var inline262 int = 0
    var inline263 int32 = s__1[inline262]
    t190 = inline263
    var inline259 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t190)
    _goml_runtime_core_string_println(inline259)
    var t191 int32
    var inline256 int = 1
    var inline257 int32 = s__1[inline256]
    t191 = inline257
    var inline253 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t191)
    _goml_runtime_core_string_println(inline253)
    var t192 int32
    var inline250 int = 2
    var inline251 int32 = s__1[inline250]
    t192 = inline251
    var inline247 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t192)
    _goml_runtime_core_string_println(inline247)
    var t__2 []int32
    var inline243 int = 1
    var inline244 int = 3
    var inline245 []int32 = s__1[inline243:inline244]
    t__2 = inline245
    var t193 int
    var inline241 int = len(t__2)
    t193 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline238)
    var t194 int32
    var inline235 int = 0
    var inline236 int32 = t__2[inline235]
    t194 = inline236
    var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_println(inline232)
    var t195 int32
    var inline229 int = 1
    var inline230 int32 = t__2[inline229]
    t195 = inline230
    var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t195)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t198 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t198
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t221 string = _goml_runtime_core_int_to_string(self__69)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t224 string = _goml_runtime_core_int32_to_string(self__72)
    return t224
}

func main() {
    main0()
}
