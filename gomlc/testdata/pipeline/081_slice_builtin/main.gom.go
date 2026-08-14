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
    var inline288 int32 = 10
    vec_push__Vec_5int32(v__0, inline288)
    var inline285 int32 = 20
    vec_push__Vec_5int32(v__0, inline285)
    var inline282 int32 = 30
    vec_push__Vec_5int32(v__0, inline282)
    var inline279 int32 = 40
    vec_push__Vec_5int32(v__0, inline279)
    var s__1 []int32
    var inline275 int = 1
    var inline276 int = 4
    var inline277 []int32 = v__0.items[inline275:inline276]
    s__1 = inline277
    var t194 int
    var inline273 int = len(s__1)
    t194 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t194)
    _goml_runtime_core_string_println(inline270)
    var t195 int32
    var inline267 int = 0
    var inline268 int32 = s__1[inline267]
    t195 = inline268
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t195)
    _goml_runtime_core_string_println(inline264)
    var t196 int32
    var inline261 int = 1
    var inline262 int32 = s__1[inline261]
    t196 = inline262
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t196)
    _goml_runtime_core_string_println(inline258)
    var t197 int32
    var inline255 int = 2
    var inline256 int32 = s__1[inline255]
    t197 = inline256
    var inline252 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
    _goml_runtime_core_string_println(inline252)
    var t__2 []int32
    var inline248 int = 1
    var inline249 int = 3
    var inline250 []int32 = s__1[inline248:inline249]
    t__2 = inline250
    var t198 int
    var inline246 int = len(t__2)
    t198 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline243)
    var t199 int32
    var inline240 int = 0
    var inline241 int32 = t__2[inline240]
    t199 = inline241
    var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
    _goml_runtime_core_string_println(inline237)
    var t200 int32
    var inline234 int = 1
    var inline235 int32 = t__2[inline234]
    t200 = inline235
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t200)
    _goml_runtime_core_string_println(inline231)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t203 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t203
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t226 string = _goml_runtime_core_int_to_string(self__67)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t229 string = _goml_runtime_core_int32_to_string(self__70)
    return t229
}

func main() {
    main0()
}
