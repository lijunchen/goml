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
    var inline261 int32 = 10
    vec_push__Vec_5int32(v__0, inline261)
    var inline258 int32 = 20
    vec_push__Vec_5int32(v__0, inline258)
    var inline255 int32 = 30
    vec_push__Vec_5int32(v__0, inline255)
    var inline252 int32 = 40
    vec_push__Vec_5int32(v__0, inline252)
    var s__1 []int32
    var inline248 int = 1
    var inline249 int = 4
    var inline250 []int32 = v__0.items[inline248:inline249]
    s__1 = inline250
    var t167 int
    var inline246 int = len(s__1)
    t167 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t167)
    _goml_runtime_core_string_println(inline243)
    var t168 int32
    var inline240 int = 0
    var inline241 int32 = s__1[inline240]
    t168 = inline241
    var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t168)
    _goml_runtime_core_string_println(inline237)
    var t169 int32
    var inline234 int = 1
    var inline235 int32 = s__1[inline234]
    t169 = inline235
    var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t169)
    _goml_runtime_core_string_println(inline231)
    var t170 int32
    var inline228 int = 2
    var inline229 int32 = s__1[inline228]
    t170 = inline229
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t170)
    _goml_runtime_core_string_println(inline225)
    var t__2 []int32
    var inline221 int = 1
    var inline222 int = 3
    var inline223 []int32 = s__1[inline221:inline222]
    t__2 = inline223
    var t171 int
    var inline219 int = len(t__2)
    t171 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t171)
    _goml_runtime_core_string_println(inline216)
    var t172 int32
    var inline213 int = 0
    var inline214 int32 = t__2[inline213]
    t172 = inline214
    var inline210 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t172)
    _goml_runtime_core_string_println(inline210)
    var t173 int32
    var inline207 int = 1
    var inline208 int32 = t__2[inline207]
    t173 = inline208
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t173)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t176 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t176
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t199 string = _goml_runtime_core_int_to_string(self__40)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t202 string = _goml_runtime_core_int32_to_string(self__43)
    return t202
}

func main() {
    main0()
}
