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
    var inline278 int32 = 10
    vec_push__Vec_5int32(v__0, inline278)
    var inline275 int32 = 20
    vec_push__Vec_5int32(v__0, inline275)
    var inline272 int32 = 30
    vec_push__Vec_5int32(v__0, inline272)
    var inline269 int32 = 40
    vec_push__Vec_5int32(v__0, inline269)
    var s__1 []int32
    var inline265 int = 1
    var inline266 int = 4
    var inline267 []int32 = v__0.items[inline265:inline266]
    s__1 = inline267
    var t184 int
    var inline263 int = len(s__1)
    t184 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t184)
    _goml_runtime_core_string_println(inline260)
    var t185 int32
    var inline257 int = 0
    var inline258 int32 = s__1[inline257]
    t185 = inline258
    var inline254 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
    _goml_runtime_core_string_println(inline254)
    var t186 int32
    var inline251 int = 1
    var inline252 int32 = s__1[inline251]
    t186 = inline252
    var inline248 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t186)
    _goml_runtime_core_string_println(inline248)
    var t187 int32
    var inline245 int = 2
    var inline246 int32 = s__1[inline245]
    t187 = inline246
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t187)
    _goml_runtime_core_string_println(inline242)
    var t__2 []int32
    var inline238 int = 1
    var inline239 int = 3
    var inline240 []int32 = s__1[inline238:inline239]
    t__2 = inline240
    var t188 int
    var inline236 int = len(t__2)
    t188 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t188)
    _goml_runtime_core_string_println(inline233)
    var t189 int32
    var inline230 int = 0
    var inline231 int32 = t__2[inline230]
    t189 = inline231
    var inline227 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t189)
    _goml_runtime_core_string_println(inline227)
    var t190 int32
    var inline224 int = 1
    var inline225 int32 = t__2[inline224]
    t190 = inline225
    var inline221 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t190)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t193 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t193
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t216 string = _goml_runtime_core_int_to_string(self__69)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t219 string = _goml_runtime_core_int32_to_string(self__72)
    return t219
}

func main() {
    main0()
}
