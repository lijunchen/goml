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
    var inline242 int32 = 10
    vec_push__Vec_5int32(v__0, inline242)
    var inline239 int32 = 20
    vec_push__Vec_5int32(v__0, inline239)
    var inline236 int32 = 30
    vec_push__Vec_5int32(v__0, inline236)
    var inline233 int32 = 40
    vec_push__Vec_5int32(v__0, inline233)
    var s__1 []int32
    var inline229 int = 1
    var inline230 int = 4
    var inline231 []int32 = v__0.items[inline229:inline230]
    s__1 = inline231
    var t148 int
    var inline227 int = len(s__1)
    t148 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t148)
    _goml_runtime_core_string_println(inline224)
    var t149 int32
    var inline221 int = 0
    var inline222 int32 = s__1[inline221]
    t149 = inline222
    var inline218 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t149)
    _goml_runtime_core_string_println(inline218)
    var t150 int32
    var inline215 int = 1
    var inline216 int32 = s__1[inline215]
    t150 = inline216
    var inline212 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t150)
    _goml_runtime_core_string_println(inline212)
    var t151 int32
    var inline209 int = 2
    var inline210 int32 = s__1[inline209]
    t151 = inline210
    var inline206 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t151)
    _goml_runtime_core_string_println(inline206)
    var t__2 []int32
    var inline202 int = 1
    var inline203 int = 3
    var inline204 []int32 = s__1[inline202:inline203]
    t__2 = inline204
    var t152 int
    var inline200 int = len(t__2)
    t152 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t152)
    _goml_runtime_core_string_println(inline197)
    var t153 int32
    var inline194 int = 0
    var inline195 int32 = t__2[inline194]
    t153 = inline195
    var inline191 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t153)
    _goml_runtime_core_string_println(inline191)
    var t154 int32
    var inline188 int = 1
    var inline189 int32 = t__2[inline188]
    t154 = inline189
    var inline185 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t154)
    _goml_runtime_core_string_println(inline185)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t157 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t157
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t180 string = _goml_runtime_core_int_to_string(self__69)
    return t180
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t183 string = _goml_runtime_core_int32_to_string(self__72)
    return t183
}

func main() {
    main0()
}
