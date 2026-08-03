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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func main0() struct{} {
    var v__0 *_goml_vec_int32
    var inline240 *_goml_vec_int32 = vec_new__Vec_5int32()
    v__0 = inline240
    var inline237 int32 = 10
    vec_push__Vec_5int32(v__0, inline237)
    var inline234 int32 = 20
    vec_push__Vec_5int32(v__0, inline234)
    var inline231 int32 = 30
    vec_push__Vec_5int32(v__0, inline231)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int
    var inline229 int = vec_len__Vec_5int32(v__0)
    len__4 = inline229
    var t185 string
    var inline227 string = _goml_runtime_core_int32_to_string(first__1)
    t185 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline224)
    var t186 string
    var inline222 string = _goml_runtime_core_int32_to_string(second__2)
    t186 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline219)
    var t187 string
    var inline217 string = _goml_runtime_core_int32_to_string(third__3)
    t187 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline214)
    var t188 string
    var inline212 string = _goml_runtime_core_int_to_string(len__4)
    t188 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
