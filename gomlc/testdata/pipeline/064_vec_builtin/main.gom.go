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
    var inline218 *_goml_vec_int32 = vec_new__Vec_5int32()
    v__0 = inline218
    var inline215 int32 = 10
    vec_push__Vec_5int32(v__0, inline215)
    var inline212 int32 = 20
    vec_push__Vec_5int32(v__0, inline212)
    var inline209 int32 = 30
    vec_push__Vec_5int32(v__0, inline209)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int
    var inline207 int = vec_len__Vec_5int32(v__0)
    len__4 = inline207
    var t163 string
    var inline205 string = _goml_runtime_core_int32_to_string(first__1)
    t163 = inline205
    var inline202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline202)
    var t164 string
    var inline200 string = _goml_runtime_core_int32_to_string(second__2)
    t164 = inline200
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline197)
    var t165 string
    var inline195 string = _goml_runtime_core_int32_to_string(third__3)
    t165 = inline195
    var inline192 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline192)
    var t166 string
    var inline190 string = _goml_runtime_core_int_to_string(len__4)
    t166 = inline190
    var inline187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
