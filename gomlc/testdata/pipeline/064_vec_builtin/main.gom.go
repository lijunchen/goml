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
    var inline199 *_goml_vec_int32 = vec_new__Vec_5int32()
    v__0 = inline199
    var inline196 int32 = 10
    vec_push__Vec_5int32(v__0, inline196)
    var inline193 int32 = 20
    vec_push__Vec_5int32(v__0, inline193)
    var inline190 int32 = 30
    vec_push__Vec_5int32(v__0, inline190)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int
    var inline188 int = vec_len__Vec_5int32(v__0)
    len__4 = inline188
    var t144 string
    var inline186 string = _goml_runtime_core_int32_to_string(first__1)
    t144 = inline186
    var inline183 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
    _goml_runtime_core_string_println(inline183)
    var t145 string
    var inline181 string = _goml_runtime_core_int32_to_string(second__2)
    t145 = inline181
    var inline178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t145)
    _goml_runtime_core_string_println(inline178)
    var t146 string
    var inline176 string = _goml_runtime_core_int32_to_string(third__3)
    t146 = inline176
    var inline173 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline173)
    var t147 string
    var inline171 string = _goml_runtime_core_int_to_string(len__4)
    t147 = inline171
    var inline168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
