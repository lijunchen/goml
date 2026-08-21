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

type Ordering int32

func main0() struct{} {
    var v__0 *_goml_vec_int32
    var inline474 *_goml_vec_int32 = vec_new__Vec_5int32()
    v__0 = inline474
    var inline471 int32 = 10
    vec_push__Vec_5int32(v__0, inline471)
    var inline468 int32 = 20
    vec_push__Vec_5int32(v__0, inline468)
    var inline465 int32 = 30
    vec_push__Vec_5int32(v__0, inline465)
    var first__1 int32 = vec_get__Vec_5int32(v__0, 0)
    var second__2 int32 = vec_get__Vec_5int32(v__0, 1)
    var third__3 int32 = vec_get__Vec_5int32(v__0, 2)
    var len__4 int
    var inline463 int = vec_len__Vec_5int32(v__0)
    len__4 = inline463
    var t419 string
    var inline461 string = _goml_runtime_core_int32_to_string(first__1)
    t419 = inline461
    var inline458 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline458)
    var t420 string
    var inline456 string = _goml_runtime_core_int32_to_string(second__2)
    t420 = inline456
    var inline453 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline453)
    var t421 string
    var inline451 string = _goml_runtime_core_int32_to_string(third__3)
    t421 = inline451
    var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline448)
    var t422 string
    var inline446 string = _goml_runtime_core_int_to_string(len__4)
    t422 = inline446
    var inline443 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline443)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
