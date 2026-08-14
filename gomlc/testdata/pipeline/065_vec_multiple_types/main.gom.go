package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

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

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_bool struct {
    items []bool
}

func vec_new__Vec_4bool() *_goml_vec_bool {
    return &_goml_vec_bool{
        items: nil,
    }
}

func vec_push__Vec_4bool(vec *_goml_vec_bool, elem bool) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_4bool(vec *_goml_vec_bool, index int) bool {
    return vec.items[index]
}

func vec_len__Vec_4bool(vec *_goml_vec_bool) int {
    return int(len(vec.items))
}

type Ordering int32

func main0() struct{} {
    var vi__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var inline514 int32 = 42
    vec_push__Vec_5int32(vi__0, inline514)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline512 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline512
    var vs__3 *_goml_vec_string
    var inline510 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline510
    var inline507 string = "hello"
    vec_push__Vec_6string(vs__3, inline507)
    var inline504 string = "world"
    vec_push__Vec_6string(vs__3, inline504)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline502 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline502
    var vb__6 *_goml_vec_bool
    var inline500 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline500
    var inline497 bool = true
    vec_push__Vec_4bool(vb__6, inline497)
    var inline494 bool = false
    vec_push__Vec_4bool(vb__6, inline494)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline492 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline492
    var t420 string
    var inline490 string = _goml_runtime_core_int32_to_string(val_i__1)
    t420 = inline490
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline487)
    var t421 string
    var inline485 string = _goml_runtime_core_int_to_string(len_i__2)
    t421 = inline485
    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline482)
    var inline479 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline479)
    var t422 string
    var inline477 string = _goml_runtime_core_int_to_string(len_s__5)
    t422 = inline477
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline474)
    var t423 string
    var inline472 string = _goml_runtime_core_bool_to_string(val_b__7)
    t423 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline469)
    var t424 string
    var inline467 string = _goml_runtime_core_int_to_string(len_b__8)
    t424 = inline467
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline464)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t427 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t427
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
