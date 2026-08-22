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
    var vi__0 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32()
    var inline517 int32 = 42
    vec_push__Vec_5int32(vi__0, inline517)
    var val_i__1 int32 = vec_get__Vec_5int32(vi__0, 0)
    var len_i__2 int
    var inline515 int = vec_len__Vec_5int32(vi__0)
    len_i__2 = inline515
    var vs__3 *_goml_vec_string
    var inline513 *_goml_vec_string = vec_new__Vec_6string()
    vs__3 = inline513
    var inline510 string = "hello"
    vec_push__Vec_6string(vs__3, inline510)
    var inline507 string = "world"
    vec_push__Vec_6string(vs__3, inline507)
    var val_s__4 string = vec_get__Vec_6string(vs__3, 1)
    var len_s__5 int
    var inline505 int = vec_len__Vec_6string(vs__3)
    len_s__5 = inline505
    var vb__6 *_goml_vec_bool
    var inline503 *_goml_vec_bool = vec_new__Vec_4bool()
    vb__6 = inline503
    var inline500 bool = true
    vec_push__Vec_4bool(vb__6, inline500)
    var inline497 bool = false
    vec_push__Vec_4bool(vb__6, inline497)
    var val_b__7 bool = vec_get__Vec_4bool(vb__6, 0)
    var len_b__8 int
    var inline495 int = vec_len__Vec_4bool(vb__6)
    len_b__8 = inline495
    var t423 string
    var inline493 string = _goml_runtime_core_int32_to_string(val_i__1)
    t423 = inline493
    var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline490)
    var t424 string
    var inline488 string = _goml_runtime_core_int_to_string(len_i__2)
    t424 = inline488
    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline485)
    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(val_s__4)
    _goml_runtime_core_string_println(inline482)
    var t425 string
    var inline480 string = _goml_runtime_core_int_to_string(len_s__5)
    t425 = inline480
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline477)
    var t426 string
    var inline475 string = _goml_runtime_core_bool_to_string(val_b__7)
    t426 = inline475
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline472)
    var t427 string
    var inline470 string = _goml_runtime_core_int_to_string(len_b__8)
    t427 = inline470
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline467)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__i32() *_goml_vec_int32 {
    var t430 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t430
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
