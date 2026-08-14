package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var inline469 string = "if"
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline469)
    _goml_runtime_core_string_println(inline470)
    var mtmp409 int = 1
    switch mtmp409 {
    case 1:
        var inline473 string = "match"
        var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline473)
        _goml_runtime_core_string_println(inline474)
    default:
    }
    var index__0 *ref_int_x
    var inline502 int = 0
    var inline503 *ref_int_x = ref__Ref_3int(inline502)
    index__0 = inline503
    Loop_loop430:
    for {
        var t431 int
        var inline488 int = ref_get__Ref_3int(index__0)
        t431 = inline488
        var t432 bool = t431 < 2
        if t432 {
            var t433 int
            var inline486 int = ref_get__Ref_3int(index__0)
            t433 = inline486
            var t434 string
            var inline484 string = _goml_runtime_core_int_to_string(t433)
            t434 = inline484
            var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
            _goml_runtime_core_string_println(inline481)
            var t435 int
            var inline479 int = ref_get__Ref_3int(index__0)
            t435 = inline479
            var t436 int = t435 + 1
            ref_set__Ref_3int(index__0, t436)
            continue
        } else {
            break Loop_loop430
        }
    }
    var values__1 *_goml_vec_string
    var inline500 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline500
    var inline497 string = "for"
    vec_push__Vec_6string(values__1, inline497)
    var for_limit415 int = vec_len__Vec_6string(values__1)
    var for_index416 int = 0
    Loop_loop426:
    for {
        var t427 bool = for_index416 < for_limit415
        if t427 {
            var for_item417 string = vec_get__Vec_6string(values__1, for_index416)
            var t428 int = for_index416 + 1
            for_index416 = t428
            var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item417)
            _goml_runtime_core_string_println(inline490)
            continue
        } else {
            break Loop_loop426
        }
    }
    var inline493 string = "done"
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline493)
    _goml_runtime_core_string_println(inline494)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
