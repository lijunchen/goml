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
    var inline472 string = "if"
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline472)
    _goml_runtime_core_string_println(inline473)
    var mtmp412 int = 1
    switch mtmp412 {
    case 1:
        var inline476 string = "match"
        var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline476)
        _goml_runtime_core_string_println(inline477)
    default:
    }
    var index__0 *ref_int_x
    var inline505 int = 0
    var inline506 *ref_int_x = ref__Ref_3int(inline505)
    index__0 = inline506
    Loop_loop433:
    for {
        var t434 int
        var inline491 int = ref_get__Ref_3int(index__0)
        t434 = inline491
        var t435 bool = t434 < 2
        if t435 {
            var t436 int
            var inline489 int = ref_get__Ref_3int(index__0)
            t436 = inline489
            var t437 string
            var inline487 string = _goml_runtime_core_int_to_string(t436)
            t437 = inline487
            var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
            _goml_runtime_core_string_println(inline484)
            var t438 int
            var inline482 int = ref_get__Ref_3int(index__0)
            t438 = inline482
            var t439 int = t438 + 1
            ref_set__Ref_3int(index__0, t439)
            continue
        } else {
            break Loop_loop433
        }
    }
    var values__1 *_goml_vec_string
    var inline503 *_goml_vec_string = vec_new__Vec_6string()
    values__1 = inline503
    var inline500 string = "for"
    vec_push__Vec_6string(values__1, inline500)
    var for_limit418 int = vec_len__Vec_6string(values__1)
    var for_index419 int = 0
    Loop_loop429:
    for {
        var t430 bool = for_index419 < for_limit418
        if t430 {
            var for_item420 string = vec_get__Vec_6string(values__1, for_index419)
            var t431 int = for_index419 + 1
            for_index419 = t431
            var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(for_item420)
            _goml_runtime_core_string_println(inline493)
            continue
        } else {
            break Loop_loop429
        }
    }
    var inline496 string = "done"
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline496)
    _goml_runtime_core_string_println(inline497)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
