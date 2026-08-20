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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type Option__int struct {
    _tag int32
    _v1_0 int
}

func find(wanted__0 int) Option__int {
    var current__1 *ref_int_x
    var inline460 int = 0
    var inline461 *ref_int_x = ref__Ref_3int(inline460)
    current__1 = inline461
    for {
        var value__2 int
        var inline458 int = ref_get__Ref_3int(current__1)
        value__2 = inline458
        var t423 bool = value__2 >= 5
        if t423 {
            return Option__int{
                _tag: 0,
            }
        } else {
            var t425 bool = value__2 == wanted__0
            if t425 {
                var t426 Option__int = Option__int{
                    _tag: 1,
                    _v1_0: value__2,
                }
                return t426
            } else {
                var t427 int = value__2 + 1
                ref_set__Ref_3int(current__1, t427)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t433 bool
    var inline483 int = 3
    var inline484 Option__int = find(inline483)
    switch inline484._tag {
    case 0:
        t433 = false
    case 1:
        t433 = true
    default:
        panic("non-exhaustive match")
    }
    var inline480 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t433)
    _goml_runtime_core_string_println(inline480)
    var t434 bool
    var inline477 int = 8
    var inline478 Option__int = find(inline477)
    switch inline478._tag {
    case 0:
        t434 = false
    case 1:
        t434 = true
    default:
        panic("non-exhaustive match")
    }
    var inline474 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t434)
    _goml_runtime_core_string_println(inline474)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t448 string = _goml_runtime_core_bool_to_string(self__148)
    return t448
}

func main() {
    main0()
}
