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
    var inline463 int = 0
    var inline464 *ref_int_x = ref__Ref_3int(inline463)
    current__1 = inline464
    for {
        var value__2 int
        var inline461 int = ref_get__Ref_3int(current__1)
        value__2 = inline461
        var t426 bool = value__2 >= 5
        if t426 {
            return Option__int{
                _tag: 0,
            }
        } else {
            var t428 bool = value__2 == wanted__0
            if t428 {
                var t429 Option__int = Option__int{
                    _tag: 1,
                    _v1_0: value__2,
                }
                return t429
            } else {
                var t430 int = value__2 + 1
                ref_set__Ref_3int(current__1, t430)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t436 bool
    var inline486 int = 3
    var inline487 Option__int = find(inline486)
    switch inline487._tag {
    case 0:
        t436 = false
    case 1:
        t436 = true
    default:
        panic("non-exhaustive match")
    }
    var inline483 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t436)
    _goml_runtime_core_string_println(inline483)
    var t437 bool
    var inline480 int = 8
    var inline481 Option__int = find(inline480)
    switch inline481._tag {
    case 0:
        t437 = false
    case 1:
        t437 = true
    default:
        panic("non-exhaustive match")
    }
    var inline477 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t437)
    _goml_runtime_core_string_println(inline477)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t451 string = _goml_runtime_core_bool_to_string(self__148)
    return t451
}

func main() {
    main0()
}
