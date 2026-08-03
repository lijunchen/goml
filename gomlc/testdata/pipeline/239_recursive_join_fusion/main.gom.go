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

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func find(wanted__0 int) Option__int {
    var current__1 *ref_int_x
    var inline190 int = 0
    var inline191 *ref_int_x = ref__Ref_3int(inline190)
    current__1 = inline191
    for {
        var value__2 int
        var inline188 int = ref_get__Ref_3int(current__1)
        value__2 = inline188
        var t151 bool = value__2 >= 5
        if t151 {
            return None{}
        } else {
            var t153 bool
            var inline186 bool = value__2 == wanted__0
            t153 = inline186
            if t153 {
                var t154 Option__int = Some{
                    _0: value__2,
                }
                return t154
            } else {
                var t155 int = value__2 + 1
                ref_set__Ref_3int(current__1, t155)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t161 bool
    var inline214 int = 3
    var inline215 Option__int = find(inline214)
    switch inline215.(type) {
    case None:
        t161 = false
    case Some:
        t161 = true
    default:
        panic("non-exhaustive match")
    }
    var inline211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t161)
    _goml_runtime_core_string_println(inline211)
    var t162 bool
    var inline207 int = 8
    var inline208 Option__int = find(inline207)
    switch inline208.(type) {
    case None:
        t162 = false
    case Some:
        t162 = true
    default:
        panic("non-exhaustive match")
    }
    var inline204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t162)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t179 string = _goml_runtime_core_bool_to_string(self__66)
    return t179
}

func main() {
    main0()
}
