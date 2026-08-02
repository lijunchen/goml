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
    var inline209 int = 0
    var inline210 *ref_int_x = ref__Ref_3int(inline209)
    current__1 = inline210
    for {
        var value__2 int
        var inline207 int = ref_get__Ref_3int(current__1)
        value__2 = inline207
        var t170 bool = value__2 >= 5
        if t170 {
            return None{}
        } else {
            var t172 bool
            var inline205 bool = value__2 == wanted__0
            t172 = inline205
            if t172 {
                var t173 Option__int = Some{
                    _0: value__2,
                }
                return t173
            } else {
                var t174 int = value__2 + 1
                ref_set__Ref_3int(current__1, t174)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t180 bool
    var inline233 int = 3
    var inline234 Option__int = find(inline233)
    switch inline234.(type) {
    case None:
        t180 = false
    case Some:
        t180 = true
    default:
        panic("non-exhaustive match")
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t180)
    _goml_runtime_core_string_println(inline230)
    var t181 bool
    var inline226 int = 8
    var inline227 Option__int = find(inline226)
    switch inline227.(type) {
    case None:
        t181 = false
    case Some:
        t181 = true
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t181)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t198 string = _goml_runtime_core_bool_to_string(self__37)
    return t198
}

func main() {
    main0()
}
