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
    var inline226 int = 0
    var inline227 *ref_int_x = ref__Ref_3int(inline226)
    current__1 = inline227
    for {
        var value__2 int
        var inline224 int = ref_get__Ref_3int(current__1)
        value__2 = inline224
        var t187 bool = value__2 >= 5
        if t187 {
            return None{}
        } else {
            var t189 bool
            var inline222 bool = value__2 == wanted__0
            t189 = inline222
            if t189 {
                var t190 Option__int = Some{
                    _0: value__2,
                }
                return t190
            } else {
                var t191 int = value__2 + 1
                ref_set__Ref_3int(current__1, t191)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t197 bool
    var inline250 int = 3
    var inline251 Option__int = find(inline250)
    switch inline251.(type) {
    case None:
        t197 = false
    case Some:
        t197 = true
    default:
        panic("non-exhaustive match")
    }
    var inline247 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t197)
    _goml_runtime_core_string_println(inline247)
    var t198 bool
    var inline243 int = 8
    var inline244 Option__int = find(inline243)
    switch inline244.(type) {
    case None:
        t198 = false
    case Some:
        t198 = true
    default:
        panic("non-exhaustive match")
    }
    var inline240 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t215 string = _goml_runtime_core_bool_to_string(self__66)
    return t215
}

func main() {
    main0()
}
