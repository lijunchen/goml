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
    var inline239 int = 0
    var inline240 *ref_int_x = ref__Ref_3int(inline239)
    current__1 = inline240
    for {
        var value__2 int
        var inline237 int = ref_get__Ref_3int(current__1)
        value__2 = inline237
        var t202 bool = value__2 >= 5
        if t202 {
            return None{}
        } else {
            var t204 bool = value__2 == wanted__0
            if t204 {
                var t205 Option__int = Some{
                    _0: value__2,
                }
                return t205
            } else {
                var t206 int = value__2 + 1
                ref_set__Ref_3int(current__1, t206)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t212 bool
    var inline262 int = 3
    var inline263 Option__int = find(inline262)
    switch inline263.(type) {
    case None:
        t212 = false
    case Some:
        t212 = true
    default:
        panic("non-exhaustive match")
    }
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t212)
    _goml_runtime_core_string_println(inline259)
    var t213 bool
    var inline256 int = 8
    var inline257 Option__int = find(inline256)
    switch inline257.(type) {
    case None:
        t213 = false
    case Some:
        t213 = true
    default:
        panic("non-exhaustive match")
    }
    var inline253 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t213)
    _goml_runtime_core_string_println(inline253)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t227 string = _goml_runtime_core_bool_to_string(self__64)
    return t227
}

func main() {
    main0()
}
