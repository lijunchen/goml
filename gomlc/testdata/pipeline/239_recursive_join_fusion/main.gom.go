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
    var inline231 int = 0
    var inline232 *ref_int_x = ref__Ref_3int(inline231)
    current__1 = inline232
    for {
        var value__2 int
        var inline229 int = ref_get__Ref_3int(current__1)
        value__2 = inline229
        var t192 bool = value__2 >= 5
        if t192 {
            return None{}
        } else {
            var t194 bool
            var inline227 bool = value__2 == wanted__0
            t194 = inline227
            if t194 {
                var t195 Option__int = Some{
                    _0: value__2,
                }
                return t195
            } else {
                var t196 int = value__2 + 1
                ref_set__Ref_3int(current__1, t196)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t202 bool
    var inline255 int = 3
    var inline256 Option__int = find(inline255)
    switch inline256.(type) {
    case None:
        t202 = false
    case Some:
        t202 = true
    default:
        panic("non-exhaustive match")
    }
    var inline252 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t202)
    _goml_runtime_core_string_println(inline252)
    var t203 bool
    var inline248 int = 8
    var inline249 Option__int = find(inline248)
    switch inline249.(type) {
    case None:
        t203 = false
    case Some:
        t203 = true
    default:
        panic("non-exhaustive match")
    }
    var inline245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t203)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t220 string = _goml_runtime_core_bool_to_string(self__66)
    return t220
}

func main() {
    main0()
}
