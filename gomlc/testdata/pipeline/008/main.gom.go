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

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 struct{}
}

func (_ B) isT() {}

func main0() struct{} {
    var x136 bool = true
    switch x136 {
    case true:
        var t144 string
        var inline160 int = 2
        var inline161 string = _goml_runtime_core_int_to_string(inline160)
        t144 = inline161
        var inline157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t144)
        _goml_runtime_core_string_println(inline157)
        return struct{}{}
    case false:
        var t146 string
        var inline166 int = 3
        var inline167 string = _goml_runtime_core_int_to_string(inline166)
        t146 = inline167
        var inline163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
        _goml_runtime_core_string_println(inline163)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
