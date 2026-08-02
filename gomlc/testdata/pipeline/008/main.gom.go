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
    var x155 bool = true
    switch x155 {
    case true:
        var t163 string
        var inline179 int = 2
        var inline180 string = _goml_runtime_core_int_to_string(inline179)
        t163 = inline180
        var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
        _goml_runtime_core_string_println(inline176)
        return struct{}{}
    case false:
        var t165 string
        var inline185 int = 3
        var inline186 string = _goml_runtime_core_int_to_string(inline185)
        t165 = inline186
        var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
        _goml_runtime_core_string_println(inline182)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
