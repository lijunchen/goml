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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var x155 bool = true
    var x156 bool = true
    switch x156 {
    case true:
        switch x155 {
        case true:
            var t160 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t160)
            return struct{}{}
        case false:
            var t162 string = _goml_m_inherent_i_int_i_int_i_to__string(456)
            println__T_string(t162)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x155 {
        case true:
            var t165 string = _goml_m_inherent_i_int_i_int_i_to__string(123)
            println__T_string(t165)
            return struct{}{}
        case false:
            var t167 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t167)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t174 string = _goml_runtime_core_int_to_string(self__5)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
