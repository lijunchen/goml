package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
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
    var a__0 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: true,
    }
    var x22 bool = a__0._0
    var x23 bool = a__0._1
    switch x23 {
    case true:
        switch x22 {
        case true:
            var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t27)
        case false:
            var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(456)
            println__T_string(t29)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x22 {
        case true:
            var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(123)
            println__T_string(t32)
        case false:
            var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t34)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__2)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv43 string
    retv43 = self__9
    return retv43
}

func main() {
    main0()
}
