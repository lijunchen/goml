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
    var a__0 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: true,
    }
    var x152 bool = a__0._0
    var x153 bool = a__0._1
    switch x153 {
    case true:
        switch x152 {
        case true:
            var t157 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t157)
        case false:
            var t159 string = _goml_m_inherent_i_int_i_int_i_to__string(456)
            println__T_string(t159)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x152 {
        case true:
            var t162 string = _goml_m_inherent_i_int_i_int_i_to__string(123)
            println__T_string(t162)
        case false:
            var t164 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t164)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int_to_string(self__5)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv173 string
    retv173 = self__38
    return retv173
}

func main() {
    main0()
}
