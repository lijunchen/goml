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
    var x64 bool = a__0._0
    var x65 bool = a__0._1
    switch x65 {
    case true:
        switch x64 {
        case true:
            var t69 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t69)
        case false:
            var t71 string = _goml_m_inherent_i_int_i_int_i_to__string(456)
            println__T_string(t71)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x64 {
        case true:
            var t74 string = _goml_m_inherent_i_int_i_int_i_to__string(123)
            println__T_string(t74)
        case false:
            var t76 string = _goml_m_inherent_i_int_i_int_i_to__string(789)
            println__T_string(t76)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int_to_string(self__5)
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv85 string
    retv85 = self__38
    return retv85
}

func main() {
    main0()
}
