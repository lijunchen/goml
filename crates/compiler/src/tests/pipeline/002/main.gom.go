package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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
        _1: false,
    }
    var x61 bool = a__0._0
    var x62 bool = a__0._1
    var jp70 Tuple2_4bool_4bool
    switch x62 {
    case true:
        var jp85 Tuple2_4bool_4bool
        switch x61 {
        case true:
            var t86 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp85 = t86
        case false:
            var t87 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp85 = t87
        default:
            panic("non-exhaustive match")
        }
        jp70 = jp85
    case false:
        var jp89 Tuple2_4bool_4bool
        switch x61 {
        case true:
            var t90 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp89 = t90
        case false:
            var t91 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp89 = t91
        default:
            panic("non-exhaustive match")
        }
        jp70 = jp89
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp70
    var x64 bool = b__1._1
    var w__2 bool = x64
    var b_1__3 bool = w__2
    var mtmp65 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x66 bool = mtmp65._0
    var x67 bool = mtmp65._1
    switch x67 {
    case true:
        switch x66 {
        case true:
            var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t75)
        case false:
            var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
            println__T_string(t77)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x66 {
        case true:
            var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t80)
        case false:
            var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(0)
            println__T_string(t82)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t72 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t72)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__5)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__35 struct{}) string {
    var retv99 string
    var t100 string = _goml_runtime_core_unit_to_string(self__35)
    retv99 = t100
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv102 string
    retv102 = self__37
    return retv102
}

func main() {
    main0()
}
