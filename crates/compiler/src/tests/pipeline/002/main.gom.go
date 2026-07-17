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
    var x58 bool = a__0._0
    var x59 bool = a__0._1
    var jp67 Tuple2_4bool_4bool
    switch x59 {
    case true:
        var jp82 Tuple2_4bool_4bool
        switch x58 {
        case true:
            var t83 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp82 = t83
        case false:
            var t84 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp82 = t84
        default:
            panic("non-exhaustive match")
        }
        jp67 = jp82
    case false:
        var jp86 Tuple2_4bool_4bool
        switch x58 {
        case true:
            var t87 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp86 = t87
        case false:
            var t88 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp86 = t88
        default:
            panic("non-exhaustive match")
        }
        jp67 = jp86
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp67
    var x61 bool = b__1._1
    var w__2 bool = x61
    var b_1__3 bool = w__2
    var mtmp62 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x63 bool = mtmp62._0
    var x64 bool = mtmp62._1
    switch x64 {
    case true:
        switch x63 {
        case true:
            var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t72)
        case false:
            var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
            println__T_string(t74)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x63 {
        case true:
            var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t77)
        case false:
            var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(0)
            println__T_string(t79)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t69 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int32_to_string(self__2)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__32 struct{}) string {
    var retv96 string
    var t97 string = _goml_runtime_core_unit_to_string(self__32)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv99 string
    retv99 = self__34
    return retv99
}

func main() {
    main0()
}
