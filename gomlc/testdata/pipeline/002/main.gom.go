package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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
        _1: false,
    }
    var x108 bool = a__0._0
    var x109 bool = a__0._1
    var jp117 Tuple2_4bool_4bool
    switch x109 {
    case true:
        var jp132 Tuple2_4bool_4bool
        switch x108 {
        case true:
            var t133 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp132 = t133
        case false:
            var t134 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp132 = t134
        default:
            panic("non-exhaustive match")
        }
        jp117 = jp132
    case false:
        var jp136 Tuple2_4bool_4bool
        switch x108 {
        case true:
            var t137 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp136 = t137
        case false:
            var t138 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp136 = t138
        default:
            panic("non-exhaustive match")
        }
        jp117 = jp136
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp117
    var x111 bool = b__1._1
    var w__2 bool = x111
    var b_1__3 bool = w__2
    var mtmp112 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x113 bool = mtmp112._0
    var x114 bool = mtmp112._1
    switch x114 {
    case true:
        switch x113 {
        case true:
            var t122 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t122)
        case false:
            var t124 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t124)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x113 {
        case true:
            var t127 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t127)
        case false:
            var t129 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t129)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t119 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t119)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv143 string
    var t144 string = _goml_runtime_core_int_to_string(self__5)
    retv143 = t144
    return retv143
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv146 string
    var t147 string = _goml_runtime_core_unit_to_string(self__36)
    retv146 = t147
    return retv146
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv149 string
    retv149 = self__38
    return retv149
}

func main() {
    main0()
}
