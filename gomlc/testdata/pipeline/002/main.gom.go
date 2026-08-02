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
    var x155 bool = true
    var x156 bool = false
    var jp164 Tuple2_4bool_4bool
    switch x156 {
    case true:
        switch x155 {
        case true:
            var t180 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp164 = t180
        case false:
            var t181 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp164 = t181
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x155 {
        case true:
            var t184 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp164 = t184
        case false:
            var t185 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp164 = t185
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x158 bool = jp164._1
    var x160 bool = true
    switch x158 {
    case true:
        switch x160 {
        case true:
            var t169 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t169)
        case false:
            var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t171)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x160 {
        case true:
            var t174 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t174)
        case false:
            var t176 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t176)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t166 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t166)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t191 string = _goml_runtime_core_int_to_string(self__5)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var t194 string = _goml_runtime_core_unit_to_string(self__36)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
