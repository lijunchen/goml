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
            var t169 string
            var inline201 int = 3
            var inline202 string = _goml_runtime_core_int_to_string(inline201)
            t169 = inline202
            var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t169)
            _goml_runtime_core_string_println(inline198)
        case false:
            var t171 string
            var inline207 int = 1
            var inline208 string = _goml_runtime_core_int_to_string(inline207)
            t171 = inline208
            var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t171)
            _goml_runtime_core_string_println(inline204)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x160 {
        case true:
            var t174 string
            var inline213 int = 2
            var inline214 string = _goml_runtime_core_int_to_string(inline213)
            t174 = inline214
            var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
            _goml_runtime_core_string_println(inline210)
        case false:
            var t176 string
            var inline219 int = 0
            var inline220 string = _goml_runtime_core_int_to_string(inline219)
            t176 = inline220
            var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
            _goml_runtime_core_string_println(inline216)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t166 string
    var inline225 string = _goml_runtime_core_unit_to_string(c__4)
    t166 = inline225
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
