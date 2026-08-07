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
    var x172 bool = true
    var x173 bool = false
    var jp181 Tuple2_4bool_4bool
    switch x173 {
    case true:
        switch x172 {
        case true:
            var t197 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp181 = t197
        case false:
            var t198 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp181 = t198
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x172 {
        case true:
            var t201 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp181 = t201
        case false:
            var t202 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp181 = t202
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x175 bool = jp181._1
    var x177 bool = true
    switch x175 {
    case true:
        switch x177 {
        case true:
            var t186 string
            var inline218 int = 3
            var inline219 string = _goml_runtime_core_int_to_string(inline218)
            t186 = inline219
            var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
            _goml_runtime_core_string_println(inline215)
        case false:
            var t188 string
            var inline224 int = 1
            var inline225 string = _goml_runtime_core_int_to_string(inline224)
            t188 = inline225
            var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
            _goml_runtime_core_string_println(inline221)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x177 {
        case true:
            var t191 string
            var inline230 int = 2
            var inline231 string = _goml_runtime_core_int_to_string(inline230)
            t191 = inline231
            var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
            _goml_runtime_core_string_println(inline227)
        case false:
            var t193 string
            var inline236 int = 0
            var inline237 string = _goml_runtime_core_int_to_string(inline236)
            t193 = inline237
            var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
            _goml_runtime_core_string_println(inline233)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t183 string
    var inline242 string = _goml_runtime_core_unit_to_string(c__4)
    t183 = inline242
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
