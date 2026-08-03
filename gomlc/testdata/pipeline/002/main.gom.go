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
    var x177 bool = true
    var x178 bool = false
    var jp186 Tuple2_4bool_4bool
    switch x178 {
    case true:
        switch x177 {
        case true:
            var t202 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp186 = t202
        case false:
            var t203 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp186 = t203
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x177 {
        case true:
            var t206 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp186 = t206
        case false:
            var t207 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp186 = t207
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x180 bool = jp186._1
    var x182 bool = true
    switch x180 {
    case true:
        switch x182 {
        case true:
            var t191 string
            var inline223 int = 3
            var inline224 string = _goml_runtime_core_int_to_string(inline223)
            t191 = inline224
            var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
            _goml_runtime_core_string_println(inline220)
        case false:
            var t193 string
            var inline229 int = 1
            var inline230 string = _goml_runtime_core_int_to_string(inline229)
            t193 = inline230
            var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
            _goml_runtime_core_string_println(inline226)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x182 {
        case true:
            var t196 string
            var inline235 int = 2
            var inline236 string = _goml_runtime_core_int_to_string(inline235)
            t196 = inline236
            var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline232)
        case false:
            var t198 string
            var inline241 int = 0
            var inline242 string = _goml_runtime_core_int_to_string(inline241)
            t198 = inline242
            var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline238)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t188 string
    var inline247 string = _goml_runtime_core_unit_to_string(c__4)
    t188 = inline247
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline244)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
