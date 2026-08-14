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

type Ordering int32

func main0() struct{} {
    var x408 bool = true
    var x409 bool = false
    var jp417 Tuple2_4bool_4bool
    switch x409 {
    case true:
        switch x408 {
        case true:
            var t433 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp417 = t433
        case false:
            var t434 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp417 = t434
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x408 {
        case true:
            var t437 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp417 = t437
        case false:
            var t438 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp417 = t438
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x411 bool = jp417._1
    var x413 bool = true
    switch x411 {
    case true:
        switch x413 {
        case true:
            var t422 string
            var inline454 int = 3
            var inline455 string = _goml_runtime_core_int_to_string(inline454)
            t422 = inline455
            var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
            _goml_runtime_core_string_println(inline451)
        case false:
            var t424 string
            var inline460 int = 1
            var inline461 string = _goml_runtime_core_int_to_string(inline460)
            t424 = inline461
            var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
            _goml_runtime_core_string_println(inline457)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x413 {
        case true:
            var t427 string
            var inline466 int = 2
            var inline467 string = _goml_runtime_core_int_to_string(inline466)
            t427 = inline467
            var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
            _goml_runtime_core_string_println(inline463)
        case false:
            var t429 string
            var inline472 int = 0
            var inline473 string = _goml_runtime_core_int_to_string(inline472)
            t429 = inline473
            var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
            _goml_runtime_core_string_println(inline469)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t419 string
    var inline478 string = _goml_runtime_core_unit_to_string(c__4)
    t419 = inline478
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline475)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
