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
    var x411 bool = true
    var x412 bool = false
    var jp420 Tuple2_4bool_4bool
    switch x412 {
    case true:
        switch x411 {
        case true:
            var t436 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp420 = t436
        case false:
            var t437 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp420 = t437
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x411 {
        case true:
            var t440 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp420 = t440
        case false:
            var t441 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp420 = t441
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x414 bool = jp420._1
    var x416 bool = true
    switch x414 {
    case true:
        switch x416 {
        case true:
            var t425 string
            var inline457 int = 3
            var inline458 string = _goml_runtime_core_int_to_string(inline457)
            t425 = inline458
            var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
            _goml_runtime_core_string_println(inline454)
        case false:
            var t427 string
            var inline463 int = 1
            var inline464 string = _goml_runtime_core_int_to_string(inline463)
            t427 = inline464
            var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
            _goml_runtime_core_string_println(inline460)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x416 {
        case true:
            var t430 string
            var inline469 int = 2
            var inline470 string = _goml_runtime_core_int_to_string(inline469)
            t430 = inline470
            var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
            _goml_runtime_core_string_println(inline466)
        case false:
            var t432 string
            var inline475 int = 0
            var inline476 string = _goml_runtime_core_int_to_string(inline475)
            t432 = inline476
            var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline472)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t422 string
    var inline481 string = _goml_runtime_core_unit_to_string(c__4)
    t422 = inline481
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline478)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
