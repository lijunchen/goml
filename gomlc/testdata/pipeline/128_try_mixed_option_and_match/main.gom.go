package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp411 Option__int32
    if primary__2 {
        var inline469 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: 4,
        }
        mtmp411 = inline469
    } else {
        mtmp411 = Option__int32{
            _tag: 0,
        }
    }
    var jp432 int32
    switch mtmp411._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x412 int32 = mtmp411._v1_0
        jp432 = x412
        var mtmp413 Option__int32
        if secondary__3 {
            var inline467 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: 9,
            }
            mtmp413 = inline467
        } else {
            mtmp413 = Option__int32{
                _tag: 0,
            }
        }
        var jp434 string
        switch mtmp413._tag {
        case 0:
            jp434 = "extra=none"
        case 1:
            var x414 int32 = mtmp413._v1_0
            var t440 string
            var inline463 string = _goml_runtime_core_int32_to_string(x414)
            t440 = inline463
            var t441 string = "extra=" + t440
            jp434 = t441
        default:
            panic("non-exhaustive match")
        }
        var t435 string
        var inline465 string = _goml_runtime_core_int32_to_string(jp432)
        t435 = inline465
        var t436 string = "value=" + t435
        var t437 string = t436 + ","
        var t438 string = t437 + jp434
        var t439 Option__string = Option__string{
            _tag: 1,
            _v1_0: t438,
        }
        return t439
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t448 Option__string = mixed(true, true)
    var t449 string
    switch t448._tag {
    case 0:
        t449 = "none"
    case 1:
        var inline488 string = t448._v1_0
        var inline490 string = "some=" + inline488
        t449 = inline490
    default:
        panic("non-exhaustive match")
    }
    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline485)
    var t450 Option__string = mixed(true, false)
    var t451 string
    switch t450._tag {
    case 0:
        t451 = "none"
    case 1:
        var inline481 string = t450._v1_0
        var inline483 string = "some=" + inline481
        t451 = inline483
    default:
        panic("non-exhaustive match")
    }
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t451)
    _goml_runtime_core_string_println(inline478)
    var t452 Option__string = mixed(false, true)
    var t453 string
    switch t452._tag {
    case 0:
        t453 = "none"
    case 1:
        var inline474 string = t452._v1_0
        var inline476 string = "some=" + inline474
        t453 = inline476
    default:
        panic("non-exhaustive match")
    }
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t453)
    _goml_runtime_core_string_println(inline471)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
