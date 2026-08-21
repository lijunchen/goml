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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__int32 {
    var jp427 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp411 Option__int32
            if inner_flag__3 {
                var inline459 Option__int32 = Option__int32{
                    _tag: 1,
                    _v1_0: 8,
                }
                mtmp411 = inline459
            } else {
                mtmp411 = Option__int32{
                    _tag: 0,
                }
            }
            var jp432 int32
            switch mtmp411._tag {
            case 0:
                return Option__int32{
                    _tag: 0,
                }
            case 1:
                var x412 int32 = mtmp411._v1_0
                jp432 = x412
                var t433 int32 = jp432 + 1
                jp427 = t433
                var t428 Option__int32 = Option__int32{
                    _tag: 1,
                    _v1_0: jp427,
                }
                return t428
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp427 = 20
            var t428 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: jp427,
            }
            return t428
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp413 Option__int32
        if inner_flag__3 {
            var inline461 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: 8,
            }
            mtmp413 = inline461
        } else {
            mtmp413 = Option__int32{
                _tag: 0,
            }
        }
        var jp435 int32
        switch mtmp413._tag {
        case 0:
            return Option__int32{
                _tag: 0,
            }
        case 1:
            var x414 int32 = mtmp413._v1_0
            jp435 = x414
            var t436 int32 = jp435 + 2
            jp427 = t436
            var t428 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: jp427,
            }
            return t428
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t444 Option__int32 = nested(true, Take, true)
    var t445 string
    switch t444._tag {
    case 0:
        t445 = "none"
    case 1:
        var inline484 int32 = t444._v1_0
        var inline486 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline484)
        var inline487 string = "some=" + inline486
        t445 = inline487
    default:
        panic("non-exhaustive match")
    }
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline481)
    var t446 Option__int32 = nested(true, Skip, false)
    var t447 string
    switch t446._tag {
    case 0:
        t447 = "none"
    case 1:
        var inline476 int32 = t446._v1_0
        var inline478 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline476)
        var inline479 string = "some=" + inline478
        t447 = inline479
    default:
        panic("non-exhaustive match")
    }
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline473)
    var t448 Option__int32 = nested(false, Take, false)
    var t449 string
    switch t448._tag {
    case 0:
        t449 = "none"
    case 1:
        var inline468 int32 = t448._v1_0
        var inline470 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline468)
        var inline471 string = "some=" + inline470
        t449 = inline471
    default:
        panic("non-exhaustive match")
    }
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline465)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t452 string = _goml_runtime_core_int32_to_string(self__33)
    return t452
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
