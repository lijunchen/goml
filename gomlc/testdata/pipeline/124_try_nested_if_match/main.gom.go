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
    var jp424 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp408 Option__int32
            if inner_flag__3 {
                var inline456 Option__int32 = Option__int32{
                    _tag: 1,
                    _v1_0: 8,
                }
                mtmp408 = inline456
            } else {
                mtmp408 = Option__int32{
                    _tag: 0,
                }
            }
            var jp429 int32
            switch mtmp408._tag {
            case 0:
                return Option__int32{
                    _tag: 0,
                }
            case 1:
                var x409 int32 = mtmp408._v1_0
                jp429 = x409
                var t430 int32 = jp429 + 1
                jp424 = t430
                var t425 Option__int32 = Option__int32{
                    _tag: 1,
                    _v1_0: jp424,
                }
                return t425
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp424 = 20
            var t425 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: jp424,
            }
            return t425
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp410 Option__int32
        if inner_flag__3 {
            var inline458 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: 8,
            }
            mtmp410 = inline458
        } else {
            mtmp410 = Option__int32{
                _tag: 0,
            }
        }
        var jp432 int32
        switch mtmp410._tag {
        case 0:
            return Option__int32{
                _tag: 0,
            }
        case 1:
            var x411 int32 = mtmp410._v1_0
            jp432 = x411
            var t433 int32 = jp432 + 2
            jp424 = t433
            var t425 Option__int32 = Option__int32{
                _tag: 1,
                _v1_0: jp424,
            }
            return t425
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t441 Option__int32 = nested(true, Take, true)
    var t442 string
    switch t441._tag {
    case 0:
        t442 = "none"
    case 1:
        var inline481 int32 = t441._v1_0
        var inline483 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline481)
        var inline484 string = "some=" + inline483
        t442 = inline484
    default:
        panic("non-exhaustive match")
    }
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline478)
    var t443 Option__int32 = nested(true, Skip, false)
    var t444 string
    switch t443._tag {
    case 0:
        t444 = "none"
    case 1:
        var inline473 int32 = t443._v1_0
        var inline475 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline473)
        var inline476 string = "some=" + inline475
        t444 = inline476
    default:
        panic("non-exhaustive match")
    }
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
    _goml_runtime_core_string_println(inline470)
    var t445 Option__int32 = nested(false, Take, false)
    var t446 string
    switch t445._tag {
    case 0:
        t446 = "none"
    case 1:
        var inline465 int32 = t445._v1_0
        var inline467 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline465)
        var inline468 string = "some=" + inline467
        t446 = inline468
    default:
        panic("non-exhaustive match")
    }
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline462)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t449 string = _goml_runtime_core_int32_to_string(self__33)
    return t449
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
