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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func maybe_total(flag__2 bool) Option__i32 {
    var mtmp411 Option__i32
    if flag__2 {
        var inline462 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 3,
        }
        mtmp411 = inline462
    } else {
        mtmp411 = Option__i32{
            _tag: 0,
        }
    }
    var jp433 int32
    switch mtmp411._tag {
    case 0:
        return Option__i32{
            _tag: 0,
        }
    case 1:
        var x412 int32 = mtmp411._v1_0
        jp433 = x412
        var mtmp413 Option__i32
        var inline458 bool = jp433 > 0
        if inline458 {
            var inline459 int32 = jp433 * 2
            var inline460 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline459,
            }
            mtmp413 = inline460
        } else {
            mtmp413 = Option__i32{
                _tag: 0,
            }
        }
        var jp435 int32
        switch mtmp413._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x414 int32 = mtmp413._v1_0
            jp435 = x414
            var t436 int32 = jp433 + jp435
            var t437 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: t436,
            }
            return t437
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t445 Option__i32 = maybe_total(true)
    var t446 string
    switch t445._tag {
    case 0:
        t446 = "none"
    case 1:
        var inline477 int32 = t445._v1_0
        var inline479 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline477)
        var inline480 string = "some=" + inline479
        t446 = inline480
    default:
        panic("non-exhaustive match")
    }
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline474)
    var t447 Option__i32 = maybe_total(false)
    var t448 string
    switch t447._tag {
    case 0:
        t448 = "none"
    case 1:
        var inline469 int32 = t447._v1_0
        var inline471 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline469)
        var inline472 string = "some=" + inline471
        t448 = inline472
    default:
        panic("non-exhaustive match")
    }
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline466)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t451 string = _goml_runtime_core_int32_to_string(self__33)
    return t451
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
