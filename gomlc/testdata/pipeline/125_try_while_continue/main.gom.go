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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x
    var inline495 int32 = 0
    var inline496 *ref_int32_x = ref__Ref_5int32(inline495)
    sum__2 = inline496
    var i__3 *ref_int32_x
    var inline492 int32 = 0
    var inline493 *ref_int32_x = ref__Ref_5int32(inline492)
    i__3 = inline493
    Loop_loop432:
    for {
        var t433 int32
        var inline488 int32 = ref_get__Ref_5int32(i__3)
        t433 = inline488
        var t434 bool = t433 < limit__1
        if t434 {
            var cur__4 int32
            var inline486 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline486
            var t435 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t435)
            var t441 bool = cur__4 == 1
            if t441 {
                continue
            } else {
                var mtmp413 Option__int32
                var inline480 bool = cur__4 == 2
                if inline480 {
                    mtmp413 = Option__int32{
                        _tag: 0,
                    }
                } else {
                    var inline481 int32 = cur__4 + 10
                    var inline482 Option__int32 = Option__int32{
                        _tag: 1,
                        _v1_0: inline481,
                    }
                    mtmp413 = inline482
                }
                var jp438 int32
                switch mtmp413._tag {
                case 0:
                    return Option__int32{
                        _tag: 0,
                    }
                case 1:
                    var x414 int32 = mtmp413._v1_0
                    jp438 = x414
                    var t439 int32
                    var inline478 int32 = ref_get__Ref_5int32(sum__2)
                    t439 = inline478
                    var t440 int32 = t439 + jp438
                    ref_set__Ref_5int32(sum__2, t440)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop432
        }
    }
    var t430 int32
    var inline490 int32 = ref_get__Ref_5int32(sum__2)
    t430 = inline490
    var t431 Option__int32 = Option__int32{
        _tag: 1,
        _v1_0: t430,
    }
    return t431
}

func main0() struct{} {
    var t449 Option__int32 = accumulate(2)
    var t450 string
    switch t449._tag {
    case 0:
        t450 = "none"
    case 1:
        var inline511 int32 = t449._v1_0
        var inline513 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline511)
        var inline514 string = "some=" + inline513
        t450 = inline514
    default:
        panic("non-exhaustive match")
    }
    var inline508 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline508)
    var t451 Option__int32 = accumulate(4)
    var t452 string
    switch t451._tag {
    case 0:
        t452 = "none"
    case 1:
        var inline503 int32 = t451._v1_0
        var inline505 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline503)
        var inline506 string = "some=" + inline505
        t452 = inline506
    default:
        panic("non-exhaustive match")
    }
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline500)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t463 string = _goml_runtime_core_int32_to_string(self__33)
    return t463
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
