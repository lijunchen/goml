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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func accumulate(limit__1 int32) Option__int32 {
    var sum__2 *ref_int32_x
    var inline492 int32 = 0
    var inline493 *ref_int32_x = ref__Ref_5int32(inline492)
    sum__2 = inline493
    var i__3 *ref_int32_x
    var inline489 int32 = 0
    var inline490 *ref_int32_x = ref__Ref_5int32(inline489)
    i__3 = inline490
    Loop_loop429:
    for {
        var t430 int32
        var inline485 int32 = ref_get__Ref_5int32(i__3)
        t430 = inline485
        var t431 bool = t430 < limit__1
        if t431 {
            var cur__4 int32
            var inline483 int32 = ref_get__Ref_5int32(i__3)
            cur__4 = inline483
            var t432 int32 = cur__4 + 1
            ref_set__Ref_5int32(i__3, t432)
            var t438 bool = cur__4 == 1
            if t438 {
                continue
            } else {
                var mtmp410 Option__int32
                var inline477 bool = cur__4 == 2
                if inline477 {
                    mtmp410 = None{}
                } else {
                    var inline478 int32 = cur__4 + 10
                    var inline479 Option__int32 = Some{
                        _0: inline478,
                    }
                    mtmp410 = inline479
                }
                var jp435 int32
                switch mtmp410.(type) {
                case None:
                    return None{}
                case Some:
                    var x411 int32 = mtmp410.(Some)._0
                    jp435 = x411
                    var t436 int32
                    var inline475 int32 = ref_get__Ref_5int32(sum__2)
                    t436 = inline475
                    var t437 int32 = t436 + jp435
                    ref_set__Ref_5int32(sum__2, t437)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            break Loop_loop429
        }
    }
    var t427 int32
    var inline487 int32 = ref_get__Ref_5int32(sum__2)
    t427 = inline487
    var t428 Option__int32 = Some{
        _0: t427,
    }
    return t428
}

func main0() struct{} {
    var t446 Option__int32 = accumulate(2)
    var t447 string
    switch t446.(type) {
    case None:
        t447 = "none"
    case Some:
        var inline508 int32 = t446.(Some)._0
        var inline510 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline508)
        var inline511 string = "some=" + inline510
        t447 = inline511
    default:
        panic("non-exhaustive match")
    }
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline505)
    var t448 Option__int32 = accumulate(4)
    var t449 string
    switch t448.(type) {
    case None:
        t449 = "none"
    case Some:
        var inline500 int32 = t448.(Some)._0
        var inline502 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline500)
        var inline503 string = "some=" + inline502
        t449 = inline503
    default:
        panic("non-exhaustive match")
    }
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
    _goml_runtime_core_string_println(inline497)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t460 string = _goml_runtime_core_int32_to_string(self__33)
    return t460
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
