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

type Option__bool struct {
    _tag int32
    _v1_0 bool
}

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func run_some() Option__int32 {
    var i__2 *ref_int32_x
    var inline517 int32 = 0
    var inline518 *ref_int32_x = ref__Ref_5int32(inline517)
    i__2 = inline518
    var total__3 *ref_int32_x
    var inline514 int32 = 0
    var inline515 *ref_int32_x = ref__Ref_5int32(inline514)
    total__3 = inline515
    Loop_loop439:
    for {
        var t440 int32
        var inline510 int32 = ref_get__Ref_5int32(i__2)
        t440 = inline510
        var mtmp408 Option__bool
        var inline506 bool = t440 < 3
        if inline506 {
            var inline507 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp408 = inline507
        } else {
            var inline508 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: false,
            }
            mtmp408 = inline508
        }
        var jp442 bool
        switch mtmp408._tag {
        case 0:
            return Option__int32{
                _tag: 0,
            }
        case 1:
            var x409 bool = mtmp408._v1_0
            jp442 = x409
            if jp442 {
                var t443 int32
                var inline504 int32 = ref_get__Ref_5int32(total__3)
                t443 = inline504
                var t444 int32
                var inline502 int32 = ref_get__Ref_5int32(i__2)
                t444 = inline502
                var t445 int32 = t443 + t444
                ref_set__Ref_5int32(total__3, t445)
                var t446 int32
                var inline498 int32 = ref_get__Ref_5int32(i__2)
                t446 = inline498
                var t447 int32 = t446 + 1
                ref_set__Ref_5int32(i__2, t447)
                continue
            } else {
                break Loop_loop439
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t437 int32
    var inline512 int32 = ref_get__Ref_5int32(total__3)
    t437 = inline512
    var t438 Option__int32 = Option__int32{
        _tag: 1,
        _v1_0: t437,
    }
    return t438
}

func run_none() Option__int32 {
    var i__4 *ref_int32_x
    var inline540 int32 = 0
    var inline541 *ref_int32_x = ref__Ref_5int32(inline540)
    i__4 = inline541
    var total__5 *ref_int32_x
    var inline537 int32 = 0
    var inline538 *ref_int32_x = ref__Ref_5int32(inline537)
    total__5 = inline538
    Loop_loop453:
    for {
        var t454 int32
        var inline533 int32 = ref_get__Ref_5int32(i__4)
        t454 = inline533
        var mtmp413 Option__bool
        var inline530 bool = t454 < 2
        if inline530 {
            var inline531 Option__bool = Option__bool{
                _tag: 1,
                _v1_0: true,
            }
            mtmp413 = inline531
        } else {
            mtmp413 = Option__bool{
                _tag: 0,
            }
        }
        var jp456 bool
        switch mtmp413._tag {
        case 0:
            return Option__int32{
                _tag: 0,
            }
        case 1:
            var x414 bool = mtmp413._v1_0
            jp456 = x414
            if jp456 {
                var t457 int32
                var inline528 int32 = ref_get__Ref_5int32(total__5)
                t457 = inline528
                var t458 int32
                var inline526 int32 = ref_get__Ref_5int32(i__4)
                t458 = inline526
                var t459 int32 = t457 + t458
                ref_set__Ref_5int32(total__5, t459)
                var t460 int32
                var inline522 int32 = ref_get__Ref_5int32(i__4)
                t460 = inline522
                var t461 int32 = t460 + 1
                ref_set__Ref_5int32(i__4, t461)
                continue
            } else {
                break Loop_loop453
            }
        default:
            panic("non-exhaustive match")
        }
    }
    var t451 int32
    var inline535 int32 = ref_get__Ref_5int32(total__5)
    t451 = inline535
    var t452 Option__int32 = Option__int32{
        _tag: 1,
        _v1_0: t451,
    }
    return t452
}

func main0() struct{} {
    var t469 Option__int32 = run_some()
    var t470 string
    switch t469._tag {
    case 0:
        t470 = "none"
    case 1:
        var inline556 int32 = t469._v1_0
        var inline558 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline556)
        var inline559 string = "some=" + inline558
        t470 = inline559
    default:
        panic("non-exhaustive match")
    }
    var inline553 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline553)
    var t471 Option__int32 = run_none()
    var t472 string
    switch t471._tag {
    case 0:
        t472 = "none"
    case 1:
        var inline548 int32 = t471._v1_0
        var inline550 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline548)
        var inline551 string = "some=" + inline550
        t472 = inline551
    default:
        panic("non-exhaustive match")
    }
    var inline545 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
    _goml_runtime_core_string_println(inline545)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t483 string = _goml_runtime_core_int32_to_string(self__33)
    return t483
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
