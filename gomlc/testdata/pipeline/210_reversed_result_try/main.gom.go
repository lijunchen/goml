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

type Result__int32__string struct {
    _tag int32
    _v0_0 string
    _v1_0 int32
}

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t418 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: 41,
        }
        return t418
    } else {
        var t419 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        return t419
    }
}

func compute(flag__1 bool) Result__int32__string {
    var mtmp408 Result__int32__string
    if flag__1 {
        var inline447 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: 41,
        }
        mtmp408 = inline447
    } else {
        var inline448 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        mtmp408 = inline448
    }
    var jp423 int32
    switch mtmp408._tag {
    case 0:
        var x409 string = mtmp408._v0_0
        var t426 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: x409,
        }
        return t426
    case 1:
        var x410 int32 = mtmp408._v1_0
        jp423 = x410
        var t424 int32 = jp423 + 1
        var t425 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: t424,
        }
        return t425
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t433 Result__int32__string = compute(true)
    var t434 string
    switch t433._tag {
    case 0:
        var inline477 string = t433._v0_0
        t434 = inline477
    case 1:
        var inline479 int32 = t433._v1_0
        var inline481 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline479)
        t434 = inline481
    default:
        panic("non-exhaustive match")
    }
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline474)
    var t435 Result__int32__string
    var inline461 bool = false
    var inline462 Result__int32__string = parse(inline461)
    var inline464 int32
    switch inline462._tag {
    case 0:
        var inline468 string = inline462._v0_0
        var inline470 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: inline468,
        }
        t435 = inline470
        var t436 string
        switch t435._tag {
        case 0:
            var inline455 string = t435._v0_0
            t436 = inline455
        case 1:
            var inline457 int32 = t435._v1_0
            var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
            t436 = inline459
        default:
            panic("non-exhaustive match")
        }
        var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
        _goml_runtime_core_string_println(inline452)
        return struct{}{}
    case 1:
        var inline471 int32 = inline462._v1_0
        inline464 = inline471
        var inline466 int32 = inline464 + 1
        var inline467 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: inline466,
        }
        t435 = inline467
        var t436 string
        switch t435._tag {
        case 0:
            var inline455 string = t435._v0_0
            t436 = inline455
        case 1:
            var inline457 int32 = t435._v1_0
            var inline459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline457)
            t436 = inline459
        default:
            panic("non-exhaustive match")
        }
        var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
        _goml_runtime_core_string_println(inline452)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t440 string = _goml_runtime_core_int32_to_string(self__33)
    return t440
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
