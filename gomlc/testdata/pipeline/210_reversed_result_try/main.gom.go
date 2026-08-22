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

type Result__i32__string struct {
    _tag int32
    _v0_0 string
    _v1_0 int32
}

func parse(flag__0 bool) Result__i32__string {
    if flag__0 {
        var t421 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: 41,
        }
        return t421
    } else {
        var t422 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        return t422
    }
}

func compute(flag__1 bool) Result__i32__string {
    var mtmp411 Result__i32__string
    if flag__1 {
        var inline450 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: 41,
        }
        mtmp411 = inline450
    } else {
        var inline451 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: "bad",
        }
        mtmp411 = inline451
    }
    var jp426 int32
    switch mtmp411._tag {
    case 0:
        var x412 string = mtmp411._v0_0
        var t429 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: x412,
        }
        return t429
    case 1:
        var x413 int32 = mtmp411._v1_0
        jp426 = x413
        var t427 int32 = jp426 + 1
        var t428 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: t427,
        }
        return t428
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t436 Result__i32__string = compute(true)
    var t437 string
    switch t436._tag {
    case 0:
        var inline480 string = t436._v0_0
        t437 = inline480
    case 1:
        var inline482 int32 = t436._v1_0
        var inline484 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline482)
        t437 = inline484
    default:
        panic("non-exhaustive match")
    }
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline477)
    var t438 Result__i32__string
    var inline464 bool = false
    var inline465 Result__i32__string = parse(inline464)
    var inline467 int32
    switch inline465._tag {
    case 0:
        var inline471 string = inline465._v0_0
        var inline473 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: inline471,
        }
        t438 = inline473
        var t439 string
        switch t438._tag {
        case 0:
            var inline458 string = t438._v0_0
            t439 = inline458
        case 1:
            var inline460 int32 = t438._v1_0
            var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
            t439 = inline462
        default:
            panic("non-exhaustive match")
        }
        var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
        _goml_runtime_core_string_println(inline455)
        return struct{}{}
    case 1:
        var inline474 int32 = inline465._v1_0
        inline467 = inline474
        var inline469 int32 = inline467 + 1
        var inline470 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: inline469,
        }
        t438 = inline470
        var t439 string
        switch t438._tag {
        case 0:
            var inline458 string = t438._v0_0
            t439 = inline458
        case 1:
            var inline460 int32 = t438._v1_0
            var inline462 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline460)
            t439 = inline462
        default:
            panic("non-exhaustive match")
        }
        var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
        _goml_runtime_core_string_println(inline455)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__33)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
