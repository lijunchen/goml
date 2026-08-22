package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_main_4 struct {}

type closure_env_main_5 struct {}

type closure_env_main_6 struct {}

type closure_env_main_7 struct {}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__isize__isize struct {
    _tag int32
    _v0_0 int
    _v1_0 int
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func main0() struct{} {
    var some__0 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 3,
    }
    var t427 closure_env_main_0 = closure_env_main_0{}
    var t428 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t427, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(some__0, t428)
    var t429 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t429)
    var t430 closure_env_main_1 = closure_env_main_1{}
    var t431 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t430, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(some__0, t431)
    var t432 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t432)
    var t433 closure_env_main_2 = closure_env_main_2{}
    var t434 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t433, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__isize____U__string(some__0, t434)
    var t435 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t435)
    var none__7 Option__isize = Option__isize{
        _tag: 0,
    }
    var converted__8 Result__isize__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__isize(none__7, "none")
    var t436 closure_env_main_3 = closure_env_main_3{}
    var t437 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t436, p0)
    }
    var t438 int = _goml_m_inherent_i_Result_i_Re_had11e393bde0ae88c9d8324ffd70f925_ing____T__isize(converted__8, t437)
    println__T_isize(t438)
    var ok__10 Result__isize__string = Result__isize__string{
        _tag: 0,
        _v0_0: 5,
    }
    var t439 closure_env_main_4 = closure_env_main_4{}
    var t440 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t439, p0)
    }
    var t441 Result__isize__string = _goml_m_inherent_i_Result_i_Re_hf15fd215f39b8121388b37682eabc3c0_ize____U__isize(ok__10, t440)
    var t442 int
    var inline590 int = 0
    switch t441._tag {
    case 0:
        var inline591 int = t441._v0_0
        t442 = inline591
    case 1:
        t442 = inline590
    default:
        panic("non-exhaustive match")
    }
    var inline587 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t442)
    _goml_runtime_core_string_println(inline587)
    var t443 closure_env_main_5 = closure_env_main_5{}
    var t444 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t443, p0)
    }
    var mapped_error__14 Result__isize__isize
    var inline582 string = "bad"
    var inline584 int = t444(inline582)
    var inline585 Result__isize__isize = Result__isize__isize{
        _tag: 1,
        _v1_0: inline584,
    }
    mapped_error__14 = inline585
    var t445 closure_env_main_6 = closure_env_main_6{}
    var t446 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t445, p0)
    }
    var t447 int
    switch mapped_error__14._tag {
    case 0:
        var inline573 int = mapped_error__14._v0_0
        t447 = inline573
    case 1:
        var inline575 int = mapped_error__14._v1_0
        var inline577 int = t446(inline575)
        t447 = inline577
    default:
        panic("non-exhaustive match")
    }
    var inline570 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t447)
    _goml_runtime_core_string_println(inline570)
    var t448 closure_env_main_7 = closure_env_main_7{}
    var t449 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t448, p0)
    }
    var next__17 Result__string__string
    var inline563 int = 5
    var inline565 Result__string__string = t449(inline563)
    next__17 = inline565
    var t450 string
    var inline559 string = "missing"
    switch next__17._tag {
    case 0:
        var inline560 string = next__17._v0_0
        t450 = inline560
    case 1:
        t450 = inline559
    default:
        panic("non-exhaustive match")
    }
    var inline556 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline556)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__isize____U__string(self__482 Option__isize, map_fn__483 func(int) string) Option__string {
    switch self__482._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x398 int = self__482._v1_0
        var t458 string = map_fn__483(x398)
        var t459 Option__string = Option__string{
            _tag: 1,
            _v1_0: t458,
        }
        return t459
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t461 string
    t461 = value__1
    _goml_runtime_core_string_println(t461)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__467 Option__string, fallback__468 string) string {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 string = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__isize____U__string(self__485 Option__isize, next__486 func(int) Option__string) Option__string {
    switch self__485._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x399 int = self__485._v1_0
        var t471 Option__string = next__486(x399)
        return t471
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__isize(self__488 Option__isize, error__489 string) Result__isize__string {
    switch self__488._tag {
    case 0:
        var t476 Result__isize__string = Result__isize__string{
            _tag: 1,
            _v1_0: error__489,
        }
        return t476
    case 1:
        var x400 int = self__488._v1_0
        var t477 Result__isize__string = Result__isize__string{
            _tag: 0,
            _v0_0: x400,
        }
        return t477
    default:
        panic("non-exhaustive match")
    }
}

func println__T_isize(value__1 int) struct{} {
    var t479 string
    var inline595 string = _goml_runtime_core_int_to_string(value__1)
    t479 = inline595
    _goml_runtime_core_string_println(t479)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_had11e393bde0ae88c9d8324ffd70f925_ing____T__isize(self__478 Result__isize__string, fallback__479 func(string) int) int {
    switch self__478._tag {
    case 0:
        var x396 int = self__478._v0_0
        return x396
    case 1:
        var x397 string = self__478._v1_0
        var t488 int = fallback__479(x397)
        return t488
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hf15fd215f39b8121388b37682eabc3c0_ize____U__isize(self__491 Result__isize__string, map_fn__492 func(int) int) Result__isize__string {
    switch self__491._tag {
    case 0:
        var x401 int = self__491._v0_0
        var t493 int = map_fn__492(x401)
        var t494 Result__isize__string = Result__isize__string{
            _tag: 0,
            _v0_0: t493,
        }
        return t494
    case 1:
        var x402 string = self__491._v1_0
        var t495 Result__isize__string = Result__isize__string{
            _tag: 1,
            _v1_0: x402,
        }
        return t495
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t526 string = _goml_runtime_core_int_to_string(self__151)
    return t526
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env418 closure_env_main_0, value__1 int) string {
    var inline597 string = _goml_runtime_core_int_to_string(value__1)
    return inline597
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env419 closure_env_main_1, value__3 int) string {
    var t532 string
    var inline599 string = _goml_runtime_core_int_to_string(value__3)
    t532 = inline599
    var t533 string = "static:" + t532
    return t533
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env420 closure_env_main_2, value__5 int) Option__string {
    var t536 string
    var inline601 string = _goml_runtime_core_int_to_string(value__5)
    t536 = inline601
    var t537 string = "value:" + t536
    var t538 Option__string = Option__string{
        _tag: 1,
        _v1_0: t537,
    }
    return t538
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env421 closure_env_main_3, error__9 string) int {
    var inline603 int = _goml_runtime_core_string_len(error__9)
    return inline603
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env422 closure_env_main_4, value__11 int) int {
    var t544 int = value__11 + 2
    return t544
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env423 closure_env_main_5, value__13 string) int {
    var inline605 int = _goml_runtime_core_string_len(value__13)
    return inline605
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env424 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env425 closure_env_main_7, value__16 int) Result__string__string {
    var t552 string
    var inline607 string = _goml_runtime_core_int_to_string(value__16)
    t552 = inline607
    var t553 string = "next:" + t552
    var t554 Result__string__string = Result__string__string{
        _tag: 0,
        _v0_0: t553,
    }
    return t554
}

func main() {
    main0()
}
