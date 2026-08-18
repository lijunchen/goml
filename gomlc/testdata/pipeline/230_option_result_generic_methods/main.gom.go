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

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Result__int__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__int__int struct {
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
    var some__0 Option__int = Option__int{
        _tag: 1,
        _v1_0: 3,
    }
    var t424 closure_env_main_0 = closure_env_main_0{}
    var t425 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t424, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t425)
    var t426 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t426)
    var t427 closure_env_main_1 = closure_env_main_1{}
    var t428 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t427, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t428)
    var t429 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t429)
    var t430 closure_env_main_2 = closure_env_main_2{}
    var t431 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t430, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, t431)
    var t432 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t432)
    var none__7 Option__int = Option__int{
        _tag: 0,
    }
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t433 closure_env_main_3 = closure_env_main_3{}
    var t434 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t433, p0)
    }
    var t435 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, t434)
    println__T_int(t435)
    var ok__10 Result__int__string = Result__int__string{
        _tag: 0,
        _v0_0: 5,
    }
    var t436 closure_env_main_4 = closure_env_main_4{}
    var t437 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t436, p0)
    }
    var t438 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, t437)
    var t439 int
    var inline587 int = 0
    switch t438._tag {
    case 0:
        var inline588 int = t438._v0_0
        t439 = inline588
    case 1:
        t439 = inline587
    default:
        panic("non-exhaustive match")
    }
    var inline584 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t439)
    _goml_runtime_core_string_println(inline584)
    var t440 closure_env_main_5 = closure_env_main_5{}
    var t441 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t440, p0)
    }
    var mapped_error__14 Result__int__int
    var inline579 string = "bad"
    var inline581 int = t441(inline579)
    var inline582 Result__int__int = Result__int__int{
        _tag: 1,
        _v1_0: inline581,
    }
    mapped_error__14 = inline582
    var t442 closure_env_main_6 = closure_env_main_6{}
    var t443 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t442, p0)
    }
    var t444 int
    switch mapped_error__14._tag {
    case 0:
        var inline570 int = mapped_error__14._v0_0
        t444 = inline570
    case 1:
        var inline572 int = mapped_error__14._v1_0
        var inline574 int = t443(inline572)
        t444 = inline574
    default:
        panic("non-exhaustive match")
    }
    var inline567 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t444)
    _goml_runtime_core_string_println(inline567)
    var t445 closure_env_main_7 = closure_env_main_7{}
    var t446 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t445, p0)
    }
    var next__17 Result__string__string
    var inline560 int = 5
    var inline562 Result__string__string = t446(inline560)
    next__17 = inline562
    var t447 string
    var inline556 string = "missing"
    switch next__17._tag {
    case 0:
        var inline557 string = next__17._v0_0
        t447 = inline557
    case 1:
        t447 = inline556
    default:
        panic("non-exhaustive match")
    }
    var inline553 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline553)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__473 Option__int, map_fn__474 func(int) string) Option__string {
    switch self__473._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x395 int = self__473._v1_0
        var t455 string = map_fn__474(x395)
        var t456 Option__string = Option__string{
            _tag: 1,
            _v1_0: t455,
        }
        return t456
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t458 string
    t458 = value__1
    _goml_runtime_core_string_println(t458)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__458 Option__string, fallback__459 string) string {
    switch self__458._tag {
    case 0:
        return fallback__459
    case 1:
        var x387 string = self__458._v1_0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__476 Option__int, next__477 func(int) Option__string) Option__string {
    switch self__476._tag {
    case 0:
        return Option__string{
            _tag: 0,
        }
    case 1:
        var x396 int = self__476._v1_0
        var t468 Option__string = next__477(x396)
        return t468
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__479 Option__int, error__480 string) Result__int__string {
    switch self__479._tag {
    case 0:
        var t473 Result__int__string = Result__int__string{
            _tag: 1,
            _v1_0: error__480,
        }
        return t473
    case 1:
        var x397 int = self__479._v1_0
        var t474 Result__int__string = Result__int__string{
            _tag: 0,
            _v0_0: x397,
        }
        return t474
    default:
        panic("non-exhaustive match")
    }
}

func println__T_int(value__1 int) struct{} {
    var t476 string
    var inline592 string = _goml_runtime_core_int_to_string(value__1)
    t476 = inline592
    _goml_runtime_core_string_println(t476)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__469 Result__int__string, fallback__470 func(string) int) int {
    switch self__469._tag {
    case 0:
        var x393 int = self__469._v0_0
        return x393
    case 1:
        var x394 string = self__469._v1_0
        var t485 int = fallback__470(x394)
        return t485
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__482 Result__int__string, map_fn__483 func(int) int) Result__int__string {
    switch self__482._tag {
    case 0:
        var x398 int = self__482._v0_0
        var t490 int = map_fn__483(x398)
        var t491 Result__int__string = Result__int__string{
            _tag: 0,
            _v0_0: t490,
        }
        return t491
    case 1:
        var x399 string = self__482._v1_0
        var t492 Result__int__string = Result__int__string{
            _tag: 1,
            _v1_0: x399,
        }
        return t492
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t523 string = _goml_runtime_core_int_to_string(self__151)
    return t523
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env415 closure_env_main_0, value__1 int) string {
    var inline594 string = _goml_runtime_core_int_to_string(value__1)
    return inline594
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env416 closure_env_main_1, value__3 int) string {
    var t529 string
    var inline596 string = _goml_runtime_core_int_to_string(value__3)
    t529 = inline596
    var t530 string = "static:" + t529
    return t530
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env417 closure_env_main_2, value__5 int) Option__string {
    var t533 string
    var inline598 string = _goml_runtime_core_int_to_string(value__5)
    t533 = inline598
    var t534 string = "value:" + t533
    var t535 Option__string = Option__string{
        _tag: 1,
        _v1_0: t534,
    }
    return t535
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env418 closure_env_main_3, error__9 string) int {
    var inline600 int = _goml_runtime_core_string_len(error__9)
    return inline600
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env419 closure_env_main_4, value__11 int) int {
    var t541 int = value__11 + 2
    return t541
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env420 closure_env_main_5, value__13 string) int {
    var inline602 int = _goml_runtime_core_string_len(value__13)
    return inline602
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env421 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env422 closure_env_main_7, value__16 int) Result__string__string {
    var t549 string
    var inline604 string = _goml_runtime_core_int_to_string(value__16)
    t549 = inline604
    var t550 string = "next:" + t549
    var t551 Result__string__string = Result__string__string{
        _tag: 0,
        _v0_0: t550,
    }
    return t551
}

func main() {
    main0()
}
