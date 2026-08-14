package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func main0() struct{} {
    var value__0 string = "a你好z"
    var t420 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline604 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t420)
    _goml_runtime_core_string_println(inline604)
    var t421 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline601 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t421)
    _goml_runtime_core_string_println(inline601)
    var t422 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline598 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t422)
    _goml_runtime_core_string_println(inline598)
    var t423 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline595 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t423)
    _goml_runtime_core_string_println(inline595)
    var t424 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline592 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t424)
    _goml_runtime_core_string_println(inline592)
    var t425 bool
    var inline580 string = ""
    var inline581 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline580)
    var inline582 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline583 bool = inline581 > inline582
    if inline583 {
        t425 = false
    } else {
        var inline584 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline585 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline580)
        var inline586 int = inline584 - inline585
        var inline587 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline586)
        if inline587 {
            var inline588 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline589 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline586, inline588)
            var inline590 bool = inline589 == inline580
            t425 = inline590
        } else {
            t425 = false
        }
    }
    var inline577 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t425)
    _goml_runtime_core_string_println(inline577)
    var t426 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline574 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t426)
    _goml_runtime_core_string_println(inline574)
    var t427 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline571 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t427)
    _goml_runtime_core_string_println(inline571)
    var t428 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline568 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t428)
    _goml_runtime_core_string_println(inline568)
    var t429 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline565 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t429)
    _goml_runtime_core_string_println(inline565)
    var t430 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline562 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t430)
    _goml_runtime_core_string_println(inline562)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__447 string, prefix__448 string) bool {
    var t444 int
    var inline620 int = _goml_runtime_core_string_len(prefix__448)
    t444 = inline620
    var t445 int
    var inline618 int = _goml_runtime_core_string_len(self__447)
    t445 = inline618
    var t446 bool = t444 <= t445
    var jp440 bool
    if t446 {
        var t447 int
        var inline611 int = _goml_runtime_core_string_len(prefix__448)
        t447 = inline611
        var inline609 bool = string_is_char_boundary(self__447, t447)
        jp440 = inline609
    } else {
        jp440 = false
    }
    if jp440 {
        var t441 int
        var inline616 int = _goml_runtime_core_string_len(prefix__448)
        t441 = inline616
        var t442 string
        var inline613 int = 0
        var inline614 string = string_byte_slice(self__447, inline613, t441)
        t442 = inline614
        var t443 bool = t442 == prefix__448
        return t443
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__449 string, suffix__450 string) bool {
    var t453 int
    var inline634 int = _goml_runtime_core_string_len(suffix__450)
    t453 = inline634
    var t454 int
    var inline632 int = _goml_runtime_core_string_len(self__449)
    t454 = inline632
    var t455 bool = t453 > t454
    if t455 {
        return false
    } else {
        var t456 int
        var inline630 int = _goml_runtime_core_string_len(self__449)
        t456 = inline630
        var t457 int
        var inline628 int = _goml_runtime_core_string_len(suffix__450)
        t457 = inline628
        var start__451 int = t456 - t457
        var t460 bool
        var inline626 bool = string_is_char_boundary(self__449, start__451)
        t460 = inline626
        if t460 {
            var t461 int
            var inline624 int = _goml_runtime_core_string_len(self__449)
            t461 = inline624
            var t462 string
            var inline622 string = string_byte_slice(self__449, start__451, t461)
            t462 = inline622
            var t463 bool = t462 == suffix__450
            return t463
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__452 string, expected__453 string) bool {
    var t468 int
    var inline662 int = _goml_runtime_core_string_len(expected__453)
    t468 = inline662
    var t469 bool = t468 == 0
    if t469 {
        return true
    } else {
        var t472 int
        var inline660 int = _goml_runtime_core_string_len(expected__453)
        t472 = inline660
        var t473 int
        var inline658 int = _goml_runtime_core_string_len(self__452)
        t473 = inline658
        var t474 bool = t472 > t473
        if t474 {
            return false
        } else {
            var t475 int
            var inline656 int = _goml_runtime_core_string_len(self__452)
            t475 = inline656
            var t476 int
            var inline654 int = _goml_runtime_core_string_len(expected__453)
            t476 = inline654
            var t477 int = t475 - t476
            var t478 int = t477 + 1
            var t479 FnIterator__int
            var inline648 int = 0
            var inline649 *ref_int_x = ref__Ref_3int(inline648)
            var inline650 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline649,
                end_1: t478,
            }
            var inline651 func() Option__int = func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline650)
            }
            var inline652 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline651)
            t479 = inline652
            var for_iter382 FnIterator__int
            for_iter382 = t479
            Loop_loop481:
            for {
                var for_next383 Option__int
                var inline644 func() Option__int = for_iter382.next_fn
                var inline645 Option__int = inline644()
                for_next383 = inline645
                switch for_next383.(type) {
                case None:
                    break Loop_loop481
                case Some:
                    var x384 int = for_next383.(Some)._0
                    var t483 int
                    var inline642 int = _goml_runtime_core_string_len(expected__453)
                    t483 = inline642
                    var end__455 int = x384 + t483
                    var t491 bool
                    var inline640 bool = string_is_char_boundary(self__452, x384)
                    t491 = inline640
                    var jp488 bool
                    if t491 {
                        var inline636 bool = string_is_char_boundary(self__452, end__455)
                        jp488 = inline636
                    } else {
                        jp488 = false
                    }
                    var jp486 bool
                    if jp488 {
                        var t489 string
                        var inline638 string = string_byte_slice(self__452, x384, end__455)
                        t489 = inline638
                        var t490 bool = t489 == expected__453
                        jp486 = t490
                    } else {
                        jp486 = false
                    }
                    if jp486 {
                        return true
                    } else {
                        continue
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
            return false
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t495 string = _goml_runtime_core_bool_to_string(self__148)
    return t495
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t498 int = _goml_runtime_core_string_len(self__36)
    return t498
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t501 bool = string_is_char_boundary(self__44, index__45)
    return t501
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline664 bool = string_is_char_boundary(self__41, start__42)
    var inline666 bool
    if inline664 {
        var inline669 bool = string_is_char_boundary(self__41, end__43)
        inline666 = inline669
    } else {
        inline666 = false
    }
    if inline666 {
        var inline667 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline667
    } else {
        var inline668 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline668
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t529 bool = index__16 < 0
    var jp521 bool
    if t529 {
        jp521 = true
    } else {
        var t530 int
        var inline673 int = _goml_runtime_core_string_len(value__15)
        t530 = inline673
        var t531 bool = index__16 > t530
        jp521 = t531
    }
    if jp521 {
        return false
    } else {
        var t524 int
        var inline677 int = _goml_runtime_core_string_len(value__15)
        t524 = inline677
        var t525 bool = index__16 == t524
        if t525 {
            return true
        } else {
            var t526 uint8
            var inline675 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t526 = inline675
            var t527_rhs uint8 = 192
            var t527 uint8 = t526 & t527_rhs
            var t528 bool = t527 != 128
            return t528
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t540 bool = string_is_char_boundary(value__21, start__22)
    var jp537 bool
    if t540 {
        var t541 bool = string_is_char_boundary(value__21, end__23)
        jp537 = t541
    } else {
        jp537 = false
    }
    if jp537 {
        var t538 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t538
    } else {
        var t539 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t539
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t544 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t544
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env418 closure_env_goml_builtin_range_0) Option__int {
    var current__496 *ref_int_x = env418.current_0
    var end__495 int = env418.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t558 bool = value__497 < end__495
    if t558 {
        var t559 int = value__497 + 1
        ref_set__Ref_3int(current__496, t559)
        var t560 Option__int = Some{
            _0: value__497,
        }
        return t560
    } else {
        return None{}
    }
}

func main() {
    main0()
}
