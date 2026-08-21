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

type Option__int struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var value__0 string = "a你好z"
    var t423 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline607 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t423)
    _goml_runtime_core_string_println(inline607)
    var t424 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline604 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t424)
    _goml_runtime_core_string_println(inline604)
    var t425 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline601 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t425)
    _goml_runtime_core_string_println(inline601)
    var t426 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline598 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t426)
    _goml_runtime_core_string_println(inline598)
    var t427 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline595 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t427)
    _goml_runtime_core_string_println(inline595)
    var t428 bool
    var inline583 string = ""
    var inline584 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline583)
    var inline585 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline586 bool = inline584 > inline585
    if inline586 {
        t428 = false
    } else {
        var inline587 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline588 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline583)
        var inline589 int = inline587 - inline588
        var inline590 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline589)
        if inline590 {
            var inline591 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline592 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline589, inline591)
            var inline593 bool = inline592 == inline583
            t428 = inline593
        } else {
            t428 = false
        }
    }
    var inline580 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t428)
    _goml_runtime_core_string_println(inline580)
    var t429 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline577 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t429)
    _goml_runtime_core_string_println(inline577)
    var t430 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline574 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t430)
    _goml_runtime_core_string_println(inline574)
    var t431 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline571 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t431)
    _goml_runtime_core_string_println(inline571)
    var t432 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline568 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t432)
    _goml_runtime_core_string_println(inline568)
    var t433 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline565 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t433)
    _goml_runtime_core_string_println(inline565)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__456 string, prefix__457 string) bool {
    var t447 int
    var inline623 int = _goml_runtime_core_string_len(prefix__457)
    t447 = inline623
    var t448 int
    var inline621 int = _goml_runtime_core_string_len(self__456)
    t448 = inline621
    var t449 bool = t447 <= t448
    var jp443 bool
    if t449 {
        var t450 int
        var inline614 int = _goml_runtime_core_string_len(prefix__457)
        t450 = inline614
        var inline612 bool = string_is_char_boundary(self__456, t450)
        jp443 = inline612
    } else {
        jp443 = false
    }
    if jp443 {
        var t444 int
        var inline619 int = _goml_runtime_core_string_len(prefix__457)
        t444 = inline619
        var t445 string
        var inline616 int = 0
        var inline617 string = string_byte_slice(self__456, inline616, t444)
        t445 = inline617
        var t446 bool = t445 == prefix__457
        return t446
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__458 string, suffix__459 string) bool {
    var t456 int
    var inline637 int = _goml_runtime_core_string_len(suffix__459)
    t456 = inline637
    var t457 int
    var inline635 int = _goml_runtime_core_string_len(self__458)
    t457 = inline635
    var t458 bool = t456 > t457
    if t458 {
        return false
    } else {
        var t459 int
        var inline633 int = _goml_runtime_core_string_len(self__458)
        t459 = inline633
        var t460 int
        var inline631 int = _goml_runtime_core_string_len(suffix__459)
        t460 = inline631
        var start__460 int = t459 - t460
        var t463 bool
        var inline629 bool = string_is_char_boundary(self__458, start__460)
        t463 = inline629
        if t463 {
            var t464 int
            var inline627 int = _goml_runtime_core_string_len(self__458)
            t464 = inline627
            var t465 string
            var inline625 string = string_byte_slice(self__458, start__460, t464)
            t465 = inline625
            var t466 bool = t465 == suffix__459
            return t466
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__461 string, expected__462 string) bool {
    var t471 int
    var inline665 int = _goml_runtime_core_string_len(expected__462)
    t471 = inline665
    var t472 bool = t471 == 0
    if t472 {
        return true
    } else {
        var t475 int
        var inline663 int = _goml_runtime_core_string_len(expected__462)
        t475 = inline663
        var t476 int
        var inline661 int = _goml_runtime_core_string_len(self__461)
        t476 = inline661
        var t477 bool = t475 > t476
        if t477 {
            return false
        } else {
            var t478 int
            var inline659 int = _goml_runtime_core_string_len(self__461)
            t478 = inline659
            var t479 int
            var inline657 int = _goml_runtime_core_string_len(expected__462)
            t479 = inline657
            var t480 int = t478 - t479
            var t481 int = t480 + 1
            var t482 FnIterator__int
            var inline651 int = 0
            var inline652 *ref_int_x = ref__Ref_3int(inline651)
            var inline653 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline652,
                end_1: t481,
            }
            var inline654 func() Option__int = func() Option__int {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline653)
            }
            var inline655 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline654)
            t482 = inline655
            var for_iter385 FnIterator__int
            for_iter385 = t482
            Loop_loop484:
            for {
                var for_next386 Option__int
                var inline647 func() Option__int = for_iter385.next_fn
                var inline648 Option__int = inline647()
                for_next386 = inline648
                switch for_next386._tag {
                case 0:
                    break Loop_loop484
                case 1:
                    var x387 int = for_next386._v1_0
                    var t486 int
                    var inline645 int = _goml_runtime_core_string_len(expected__462)
                    t486 = inline645
                    var end__464 int = x387 + t486
                    var t494 bool
                    var inline643 bool = string_is_char_boundary(self__461, x387)
                    t494 = inline643
                    var jp491 bool
                    if t494 {
                        var inline639 bool = string_is_char_boundary(self__461, end__464)
                        jp491 = inline639
                    } else {
                        jp491 = false
                    }
                    var jp489 bool
                    if jp491 {
                        var t492 string
                        var inline641 string = string_byte_slice(self__461, x387, end__464)
                        t492 = inline641
                        var t493 bool = t492 == expected__462
                        jp489 = t493
                    } else {
                        jp489 = false
                    }
                    if jp489 {
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
    var t498 string = _goml_runtime_core_bool_to_string(self__148)
    return t498
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t501 int = _goml_runtime_core_string_len(self__36)
    return t501
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t504 bool = string_is_char_boundary(self__44, index__45)
    return t504
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline667 bool = string_is_char_boundary(self__41, start__42)
    var inline669 bool
    if inline667 {
        var inline672 bool = string_is_char_boundary(self__41, end__43)
        inline669 = inline672
    } else {
        inline669 = false
    }
    if inline669 {
        var inline670 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline670
    } else {
        var inline671 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline671
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t532 bool = index__16 < 0
    var jp524 bool
    if t532 {
        jp524 = true
    } else {
        var t533 int
        var inline676 int = _goml_runtime_core_string_len(value__15)
        t533 = inline676
        var t534 bool = index__16 > t533
        jp524 = t534
    }
    if jp524 {
        return false
    } else {
        var t527 int
        var inline680 int = _goml_runtime_core_string_len(value__15)
        t527 = inline680
        var t528 bool = index__16 == t527
        if t528 {
            return true
        } else {
            var t529 uint8
            var inline678 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t529 = inline678
            var t530_rhs uint8 = 192
            var t530 uint8 = t529 & t530_rhs
            var t531 bool = t530 != 128
            return t531
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t543 bool = string_is_char_boundary(value__21, start__22)
    var jp540 bool
    if t543 {
        var t544 bool = string_is_char_boundary(value__21, end__23)
        jp540 = t544
    } else {
        jp540 = false
    }
    if jp540 {
        var t541 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t541
    } else {
        var t542 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t542
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t547 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t547
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env421 closure_env_goml_builtin_range_0) Option__int {
    var current__505 *ref_int_x = env421.current_0
    var end__504 int = env421.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t561 bool = value__506 < end__504
    if t561 {
        var t562 int = value__506 + 1
        ref_set__Ref_3int(current__505, t562)
        var t563 Option__int = Option__int{
            _tag: 1,
            _v1_0: value__506,
        }
        return t563
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
