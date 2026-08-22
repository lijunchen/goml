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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_NoDefault_x struct {
    value NoDefault
}

type Settings struct {
    enabled bool
    retries int
    label string
}

type NoDefault struct {
    value int
}

type Empty struct {}

type PairDefaults__isize struct {
    first int
    second int
    nested Option__isize
}

type Ordering int32

type State struct {
    _tag int32
    _v1_0 int
}

type Message struct {
    _tag int32
    _v0_0 string
    _v0_1 int
}

type Event struct {
    _tag int32
    _v0_0 string
    _v0_1 int
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Lazy__NoDefault struct {
    _tag int32
    _v1_0 NoDefault
}

type Selected__isize struct {
    _tag int32
    _v0_0 int
    _v0_1 int
    _v1_0 *ref_NoDefault_x
}

func _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(self__0 Settings, other__1 Settings) bool {
    var jp443 bool
    var t447 bool = self__0.enabled
    var t448 bool = other__1.enabled
    var inline601 bool = t447 == t448
    jp443 = inline601
    var jp438 bool
    if jp443 {
        var t444 int = self__0.retries
        var t445 int = other__1.retries
        var inline603 bool = t444 == t445
        jp438 = inline603
    } else {
        jp438 = false
    }
    if jp438 {
        var t439 string = self__0.label
        var t440 string = other__1.label
        var inline605 bool = t439 == t440
        return inline605
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t452 bool
    t452 = false
    var t453 int
    t453 = 0
    var t454 string
    t454 = ""
    var t455 Settings = Settings{
        enabled: t452,
        retries: t453,
        label: t454,
    }
    return t455
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x412 bool = self__2.enabled
    var x413 int = self__2.retries
    var x414 string = self__2.label
    var t458 string = "Settings { " + "enabled: "
    var t459 string
    var inline614 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x412)
    t459 = inline614
    var t460 string = t458 + t459
    var t461 string = t460 + ", "
    var t462 string = t461 + "retries: "
    var t463 string
    var inline612 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x413)
    t463 = inline612
    var t464 string = t462 + t463
    var t465 string = t464 + ", "
    var t466 string = t465 + "label: "
    var t467 string
    var inline610 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x414)
    t467 = inline610
    var t468 string = t466 + t467
    var t469 string = t468 + " }"
    return t469
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t472 Empty = Empty{}
    return t472
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x415 int = self__7._v1_0
        var t481 string
        var inline616 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x415)
        t481 = inline616
        var t482 string = "State::Running(" + t481
        var t483 string = t482 + ")"
        return t483
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9._tag {
    case 0:
        var x416 string = self__9._v0_0
        var x417 int = self__9._v0_1
        var t493 string
        var inline622 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x416)
        t493 = inline622
        var t494 string = "Message::Data(" + t493
        var t495 string = t494 + ", "
        var t496 string
        var inline620 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x417)
        t496 = inline620
        var t497 string = t495 + t496
        var t498 string = t497 + ")"
        return t498
    case 1:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t517 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t517)
    var t518 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t519 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t518)
    var t520 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t519)
    println__T_string(t520)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t521 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t521)
    var pair__17 PairDefaults__isize = _goml_m_trait__impl_i_Default_i_PairDefaults____isize_i_default()
    var t522 int = pair__17.first
    var t523 int = pair__17.second
    var t524 int = t522 + t523
    var t525 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t524)
    println__T_string(t525)
    var t526 Option__isize = pair__17.nested
    var t527 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__isize(t526)
    var t528 string
    var inline674 string = _goml_runtime_core_bool_to_string(t527)
    t528 = inline674
    println__T_string(t528)
    var state__18 State
    state__18 = State{
        _tag: 0,
    }
    var t529 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline670 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t529)
    _goml_runtime_core_string_println(inline670)
    var message__19 Message
    var inline666 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline667 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline668 Message = Message{
        _tag: 0,
        _v0_0: inline666,
        _v0_1: inline667,
    }
    message__19 = inline668
    var t530 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline663 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t530)
    _goml_runtime_core_string_println(inline663)
    var event__20 Event
    var inline659 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline660 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline661 Event = Event{
        _tag: 0,
        _v0_0: inline659,
        _v0_1: inline660,
    }
    event__20 = inline661
    var t531 string
    switch event__20._tag {
    case 0:
        var inline646 string = event__20._v0_0
        var inline647 int = event__20._v0_1
        var inline650 string = "Event::Data { " + "name: "
        var inline651 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline646)
        var inline652 string = inline650 + inline651
        var inline653 string = inline652 + ", "
        var inline654 string = inline653 + "count: "
        var inline655 string = _goml_m_trait__impl_i_Debug_i_isize_i_debug(inline647)
        var inline656 string = inline654 + inline655
        var inline657 string = inline656 + " }"
        t531 = inline657
    case 1:
        t531 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline643 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t531)
    _goml_runtime_core_string_println(inline643)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault{
        _tag: 0,
    }
    var jp533 string
    switch lazy__21._tag {
    case 0:
        jp533 = "empty"
    case 1:
        jp533 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline639 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp533)
    _goml_runtime_core_string_println(inline639)
    var selected__22 Selected__isize
    var inline635 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline636 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline637 Selected__isize = Selected__isize{
        _tag: 0,
        _v0_0: inline635,
        _v0_1: inline636,
    }
    selected__22 = inline637
    var jp535 string
    switch selected__22._tag {
    case 0:
        var x430 int = selected__22._v0_0
        var x431 int = selected__22._v0_1
        var t537 int = x430 + x431
        var inline630 string = _goml_runtime_core_int_to_string(t537)
        jp535 = inline630
    case 1:
        jp535 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline632 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp535)
    _goml_runtime_core_string_println(inline632)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_isize_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_isize_i_debug(self__166 int) string {
    var inline678 string = _goml_runtime_core_int_to_string(self__166)
    return inline678
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__164 string) string {
    return self__164
}

func println__T_string(value__1 string) struct{} {
    var t564 string
    t564 = value__1
    _goml_runtime_core_string_println(t564)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t568 string = _goml_runtime_core_bool_to_string(self__148)
    return t568
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____isize_i_default() PairDefaults__isize {
    var t571 int
    t571 = 0
    var t572 int
    t572 = 0
    var t573 Option__isize
    t573 = Option__isize{
        _tag: 0,
    }
    var t574 PairDefaults__isize = PairDefaults__isize{
        first: t571,
        second: t572,
        nested: t573,
    }
    return t574
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t577 string = _goml_runtime_core_int_to_string(self__32)
    return t577
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__isize(self__466 Option__isize) bool {
    var t580 bool
    switch self__466._tag {
    case 0:
        t580 = false
    case 1:
        t580 = true
    default:
        panic("non-exhaustive match")
    }
    var t581 bool = !t580
    return t581
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t591 string = _goml_runtime_core_int_to_string(self__151)
    return t591
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
