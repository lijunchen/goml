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

type PairDefaults__int struct {
    first int
    second int
    nested Option__int
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

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Lazy__NoDefault struct {
    _tag int32
    _v1_0 NoDefault
}

type Selected__int struct {
    _tag int32
    _v0_0 int
    _v0_1 int
    _v1_0 *ref_NoDefault_x
}

func _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(self__0 Settings, other__1 Settings) bool {
    var jp440 bool
    var t444 bool = self__0.enabled
    var t445 bool = other__1.enabled
    var inline598 bool = t444 == t445
    jp440 = inline598
    var jp435 bool
    if jp440 {
        var t441 int = self__0.retries
        var t442 int = other__1.retries
        var inline600 bool = t441 == t442
        jp435 = inline600
    } else {
        jp435 = false
    }
    if jp435 {
        var t436 string = self__0.label
        var t437 string = other__1.label
        var inline602 bool = t436 == t437
        return inline602
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t449 bool
    t449 = false
    var t450 int
    t450 = 0
    var t451 string
    t451 = ""
    var t452 Settings = Settings{
        enabled: t449,
        retries: t450,
        label: t451,
    }
    return t452
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x409 bool = self__2.enabled
    var x410 int = self__2.retries
    var x411 string = self__2.label
    var t455 string = "Settings { " + "enabled: "
    var t456 string
    var inline611 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x409)
    t456 = inline611
    var t457 string = t455 + t456
    var t458 string = t457 + ", "
    var t459 string = t458 + "retries: "
    var t460 string
    var inline609 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x410)
    t460 = inline609
    var t461 string = t459 + t460
    var t462 string = t461 + ", "
    var t463 string = t462 + "label: "
    var t464 string
    var inline607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x411)
    t464 = inline607
    var t465 string = t463 + t464
    var t466 string = t465 + " }"
    return t466
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t469 Empty = Empty{}
    return t469
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x412 int = self__7._v1_0
        var t478 string
        var inline613 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x412)
        t478 = inline613
        var t479 string = "State::Running(" + t478
        var t480 string = t479 + ")"
        return t480
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9._tag {
    case 0:
        var x413 string = self__9._v0_0
        var x414 int = self__9._v0_1
        var t490 string
        var inline619 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x413)
        t490 = inline619
        var t491 string = "Message::Data(" + t490
        var t492 string = t491 + ", "
        var t493 string
        var inline617 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x414)
        t493 = inline617
        var t494 string = t492 + t493
        var t495 string = t494 + ")"
        return t495
    case 1:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t514 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t514)
    var t515 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t516 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t515)
    var t517 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t516)
    println__T_string(t517)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t518 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t518)
    var pair__17 PairDefaults__int = _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default()
    var t519 int = pair__17.first
    var t520 int = pair__17.second
    var t521 int = t519 + t520
    var t522 string = _goml_m_inherent_i_int_i_int_i_to__string(t521)
    println__T_string(t522)
    var t523 Option__int = pair__17.nested
    var t524 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(t523)
    var t525 string
    var inline671 string = _goml_runtime_core_bool_to_string(t524)
    t525 = inline671
    println__T_string(t525)
    var state__18 State
    state__18 = State{
        _tag: 0,
    }
    var t526 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline667 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t526)
    _goml_runtime_core_string_println(inline667)
    var message__19 Message
    var inline663 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline664 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline665 Message = Message{
        _tag: 0,
        _v0_0: inline663,
        _v0_1: inline664,
    }
    message__19 = inline665
    var t527 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline660 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t527)
    _goml_runtime_core_string_println(inline660)
    var event__20 Event
    var inline656 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline657 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline658 Event = Event{
        _tag: 0,
        _v0_0: inline656,
        _v0_1: inline657,
    }
    event__20 = inline658
    var t528 string
    switch event__20._tag {
    case 0:
        var inline643 string = event__20._v0_0
        var inline644 int = event__20._v0_1
        var inline647 string = "Event::Data { " + "name: "
        var inline648 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline643)
        var inline649 string = inline647 + inline648
        var inline650 string = inline649 + ", "
        var inline651 string = inline650 + "count: "
        var inline652 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline644)
        var inline653 string = inline651 + inline652
        var inline654 string = inline653 + " }"
        t528 = inline654
    case 1:
        t528 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline640 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t528)
    _goml_runtime_core_string_println(inline640)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault{
        _tag: 0,
    }
    var jp530 string
    switch lazy__21._tag {
    case 0:
        jp530 = "empty"
    case 1:
        jp530 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline636 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp530)
    _goml_runtime_core_string_println(inline636)
    var selected__22 Selected__int
    var inline632 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline633 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline634 Selected__int = Selected__int{
        _tag: 0,
        _v0_0: inline632,
        _v0_1: inline633,
    }
    selected__22 = inline634
    var jp532 string
    switch selected__22._tag {
    case 0:
        var x427 int = selected__22._v0_0
        var x428 int = selected__22._v0_1
        var t534 int = x427 + x428
        var inline627 string = _goml_runtime_core_int_to_string(t534)
        jp532 = inline627
    case 1:
        jp532 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline629 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp532)
    _goml_runtime_core_string_println(inline629)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_int_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__166 int) string {
    var inline675 string = _goml_runtime_core_int_to_string(self__166)
    return inline675
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__164 string) string {
    return self__164
}

func println__T_string(value__1 string) struct{} {
    var t561 string
    t561 = value__1
    _goml_runtime_core_string_println(t561)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t565 string = _goml_runtime_core_bool_to_string(self__148)
    return t565
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default() PairDefaults__int {
    var t568 int
    t568 = 0
    var t569 int
    t569 = 0
    var t570 Option__int
    t570 = Option__int{
        _tag: 0,
    }
    var t571 PairDefaults__int = PairDefaults__int{
        first: t568,
        second: t569,
        nested: t570,
    }
    return t571
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t574 string = _goml_runtime_core_int_to_string(self__32)
    return t574
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(self__457 Option__int) bool {
    var t577 bool
    switch self__457._tag {
    case 0:
        t577 = false
    case 1:
        t577 = true
    default:
        panic("non-exhaustive match")
    }
    var t578 bool = !t577
    return t578
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t588 string = _goml_runtime_core_int_to_string(self__151)
    return t588
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
