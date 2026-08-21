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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type NoTraits struct {
    value int
}

type Wrapper__NoTraits struct {
    value NoTraits
}

type Generic__NoTraits__NoTraits struct {
    first Wrapper__NoTraits
    second Wrapper__NoTraits
}

type Ordering int32

type GenericChoice__NoTraits__NoTraits struct {
    _tag int32
    _v1_0 Wrapper__NoTraits
}

func main0() struct{} {
    var t429 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t429,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t430 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t430)
    var t431 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t431)
    var t432 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t433 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t434 bool = t432 == t433
    var inline566 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t434)
    _goml_runtime_core_string_println(inline566)
    var empty__27 GenericChoice__NoTraits__NoTraits = GenericChoice__NoTraits__NoTraits{
        _tag: 0,
    }
    var value__28 GenericChoice__NoTraits__NoTraits = GenericChoice__NoTraits__NoTraits{
        _tag: 1,
        _v1_0: wrapped__24,
    }
    var t435 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline563 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline563)
    var t436 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline560 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline560)
    var t437 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t438 bool = !t437
    var inline557 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t438)
    _goml_runtime_core_string_println(inline557)
    var t439 uint64
    var inline548_source int = 0
    var inline548 uint64 = uint64(int(inline548_source))
    var inline549 uint64 = inline548 + 14695981039346656037
    var inline550 uint64 = inline549 + 2
    var inline551_source int = 0
    var inline551 uint64 = uint64(int(inline551_source))
    var inline552 uint64 = inline551 + 1099511628211
    var inline553 uint64 = inline550 * inline552
    var inline554 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline555 uint64 = inline553 + inline554
    t439 = inline555
    var t440 uint64
    var inline534_source int = 0
    var inline534 uint64 = uint64(int(inline534_source))
    var inline535 uint64 = inline534 + 14695981039346656037
    var inline536 uint64 = inline535 + 2
    var inline537_source int = 0
    var inline537 uint64 = uint64(int(inline537_source))
    var inline538 uint64 = inline537 + 1099511628211
    var inline539 uint64 = inline536 * inline538
    var inline540 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline541 uint64 = inline539 + inline540
    t440 = inline541
    var t441 bool = t439 == t440
    var inline526 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t441)
    _goml_runtime_core_string_println(inline526)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t444 string
    t444 = value__1
    _goml_runtime_core_string_println(t444)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t448 string = "Generic { " + "first: "
    var t449 string
    t449 = "wrapped"
    var t450 string = t448 + t449
    var t451 string = t450 + ", "
    var t452 string = t451 + "second: "
    var t453 string
    t453 = "wrapped"
    var t454 string = t452 + t453
    var t455 string = t454 + " }"
    return t455
}

func println__T_bool(value__1 bool) struct{} {
    var t457 string
    var inline572 string = _goml_runtime_core_bool_to_string(value__1)
    t457 = inline572
    _goml_runtime_core_string_println(t457)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp464 bool
    jp464 = true
    if jp464 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t473_source int = 0
    var t473 uint64 = uint64(int(t473_source))
    var h__10 uint64 = t473 + 14695981039346656037
    var t474_source int = 0
    var t474 uint64 = uint64(int(t474_source))
    var t475 uint64 = t474 + 1099511628211
    var t476 uint64 = h__10 * t475
    var t478 uint64
    t478 = 7
    var h__11 uint64 = t476 + t478
    var t479_source int = 0
    var t479 uint64 = uint64(int(t479_source))
    var t480 uint64 = t479 + 1099511628211
    var t481 uint64 = h__11 * t480
    var t483 uint64
    t483 = 7
    var h__12 uint64 = t481 + t483
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13._tag {
    case 0:
        return "GenericChoice::Empty"
    case 1:
        var t488 string
        t488 = "wrapped"
        var t489 string = "GenericChoice::Value(" + t488
        var t490 string = t489 + ")"
        return t490
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
    switch other__16._tag {
    case 0:
        switch self__15._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        switch self__15._tag {
        case 1:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t520 string = _goml_runtime_core_bool_to_string(self__148)
    return t520
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
