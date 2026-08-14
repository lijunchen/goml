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

type GenericChoice__NoTraits__NoTraits interface {
    isGenericChoice__NoTraits__NoTraits()
}

type Empty struct {}

func (_ Empty) isGenericChoice__NoTraits__NoTraits() {}

type Value struct {
    _0 Wrapper__NoTraits
}

func (_ Value) isGenericChoice__NoTraits__NoTraits() {}

func main0() struct{} {
    var t426 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t426,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t427 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t427)
    var t428 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t428)
    var t429 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t430 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t431 bool = t429 == t430
    var inline563 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t431)
    _goml_runtime_core_string_println(inline563)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t432 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline560 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline560)
    var t433 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline557 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline557)
    var t434 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t435 bool = !t434
    var inline554 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t435)
    _goml_runtime_core_string_println(inline554)
    var t436 uint64
    var inline545_source int = 0
    var inline545 uint64 = uint64(int(inline545_source))
    var inline546 uint64 = inline545 + 14695981039346656037
    var inline547 uint64 = inline546 + 2
    var inline548_source int = 0
    var inline548 uint64 = uint64(int(inline548_source))
    var inline549 uint64 = inline548 + 1099511628211
    var inline550 uint64 = inline547 * inline549
    var inline551 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline552 uint64 = inline550 + inline551
    t436 = inline552
    var t437 uint64
    var inline531_source int = 0
    var inline531 uint64 = uint64(int(inline531_source))
    var inline532 uint64 = inline531 + 14695981039346656037
    var inline533 uint64 = inline532 + 2
    var inline534_source int = 0
    var inline534 uint64 = uint64(int(inline534_source))
    var inline535 uint64 = inline534 + 1099511628211
    var inline536 uint64 = inline533 * inline535
    var inline537 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline538 uint64 = inline536 + inline537
    t437 = inline538
    var t438 bool = t436 == t437
    var inline523 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t438)
    _goml_runtime_core_string_println(inline523)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t441 string
    t441 = value__1
    _goml_runtime_core_string_println(t441)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t445 string = "Generic { " + "first: "
    var t446 string
    t446 = "wrapped"
    var t447 string = t445 + t446
    var t448 string = t447 + ", "
    var t449 string = t448 + "second: "
    var t450 string
    t450 = "wrapped"
    var t451 string = t449 + t450
    var t452 string = t451 + " }"
    return t452
}

func println__T_bool(value__1 bool) struct{} {
    var t454 string
    var inline569 string = _goml_runtime_core_bool_to_string(value__1)
    t454 = inline569
    _goml_runtime_core_string_println(t454)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp461 bool
    jp461 = true
    if jp461 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t470_source int = 0
    var t470 uint64 = uint64(int(t470_source))
    var h__10 uint64 = t470 + 14695981039346656037
    var t471_source int = 0
    var t471 uint64 = uint64(int(t471_source))
    var t472 uint64 = t471 + 1099511628211
    var t473 uint64 = h__10 * t472
    var t475 uint64
    t475 = 7
    var h__11 uint64 = t473 + t475
    var t476_source int = 0
    var t476 uint64 = uint64(int(t476_source))
    var t477 uint64 = t476 + 1099511628211
    var t478 uint64 = h__11 * t477
    var t480 uint64
    t480 = 7
    var h__12 uint64 = t478 + t480
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t485 string
        t485 = "wrapped"
        var t486 string = "GenericChoice::Value(" + t485
        var t487 string = t486 + ")"
        return t487
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
    switch other__16.(type) {
    case Empty:
        switch self__15.(type) {
        case Empty:
            return true
        default:
            return false
        }
    case Value:
        switch self__15.(type) {
        case Value:
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
    var t517 string = _goml_runtime_core_bool_to_string(self__148)
    return t517
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
