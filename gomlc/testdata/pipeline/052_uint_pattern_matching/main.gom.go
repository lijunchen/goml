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

type Counter struct {
    start uint32
    end uint64
}

type Ordering int32

func is_flag8(value__0 uint8) bool {
    switch value__0 {
    case 0:
        return true
    case 200:
        return true
    default:
        return false
    }
}

func is_flag16(value__1 uint16) bool {
    switch value__1 {
    case 1024:
        return true
    case 65000:
        return true
    default:
        return false
    }
}

func is_flag32(value__2 uint32) bool {
    switch value__2 {
    case 4000000000:
        return true
    case 1234567890:
        return true
    default:
        return false
    }
}

func report(label__5 string, value__6 bool) string {
    var t438 string
    var inline478 string = _goml_runtime_core_bool_to_string(value__6)
    t438 = inline478
    var t439 string = label__5 + t438
    return t439
}

func main0() struct{} {
    var t441 bool = is_flag8(200)
    var t442 string = report("u8_hit=", t441)
    var t443 bool = is_flag8(15)
    var t444 string = report(",u8_miss=", t443)
    var t445 string = t442 + t444
    var t446 bool = is_flag16(65000)
    var t447 string = report(",u16_hit=", t446)
    var t448 string = t445 + t447
    var t449 bool = is_flag16(42)
    var t450 string = report(",u16_miss=", t449)
    var t451 string = t448 + t450
    var t452 bool = is_flag32(1234567890)
    var t453 string = report(",u32_hit=", t452)
    var t454 string = t451 + t453
    var t455 bool
    var inline513 uint32 = 99
    switch inline513 {
    case 4000000000:
        t455 = true
    case 1234567890:
        t455 = true
    default:
        t455 = false
    }
    var t456 string
    var inline509 string = ",u32_miss="
    var inline510 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t455)
    var inline511 string = inline509 + inline510
    t456 = inline511
    var t457 string = t454 + t456
    var t458 bool
    var inline507 uint64 = 900000000
    switch inline507 {
    case 900000000:
        t458 = true
    case 600000000:
        t458 = true
    default:
        t458 = false
    }
    var t459 string
    var inline503 string = ",u64_hit="
    var inline504 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t458)
    var inline505 string = inline503 + inline504
    t459 = inline505
    var t460 string = t457 + t459
    var t461 bool
    var inline501 uint64 = 700000000
    switch inline501 {
    case 900000000:
        t461 = true
    case 600000000:
        t461 = true
    default:
        t461 = false
    }
    var t462 string
    var inline497 string = ",u64_miss="
    var inline498 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t461)
    var inline499 string = inline497 + inline498
    t462 = inline499
    var t463 string = t460 + t462
    var t464 bool
    var inline494 uint32 = 4000000000
    var inline495 uint64 = 900000000
    switch inline495 {
    case 900000000:
        switch inline494 {
        case 4000000000:
            t464 = true
        default:
            t464 = false
        }
    case 600000000:
        t464 = true
    default:
        t464 = false
    }
    var t465 string
    var inline490 string = ",struct_first="
    var inline491 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t464)
    var inline492 string = inline490 + inline491
    t465 = inline492
    var t466 string = t463 + t465
    var t467 bool
    var inline487 uint32 = 12
    var inline488 uint64 = 600000000
    switch inline488 {
    case 900000000:
        switch inline487 {
        case 4000000000:
            t467 = true
        default:
            t467 = false
        }
    case 600000000:
        t467 = true
    default:
        t467 = false
    }
    var t468 string
    var inline483 string = ",struct_second="
    var inline484 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t467)
    var inline485 string = inline483 + inline484
    t468 = inline485
    var message__9 string = t466 + t468
    var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline480)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t471 string = _goml_runtime_core_bool_to_string(self__148)
    return t471
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
