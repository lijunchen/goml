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
    var t435 string
    var inline475 string = _goml_runtime_core_bool_to_string(value__6)
    t435 = inline475
    var t436 string = label__5 + t435
    return t436
}

func main0() struct{} {
    var t438 bool = is_flag8(200)
    var t439 string = report("u8_hit=", t438)
    var t440 bool = is_flag8(15)
    var t441 string = report(",u8_miss=", t440)
    var t442 string = t439 + t441
    var t443 bool = is_flag16(65000)
    var t444 string = report(",u16_hit=", t443)
    var t445 string = t442 + t444
    var t446 bool = is_flag16(42)
    var t447 string = report(",u16_miss=", t446)
    var t448 string = t445 + t447
    var t449 bool = is_flag32(1234567890)
    var t450 string = report(",u32_hit=", t449)
    var t451 string = t448 + t450
    var t452 bool
    var inline510 uint32 = 99
    switch inline510 {
    case 4000000000:
        t452 = true
    case 1234567890:
        t452 = true
    default:
        t452 = false
    }
    var t453 string
    var inline506 string = ",u32_miss="
    var inline507 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t452)
    var inline508 string = inline506 + inline507
    t453 = inline508
    var t454 string = t451 + t453
    var t455 bool
    var inline504 uint64 = 900000000
    switch inline504 {
    case 900000000:
        t455 = true
    case 600000000:
        t455 = true
    default:
        t455 = false
    }
    var t456 string
    var inline500 string = ",u64_hit="
    var inline501 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t455)
    var inline502 string = inline500 + inline501
    t456 = inline502
    var t457 string = t454 + t456
    var t458 bool
    var inline498 uint64 = 700000000
    switch inline498 {
    case 900000000:
        t458 = true
    case 600000000:
        t458 = true
    default:
        t458 = false
    }
    var t459 string
    var inline494 string = ",u64_miss="
    var inline495 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t458)
    var inline496 string = inline494 + inline495
    t459 = inline496
    var t460 string = t457 + t459
    var t461 bool
    var inline491 uint32 = 4000000000
    var inline492 uint64 = 900000000
    switch inline492 {
    case 900000000:
        switch inline491 {
        case 4000000000:
            t461 = true
        default:
            t461 = false
        }
    case 600000000:
        t461 = true
    default:
        t461 = false
    }
    var t462 string
    var inline487 string = ",struct_first="
    var inline488 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t461)
    var inline489 string = inline487 + inline488
    t462 = inline489
    var t463 string = t460 + t462
    var t464 bool
    var inline484 uint32 = 12
    var inline485 uint64 = 600000000
    switch inline485 {
    case 900000000:
        switch inline484 {
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
    var inline480 string = ",struct_second="
    var inline481 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t464)
    var inline482 string = inline480 + inline481
    t465 = inline482
    var message__9 string = t463 + t465
    var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline477)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t468 string = _goml_runtime_core_bool_to_string(self__148)
    return t468
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
