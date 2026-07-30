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

func is_flag8(value__0 uint8) bool {
    var retv72 bool
    var jp74 bool
    switch value__0 {
    case 0:
        jp74 = true
    case 200:
        jp74 = true
    default:
        jp74 = false
    }
    retv72 = jp74
    return retv72
}

func is_flag16(value__1 uint16) bool {
    var retv76 bool
    var jp78 bool
    switch value__1 {
    case 1024:
        jp78 = true
    case 65000:
        jp78 = true
    default:
        jp78 = false
    }
    retv76 = jp78
    return retv76
}

func is_flag32(value__2 uint32) bool {
    var retv80 bool
    var jp82 bool
    switch value__2 {
    case 4000000000:
        jp82 = true
    case 1234567890:
        jp82 = true
    default:
        jp82 = false
    }
    retv80 = jp82
    return retv80
}

func is_flag64(value__3 uint64) bool {
    var retv84 bool
    var jp86 bool
    switch value__3 {
    case 900000000:
        jp86 = true
    case 600000000:
        jp86 = true
    default:
        jp86 = false
    }
    retv84 = jp86
    return retv84
}

func match_struct(counter__4 Counter) bool {
    var retv88 bool
    var x68 uint32 = counter__4.start
    var x69 uint64 = counter__4.end
    var jp90 bool
    switch x69 {
    case 900000000:
        var jp92 bool
        switch x68 {
        case 4000000000:
            jp92 = true
        default:
            jp92 = false
        }
        jp90 = jp92
    case 600000000:
        jp90 = true
    default:
        jp90 = false
    }
    retv88 = jp90
    return retv88
}

func report(label__5 string, value__6 bool) string {
    var retv94 string
    var t95 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t96 string = label__5 + t95
    retv94 = t96
    return retv94
}

func main0() struct{} {
    var counter__7 Counter = Counter{
        start: 4000000000,
        end: 900000000,
    }
    var alt_counter__8 Counter = Counter{
        start: 12,
        end: 600000000,
    }
    var t98 bool = is_flag8(200)
    var t99 string = report("u8_hit=", t98)
    var t100 bool = is_flag8(15)
    var t101 string = report(",u8_miss=", t100)
    var t102 string = t99 + t101
    var t103 bool = is_flag16(65000)
    var t104 string = report(",u16_hit=", t103)
    var t105 string = t102 + t104
    var t106 bool = is_flag16(42)
    var t107 string = report(",u16_miss=", t106)
    var t108 string = t105 + t107
    var t109 bool = is_flag32(1234567890)
    var t110 string = report(",u32_hit=", t109)
    var t111 string = t108 + t110
    var t112 bool = is_flag32(99)
    var t113 string = report(",u32_miss=", t112)
    var t114 string = t111 + t113
    var t115 bool = is_flag64(900000000)
    var t116 string = report(",u64_hit=", t115)
    var t117 string = t114 + t116
    var t118 bool = is_flag64(700000000)
    var t119 string = report(",u64_miss=", t118)
    var t120 string = t117 + t119
    var t121 bool = match_struct(counter__7)
    var t122 string = report(",struct_first=", t121)
    var t123 string = t120 + t122
    var t124 bool = match_struct(alt_counter__8)
    var t125 string = report(",struct_second=", t124)
    var message__9 string = t123 + t125
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv127 string
    var t128 string = _goml_runtime_core_bool_to_string(self__37)
    retv127 = t128
    return retv127
}

func println__T_string(value__1 string) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv133 string
    retv133 = self__38
    return retv133
}

func main() {
    main0()
}
