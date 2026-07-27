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
    var retv68 bool
    var jp70 bool
    switch value__0 {
    case 0:
        jp70 = true
    case 200:
        jp70 = true
    default:
        jp70 = false
    }
    retv68 = jp70
    return retv68
}

func is_flag16(value__1 uint16) bool {
    var retv72 bool
    var jp74 bool
    switch value__1 {
    case 1024:
        jp74 = true
    case 65000:
        jp74 = true
    default:
        jp74 = false
    }
    retv72 = jp74
    return retv72
}

func is_flag32(value__2 uint32) bool {
    var retv76 bool
    var jp78 bool
    switch value__2 {
    case 4000000000:
        jp78 = true
    case 1234567890:
        jp78 = true
    default:
        jp78 = false
    }
    retv76 = jp78
    return retv76
}

func is_flag64(value__3 uint64) bool {
    var retv80 bool
    var jp82 bool
    switch value__3 {
    case 900000000:
        jp82 = true
    case 600000000:
        jp82 = true
    default:
        jp82 = false
    }
    retv80 = jp82
    return retv80
}

func match_struct(counter__4 Counter) bool {
    var retv84 bool
    var x64 uint32 = counter__4.start
    var x65 uint64 = counter__4.end
    var jp86 bool
    switch x65 {
    case 900000000:
        var jp88 bool
        switch x64 {
        case 4000000000:
            jp88 = true
        default:
            jp88 = false
        }
        jp86 = jp88
    case 600000000:
        jp86 = true
    default:
        jp86 = false
    }
    retv84 = jp86
    return retv84
}

func report(label__5 string, value__6 bool) string {
    var retv90 string
    var t91 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t92 string = label__5 + t91
    retv90 = t92
    return retv90
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
    var t94 bool = is_flag8(200)
    var t95 string = report("u8_hit=", t94)
    var t96 bool = is_flag8(15)
    var t97 string = report(",u8_miss=", t96)
    var t98 string = t95 + t97
    var t99 bool = is_flag16(65000)
    var t100 string = report(",u16_hit=", t99)
    var t101 string = t98 + t100
    var t102 bool = is_flag16(42)
    var t103 string = report(",u16_miss=", t102)
    var t104 string = t101 + t103
    var t105 bool = is_flag32(1234567890)
    var t106 string = report(",u32_hit=", t105)
    var t107 string = t104 + t106
    var t108 bool = is_flag32(99)
    var t109 string = report(",u32_miss=", t108)
    var t110 string = t107 + t109
    var t111 bool = is_flag64(900000000)
    var t112 string = report(",u64_hit=", t111)
    var t113 string = t110 + t112
    var t114 bool = is_flag64(700000000)
    var t115 string = report(",u64_miss=", t114)
    var t116 string = t113 + t115
    var t117 bool = match_struct(counter__7)
    var t118 string = report(",struct_first=", t117)
    var t119 string = t116 + t118
    var t120 bool = match_struct(alt_counter__8)
    var t121 string = report(",struct_second=", t120)
    var message__9 string = t119 + t121
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv123 string
    var t124 string = _goml_runtime_core_bool_to_string(self__37)
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func main() {
    main0()
}
