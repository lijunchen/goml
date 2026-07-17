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
    var retv65 bool
    var jp67 bool
    switch value__0 {
    case 0:
        jp67 = true
    case 200:
        jp67 = true
    default:
        jp67 = false
    }
    retv65 = jp67
    return retv65
}

func is_flag16(value__1 uint16) bool {
    var retv69 bool
    var jp71 bool
    switch value__1 {
    case 1024:
        jp71 = true
    case 65000:
        jp71 = true
    default:
        jp71 = false
    }
    retv69 = jp71
    return retv69
}

func is_flag32(value__2 uint32) bool {
    var retv73 bool
    var jp75 bool
    switch value__2 {
    case 4000000000:
        jp75 = true
    case 1234567890:
        jp75 = true
    default:
        jp75 = false
    }
    retv73 = jp75
    return retv73
}

func is_flag64(value__3 uint64) bool {
    var retv77 bool
    var jp79 bool
    switch value__3 {
    case 900000000:
        jp79 = true
    case 600000000:
        jp79 = true
    default:
        jp79 = false
    }
    retv77 = jp79
    return retv77
}

func match_struct(counter__4 Counter) bool {
    var retv81 bool
    var x61 uint32 = counter__4.start
    var x62 uint64 = counter__4.end
    var jp83 bool
    switch x62 {
    case 900000000:
        var jp85 bool
        switch x61 {
        case 4000000000:
            jp85 = true
        default:
            jp85 = false
        }
        jp83 = jp85
    case 600000000:
        jp83 = true
    default:
        jp83 = false
    }
    retv81 = jp83
    return retv81
}

func report(label__5 string, value__6 bool) string {
    var retv87 string
    var t88 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t89 string = label__5 + t88
    retv87 = t89
    return retv87
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
    var t91 bool = is_flag8(200)
    var t92 string = report("u8_hit=", t91)
    var t93 bool = is_flag8(15)
    var t94 string = report(",u8_miss=", t93)
    var t95 string = t92 + t94
    var t96 bool = is_flag16(65000)
    var t97 string = report(",u16_hit=", t96)
    var t98 string = t95 + t97
    var t99 bool = is_flag16(42)
    var t100 string = report(",u16_miss=", t99)
    var t101 string = t98 + t100
    var t102 bool = is_flag32(1234567890)
    var t103 string = report(",u32_hit=", t102)
    var t104 string = t101 + t103
    var t105 bool = is_flag32(99)
    var t106 string = report(",u32_miss=", t105)
    var t107 string = t104 + t106
    var t108 bool = is_flag64(900000000)
    var t109 string = report(",u64_hit=", t108)
    var t110 string = t107 + t109
    var t111 bool = is_flag64(700000000)
    var t112 string = report(",u64_miss=", t111)
    var t113 string = t110 + t112
    var t114 bool = match_struct(counter__7)
    var t115 string = report(",struct_first=", t114)
    var t116 string = t113 + t115
    var t117 bool = match_struct(alt_counter__8)
    var t118 string = report(",struct_second=", t117)
    var message__9 string = t116 + t118
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv120 string
    var t121 string = _goml_runtime_core_bool_to_string(self__36)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv126 string
    retv126 = self__37
    return retv126
}

func main() {
    main0()
}
