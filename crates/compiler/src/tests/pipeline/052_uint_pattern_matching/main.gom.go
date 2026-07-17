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
    var retv62 bool
    var jp64 bool
    switch value__0 {
    case 0:
        jp64 = true
    case 200:
        jp64 = true
    default:
        jp64 = false
    }
    retv62 = jp64
    return retv62
}

func is_flag16(value__1 uint16) bool {
    var retv66 bool
    var jp68 bool
    switch value__1 {
    case 1024:
        jp68 = true
    case 65000:
        jp68 = true
    default:
        jp68 = false
    }
    retv66 = jp68
    return retv66
}

func is_flag32(value__2 uint32) bool {
    var retv70 bool
    var jp72 bool
    switch value__2 {
    case 4000000000:
        jp72 = true
    case 1234567890:
        jp72 = true
    default:
        jp72 = false
    }
    retv70 = jp72
    return retv70
}

func is_flag64(value__3 uint64) bool {
    var retv74 bool
    var jp76 bool
    switch value__3 {
    case 900000000:
        jp76 = true
    case 600000000:
        jp76 = true
    default:
        jp76 = false
    }
    retv74 = jp76
    return retv74
}

func match_struct(counter__4 Counter) bool {
    var retv78 bool
    var x58 uint32 = counter__4.start
    var x59 uint64 = counter__4.end
    var jp80 bool
    switch x59 {
    case 900000000:
        var jp82 bool
        switch x58 {
        case 4000000000:
            jp82 = true
        default:
            jp82 = false
        }
        jp80 = jp82
    case 600000000:
        jp80 = true
    default:
        jp80 = false
    }
    retv78 = jp80
    return retv78
}

func report(label__5 string, value__6 bool) string {
    var retv84 string
    var t85 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t86 string = label__5 + t85
    retv84 = t86
    return retv84
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
    var t88 bool = is_flag8(200)
    var t89 string = report("u8_hit=", t88)
    var t90 bool = is_flag8(15)
    var t91 string = report(",u8_miss=", t90)
    var t92 string = t89 + t91
    var t93 bool = is_flag16(65000)
    var t94 string = report(",u16_hit=", t93)
    var t95 string = t92 + t94
    var t96 bool = is_flag16(42)
    var t97 string = report(",u16_miss=", t96)
    var t98 string = t95 + t97
    var t99 bool = is_flag32(1234567890)
    var t100 string = report(",u32_hit=", t99)
    var t101 string = t98 + t100
    var t102 bool = is_flag32(99)
    var t103 string = report(",u32_miss=", t102)
    var t104 string = t101 + t103
    var t105 bool = is_flag64(900000000)
    var t106 string = report(",u64_hit=", t105)
    var t107 string = t104 + t106
    var t108 bool = is_flag64(700000000)
    var t109 string = report(",u64_miss=", t108)
    var t110 string = t107 + t109
    var t111 bool = match_struct(counter__7)
    var t112 string = report(",struct_first=", t111)
    var t113 string = t110 + t112
    var t114 bool = match_struct(alt_counter__8)
    var t115 string = report(",struct_second=", t114)
    var message__9 string = t113 + t115
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv117 string
    var t118 string = _goml_runtime_core_bool_to_string(self__33)
    retv117 = t118
    return retv117
}

func println__T_string(value__1 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv123 string
    retv123 = self__34
    return retv123
}

func main() {
    main0()
}
