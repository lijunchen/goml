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
    var retv112 bool
    var jp114 bool
    switch value__0 {
    case 0:
        jp114 = true
    case 200:
        jp114 = true
    default:
        jp114 = false
    }
    retv112 = jp114
    return retv112
}

func is_flag16(value__1 uint16) bool {
    var retv116 bool
    var jp118 bool
    switch value__1 {
    case 1024:
        jp118 = true
    case 65000:
        jp118 = true
    default:
        jp118 = false
    }
    retv116 = jp118
    return retv116
}

func is_flag32(value__2 uint32) bool {
    var retv120 bool
    var jp122 bool
    switch value__2 {
    case 4000000000:
        jp122 = true
    case 1234567890:
        jp122 = true
    default:
        jp122 = false
    }
    retv120 = jp122
    return retv120
}

func is_flag64(value__3 uint64) bool {
    var retv124 bool
    var jp126 bool
    switch value__3 {
    case 900000000:
        jp126 = true
    case 600000000:
        jp126 = true
    default:
        jp126 = false
    }
    retv124 = jp126
    return retv124
}

func match_struct(counter__4 Counter) bool {
    var retv128 bool
    var x108 uint32 = counter__4.start
    var x109 uint64 = counter__4.end
    var jp130 bool
    switch x109 {
    case 900000000:
        var jp132 bool
        switch x108 {
        case 4000000000:
            jp132 = true
        default:
            jp132 = false
        }
        jp130 = jp132
    case 600000000:
        jp130 = true
    default:
        jp130 = false
    }
    retv128 = jp130
    return retv128
}

func report(label__5 string, value__6 bool) string {
    var retv134 string
    var t135 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t136 string = label__5 + t135
    retv134 = t136
    return retv134
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
    var t138 bool = is_flag8(200)
    var t139 string = report("u8_hit=", t138)
    var t140 bool = is_flag8(15)
    var t141 string = report(",u8_miss=", t140)
    var t142 string = t139 + t141
    var t143 bool = is_flag16(65000)
    var t144 string = report(",u16_hit=", t143)
    var t145 string = t142 + t144
    var t146 bool = is_flag16(42)
    var t147 string = report(",u16_miss=", t146)
    var t148 string = t145 + t147
    var t149 bool = is_flag32(1234567890)
    var t150 string = report(",u32_hit=", t149)
    var t151 string = t148 + t150
    var t152 bool = is_flag32(99)
    var t153 string = report(",u32_miss=", t152)
    var t154 string = t151 + t153
    var t155 bool = is_flag64(900000000)
    var t156 string = report(",u64_hit=", t155)
    var t157 string = t154 + t156
    var t158 bool = is_flag64(700000000)
    var t159 string = report(",u64_miss=", t158)
    var t160 string = t157 + t159
    var t161 bool = match_struct(counter__7)
    var t162 string = report(",struct_first=", t161)
    var t163 string = t160 + t162
    var t164 bool = match_struct(alt_counter__8)
    var t165 string = report(",struct_second=", t164)
    var message__9 string = t163 + t165
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv167 string
    var t168 string = _goml_runtime_core_bool_to_string(self__37)
    retv167 = t168
    return retv167
}

func println__T_string(value__1 string) struct{} {
    var t170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv173 string
    retv173 = self__38
    return retv173
}

func main() {
    main0()
}
