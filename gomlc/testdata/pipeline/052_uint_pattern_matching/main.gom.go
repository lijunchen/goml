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
    var retv156 bool
    var jp158 bool
    switch value__0 {
    case 0:
        jp158 = true
    case 200:
        jp158 = true
    default:
        jp158 = false
    }
    retv156 = jp158
    return retv156
}

func is_flag16(value__1 uint16) bool {
    var retv160 bool
    var jp162 bool
    switch value__1 {
    case 1024:
        jp162 = true
    case 65000:
        jp162 = true
    default:
        jp162 = false
    }
    retv160 = jp162
    return retv160
}

func is_flag32(value__2 uint32) bool {
    var retv164 bool
    var jp166 bool
    switch value__2 {
    case 4000000000:
        jp166 = true
    case 1234567890:
        jp166 = true
    default:
        jp166 = false
    }
    retv164 = jp166
    return retv164
}

func is_flag64(value__3 uint64) bool {
    var retv168 bool
    var jp170 bool
    switch value__3 {
    case 900000000:
        jp170 = true
    case 600000000:
        jp170 = true
    default:
        jp170 = false
    }
    retv168 = jp170
    return retv168
}

func match_struct(counter__4 Counter) bool {
    var retv172 bool
    var x152 uint32 = counter__4.start
    var x153 uint64 = counter__4.end
    var jp174 bool
    switch x153 {
    case 900000000:
        var jp176 bool
        switch x152 {
        case 4000000000:
            jp176 = true
        default:
            jp176 = false
        }
        jp174 = jp176
    case 600000000:
        jp174 = true
    default:
        jp174 = false
    }
    retv172 = jp174
    return retv172
}

func report(label__5 string, value__6 bool) string {
    var retv178 string
    var t179 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t180 string = label__5 + t179
    retv178 = t180
    return retv178
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
    var t182 bool = is_flag8(200)
    var t183 string = report("u8_hit=", t182)
    var t184 bool = is_flag8(15)
    var t185 string = report(",u8_miss=", t184)
    var t186 string = t183 + t185
    var t187 bool = is_flag16(65000)
    var t188 string = report(",u16_hit=", t187)
    var t189 string = t186 + t188
    var t190 bool = is_flag16(42)
    var t191 string = report(",u16_miss=", t190)
    var t192 string = t189 + t191
    var t193 bool = is_flag32(1234567890)
    var t194 string = report(",u32_hit=", t193)
    var t195 string = t192 + t194
    var t196 bool = is_flag32(99)
    var t197 string = report(",u32_miss=", t196)
    var t198 string = t195 + t197
    var t199 bool = is_flag64(900000000)
    var t200 string = report(",u64_hit=", t199)
    var t201 string = t198 + t200
    var t202 bool = is_flag64(700000000)
    var t203 string = report(",u64_miss=", t202)
    var t204 string = t201 + t203
    var t205 bool = match_struct(counter__7)
    var t206 string = report(",struct_first=", t205)
    var t207 string = t204 + t206
    var t208 bool = match_struct(alt_counter__8)
    var t209 string = report(",struct_second=", t208)
    var message__9 string = t207 + t209
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv211 string
    var t212 string = _goml_runtime_core_bool_to_string(self__37)
    retv211 = t212
    return retv211
}

func println__T_string(value__1 string) struct{} {
    var t214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv217 string
    retv217 = self__38
    return retv217
}

func main() {
    main0()
}
