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
    var t163 string
    var inline203 string = _goml_runtime_core_bool_to_string(value__6)
    t163 = inline203
    var t164 string = label__5 + t163
    return t164
}

func main0() struct{} {
    var t166 bool = is_flag8(200)
    var t167 string = report("u8_hit=", t166)
    var t168 bool = is_flag8(15)
    var t169 string = report(",u8_miss=", t168)
    var t170 string = t167 + t169
    var t171 bool = is_flag16(65000)
    var t172 string = report(",u16_hit=", t171)
    var t173 string = t170 + t172
    var t174 bool = is_flag16(42)
    var t175 string = report(",u16_miss=", t174)
    var t176 string = t173 + t175
    var t177 bool = is_flag32(1234567890)
    var t178 string = report(",u32_hit=", t177)
    var t179 string = t176 + t178
    var t180 bool
    var inline238 uint32 = 99
    switch inline238 {
    case 4000000000:
        t180 = true
    case 1234567890:
        t180 = true
    default:
        t180 = false
    }
    var t181 string
    var inline234 string = ",u32_miss="
    var inline235 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t180)
    var inline236 string = inline234 + inline235
    t181 = inline236
    var t182 string = t179 + t181
    var t183 bool
    var inline232 uint64 = 900000000
    switch inline232 {
    case 900000000:
        t183 = true
    case 600000000:
        t183 = true
    default:
        t183 = false
    }
    var t184 string
    var inline228 string = ",u64_hit="
    var inline229 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t183)
    var inline230 string = inline228 + inline229
    t184 = inline230
    var t185 string = t182 + t184
    var t186 bool
    var inline226 uint64 = 700000000
    switch inline226 {
    case 900000000:
        t186 = true
    case 600000000:
        t186 = true
    default:
        t186 = false
    }
    var t187 string
    var inline222 string = ",u64_miss="
    var inline223 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t186)
    var inline224 string = inline222 + inline223
    t187 = inline224
    var t188 string = t185 + t187
    var t189 bool
    var inline219 uint32 = 4000000000
    var inline220 uint64 = 900000000
    switch inline220 {
    case 900000000:
        switch inline219 {
        case 4000000000:
            t189 = true
        default:
            t189 = false
        }
    case 600000000:
        t189 = true
    default:
        t189 = false
    }
    var t190 string
    var inline215 string = ",struct_first="
    var inline216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    var inline217 string = inline215 + inline216
    t190 = inline217
    var t191 string = t188 + t190
    var t192 bool
    var inline212 uint32 = 12
    var inline213 uint64 = 600000000
    switch inline213 {
    case 900000000:
        switch inline212 {
        case 4000000000:
            t192 = true
        default:
            t192 = false
        }
    case 600000000:
        t192 = true
    default:
        t192 = false
    }
    var t193 string
    var inline208 string = ",struct_second="
    var inline209 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    var inline210 string = inline208 + inline209
    t193 = inline210
    var message__9 string = t191 + t193
    var inline205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t196 string = _goml_runtime_core_bool_to_string(self__66)
    return t196
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
