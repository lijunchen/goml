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
    var t199 string
    var inline239 string = _goml_runtime_core_bool_to_string(value__6)
    t199 = inline239
    var t200 string = label__5 + t199
    return t200
}

func main0() struct{} {
    var t202 bool = is_flag8(200)
    var t203 string = report("u8_hit=", t202)
    var t204 bool = is_flag8(15)
    var t205 string = report(",u8_miss=", t204)
    var t206 string = t203 + t205
    var t207 bool = is_flag16(65000)
    var t208 string = report(",u16_hit=", t207)
    var t209 string = t206 + t208
    var t210 bool = is_flag16(42)
    var t211 string = report(",u16_miss=", t210)
    var t212 string = t209 + t211
    var t213 bool = is_flag32(1234567890)
    var t214 string = report(",u32_hit=", t213)
    var t215 string = t212 + t214
    var t216 bool
    var inline274 uint32 = 99
    switch inline274 {
    case 4000000000:
        t216 = true
    case 1234567890:
        t216 = true
    default:
        t216 = false
    }
    var t217 string
    var inline270 string = ",u32_miss="
    var inline271 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t216)
    var inline272 string = inline270 + inline271
    t217 = inline272
    var t218 string = t215 + t217
    var t219 bool
    var inline268 uint64 = 900000000
    switch inline268 {
    case 900000000:
        t219 = true
    case 600000000:
        t219 = true
    default:
        t219 = false
    }
    var t220 string
    var inline264 string = ",u64_hit="
    var inline265 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t219)
    var inline266 string = inline264 + inline265
    t220 = inline266
    var t221 string = t218 + t220
    var t222 bool
    var inline262 uint64 = 700000000
    switch inline262 {
    case 900000000:
        t222 = true
    case 600000000:
        t222 = true
    default:
        t222 = false
    }
    var t223 string
    var inline258 string = ",u64_miss="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t222)
    var inline260 string = inline258 + inline259
    t223 = inline260
    var t224 string = t221 + t223
    var t225 bool
    var inline255 uint32 = 4000000000
    var inline256 uint64 = 900000000
    switch inline256 {
    case 900000000:
        switch inline255 {
        case 4000000000:
            t225 = true
        default:
            t225 = false
        }
    case 600000000:
        t225 = true
    default:
        t225 = false
    }
    var t226 string
    var inline251 string = ",struct_first="
    var inline252 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t225)
    var inline253 string = inline251 + inline252
    t226 = inline253
    var t227 string = t224 + t226
    var t228 bool
    var inline248 uint32 = 12
    var inline249 uint64 = 600000000
    switch inline249 {
    case 900000000:
        switch inline248 {
        case 4000000000:
            t228 = true
        default:
            t228 = false
        }
    case 600000000:
        t228 = true
    default:
        t228 = false
    }
    var t229 string
    var inline244 string = ",struct_second="
    var inline245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t228)
    var inline246 string = inline244 + inline245
    t229 = inline246
    var message__9 string = t227 + t229
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline241)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t232 string = _goml_runtime_core_bool_to_string(self__66)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
