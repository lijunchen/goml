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
    var t204 string
    var inline244 string = _goml_runtime_core_bool_to_string(value__6)
    t204 = inline244
    var t205 string = label__5 + t204
    return t205
}

func main0() struct{} {
    var t207 bool = is_flag8(200)
    var t208 string = report("u8_hit=", t207)
    var t209 bool = is_flag8(15)
    var t210 string = report(",u8_miss=", t209)
    var t211 string = t208 + t210
    var t212 bool = is_flag16(65000)
    var t213 string = report(",u16_hit=", t212)
    var t214 string = t211 + t213
    var t215 bool = is_flag16(42)
    var t216 string = report(",u16_miss=", t215)
    var t217 string = t214 + t216
    var t218 bool = is_flag32(1234567890)
    var t219 string = report(",u32_hit=", t218)
    var t220 string = t217 + t219
    var t221 bool
    var inline279 uint32 = 99
    switch inline279 {
    case 4000000000:
        t221 = true
    case 1234567890:
        t221 = true
    default:
        t221 = false
    }
    var t222 string
    var inline275 string = ",u32_miss="
    var inline276 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t221)
    var inline277 string = inline275 + inline276
    t222 = inline277
    var t223 string = t220 + t222
    var t224 bool
    var inline273 uint64 = 900000000
    switch inline273 {
    case 900000000:
        t224 = true
    case 600000000:
        t224 = true
    default:
        t224 = false
    }
    var t225 string
    var inline269 string = ",u64_hit="
    var inline270 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t224)
    var inline271 string = inline269 + inline270
    t225 = inline271
    var t226 string = t223 + t225
    var t227 bool
    var inline267 uint64 = 700000000
    switch inline267 {
    case 900000000:
        t227 = true
    case 600000000:
        t227 = true
    default:
        t227 = false
    }
    var t228 string
    var inline263 string = ",u64_miss="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t227)
    var inline265 string = inline263 + inline264
    t228 = inline265
    var t229 string = t226 + t228
    var t230 bool
    var inline260 uint32 = 4000000000
    var inline261 uint64 = 900000000
    switch inline261 {
    case 900000000:
        switch inline260 {
        case 4000000000:
            t230 = true
        default:
            t230 = false
        }
    case 600000000:
        t230 = true
    default:
        t230 = false
    }
    var t231 string
    var inline256 string = ",struct_first="
    var inline257 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t230)
    var inline258 string = inline256 + inline257
    t231 = inline258
    var t232 string = t229 + t231
    var t233 bool
    var inline253 uint32 = 12
    var inline254 uint64 = 600000000
    switch inline254 {
    case 900000000:
        switch inline253 {
        case 4000000000:
            t233 = true
        default:
            t233 = false
        }
    case 600000000:
        t233 = true
    default:
        t233 = false
    }
    var t234 string
    var inline249 string = ",struct_second="
    var inline250 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t233)
    var inline251 string = inline249 + inline250
    t234 = inline251
    var message__9 string = t232 + t234
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t237 string = _goml_runtime_core_bool_to_string(self__66)
    return t237
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
