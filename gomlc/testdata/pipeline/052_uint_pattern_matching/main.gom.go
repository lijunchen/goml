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
    var t214 string
    var inline254 string = _goml_runtime_core_bool_to_string(value__6)
    t214 = inline254
    var t215 string = label__5 + t214
    return t215
}

func main0() struct{} {
    var t217 bool = is_flag8(200)
    var t218 string = report("u8_hit=", t217)
    var t219 bool = is_flag8(15)
    var t220 string = report(",u8_miss=", t219)
    var t221 string = t218 + t220
    var t222 bool = is_flag16(65000)
    var t223 string = report(",u16_hit=", t222)
    var t224 string = t221 + t223
    var t225 bool = is_flag16(42)
    var t226 string = report(",u16_miss=", t225)
    var t227 string = t224 + t226
    var t228 bool = is_flag32(1234567890)
    var t229 string = report(",u32_hit=", t228)
    var t230 string = t227 + t229
    var t231 bool
    var inline289 uint32 = 99
    switch inline289 {
    case 4000000000:
        t231 = true
    case 1234567890:
        t231 = true
    default:
        t231 = false
    }
    var t232 string
    var inline285 string = ",u32_miss="
    var inline286 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t231)
    var inline287 string = inline285 + inline286
    t232 = inline287
    var t233 string = t230 + t232
    var t234 bool
    var inline283 uint64 = 900000000
    switch inline283 {
    case 900000000:
        t234 = true
    case 600000000:
        t234 = true
    default:
        t234 = false
    }
    var t235 string
    var inline279 string = ",u64_hit="
    var inline280 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t234)
    var inline281 string = inline279 + inline280
    t235 = inline281
    var t236 string = t233 + t235
    var t237 bool
    var inline277 uint64 = 700000000
    switch inline277 {
    case 900000000:
        t237 = true
    case 600000000:
        t237 = true
    default:
        t237 = false
    }
    var t238 string
    var inline273 string = ",u64_miss="
    var inline274 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t237)
    var inline275 string = inline273 + inline274
    t238 = inline275
    var t239 string = t236 + t238
    var t240 bool
    var inline270 uint32 = 4000000000
    var inline271 uint64 = 900000000
    switch inline271 {
    case 900000000:
        switch inline270 {
        case 4000000000:
            t240 = true
        default:
            t240 = false
        }
    case 600000000:
        t240 = true
    default:
        t240 = false
    }
    var t241 string
    var inline266 string = ",struct_first="
    var inline267 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t240)
    var inline268 string = inline266 + inline267
    t241 = inline268
    var t242 string = t239 + t241
    var t243 bool
    var inline263 uint32 = 12
    var inline264 uint64 = 600000000
    switch inline264 {
    case 900000000:
        switch inline263 {
        case 4000000000:
            t243 = true
        default:
            t243 = false
        }
    case 600000000:
        t243 = true
    default:
        t243 = false
    }
    var t244 string
    var inline259 string = ",struct_second="
    var inline260 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t243)
    var inline261 string = inline259 + inline260
    t244 = inline261
    var message__9 string = t242 + t244
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline256)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t247 string = _goml_runtime_core_bool_to_string(self__64)
    return t247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
