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
    var t209 string
    var inline249 string = _goml_runtime_core_bool_to_string(value__6)
    t209 = inline249
    var t210 string = label__5 + t209
    return t210
}

func main0() struct{} {
    var t212 bool = is_flag8(200)
    var t213 string = report("u8_hit=", t212)
    var t214 bool = is_flag8(15)
    var t215 string = report(",u8_miss=", t214)
    var t216 string = t213 + t215
    var t217 bool = is_flag16(65000)
    var t218 string = report(",u16_hit=", t217)
    var t219 string = t216 + t218
    var t220 bool = is_flag16(42)
    var t221 string = report(",u16_miss=", t220)
    var t222 string = t219 + t221
    var t223 bool = is_flag32(1234567890)
    var t224 string = report(",u32_hit=", t223)
    var t225 string = t222 + t224
    var t226 bool
    var inline284 uint32 = 99
    switch inline284 {
    case 4000000000:
        t226 = true
    case 1234567890:
        t226 = true
    default:
        t226 = false
    }
    var t227 string
    var inline280 string = ",u32_miss="
    var inline281 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t226)
    var inline282 string = inline280 + inline281
    t227 = inline282
    var t228 string = t225 + t227
    var t229 bool
    var inline278 uint64 = 900000000
    switch inline278 {
    case 900000000:
        t229 = true
    case 600000000:
        t229 = true
    default:
        t229 = false
    }
    var t230 string
    var inline274 string = ",u64_hit="
    var inline275 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t229)
    var inline276 string = inline274 + inline275
    t230 = inline276
    var t231 string = t228 + t230
    var t232 bool
    var inline272 uint64 = 700000000
    switch inline272 {
    case 900000000:
        t232 = true
    case 600000000:
        t232 = true
    default:
        t232 = false
    }
    var t233 string
    var inline268 string = ",u64_miss="
    var inline269 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t232)
    var inline270 string = inline268 + inline269
    t233 = inline270
    var t234 string = t231 + t233
    var t235 bool
    var inline265 uint32 = 4000000000
    var inline266 uint64 = 900000000
    switch inline266 {
    case 900000000:
        switch inline265 {
        case 4000000000:
            t235 = true
        default:
            t235 = false
        }
    case 600000000:
        t235 = true
    default:
        t235 = false
    }
    var t236 string
    var inline261 string = ",struct_first="
    var inline262 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t235)
    var inline263 string = inline261 + inline262
    t236 = inline263
    var t237 string = t234 + t236
    var t238 bool
    var inline258 uint32 = 12
    var inline259 uint64 = 600000000
    switch inline259 {
    case 900000000:
        switch inline258 {
        case 4000000000:
            t238 = true
        default:
            t238 = false
        }
    case 600000000:
        t238 = true
    default:
        t238 = false
    }
    var t239 string
    var inline254 string = ",struct_second="
    var inline255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t238)
    var inline256 string = inline254 + inline255
    t239 = inline256
    var message__9 string = t237 + t239
    var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline251)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t242 string = _goml_runtime_core_bool_to_string(self__64)
    return t242
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
