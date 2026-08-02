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
    var t182 string
    var inline222 string = _goml_runtime_core_bool_to_string(value__6)
    t182 = inline222
    var t183 string = label__5 + t182
    return t183
}

func main0() struct{} {
    var t185 bool = is_flag8(200)
    var t186 string = report("u8_hit=", t185)
    var t187 bool = is_flag8(15)
    var t188 string = report(",u8_miss=", t187)
    var t189 string = t186 + t188
    var t190 bool = is_flag16(65000)
    var t191 string = report(",u16_hit=", t190)
    var t192 string = t189 + t191
    var t193 bool = is_flag16(42)
    var t194 string = report(",u16_miss=", t193)
    var t195 string = t192 + t194
    var t196 bool = is_flag32(1234567890)
    var t197 string = report(",u32_hit=", t196)
    var t198 string = t195 + t197
    var t199 bool
    var inline257 uint32 = 99
    switch inline257 {
    case 4000000000:
        t199 = true
    case 1234567890:
        t199 = true
    default:
        t199 = false
    }
    var t200 string
    var inline253 string = ",u32_miss="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    var inline255 string = inline253 + inline254
    t200 = inline255
    var t201 string = t198 + t200
    var t202 bool
    var inline251 uint64 = 900000000
    switch inline251 {
    case 900000000:
        t202 = true
    case 600000000:
        t202 = true
    default:
        t202 = false
    }
    var t203 string
    var inline247 string = ",u64_hit="
    var inline248 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t202)
    var inline249 string = inline247 + inline248
    t203 = inline249
    var t204 string = t201 + t203
    var t205 bool
    var inline245 uint64 = 700000000
    switch inline245 {
    case 900000000:
        t205 = true
    case 600000000:
        t205 = true
    default:
        t205 = false
    }
    var t206 string
    var inline241 string = ",u64_miss="
    var inline242 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t205)
    var inline243 string = inline241 + inline242
    t206 = inline243
    var t207 string = t204 + t206
    var t208 bool
    var inline238 uint32 = 4000000000
    var inline239 uint64 = 900000000
    switch inline239 {
    case 900000000:
        switch inline238 {
        case 4000000000:
            t208 = true
        default:
            t208 = false
        }
    case 600000000:
        t208 = true
    default:
        t208 = false
    }
    var t209 string
    var inline234 string = ",struct_first="
    var inline235 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t208)
    var inline236 string = inline234 + inline235
    t209 = inline236
    var t210 string = t207 + t209
    var t211 bool
    var inline231 uint32 = 12
    var inline232 uint64 = 600000000
    switch inline232 {
    case 900000000:
        switch inline231 {
        case 4000000000:
            t211 = true
        default:
            t211 = false
        }
    case 600000000:
        t211 = true
    default:
        t211 = false
    }
    var t212 string
    var inline227 string = ",struct_second="
    var inline228 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    var inline229 string = inline227 + inline228
    t212 = inline229
    var message__9 string = t210 + t212
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline224)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t215 string = _goml_runtime_core_bool_to_string(self__37)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
