package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int64_to_string(x int64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint16_to_string(x uint16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func show_u8(value__0 uint8) struct{} {
    println__T_uint8(value__0)
    return struct{}{}
}

func show_u16(value__1 uint16) struct{} {
    println__T_uint16(value__1)
    return struct{}{}
}

func show_u32(value__2 uint32) struct{} {
    println__T_uint32(value__2)
    return struct{}{}
}

func show_u64(value__3 uint64) struct{} {
    println__T_uint64(value__3)
    return struct{}{}
}

func show_i8(value__4 int8) struct{} {
    println__T_int8(value__4)
    return struct{}{}
}

func show_i16(value__5 int16) struct{} {
    println__T_int16(value__5)
    return struct{}{}
}

func show_i32(value__6 int32) struct{} {
    println__T_int32(value__6)
    return struct{}{}
}

func show_i64(value__7 int64) struct{} {
    println__T_int64(value__7)
    return struct{}{}
}

func unsigned_ops() struct{} {
    var t141_lhs uint8 = 13
    var t141_rhs uint8 = 5
    var t141 uint8 = t141_lhs % t141_rhs
    show_u8(t141)
    var t142_lhs uint8 = 12
    var t142_rhs uint8 = 10
    var t142 uint8 = t142_lhs & t142_rhs
    show_u8(t142)
    var t143_lhs uint8 = 12
    var t143_rhs uint8 = 3
    var t143 uint8 = t143_lhs | t143_rhs
    show_u8(t143)
    var t144_lhs uint8 = 12
    var t144_rhs uint8 = 10
    var t144 uint8 = t144_lhs ^ t144_rhs
    show_u8(t144)
    var t145_lhs uint8 = 1
    var t145_rhs uint32 = 7
    var t145 uint8 = t145_lhs << t145_rhs
    show_u8(t145)
    var t146_lhs uint8 = 128
    var t146_rhs uint16 = 7
    var t146 uint8 = t146_lhs >> t146_rhs
    show_u8(t146)
    var t147_operand uint8 = 0
    var t147 uint8 = ^t147_operand
    show_u8(t147)
    var t148_lhs uint8 = 1
    var t148_rhs uint8 = 8
    var t148 uint8 = t148_lhs << t148_rhs
    show_u8(t148)
    var t149_lhs uint16 = 513
    var t149_rhs uint16 = 256
    var t149 uint16 = t149_lhs % t149_rhs
    show_u16(t149)
    var t150_lhs uint16 = 3855
    var t150_rhs uint16 = 255
    var t150 uint16 = t150_lhs & t150_rhs
    show_u16(t150)
    var t151_lhs uint16 = 3840
    var t151_rhs uint16 = 15
    var t151 uint16 = t151_lhs | t151_rhs
    show_u16(t151)
    var t152_lhs uint16 = 43690
    var t152_rhs uint16 = 3855
    var t152 uint16 = t152_lhs ^ t152_rhs
    show_u16(t152)
    var t153_lhs uint16 = 1
    var t153_rhs uint32 = 15
    var t153 uint16 = t153_lhs << t153_rhs
    show_u16(t153)
    var t154_lhs uint16 = 32768
    var t154_rhs uint8 = 15
    var t154 uint16 = t154_lhs >> t154_rhs
    show_u16(t154)
    var t155_operand uint16 = 0
    var t155 uint16 = ^t155_operand
    show_u16(t155)
    var t156_lhs uint32 = 1000000001
    var t156_rhs uint32 = 1000
    var t156 uint32 = t156_lhs % t156_rhs
    show_u32(t156)
    var t157_lhs uint32 = 4042322160
    var t157_rhs uint32 = 252645135
    var t157 uint32 = t157_lhs & t157_rhs
    show_u32(t157)
    var t158_lhs uint32 = 4042322160
    var t158_rhs uint32 = 252645135
    var t158 uint32 = t158_lhs | t158_rhs
    show_u32(t158)
    var t159_lhs uint32 = 4042322160
    var t159_rhs uint32 = 252645135
    var t159 uint32 = t159_lhs ^ t159_rhs
    show_u32(t159)
    var t160_lhs uint32 = 1
    var t160_rhs uint32 = 31
    var t160 uint32 = t160_lhs << t160_rhs
    show_u32(t160)
    var t161_lhs uint32 = 2147483648
    var t161_rhs uint32 = 31
    var t161 uint32 = t161_lhs >> t161_rhs
    show_u32(t161)
    var t162_operand uint32 = 0
    var t162 uint32 = ^t162_operand
    show_u32(t162)
    var t163_lhs uint64 = 1000000000001
    var t163_rhs uint64 = 1000
    var t163 uint64 = t163_lhs % t163_rhs
    show_u64(t163)
    var t164_lhs uint64 = 17361641481138401520
    var t164_rhs uint64 = 1085102592571150095
    var t164 uint64 = t164_lhs & t164_rhs
    show_u64(t164)
    var t165_lhs uint64 = 17361641481138401520
    var t165_rhs uint64 = 1085102592571150095
    var t165 uint64 = t165_lhs | t165_rhs
    show_u64(t165)
    var t166_lhs uint64 = 17361641481138401520
    var t166_rhs uint64 = 1085102592571150095
    var t166 uint64 = t166_lhs ^ t166_rhs
    show_u64(t166)
    var t167_lhs uint64 = 1
    var t167_rhs uint64 = 63
    var t167 uint64 = t167_lhs << t167_rhs
    show_u64(t167)
    var t168_lhs uint64 = 9223372036854775808
    var t168_rhs uint64 = 63
    var t168 uint64 = t168_lhs >> t168_rhs
    show_u64(t168)
    var t169_operand uint64 = 0
    var t169 uint64 = ^t169_operand
    show_u64(t169)
    return struct{}{}
}

func signed_ops() struct{} {
    var t172 int8 = -13
    var t173_rhs int8 = 5
    var t173 int8 = t172 % t173_rhs
    show_i8(t173)
    var t174 int8 = -8
    var t175_rhs int8 = 2
    var t175 int8 = t174 >> t175_rhs
    show_i8(t175)
    var t176_lhs int8 = 1
    var t176_rhs int8 = 6
    var t176 int8 = t176_lhs << t176_rhs
    show_i8(t176)
    var t177_operand int8 = 0
    var t177 int8 = ^t177_operand
    show_i8(t177)
    var t178 int8 = -1
    var t179_rhs int8 = 7
    var t179 int8 = t178 >> t179_rhs
    show_i8(t179)
    var t180 int16 = -513
    var t181_rhs int16 = 256
    var t181 int16 = t180 % t181_rhs
    show_i16(t181)
    var t182 int16 = -32767
    var t183 int16 = t182 - 1
    var t184_rhs int16 = 15
    var t184 int16 = t183 >> t184_rhs
    show_i16(t184)
    var t185_lhs int16 = 1
    var t185_rhs int16 = 14
    var t185 int16 = t185_lhs << t185_rhs
    show_i16(t185)
    var t186_operand int16 = 255
    var t186 int16 = ^t186_operand
    show_i16(t186)
    var t187 int32 = -1000000001
    var t188_rhs int32 = 1000
    var t188 int32 = t187 % t188_rhs
    show_i32(t188)
    var t189 int32 = -2147483647
    var t190 int32 = t189 - 1
    var t191_rhs int32 = 31
    var t191 int32 = t190 >> t191_rhs
    show_i32(t191)
    var t192_lhs int32 = 1
    var t192_rhs int32 = 30
    var t192 int32 = t192_lhs << t192_rhs
    show_i32(t192)
    var t193_operand int32 = 65535
    var t193 int32 = ^t193_operand
    show_i32(t193)
    var t194 int64 = -1000000000001
    var t195_rhs int64 = 1000
    var t195 int64 = t194 % t195_rhs
    show_i64(t195)
    var t196 int64 = -9223372036854775807
    var t197_rhs int64 = 62
    var t197 int64 = t196 >> t197_rhs
    show_i64(t197)
    var t198_lhs int64 = 1
    var t198_rhs int64 = 62
    var t198 int64 = t198_lhs << t198_rhs
    show_i64(t198)
    var t199_operand int64 = 4294967295
    var t199 int64 = ^t199_operand
    show_i64(t199)
    return struct{}{}
}

func precedence() struct{} {
    var t202_lhs uint8 = 3
    var t202_rhs uint8 = 1
    var t202 uint8 = t202_lhs & t202_rhs
    var t203_lhs uint8 = 2
    var t203 uint8 = t203_lhs ^ t202
    var t204_lhs uint8 = 1
    var t204 uint8 = t204_lhs | t203
    show_u8(t204)
    var t205 uint8 = 2 + 1
    var t206_lhs uint8 = 1
    var t206 uint8 = t206_lhs << t205
    show_u8(t206)
    var t207_lhs uint8 = 1
    var t207_rhs uint8 = 2
    var t207 uint8 = t207_lhs | t207_rhs
    var t208 bool = t207 == 3
    var t209 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t208)
    println__T_string(t209)
    var t210_lhs uint8 = 8
    var t210_rhs uint8 = 1
    var t210 uint8 = t210_lhs >> t210_rhs
    var t211 bool = t210 < 5
    var t212 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    println__T_string(t212)
    var t213_operand uint8 = 1
    var t213 uint8 = ^t213_operand
    var t214_rhs uint8 = 15
    var t214 uint8 = t213 & t214_rhs
    show_u8(t214)
    return struct{}{}
}

func casts() struct{} {
    var t217_source uint16 = 511
    var t217 uint8 = uint8(uint16(t217_source))
    show_u8(t217)
    var t218_source uint16 = 256
    var t218 uint8 = uint8(uint16(t218_source))
    show_u8(t218)
    var t219 int16 = -1
    var t220 uint8 = uint8(int16(t219))
    show_u8(t220)
    var t221_source uint8 = 255
    var t221 int8 = int8(uint8(t221_source))
    show_i8(t221)
    var t222_source uint8 = 128
    var t222 int8 = int8(uint8(t222_source))
    show_i8(t222)
    var t223 int16 = -129
    var t224 int8 = int8(int16(t223))
    show_i8(t224)
    var t225_source uint16 = 65535
    var t225 int16 = int16(uint16(t225_source))
    show_i16(t225)
    var t226 int32 = -1
    var t227 uint16 = uint16(int32(t226))
    show_u16(t227)
    var t228 int8 = -1
    var t229 uint64 = uint64(int8(t228))
    show_u64(t229)
    var t230_source uint64 = 18446744073709551615
    var t230 int32 = int32(uint64(t230_source))
    show_i32(t230)
    var t231_source uint8 = 65
    var t231 uint32 = uint32(uint8(t231_source))
    show_u32(t231)
    var t232_source uint32 = 4294967295
    var t232 int64 = int64(uint32(t232_source))
    show_i64(t232)
    var t233_source rune = 65
    var t233 uint32 = uint32(rune(t233_source))
    show_u32(t233)
    var t234_source uint32 = 128512
    var t234 rune = rune(uint32(t234_source))
    var t235 string = _goml_m_inherent_i_char_i_char_i_to__string(t234)
    println__T_string(t235)
    var t236_source uint16 = 300
    var t236 uint8 = uint8(uint16(t236_source))
    var t237 uint32 = uint32(uint8(t236))
    show_u32(t237)
    return struct{}{}
}

func contextual(value__8 uint8) uint8 {
    var retv240 uint8
    var t241 uint8 = ^value__8
    var t242_rhs uint8 = 15
    var t242 uint8 = t241 & t242_rhs
    var t243_lhs uint8 = 1
    var t243_rhs int32 = 4
    var t243 uint8 = t243_lhs << t243_rhs
    var t244_rhs uint8 = 31
    var t244 uint8 = t243 % t244_rhs
    var t245 uint8 = t242 | t244
    retv240 = t245
    return retv240
}

func main0() struct{} {
    unsigned_ops()
    signed_ops()
    precedence()
    casts()
    var t247 uint8 = contextual(10)
    show_u8(t247)
    return struct{}{}
}

func println__T_uint8(value__1 uint8) struct{} {
    var t250 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func println__T_uint16(value__1 uint16) struct{} {
    var t253 string = _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(value__1)
    _goml_runtime_core_string_println(t253)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t256 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(value__1)
    _goml_runtime_core_string_println(t256)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t259 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func println__T_int8(value__1 int8) struct{} {
    var t262 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(value__1)
    _goml_runtime_core_string_println(t262)
    return struct{}{}
}

func println__T_int16(value__1 int16) struct{} {
    var t265 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(value__1)
    _goml_runtime_core_string_println(t265)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t268 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t268)
    return struct{}{}
}

func println__T_int64(value__1 int64) struct{} {
    var t271 string = _goml_m_trait__impl_i_ToString_i_int64_i_to__string(value__1)
    _goml_runtime_core_string_println(t271)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t274)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv277 string
    var t278 string = _goml_runtime_core_bool_to_string(self__33)
    retv277 = t278
    return retv277
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv280 string
    var t281 string = _goml_runtime_core_char_to_string(self__3)
    retv280 = t281
    return retv280
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv283 string
    var t284 string = _goml_runtime_core_uint8_to_string(self__40)
    retv283 = t284
    return retv283
}

func _goml_m_trait__impl_i_ToString_i_uint16_i_to__string(self__41 uint16) string {
    var retv286 string
    var t287 string = _goml_runtime_core_uint16_to_string(self__41)
    retv286 = t287
    return retv286
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__42 uint32) string {
    var retv289 string
    var t290 string = _goml_runtime_core_uint32_to_string(self__42)
    retv289 = t290
    return retv289
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__43 uint64) string {
    var retv292 string
    var t293 string = _goml_runtime_core_uint64_to_string(self__43)
    retv292 = t293
    return retv292
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__36 int8) string {
    var retv295 string
    var t296 string = _goml_runtime_core_int8_to_string(self__36)
    retv295 = t296
    return retv295
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__37 int16) string {
    var retv298 string
    var t299 string = _goml_runtime_core_int16_to_string(self__37)
    retv298 = t299
    return retv298
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv301 string
    var t302 string = _goml_runtime_core_int32_to_string(self__38)
    retv301 = t302
    return retv301
}

func _goml_m_trait__impl_i_ToString_i_int64_i_to__string(self__39 int64) string {
    var retv304 string
    var t305 string = _goml_runtime_core_int64_to_string(self__39)
    retv304 = t305
    return retv304
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv307 string
    retv307 = self__34
    return retv307
}

func main() {
    main0()
}
