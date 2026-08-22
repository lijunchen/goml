package main

import (
    _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29 "math"
)

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

func _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(arg0 float32) uint32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32bits(arg0)
}

func _goml_ffi_math_x00_Float32frombit__q__m__z_f32_h1605b723e1fe36562e475cf246e4961c(arg0 uint32) float32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32frombits(arg0)
}

func _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(arg0 uint64) float64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64frombits(arg0)
}

func _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(arg0 float64) uint64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64bits(arg0)
}

type _goml_vec_uint32 struct {
    items []uint32
}

func vec_new__Vec_6uint32() *_goml_vec_uint32 {
    return &_goml_vec_uint32{
        items: nil,
    }
}

func vec_push__Vec_6uint32(vec *_goml_vec_uint32, elem uint32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6uint32(vec *_goml_vec_uint32, index int) uint32 {
    return vec.items[index]
}

func vec_set__Vec_6uint32(vec *_goml_vec_uint32, index int, value uint32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_6uint32(vec *_goml_vec_uint32) int {
    return int(len(vec.items))
}

func vec_truncate__Vec_6uint32(vec *_goml_vec_uint32, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_set__Vec_5uint8(vec *_goml_vec_uint8, index int, value uint8) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type Tuple2_4bool_7float64 struct {
    _0 bool
    _1 float64
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_4bool_3int struct {
    _0 bool
    _1 int
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type Ordering int32

func main0() struct{} {
    var single__0 float32 = 1.5
    var t831 uint32 = _goml_m_inherent_i_f32_i_f32_i_to__bits(single__0)
    println__T_u32(t831)
    var t832 float32 = float32_from_bits(1069547520)
    var t833 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(t832)
    println__T_string(t833)
    var t834 float64 = _goml_m_inherent_i_f32_i_f32_i_to__f64(single__0)
    var t835 uint64 = _goml_m_inherent_i_f64_i_f64_i_to__bits(t834)
    println__T_u64(t835)
    var t836 float32 = _goml_m_inherent_i_f64_i_f64_i_to__f32(16777217)
    var t837 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(t836)
    println__T_string(t837)
    var t838 float64 = float64_from_bits(4607182418800017408)
    var t839 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(t838)
    println__T_string(t839)
    var mtmp801 Tuple2_4bool_7float64 = string_parse_float64("0x1.8p+1")
    var x802 bool = mtmp801._0
    var x803 float64 = mtmp801._1
    println__T_bool(x802)
    println__T_f64(x803)
    var mtmp806 Tuple2_4bool_7float64
    var inline2072 string = "1.7976931348623157e308"
    var inline2073 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline2072)
    mtmp806 = inline2073
    var x807 bool = mtmp806._0
    var x808 float64 = mtmp806._1
    var inline2069 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x807)
    _goml_runtime_core_string_println(inline2069)
    var t840 uint64
    var inline2067 uint64 = __goml_builtin_float64_to_bits(x808)
    t840 = inline2067
    var inline2064 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(t840)
    _goml_runtime_core_string_println(inline2064)
    var mtmp811 Tuple2_4bool_7float64
    var inline2061 string = "5e-324"
    var inline2062 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline2061)
    mtmp811 = inline2062
    var x812 bool = mtmp811._0
    var x813 float64 = mtmp811._1
    var inline2058 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x812)
    _goml_runtime_core_string_println(inline2058)
    var t841 uint64
    var inline2056 uint64 = __goml_builtin_float64_to_bits(x813)
    t841 = inline2056
    var inline2053 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(t841)
    _goml_runtime_core_string_println(inline2053)
    var mtmp816 Tuple2_4bool_7float64
    var inline2050 string = "3.4028235e38"
    var inline2051 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float32(inline2050)
    mtmp816 = inline2051
    var x817 bool = mtmp816._0
    var x818 float64 = mtmp816._1
    var inline2047 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x817)
    _goml_runtime_core_string_println(inline2047)
    var t842 float32
    var inline2045 float32 = __goml_builtin_float64_to_float32(x818)
    t842 = inline2045
    var t843 uint32
    var inline2043 uint32 = __goml_builtin_float32_to_bits(t842)
    t843 = inline2043
    var inline2040 string = _goml_m_trait__impl_i_ToString_i_u32_i_to__string(t843)
    _goml_runtime_core_string_println(inline2040)
    var mtmp821 Tuple2_4bool_7float64
    var inline2037 string = "1e309"
    var inline2038 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline2037)
    mtmp821 = inline2038
    var x822 bool = mtmp821._0
    var x823 float64 = mtmp821._1
    var inline2034 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x822)
    _goml_runtime_core_string_println(inline2034)
    var inline2031 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(x823)
    _goml_runtime_core_string_println(inline2031)
    var mtmp826 Tuple2_4bool_7float64
    var inline2028 string = "1e-400"
    var inline2029 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline2028)
    mtmp826 = inline2029
    var x827 bool = mtmp826._0
    var x828 float64 = mtmp826._1
    var inline2025 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x827)
    _goml_runtime_core_string_println(inline2025)
    var inline2022 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(x828)
    _goml_runtime_core_string_println(inline2022)
    return struct{}{}
}

func println__T_u32(value__1 uint32) struct{} {
    var t846 string
    var inline2075 string = __goml_builtin_uint32_to_string(value__1)
    t846 = inline2075
    _goml_runtime_core_string_println(t846)
    return struct{}{}
}

func _goml_m_inherent_i_f32_i_f32_i_to__bits(self__855 float32) uint32 {
    var inline2077 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(self__855)
    return inline2077
}

func println__T_string(value__1 string) struct{} {
    var t852 string
    t852 = value__1
    _goml_runtime_core_string_println(t852)
    return struct{}{}
}

func float32_from_bits(value__237 uint32) float32 {
    var inline2080 float32 = _goml_ffi_math_x00_Float32frombit__q__m__z_f32_h1605b723e1fe36562e475cf246e4961c(value__237)
    return inline2080
}

func _goml_m_trait__impl_i_ToString_i_f32_i_to__string(self__413 float32) string {
    var inline2082 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(self__413)
    var inline2083 uint64 = uint64(uint32(inline2082))
    var inline2084 string = format_float_bits(inline2083, 23, 8, 127)
    return inline2084
}

func println__T_u64(value__1 uint64) struct{} {
    var t861 string
    var inline2086 string = __goml_builtin_uint64_to_string(value__1)
    t861 = inline2086
    _goml_runtime_core_string_println(t861)
    return struct{}{}
}

func _goml_m_inherent_i_f32_i_f32_i_to__f64(self__857 float32) float64 {
    var inline2088 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(self__857)
    var inline2089 uint64 = float32_bits_to_float64_bits(inline2088)
    var inline2090 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(inline2089)
    return inline2090
}

func _goml_m_inherent_i_f64_i_f64_i_to__bits(self__858 float64) uint64 {
    var inline2092 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__858)
    return inline2092
}

func _goml_m_inherent_i_f64_i_f64_i_to__f32(self__860 float64) float32 {
    var inline2094 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__860)
    var inline2095 uint32 = float64_bits_to_float32_bits(inline2094)
    var inline2096 float32 = _goml_ffi_math_x00_Float32frombit__q__m__z_f32_h1605b723e1fe36562e475cf246e4961c(inline2095)
    return inline2096
}

func float64_from_bits(value__239 uint64) float64 {
    var inline2098 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(value__239)
    return inline2098
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__414 float64) string {
    var inline2100 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__414)
    var inline2101 string = format_float_bits(inline2100, 52, 11, 1023)
    return inline2101
}

func string_parse_float64(value__235 string) Tuple2_4bool_7float64 {
    var inline2103 Tuple2_4bool_6uint64 = parsed_float_bits(value__235, 52, 1023)
    var inline2104 bool = inline2103._0
    var inline2105 uint64 = inline2103._1
    var inline2108 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(inline2105)
    var inline2109 Tuple2_4bool_7float64 = Tuple2_4bool_7float64{
        _0: inline2104,
        _1: inline2108,
    }
    return inline2109
}

func println__T_bool(value__1 bool) struct{} {
    var t882 string
    var inline2111 string = _goml_runtime_core_bool_to_string(value__1)
    t882 = inline2111
    _goml_runtime_core_string_println(t882)
    return struct{}{}
}

func println__T_f64(value__1 float64) struct{} {
    var t885 string
    var inline2113 string = __goml_builtin_float64_to_string(value__1)
    t885 = inline2113
    _goml_runtime_core_string_println(t885)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_u32_i_to__string(self__411 uint32) string {
    var inline2125 uint64 = uint64(uint32(self__411))
    var inline2126 string = decimal_string(inline2125)
    return inline2126
}

func __goml_builtin_float32_to_bits(value__202 float32) uint32 {
    var t895 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__202)
    return t895
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__412 uint64) string {
    var inline2128 string = decimal_string(self__412)
    return inline2128
}

func __goml_builtin_float64_to_bits(value__204 float64) uint64 {
    var t916 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__204)
    return t916
}

func __goml_builtin_float64_to_float32(value__207 float64) float32 {
    var t919 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__207)
    var t920 uint32 = float64_bits_to_float32_bits(t919)
    var t921 float32 = _goml_ffi_math_x00_Float32frombit__q__m__z_f32_h1605b723e1fe36562e475cf246e4961c(t920)
    return t921
}

func __goml_builtin_float64_to_string(value__195 float64) string {
    var t927 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__195)
    var t928 string = format_float_bits(t927, 52, 11, 1023)
    return t928
}

func __goml_builtin_string_parse_float64(value__199 string) Tuple2_4bool_7float64 {
    var mtmp348 Tuple2_4bool_6uint64 = parsed_float_bits(value__199, 52, 1023)
    var x349 bool = mtmp348._0
    var x350 uint64 = mtmp348._1
    var t931 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(x350)
    var t932 Tuple2_4bool_7float64 = Tuple2_4bool_7float64{
        _0: x349,
        _1: t931,
    }
    return t932
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t935 string = _goml_runtime_core_bool_to_string(self__401)
    return t935
}

func __goml_builtin_string_parse_float32(value__196 string) Tuple2_4bool_7float64 {
    var mtmp345 Tuple2_4bool_6uint64 = parsed_float_bits(value__196, 23, 127)
    var x346 bool = mtmp345._0
    var x347 uint64 = mtmp345._1
    var t938 uint32 = uint32(uint64(x347))
    var t939 uint64 = float32_bits_to_float64_bits(t938)
    var t940 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(t939)
    var t941 Tuple2_4bool_7float64 = Tuple2_4bool_7float64{
        _0: x346,
        _1: t940,
    }
    return t941
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t944 uint64 = uint64(uint32(value__230))
    var t945 string = decimal_string(t944)
    return t945
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t948 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t948
    var t949 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t949 != 0
    var t950_lhs uint64 = 1
    var t950 uint64 = t950_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t950 - 1
    var t951 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t951 & exponent_mask__166
    var t952_lhs uint64 = 1
    var t952 uint64 = t952_lhs << mantissa_bits__161
    var t953 uint64 = t952 - 1
    var fraction__168 uint64 = bits__160 & t953
    var t1017 bool = exponent__167 == exponent_mask__166
    if t1017 {
        var t1019 bool = fraction__168 == 0
        if t1019 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t1025 bool = exponent__167 == 0
        var jp1023 bool
        if t1025 {
            var t1026 bool = fraction__168 == 0
            jp1023 = t1026
        } else {
            jp1023 = false
        }
        if jp1023 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t1014 bool = exponent__167 == 0
            var jp956 uint64
            if t1014 {
                jp956 = fraction__168
            } else {
                var t1015_lhs uint64 = 1
                var t1015 uint64 = t1015_lhs << mantissa_bits__161
                var t1016 uint64 = fraction__168 | t1015
                jp956 = t1016
            }
            var t1008 bool = exponent__167 == 0
            var jp958 int
            if t1008 {
                var t1009 int = 1 - exponent_bias__163
                var t1010 int = t1009 - mantissa_bits__161
                jp958 = t1010
            } else {
                var t1011 int = int(uint64(exponent__167))
                var t1012 int = t1011 - exponent_bias__163
                var t1013 int = t1012 - mantissa_bits__161
                jp958 = t1013
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp956)
            var t963 bool = jp958 >= 0
            var jp960 int
            if t963 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp958)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t982 bool = mantissa_bits__161 == 23
                var jp965 int
                if t982 {
                    jp965 = 9
                } else {
                    jp965 = 17
                }
                var t979 int
                var inline2136 int = _goml_runtime_core_string_len(digits__173)
                t979 = inline2136
                var t980 bool = t979 < jp965
                var jp967 int
                if t980 {
                    var inline2130 int = _goml_runtime_core_string_len(digits__173)
                    jp967 = inline2130
                } else {
                    jp967 = jp965
                }
                var count__176 int = 1
                Loop_loop970:
                for {
                    var t971 bool = count__176 <= jp967
                    if t971 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t972 int
                        var inline2132 int = _goml_runtime_core_string_len(digits__173)
                        t972 = inline2132
                        var jp974 int
                        if x319 {
                            jp974 = 1
                        } else {
                            jp974 = 0
                        }
                        var point__180 int = t972 + jp974
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t978 bool = x322 == bits__160
                        if t978 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t976 int = compound_old324 + compound_value325
                            count__176 = t976
                            continue
                        }
                    } else {
                        break Loop_loop970
                    }
                }
                var inline2134 int = _goml_runtime_core_string_len(digits__173)
                jp960 = inline2134
                var t961 string = float_natural_decimal(exact_value__171)
                var t962 string = fixed_float_text(t961, jp960, negative__165)
                return t962
            } else {
                var count__183 int = 0
                var t1004 int = 0 - jp958
                Loop_loop1003:
                for {
                    var t1005 bool = count__183 < t1004
                    if t1005 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t1006 int = compound_old329 + compound_value330
                        count__183 = t1006
                        continue
                    } else {
                        break Loop_loop1003
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t984 int
                var inline2142 int = _goml_runtime_core_string_len(digits__184)
                t984 = inline2142
                var point__185 int = t984 + jp958
                var t1002 bool = mantissa_bits__161 == 23
                var jp986 int
                if t1002 {
                    jp986 = 9
                } else {
                    jp986 = 17
                }
                var t999 int
                var inline2140 int = _goml_runtime_core_string_len(digits__184)
                t999 = inline2140
                var t1000 bool = t999 < jp986
                var jp988 int
                if t1000 {
                    var inline2138 int = _goml_runtime_core_string_len(digits__184)
                    jp988 = inline2138
                } else {
                    jp988 = jp986
                }
                count__183 = 1
                Loop_loop990:
                for {
                    var t991 bool = count__183 <= jp988
                    if t991 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp993 int
                        if x336 {
                            jp993 = 1
                        } else {
                            jp993 = 0
                        }
                        var t994 int = point__185 + jp993
                        var candidate__191 string = fixed_float_text(rounded__190, t994, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t998 bool = x339 == bits__160
                        if t998 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t996 int = compound_old341 + compound_value342
                            count__183 = t996
                            continue
                        }
                    } else {
                        break Loop_loop990
                    }
                }
                jp960 = point__185
                var t961 string = float_natural_decimal(exact_value__171)
                var t962 string = fixed_float_text(t961, jp960, negative__165)
                return t962
            }
        }
    }
}

func __goml_builtin_uint64_to_string(value__231 uint64) string {
    var t1029 string = decimal_string(value__231)
    return t1029
}

func float32_bits_to_float64_bits(value__123 uint32) uint64 {
    var t1032 uint64 = uint64(uint32(value__123))
    var t1033_rhs uint64 = 2147483648
    var t1033 uint64 = t1032 & t1033_rhs
    var sign__124_rhs int = 32
    var sign__124 uint64 = t1033 << sign__124_rhs
    var t1034_rhs int = 23
    var t1034 uint32 = value__123 >> t1034_rhs
    var exponent__125_rhs uint32 = 255
    var exponent__125 uint32 = t1034 & exponent__125_rhs
    var fraction__126_rhs uint32 = 8388607
    var fraction__126 uint32 = value__123 & fraction__126_rhs
    var t1037 bool = exponent__125 == 255
    if t1037 {
        var t1040 bool = fraction__126 == 0
        if t1040 {
            var t1041_rhs uint64 = 9218868437227405312
            var t1041 uint64 = sign__124 | t1041_rhs
            return t1041
        } else {
            return 9221120237041090560
        }
    } else {
        var t1044 bool = exponent__125 == 0
        if t1044 {
            var t1047 bool = fraction__126 == 0
            if t1047 {
                return sign__124
            } else {
                var high__127 int = 0
                var remaining__128 uint32 = fraction__126
                Loop_loop1059:
                for {
                    var t1060 bool = remaining__128 > 1
                    if t1060 {
                        var compound_old220 uint32 = remaining__128
                        var compound_value221 int = 1
                        var t1061 uint32 = compound_old220 >> compound_value221
                        remaining__128 = t1061
                        var compound_old223 int = high__127
                        var compound_value224 int = 1
                        var t1063 int = compound_old223 + compound_value224
                        high__127 = t1063
                        continue
                    } else {
                        break Loop_loop1059
                    }
                }
                var unbiased__129 int = high__127 - 149
                var t1049 int = unbiased__129 + 1023
                var t1050 uint64 = uint64(int(t1049))
                var t1051_rhs int = 52
                var t1051 uint64 = t1050 << t1051_rhs
                var t1052 uint64 = sign__124 | t1051
                var t1053 uint64 = uint64(uint32(fraction__126))
                var t1054_lhs uint64 = 1
                var t1054 uint64 = t1054_lhs << high__127
                var t1055 uint64 = t1053 - t1054
                var t1056 int = 52 - high__127
                var t1057 uint64 = t1055 << t1056
                var t1058 uint64 = t1052 | t1057
                return t1058
            }
        } else {
            var t1065 int = int(uint32(exponent__125))
            var t1066 int = t1065 - 127
            var t1067 int = t1066 + 1023
            var t1068 uint64 = uint64(int(t1067))
            var t1069_rhs int = 52
            var t1069 uint64 = t1068 << t1069_rhs
            var t1070 uint64 = sign__124 | t1069
            var t1071 uint64 = uint64(uint32(fraction__126))
            var t1072_rhs int = 29
            var t1072 uint64 = t1071 << t1072_rhs
            var t1073 uint64 = t1070 | t1072
            return t1073
        }
    }
}

func float64_bits_to_float32_bits(value__130 uint64) uint32 {
    var t1076_rhs int = 32
    var t1076 uint64 = value__130 >> t1076_rhs
    var t1077_rhs uint64 = 2147483648
    var t1077 uint64 = t1076 & t1077_rhs
    var sign__131 uint32 = uint32(uint64(t1077))
    var t1078_rhs int = 52
    var t1078 uint64 = value__130 >> t1078_rhs
    var exponent__132_rhs uint64 = 2047
    var exponent__132 uint64 = t1078 & exponent__132_rhs
    var fraction__133_rhs uint64 = 4503599627370495
    var fraction__133 uint64 = value__130 & fraction__133_rhs
    var t1081 bool = exponent__132 == 2047
    if t1081 {
        var t1084 bool = fraction__133 == 0
        if t1084 {
            var t1085_rhs uint32 = 2139095040
            var t1085 uint32 = sign__131 | t1085_rhs
            return t1085
        } else {
            return 2143289344
        }
    } else {
        var t1105 bool = exponent__132 == 0
        var jp1089 bool
        if t1105 {
            var t1106 bool = fraction__133 == 0
            jp1089 = t1106
        } else {
            jp1089 = false
        }
        if jp1089 {
            return sign__131
        } else {
            var t1102 bool = exponent__132 == 0
            var jp1091 uint64
            if t1102 {
                jp1091 = fraction__133
            } else {
                var t1103_lhs uint64 = 1
                var t1103_rhs int = 52
                var t1103 uint64 = t1103_lhs << t1103_rhs
                var t1104 uint64 = fraction__133 | t1103
                jp1091 = t1104
            }
            var t1098 bool = exponent__132 == 0
            var jp1093 int
            if t1098 {
                jp1093 = -1074
            } else {
                var t1099 int = int(uint64(exponent__132))
                var t1100 int = t1099 - 1023
                var t1101 int = t1100 - 52
                jp1093 = t1101
            }
            var t1094 FloatNatural = float_natural_from_u64(jp1091)
            var t1095 FloatNatural
            var inline2144 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            vec_push__Vec_6uint32(inline2144, 1)
            var inline2146 FloatNatural = FloatNatural{
                words: inline2144,
            }
            t1095 = inline2146
            var mtmp227 Tuple2_6uint64_4bool = float_rational_bits(t1094, t1095, jp1093, 23, 127)
            var x228 uint64 = mtmp227._0
            var t1096 uint32 = uint32(uint64(x228))
            var t1097 uint32 = sign__131 | t1096
            return t1097
        }
    }
}

func parsed_float_bits(value__107 string, mantissa_bits__108 int, exponent_bias__109 int) Tuple2_4bool_6uint64 {
    var parsed__110 ParsedFloat = parse_float_text(value__107)
    var t1200 bool = parsed__110.valid
    var t1201 bool = !t1200
    if t1201 {
        var t1202 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1202
    } else {
        var t1194 bool = parsed__110.negative
        var jp1111 uint64
        if t1194 {
            var t1199 bool = mantissa_bits__108 == 23
            var jp1196 int
            if t1199 {
                jp1196 = 8
            } else {
                jp1196 = 11
            }
            var t1197 int = mantissa_bits__108 + jp1196
            var t1198_lhs uint64 = 1
            var t1198 uint64 = t1198_lhs << t1197
            jp1111 = t1198
        } else {
            jp1111 = 0
        }
        var t1193 bool = mantissa_bits__108 == 23
        var jp1113 int
        if t1193 {
            jp1113 = 8
        } else {
            jp1113 = 11
        }
        var t1114_lhs uint64 = 1
        var t1114 uint64 = t1114_lhs << jp1113
        var t1115 uint64 = t1114 - 1
        var exponent_mask__112 uint64 = t1115 << mantissa_bits__108
        var t1171 int = parsed__110.special
        var t1172 bool = t1171 == 1
        if t1172 {
            var t1173 uint64 = jp1111 | exponent_mask__112
            var t1174 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1173,
            }
            return t1174
        } else {
            var t1176 int = parsed__110.special
            var t1177 bool = t1176 == 2
            if t1177 {
                var t1181 int = mantissa_bits__108 - 1
                var t1182_lhs uint64 = 1
                var t1182 uint64 = t1182_lhs << t1181
                var t1183 uint64 = exponent_mask__112 | t1182
                var t1188 bool = mantissa_bits__108 == 52
                var jp1185 uint64
                if t1188 {
                    jp1185 = 1
                } else {
                    jp1185 = 0
                }
                var t1186 uint64 = t1183 | jp1185
                var t1187 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1186,
                }
                return t1187
            } else {
                var t1190 FloatNatural = parsed__110.numerator
                var t1191 bool
                var inline2148 *_goml_vec_uint32 = t1190.words
                var inline2149 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2148)
                t1191 = inline2149
                if t1191 {
                    var t1192 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1111,
                    }
                    return t1192
                } else {
                    var t1154 bool = parsed__110.hexadecimal
                    var t1155 bool = !t1154
                    if t1155 {
                        var t1156 int = parsed__110.significant_digits
                        var t1157 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1156 + t1157
                        var t1170 bool = mantissa_bits__108 == 23
                        var jp1159 int
                        if t1170 {
                            jp1159 = 40
                        } else {
                            jp1159 = 310
                        }
                        var t1169 bool = mantissa_bits__108 == 23
                        var jp1161 int
                        if t1169 {
                            jp1161 = -46
                        } else {
                            jp1161 = -325
                        }
                        var t1163 bool = decimal_position__113 > jp1159
                        if t1163 {
                            var t1164 uint64 = jp1111 | exponent_mask__112
                            var t1165 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1164,
                            }
                            return t1165
                        } else {
                            var t1167 bool = decimal_position__113 < jp1161
                            if t1167 {
                                var t1168 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1111,
                                }
                                return t1168
                            } else {
                                var t1150 bool = parsed__110.hexadecimal
                                var t1151 bool = !t1150
                                var jp1145 bool
                                if t1151 {
                                    var t1152 int = parsed__110.decimal_exponent
                                    var t1153 bool = t1152 < 0
                                    jp1145 = t1153
                                } else {
                                    jp1145 = false
                                }
                                var jp1119 FloatNatural
                                if jp1145 {
                                    var t1146 int = parsed__110.decimal_exponent
                                    var t1147 int = 0 - t1146
                                    var t1148 FloatNatural = float_natural_power5(t1147)
                                    jp1119 = t1148
                                } else {
                                    var inline2151 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2151, 1)
                                    var inline2153 FloatNatural = FloatNatural{
                                        words: inline2151,
                                    }
                                    jp1119 = inline2153
                                }
                                var t1140 bool = parsed__110.hexadecimal
                                var t1141 bool = !t1140
                                var jp1131 bool
                                if t1141 {
                                    var t1142 int = parsed__110.decimal_exponent
                                    var t1143 bool = t1142 > 0
                                    jp1131 = t1143
                                } else {
                                    jp1131 = false
                                }
                                var jp1121 FloatNatural
                                if jp1131 {
                                    var t1132 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1132)
                                    var count__118 int = 0
                                    Loop_loop1134:
                                    for {
                                        var t1135 int = parsed__110.decimal_exponent
                                        var t1136 bool = count__118 < t1135
                                        if t1136 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1137 int = compound_old213 + compound_value214
                                            count__118 = t1137
                                            continue
                                        } else {
                                            break Loop_loop1134
                                        }
                                    }
                                    jp1121 = result__117
                                    var t1127 bool = parsed__110.hexadecimal
                                    var jp1123 int
                                    if t1127 {
                                        var t1128 int = parsed__110.binary_exponent
                                        jp1123 = t1128
                                    } else {
                                        var t1129 int = parsed__110.decimal_exponent
                                        jp1123 = t1129
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1121, jp1119, jp1123, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1124 bool = !x219
                                    var t1125 uint64 = jp1111 | x218
                                    var t1126 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1124,
                                        _1: t1125,
                                    }
                                    return t1126
                                } else {
                                    var t1139 FloatNatural = parsed__110.numerator
                                    jp1121 = t1139
                                    var t1127 bool = parsed__110.hexadecimal
                                    var jp1123 int
                                    if t1127 {
                                        var t1128 int = parsed__110.binary_exponent
                                        jp1123 = t1128
                                    } else {
                                        var t1129 int = parsed__110.decimal_exponent
                                        jp1123 = t1129
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1121, jp1119, jp1123, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1124 bool = !x219
                                    var t1125 uint64 = jp1111 | x218
                                    var t1126 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1124,
                                        _1: t1125,
                                    }
                                    return t1126
                                }
                            }
                        }
                    } else {
                        var t1150 bool = parsed__110.hexadecimal
                        var t1151 bool = !t1150
                        var jp1145 bool
                        if t1151 {
                            var t1152 int = parsed__110.decimal_exponent
                            var t1153 bool = t1152 < 0
                            jp1145 = t1153
                        } else {
                            jp1145 = false
                        }
                        var jp1119 FloatNatural
                        if jp1145 {
                            var t1146 int = parsed__110.decimal_exponent
                            var t1147 int = 0 - t1146
                            var t1148 FloatNatural = float_natural_power5(t1147)
                            jp1119 = t1148
                        } else {
                            var inline2151 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2151, 1)
                            var inline2153 FloatNatural = FloatNatural{
                                words: inline2151,
                            }
                            jp1119 = inline2153
                        }
                        var t1140 bool = parsed__110.hexadecimal
                        var t1141 bool = !t1140
                        var jp1131 bool
                        if t1141 {
                            var t1142 int = parsed__110.decimal_exponent
                            var t1143 bool = t1142 > 0
                            jp1131 = t1143
                        } else {
                            jp1131 = false
                        }
                        var jp1121 FloatNatural
                        if jp1131 {
                            var t1132 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1132)
                            var count__118 int = 0
                            Loop_loop1134__2:
                            for {
                                var t1135 int = parsed__110.decimal_exponent
                                var t1136 bool = count__118 < t1135
                                if t1136 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1137 int = compound_old213 + compound_value214
                                    count__118 = t1137
                                    continue
                                } else {
                                    break Loop_loop1134__2
                                }
                            }
                            jp1121 = result__117
                            var t1127 bool = parsed__110.hexadecimal
                            var jp1123 int
                            if t1127 {
                                var t1128 int = parsed__110.binary_exponent
                                jp1123 = t1128
                            } else {
                                var t1129 int = parsed__110.decimal_exponent
                                jp1123 = t1129
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1121, jp1119, jp1123, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1124 bool = !x219
                            var t1125 uint64 = jp1111 | x218
                            var t1126 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1124,
                                _1: t1125,
                            }
                            return t1126
                        } else {
                            var t1139 FloatNatural = parsed__110.numerator
                            jp1121 = t1139
                            var t1127 bool = parsed__110.hexadecimal
                            var jp1123 int
                            if t1127 {
                                var t1128 int = parsed__110.binary_exponent
                                jp1123 = t1128
                            } else {
                                var t1129 int = parsed__110.decimal_exponent
                                jp1123 = t1129
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1121, jp1119, jp1123, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1124 bool = !x219
                            var t1125 uint64 = jp1111 | x218
                            var t1126 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1124,
                                _1: t1125,
                            }
                            return t1126
                        }
                    }
                }
            }
        }
    }
}

func decimal_string(value__208 uint64) string {
    var t1225 bool = value__208 == 0
    if t1225 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1218:
        for {
            var t1219 bool = remaining__210 > 0
            if t1219 {
                var t1220_rhs uint64 = 10
                var t1220 uint64 = remaining__210 % t1220_rhs
                var t1221 uint8 = uint8(uint64(t1220))
                var t1222 uint8 = t1221 + 48
                vec_push__Vec_5uint8(reversed__209, t1222)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1223 uint64 = compound_old353 / compound_value354
                remaining__210 = t1223
                continue
            } else {
                break Loop_loop1218
            }
        }
        var t1207 int
        var inline2163 int = vec_len__Vec_5uint8(reversed__209)
        t1207 = inline2163
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1207)
        var offset__212 int = 0
        Loop_loop1209:
        for {
            var t1210 int
            var inline2161 int = vec_len__Vec_5uint8(reversed__209)
            t1210 = inline2161
            var t1211 bool = offset__212 < t1210
            if t1211 {
                var t1212 int
                var inline2159 int = vec_len__Vec_5uint8(reversed__209)
                t1212 = inline2159
                var t1213 int = t1212 - offset__212
                var t1214 int = t1213 - 1
                var t1215 uint8 = vec_get__Vec_5uint8(reversed__209, t1214)
                vec_push__Vec_5uint8(bytes__211, t1215)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1216 int = compound_old358 + compound_value359
                offset__212 = t1216
                continue
            } else {
                break Loop_loop1209
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline2169 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2170 FloatNatural = FloatNatural{
        words: inline2169,
    }
    result__2 = inline2170
    var t1229 bool = value__1 != 0
    if t1229 {
        var t1230 *_goml_vec_uint32 = result__2.words
        var t1231 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t1230, t1231)
        var t1232_rhs int = 32
        var t1232 uint64 = value__1 >> t1232_rhs
        var high__3 uint32 = uint32(uint64(t1232))
        var t1234 bool = high__3 != 0
        if t1234 {
            var t1235 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t1235, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1264 bool
    var inline2187 *_goml_vec_uint32 = value__28.words
    var inline2188 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2187)
    t1264 = inline2188
    if t1264 {
        var inline2172 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2173 FloatNatural = FloatNatural{
            words: inline2172,
        }
        return inline2173
    } else {
        var t1267 bool = bits__29 == 0
        if t1267 {
            var t1268 FloatNatural = float_natural_copy(value__28)
            return t1268
        } else {
            var result__30 FloatNatural
            var inline2184 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2185 FloatNatural = FloatNatural{
                words: inline2184,
            }
            result__30 = inline2185
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1259:
            for {
                var t1260 bool = index__33 < word_shift__31
                if t1260 {
                    var t1261 *_goml_vec_uint32 = result__30.words
                    var inline2175 uint32 = 0
                    vec_push__Vec_6uint32(t1261, inline2175)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1262 int = compound_old52 + compound_value53
                    index__33 = t1262
                    continue
                } else {
                    break Loop_loop1259
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop1247:
            for {
                var t1248 *_goml_vec_uint32 = value__28.words
                var t1249 int
                var inline2180 int = vec_len__Vec_6uint32(t1248)
                t1249 = inline2180
                var t1250 bool = index__33 < t1249
                if t1250 {
                    var t1251 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1251, index__33)
                    var t1252 uint64 = uint64(uint32(word__35))
                    var t1253 uint64 = t1252 << bit_shift__32
                    var shifted__36 uint64 = t1253 | carry__34
                    var t1254 *_goml_vec_uint32 = result__30.words
                    var t1255 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1254, t1255)
                    var t1256_rhs int = 32
                    var t1256 uint64 = shifted__36 >> t1256_rhs
                    carry__34 = t1256
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1257 int = compound_old59 + compound_value60
                    index__33 = t1257
                    continue
                } else {
                    break Loop_loop1247
                }
            }
            var t1243 bool = carry__34 != 0
            if t1243 {
                var t1244 *_goml_vec_uint32 = result__30.words
                var t1245 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t1244, t1245)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1291 bool
    var inline2203 *_goml_vec_uint32 = value__49.words
    var inline2204 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2203)
    t1291 = inline2204
    if t1291 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1284:
        for {
            var t1285 bool
            var inline2192 *_goml_vec_uint32 = current__50.words
            var inline2193 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2192)
            t1285 = inline2193
            var t1286 bool = !t1285
            if t1286 {
                var t1287 uint32 = float_natural_divide_small(current__50, 10)
                var t1288 uint8 = uint8(uint32(t1287))
                var t1289 uint8 = t1288 + 48
                vec_push__Vec_5uint8(reversed__51, t1289)
                continue
            } else {
                break Loop_loop1284
            }
        }
        var t1273 int
        var inline2201 int = vec_len__Vec_5uint8(reversed__51)
        t1273 = inline2201
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1273)
        var offset__53 int = 0
        Loop_loop1275:
        for {
            var t1276 int
            var inline2199 int = vec_len__Vec_5uint8(reversed__51)
            t1276 = inline2199
            var t1277 bool = offset__53 < t1276
            if t1277 {
                var t1278 int
                var inline2197 int = vec_len__Vec_5uint8(reversed__51)
                t1278 = inline2197
                var t1279 int = t1278 - offset__53
                var t1280 int = t1279 - 1
                var t1281 uint8 = vec_get__Vec_5uint8(reversed__51, t1280)
                vec_push__Vec_5uint8(output__52, t1281)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1282 int = compound_old98 + compound_value99
                offset__53 = t1282
                continue
            } else {
                break Loop_loop1275
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1294 int = _goml_runtime_core_string_len(self__289)
    return t1294
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1297 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1297)
    var index__148 int = 0
    Loop_loop1352:
    for {
        var t1353 bool = index__148 < count__146
        if t1353 {
            var t1354 uint8
            var inline2208 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1354 = inline2208
            vec_push__Vec_5uint8(output__147, t1354)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1355 int = compound_old267 + compound_value268
            index__148 = t1355
            continue
        } else {
            break Loop_loop1352
        }
    }
    var t1349 int
    var inline2229 int = _goml_runtime_core_string_len(exact__145)
    t1349 = inline2229
    var t1350 bool = count__146 == t1349
    if t1350 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1351 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1351
    } else {
        var next__150 uint8
        var inline2227 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2227
        var trailing__151 bool = false
        var t1300 int = count__146 + 1
        index__148 = t1300
        Loop_loop1341:
        for {
            var t1342 int
            var inline2212 int = _goml_runtime_core_string_len(exact__145)
            t1342 = inline2212
            var t1343 bool = index__148 < t1342
            if t1343 {
                var t1347 uint8
                var inline2210 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1347 = inline2210
                var t1348 bool = t1347 != 48
                if t1348 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1345 int = compound_old278 + compound_value279
                index__148 = t1345
                continue
            } else {
                break Loop_loop1341
            }
        }
        var t1329 bool = next__150 > 53
        var jp1303 bool
        if t1329 {
            jp1303 = true
        } else {
            var t1332 bool = next__150 == 53
            if t1332 {
                if trailing__151 {
                    jp1303 = true
                } else {
                    var t1335 int
                    var inline2214 int = vec_len__Vec_5uint8(output__147)
                    t1335 = inline2214
                    var t1336 int = t1335 - 1
                    var t1337 uint8 = vec_get__Vec_5uint8(output__147, t1336)
                    var t1338 uint8 = t1337 - 48
                    var t1339_rhs uint8 = 2
                    var t1339 uint8 = t1338 % t1339_rhs
                    var t1340 bool = t1339 == 1
                    jp1303 = t1340
                }
            } else {
                jp1303 = false
            }
        }
        if jp1303 {
            var index__153 int
            var inline2225 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2225
            Loop_loop1317:
            for {
                var t1318 bool = index__153 > 0
                if t1318 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1319 int = compound_old282 - compound_value283
                    index__153 = t1319
                    var t1322 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1323 bool = t1322 < 57
                    if t1323 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1324 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1324)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1326 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1326
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1317
                }
            }
            var t1307 int
            var inline2223 int = vec_len__Vec_5uint8(output__147)
            t1307 = inline2223
            var t1308 int = t1307 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1308)
            var inline2220 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2220)
            index__153 = 0
            Loop_loop1311:
            for {
                var t1312 int
                var inline2218 int = vec_len__Vec_5uint8(output__147)
                t1312 = inline2218
                var t1313 bool = index__153 < t1312
                if t1313 {
                    var t1314 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1314)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1315 int = compound_old302 + compound_value303
                    index__153 = t1315
                    continue
                } else {
                    break Loop_loop1311
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1310 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1310
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1328 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1328
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2236 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2236
    Loop_loop1361:
    for {
        var t1366 bool = length__159 > 1
        var jp1363 bool
        if t1366 {
            var t1367 int = length__159 - 1
            var t1368 uint8
            var inline2231 uint8 = _goml_runtime_core_string_byte_get(value__158, t1367)
            t1368 = inline2231
            var t1369 bool = t1368 == 48
            jp1363 = t1369
        } else {
            jp1363 = false
        }
        if jp1363 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1364 int = compound_old312 - compound_value313
            length__159 = t1364
            continue
        } else {
            break Loop_loop1361
        }
    }
    var inline2233 int = 0
    var inline2234 string = string_byte_slice(value__158, inline2233, length__159)
    return inline2234
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2238 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2238)
    } else {}
    var t1374 bool = decimal_point__138 <= 0
    if t1374 {
        var inline2253 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2253)
        var inline2250 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2250)
        var index__141 int = 0
        var t1384 int = 0 - decimal_point__138
        Loop_loop1383:
        for {
            var t1385 bool = index__141 < t1384
            if t1385 {
                var inline2241 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2241)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1386 int = compound_old234 + compound_value235
                index__141 = t1386
                continue
            } else {
                break Loop_loop1383
            }
        }
        index__141 = 0
        Loop_loop1377:
        for {
            var t1378 int
            var inline2248 int = _goml_runtime_core_string_len(digits__137)
            t1378 = inline2248
            var t1379 bool = index__141 < t1378
            if t1379 {
                var t1380 uint8
                var inline2246 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1380 = inline2246
                vec_push__Vec_5uint8(bytes__140, t1380)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1381 int = compound_old240 + compound_value241
                index__141 = t1381
                continue
            } else {
                break Loop_loop1377
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1389 int
        var inline2278 int = _goml_runtime_core_string_len(digits__137)
        t1389 = inline2278
        var t1390 bool = decimal_point__138 >= t1389
        if t1390 {
            var index__142 int = 0
            Loop_loop1397:
            for {
                var t1398 int
                var inline2260 int = _goml_runtime_core_string_len(digits__137)
                t1398 = inline2260
                var t1399 bool = index__142 < t1398
                if t1399 {
                    var t1400 uint8
                    var inline2258 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1400 = inline2258
                    vec_push__Vec_5uint8(bytes__140, t1400)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1401 int = compound_old244 + compound_value245
                    index__142 = t1401
                    continue
                } else {
                    break Loop_loop1397
                }
            }
            Loop_loop1393:
            for {
                var t1394 bool = index__142 < decimal_point__138
                if t1394 {
                    var inline2262 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2262)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1395 int = compound_old249 + compound_value250
                    index__142 = t1395
                    continue
                } else {
                    break Loop_loop1393
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1411:
            for {
                var t1412 bool = index__143 < decimal_point__138
                if t1412 {
                    var t1413 uint8
                    var inline2267 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1413 = inline2267
                    vec_push__Vec_5uint8(bytes__140, t1413)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1414 int = compound_old253 + compound_value254
                    index__143 = t1414
                    continue
                } else {
                    break Loop_loop1411
                }
            }
            var inline2275 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2275)
            Loop_loop1405:
            for {
                var t1406 int
                var inline2273 int = _goml_runtime_core_string_len(digits__137)
                t1406 = inline2273
                var t1407 bool = index__143 < t1406
                if t1407 {
                    var t1408 uint8
                    var inline2271 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1408 = inline2271
                    vec_push__Vec_5uint8(bytes__140, t1408)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1409 int = compound_old259 + compound_value260
                    index__143 = t1409
                    continue
                } else {
                    break Loop_loop1405
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1438 bool = factor__16 == 0
    if t1438 {
        var t1439 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1439, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1432 uint64 = uint64(uint32(factor__16))
        Loop_loop1425:
        for {
            var t1426 *_goml_vec_uint32 = value__15.words
            var t1427 int
            var inline2282 int = vec_len__Vec_6uint32(t1426)
            t1427 = inline2282
            var t1428 bool = index__18 < t1427
            if t1428 {
                var t1429 *_goml_vec_uint32 = value__15.words
                var t1430 uint32 = vec_get__Vec_6uint32(t1429, index__18)
                var t1431 uint64 = uint64(uint32(t1430))
                var t1433 uint64 = t1431 * t1432
                var product__19 uint64 = t1433 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1435_rhs int = 32
                var t1435 uint64 = product__19 >> t1435_rhs
                carry__17 = t1435
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1436 int = compound_old30 + compound_value31
                index__18 = t1436
                continue
            } else {
                break Loop_loop1425
            }
        }
        var t1421 bool = carry__17 != 0
        if t1421 {
            var t1422 *_goml_vec_uint32 = value__15.words
            var t1423 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1422, t1423)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1522 bool
    var inline2286 *_goml_vec_uint32 = numerator__65.words
    var inline2287 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2286)
    t1522 = inline2287
    if t1522 {
        var t1523 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1523
    } else {
        var t1519 bool = binary_shift__67 >= 0
        var jp1444 FloatNatural
        if t1519 {
            var t1520 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1444 = t1520
        } else {
            var t1521 FloatNatural = float_natural_copy(numerator__65)
            jp1444 = t1521
        }
        var t1515 bool = binary_shift__67 >= 0
        var jp1446 FloatNatural
        if t1515 {
            var t1516 FloatNatural = float_natural_copy(denominator__66)
            jp1446 = t1516
        } else {
            var t1517 int = 0 - binary_shift__67
            var t1518 FloatNatural = float_natural_shift_left(denominator__66, t1517)
            jp1446 = t1518
        }
        var t1447 int = float_natural_bit_length(jp1444)
        var t1448 int = float_natural_bit_length(jp1446)
        var exponent__72 int = t1447 - t1448
        var t1509 bool = exponent__72 >= 0
        var jp1450 int
        if t1509 {
            var t1510 FloatNatural = float_natural_shift_left(jp1446, exponent__72)
            var t1511 int = float_natural_compare(jp1444, t1510)
            jp1450 = t1511
        } else {
            var t1512 int = 0 - exponent__72
            var t1513 FloatNatural = float_natural_shift_left(jp1444, t1512)
            var t1514 int = float_natural_compare(t1513, jp1446)
            jp1450 = t1514
        }
        var t1506 bool = jp1450 < 0
        if t1506 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1507 int = compound_old120 - compound_value121
            exponent__72 = t1507
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1500 bool = exponent__72 > exponent_bias__69
        if t1500 {
            var t1501 int = exponent_bias__69 + exponent_bias__69
            var t1502 int = t1501 + 1
            var t1503 uint64 = uint64(int(t1502))
            var t1504 uint64 = t1503 << mantissa_bits__68
            var t1505 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1504,
                _1: true,
            }
            return t1505
        } else {
            var t1495 bool = exponent__72 < minimum_exponent__74
            var jp1454 uint64
            if t1495 {
                var t1496 int = mantissa_bits__68 - minimum_exponent__74
                var t1497 uint64 = float_rational_quotient(jp1444, jp1446, t1496)
                jp1454 = t1497
            } else {
                var t1498 int = mantissa_bits__68 - exponent__72
                var t1499 uint64 = float_rational_quotient(jp1444, jp1446, t1498)
                jp1454 = t1499
            }
            var mantissa__76 uint64 = jp1454
            var t1457 bool = exponent__72 < minimum_exponent__74
            if t1457 {
                var t1460 bool = mantissa__76 == 0
                if t1460 {
                    var t1461 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1461
                } else {
                    var t1464_lhs uint64 = 1
                    var t1464 uint64 = t1464_lhs << mantissa_bits__68
                    var t1465 bool = mantissa__76 >= t1464
                    if t1465 {
                        var t1466_lhs uint64 = 1
                        var t1466 uint64 = t1466_lhs << mantissa_bits__68
                        var t1467_lhs uint64 = 1
                        var t1467 uint64 = t1467_lhs << mantissa_bits__68
                        var t1468 uint64 = mantissa__76 - t1467
                        var t1469 uint64 = t1466 | t1468
                        var t1470 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1469,
                            _1: false,
                        }
                        return t1470
                    } else {
                        var t1471 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1471
                    }
                }
            } else {
                var t1488 int = mantissa_bits__68 + 1
                var t1489_lhs uint64 = 1
                var t1489 uint64 = t1489_lhs << t1488
                var t1490 bool = mantissa__76 >= t1489
                if t1490 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1491 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1491
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1493 int = compound_old128 + compound_value129
                    exponent__72 = t1493
                } else {}
                var t1475 bool = exponent__72 > exponent_bias__69
                if t1475 {
                    var t1476 int = exponent_bias__69 + exponent_bias__69
                    var t1477 int = t1476 + 1
                    var t1478 uint64 = uint64(int(t1477))
                    var t1479 uint64 = t1478 << mantissa_bits__68
                    var t1480 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1479,
                        _1: true,
                    }
                    return t1480
                } else {
                    var t1481 int = exponent__72 + exponent_bias__69
                    var t1482 uint64 = uint64(int(t1481))
                    var t1483 uint64 = t1482 << mantissa_bits__68
                    var t1484_lhs uint64 = 1
                    var t1484 uint64 = t1484_lhs << mantissa_bits__68
                    var t1485 uint64 = mantissa__76 - t1484
                    var t1486 uint64 = t1483 | t1485
                    var t1487 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1486,
                        _1: false,
                    }
                    return t1487
                }
            }
        }
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1711 bool = string_equals_ascii_case(value__84, "nan")
    if t1711 {
        var t1712 FloatNatural
        var inline2289 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2290 FloatNatural = FloatNatural{
            words: inline2289,
        }
        t1712 = inline2290
        var t1713 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1712,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1713
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1703 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1704 bool = index__85 < t1703
        var jp1698 bool
        if t1704 {
            var t1707 uint8
            var inline2294 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1707 = inline2294
            var t1708 bool = t1707 == 43
            if t1708 {
                jp1698 = true
            } else {
                var t1709 uint8
                var inline2292 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1709 = inline2292
                var t1710 bool = t1709 == 45
                jp1698 = t1710
            }
        } else {
            jp1698 = false
        }
        if jp1698 {
            var t1699 uint8
            var inline2296 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1699 = inline2296
            var t1700 bool = t1699 == 45
            negative__86 = t1700
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1701 int = compound_old140 + compound_value141
            index__85 = t1701
        } else {}
        var t1531 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1531)
        var t1695 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1692 bool
        if t1695 {
            jp1692 = true
        } else {
            var t1696 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1692 = t1696
        }
        if jp1692 {
            var t1693 FloatNatural
            var inline2298 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2299 FloatNatural = FloatNatural{
                words: inline2298,
            }
            t1693 = inline2299
            var t1694 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1693,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1694
        } else {
            var t1686 int = index__85 + 2
            var t1687 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1688 bool = t1686 <= t1687
            var jp1681 bool
            if t1688 {
                var t1689 uint8
                var inline2301 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1689 = inline2301
                var t1690 bool = t1689 == 48
                jp1681 = t1690
            } else {
                jp1681 = false
            }
            var jp1534 bool
            if jp1681 {
                var t1682 int = index__85 + 1
                var t1683 uint8
                var inline2310 uint8 = _goml_runtime_core_string_byte_get(value__84, t1682)
                t1683 = inline2310
                var t1684 uint8
                var inline2303 bool = t1683 >= 65
                var inline2305 bool
                if inline2303 {
                    var inline2308 bool = t1683 <= 90
                    inline2305 = inline2308
                } else {
                    inline2305 = false
                }
                if inline2305 {
                    var inline2306 uint8 = 97 - 65
                    var inline2307 uint8 = t1683 + inline2306
                    t1684 = inline2307
                    var t1685 bool = t1684 == 120
                    jp1534 = t1685
                    if jp1534 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1678 int = compound_old145 + compound_value146
                        index__85 = t1678
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1537 int
                    if jp1534 {
                        jp1537 = 16
                    } else {
                        jp1537 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1631 uint32 = uint32(int(jp1537))
                    Loop_loop1627:
                    for {
                        var t1628 int
                        var inline2324 int = _goml_runtime_core_string_len(value__84)
                        t1628 = inline2324
                        var t1629 bool = index__85 < t1628
                        if t1629 {
                            var current__97 uint8
                            var inline2322 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2322
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1537)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1631)
                                var t1632 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1632)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1643 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1643
                                } else {}
                                var t1641 bool = significant_digits__95 > 0
                                var jp1638 bool
                                if t1641 {
                                    jp1638 = true
                                } else {
                                    var t1642 bool = x151 != 0
                                    jp1638 = t1642
                                }
                                if jp1638 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1639 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1639
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1635 int = compound_old164 + compound_value165
                                index__85 = t1635
                                continue
                            } else {
                                var t1646 bool = current__97 == 95
                                if t1646 {
                                    var t1667 int = index__85 + 1
                                    var t1668 int
                                    var inline2320 int = _goml_runtime_core_string_len(value__84)
                                    t1668 = inline2320
                                    var t1669 bool = t1667 >= t1668
                                    if t1669 {
                                        var inline2312 FloatNatural = float_natural_zero()
                                        var inline2313 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2312,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2313
                                    } else {
                                        var t1648 int = index__85 + 1
                                        var t1649 uint8
                                        var inline2318 uint8 = _goml_runtime_core_string_byte_get(value__84, t1648)
                                        t1649 = inline2318
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1649, jp1537)
                                        var x169 bool = mtmp168._0
                                        var jp1664 bool
                                        if jp1534 {
                                            var t1666 bool = !saw_digit__92
                                            jp1664 = t1666
                                        } else {
                                            jp1664 = false
                                        }
                                        var jp1651 bool
                                        if jp1664 {
                                            var t1665 bool = index__85 == mantissa_start__89
                                            jp1651 = t1665
                                        } else {
                                            jp1651 = false
                                        }
                                        var t1661 bool = !previous_digit__96
                                        var jp1659 bool
                                        if t1661 {
                                            var t1662 bool = !jp1651
                                            jp1659 = t1662
                                        } else {
                                            jp1659 = false
                                        }
                                        var jp1656 bool
                                        if jp1659 {
                                            jp1656 = true
                                        } else {
                                            var t1660 bool = !x169
                                            jp1656 = t1660
                                        }
                                        if jp1656 {
                                            var inline2315 FloatNatural = float_natural_zero()
                                            var inline2316 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2315,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2316
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1653 int = compound_old173 + compound_value174
                                            index__85 = t1653
                                            continue
                                        }
                                    }
                                } else {
                                    var t1676 bool = current__97 == 46
                                    var jp1673 bool
                                    if t1676 {
                                        var t1677 bool = !saw_dot__93
                                        jp1673 = t1677
                                    } else {
                                        jp1673 = false
                                    }
                                    if jp1673 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1674 int = compound_old178 + compound_value179
                                        index__85 = t1674
                                        continue
                                    } else {
                                        break Loop_loop1627
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1627
                        }
                    }
                    var t1625 bool = !saw_digit__92
                    if t1625 {
                        var inline2326 FloatNatural = float_natural_zero()
                        var inline2327 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2326,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2327
                    } else {
                        var jp1541 uint8
                        if jp1534 {
                            jp1541 = 112
                        } else {
                            jp1541 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1620 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1621 bool = index__85 < t1620
                        var jp1558 bool
                        if t1621 {
                            var t1622 uint8
                            var inline2329 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1622 = inline2329
                            var t1623 uint8 = ascii_lower(t1622)
                            var t1624 bool = t1623 == jp1541
                            jp1558 = t1624
                        } else {
                            jp1558 = false
                        }
                        if jp1558 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1559 int = compound_old183 + compound_value184
                            index__85 = t1559
                            var t1610 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1611 bool = index__85 < t1610
                            var jp1605 bool
                            if t1611 {
                                var t1614 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1615 bool = t1614 == 43
                                if t1615 {
                                    jp1605 = true
                                } else {
                                    var t1616 uint8
                                    var inline2331 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1616 = inline2331
                                    var t1617 bool = t1616 == 45
                                    jp1605 = t1617
                                }
                            } else {
                                jp1605 = false
                            }
                            if jp1605 {
                                var t1606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1607 bool = t1606 == 45
                                exponent_negative__104 = t1607
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1608 int = compound_old187 + compound_value188
                                index__85 = t1608
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1566:
                            for {
                                var t1567 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1568 bool = index__85 < t1567
                                if t1568 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1602 bool = current__106 >= 48
                                    var jp1571 bool
                                    if t1602 {
                                        var t1603 bool = current__106 <= 57
                                        jp1571 = t1603
                                    } else {
                                        jp1571 = false
                                    }
                                    if jp1571 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1575 bool = exponent__103 < 1000000
                                        if t1575 {
                                            var t1576 int = exponent__103 * 10
                                            var t1577 uint8 = current__106 - 48
                                            var t1578 int = int(uint8(t1577))
                                            var t1579 int = t1576 + t1578
                                            exponent__103 = t1579
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1573 int = compound_old196 + compound_value197
                                        index__85 = t1573
                                        continue
                                    } else {
                                        var t1581 bool = current__106 == 95
                                        if t1581 {
                                            var t1598 bool = !previous_digit__96
                                            var jp1594 bool
                                            if t1598 {
                                                jp1594 = true
                                            } else {
                                                var t1599 int = index__85 + 1
                                                var t1600 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1601 bool = t1599 >= t1600
                                                jp1594 = t1601
                                            }
                                            var jp1589 bool
                                            if jp1594 {
                                                jp1589 = true
                                            } else {
                                                var t1595 int = index__85 + 1
                                                var t1596 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1595)
                                                var t1597 bool = t1596 < 48
                                                jp1589 = t1597
                                            }
                                            var jp1586 bool
                                            if jp1589 {
                                                jp1586 = true
                                            } else {
                                                var t1590 int = index__85 + 1
                                                var t1591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1590)
                                                var t1592 bool = t1591 > 57
                                                jp1586 = t1592
                                            }
                                            if jp1586 {
                                                var t1587 ParsedFloat = invalid_parsed_float()
                                                return t1587
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1583 int = compound_old201 + compound_value202
                                                index__85 = t1583
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1566
                                        }
                                    }
                                } else {
                                    break Loop_loop1566
                                }
                            }
                            var t1564 bool = !exponent_digits__105
                            if t1564 {
                                var t1565 ParsedFloat = invalid_parsed_float()
                                return t1565
                            } else {
                                var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1555 bool = index__85 != t1554
                                if t1555 {
                                    var t1556 ParsedFloat = invalid_parsed_float()
                                    return t1556
                                } else {
                                    if exponent_negative__104 {
                                        var t1553 int = 0 - exponent__103
                                        exponent__103 = t1553
                                    } else {}
                                    var jp1546 int
                                    if jp1534 {
                                        jp1546 = 0
                                    } else {
                                        var t1552 int = exponent__103 - fraction_digits__94
                                        jp1546 = t1552
                                    }
                                    var jp1548 int
                                    if jp1534 {
                                        var t1550 int = fraction_digits__94 * 4
                                        var t1551 int = exponent__103 - t1550
                                        jp1548 = t1551
                                    } else {
                                        jp1548 = 0
                                    }
                                    var t1549 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1546,
                                        binary_exponent: jp1548,
                                        hexadecimal: jp1534,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1549
                                }
                            }
                        } else {
                            if jp1534 {
                                var t1619 ParsedFloat = invalid_parsed_float()
                                return t1619
                            } else {
                                var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1555 bool = index__85 != t1554
                                if t1555 {
                                    var t1556 ParsedFloat = invalid_parsed_float()
                                    return t1556
                                } else {
                                    if exponent_negative__104 {
                                        var t1553 int = 0 - exponent__103
                                        exponent__103 = t1553
                                    } else {}
                                    var jp1546 int
                                    if jp1534 {
                                        jp1546 = 0
                                    } else {
                                        var t1552 int = exponent__103 - fraction_digits__94
                                        jp1546 = t1552
                                    }
                                    var jp1548 int
                                    if jp1534 {
                                        var t1550 int = fraction_digits__94 * 4
                                        var t1551 int = exponent__103 - t1550
                                        jp1548 = t1551
                                    } else {
                                        jp1548 = 0
                                    }
                                    var t1549 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1546,
                                        binary_exponent: jp1548,
                                        hexadecimal: jp1534,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1549
                                }
                            }
                        }
                    }
                } else {
                    t1684 = t1683
                    var t1685 bool = t1684 == 120
                    jp1534 = t1685
                    if jp1534 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1678 int = compound_old145 + compound_value146
                        index__85 = t1678
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1537 int
                    if jp1534 {
                        jp1537 = 16
                    } else {
                        jp1537 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1631 uint32 = uint32(int(jp1537))
                    Loop_loop1627__2:
                    for {
                        var t1628 int
                        var inline2324 int = _goml_runtime_core_string_len(value__84)
                        t1628 = inline2324
                        var t1629 bool = index__85 < t1628
                        if t1629 {
                            var current__97 uint8
                            var inline2322 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2322
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1537)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1631)
                                var t1632 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1632)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1643 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1643
                                } else {}
                                var t1641 bool = significant_digits__95 > 0
                                var jp1638 bool
                                if t1641 {
                                    jp1638 = true
                                } else {
                                    var t1642 bool = x151 != 0
                                    jp1638 = t1642
                                }
                                if jp1638 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1639 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1639
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1635 int = compound_old164 + compound_value165
                                index__85 = t1635
                                continue
                            } else {
                                var t1646 bool = current__97 == 95
                                if t1646 {
                                    var t1667 int = index__85 + 1
                                    var t1668 int
                                    var inline2320 int = _goml_runtime_core_string_len(value__84)
                                    t1668 = inline2320
                                    var t1669 bool = t1667 >= t1668
                                    if t1669 {
                                        var inline2312 FloatNatural = float_natural_zero()
                                        var inline2313 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2312,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2313
                                    } else {
                                        var t1648 int = index__85 + 1
                                        var t1649 uint8
                                        var inline2318 uint8 = _goml_runtime_core_string_byte_get(value__84, t1648)
                                        t1649 = inline2318
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1649, jp1537)
                                        var x169 bool = mtmp168._0
                                        var jp1664 bool
                                        if jp1534 {
                                            var t1666 bool = !saw_digit__92
                                            jp1664 = t1666
                                        } else {
                                            jp1664 = false
                                        }
                                        var jp1651 bool
                                        if jp1664 {
                                            var t1665 bool = index__85 == mantissa_start__89
                                            jp1651 = t1665
                                        } else {
                                            jp1651 = false
                                        }
                                        var t1661 bool = !previous_digit__96
                                        var jp1659 bool
                                        if t1661 {
                                            var t1662 bool = !jp1651
                                            jp1659 = t1662
                                        } else {
                                            jp1659 = false
                                        }
                                        var jp1656 bool
                                        if jp1659 {
                                            jp1656 = true
                                        } else {
                                            var t1660 bool = !x169
                                            jp1656 = t1660
                                        }
                                        if jp1656 {
                                            var inline2315 FloatNatural = float_natural_zero()
                                            var inline2316 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2315,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2316
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1653 int = compound_old173 + compound_value174
                                            index__85 = t1653
                                            continue
                                        }
                                    }
                                } else {
                                    var t1676 bool = current__97 == 46
                                    var jp1673 bool
                                    if t1676 {
                                        var t1677 bool = !saw_dot__93
                                        jp1673 = t1677
                                    } else {
                                        jp1673 = false
                                    }
                                    if jp1673 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1674 int = compound_old178 + compound_value179
                                        index__85 = t1674
                                        continue
                                    } else {
                                        break Loop_loop1627__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1627__2
                        }
                    }
                    var t1625 bool = !saw_digit__92
                    if t1625 {
                        var inline2326 FloatNatural = float_natural_zero()
                        var inline2327 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2326,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2327
                    } else {
                        var jp1541 uint8
                        if jp1534 {
                            jp1541 = 112
                        } else {
                            jp1541 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1620 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1621 bool = index__85 < t1620
                        var jp1558 bool
                        if t1621 {
                            var t1622 uint8
                            var inline2329 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1622 = inline2329
                            var t1623 uint8 = ascii_lower(t1622)
                            var t1624 bool = t1623 == jp1541
                            jp1558 = t1624
                        } else {
                            jp1558 = false
                        }
                        if jp1558 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1559 int = compound_old183 + compound_value184
                            index__85 = t1559
                            var t1610 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1611 bool = index__85 < t1610
                            var jp1605 bool
                            if t1611 {
                                var t1614 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1615 bool = t1614 == 43
                                if t1615 {
                                    jp1605 = true
                                } else {
                                    var t1616 uint8
                                    var inline2331 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1616 = inline2331
                                    var t1617 bool = t1616 == 45
                                    jp1605 = t1617
                                }
                            } else {
                                jp1605 = false
                            }
                            if jp1605 {
                                var t1606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1607 bool = t1606 == 45
                                exponent_negative__104 = t1607
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1608 int = compound_old187 + compound_value188
                                index__85 = t1608
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1566__2:
                            for {
                                var t1567 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1568 bool = index__85 < t1567
                                if t1568 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1602 bool = current__106 >= 48
                                    var jp1571 bool
                                    if t1602 {
                                        var t1603 bool = current__106 <= 57
                                        jp1571 = t1603
                                    } else {
                                        jp1571 = false
                                    }
                                    if jp1571 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1575 bool = exponent__103 < 1000000
                                        if t1575 {
                                            var t1576 int = exponent__103 * 10
                                            var t1577 uint8 = current__106 - 48
                                            var t1578 int = int(uint8(t1577))
                                            var t1579 int = t1576 + t1578
                                            exponent__103 = t1579
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1573 int = compound_old196 + compound_value197
                                        index__85 = t1573
                                        continue
                                    } else {
                                        var t1581 bool = current__106 == 95
                                        if t1581 {
                                            var t1598 bool = !previous_digit__96
                                            var jp1594 bool
                                            if t1598 {
                                                jp1594 = true
                                            } else {
                                                var t1599 int = index__85 + 1
                                                var t1600 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1601 bool = t1599 >= t1600
                                                jp1594 = t1601
                                            }
                                            var jp1589 bool
                                            if jp1594 {
                                                jp1589 = true
                                            } else {
                                                var t1595 int = index__85 + 1
                                                var t1596 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1595)
                                                var t1597 bool = t1596 < 48
                                                jp1589 = t1597
                                            }
                                            var jp1586 bool
                                            if jp1589 {
                                                jp1586 = true
                                            } else {
                                                var t1590 int = index__85 + 1
                                                var t1591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1590)
                                                var t1592 bool = t1591 > 57
                                                jp1586 = t1592
                                            }
                                            if jp1586 {
                                                var t1587 ParsedFloat = invalid_parsed_float()
                                                return t1587
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1583 int = compound_old201 + compound_value202
                                                index__85 = t1583
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1566__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1566__2
                                }
                            }
                            var t1564 bool = !exponent_digits__105
                            if t1564 {
                                var t1565 ParsedFloat = invalid_parsed_float()
                                return t1565
                            } else {
                                var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1555 bool = index__85 != t1554
                                if t1555 {
                                    var t1556 ParsedFloat = invalid_parsed_float()
                                    return t1556
                                } else {
                                    if exponent_negative__104 {
                                        var t1553 int = 0 - exponent__103
                                        exponent__103 = t1553
                                    } else {}
                                    var jp1546 int
                                    if jp1534 {
                                        jp1546 = 0
                                    } else {
                                        var t1552 int = exponent__103 - fraction_digits__94
                                        jp1546 = t1552
                                    }
                                    var jp1548 int
                                    if jp1534 {
                                        var t1550 int = fraction_digits__94 * 4
                                        var t1551 int = exponent__103 - t1550
                                        jp1548 = t1551
                                    } else {
                                        jp1548 = 0
                                    }
                                    var t1549 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1546,
                                        binary_exponent: jp1548,
                                        hexadecimal: jp1534,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1549
                                }
                            }
                        } else {
                            if jp1534 {
                                var t1619 ParsedFloat = invalid_parsed_float()
                                return t1619
                            } else {
                                var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1555 bool = index__85 != t1554
                                if t1555 {
                                    var t1556 ParsedFloat = invalid_parsed_float()
                                    return t1556
                                } else {
                                    if exponent_negative__104 {
                                        var t1553 int = 0 - exponent__103
                                        exponent__103 = t1553
                                    } else {}
                                    var jp1546 int
                                    if jp1534 {
                                        jp1546 = 0
                                    } else {
                                        var t1552 int = exponent__103 - fraction_digits__94
                                        jp1546 = t1552
                                    }
                                    var jp1548 int
                                    if jp1534 {
                                        var t1550 int = fraction_digits__94 * 4
                                        var t1551 int = exponent__103 - t1550
                                        jp1548 = t1551
                                    } else {
                                        jp1548 = 0
                                    }
                                    var t1549 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1546,
                                        binary_exponent: jp1548,
                                        hexadecimal: jp1534,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1549
                                }
                            }
                        }
                    }
                }
            } else {
                jp1534 = false
                if jp1534 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1678 int = compound_old145 + compound_value146
                    index__85 = t1678
                } else {}
                var mantissa_start__89 int = index__85
                var jp1537 int
                if jp1534 {
                    jp1537 = 16
                } else {
                    jp1537 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1631 uint32 = uint32(int(jp1537))
                Loop_loop1627__3:
                for {
                    var t1628 int
                    var inline2324 int = _goml_runtime_core_string_len(value__84)
                    t1628 = inline2324
                    var t1629 bool = index__85 < t1628
                    if t1629 {
                        var current__97 uint8
                        var inline2322 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2322
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1537)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1631)
                            var t1632 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1632)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1643 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1643
                            } else {}
                            var t1641 bool = significant_digits__95 > 0
                            var jp1638 bool
                            if t1641 {
                                jp1638 = true
                            } else {
                                var t1642 bool = x151 != 0
                                jp1638 = t1642
                            }
                            if jp1638 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1639 int = compound_old160 + compound_value161
                                significant_digits__95 = t1639
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1635 int = compound_old164 + compound_value165
                            index__85 = t1635
                            continue
                        } else {
                            var t1646 bool = current__97 == 95
                            if t1646 {
                                var t1667 int = index__85 + 1
                                var t1668 int
                                var inline2320 int = _goml_runtime_core_string_len(value__84)
                                t1668 = inline2320
                                var t1669 bool = t1667 >= t1668
                                if t1669 {
                                    var inline2312 FloatNatural = float_natural_zero()
                                    var inline2313 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2312,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2313
                                } else {
                                    var t1648 int = index__85 + 1
                                    var t1649 uint8
                                    var inline2318 uint8 = _goml_runtime_core_string_byte_get(value__84, t1648)
                                    t1649 = inline2318
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1649, jp1537)
                                    var x169 bool = mtmp168._0
                                    var jp1664 bool
                                    if jp1534 {
                                        var t1666 bool = !saw_digit__92
                                        jp1664 = t1666
                                    } else {
                                        jp1664 = false
                                    }
                                    var jp1651 bool
                                    if jp1664 {
                                        var t1665 bool = index__85 == mantissa_start__89
                                        jp1651 = t1665
                                    } else {
                                        jp1651 = false
                                    }
                                    var t1661 bool = !previous_digit__96
                                    var jp1659 bool
                                    if t1661 {
                                        var t1662 bool = !jp1651
                                        jp1659 = t1662
                                    } else {
                                        jp1659 = false
                                    }
                                    var jp1656 bool
                                    if jp1659 {
                                        jp1656 = true
                                    } else {
                                        var t1660 bool = !x169
                                        jp1656 = t1660
                                    }
                                    if jp1656 {
                                        var inline2315 FloatNatural = float_natural_zero()
                                        var inline2316 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2315,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2316
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1653 int = compound_old173 + compound_value174
                                        index__85 = t1653
                                        continue
                                    }
                                }
                            } else {
                                var t1676 bool = current__97 == 46
                                var jp1673 bool
                                if t1676 {
                                    var t1677 bool = !saw_dot__93
                                    jp1673 = t1677
                                } else {
                                    jp1673 = false
                                }
                                if jp1673 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1674 int = compound_old178 + compound_value179
                                    index__85 = t1674
                                    continue
                                } else {
                                    break Loop_loop1627__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1627__3
                    }
                }
                var t1625 bool = !saw_digit__92
                if t1625 {
                    var inline2326 FloatNatural = float_natural_zero()
                    var inline2327 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2326,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2327
                } else {
                    var jp1541 uint8
                    if jp1534 {
                        jp1541 = 112
                    } else {
                        jp1541 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1620 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1621 bool = index__85 < t1620
                    var jp1558 bool
                    if t1621 {
                        var t1622 uint8
                        var inline2329 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1622 = inline2329
                        var t1623 uint8 = ascii_lower(t1622)
                        var t1624 bool = t1623 == jp1541
                        jp1558 = t1624
                    } else {
                        jp1558 = false
                    }
                    if jp1558 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1559 int = compound_old183 + compound_value184
                        index__85 = t1559
                        var t1610 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1611 bool = index__85 < t1610
                        var jp1605 bool
                        if t1611 {
                            var t1614 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1615 bool = t1614 == 43
                            if t1615 {
                                jp1605 = true
                            } else {
                                var t1616 uint8
                                var inline2331 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1616 = inline2331
                                var t1617 bool = t1616 == 45
                                jp1605 = t1617
                            }
                        } else {
                            jp1605 = false
                        }
                        if jp1605 {
                            var t1606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1607 bool = t1606 == 45
                            exponent_negative__104 = t1607
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1608 int = compound_old187 + compound_value188
                            index__85 = t1608
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1566__3:
                        for {
                            var t1567 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1568 bool = index__85 < t1567
                            if t1568 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1602 bool = current__106 >= 48
                                var jp1571 bool
                                if t1602 {
                                    var t1603 bool = current__106 <= 57
                                    jp1571 = t1603
                                } else {
                                    jp1571 = false
                                }
                                if jp1571 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1575 bool = exponent__103 < 1000000
                                    if t1575 {
                                        var t1576 int = exponent__103 * 10
                                        var t1577 uint8 = current__106 - 48
                                        var t1578 int = int(uint8(t1577))
                                        var t1579 int = t1576 + t1578
                                        exponent__103 = t1579
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1573 int = compound_old196 + compound_value197
                                    index__85 = t1573
                                    continue
                                } else {
                                    var t1581 bool = current__106 == 95
                                    if t1581 {
                                        var t1598 bool = !previous_digit__96
                                        var jp1594 bool
                                        if t1598 {
                                            jp1594 = true
                                        } else {
                                            var t1599 int = index__85 + 1
                                            var t1600 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1601 bool = t1599 >= t1600
                                            jp1594 = t1601
                                        }
                                        var jp1589 bool
                                        if jp1594 {
                                            jp1589 = true
                                        } else {
                                            var t1595 int = index__85 + 1
                                            var t1596 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1595)
                                            var t1597 bool = t1596 < 48
                                            jp1589 = t1597
                                        }
                                        var jp1586 bool
                                        if jp1589 {
                                            jp1586 = true
                                        } else {
                                            var t1590 int = index__85 + 1
                                            var t1591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1590)
                                            var t1592 bool = t1591 > 57
                                            jp1586 = t1592
                                        }
                                        if jp1586 {
                                            var t1587 ParsedFloat = invalid_parsed_float()
                                            return t1587
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1583 int = compound_old201 + compound_value202
                                            index__85 = t1583
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1566__3
                                    }
                                }
                            } else {
                                break Loop_loop1566__3
                            }
                        }
                        var t1564 bool = !exponent_digits__105
                        if t1564 {
                            var t1565 ParsedFloat = invalid_parsed_float()
                            return t1565
                        } else {
                            var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1555 bool = index__85 != t1554
                            if t1555 {
                                var t1556 ParsedFloat = invalid_parsed_float()
                                return t1556
                            } else {
                                if exponent_negative__104 {
                                    var t1553 int = 0 - exponent__103
                                    exponent__103 = t1553
                                } else {}
                                var jp1546 int
                                if jp1534 {
                                    jp1546 = 0
                                } else {
                                    var t1552 int = exponent__103 - fraction_digits__94
                                    jp1546 = t1552
                                }
                                var jp1548 int
                                if jp1534 {
                                    var t1550 int = fraction_digits__94 * 4
                                    var t1551 int = exponent__103 - t1550
                                    jp1548 = t1551
                                } else {
                                    jp1548 = 0
                                }
                                var t1549 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1546,
                                    binary_exponent: jp1548,
                                    hexadecimal: jp1534,
                                    significant_digits: significant_digits__95,
                                }
                                return t1549
                            }
                        }
                    } else {
                        if jp1534 {
                            var t1619 ParsedFloat = invalid_parsed_float()
                            return t1619
                        } else {
                            var t1554 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1555 bool = index__85 != t1554
                            if t1555 {
                                var t1556 ParsedFloat = invalid_parsed_float()
                                return t1556
                            } else {
                                if exponent_negative__104 {
                                    var t1553 int = 0 - exponent__103
                                    exponent__103 = t1553
                                } else {}
                                var jp1546 int
                                if jp1534 {
                                    jp1546 = 0
                                } else {
                                    var t1552 int = exponent__103 - fraction_digits__94
                                    jp1546 = t1552
                                }
                                var jp1548 int
                                if jp1534 {
                                    var t1550 int = fraction_digits__94 * 4
                                    var t1551 int = exponent__103 - t1550
                                    jp1548 = t1551
                                } else {
                                    jp1548 = 0
                                }
                                var t1549 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1546,
                                    binary_exponent: jp1548,
                                    hexadecimal: jp1534,
                                    significant_digits: significant_digits__95,
                                }
                                return t1549
                            }
                        }
                    }
                }
            }
        }
    }
}

func float_natural_power5(exponent__25 int) FloatNatural {
    var result__26 FloatNatural
    var inline2336 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2336, 1)
    var inline2338 FloatNatural = FloatNatural{
        words: inline2336,
    }
    result__26 = inline2338
    var count__27 int = 0
    Loop_loop1721:
    for {
        var t1722 bool = count__27 < exponent__25
        if t1722 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1723 int = compound_old46 + compound_value47
            count__27 = t1723
            continue
        } else {
            break Loop_loop1721
        }
    }
    return result__26
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2344 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2345 FloatNatural = FloatNatural{
        words: inline2344,
    }
    result__5 = inline2345
    var index__6 int = 0
    Loop_loop1728:
    for {
        var t1729 *_goml_vec_uint32 = value__4.words
        var t1730 int
        var inline2342 int = vec_len__Vec_6uint32(t1729)
        t1730 = inline2342
        var t1731 bool = index__6 < t1730
        if t1731 {
            var t1732 *_goml_vec_uint32 = result__5.words
            var t1733 *_goml_vec_uint32 = value__4.words
            var t1734 uint32 = vec_get__Vec_6uint32(t1733, index__6)
            vec_push__Vec_6uint32(t1732, t1734)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1735 int = compound_old4 + compound_value5
            index__6 = t1735
            continue
        } else {
            break Loop_loop1728
        }
    }
    return result__5
}

func float_natural_zero() FloatNatural {
    var t1744 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1745 FloatNatural = FloatNatural{
        words: t1744,
    }
    return t1745
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1753 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2347 int = vec_len__Vec_6uint32(t1753)
    index__47 = inline2347
    var t1764 uint64 = uint64(uint32(divisor__45))
    var t1767 uint64 = uint64(uint32(divisor__45))
    Loop_loop1756:
    for {
        var t1757 bool = index__47 > 0
        if t1757 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1758 int = compound_old83 - compound_value84
            index__47 = t1758
            var t1760_rhs int = 32
            var t1760 uint64 = remainder__46 << t1760_rhs
            var t1761 *_goml_vec_uint32 = value__44.words
            var t1762 uint32 = vec_get__Vec_6uint32(t1761, index__47)
            var t1763 uint64 = uint64(uint32(t1762))
            var current__48 uint64 = t1760 | t1763
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1765 uint64 = current__48 / t1764
            var value90 uint32 = uint32(uint64(t1765))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1768 uint64 = current__48 % t1767
            remainder__46 = t1768
            continue
        } else {
            break Loop_loop1756
        }
    }
    float_natural_trim(value__44)
    var t1755 uint32 = uint32(uint64(remainder__46))
    return t1755
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1771 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1771
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2349 bool = string_is_char_boundary(self__294, start__295)
    var inline2351 bool
    if inline2349 {
        var inline2354 bool = string_is_char_boundary(self__294, end__296)
        inline2351 = inline2354
    } else {
        inline2351 = false
    }
    if inline2351 {
        var inline2352 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2352
    } else {
        var inline2353 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2353
    }
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1796 *_goml_vec_uint32 = value__9.words
    var t1797 bool
    var inline2363 int = vec_len__Vec_6uint32(t1796)
    var inline2364 bool = inline2363 == 0
    t1797 = inline2364
    if t1797 {
        return 0
    } else {
        var t1780 *_goml_vec_uint32 = value__9.words
        var t1781 *_goml_vec_uint32 = value__9.words
        var t1782 int
        var inline2361 int = vec_len__Vec_6uint32(t1781)
        t1782 = inline2361
        var t1783 int = t1782 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1780, t1783)
        var bits__11 int = 0
        Loop_loop1790:
        for {
            var t1791 bool = high__10 != 0
            if t1791 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1792 uint32 = compound_old9 >> compound_value10
                high__10 = t1792
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1794 int = compound_old12 + compound_value13
                bits__11 = t1794
                continue
            } else {
                break Loop_loop1790
            }
        }
        var t1785 *_goml_vec_uint32 = value__9.words
        var t1786 int
        var inline2359 int = vec_len__Vec_6uint32(t1785)
        t1786 = inline2359
        var t1787 int = t1786 - 1
        var t1788 int = t1787 * 32
        var t1789 int = t1788 + bits__11
        return t1789
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1819 *_goml_vec_uint32 = left__12.words
    var t1820 int
    var inline2374 int = vec_len__Vec_6uint32(t1819)
    t1820 = inline2374
    var t1821 *_goml_vec_uint32 = right__13.words
    var t1822 int
    var inline2372 int = vec_len__Vec_6uint32(t1821)
    t1822 = inline2372
    var t1823 bool = t1820 < t1822
    if t1823 {
        return -1
    } else {
        var t1825 *_goml_vec_uint32 = left__12.words
        var t1826 int
        var inline2368 int = vec_len__Vec_6uint32(t1825)
        t1826 = inline2368
        var t1827 *_goml_vec_uint32 = right__13.words
        var t1828 int
        var inline2366 int = vec_len__Vec_6uint32(t1827)
        t1828 = inline2366
        var t1829 bool = t1826 > t1828
        if t1829 {
            return 1
        } else {
            var t1801 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2370 int = vec_len__Vec_6uint32(t1801)
            index__14 = inline2370
            Loop_loop1803:
            for {
                var t1804 bool = index__14 > 0
                if t1804 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1805 int = compound_old17 - compound_value18
                    index__14 = t1805
                    var t1808 *_goml_vec_uint32 = left__12.words
                    var t1809 uint32 = vec_get__Vec_6uint32(t1808, index__14)
                    var t1810 *_goml_vec_uint32 = right__13.words
                    var t1811 uint32 = vec_get__Vec_6uint32(t1810, index__14)
                    var t1812 bool = t1809 < t1811
                    if t1812 {
                        return -1
                    } else {
                        var t1814 *_goml_vec_uint32 = left__12.words
                        var t1815 uint32 = vec_get__Vec_6uint32(t1814, index__14)
                        var t1816 *_goml_vec_uint32 = right__13.words
                        var t1817 uint32 = vec_get__Vec_6uint32(t1816, index__14)
                        var t1818 bool = t1815 > t1817
                        if t1818 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1803
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1865 bool = shift__57 >= 0
    var jp1833 FloatNatural
    if t1865 {
        var t1866 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1833 = t1866
    } else {
        var t1867 FloatNatural = float_natural_copy(numerator__55)
        jp1833 = t1867
    }
    var t1861 bool = shift__57 >= 0
    var jp1835 FloatNatural
    if t1861 {
        var t1862 FloatNatural = float_natural_copy(denominator__56)
        jp1835 = t1862
    } else {
        var t1863 int = 0 - shift__57
        var t1864 FloatNatural = float_natural_shift_left(denominator__56, t1863)
        jp1835 = t1864
    }
    var quotient__60 uint64 = 0
    Loop_loop1848:
    for {
        var t1849 int = float_natural_compare(jp1833, jp1835)
        var t1850 bool = t1849 >= 0
        if t1850 {
            var t1851 int = float_natural_bit_length(jp1833)
            var t1852 int = float_natural_bit_length(jp1835)
            var offset__61 int = t1851 - t1852
            var part__62 FloatNatural = float_natural_shift_left(jp1835, offset__61)
            var t1856 int = float_natural_compare(jp1833, part__62)
            var t1857 bool = t1856 < 0
            if t1857 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1858 int = compound_old105 - compound_value106
                offset__61 = t1858
                var t1860 FloatNatural = float_natural_shift_left(jp1835, offset__61)
                part__62 = t1860
            } else {}
            float_natural_subtract(jp1833, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1854 uint64 = compound_old111 | compound_value112
            quotient__60 = t1854
            continue
        } else {
            break Loop_loop1848
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1833, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1835)
    var t1842 bool = rounding__64 > 0
    var jp1839 bool
    if t1842 {
        jp1839 = true
    } else {
        var t1845 bool = rounding__64 == 0
        if t1845 {
            var t1846_rhs uint64 = 1
            var t1846 uint64 = quotient__60 & t1846_rhs
            var t1847 bool = t1846 == 1
            jp1839 = t1847
        } else {
            jp1839 = false
        }
    }
    if jp1839 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1840 uint64 = compound_old115 + compound_value116
        quotient__60 = t1840
    } else {}
    return quotient__60
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1882 int
    var inline2391 int = _goml_runtime_core_string_len(value__78)
    t1882 = inline2391
    var t1883 int
    var inline2389 int = _goml_runtime_core_string_len(expected__79)
    t1883 = inline2389
    var t1884 bool = t1882 != t1883
    if t1884 {
        return false
    } else {
        var index__80 int = 0
        var inline2381 uint8 = 97 - 65
        Loop_loop1872:
        for {
            var t1873 int
            var inline2387 int = _goml_runtime_core_string_len(value__78)
            t1873 = inline2387
            var t1874 bool = index__80 < t1873
            if t1874 {
                var t1878 uint8
                var inline2385 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1878 = inline2385
                var t1879 uint8
                var inline2378 bool = t1878 >= 65
                var inline2380 bool
                if inline2378 {
                    var inline2383 bool = t1878 <= 90
                    inline2380 = inline2383
                } else {
                    inline2380 = false
                }
                if inline2380 {
                    var inline2382 uint8 = t1878 + inline2381
                    t1879 = inline2382
                    var t1880 uint8
                    var inline2376 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1880 = inline2376
                    var t1881 bool = t1879 != t1880
                    if t1881 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1876 int = compound_old134 + compound_value135
                        index__80 = t1876
                        continue
                    }
                } else {
                    t1879 = t1878
                    var t1880 uint8
                    var inline2376 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1880 = inline2376
                    var t1881 bool = t1879 != t1880
                    if t1881 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1876 int = compound_old134 + compound_value135
                        index__80 = t1876
                        continue
                    }
                }
            } else {
                break Loop_loop1872
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1893 bool = value__77 >= 65
    var jp1890 bool
    if t1893 {
        var t1894 bool = value__77 <= 90
        jp1890 = t1894
    } else {
        jp1890 = false
    }
    if jp1890 {
        var t1891 uint8 = 97 - 65
        var t1892 uint8 = value__77 + t1891
        return t1892
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1921 bool = value__81 >= 48
    var jp1905 bool
    if t1921 {
        var t1922 bool = value__81 <= 57
        jp1905 = t1922
    } else {
        jp1905 = false
    }
    var jp1898 int
    if jp1905 {
        var t1906 uint8 = value__81 - 48
        var t1907 int = int(uint8(t1906))
        jp1898 = t1907
        var t1901 bool = jp1898 < base__82
        if t1901 {
            var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1898,
            }
            return t1902
        } else {
            var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1903
        }
    } else {
        var t1917 uint8
        var inline2407 bool = value__81 >= 65
        var inline2409 bool
        if inline2407 {
            var inline2412 bool = value__81 <= 90
            inline2409 = inline2412
        } else {
            inline2409 = false
        }
        if inline2409 {
            var inline2410 uint8 = 97 - 65
            var inline2411 uint8 = value__81 + inline2410
            t1917 = inline2411
            var t1918 bool = t1917 >= 97
            var jp1911 bool
            if t1918 {
                var t1919 uint8
                var inline2393 bool = value__81 >= 65
                var inline2395 bool
                if inline2393 {
                    var inline2398 bool = value__81 <= 90
                    inline2395 = inline2398
                } else {
                    inline2395 = false
                }
                if inline2395 {
                    var inline2396 uint8 = 97 - 65
                    var inline2397 uint8 = value__81 + inline2396
                    t1919 = inline2397
                    var t1920 bool = t1919 <= 102
                    jp1911 = t1920
                    if jp1911 {
                        var t1912 uint8
                        var inline2400 bool = value__81 >= 65
                        var inline2402 bool
                        if inline2400 {
                            var inline2405 bool = value__81 <= 90
                            inline2402 = inline2405
                        } else {
                            inline2402 = false
                        }
                        if inline2402 {
                            var inline2403 uint8 = 97 - 65
                            var inline2404 uint8 = value__81 + inline2403
                            t1912 = inline2404
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        } else {
                            t1912 = value__81
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        }
                    } else {
                        var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1916
                    }
                } else {
                    t1919 = value__81
                    var t1920 bool = t1919 <= 102
                    jp1911 = t1920
                    if jp1911 {
                        var t1912 uint8
                        var inline2400 bool = value__81 >= 65
                        var inline2402 bool
                        if inline2400 {
                            var inline2405 bool = value__81 <= 90
                            inline2402 = inline2405
                        } else {
                            inline2402 = false
                        }
                        if inline2402 {
                            var inline2403 uint8 = 97 - 65
                            var inline2404 uint8 = value__81 + inline2403
                            t1912 = inline2404
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        } else {
                            t1912 = value__81
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        }
                    } else {
                        var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1916
                    }
                }
            } else {
                jp1911 = false
                if jp1911 {
                    var t1912 uint8
                    var inline2400 bool = value__81 >= 65
                    var inline2402 bool
                    if inline2400 {
                        var inline2405 bool = value__81 <= 90
                        inline2402 = inline2405
                    } else {
                        inline2402 = false
                    }
                    if inline2402 {
                        var inline2403 uint8 = 97 - 65
                        var inline2404 uint8 = value__81 + inline2403
                        t1912 = inline2404
                        var t1913 uint8 = t1912 - 97
                        var t1914 uint8 = t1913 + 10
                        var t1915 int = int(uint8(t1914))
                        jp1898 = t1915
                        var t1901 bool = jp1898 < base__82
                        if t1901 {
                            var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1898,
                            }
                            return t1902
                        } else {
                            var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1903
                        }
                    } else {
                        t1912 = value__81
                        var t1913 uint8 = t1912 - 97
                        var t1914 uint8 = t1913 + 10
                        var t1915 int = int(uint8(t1914))
                        jp1898 = t1915
                        var t1901 bool = jp1898 < base__82
                        if t1901 {
                            var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1898,
                            }
                            return t1902
                        } else {
                            var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1903
                        }
                    }
                } else {
                    var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1916
                }
            }
        } else {
            t1917 = value__81
            var t1918 bool = t1917 >= 97
            var jp1911 bool
            if t1918 {
                var t1919 uint8
                var inline2393 bool = value__81 >= 65
                var inline2395 bool
                if inline2393 {
                    var inline2398 bool = value__81 <= 90
                    inline2395 = inline2398
                } else {
                    inline2395 = false
                }
                if inline2395 {
                    var inline2396 uint8 = 97 - 65
                    var inline2397 uint8 = value__81 + inline2396
                    t1919 = inline2397
                    var t1920 bool = t1919 <= 102
                    jp1911 = t1920
                    if jp1911 {
                        var t1912 uint8
                        var inline2400 bool = value__81 >= 65
                        var inline2402 bool
                        if inline2400 {
                            var inline2405 bool = value__81 <= 90
                            inline2402 = inline2405
                        } else {
                            inline2402 = false
                        }
                        if inline2402 {
                            var inline2403 uint8 = 97 - 65
                            var inline2404 uint8 = value__81 + inline2403
                            t1912 = inline2404
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        } else {
                            t1912 = value__81
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        }
                    } else {
                        var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1916
                    }
                } else {
                    t1919 = value__81
                    var t1920 bool = t1919 <= 102
                    jp1911 = t1920
                    if jp1911 {
                        var t1912 uint8
                        var inline2400 bool = value__81 >= 65
                        var inline2402 bool
                        if inline2400 {
                            var inline2405 bool = value__81 <= 90
                            inline2402 = inline2405
                        } else {
                            inline2402 = false
                        }
                        if inline2402 {
                            var inline2403 uint8 = 97 - 65
                            var inline2404 uint8 = value__81 + inline2403
                            t1912 = inline2404
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        } else {
                            t1912 = value__81
                            var t1913 uint8 = t1912 - 97
                            var t1914 uint8 = t1913 + 10
                            var t1915 int = int(uint8(t1914))
                            jp1898 = t1915
                            var t1901 bool = jp1898 < base__82
                            if t1901 {
                                var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1898,
                                }
                                return t1902
                            } else {
                                var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1903
                            }
                        }
                    } else {
                        var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1916
                    }
                }
            } else {
                jp1911 = false
                if jp1911 {
                    var t1912 uint8
                    var inline2400 bool = value__81 >= 65
                    var inline2402 bool
                    if inline2400 {
                        var inline2405 bool = value__81 <= 90
                        inline2402 = inline2405
                    } else {
                        inline2402 = false
                    }
                    if inline2402 {
                        var inline2403 uint8 = 97 - 65
                        var inline2404 uint8 = value__81 + inline2403
                        t1912 = inline2404
                        var t1913 uint8 = t1912 - 97
                        var t1914 uint8 = t1913 + 10
                        var t1915 int = int(uint8(t1914))
                        jp1898 = t1915
                        var t1901 bool = jp1898 < base__82
                        if t1901 {
                            var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1898,
                            }
                            return t1902
                        } else {
                            var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1903
                        }
                    } else {
                        t1912 = value__81
                        var t1913 uint8 = t1912 - 97
                        var t1914 uint8 = t1913 + 10
                        var t1915 int = int(uint8(t1914))
                        jp1898 = t1915
                        var t1901 bool = jp1898 < base__82
                        if t1901 {
                            var t1902 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1898,
                            }
                            return t1902
                        } else {
                            var t1903 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1903
                        }
                    }
                } else {
                    var t1916 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1916
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1925:
    for {
        var t1926 bool = carry__22 != 0
        if t1926 {
            var t1935 *_goml_vec_uint32 = value__20.words
            var t1936 int
            var inline2417 int = vec_len__Vec_6uint32(t1935)
            t1936 = inline2417
            var t1937 bool = index__23 == t1936
            if t1937 {
                var t1938 *_goml_vec_uint32 = value__20.words
                var inline2414 uint32 = 0
                vec_push__Vec_6uint32(t1938, inline2414)
            } else {}
            var t1928 *_goml_vec_uint32 = value__20.words
            var t1929 uint32 = vec_get__Vec_6uint32(t1928, index__23)
            var t1930 uint64 = uint64(uint32(t1929))
            var sum__24 uint64 = t1930 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1932_rhs int = 32
            var t1932 uint64 = sum__24 >> t1932_rhs
            carry__22 = t1932
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1933 int = compound_old42 + compound_value43
            index__23 = t1933
            continue
        } else {
            break Loop_loop1925
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1942 FloatNatural
    var inline2419 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2420 FloatNatural = FloatNatural{
        words: inline2419,
    }
    t1942 = inline2420
    var t1943 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1942,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1943
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1946 int = vec_len__Vec_6uint32(self__528)
    var t1947 bool = t1946 == 0
    return t1947
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1950:
    for {
        var t1958 *_goml_vec_uint32 = value__7.words
        var t1959 bool
        var inline2428 int = vec_len__Vec_6uint32(t1958)
        var inline2429 bool = inline2428 == 0
        t1959 = inline2429
        var t1960 bool = !t1959
        var jp1952 bool
        if t1960 {
            var t1961 *_goml_vec_uint32 = value__7.words
            var t1962 *_goml_vec_uint32 = value__7.words
            var t1963 int
            var inline2422 int = vec_len__Vec_6uint32(t1962)
            t1963 = inline2422
            var t1964 int = t1963 - 1
            var t1965 uint32 = vec_get__Vec_6uint32(t1961, t1964)
            var t1966 bool = t1965 == 0
            jp1952 = t1966
        } else {
            jp1952 = false
        }
        if jp1952 {
            var t1953 *_goml_vec_uint32 = value__7.words
            var t1954 *_goml_vec_uint32 = value__7.words
            var t1955 int
            var inline2426 int = vec_len__Vec_6uint32(t1954)
            t1955 = inline2426
            var t1956 int = t1955 - 1
            vec_truncate__Vec_6uint32(t1953, t1956)
            continue
        } else {
            break Loop_loop1950
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1975 bool = string_is_char_boundary(value__274, start__275)
    var jp1972 bool
    if t1975 {
        var t1976 bool = string_is_char_boundary(value__274, end__276)
        jp1972 = t1976
    } else {
        jp1972 = false
    }
    if jp1972 {
        var t1973 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1973
    } else {
        var t1974 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1974
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1982:
    for {
        var t1983 *_goml_vec_uint32 = value__37.words
        var t1984 int
        var inline2433 int = vec_len__Vec_6uint32(t1983)
        t1984 = inline2433
        var t1985 bool = index__41 < t1984
        if t1985 {
            var t1999 *_goml_vec_uint32 = other__38.words
            var t2000 int
            var inline2431 int = vec_len__Vec_6uint32(t1999)
            t2000 = inline2431
            var t2001 bool = index__41 < t2000
            var jp1987 uint64
            if t2001 {
                var t2002 *_goml_vec_uint32 = other__38.words
                var t2003 uint32 = vec_get__Vec_6uint32(t2002, index__41)
                var t2004 uint64 = uint64(uint32(t2003))
                jp1987 = t2004
            } else {
                jp1987 = 0
            }
            var right__42 uint64 = jp1987 + borrow__40
            var t1988 *_goml_vec_uint32 = value__37.words
            var t1989 uint32 = vec_get__Vec_6uint32(t1988, index__41)
            var left__43 uint64 = uint64(uint32(t1989))
            var t1993 bool = left__43 >= right__42
            if t1993 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1994 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1994))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1996 uint64 = base__39 + left__43
                var t1997 uint64 = t1996 - right__42
                var value75 uint32 = uint32(uint64(t1997))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1991 int = compound_old79 + compound_value80
            index__41 = t1991
            continue
        } else {
            break Loop_loop1982
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t2018 bool = index__269 < 0
    var jp2010 bool
    if t2018 {
        jp2010 = true
    } else {
        var t2019 int
        var inline2435 int = _goml_runtime_core_string_len(value__268)
        t2019 = inline2435
        var t2020 bool = index__269 > t2019
        jp2010 = t2020
    }
    if jp2010 {
        return false
    } else {
        var t2013 int
        var inline2439 int = _goml_runtime_core_string_len(value__268)
        t2013 = inline2439
        var t2014 bool = index__269 == t2013
        if t2014 {
            return true
        } else {
            var t2015 uint8
            var inline2437 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t2015 = inline2437
            var t2016_rhs uint8 = 192
            var t2016 uint8 = t2015 & t2016_rhs
            var t2017 bool = t2016 != 128
            return t2017
        }
    }
}

func main() {
    main0()
}
