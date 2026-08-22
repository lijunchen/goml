package main

import (
    _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29 "math"
)

import (
    _goml_os "os"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
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

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

func _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(arg0 float32) uint32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32bits(arg0)
}

func _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(arg0 float64) uint64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64bits(arg0)
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

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
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

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func main0() struct{} {
    var small__10 float32 = 1.5
    var large__11 float64 = 2.25
    var value__12 uint8 = 65
    var t821 int
    var inline1964 int8 = -1
    var inline1965 int16 = 2
    var inline1966 int32 = 3
    var inline1967 int64 = 4
    var inline1968 int = 5
    var inline1969 int = int(int8(inline1964))
    var inline1970 int = int(int16(inline1965))
    var inline1971 int = inline1969 + inline1970
    var inline1972 int = int(int32(inline1966))
    var inline1973 int = inline1971 + inline1972
    var inline1974 int = int(int64(inline1967))
    var inline1975 int = inline1973 + inline1974
    var inline1976 int = inline1975 + inline1968
    t821 = inline1976
    var inline1961 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t821)
    _goml_runtime_core_string_println(inline1961)
    var t822 uint
    var inline1947 uint8 = 1
    var inline1948 uint16 = 2
    var inline1949 uint32 = 3
    var inline1950 uint64 = 4
    var inline1951 uint = 5
    var inline1952 uint = uint(uint8(inline1947))
    var inline1953 uint = uint(uint16(inline1948))
    var inline1954 uint = inline1952 + inline1953
    var inline1955 uint = uint(uint32(inline1949))
    var inline1956 uint = inline1954 + inline1955
    var inline1957 uint = uint(uint64(inline1950))
    var inline1958 uint = inline1956 + inline1957
    var inline1959 uint = inline1958 + inline1951
    t822 = inline1959
    var inline1944 string = _goml_m_trait__impl_i_ToString_i_usize_i_to__string(t822)
    _goml_runtime_core_string_println(inline1944)
    var t823 string
    var inline1942 string = __goml_builtin_float32_to_string(small__10)
    t823 = inline1942
    var t824 string = t823 + ","
    var t825 string
    var inline1940 string = __goml_builtin_float64_to_string(large__11)
    t825 = inline1940
    var t826 string = t824 + t825
    var inline1937 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline1937)
    var t827 uint16 = uint16(uint8(value__12))
    var inline1934 string = _goml_m_trait__impl_i_ToString_i_u16_i_to__string(t827)
    _goml_runtime_core_string_println(inline1934)
    var t828 Option__char
    var inline1931 uint32 = 65
    var inline1932 Option__char = __goml_builtin_char_from_uint32(inline1931)
    t828 = inline1932
    var t829 rune
    var inline1927 rune = 63
    switch t828._tag {
    case 0:
        t829 = inline1927
    case 1:
        var inline1928 rune = t828._v1_0
        t829 = inline1928
    default:
        panic("non-exhaustive match")
    }
    var inline1924 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(t829)
    _goml_runtime_core_string_println(inline1924)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline2000 int64 = int64(int(self__404))
    var inline2001 string = signed_decimal_string(inline2000)
    return inline2001
}

func _goml_m_trait__impl_i_ToString_i_usize_i_to__string(self__704 uint) string {
    var inline2003 uint64 = uint64(uint(self__704))
    var inline2004 string = decimal_string(inline2003)
    return inline2004
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t869 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t870 uint64 = uint64(uint32(t869))
    var t871 string = format_float_bits(t870, 23, 8, 127)
    return t871
}

func __goml_builtin_float64_to_string(value__195 float64) string {
    var t874 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__195)
    var t875 string = format_float_bits(t874, 52, 11, 1023)
    return t875
}

func _goml_m_trait__impl_i_ToString_i_u16_i_to__string(self__410 uint16) string {
    var inline2006 uint64 = uint64(uint16(self__410))
    var inline2007 string = decimal_string(inline2006)
    return inline2007
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__403 rune) string {
    var inline2009 uint32 = uint32(rune(self__403))
    var inline2010 bool = utf8_valid_scalar(inline2009)
    if inline2010 {
        var inline2011 string = _goml_runtime_core_char_to_string(self__403)
        return inline2011
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t886 bool
    var inline2014 bool = value__283 <= 1114111
    if inline2014 {
        var inline2015 bool = value__283 >= 55296
        var inline2017 bool
        if inline2015 {
            var inline2019 bool = value__283 <= 57343
            inline2017 = inline2019
        } else {
            inline2017 = false
        }
        var inline2018 bool = !inline2017
        t886 = inline2018
    } else {
        t886 = false
    }
    if t886 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t887 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t887
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t898 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t898
    var t899 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t899 != 0
    var t900_lhs uint64 = 1
    var t900 uint64 = t900_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t900 - 1
    var t901 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t901 & exponent_mask__166
    var t902_lhs uint64 = 1
    var t902 uint64 = t902_lhs << mantissa_bits__161
    var t903 uint64 = t902 - 1
    var fraction__168 uint64 = bits__160 & t903
    var t967 bool = exponent__167 == exponent_mask__166
    if t967 {
        var t969 bool = fraction__168 == 0
        if t969 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t975 bool = exponent__167 == 0
        var jp973 bool
        if t975 {
            var t976 bool = fraction__168 == 0
            jp973 = t976
        } else {
            jp973 = false
        }
        if jp973 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t964 bool = exponent__167 == 0
            var jp906 uint64
            if t964 {
                jp906 = fraction__168
            } else {
                var t965_lhs uint64 = 1
                var t965 uint64 = t965_lhs << mantissa_bits__161
                var t966 uint64 = fraction__168 | t965
                jp906 = t966
            }
            var t958 bool = exponent__167 == 0
            var jp908 int
            if t958 {
                var t959 int = 1 - exponent_bias__163
                var t960 int = t959 - mantissa_bits__161
                jp908 = t960
            } else {
                var t961 int = int(uint64(exponent__167))
                var t962 int = t961 - exponent_bias__163
                var t963 int = t962 - mantissa_bits__161
                jp908 = t963
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp906)
            var t913 bool = jp908 >= 0
            var jp910 int
            if t913 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp908)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t932 bool = mantissa_bits__161 == 23
                var jp915 int
                if t932 {
                    jp915 = 9
                } else {
                    jp915 = 17
                }
                var t929 int
                var inline2035 int = _goml_runtime_core_string_len(digits__173)
                t929 = inline2035
                var t930 bool = t929 < jp915
                var jp917 int
                if t930 {
                    var inline2029 int = _goml_runtime_core_string_len(digits__173)
                    jp917 = inline2029
                } else {
                    jp917 = jp915
                }
                var count__176 int = 1
                Loop_loop920:
                for {
                    var t921 bool = count__176 <= jp917
                    if t921 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t922 int
                        var inline2031 int = _goml_runtime_core_string_len(digits__173)
                        t922 = inline2031
                        var jp924 int
                        if x319 {
                            jp924 = 1
                        } else {
                            jp924 = 0
                        }
                        var point__180 int = t922 + jp924
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t928 bool = x322 == bits__160
                        if t928 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t926 int = compound_old324 + compound_value325
                            count__176 = t926
                            continue
                        }
                    } else {
                        break Loop_loop920
                    }
                }
                var inline2033 int = _goml_runtime_core_string_len(digits__173)
                jp910 = inline2033
                var t911 string = float_natural_decimal(exact_value__171)
                var t912 string = fixed_float_text(t911, jp910, negative__165)
                return t912
            } else {
                var count__183 int = 0
                var t954 int = 0 - jp908
                Loop_loop953:
                for {
                    var t955 bool = count__183 < t954
                    if t955 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t956 int = compound_old329 + compound_value330
                        count__183 = t956
                        continue
                    } else {
                        break Loop_loop953
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t934 int
                var inline2041 int = _goml_runtime_core_string_len(digits__184)
                t934 = inline2041
                var point__185 int = t934 + jp908
                var t952 bool = mantissa_bits__161 == 23
                var jp936 int
                if t952 {
                    jp936 = 9
                } else {
                    jp936 = 17
                }
                var t949 int
                var inline2039 int = _goml_runtime_core_string_len(digits__184)
                t949 = inline2039
                var t950 bool = t949 < jp936
                var jp938 int
                if t950 {
                    var inline2037 int = _goml_runtime_core_string_len(digits__184)
                    jp938 = inline2037
                } else {
                    jp938 = jp936
                }
                count__183 = 1
                Loop_loop940:
                for {
                    var t941 bool = count__183 <= jp938
                    if t941 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp943 int
                        if x336 {
                            jp943 = 1
                        } else {
                            jp943 = 0
                        }
                        var t944 int = point__185 + jp943
                        var candidate__191 string = fixed_float_text(rounded__190, t944, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t948 bool = x339 == bits__160
                        if t948 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t946 int = compound_old341 + compound_value342
                            count__183 = t946
                            continue
                        }
                    } else {
                        break Loop_loop940
                    }
                }
                jp910 = point__185
                var t911 string = float_natural_decimal(exact_value__171)
                var t912 string = fixed_float_text(t911, jp910, negative__165)
                return t912
            }
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t992 bool = value__257 <= 1114111
    if t992 {
        var t996 bool = value__257 >= 55296
        var jp994 bool
        if t996 {
            var t997 bool = value__257 <= 57343
            jp994 = t997
        } else {
            jp994 = false
        }
        var t995 bool = !jp994
        return t995
    } else {
        return false
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1002 bool = value__214 < 0
    if t1002 {
        var t1003 uint64 = uint64(int64(value__214))
        var t1004 uint64 = 0 - t1003
        var t1005 string = decimal_string(t1004)
        var t1006 string = "-" + t1005
        return t1006
    } else {
        var t1007 uint64 = uint64(int64(value__214))
        var t1008 string = decimal_string(t1007)
        return t1008
    }
}

func decimal_string(value__208 uint64) string {
    var t1031 bool = value__208 == 0
    if t1031 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1024:
        for {
            var t1025 bool = remaining__210 > 0
            if t1025 {
                var t1026_rhs uint64 = 10
                var t1026 uint64 = remaining__210 % t1026_rhs
                var t1027 uint8 = uint8(uint64(t1026))
                var t1028 uint8 = t1027 + 48
                vec_push__Vec_5uint8(reversed__209, t1028)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1029 uint64 = compound_old353 / compound_value354
                remaining__210 = t1029
                continue
            } else {
                break Loop_loop1024
            }
        }
        var t1013 int
        var inline2058 int = vec_len__Vec_5uint8(reversed__209)
        t1013 = inline2058
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1013)
        var offset__212 int = 0
        Loop_loop1015:
        for {
            var t1016 int
            var inline2056 int = vec_len__Vec_5uint8(reversed__209)
            t1016 = inline2056
            var t1017 bool = offset__212 < t1016
            if t1017 {
                var t1018 int
                var inline2054 int = vec_len__Vec_5uint8(reversed__209)
                t1018 = inline2054
                var t1019 int = t1018 - offset__212
                var t1020 int = t1019 - 1
                var t1021 uint8 = vec_get__Vec_5uint8(reversed__209, t1020)
                vec_push__Vec_5uint8(bytes__211, t1021)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1022 int = compound_old358 + compound_value359
                offset__212 = t1022
                continue
            } else {
                break Loop_loop1015
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline2064 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2065 FloatNatural = FloatNatural{
        words: inline2064,
    }
    result__2 = inline2065
    var t1035 bool = value__1 != 0
    if t1035 {
        var t1036 *_goml_vec_uint32 = result__2.words
        var t1037 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t1036, t1037)
        var t1038_rhs int = 32
        var t1038 uint64 = value__1 >> t1038_rhs
        var high__3 uint32 = uint32(uint64(t1038))
        var t1040 bool = high__3 != 0
        if t1040 {
            var t1041 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t1041, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1070 bool
    var inline2082 *_goml_vec_uint32 = value__28.words
    var inline2083 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2082)
    t1070 = inline2083
    if t1070 {
        var inline2067 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2068 FloatNatural = FloatNatural{
            words: inline2067,
        }
        return inline2068
    } else {
        var t1073 bool = bits__29 == 0
        if t1073 {
            var t1074 FloatNatural = float_natural_copy(value__28)
            return t1074
        } else {
            var result__30 FloatNatural
            var inline2079 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2080 FloatNatural = FloatNatural{
                words: inline2079,
            }
            result__30 = inline2080
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1065:
            for {
                var t1066 bool = index__33 < word_shift__31
                if t1066 {
                    var t1067 *_goml_vec_uint32 = result__30.words
                    var inline2070 uint32 = 0
                    vec_push__Vec_6uint32(t1067, inline2070)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1068 int = compound_old52 + compound_value53
                    index__33 = t1068
                    continue
                } else {
                    break Loop_loop1065
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop1053:
            for {
                var t1054 *_goml_vec_uint32 = value__28.words
                var t1055 int
                var inline2075 int = vec_len__Vec_6uint32(t1054)
                t1055 = inline2075
                var t1056 bool = index__33 < t1055
                if t1056 {
                    var t1057 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1057, index__33)
                    var t1058 uint64 = uint64(uint32(word__35))
                    var t1059 uint64 = t1058 << bit_shift__32
                    var shifted__36 uint64 = t1059 | carry__34
                    var t1060 *_goml_vec_uint32 = result__30.words
                    var t1061 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1060, t1061)
                    var t1062_rhs int = 32
                    var t1062 uint64 = shifted__36 >> t1062_rhs
                    carry__34 = t1062
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1063 int = compound_old59 + compound_value60
                    index__33 = t1063
                    continue
                } else {
                    break Loop_loop1053
                }
            }
            var t1049 bool = carry__34 != 0
            if t1049 {
                var t1050 *_goml_vec_uint32 = result__30.words
                var t1051 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t1050, t1051)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1097 bool
    var inline2098 *_goml_vec_uint32 = value__49.words
    var inline2099 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2098)
    t1097 = inline2099
    if t1097 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1090:
        for {
            var t1091 bool
            var inline2087 *_goml_vec_uint32 = current__50.words
            var inline2088 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2087)
            t1091 = inline2088
            var t1092 bool = !t1091
            if t1092 {
                var t1093 uint32 = float_natural_divide_small(current__50, 10)
                var t1094 uint8 = uint8(uint32(t1093))
                var t1095 uint8 = t1094 + 48
                vec_push__Vec_5uint8(reversed__51, t1095)
                continue
            } else {
                break Loop_loop1090
            }
        }
        var t1079 int
        var inline2096 int = vec_len__Vec_5uint8(reversed__51)
        t1079 = inline2096
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1079)
        var offset__53 int = 0
        Loop_loop1081:
        for {
            var t1082 int
            var inline2094 int = vec_len__Vec_5uint8(reversed__51)
            t1082 = inline2094
            var t1083 bool = offset__53 < t1082
            if t1083 {
                var t1084 int
                var inline2092 int = vec_len__Vec_5uint8(reversed__51)
                t1084 = inline2092
                var t1085 int = t1084 - offset__53
                var t1086 int = t1085 - 1
                var t1087 uint8 = vec_get__Vec_5uint8(reversed__51, t1086)
                vec_push__Vec_5uint8(output__52, t1087)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1088 int = compound_old98 + compound_value99
                offset__53 = t1088
                continue
            } else {
                break Loop_loop1081
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1100 int = _goml_runtime_core_string_len(self__289)
    return t1100
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1103 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1103)
    var index__148 int = 0
    Loop_loop1158:
    for {
        var t1159 bool = index__148 < count__146
        if t1159 {
            var t1160 uint8
            var inline2103 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1160 = inline2103
            vec_push__Vec_5uint8(output__147, t1160)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1161 int = compound_old267 + compound_value268
            index__148 = t1161
            continue
        } else {
            break Loop_loop1158
        }
    }
    var t1155 int
    var inline2124 int = _goml_runtime_core_string_len(exact__145)
    t1155 = inline2124
    var t1156 bool = count__146 == t1155
    if t1156 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1157 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1157
    } else {
        var next__150 uint8
        var inline2122 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2122
        var trailing__151 bool = false
        var t1106 int = count__146 + 1
        index__148 = t1106
        Loop_loop1147:
        for {
            var t1148 int
            var inline2107 int = _goml_runtime_core_string_len(exact__145)
            t1148 = inline2107
            var t1149 bool = index__148 < t1148
            if t1149 {
                var t1153 uint8
                var inline2105 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1153 = inline2105
                var t1154 bool = t1153 != 48
                if t1154 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1151 int = compound_old278 + compound_value279
                index__148 = t1151
                continue
            } else {
                break Loop_loop1147
            }
        }
        var t1135 bool = next__150 > 53
        var jp1109 bool
        if t1135 {
            jp1109 = true
        } else {
            var t1138 bool = next__150 == 53
            if t1138 {
                if trailing__151 {
                    jp1109 = true
                } else {
                    var t1141 int
                    var inline2109 int = vec_len__Vec_5uint8(output__147)
                    t1141 = inline2109
                    var t1142 int = t1141 - 1
                    var t1143 uint8 = vec_get__Vec_5uint8(output__147, t1142)
                    var t1144 uint8 = t1143 - 48
                    var t1145_rhs uint8 = 2
                    var t1145 uint8 = t1144 % t1145_rhs
                    var t1146 bool = t1145 == 1
                    jp1109 = t1146
                }
            } else {
                jp1109 = false
            }
        }
        if jp1109 {
            var index__153 int
            var inline2120 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2120
            Loop_loop1123:
            for {
                var t1124 bool = index__153 > 0
                if t1124 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1125 int = compound_old282 - compound_value283
                    index__153 = t1125
                    var t1128 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1129 bool = t1128 < 57
                    if t1129 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1130 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1130)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1132 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1132
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1123
                }
            }
            var t1113 int
            var inline2118 int = vec_len__Vec_5uint8(output__147)
            t1113 = inline2118
            var t1114 int = t1113 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1114)
            var inline2115 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2115)
            index__153 = 0
            Loop_loop1117:
            for {
                var t1118 int
                var inline2113 int = vec_len__Vec_5uint8(output__147)
                t1118 = inline2113
                var t1119 bool = index__153 < t1118
                if t1119 {
                    var t1120 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1120)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1121 int = compound_old302 + compound_value303
                    index__153 = t1121
                    continue
                } else {
                    break Loop_loop1117
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1116 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1116
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1134 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1134
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2131 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2131
    Loop_loop1167:
    for {
        var t1172 bool = length__159 > 1
        var jp1169 bool
        if t1172 {
            var t1173 int = length__159 - 1
            var t1174 uint8
            var inline2126 uint8 = _goml_runtime_core_string_byte_get(value__158, t1173)
            t1174 = inline2126
            var t1175 bool = t1174 == 48
            jp1169 = t1175
        } else {
            jp1169 = false
        }
        if jp1169 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1170 int = compound_old312 - compound_value313
            length__159 = t1170
            continue
        } else {
            break Loop_loop1167
        }
    }
    var inline2128 int = 0
    var inline2129 string = string_byte_slice(value__158, inline2128, length__159)
    return inline2129
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2133 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2133)
    } else {}
    var t1180 bool = decimal_point__138 <= 0
    if t1180 {
        var inline2148 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2148)
        var inline2145 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2145)
        var index__141 int = 0
        var t1190 int = 0 - decimal_point__138
        Loop_loop1189:
        for {
            var t1191 bool = index__141 < t1190
            if t1191 {
                var inline2136 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2136)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1192 int = compound_old234 + compound_value235
                index__141 = t1192
                continue
            } else {
                break Loop_loop1189
            }
        }
        index__141 = 0
        Loop_loop1183:
        for {
            var t1184 int
            var inline2143 int = _goml_runtime_core_string_len(digits__137)
            t1184 = inline2143
            var t1185 bool = index__141 < t1184
            if t1185 {
                var t1186 uint8
                var inline2141 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1186 = inline2141
                vec_push__Vec_5uint8(bytes__140, t1186)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1187 int = compound_old240 + compound_value241
                index__141 = t1187
                continue
            } else {
                break Loop_loop1183
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1195 int
        var inline2173 int = _goml_runtime_core_string_len(digits__137)
        t1195 = inline2173
        var t1196 bool = decimal_point__138 >= t1195
        if t1196 {
            var index__142 int = 0
            Loop_loop1203:
            for {
                var t1204 int
                var inline2155 int = _goml_runtime_core_string_len(digits__137)
                t1204 = inline2155
                var t1205 bool = index__142 < t1204
                if t1205 {
                    var t1206 uint8
                    var inline2153 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1206 = inline2153
                    vec_push__Vec_5uint8(bytes__140, t1206)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1207 int = compound_old244 + compound_value245
                    index__142 = t1207
                    continue
                } else {
                    break Loop_loop1203
                }
            }
            Loop_loop1199:
            for {
                var t1200 bool = index__142 < decimal_point__138
                if t1200 {
                    var inline2157 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2157)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1201 int = compound_old249 + compound_value250
                    index__142 = t1201
                    continue
                } else {
                    break Loop_loop1199
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1217:
            for {
                var t1218 bool = index__143 < decimal_point__138
                if t1218 {
                    var t1219 uint8
                    var inline2162 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1219 = inline2162
                    vec_push__Vec_5uint8(bytes__140, t1219)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1220 int = compound_old253 + compound_value254
                    index__143 = t1220
                    continue
                } else {
                    break Loop_loop1217
                }
            }
            var inline2170 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2170)
            Loop_loop1211:
            for {
                var t1212 int
                var inline2168 int = _goml_runtime_core_string_len(digits__137)
                t1212 = inline2168
                var t1213 bool = index__143 < t1212
                if t1213 {
                    var t1214 uint8
                    var inline2166 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1214 = inline2166
                    vec_push__Vec_5uint8(bytes__140, t1214)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1215 int = compound_old259 + compound_value260
                    index__143 = t1215
                    continue
                } else {
                    break Loop_loop1211
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        }
    }
}

func parsed_float_bits(value__107 string, mantissa_bits__108 int, exponent_bias__109 int) Tuple2_4bool_6uint64 {
    var parsed__110 ParsedFloat = parse_float_text(value__107)
    var t1316 bool = parsed__110.valid
    var t1317 bool = !t1316
    if t1317 {
        var t1318 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1318
    } else {
        var t1310 bool = parsed__110.negative
        var jp1227 uint64
        if t1310 {
            var t1315 bool = mantissa_bits__108 == 23
            var jp1312 int
            if t1315 {
                jp1312 = 8
            } else {
                jp1312 = 11
            }
            var t1313 int = mantissa_bits__108 + jp1312
            var t1314_lhs uint64 = 1
            var t1314 uint64 = t1314_lhs << t1313
            jp1227 = t1314
        } else {
            jp1227 = 0
        }
        var t1309 bool = mantissa_bits__108 == 23
        var jp1229 int
        if t1309 {
            jp1229 = 8
        } else {
            jp1229 = 11
        }
        var t1230_lhs uint64 = 1
        var t1230 uint64 = t1230_lhs << jp1229
        var t1231 uint64 = t1230 - 1
        var exponent_mask__112 uint64 = t1231 << mantissa_bits__108
        var t1287 int = parsed__110.special
        var t1288 bool = t1287 == 1
        if t1288 {
            var t1289 uint64 = jp1227 | exponent_mask__112
            var t1290 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1289,
            }
            return t1290
        } else {
            var t1292 int = parsed__110.special
            var t1293 bool = t1292 == 2
            if t1293 {
                var t1297 int = mantissa_bits__108 - 1
                var t1298_lhs uint64 = 1
                var t1298 uint64 = t1298_lhs << t1297
                var t1299 uint64 = exponent_mask__112 | t1298
                var t1304 bool = mantissa_bits__108 == 52
                var jp1301 uint64
                if t1304 {
                    jp1301 = 1
                } else {
                    jp1301 = 0
                }
                var t1302 uint64 = t1299 | jp1301
                var t1303 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1302,
                }
                return t1303
            } else {
                var t1306 FloatNatural = parsed__110.numerator
                var t1307 bool
                var inline2175 *_goml_vec_uint32 = t1306.words
                var inline2176 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2175)
                t1307 = inline2176
                if t1307 {
                    var t1308 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1227,
                    }
                    return t1308
                } else {
                    var t1270 bool = parsed__110.hexadecimal
                    var t1271 bool = !t1270
                    if t1271 {
                        var t1272 int = parsed__110.significant_digits
                        var t1273 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1272 + t1273
                        var t1286 bool = mantissa_bits__108 == 23
                        var jp1275 int
                        if t1286 {
                            jp1275 = 40
                        } else {
                            jp1275 = 310
                        }
                        var t1285 bool = mantissa_bits__108 == 23
                        var jp1277 int
                        if t1285 {
                            jp1277 = -46
                        } else {
                            jp1277 = -325
                        }
                        var t1279 bool = decimal_position__113 > jp1275
                        if t1279 {
                            var t1280 uint64 = jp1227 | exponent_mask__112
                            var t1281 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1280,
                            }
                            return t1281
                        } else {
                            var t1283 bool = decimal_position__113 < jp1277
                            if t1283 {
                                var t1284 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1227,
                                }
                                return t1284
                            } else {
                                var t1266 bool = parsed__110.hexadecimal
                                var t1267 bool = !t1266
                                var jp1261 bool
                                if t1267 {
                                    var t1268 int = parsed__110.decimal_exponent
                                    var t1269 bool = t1268 < 0
                                    jp1261 = t1269
                                } else {
                                    jp1261 = false
                                }
                                var jp1235 FloatNatural
                                if jp1261 {
                                    var t1262 int = parsed__110.decimal_exponent
                                    var t1263 int = 0 - t1262
                                    var t1264 FloatNatural = float_natural_power5(t1263)
                                    jp1235 = t1264
                                } else {
                                    var inline2178 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2178, 1)
                                    var inline2180 FloatNatural = FloatNatural{
                                        words: inline2178,
                                    }
                                    jp1235 = inline2180
                                }
                                var t1256 bool = parsed__110.hexadecimal
                                var t1257 bool = !t1256
                                var jp1247 bool
                                if t1257 {
                                    var t1258 int = parsed__110.decimal_exponent
                                    var t1259 bool = t1258 > 0
                                    jp1247 = t1259
                                } else {
                                    jp1247 = false
                                }
                                var jp1237 FloatNatural
                                if jp1247 {
                                    var t1248 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1248)
                                    var count__118 int = 0
                                    Loop_loop1250:
                                    for {
                                        var t1251 int = parsed__110.decimal_exponent
                                        var t1252 bool = count__118 < t1251
                                        if t1252 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1253 int = compound_old213 + compound_value214
                                            count__118 = t1253
                                            continue
                                        } else {
                                            break Loop_loop1250
                                        }
                                    }
                                    jp1237 = result__117
                                    var t1243 bool = parsed__110.hexadecimal
                                    var jp1239 int
                                    if t1243 {
                                        var t1244 int = parsed__110.binary_exponent
                                        jp1239 = t1244
                                    } else {
                                        var t1245 int = parsed__110.decimal_exponent
                                        jp1239 = t1245
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1237, jp1235, jp1239, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1240 bool = !x219
                                    var t1241 uint64 = jp1227 | x218
                                    var t1242 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1240,
                                        _1: t1241,
                                    }
                                    return t1242
                                } else {
                                    var t1255 FloatNatural = parsed__110.numerator
                                    jp1237 = t1255
                                    var t1243 bool = parsed__110.hexadecimal
                                    var jp1239 int
                                    if t1243 {
                                        var t1244 int = parsed__110.binary_exponent
                                        jp1239 = t1244
                                    } else {
                                        var t1245 int = parsed__110.decimal_exponent
                                        jp1239 = t1245
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1237, jp1235, jp1239, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1240 bool = !x219
                                    var t1241 uint64 = jp1227 | x218
                                    var t1242 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1240,
                                        _1: t1241,
                                    }
                                    return t1242
                                }
                            }
                        }
                    } else {
                        var t1266 bool = parsed__110.hexadecimal
                        var t1267 bool = !t1266
                        var jp1261 bool
                        if t1267 {
                            var t1268 int = parsed__110.decimal_exponent
                            var t1269 bool = t1268 < 0
                            jp1261 = t1269
                        } else {
                            jp1261 = false
                        }
                        var jp1235 FloatNatural
                        if jp1261 {
                            var t1262 int = parsed__110.decimal_exponent
                            var t1263 int = 0 - t1262
                            var t1264 FloatNatural = float_natural_power5(t1263)
                            jp1235 = t1264
                        } else {
                            var inline2178 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2178, 1)
                            var inline2180 FloatNatural = FloatNatural{
                                words: inline2178,
                            }
                            jp1235 = inline2180
                        }
                        var t1256 bool = parsed__110.hexadecimal
                        var t1257 bool = !t1256
                        var jp1247 bool
                        if t1257 {
                            var t1258 int = parsed__110.decimal_exponent
                            var t1259 bool = t1258 > 0
                            jp1247 = t1259
                        } else {
                            jp1247 = false
                        }
                        var jp1237 FloatNatural
                        if jp1247 {
                            var t1248 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1248)
                            var count__118 int = 0
                            Loop_loop1250__2:
                            for {
                                var t1251 int = parsed__110.decimal_exponent
                                var t1252 bool = count__118 < t1251
                                if t1252 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1253 int = compound_old213 + compound_value214
                                    count__118 = t1253
                                    continue
                                } else {
                                    break Loop_loop1250__2
                                }
                            }
                            jp1237 = result__117
                            var t1243 bool = parsed__110.hexadecimal
                            var jp1239 int
                            if t1243 {
                                var t1244 int = parsed__110.binary_exponent
                                jp1239 = t1244
                            } else {
                                var t1245 int = parsed__110.decimal_exponent
                                jp1239 = t1245
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1237, jp1235, jp1239, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1240 bool = !x219
                            var t1241 uint64 = jp1227 | x218
                            var t1242 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1240,
                                _1: t1241,
                            }
                            return t1242
                        } else {
                            var t1255 FloatNatural = parsed__110.numerator
                            jp1237 = t1255
                            var t1243 bool = parsed__110.hexadecimal
                            var jp1239 int
                            if t1243 {
                                var t1244 int = parsed__110.binary_exponent
                                jp1239 = t1244
                            } else {
                                var t1245 int = parsed__110.decimal_exponent
                                jp1239 = t1245
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1237, jp1235, jp1239, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1240 bool = !x219
                            var t1241 uint64 = jp1227 | x218
                            var t1242 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1240,
                                _1: t1241,
                            }
                            return t1242
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1340 bool = factor__16 == 0
    if t1340 {
        var t1341 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1341, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1334 uint64 = uint64(uint32(factor__16))
        Loop_loop1327:
        for {
            var t1328 *_goml_vec_uint32 = value__15.words
            var t1329 int
            var inline2184 int = vec_len__Vec_6uint32(t1328)
            t1329 = inline2184
            var t1330 bool = index__18 < t1329
            if t1330 {
                var t1331 *_goml_vec_uint32 = value__15.words
                var t1332 uint32 = vec_get__Vec_6uint32(t1331, index__18)
                var t1333 uint64 = uint64(uint32(t1332))
                var t1335 uint64 = t1333 * t1334
                var product__19 uint64 = t1335 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1337_rhs int = 32
                var t1337 uint64 = product__19 >> t1337_rhs
                carry__17 = t1337
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1338 int = compound_old30 + compound_value31
                index__18 = t1338
                continue
            } else {
                break Loop_loop1327
            }
        }
        var t1323 bool = carry__17 != 0
        if t1323 {
            var t1324 *_goml_vec_uint32 = value__15.words
            var t1325 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1324, t1325)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1349 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1350 FloatNatural = FloatNatural{
        words: t1349,
    }
    return t1350
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2195 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2196 FloatNatural = FloatNatural{
        words: inline2195,
    }
    result__5 = inline2196
    var index__6 int = 0
    Loop_loop1360:
    for {
        var t1361 *_goml_vec_uint32 = value__4.words
        var t1362 int
        var inline2193 int = vec_len__Vec_6uint32(t1361)
        t1362 = inline2193
        var t1363 bool = index__6 < t1362
        if t1363 {
            var t1364 *_goml_vec_uint32 = result__5.words
            var t1365 *_goml_vec_uint32 = value__4.words
            var t1366 uint32 = vec_get__Vec_6uint32(t1365, index__6)
            vec_push__Vec_6uint32(t1364, t1366)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1367 int = compound_old4 + compound_value5
            index__6 = t1367
            continue
        } else {
            break Loop_loop1360
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1374 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2198 int = vec_len__Vec_6uint32(t1374)
    index__47 = inline2198
    var t1385 uint64 = uint64(uint32(divisor__45))
    var t1388 uint64 = uint64(uint32(divisor__45))
    Loop_loop1377:
    for {
        var t1378 bool = index__47 > 0
        if t1378 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1379 int = compound_old83 - compound_value84
            index__47 = t1379
            var t1381_rhs int = 32
            var t1381 uint64 = remainder__46 << t1381_rhs
            var t1382 *_goml_vec_uint32 = value__44.words
            var t1383 uint32 = vec_get__Vec_6uint32(t1382, index__47)
            var t1384 uint64 = uint64(uint32(t1383))
            var current__48 uint64 = t1381 | t1384
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1386 uint64 = current__48 / t1385
            var value90 uint32 = uint32(uint64(t1386))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1389 uint64 = current__48 % t1388
            remainder__46 = t1389
            continue
        } else {
            break Loop_loop1377
        }
    }
    float_natural_trim(value__44)
    var t1376 uint32 = uint32(uint64(remainder__46))
    return t1376
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1392 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1392
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2200 bool = string_is_char_boundary(self__294, start__295)
    var inline2202 bool
    if inline2200 {
        var inline2205 bool = string_is_char_boundary(self__294, end__296)
        inline2202 = inline2205
    } else {
        inline2202 = false
    }
    if inline2202 {
        var inline2203 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2203
    } else {
        var inline2204 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2204
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1580 bool = string_equals_ascii_case(value__84, "nan")
    if t1580 {
        var t1581 FloatNatural
        var inline2207 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2208 FloatNatural = FloatNatural{
            words: inline2207,
        }
        t1581 = inline2208
        var t1582 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1581,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1582
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1572 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1573 bool = index__85 < t1572
        var jp1567 bool
        if t1573 {
            var t1576 uint8
            var inline2212 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1576 = inline2212
            var t1577 bool = t1576 == 43
            if t1577 {
                jp1567 = true
            } else {
                var t1578 uint8
                var inline2210 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1578 = inline2210
                var t1579 bool = t1578 == 45
                jp1567 = t1579
            }
        } else {
            jp1567 = false
        }
        if jp1567 {
            var t1568 uint8
            var inline2214 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1568 = inline2214
            var t1569 bool = t1568 == 45
            negative__86 = t1569
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1570 int = compound_old140 + compound_value141
            index__85 = t1570
        } else {}
        var t1400 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1400)
        var t1564 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1561 bool
        if t1564 {
            jp1561 = true
        } else {
            var t1565 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1561 = t1565
        }
        if jp1561 {
            var t1562 FloatNatural
            var inline2216 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2217 FloatNatural = FloatNatural{
                words: inline2216,
            }
            t1562 = inline2217
            var t1563 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1562,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1563
        } else {
            var t1555 int = index__85 + 2
            var t1556 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1557 bool = t1555 <= t1556
            var jp1550 bool
            if t1557 {
                var t1558 uint8
                var inline2219 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1558 = inline2219
                var t1559 bool = t1558 == 48
                jp1550 = t1559
            } else {
                jp1550 = false
            }
            var jp1403 bool
            if jp1550 {
                var t1551 int = index__85 + 1
                var t1552 uint8
                var inline2228 uint8 = _goml_runtime_core_string_byte_get(value__84, t1551)
                t1552 = inline2228
                var t1553 uint8
                var inline2221 bool = t1552 >= 65
                var inline2223 bool
                if inline2221 {
                    var inline2226 bool = t1552 <= 90
                    inline2223 = inline2226
                } else {
                    inline2223 = false
                }
                if inline2223 {
                    var inline2224 uint8 = 97 - 65
                    var inline2225 uint8 = t1552 + inline2224
                    t1553 = inline2225
                    var t1554 bool = t1553 == 120
                    jp1403 = t1554
                    if jp1403 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1547 int = compound_old145 + compound_value146
                        index__85 = t1547
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1406 int
                    if jp1403 {
                        jp1406 = 16
                    } else {
                        jp1406 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1500 uint32 = uint32(int(jp1406))
                    Loop_loop1496:
                    for {
                        var t1497 int
                        var inline2242 int = _goml_runtime_core_string_len(value__84)
                        t1497 = inline2242
                        var t1498 bool = index__85 < t1497
                        if t1498 {
                            var current__97 uint8
                            var inline2240 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2240
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1406)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1500)
                                var t1501 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1501)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1512 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1512
                                } else {}
                                var t1510 bool = significant_digits__95 > 0
                                var jp1507 bool
                                if t1510 {
                                    jp1507 = true
                                } else {
                                    var t1511 bool = x151 != 0
                                    jp1507 = t1511
                                }
                                if jp1507 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1508 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1508
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1504 int = compound_old164 + compound_value165
                                index__85 = t1504
                                continue
                            } else {
                                var t1515 bool = current__97 == 95
                                if t1515 {
                                    var t1536 int = index__85 + 1
                                    var t1537 int
                                    var inline2238 int = _goml_runtime_core_string_len(value__84)
                                    t1537 = inline2238
                                    var t1538 bool = t1536 >= t1537
                                    if t1538 {
                                        var inline2230 FloatNatural = float_natural_zero()
                                        var inline2231 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2230,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2231
                                    } else {
                                        var t1517 int = index__85 + 1
                                        var t1518 uint8
                                        var inline2236 uint8 = _goml_runtime_core_string_byte_get(value__84, t1517)
                                        t1518 = inline2236
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1518, jp1406)
                                        var x169 bool = mtmp168._0
                                        var jp1533 bool
                                        if jp1403 {
                                            var t1535 bool = !saw_digit__92
                                            jp1533 = t1535
                                        } else {
                                            jp1533 = false
                                        }
                                        var jp1520 bool
                                        if jp1533 {
                                            var t1534 bool = index__85 == mantissa_start__89
                                            jp1520 = t1534
                                        } else {
                                            jp1520 = false
                                        }
                                        var t1530 bool = !previous_digit__96
                                        var jp1528 bool
                                        if t1530 {
                                            var t1531 bool = !jp1520
                                            jp1528 = t1531
                                        } else {
                                            jp1528 = false
                                        }
                                        var jp1525 bool
                                        if jp1528 {
                                            jp1525 = true
                                        } else {
                                            var t1529 bool = !x169
                                            jp1525 = t1529
                                        }
                                        if jp1525 {
                                            var inline2233 FloatNatural = float_natural_zero()
                                            var inline2234 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2233,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2234
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1522 int = compound_old173 + compound_value174
                                            index__85 = t1522
                                            continue
                                        }
                                    }
                                } else {
                                    var t1545 bool = current__97 == 46
                                    var jp1542 bool
                                    if t1545 {
                                        var t1546 bool = !saw_dot__93
                                        jp1542 = t1546
                                    } else {
                                        jp1542 = false
                                    }
                                    if jp1542 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1543 int = compound_old178 + compound_value179
                                        index__85 = t1543
                                        continue
                                    } else {
                                        break Loop_loop1496
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1496
                        }
                    }
                    var t1494 bool = !saw_digit__92
                    if t1494 {
                        var inline2244 FloatNatural = float_natural_zero()
                        var inline2245 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2244,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2245
                    } else {
                        var jp1410 uint8
                        if jp1403 {
                            jp1410 = 112
                        } else {
                            jp1410 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1489 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1490 bool = index__85 < t1489
                        var jp1427 bool
                        if t1490 {
                            var t1491 uint8
                            var inline2247 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1491 = inline2247
                            var t1492 uint8 = ascii_lower(t1491)
                            var t1493 bool = t1492 == jp1410
                            jp1427 = t1493
                        } else {
                            jp1427 = false
                        }
                        if jp1427 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1428 int = compound_old183 + compound_value184
                            index__85 = t1428
                            var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1480 bool = index__85 < t1479
                            var jp1474 bool
                            if t1480 {
                                var t1483 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1484 bool = t1483 == 43
                                if t1484 {
                                    jp1474 = true
                                } else {
                                    var t1485 uint8
                                    var inline2249 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1485 = inline2249
                                    var t1486 bool = t1485 == 45
                                    jp1474 = t1486
                                }
                            } else {
                                jp1474 = false
                            }
                            if jp1474 {
                                var t1475 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1476 bool = t1475 == 45
                                exponent_negative__104 = t1476
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1477 int = compound_old187 + compound_value188
                                index__85 = t1477
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1435:
                            for {
                                var t1436 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1437 bool = index__85 < t1436
                                if t1437 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1471 bool = current__106 >= 48
                                    var jp1440 bool
                                    if t1471 {
                                        var t1472 bool = current__106 <= 57
                                        jp1440 = t1472
                                    } else {
                                        jp1440 = false
                                    }
                                    if jp1440 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1444 bool = exponent__103 < 1000000
                                        if t1444 {
                                            var t1445 int = exponent__103 * 10
                                            var t1446 uint8 = current__106 - 48
                                            var t1447 int = int(uint8(t1446))
                                            var t1448 int = t1445 + t1447
                                            exponent__103 = t1448
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1442 int = compound_old196 + compound_value197
                                        index__85 = t1442
                                        continue
                                    } else {
                                        var t1450 bool = current__106 == 95
                                        if t1450 {
                                            var t1467 bool = !previous_digit__96
                                            var jp1463 bool
                                            if t1467 {
                                                jp1463 = true
                                            } else {
                                                var t1468 int = index__85 + 1
                                                var t1469 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1470 bool = t1468 >= t1469
                                                jp1463 = t1470
                                            }
                                            var jp1458 bool
                                            if jp1463 {
                                                jp1458 = true
                                            } else {
                                                var t1464 int = index__85 + 1
                                                var t1465 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1464)
                                                var t1466 bool = t1465 < 48
                                                jp1458 = t1466
                                            }
                                            var jp1455 bool
                                            if jp1458 {
                                                jp1455 = true
                                            } else {
                                                var t1459 int = index__85 + 1
                                                var t1460 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1459)
                                                var t1461 bool = t1460 > 57
                                                jp1455 = t1461
                                            }
                                            if jp1455 {
                                                var t1456 ParsedFloat = invalid_parsed_float()
                                                return t1456
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1452 int = compound_old201 + compound_value202
                                                index__85 = t1452
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1435
                                        }
                                    }
                                } else {
                                    break Loop_loop1435
                                }
                            }
                            var t1433 bool = !exponent_digits__105
                            if t1433 {
                                var t1434 ParsedFloat = invalid_parsed_float()
                                return t1434
                            } else {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 != t1423
                                if t1424 {
                                    var t1425 ParsedFloat = invalid_parsed_float()
                                    return t1425
                                } else {
                                    if exponent_negative__104 {
                                        var t1422 int = 0 - exponent__103
                                        exponent__103 = t1422
                                    } else {}
                                    var jp1415 int
                                    if jp1403 {
                                        jp1415 = 0
                                    } else {
                                        var t1421 int = exponent__103 - fraction_digits__94
                                        jp1415 = t1421
                                    }
                                    var jp1417 int
                                    if jp1403 {
                                        var t1419 int = fraction_digits__94 * 4
                                        var t1420 int = exponent__103 - t1419
                                        jp1417 = t1420
                                    } else {
                                        jp1417 = 0
                                    }
                                    var t1418 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1415,
                                        binary_exponent: jp1417,
                                        hexadecimal: jp1403,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1418
                                }
                            }
                        } else {
                            if jp1403 {
                                var t1488 ParsedFloat = invalid_parsed_float()
                                return t1488
                            } else {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 != t1423
                                if t1424 {
                                    var t1425 ParsedFloat = invalid_parsed_float()
                                    return t1425
                                } else {
                                    if exponent_negative__104 {
                                        var t1422 int = 0 - exponent__103
                                        exponent__103 = t1422
                                    } else {}
                                    var jp1415 int
                                    if jp1403 {
                                        jp1415 = 0
                                    } else {
                                        var t1421 int = exponent__103 - fraction_digits__94
                                        jp1415 = t1421
                                    }
                                    var jp1417 int
                                    if jp1403 {
                                        var t1419 int = fraction_digits__94 * 4
                                        var t1420 int = exponent__103 - t1419
                                        jp1417 = t1420
                                    } else {
                                        jp1417 = 0
                                    }
                                    var t1418 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1415,
                                        binary_exponent: jp1417,
                                        hexadecimal: jp1403,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1418
                                }
                            }
                        }
                    }
                } else {
                    t1553 = t1552
                    var t1554 bool = t1553 == 120
                    jp1403 = t1554
                    if jp1403 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1547 int = compound_old145 + compound_value146
                        index__85 = t1547
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1406 int
                    if jp1403 {
                        jp1406 = 16
                    } else {
                        jp1406 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1500 uint32 = uint32(int(jp1406))
                    Loop_loop1496__2:
                    for {
                        var t1497 int
                        var inline2242 int = _goml_runtime_core_string_len(value__84)
                        t1497 = inline2242
                        var t1498 bool = index__85 < t1497
                        if t1498 {
                            var current__97 uint8
                            var inline2240 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2240
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1406)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1500)
                                var t1501 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1501)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1512 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1512
                                } else {}
                                var t1510 bool = significant_digits__95 > 0
                                var jp1507 bool
                                if t1510 {
                                    jp1507 = true
                                } else {
                                    var t1511 bool = x151 != 0
                                    jp1507 = t1511
                                }
                                if jp1507 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1508 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1508
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1504 int = compound_old164 + compound_value165
                                index__85 = t1504
                                continue
                            } else {
                                var t1515 bool = current__97 == 95
                                if t1515 {
                                    var t1536 int = index__85 + 1
                                    var t1537 int
                                    var inline2238 int = _goml_runtime_core_string_len(value__84)
                                    t1537 = inline2238
                                    var t1538 bool = t1536 >= t1537
                                    if t1538 {
                                        var inline2230 FloatNatural = float_natural_zero()
                                        var inline2231 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2230,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2231
                                    } else {
                                        var t1517 int = index__85 + 1
                                        var t1518 uint8
                                        var inline2236 uint8 = _goml_runtime_core_string_byte_get(value__84, t1517)
                                        t1518 = inline2236
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1518, jp1406)
                                        var x169 bool = mtmp168._0
                                        var jp1533 bool
                                        if jp1403 {
                                            var t1535 bool = !saw_digit__92
                                            jp1533 = t1535
                                        } else {
                                            jp1533 = false
                                        }
                                        var jp1520 bool
                                        if jp1533 {
                                            var t1534 bool = index__85 == mantissa_start__89
                                            jp1520 = t1534
                                        } else {
                                            jp1520 = false
                                        }
                                        var t1530 bool = !previous_digit__96
                                        var jp1528 bool
                                        if t1530 {
                                            var t1531 bool = !jp1520
                                            jp1528 = t1531
                                        } else {
                                            jp1528 = false
                                        }
                                        var jp1525 bool
                                        if jp1528 {
                                            jp1525 = true
                                        } else {
                                            var t1529 bool = !x169
                                            jp1525 = t1529
                                        }
                                        if jp1525 {
                                            var inline2233 FloatNatural = float_natural_zero()
                                            var inline2234 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2233,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2234
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1522 int = compound_old173 + compound_value174
                                            index__85 = t1522
                                            continue
                                        }
                                    }
                                } else {
                                    var t1545 bool = current__97 == 46
                                    var jp1542 bool
                                    if t1545 {
                                        var t1546 bool = !saw_dot__93
                                        jp1542 = t1546
                                    } else {
                                        jp1542 = false
                                    }
                                    if jp1542 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1543 int = compound_old178 + compound_value179
                                        index__85 = t1543
                                        continue
                                    } else {
                                        break Loop_loop1496__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1496__2
                        }
                    }
                    var t1494 bool = !saw_digit__92
                    if t1494 {
                        var inline2244 FloatNatural = float_natural_zero()
                        var inline2245 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2244,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2245
                    } else {
                        var jp1410 uint8
                        if jp1403 {
                            jp1410 = 112
                        } else {
                            jp1410 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1489 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1490 bool = index__85 < t1489
                        var jp1427 bool
                        if t1490 {
                            var t1491 uint8
                            var inline2247 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1491 = inline2247
                            var t1492 uint8 = ascii_lower(t1491)
                            var t1493 bool = t1492 == jp1410
                            jp1427 = t1493
                        } else {
                            jp1427 = false
                        }
                        if jp1427 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1428 int = compound_old183 + compound_value184
                            index__85 = t1428
                            var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1480 bool = index__85 < t1479
                            var jp1474 bool
                            if t1480 {
                                var t1483 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1484 bool = t1483 == 43
                                if t1484 {
                                    jp1474 = true
                                } else {
                                    var t1485 uint8
                                    var inline2249 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1485 = inline2249
                                    var t1486 bool = t1485 == 45
                                    jp1474 = t1486
                                }
                            } else {
                                jp1474 = false
                            }
                            if jp1474 {
                                var t1475 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1476 bool = t1475 == 45
                                exponent_negative__104 = t1476
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1477 int = compound_old187 + compound_value188
                                index__85 = t1477
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1435__2:
                            for {
                                var t1436 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1437 bool = index__85 < t1436
                                if t1437 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1471 bool = current__106 >= 48
                                    var jp1440 bool
                                    if t1471 {
                                        var t1472 bool = current__106 <= 57
                                        jp1440 = t1472
                                    } else {
                                        jp1440 = false
                                    }
                                    if jp1440 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1444 bool = exponent__103 < 1000000
                                        if t1444 {
                                            var t1445 int = exponent__103 * 10
                                            var t1446 uint8 = current__106 - 48
                                            var t1447 int = int(uint8(t1446))
                                            var t1448 int = t1445 + t1447
                                            exponent__103 = t1448
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1442 int = compound_old196 + compound_value197
                                        index__85 = t1442
                                        continue
                                    } else {
                                        var t1450 bool = current__106 == 95
                                        if t1450 {
                                            var t1467 bool = !previous_digit__96
                                            var jp1463 bool
                                            if t1467 {
                                                jp1463 = true
                                            } else {
                                                var t1468 int = index__85 + 1
                                                var t1469 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1470 bool = t1468 >= t1469
                                                jp1463 = t1470
                                            }
                                            var jp1458 bool
                                            if jp1463 {
                                                jp1458 = true
                                            } else {
                                                var t1464 int = index__85 + 1
                                                var t1465 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1464)
                                                var t1466 bool = t1465 < 48
                                                jp1458 = t1466
                                            }
                                            var jp1455 bool
                                            if jp1458 {
                                                jp1455 = true
                                            } else {
                                                var t1459 int = index__85 + 1
                                                var t1460 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1459)
                                                var t1461 bool = t1460 > 57
                                                jp1455 = t1461
                                            }
                                            if jp1455 {
                                                var t1456 ParsedFloat = invalid_parsed_float()
                                                return t1456
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1452 int = compound_old201 + compound_value202
                                                index__85 = t1452
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1435__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1435__2
                                }
                            }
                            var t1433 bool = !exponent_digits__105
                            if t1433 {
                                var t1434 ParsedFloat = invalid_parsed_float()
                                return t1434
                            } else {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 != t1423
                                if t1424 {
                                    var t1425 ParsedFloat = invalid_parsed_float()
                                    return t1425
                                } else {
                                    if exponent_negative__104 {
                                        var t1422 int = 0 - exponent__103
                                        exponent__103 = t1422
                                    } else {}
                                    var jp1415 int
                                    if jp1403 {
                                        jp1415 = 0
                                    } else {
                                        var t1421 int = exponent__103 - fraction_digits__94
                                        jp1415 = t1421
                                    }
                                    var jp1417 int
                                    if jp1403 {
                                        var t1419 int = fraction_digits__94 * 4
                                        var t1420 int = exponent__103 - t1419
                                        jp1417 = t1420
                                    } else {
                                        jp1417 = 0
                                    }
                                    var t1418 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1415,
                                        binary_exponent: jp1417,
                                        hexadecimal: jp1403,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1418
                                }
                            }
                        } else {
                            if jp1403 {
                                var t1488 ParsedFloat = invalid_parsed_float()
                                return t1488
                            } else {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 != t1423
                                if t1424 {
                                    var t1425 ParsedFloat = invalid_parsed_float()
                                    return t1425
                                } else {
                                    if exponent_negative__104 {
                                        var t1422 int = 0 - exponent__103
                                        exponent__103 = t1422
                                    } else {}
                                    var jp1415 int
                                    if jp1403 {
                                        jp1415 = 0
                                    } else {
                                        var t1421 int = exponent__103 - fraction_digits__94
                                        jp1415 = t1421
                                    }
                                    var jp1417 int
                                    if jp1403 {
                                        var t1419 int = fraction_digits__94 * 4
                                        var t1420 int = exponent__103 - t1419
                                        jp1417 = t1420
                                    } else {
                                        jp1417 = 0
                                    }
                                    var t1418 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1415,
                                        binary_exponent: jp1417,
                                        hexadecimal: jp1403,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1418
                                }
                            }
                        }
                    }
                }
            } else {
                jp1403 = false
                if jp1403 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1547 int = compound_old145 + compound_value146
                    index__85 = t1547
                } else {}
                var mantissa_start__89 int = index__85
                var jp1406 int
                if jp1403 {
                    jp1406 = 16
                } else {
                    jp1406 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1500 uint32 = uint32(int(jp1406))
                Loop_loop1496__3:
                for {
                    var t1497 int
                    var inline2242 int = _goml_runtime_core_string_len(value__84)
                    t1497 = inline2242
                    var t1498 bool = index__85 < t1497
                    if t1498 {
                        var current__97 uint8
                        var inline2240 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2240
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1406)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1500)
                            var t1501 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1501)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1512 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1512
                            } else {}
                            var t1510 bool = significant_digits__95 > 0
                            var jp1507 bool
                            if t1510 {
                                jp1507 = true
                            } else {
                                var t1511 bool = x151 != 0
                                jp1507 = t1511
                            }
                            if jp1507 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1508 int = compound_old160 + compound_value161
                                significant_digits__95 = t1508
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1504 int = compound_old164 + compound_value165
                            index__85 = t1504
                            continue
                        } else {
                            var t1515 bool = current__97 == 95
                            if t1515 {
                                var t1536 int = index__85 + 1
                                var t1537 int
                                var inline2238 int = _goml_runtime_core_string_len(value__84)
                                t1537 = inline2238
                                var t1538 bool = t1536 >= t1537
                                if t1538 {
                                    var inline2230 FloatNatural = float_natural_zero()
                                    var inline2231 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2230,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2231
                                } else {
                                    var t1517 int = index__85 + 1
                                    var t1518 uint8
                                    var inline2236 uint8 = _goml_runtime_core_string_byte_get(value__84, t1517)
                                    t1518 = inline2236
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1518, jp1406)
                                    var x169 bool = mtmp168._0
                                    var jp1533 bool
                                    if jp1403 {
                                        var t1535 bool = !saw_digit__92
                                        jp1533 = t1535
                                    } else {
                                        jp1533 = false
                                    }
                                    var jp1520 bool
                                    if jp1533 {
                                        var t1534 bool = index__85 == mantissa_start__89
                                        jp1520 = t1534
                                    } else {
                                        jp1520 = false
                                    }
                                    var t1530 bool = !previous_digit__96
                                    var jp1528 bool
                                    if t1530 {
                                        var t1531 bool = !jp1520
                                        jp1528 = t1531
                                    } else {
                                        jp1528 = false
                                    }
                                    var jp1525 bool
                                    if jp1528 {
                                        jp1525 = true
                                    } else {
                                        var t1529 bool = !x169
                                        jp1525 = t1529
                                    }
                                    if jp1525 {
                                        var inline2233 FloatNatural = float_natural_zero()
                                        var inline2234 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2233,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2234
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1522 int = compound_old173 + compound_value174
                                        index__85 = t1522
                                        continue
                                    }
                                }
                            } else {
                                var t1545 bool = current__97 == 46
                                var jp1542 bool
                                if t1545 {
                                    var t1546 bool = !saw_dot__93
                                    jp1542 = t1546
                                } else {
                                    jp1542 = false
                                }
                                if jp1542 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1543 int = compound_old178 + compound_value179
                                    index__85 = t1543
                                    continue
                                } else {
                                    break Loop_loop1496__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1496__3
                    }
                }
                var t1494 bool = !saw_digit__92
                if t1494 {
                    var inline2244 FloatNatural = float_natural_zero()
                    var inline2245 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2244,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2245
                } else {
                    var jp1410 uint8
                    if jp1403 {
                        jp1410 = 112
                    } else {
                        jp1410 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1489 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1490 bool = index__85 < t1489
                    var jp1427 bool
                    if t1490 {
                        var t1491 uint8
                        var inline2247 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1491 = inline2247
                        var t1492 uint8 = ascii_lower(t1491)
                        var t1493 bool = t1492 == jp1410
                        jp1427 = t1493
                    } else {
                        jp1427 = false
                    }
                    if jp1427 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1428 int = compound_old183 + compound_value184
                        index__85 = t1428
                        var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1480 bool = index__85 < t1479
                        var jp1474 bool
                        if t1480 {
                            var t1483 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1484 bool = t1483 == 43
                            if t1484 {
                                jp1474 = true
                            } else {
                                var t1485 uint8
                                var inline2249 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1485 = inline2249
                                var t1486 bool = t1485 == 45
                                jp1474 = t1486
                            }
                        } else {
                            jp1474 = false
                        }
                        if jp1474 {
                            var t1475 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1476 bool = t1475 == 45
                            exponent_negative__104 = t1476
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1477 int = compound_old187 + compound_value188
                            index__85 = t1477
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1435__3:
                        for {
                            var t1436 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1437 bool = index__85 < t1436
                            if t1437 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1471 bool = current__106 >= 48
                                var jp1440 bool
                                if t1471 {
                                    var t1472 bool = current__106 <= 57
                                    jp1440 = t1472
                                } else {
                                    jp1440 = false
                                }
                                if jp1440 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1444 bool = exponent__103 < 1000000
                                    if t1444 {
                                        var t1445 int = exponent__103 * 10
                                        var t1446 uint8 = current__106 - 48
                                        var t1447 int = int(uint8(t1446))
                                        var t1448 int = t1445 + t1447
                                        exponent__103 = t1448
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1442 int = compound_old196 + compound_value197
                                    index__85 = t1442
                                    continue
                                } else {
                                    var t1450 bool = current__106 == 95
                                    if t1450 {
                                        var t1467 bool = !previous_digit__96
                                        var jp1463 bool
                                        if t1467 {
                                            jp1463 = true
                                        } else {
                                            var t1468 int = index__85 + 1
                                            var t1469 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1470 bool = t1468 >= t1469
                                            jp1463 = t1470
                                        }
                                        var jp1458 bool
                                        if jp1463 {
                                            jp1458 = true
                                        } else {
                                            var t1464 int = index__85 + 1
                                            var t1465 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1464)
                                            var t1466 bool = t1465 < 48
                                            jp1458 = t1466
                                        }
                                        var jp1455 bool
                                        if jp1458 {
                                            jp1455 = true
                                        } else {
                                            var t1459 int = index__85 + 1
                                            var t1460 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1459)
                                            var t1461 bool = t1460 > 57
                                            jp1455 = t1461
                                        }
                                        if jp1455 {
                                            var t1456 ParsedFloat = invalid_parsed_float()
                                            return t1456
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1452 int = compound_old201 + compound_value202
                                            index__85 = t1452
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1435__3
                                    }
                                }
                            } else {
                                break Loop_loop1435__3
                            }
                        }
                        var t1433 bool = !exponent_digits__105
                        if t1433 {
                            var t1434 ParsedFloat = invalid_parsed_float()
                            return t1434
                        } else {
                            var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1424 bool = index__85 != t1423
                            if t1424 {
                                var t1425 ParsedFloat = invalid_parsed_float()
                                return t1425
                            } else {
                                if exponent_negative__104 {
                                    var t1422 int = 0 - exponent__103
                                    exponent__103 = t1422
                                } else {}
                                var jp1415 int
                                if jp1403 {
                                    jp1415 = 0
                                } else {
                                    var t1421 int = exponent__103 - fraction_digits__94
                                    jp1415 = t1421
                                }
                                var jp1417 int
                                if jp1403 {
                                    var t1419 int = fraction_digits__94 * 4
                                    var t1420 int = exponent__103 - t1419
                                    jp1417 = t1420
                                } else {
                                    jp1417 = 0
                                }
                                var t1418 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1415,
                                    binary_exponent: jp1417,
                                    hexadecimal: jp1403,
                                    significant_digits: significant_digits__95,
                                }
                                return t1418
                            }
                        }
                    } else {
                        if jp1403 {
                            var t1488 ParsedFloat = invalid_parsed_float()
                            return t1488
                        } else {
                            var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1424 bool = index__85 != t1423
                            if t1424 {
                                var t1425 ParsedFloat = invalid_parsed_float()
                                return t1425
                            } else {
                                if exponent_negative__104 {
                                    var t1422 int = 0 - exponent__103
                                    exponent__103 = t1422
                                } else {}
                                var jp1415 int
                                if jp1403 {
                                    jp1415 = 0
                                } else {
                                    var t1421 int = exponent__103 - fraction_digits__94
                                    jp1415 = t1421
                                }
                                var jp1417 int
                                if jp1403 {
                                    var t1419 int = fraction_digits__94 * 4
                                    var t1420 int = exponent__103 - t1419
                                    jp1417 = t1420
                                } else {
                                    jp1417 = 0
                                }
                                var t1418 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1415,
                                    binary_exponent: jp1417,
                                    hexadecimal: jp1403,
                                    significant_digits: significant_digits__95,
                                }
                                return t1418
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
    var inline2251 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2251, 1)
    var inline2253 FloatNatural = FloatNatural{
        words: inline2251,
    }
    result__26 = inline2253
    var count__27 int = 0
    Loop_loop1586:
    for {
        var t1587 bool = count__27 < exponent__25
        if t1587 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1588 int = compound_old46 + compound_value47
            count__27 = t1588
            continue
        } else {
            break Loop_loop1586
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1675 bool
    var inline2255 *_goml_vec_uint32 = numerator__65.words
    var inline2256 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2255)
    t1675 = inline2256
    if t1675 {
        var t1676 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1676
    } else {
        var t1672 bool = binary_shift__67 >= 0
        var jp1597 FloatNatural
        if t1672 {
            var t1673 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1597 = t1673
        } else {
            var t1674 FloatNatural = float_natural_copy(numerator__65)
            jp1597 = t1674
        }
        var t1668 bool = binary_shift__67 >= 0
        var jp1599 FloatNatural
        if t1668 {
            var t1669 FloatNatural = float_natural_copy(denominator__66)
            jp1599 = t1669
        } else {
            var t1670 int = 0 - binary_shift__67
            var t1671 FloatNatural = float_natural_shift_left(denominator__66, t1670)
            jp1599 = t1671
        }
        var t1600 int = float_natural_bit_length(jp1597)
        var t1601 int = float_natural_bit_length(jp1599)
        var exponent__72 int = t1600 - t1601
        var t1662 bool = exponent__72 >= 0
        var jp1603 int
        if t1662 {
            var t1663 FloatNatural = float_natural_shift_left(jp1599, exponent__72)
            var t1664 int = float_natural_compare(jp1597, t1663)
            jp1603 = t1664
        } else {
            var t1665 int = 0 - exponent__72
            var t1666 FloatNatural = float_natural_shift_left(jp1597, t1665)
            var t1667 int = float_natural_compare(t1666, jp1599)
            jp1603 = t1667
        }
        var t1659 bool = jp1603 < 0
        if t1659 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1660 int = compound_old120 - compound_value121
            exponent__72 = t1660
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1653 bool = exponent__72 > exponent_bias__69
        if t1653 {
            var t1654 int = exponent_bias__69 + exponent_bias__69
            var t1655 int = t1654 + 1
            var t1656 uint64 = uint64(int(t1655))
            var t1657 uint64 = t1656 << mantissa_bits__68
            var t1658 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1657,
                _1: true,
            }
            return t1658
        } else {
            var t1648 bool = exponent__72 < minimum_exponent__74
            var jp1607 uint64
            if t1648 {
                var t1649 int = mantissa_bits__68 - minimum_exponent__74
                var t1650 uint64 = float_rational_quotient(jp1597, jp1599, t1649)
                jp1607 = t1650
            } else {
                var t1651 int = mantissa_bits__68 - exponent__72
                var t1652 uint64 = float_rational_quotient(jp1597, jp1599, t1651)
                jp1607 = t1652
            }
            var mantissa__76 uint64 = jp1607
            var t1610 bool = exponent__72 < minimum_exponent__74
            if t1610 {
                var t1613 bool = mantissa__76 == 0
                if t1613 {
                    var t1614 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1614
                } else {
                    var t1617_lhs uint64 = 1
                    var t1617 uint64 = t1617_lhs << mantissa_bits__68
                    var t1618 bool = mantissa__76 >= t1617
                    if t1618 {
                        var t1619_lhs uint64 = 1
                        var t1619 uint64 = t1619_lhs << mantissa_bits__68
                        var t1620_lhs uint64 = 1
                        var t1620 uint64 = t1620_lhs << mantissa_bits__68
                        var t1621 uint64 = mantissa__76 - t1620
                        var t1622 uint64 = t1619 | t1621
                        var t1623 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1622,
                            _1: false,
                        }
                        return t1623
                    } else {
                        var t1624 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1624
                    }
                }
            } else {
                var t1641 int = mantissa_bits__68 + 1
                var t1642_lhs uint64 = 1
                var t1642 uint64 = t1642_lhs << t1641
                var t1643 bool = mantissa__76 >= t1642
                if t1643 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1644 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1644
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1646 int = compound_old128 + compound_value129
                    exponent__72 = t1646
                } else {}
                var t1628 bool = exponent__72 > exponent_bias__69
                if t1628 {
                    var t1629 int = exponent_bias__69 + exponent_bias__69
                    var t1630 int = t1629 + 1
                    var t1631 uint64 = uint64(int(t1630))
                    var t1632 uint64 = t1631 << mantissa_bits__68
                    var t1633 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1632,
                        _1: true,
                    }
                    return t1633
                } else {
                    var t1634 int = exponent__72 + exponent_bias__69
                    var t1635 uint64 = uint64(int(t1634))
                    var t1636 uint64 = t1635 << mantissa_bits__68
                    var t1637_lhs uint64 = 1
                    var t1637 uint64 = t1637_lhs << mantissa_bits__68
                    var t1638 uint64 = mantissa__76 - t1637
                    var t1639 uint64 = t1636 | t1638
                    var t1640 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1639,
                        _1: false,
                    }
                    return t1640
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1681 int = vec_len__Vec_6uint32(self__528)
    var t1682 bool = t1681 == 0
    return t1682
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1685:
    for {
        var t1693 *_goml_vec_uint32 = value__7.words
        var t1694 bool
        var inline2267 int = vec_len__Vec_6uint32(t1693)
        var inline2268 bool = inline2267 == 0
        t1694 = inline2268
        var t1695 bool = !t1694
        var jp1687 bool
        if t1695 {
            var t1696 *_goml_vec_uint32 = value__7.words
            var t1697 *_goml_vec_uint32 = value__7.words
            var t1698 int
            var inline2261 int = vec_len__Vec_6uint32(t1697)
            t1698 = inline2261
            var t1699 int = t1698 - 1
            var t1700 uint32 = vec_get__Vec_6uint32(t1696, t1699)
            var t1701 bool = t1700 == 0
            jp1687 = t1701
        } else {
            jp1687 = false
        }
        if jp1687 {
            var t1688 *_goml_vec_uint32 = value__7.words
            var t1689 *_goml_vec_uint32 = value__7.words
            var t1690 int
            var inline2265 int = vec_len__Vec_6uint32(t1689)
            t1690 = inline2265
            var t1691 int = t1690 - 1
            vec_truncate__Vec_6uint32(t1688, t1691)
            continue
        } else {
            break Loop_loop1685
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1710 bool = string_is_char_boundary(value__274, start__275)
    var jp1707 bool
    if t1710 {
        var t1711 bool = string_is_char_boundary(value__274, end__276)
        jp1707 = t1711
    } else {
        jp1707 = false
    }
    if jp1707 {
        var t1708 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1708
    } else {
        var t1709 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1709
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1726 int
    var inline2285 int = _goml_runtime_core_string_len(value__78)
    t1726 = inline2285
    var t1727 int
    var inline2283 int = _goml_runtime_core_string_len(expected__79)
    t1727 = inline2283
    var t1728 bool = t1726 != t1727
    if t1728 {
        return false
    } else {
        var index__80 int = 0
        var inline2275 uint8 = 97 - 65
        Loop_loop1716:
        for {
            var t1717 int
            var inline2281 int = _goml_runtime_core_string_len(value__78)
            t1717 = inline2281
            var t1718 bool = index__80 < t1717
            if t1718 {
                var t1722 uint8
                var inline2279 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1722 = inline2279
                var t1723 uint8
                var inline2272 bool = t1722 >= 65
                var inline2274 bool
                if inline2272 {
                    var inline2277 bool = t1722 <= 90
                    inline2274 = inline2277
                } else {
                    inline2274 = false
                }
                if inline2274 {
                    var inline2276 uint8 = t1722 + inline2275
                    t1723 = inline2276
                    var t1724 uint8
                    var inline2270 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1724 = inline2270
                    var t1725 bool = t1723 != t1724
                    if t1725 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1720 int = compound_old134 + compound_value135
                        index__80 = t1720
                        continue
                    }
                } else {
                    t1723 = t1722
                    var t1724 uint8
                    var inline2270 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1724 = inline2270
                    var t1725 bool = t1723 != t1724
                    if t1725 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1720 int = compound_old134 + compound_value135
                        index__80 = t1720
                        continue
                    }
                }
            } else {
                break Loop_loop1716
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1737 bool = value__77 >= 65
    var jp1734 bool
    if t1737 {
        var t1738 bool = value__77 <= 90
        jp1734 = t1738
    } else {
        jp1734 = false
    }
    if jp1734 {
        var t1735 uint8 = 97 - 65
        var t1736 uint8 = value__77 + t1735
        return t1736
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1765 bool = value__81 >= 48
    var jp1749 bool
    if t1765 {
        var t1766 bool = value__81 <= 57
        jp1749 = t1766
    } else {
        jp1749 = false
    }
    var jp1742 int
    if jp1749 {
        var t1750 uint8 = value__81 - 48
        var t1751 int = int(uint8(t1750))
        jp1742 = t1751
        var t1745 bool = jp1742 < base__82
        if t1745 {
            var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1742,
            }
            return t1746
        } else {
            var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1747
        }
    } else {
        var t1761 uint8
        var inline2301 bool = value__81 >= 65
        var inline2303 bool
        if inline2301 {
            var inline2306 bool = value__81 <= 90
            inline2303 = inline2306
        } else {
            inline2303 = false
        }
        if inline2303 {
            var inline2304 uint8 = 97 - 65
            var inline2305 uint8 = value__81 + inline2304
            t1761 = inline2305
            var t1762 bool = t1761 >= 97
            var jp1755 bool
            if t1762 {
                var t1763 uint8
                var inline2287 bool = value__81 >= 65
                var inline2289 bool
                if inline2287 {
                    var inline2292 bool = value__81 <= 90
                    inline2289 = inline2292
                } else {
                    inline2289 = false
                }
                if inline2289 {
                    var inline2290 uint8 = 97 - 65
                    var inline2291 uint8 = value__81 + inline2290
                    t1763 = inline2291
                    var t1764 bool = t1763 <= 102
                    jp1755 = t1764
                    if jp1755 {
                        var t1756 uint8
                        var inline2294 bool = value__81 >= 65
                        var inline2296 bool
                        if inline2294 {
                            var inline2299 bool = value__81 <= 90
                            inline2296 = inline2299
                        } else {
                            inline2296 = false
                        }
                        if inline2296 {
                            var inline2297 uint8 = 97 - 65
                            var inline2298 uint8 = value__81 + inline2297
                            t1756 = inline2298
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        } else {
                            t1756 = value__81
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        }
                    } else {
                        var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1760
                    }
                } else {
                    t1763 = value__81
                    var t1764 bool = t1763 <= 102
                    jp1755 = t1764
                    if jp1755 {
                        var t1756 uint8
                        var inline2294 bool = value__81 >= 65
                        var inline2296 bool
                        if inline2294 {
                            var inline2299 bool = value__81 <= 90
                            inline2296 = inline2299
                        } else {
                            inline2296 = false
                        }
                        if inline2296 {
                            var inline2297 uint8 = 97 - 65
                            var inline2298 uint8 = value__81 + inline2297
                            t1756 = inline2298
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        } else {
                            t1756 = value__81
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        }
                    } else {
                        var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1760
                    }
                }
            } else {
                jp1755 = false
                if jp1755 {
                    var t1756 uint8
                    var inline2294 bool = value__81 >= 65
                    var inline2296 bool
                    if inline2294 {
                        var inline2299 bool = value__81 <= 90
                        inline2296 = inline2299
                    } else {
                        inline2296 = false
                    }
                    if inline2296 {
                        var inline2297 uint8 = 97 - 65
                        var inline2298 uint8 = value__81 + inline2297
                        t1756 = inline2298
                        var t1757 uint8 = t1756 - 97
                        var t1758 uint8 = t1757 + 10
                        var t1759 int = int(uint8(t1758))
                        jp1742 = t1759
                        var t1745 bool = jp1742 < base__82
                        if t1745 {
                            var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1742,
                            }
                            return t1746
                        } else {
                            var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1747
                        }
                    } else {
                        t1756 = value__81
                        var t1757 uint8 = t1756 - 97
                        var t1758 uint8 = t1757 + 10
                        var t1759 int = int(uint8(t1758))
                        jp1742 = t1759
                        var t1745 bool = jp1742 < base__82
                        if t1745 {
                            var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1742,
                            }
                            return t1746
                        } else {
                            var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1747
                        }
                    }
                } else {
                    var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1760
                }
            }
        } else {
            t1761 = value__81
            var t1762 bool = t1761 >= 97
            var jp1755 bool
            if t1762 {
                var t1763 uint8
                var inline2287 bool = value__81 >= 65
                var inline2289 bool
                if inline2287 {
                    var inline2292 bool = value__81 <= 90
                    inline2289 = inline2292
                } else {
                    inline2289 = false
                }
                if inline2289 {
                    var inline2290 uint8 = 97 - 65
                    var inline2291 uint8 = value__81 + inline2290
                    t1763 = inline2291
                    var t1764 bool = t1763 <= 102
                    jp1755 = t1764
                    if jp1755 {
                        var t1756 uint8
                        var inline2294 bool = value__81 >= 65
                        var inline2296 bool
                        if inline2294 {
                            var inline2299 bool = value__81 <= 90
                            inline2296 = inline2299
                        } else {
                            inline2296 = false
                        }
                        if inline2296 {
                            var inline2297 uint8 = 97 - 65
                            var inline2298 uint8 = value__81 + inline2297
                            t1756 = inline2298
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        } else {
                            t1756 = value__81
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        }
                    } else {
                        var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1760
                    }
                } else {
                    t1763 = value__81
                    var t1764 bool = t1763 <= 102
                    jp1755 = t1764
                    if jp1755 {
                        var t1756 uint8
                        var inline2294 bool = value__81 >= 65
                        var inline2296 bool
                        if inline2294 {
                            var inline2299 bool = value__81 <= 90
                            inline2296 = inline2299
                        } else {
                            inline2296 = false
                        }
                        if inline2296 {
                            var inline2297 uint8 = 97 - 65
                            var inline2298 uint8 = value__81 + inline2297
                            t1756 = inline2298
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        } else {
                            t1756 = value__81
                            var t1757 uint8 = t1756 - 97
                            var t1758 uint8 = t1757 + 10
                            var t1759 int = int(uint8(t1758))
                            jp1742 = t1759
                            var t1745 bool = jp1742 < base__82
                            if t1745 {
                                var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1742,
                                }
                                return t1746
                            } else {
                                var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1747
                            }
                        }
                    } else {
                        var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1760
                    }
                }
            } else {
                jp1755 = false
                if jp1755 {
                    var t1756 uint8
                    var inline2294 bool = value__81 >= 65
                    var inline2296 bool
                    if inline2294 {
                        var inline2299 bool = value__81 <= 90
                        inline2296 = inline2299
                    } else {
                        inline2296 = false
                    }
                    if inline2296 {
                        var inline2297 uint8 = 97 - 65
                        var inline2298 uint8 = value__81 + inline2297
                        t1756 = inline2298
                        var t1757 uint8 = t1756 - 97
                        var t1758 uint8 = t1757 + 10
                        var t1759 int = int(uint8(t1758))
                        jp1742 = t1759
                        var t1745 bool = jp1742 < base__82
                        if t1745 {
                            var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1742,
                            }
                            return t1746
                        } else {
                            var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1747
                        }
                    } else {
                        t1756 = value__81
                        var t1757 uint8 = t1756 - 97
                        var t1758 uint8 = t1757 + 10
                        var t1759 int = int(uint8(t1758))
                        jp1742 = t1759
                        var t1745 bool = jp1742 < base__82
                        if t1745 {
                            var t1746 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1742,
                            }
                            return t1746
                        } else {
                            var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1747
                        }
                    }
                } else {
                    var t1760 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1760
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1769:
    for {
        var t1770 bool = carry__22 != 0
        if t1770 {
            var t1779 *_goml_vec_uint32 = value__20.words
            var t1780 int
            var inline2311 int = vec_len__Vec_6uint32(t1779)
            t1780 = inline2311
            var t1781 bool = index__23 == t1780
            if t1781 {
                var t1782 *_goml_vec_uint32 = value__20.words
                var inline2308 uint32 = 0
                vec_push__Vec_6uint32(t1782, inline2308)
            } else {}
            var t1772 *_goml_vec_uint32 = value__20.words
            var t1773 uint32 = vec_get__Vec_6uint32(t1772, index__23)
            var t1774 uint64 = uint64(uint32(t1773))
            var sum__24 uint64 = t1774 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1776_rhs int = 32
            var t1776 uint64 = sum__24 >> t1776_rhs
            carry__22 = t1776
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1777 int = compound_old42 + compound_value43
            index__23 = t1777
            continue
        } else {
            break Loop_loop1769
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1786 FloatNatural
    var inline2313 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2314 FloatNatural = FloatNatural{
        words: inline2313,
    }
    t1786 = inline2314
    var t1787 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1786,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1787
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1807 *_goml_vec_uint32 = value__9.words
    var t1808 bool
    var inline2320 int = vec_len__Vec_6uint32(t1807)
    var inline2321 bool = inline2320 == 0
    t1808 = inline2321
    if t1808 {
        return 0
    } else {
        var t1791 *_goml_vec_uint32 = value__9.words
        var t1792 *_goml_vec_uint32 = value__9.words
        var t1793 int
        var inline2318 int = vec_len__Vec_6uint32(t1792)
        t1793 = inline2318
        var t1794 int = t1793 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1791, t1794)
        var bits__11 int = 0
        Loop_loop1801:
        for {
            var t1802 bool = high__10 != 0
            if t1802 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1803 uint32 = compound_old9 >> compound_value10
                high__10 = t1803
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1805 int = compound_old12 + compound_value13
                bits__11 = t1805
                continue
            } else {
                break Loop_loop1801
            }
        }
        var t1796 *_goml_vec_uint32 = value__9.words
        var t1797 int
        var inline2316 int = vec_len__Vec_6uint32(t1796)
        t1797 = inline2316
        var t1798 int = t1797 - 1
        var t1799 int = t1798 * 32
        var t1800 int = t1799 + bits__11
        return t1800
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1830 *_goml_vec_uint32 = left__12.words
    var t1831 int
    var inline2331 int = vec_len__Vec_6uint32(t1830)
    t1831 = inline2331
    var t1832 *_goml_vec_uint32 = right__13.words
    var t1833 int
    var inline2329 int = vec_len__Vec_6uint32(t1832)
    t1833 = inline2329
    var t1834 bool = t1831 < t1833
    if t1834 {
        return -1
    } else {
        var t1836 *_goml_vec_uint32 = left__12.words
        var t1837 int
        var inline2325 int = vec_len__Vec_6uint32(t1836)
        t1837 = inline2325
        var t1838 *_goml_vec_uint32 = right__13.words
        var t1839 int
        var inline2323 int = vec_len__Vec_6uint32(t1838)
        t1839 = inline2323
        var t1840 bool = t1837 > t1839
        if t1840 {
            return 1
        } else {
            var t1812 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2327 int = vec_len__Vec_6uint32(t1812)
            index__14 = inline2327
            Loop_loop1814:
            for {
                var t1815 bool = index__14 > 0
                if t1815 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1816 int = compound_old17 - compound_value18
                    index__14 = t1816
                    var t1819 *_goml_vec_uint32 = left__12.words
                    var t1820 uint32 = vec_get__Vec_6uint32(t1819, index__14)
                    var t1821 *_goml_vec_uint32 = right__13.words
                    var t1822 uint32 = vec_get__Vec_6uint32(t1821, index__14)
                    var t1823 bool = t1820 < t1822
                    if t1823 {
                        return -1
                    } else {
                        var t1825 *_goml_vec_uint32 = left__12.words
                        var t1826 uint32 = vec_get__Vec_6uint32(t1825, index__14)
                        var t1827 *_goml_vec_uint32 = right__13.words
                        var t1828 uint32 = vec_get__Vec_6uint32(t1827, index__14)
                        var t1829 bool = t1826 > t1828
                        if t1829 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1814
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1876 bool = shift__57 >= 0
    var jp1844 FloatNatural
    if t1876 {
        var t1877 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1844 = t1877
    } else {
        var t1878 FloatNatural = float_natural_copy(numerator__55)
        jp1844 = t1878
    }
    var t1872 bool = shift__57 >= 0
    var jp1846 FloatNatural
    if t1872 {
        var t1873 FloatNatural = float_natural_copy(denominator__56)
        jp1846 = t1873
    } else {
        var t1874 int = 0 - shift__57
        var t1875 FloatNatural = float_natural_shift_left(denominator__56, t1874)
        jp1846 = t1875
    }
    var quotient__60 uint64 = 0
    Loop_loop1859:
    for {
        var t1860 int = float_natural_compare(jp1844, jp1846)
        var t1861 bool = t1860 >= 0
        if t1861 {
            var t1862 int = float_natural_bit_length(jp1844)
            var t1863 int = float_natural_bit_length(jp1846)
            var offset__61 int = t1862 - t1863
            var part__62 FloatNatural = float_natural_shift_left(jp1846, offset__61)
            var t1867 int = float_natural_compare(jp1844, part__62)
            var t1868 bool = t1867 < 0
            if t1868 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1869 int = compound_old105 - compound_value106
                offset__61 = t1869
                var t1871 FloatNatural = float_natural_shift_left(jp1846, offset__61)
                part__62 = t1871
            } else {}
            float_natural_subtract(jp1844, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1865 uint64 = compound_old111 | compound_value112
            quotient__60 = t1865
            continue
        } else {
            break Loop_loop1859
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1844, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1846)
    var t1853 bool = rounding__64 > 0
    var jp1850 bool
    if t1853 {
        jp1850 = true
    } else {
        var t1856 bool = rounding__64 == 0
        if t1856 {
            var t1857_rhs uint64 = 1
            var t1857 uint64 = quotient__60 & t1857_rhs
            var t1858 bool = t1857 == 1
            jp1850 = t1858
        } else {
            jp1850 = false
        }
    }
    if jp1850 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1851 uint64 = compound_old115 + compound_value116
        quotient__60 = t1851
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1894 bool = index__269 < 0
    var jp1886 bool
    if t1894 {
        jp1886 = true
    } else {
        var t1895 int
        var inline2333 int = _goml_runtime_core_string_len(value__268)
        t1895 = inline2333
        var t1896 bool = index__269 > t1895
        jp1886 = t1896
    }
    if jp1886 {
        return false
    } else {
        var t1889 int
        var inline2337 int = _goml_runtime_core_string_len(value__268)
        t1889 = inline2337
        var t1890 bool = index__269 == t1889
        if t1890 {
            return true
        } else {
            var t1891 uint8
            var inline2335 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1891 = inline2335
            var t1892_rhs uint8 = 192
            var t1892 uint8 = t1891 & t1892_rhs
            var t1893 bool = t1892 != 128
            return t1893
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1900:
    for {
        var t1901 *_goml_vec_uint32 = value__37.words
        var t1902 int
        var inline2341 int = vec_len__Vec_6uint32(t1901)
        t1902 = inline2341
        var t1903 bool = index__41 < t1902
        if t1903 {
            var t1917 *_goml_vec_uint32 = other__38.words
            var t1918 int
            var inline2339 int = vec_len__Vec_6uint32(t1917)
            t1918 = inline2339
            var t1919 bool = index__41 < t1918
            var jp1905 uint64
            if t1919 {
                var t1920 *_goml_vec_uint32 = other__38.words
                var t1921 uint32 = vec_get__Vec_6uint32(t1920, index__41)
                var t1922 uint64 = uint64(uint32(t1921))
                jp1905 = t1922
            } else {
                jp1905 = 0
            }
            var right__42 uint64 = jp1905 + borrow__40
            var t1906 *_goml_vec_uint32 = value__37.words
            var t1907 uint32 = vec_get__Vec_6uint32(t1906, index__41)
            var left__43 uint64 = uint64(uint32(t1907))
            var t1911 bool = left__43 >= right__42
            if t1911 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1912 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1912))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1914 uint64 = base__39 + left__43
                var t1915 uint64 = t1914 - right__42
                var value75 uint32 = uint32(uint64(t1915))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1909 int = compound_old79 + compound_value80
            index__41 = t1909
            continue
        } else {
            break Loop_loop1900
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
