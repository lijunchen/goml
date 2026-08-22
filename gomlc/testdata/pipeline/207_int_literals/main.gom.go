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

func _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(arg0 float64) uint64 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float64bits(arg0)
}

func _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(arg0 float32) uint32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32bits(arg0)
}

func array_get__Array_2_5uint8(arr [2]uint8, index int) uint8 {
    return arr[index]
}

func array_get__Array_2_6uint16(arr [2]uint16, index int) uint16 {
    return arr[index]
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

type ref_uint32_x struct {
    value uint32
}

func ref__Ref_6uint32(value uint32) *ref_uint32_x {
    return &ref_uint32_x{
        value: value,
    }
}

func ref_get__Ref_6uint32(reference *ref_uint32_x) uint32 {
    return reference.value
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

func increment(value__0 uint8) uint8 {
    var t809 uint8 = value__0 + 1
    return t809
}

func main0() struct{} {
    var default_integer__2 int = 40
    var default_float__3 float64 = 0.5
    var small__4 uint8 = 41
    var inferred__5 [2]uint8 = [2]uint8{small__4, 2}
    var jp819 uint8
    jp819 = 1
    var values__7 [2]uint16 = [2]uint16{10, 20}
    var delayed__8 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__u32(0)
    var byte__9 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get("A", 0)
    var t820 int = default_integer__2 + 2
    var t821 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t820)
    println__T_string(t821)
    var t822 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(default_float__3)
    println__T_string(t822)
    var t823 uint8 = increment(small__4)
    var t824 string
    var inline1964 string = __goml_builtin_uint8_to_string(t823)
    t824 = inline1964
    var inline1961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t824)
    _goml_runtime_core_string_println(inline1961)
    var t825 uint8 = array_get__Array_2_5uint8(inferred__5, 1)
    var t826 string
    var inline1959 string = __goml_builtin_uint8_to_string(t825)
    t826 = inline1959
    var inline1956 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline1956)
    var t827 string
    var inline1954 string = __goml_builtin_uint8_to_string(jp819)
    t827 = inline1954
    var inline1951 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t827)
    _goml_runtime_core_string_println(inline1951)
    var t828 uint16 = array_get__Array_2_6uint16(values__7, 1)
    var t829 string
    var inline1949 string = __goml_builtin_uint16_to_string(t828)
    t829 = inline1949
    var inline1946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
    _goml_runtime_core_string_println(inline1946)
    var t830 float32
    t830 = 1
    var t831 string
    var inline1943 string = __goml_builtin_float32_to_string(t830)
    t831 = inline1943
    var inline1940 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline1940)
    var t832 int16
    t832 = 2
    var t833 string
    var inline1937 string = __goml_builtin_int16_to_string(t832)
    t833 = inline1937
    var inline1934 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t833)
    _goml_runtime_core_string_println(inline1934)
    var t834 uint32
    var inline1932 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__u32(delayed__8)
    t834 = inline1932
    var t835 string
    var inline1930 string = __goml_builtin_uint32_to_string(t834)
    t835 = inline1930
    var inline1927 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline1927)
    var t836 bool = byte__9 == 65
    var t837 string
    var inline1925 string = _goml_runtime_core_bool_to_string(t836)
    t837 = inline1925
    var inline1922 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
    _goml_runtime_core_string_println(inline1922)
    var jp839 string
    switch byte__9 {
    case 65:
        jp839 = "byte"
    default:
        jp839 = "other"
    }
    var inline1919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp839)
    _goml_runtime_core_string_println(inline1919)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__u32(self__685 *ref_uint32_x) uint32 {
    var t842 uint32 = ref_get__Ref_6uint32(self__685)
    return t842
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__u32(value__684 uint32) *ref_uint32_x {
    var t845 *ref_uint32_x = ref__Ref_6uint32(value__684)
    return t845
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t848 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t848
}

func println__T_string(value__1 string) struct{} {
    var t850 string
    t850 = value__1
    _goml_runtime_core_string_println(t850)
    return struct{}{}
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1967 int64 = int64(int(self__285))
    var inline1968 string = signed_decimal_string(inline1967)
    return inline1968
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__414 float64) string {
    var inline1970 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__414)
    var inline1971 string = format_float_bits(inline1970, 52, 11, 1023)
    return inline1971
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t888 uint64 = uint64(uint8(value__228))
    var t889 string = decimal_string(t888)
    return t889
}

func __goml_builtin_uint16_to_string(value__229 uint16) string {
    var t892 uint64 = uint64(uint16(value__229))
    var t893 string = decimal_string(t892)
    return t893
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t896 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t897 uint64 = uint64(uint32(t896))
    var t898 string = format_float_bits(t897, 23, 8, 127)
    return t898
}

func __goml_builtin_int16_to_string(value__224 int16) string {
    var t901 int64 = int64(int16(value__224))
    var inline1997 bool = t901 < 0
    if inline1997 {
        var inline1998 uint64 = uint64(int64(t901))
        var inline1999 uint64 = 0 - inline1998
        var inline2000 string = decimal_string(inline1999)
        var inline2001 string = "-" + inline2000
        return inline2001
    } else {
        var inline2002 uint64 = uint64(int64(t901))
        var inline2003 string = decimal_string(inline2002)
        return inline2003
    }
}

func __goml_builtin_uint32_to_string(value__230 uint32) string {
    var t905 uint64 = uint64(uint32(value__230))
    var t906 string = decimal_string(t905)
    return t906
}

func signed_decimal_string(value__214 int64) string {
    var t911 bool = value__214 < 0
    if t911 {
        var t912 uint64 = uint64(int64(value__214))
        var t913 uint64 = 0 - t912
        var t914 string = decimal_string(t913)
        var t915 string = "-" + t914
        return t915
    } else {
        var t916 uint64 = uint64(int64(value__214))
        var t917 string = decimal_string(t916)
        return t917
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t920 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t920
    var t921 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t921 != 0
    var t922_lhs uint64 = 1
    var t922 uint64 = t922_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t922 - 1
    var t923 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t923 & exponent_mask__166
    var t924_lhs uint64 = 1
    var t924 uint64 = t924_lhs << mantissa_bits__161
    var t925 uint64 = t924 - 1
    var fraction__168 uint64 = bits__160 & t925
    var t989 bool = exponent__167 == exponent_mask__166
    if t989 {
        var t991 bool = fraction__168 == 0
        if t991 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t997 bool = exponent__167 == 0
        var jp995 bool
        if t997 {
            var t998 bool = fraction__168 == 0
            jp995 = t998
        } else {
            jp995 = false
        }
        if jp995 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t986 bool = exponent__167 == 0
            var jp928 uint64
            if t986 {
                jp928 = fraction__168
            } else {
                var t987_lhs uint64 = 1
                var t987 uint64 = t987_lhs << mantissa_bits__161
                var t988 uint64 = fraction__168 | t987
                jp928 = t988
            }
            var t980 bool = exponent__167 == 0
            var jp930 int
            if t980 {
                var t981 int = 1 - exponent_bias__163
                var t982 int = t981 - mantissa_bits__161
                jp930 = t982
            } else {
                var t983 int = int(uint64(exponent__167))
                var t984 int = t983 - exponent_bias__163
                var t985 int = t984 - mantissa_bits__161
                jp930 = t985
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp928)
            var t935 bool = jp930 >= 0
            var jp932 int
            if t935 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp930)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t954 bool = mantissa_bits__161 == 23
                var jp937 int
                if t954 {
                    jp937 = 9
                } else {
                    jp937 = 17
                }
                var t951 int
                var inline2011 int = _goml_runtime_core_string_len(digits__173)
                t951 = inline2011
                var t952 bool = t951 < jp937
                var jp939 int
                if t952 {
                    var inline2005 int = _goml_runtime_core_string_len(digits__173)
                    jp939 = inline2005
                } else {
                    jp939 = jp937
                }
                var count__176 int = 1
                Loop_loop942:
                for {
                    var t943 bool = count__176 <= jp939
                    if t943 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t944 int
                        var inline2007 int = _goml_runtime_core_string_len(digits__173)
                        t944 = inline2007
                        var jp946 int
                        if x319 {
                            jp946 = 1
                        } else {
                            jp946 = 0
                        }
                        var point__180 int = t944 + jp946
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t950 bool = x322 == bits__160
                        if t950 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t948 int = compound_old324 + compound_value325
                            count__176 = t948
                            continue
                        }
                    } else {
                        break Loop_loop942
                    }
                }
                var inline2009 int = _goml_runtime_core_string_len(digits__173)
                jp932 = inline2009
                var t933 string = float_natural_decimal(exact_value__171)
                var t934 string = fixed_float_text(t933, jp932, negative__165)
                return t934
            } else {
                var count__183 int = 0
                var t976 int = 0 - jp930
                Loop_loop975:
                for {
                    var t977 bool = count__183 < t976
                    if t977 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t978 int = compound_old329 + compound_value330
                        count__183 = t978
                        continue
                    } else {
                        break Loop_loop975
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t956 int
                var inline2017 int = _goml_runtime_core_string_len(digits__184)
                t956 = inline2017
                var point__185 int = t956 + jp930
                var t974 bool = mantissa_bits__161 == 23
                var jp958 int
                if t974 {
                    jp958 = 9
                } else {
                    jp958 = 17
                }
                var t971 int
                var inline2015 int = _goml_runtime_core_string_len(digits__184)
                t971 = inline2015
                var t972 bool = t971 < jp958
                var jp960 int
                if t972 {
                    var inline2013 int = _goml_runtime_core_string_len(digits__184)
                    jp960 = inline2013
                } else {
                    jp960 = jp958
                }
                count__183 = 1
                Loop_loop962:
                for {
                    var t963 bool = count__183 <= jp960
                    if t963 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp965 int
                        if x336 {
                            jp965 = 1
                        } else {
                            jp965 = 0
                        }
                        var t966 int = point__185 + jp965
                        var candidate__191 string = fixed_float_text(rounded__190, t966, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t970 bool = x339 == bits__160
                        if t970 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t968 int = compound_old341 + compound_value342
                            count__183 = t968
                            continue
                        }
                    } else {
                        break Loop_loop962
                    }
                }
                jp932 = point__185
                var t933 string = float_natural_decimal(exact_value__171)
                var t934 string = fixed_float_text(t933, jp932, negative__165)
                return t934
            }
        }
    }
}

func decimal_string(value__208 uint64) string {
    var t1021 bool = value__208 == 0
    if t1021 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1014:
        for {
            var t1015 bool = remaining__210 > 0
            if t1015 {
                var t1016_rhs uint64 = 10
                var t1016 uint64 = remaining__210 % t1016_rhs
                var t1017 uint8 = uint8(uint64(t1016))
                var t1018 uint8 = t1017 + 48
                vec_push__Vec_5uint8(reversed__209, t1018)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1019 uint64 = compound_old353 / compound_value354
                remaining__210 = t1019
                continue
            } else {
                break Loop_loop1014
            }
        }
        var t1003 int
        var inline2027 int = vec_len__Vec_5uint8(reversed__209)
        t1003 = inline2027
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1003)
        var offset__212 int = 0
        Loop_loop1005:
        for {
            var t1006 int
            var inline2025 int = vec_len__Vec_5uint8(reversed__209)
            t1006 = inline2025
            var t1007 bool = offset__212 < t1006
            if t1007 {
                var t1008 int
                var inline2023 int = vec_len__Vec_5uint8(reversed__209)
                t1008 = inline2023
                var t1009 int = t1008 - offset__212
                var t1010 int = t1009 - 1
                var t1011 uint8 = vec_get__Vec_5uint8(reversed__209, t1010)
                vec_push__Vec_5uint8(bytes__211, t1011)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1012 int = compound_old358 + compound_value359
                offset__212 = t1012
                continue
            } else {
                break Loop_loop1005
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline2033 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2034 FloatNatural = FloatNatural{
        words: inline2033,
    }
    result__2 = inline2034
    var t1025 bool = value__1 != 0
    if t1025 {
        var t1026 *_goml_vec_uint32 = result__2.words
        var t1027 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t1026, t1027)
        var t1028_rhs int = 32
        var t1028 uint64 = value__1 >> t1028_rhs
        var high__3 uint32 = uint32(uint64(t1028))
        var t1030 bool = high__3 != 0
        if t1030 {
            var t1031 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t1031, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1060 bool
    var inline2051 *_goml_vec_uint32 = value__28.words
    var inline2052 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2051)
    t1060 = inline2052
    if t1060 {
        var inline2036 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2037 FloatNatural = FloatNatural{
            words: inline2036,
        }
        return inline2037
    } else {
        var t1063 bool = bits__29 == 0
        if t1063 {
            var t1064 FloatNatural = float_natural_copy(value__28)
            return t1064
        } else {
            var result__30 FloatNatural
            var inline2048 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2049 FloatNatural = FloatNatural{
                words: inline2048,
            }
            result__30 = inline2049
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1055:
            for {
                var t1056 bool = index__33 < word_shift__31
                if t1056 {
                    var t1057 *_goml_vec_uint32 = result__30.words
                    var inline2039 uint32 = 0
                    vec_push__Vec_6uint32(t1057, inline2039)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1058 int = compound_old52 + compound_value53
                    index__33 = t1058
                    continue
                } else {
                    break Loop_loop1055
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop1043:
            for {
                var t1044 *_goml_vec_uint32 = value__28.words
                var t1045 int
                var inline2044 int = vec_len__Vec_6uint32(t1044)
                t1045 = inline2044
                var t1046 bool = index__33 < t1045
                if t1046 {
                    var t1047 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1047, index__33)
                    var t1048 uint64 = uint64(uint32(word__35))
                    var t1049 uint64 = t1048 << bit_shift__32
                    var shifted__36 uint64 = t1049 | carry__34
                    var t1050 *_goml_vec_uint32 = result__30.words
                    var t1051 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1050, t1051)
                    var t1052_rhs int = 32
                    var t1052 uint64 = shifted__36 >> t1052_rhs
                    carry__34 = t1052
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1053 int = compound_old59 + compound_value60
                    index__33 = t1053
                    continue
                } else {
                    break Loop_loop1043
                }
            }
            var t1039 bool = carry__34 != 0
            if t1039 {
                var t1040 *_goml_vec_uint32 = result__30.words
                var t1041 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t1040, t1041)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1087 bool
    var inline2067 *_goml_vec_uint32 = value__49.words
    var inline2068 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2067)
    t1087 = inline2068
    if t1087 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1080:
        for {
            var t1081 bool
            var inline2056 *_goml_vec_uint32 = current__50.words
            var inline2057 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2056)
            t1081 = inline2057
            var t1082 bool = !t1081
            if t1082 {
                var t1083 uint32 = float_natural_divide_small(current__50, 10)
                var t1084 uint8 = uint8(uint32(t1083))
                var t1085 uint8 = t1084 + 48
                vec_push__Vec_5uint8(reversed__51, t1085)
                continue
            } else {
                break Loop_loop1080
            }
        }
        var t1069 int
        var inline2065 int = vec_len__Vec_5uint8(reversed__51)
        t1069 = inline2065
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1069)
        var offset__53 int = 0
        Loop_loop1071:
        for {
            var t1072 int
            var inline2063 int = vec_len__Vec_5uint8(reversed__51)
            t1072 = inline2063
            var t1073 bool = offset__53 < t1072
            if t1073 {
                var t1074 int
                var inline2061 int = vec_len__Vec_5uint8(reversed__51)
                t1074 = inline2061
                var t1075 int = t1074 - offset__53
                var t1076 int = t1075 - 1
                var t1077 uint8 = vec_get__Vec_5uint8(reversed__51, t1076)
                vec_push__Vec_5uint8(output__52, t1077)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1078 int = compound_old98 + compound_value99
                offset__53 = t1078
                continue
            } else {
                break Loop_loop1071
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1090 int = _goml_runtime_core_string_len(self__289)
    return t1090
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1093 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1093)
    var index__148 int = 0
    Loop_loop1148:
    for {
        var t1149 bool = index__148 < count__146
        if t1149 {
            var t1150 uint8
            var inline2072 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1150 = inline2072
            vec_push__Vec_5uint8(output__147, t1150)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1151 int = compound_old267 + compound_value268
            index__148 = t1151
            continue
        } else {
            break Loop_loop1148
        }
    }
    var t1145 int
    var inline2093 int = _goml_runtime_core_string_len(exact__145)
    t1145 = inline2093
    var t1146 bool = count__146 == t1145
    if t1146 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1147 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1147
    } else {
        var next__150 uint8
        var inline2091 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2091
        var trailing__151 bool = false
        var t1096 int = count__146 + 1
        index__148 = t1096
        Loop_loop1137:
        for {
            var t1138 int
            var inline2076 int = _goml_runtime_core_string_len(exact__145)
            t1138 = inline2076
            var t1139 bool = index__148 < t1138
            if t1139 {
                var t1143 uint8
                var inline2074 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1143 = inline2074
                var t1144 bool = t1143 != 48
                if t1144 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1141 int = compound_old278 + compound_value279
                index__148 = t1141
                continue
            } else {
                break Loop_loop1137
            }
        }
        var t1125 bool = next__150 > 53
        var jp1099 bool
        if t1125 {
            jp1099 = true
        } else {
            var t1128 bool = next__150 == 53
            if t1128 {
                if trailing__151 {
                    jp1099 = true
                } else {
                    var t1131 int
                    var inline2078 int = vec_len__Vec_5uint8(output__147)
                    t1131 = inline2078
                    var t1132 int = t1131 - 1
                    var t1133 uint8 = vec_get__Vec_5uint8(output__147, t1132)
                    var t1134 uint8 = t1133 - 48
                    var t1135_rhs uint8 = 2
                    var t1135 uint8 = t1134 % t1135_rhs
                    var t1136 bool = t1135 == 1
                    jp1099 = t1136
                }
            } else {
                jp1099 = false
            }
        }
        if jp1099 {
            var index__153 int
            var inline2089 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2089
            Loop_loop1113:
            for {
                var t1114 bool = index__153 > 0
                if t1114 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1115 int = compound_old282 - compound_value283
                    index__153 = t1115
                    var t1118 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1119 bool = t1118 < 57
                    if t1119 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1120 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1120)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1122 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1122
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1113
                }
            }
            var t1103 int
            var inline2087 int = vec_len__Vec_5uint8(output__147)
            t1103 = inline2087
            var t1104 int = t1103 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1104)
            var inline2084 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2084)
            index__153 = 0
            Loop_loop1107:
            for {
                var t1108 int
                var inline2082 int = vec_len__Vec_5uint8(output__147)
                t1108 = inline2082
                var t1109 bool = index__153 < t1108
                if t1109 {
                    var t1110 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1110)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1111 int = compound_old302 + compound_value303
                    index__153 = t1111
                    continue
                } else {
                    break Loop_loop1107
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1106 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1106
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1124 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1124
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2100 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2100
    Loop_loop1157:
    for {
        var t1162 bool = length__159 > 1
        var jp1159 bool
        if t1162 {
            var t1163 int = length__159 - 1
            var t1164 uint8
            var inline2095 uint8 = _goml_runtime_core_string_byte_get(value__158, t1163)
            t1164 = inline2095
            var t1165 bool = t1164 == 48
            jp1159 = t1165
        } else {
            jp1159 = false
        }
        if jp1159 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1160 int = compound_old312 - compound_value313
            length__159 = t1160
            continue
        } else {
            break Loop_loop1157
        }
    }
    var inline2097 int = 0
    var inline2098 string = string_byte_slice(value__158, inline2097, length__159)
    return inline2098
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2102 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2102)
    } else {}
    var t1170 bool = decimal_point__138 <= 0
    if t1170 {
        var inline2117 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2117)
        var inline2114 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2114)
        var index__141 int = 0
        var t1180 int = 0 - decimal_point__138
        Loop_loop1179:
        for {
            var t1181 bool = index__141 < t1180
            if t1181 {
                var inline2105 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2105)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1182 int = compound_old234 + compound_value235
                index__141 = t1182
                continue
            } else {
                break Loop_loop1179
            }
        }
        index__141 = 0
        Loop_loop1173:
        for {
            var t1174 int
            var inline2112 int = _goml_runtime_core_string_len(digits__137)
            t1174 = inline2112
            var t1175 bool = index__141 < t1174
            if t1175 {
                var t1176 uint8
                var inline2110 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1176 = inline2110
                vec_push__Vec_5uint8(bytes__140, t1176)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1177 int = compound_old240 + compound_value241
                index__141 = t1177
                continue
            } else {
                break Loop_loop1173
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1185 int
        var inline2142 int = _goml_runtime_core_string_len(digits__137)
        t1185 = inline2142
        var t1186 bool = decimal_point__138 >= t1185
        if t1186 {
            var index__142 int = 0
            Loop_loop1193:
            for {
                var t1194 int
                var inline2124 int = _goml_runtime_core_string_len(digits__137)
                t1194 = inline2124
                var t1195 bool = index__142 < t1194
                if t1195 {
                    var t1196 uint8
                    var inline2122 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1196 = inline2122
                    vec_push__Vec_5uint8(bytes__140, t1196)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1197 int = compound_old244 + compound_value245
                    index__142 = t1197
                    continue
                } else {
                    break Loop_loop1193
                }
            }
            Loop_loop1189:
            for {
                var t1190 bool = index__142 < decimal_point__138
                if t1190 {
                    var inline2126 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2126)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1191 int = compound_old249 + compound_value250
                    index__142 = t1191
                    continue
                } else {
                    break Loop_loop1189
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1207:
            for {
                var t1208 bool = index__143 < decimal_point__138
                if t1208 {
                    var t1209 uint8
                    var inline2131 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1209 = inline2131
                    vec_push__Vec_5uint8(bytes__140, t1209)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1210 int = compound_old253 + compound_value254
                    index__143 = t1210
                    continue
                } else {
                    break Loop_loop1207
                }
            }
            var inline2139 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2139)
            Loop_loop1201:
            for {
                var t1202 int
                var inline2137 int = _goml_runtime_core_string_len(digits__137)
                t1202 = inline2137
                var t1203 bool = index__143 < t1202
                if t1203 {
                    var t1204 uint8
                    var inline2135 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1204 = inline2135
                    vec_push__Vec_5uint8(bytes__140, t1204)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1205 int = compound_old259 + compound_value260
                    index__143 = t1205
                    continue
                } else {
                    break Loop_loop1201
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
    var t1306 bool = parsed__110.valid
    var t1307 bool = !t1306
    if t1307 {
        var t1308 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1308
    } else {
        var t1300 bool = parsed__110.negative
        var jp1217 uint64
        if t1300 {
            var t1305 bool = mantissa_bits__108 == 23
            var jp1302 int
            if t1305 {
                jp1302 = 8
            } else {
                jp1302 = 11
            }
            var t1303 int = mantissa_bits__108 + jp1302
            var t1304_lhs uint64 = 1
            var t1304 uint64 = t1304_lhs << t1303
            jp1217 = t1304
        } else {
            jp1217 = 0
        }
        var t1299 bool = mantissa_bits__108 == 23
        var jp1219 int
        if t1299 {
            jp1219 = 8
        } else {
            jp1219 = 11
        }
        var t1220_lhs uint64 = 1
        var t1220 uint64 = t1220_lhs << jp1219
        var t1221 uint64 = t1220 - 1
        var exponent_mask__112 uint64 = t1221 << mantissa_bits__108
        var t1277 int = parsed__110.special
        var t1278 bool = t1277 == 1
        if t1278 {
            var t1279 uint64 = jp1217 | exponent_mask__112
            var t1280 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1279,
            }
            return t1280
        } else {
            var t1282 int = parsed__110.special
            var t1283 bool = t1282 == 2
            if t1283 {
                var t1287 int = mantissa_bits__108 - 1
                var t1288_lhs uint64 = 1
                var t1288 uint64 = t1288_lhs << t1287
                var t1289 uint64 = exponent_mask__112 | t1288
                var t1294 bool = mantissa_bits__108 == 52
                var jp1291 uint64
                if t1294 {
                    jp1291 = 1
                } else {
                    jp1291 = 0
                }
                var t1292 uint64 = t1289 | jp1291
                var t1293 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1292,
                }
                return t1293
            } else {
                var t1296 FloatNatural = parsed__110.numerator
                var t1297 bool
                var inline2144 *_goml_vec_uint32 = t1296.words
                var inline2145 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2144)
                t1297 = inline2145
                if t1297 {
                    var t1298 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1217,
                    }
                    return t1298
                } else {
                    var t1260 bool = parsed__110.hexadecimal
                    var t1261 bool = !t1260
                    if t1261 {
                        var t1262 int = parsed__110.significant_digits
                        var t1263 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1262 + t1263
                        var t1276 bool = mantissa_bits__108 == 23
                        var jp1265 int
                        if t1276 {
                            jp1265 = 40
                        } else {
                            jp1265 = 310
                        }
                        var t1275 bool = mantissa_bits__108 == 23
                        var jp1267 int
                        if t1275 {
                            jp1267 = -46
                        } else {
                            jp1267 = -325
                        }
                        var t1269 bool = decimal_position__113 > jp1265
                        if t1269 {
                            var t1270 uint64 = jp1217 | exponent_mask__112
                            var t1271 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1270,
                            }
                            return t1271
                        } else {
                            var t1273 bool = decimal_position__113 < jp1267
                            if t1273 {
                                var t1274 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1217,
                                }
                                return t1274
                            } else {
                                var t1256 bool = parsed__110.hexadecimal
                                var t1257 bool = !t1256
                                var jp1251 bool
                                if t1257 {
                                    var t1258 int = parsed__110.decimal_exponent
                                    var t1259 bool = t1258 < 0
                                    jp1251 = t1259
                                } else {
                                    jp1251 = false
                                }
                                var jp1225 FloatNatural
                                if jp1251 {
                                    var t1252 int = parsed__110.decimal_exponent
                                    var t1253 int = 0 - t1252
                                    var t1254 FloatNatural = float_natural_power5(t1253)
                                    jp1225 = t1254
                                } else {
                                    var inline2147 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2147, 1)
                                    var inline2149 FloatNatural = FloatNatural{
                                        words: inline2147,
                                    }
                                    jp1225 = inline2149
                                }
                                var t1246 bool = parsed__110.hexadecimal
                                var t1247 bool = !t1246
                                var jp1237 bool
                                if t1247 {
                                    var t1248 int = parsed__110.decimal_exponent
                                    var t1249 bool = t1248 > 0
                                    jp1237 = t1249
                                } else {
                                    jp1237 = false
                                }
                                var jp1227 FloatNatural
                                if jp1237 {
                                    var t1238 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1238)
                                    var count__118 int = 0
                                    Loop_loop1240:
                                    for {
                                        var t1241 int = parsed__110.decimal_exponent
                                        var t1242 bool = count__118 < t1241
                                        if t1242 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1243 int = compound_old213 + compound_value214
                                            count__118 = t1243
                                            continue
                                        } else {
                                            break Loop_loop1240
                                        }
                                    }
                                    jp1227 = result__117
                                    var t1233 bool = parsed__110.hexadecimal
                                    var jp1229 int
                                    if t1233 {
                                        var t1234 int = parsed__110.binary_exponent
                                        jp1229 = t1234
                                    } else {
                                        var t1235 int = parsed__110.decimal_exponent
                                        jp1229 = t1235
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1227, jp1225, jp1229, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1230 bool = !x219
                                    var t1231 uint64 = jp1217 | x218
                                    var t1232 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1230,
                                        _1: t1231,
                                    }
                                    return t1232
                                } else {
                                    var t1245 FloatNatural = parsed__110.numerator
                                    jp1227 = t1245
                                    var t1233 bool = parsed__110.hexadecimal
                                    var jp1229 int
                                    if t1233 {
                                        var t1234 int = parsed__110.binary_exponent
                                        jp1229 = t1234
                                    } else {
                                        var t1235 int = parsed__110.decimal_exponent
                                        jp1229 = t1235
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1227, jp1225, jp1229, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1230 bool = !x219
                                    var t1231 uint64 = jp1217 | x218
                                    var t1232 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1230,
                                        _1: t1231,
                                    }
                                    return t1232
                                }
                            }
                        }
                    } else {
                        var t1256 bool = parsed__110.hexadecimal
                        var t1257 bool = !t1256
                        var jp1251 bool
                        if t1257 {
                            var t1258 int = parsed__110.decimal_exponent
                            var t1259 bool = t1258 < 0
                            jp1251 = t1259
                        } else {
                            jp1251 = false
                        }
                        var jp1225 FloatNatural
                        if jp1251 {
                            var t1252 int = parsed__110.decimal_exponent
                            var t1253 int = 0 - t1252
                            var t1254 FloatNatural = float_natural_power5(t1253)
                            jp1225 = t1254
                        } else {
                            var inline2147 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2147, 1)
                            var inline2149 FloatNatural = FloatNatural{
                                words: inline2147,
                            }
                            jp1225 = inline2149
                        }
                        var t1246 bool = parsed__110.hexadecimal
                        var t1247 bool = !t1246
                        var jp1237 bool
                        if t1247 {
                            var t1248 int = parsed__110.decimal_exponent
                            var t1249 bool = t1248 > 0
                            jp1237 = t1249
                        } else {
                            jp1237 = false
                        }
                        var jp1227 FloatNatural
                        if jp1237 {
                            var t1238 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1238)
                            var count__118 int = 0
                            Loop_loop1240__2:
                            for {
                                var t1241 int = parsed__110.decimal_exponent
                                var t1242 bool = count__118 < t1241
                                if t1242 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1243 int = compound_old213 + compound_value214
                                    count__118 = t1243
                                    continue
                                } else {
                                    break Loop_loop1240__2
                                }
                            }
                            jp1227 = result__117
                            var t1233 bool = parsed__110.hexadecimal
                            var jp1229 int
                            if t1233 {
                                var t1234 int = parsed__110.binary_exponent
                                jp1229 = t1234
                            } else {
                                var t1235 int = parsed__110.decimal_exponent
                                jp1229 = t1235
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1227, jp1225, jp1229, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1230 bool = !x219
                            var t1231 uint64 = jp1217 | x218
                            var t1232 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1230,
                                _1: t1231,
                            }
                            return t1232
                        } else {
                            var t1245 FloatNatural = parsed__110.numerator
                            jp1227 = t1245
                            var t1233 bool = parsed__110.hexadecimal
                            var jp1229 int
                            if t1233 {
                                var t1234 int = parsed__110.binary_exponent
                                jp1229 = t1234
                            } else {
                                var t1235 int = parsed__110.decimal_exponent
                                jp1229 = t1235
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1227, jp1225, jp1229, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1230 bool = !x219
                            var t1231 uint64 = jp1217 | x218
                            var t1232 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1230,
                                _1: t1231,
                            }
                            return t1232
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1330 bool = factor__16 == 0
    if t1330 {
        var t1331 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1331, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1324 uint64 = uint64(uint32(factor__16))
        Loop_loop1317:
        for {
            var t1318 *_goml_vec_uint32 = value__15.words
            var t1319 int
            var inline2153 int = vec_len__Vec_6uint32(t1318)
            t1319 = inline2153
            var t1320 bool = index__18 < t1319
            if t1320 {
                var t1321 *_goml_vec_uint32 = value__15.words
                var t1322 uint32 = vec_get__Vec_6uint32(t1321, index__18)
                var t1323 uint64 = uint64(uint32(t1322))
                var t1325 uint64 = t1323 * t1324
                var product__19 uint64 = t1325 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1327_rhs int = 32
                var t1327 uint64 = product__19 >> t1327_rhs
                carry__17 = t1327
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1328 int = compound_old30 + compound_value31
                index__18 = t1328
                continue
            } else {
                break Loop_loop1317
            }
        }
        var t1313 bool = carry__17 != 0
        if t1313 {
            var t1314 *_goml_vec_uint32 = value__15.words
            var t1315 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1314, t1315)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1339 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1340 FloatNatural = FloatNatural{
        words: t1339,
    }
    return t1340
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2164 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2165 FloatNatural = FloatNatural{
        words: inline2164,
    }
    result__5 = inline2165
    var index__6 int = 0
    Loop_loop1350:
    for {
        var t1351 *_goml_vec_uint32 = value__4.words
        var t1352 int
        var inline2162 int = vec_len__Vec_6uint32(t1351)
        t1352 = inline2162
        var t1353 bool = index__6 < t1352
        if t1353 {
            var t1354 *_goml_vec_uint32 = result__5.words
            var t1355 *_goml_vec_uint32 = value__4.words
            var t1356 uint32 = vec_get__Vec_6uint32(t1355, index__6)
            vec_push__Vec_6uint32(t1354, t1356)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1357 int = compound_old4 + compound_value5
            index__6 = t1357
            continue
        } else {
            break Loop_loop1350
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1364 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2167 int = vec_len__Vec_6uint32(t1364)
    index__47 = inline2167
    var t1375 uint64 = uint64(uint32(divisor__45))
    var t1378 uint64 = uint64(uint32(divisor__45))
    Loop_loop1367:
    for {
        var t1368 bool = index__47 > 0
        if t1368 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1369 int = compound_old83 - compound_value84
            index__47 = t1369
            var t1371_rhs int = 32
            var t1371 uint64 = remainder__46 << t1371_rhs
            var t1372 *_goml_vec_uint32 = value__44.words
            var t1373 uint32 = vec_get__Vec_6uint32(t1372, index__47)
            var t1374 uint64 = uint64(uint32(t1373))
            var current__48 uint64 = t1371 | t1374
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1376 uint64 = current__48 / t1375
            var value90 uint32 = uint32(uint64(t1376))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1379 uint64 = current__48 % t1378
            remainder__46 = t1379
            continue
        } else {
            break Loop_loop1367
        }
    }
    float_natural_trim(value__44)
    var t1366 uint32 = uint32(uint64(remainder__46))
    return t1366
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2169 bool = string_is_char_boundary(self__294, start__295)
    var inline2171 bool
    if inline2169 {
        var inline2174 bool = string_is_char_boundary(self__294, end__296)
        inline2171 = inline2174
    } else {
        inline2171 = false
    }
    if inline2171 {
        var inline2172 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2172
    } else {
        var inline2173 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2173
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1567 bool = string_equals_ascii_case(value__84, "nan")
    if t1567 {
        var t1568 FloatNatural
        var inline2176 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2177 FloatNatural = FloatNatural{
            words: inline2176,
        }
        t1568 = inline2177
        var t1569 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1568,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1569
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1559 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1560 bool = index__85 < t1559
        var jp1554 bool
        if t1560 {
            var t1563 uint8
            var inline2181 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1563 = inline2181
            var t1564 bool = t1563 == 43
            if t1564 {
                jp1554 = true
            } else {
                var t1565 uint8
                var inline2179 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1565 = inline2179
                var t1566 bool = t1565 == 45
                jp1554 = t1566
            }
        } else {
            jp1554 = false
        }
        if jp1554 {
            var t1555 uint8
            var inline2183 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1555 = inline2183
            var t1556 bool = t1555 == 45
            negative__86 = t1556
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1557 int = compound_old140 + compound_value141
            index__85 = t1557
        } else {}
        var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1387)
        var t1551 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1548 bool
        if t1551 {
            jp1548 = true
        } else {
            var t1552 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1548 = t1552
        }
        if jp1548 {
            var t1549 FloatNatural
            var inline2185 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2186 FloatNatural = FloatNatural{
                words: inline2185,
            }
            t1549 = inline2186
            var t1550 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1549,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1550
        } else {
            var t1542 int = index__85 + 2
            var t1543 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1544 bool = t1542 <= t1543
            var jp1537 bool
            if t1544 {
                var t1545 uint8
                var inline2188 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1545 = inline2188
                var t1546 bool = t1545 == 48
                jp1537 = t1546
            } else {
                jp1537 = false
            }
            var jp1390 bool
            if jp1537 {
                var t1538 int = index__85 + 1
                var t1539 uint8
                var inline2197 uint8 = _goml_runtime_core_string_byte_get(value__84, t1538)
                t1539 = inline2197
                var t1540 uint8
                var inline2190 bool = t1539 >= 65
                var inline2192 bool
                if inline2190 {
                    var inline2195 bool = t1539 <= 90
                    inline2192 = inline2195
                } else {
                    inline2192 = false
                }
                if inline2192 {
                    var inline2193 uint8 = 97 - 65
                    var inline2194 uint8 = t1539 + inline2193
                    t1540 = inline2194
                    var t1541 bool = t1540 == 120
                    jp1390 = t1541
                    if jp1390 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1534 int = compound_old145 + compound_value146
                        index__85 = t1534
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1393 int
                    if jp1390 {
                        jp1393 = 16
                    } else {
                        jp1393 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1487 uint32 = uint32(int(jp1393))
                    Loop_loop1483:
                    for {
                        var t1484 int
                        var inline2211 int = _goml_runtime_core_string_len(value__84)
                        t1484 = inline2211
                        var t1485 bool = index__85 < t1484
                        if t1485 {
                            var current__97 uint8
                            var inline2209 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2209
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1393)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1487)
                                var t1488 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1488)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1499 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1499
                                } else {}
                                var t1497 bool = significant_digits__95 > 0
                                var jp1494 bool
                                if t1497 {
                                    jp1494 = true
                                } else {
                                    var t1498 bool = x151 != 0
                                    jp1494 = t1498
                                }
                                if jp1494 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1495 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1495
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1491 int = compound_old164 + compound_value165
                                index__85 = t1491
                                continue
                            } else {
                                var t1502 bool = current__97 == 95
                                if t1502 {
                                    var t1523 int = index__85 + 1
                                    var t1524 int
                                    var inline2207 int = _goml_runtime_core_string_len(value__84)
                                    t1524 = inline2207
                                    var t1525 bool = t1523 >= t1524
                                    if t1525 {
                                        var inline2199 FloatNatural = float_natural_zero()
                                        var inline2200 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2199,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2200
                                    } else {
                                        var t1504 int = index__85 + 1
                                        var t1505 uint8
                                        var inline2205 uint8 = _goml_runtime_core_string_byte_get(value__84, t1504)
                                        t1505 = inline2205
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1505, jp1393)
                                        var x169 bool = mtmp168._0
                                        var jp1520 bool
                                        if jp1390 {
                                            var t1522 bool = !saw_digit__92
                                            jp1520 = t1522
                                        } else {
                                            jp1520 = false
                                        }
                                        var jp1507 bool
                                        if jp1520 {
                                            var t1521 bool = index__85 == mantissa_start__89
                                            jp1507 = t1521
                                        } else {
                                            jp1507 = false
                                        }
                                        var t1517 bool = !previous_digit__96
                                        var jp1515 bool
                                        if t1517 {
                                            var t1518 bool = !jp1507
                                            jp1515 = t1518
                                        } else {
                                            jp1515 = false
                                        }
                                        var jp1512 bool
                                        if jp1515 {
                                            jp1512 = true
                                        } else {
                                            var t1516 bool = !x169
                                            jp1512 = t1516
                                        }
                                        if jp1512 {
                                            var inline2202 FloatNatural = float_natural_zero()
                                            var inline2203 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2202,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2203
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1509 int = compound_old173 + compound_value174
                                            index__85 = t1509
                                            continue
                                        }
                                    }
                                } else {
                                    var t1532 bool = current__97 == 46
                                    var jp1529 bool
                                    if t1532 {
                                        var t1533 bool = !saw_dot__93
                                        jp1529 = t1533
                                    } else {
                                        jp1529 = false
                                    }
                                    if jp1529 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1530 int = compound_old178 + compound_value179
                                        index__85 = t1530
                                        continue
                                    } else {
                                        break Loop_loop1483
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1483
                        }
                    }
                    var t1481 bool = !saw_digit__92
                    if t1481 {
                        var inline2213 FloatNatural = float_natural_zero()
                        var inline2214 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2213,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2214
                    } else {
                        var jp1397 uint8
                        if jp1390 {
                            jp1397 = 112
                        } else {
                            jp1397 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1476 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1477 bool = index__85 < t1476
                        var jp1414 bool
                        if t1477 {
                            var t1478 uint8
                            var inline2216 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1478 = inline2216
                            var t1479 uint8 = ascii_lower(t1478)
                            var t1480 bool = t1479 == jp1397
                            jp1414 = t1480
                        } else {
                            jp1414 = false
                        }
                        if jp1414 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1415 int = compound_old183 + compound_value184
                            index__85 = t1415
                            var t1466 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1467 bool = index__85 < t1466
                            var jp1461 bool
                            if t1467 {
                                var t1470 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1471 bool = t1470 == 43
                                if t1471 {
                                    jp1461 = true
                                } else {
                                    var t1472 uint8
                                    var inline2218 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1472 = inline2218
                                    var t1473 bool = t1472 == 45
                                    jp1461 = t1473
                                }
                            } else {
                                jp1461 = false
                            }
                            if jp1461 {
                                var t1462 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1463 bool = t1462 == 45
                                exponent_negative__104 = t1463
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1464 int = compound_old187 + compound_value188
                                index__85 = t1464
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1422:
                            for {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 < t1423
                                if t1424 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1458 bool = current__106 >= 48
                                    var jp1427 bool
                                    if t1458 {
                                        var t1459 bool = current__106 <= 57
                                        jp1427 = t1459
                                    } else {
                                        jp1427 = false
                                    }
                                    if jp1427 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1431 bool = exponent__103 < 1000000
                                        if t1431 {
                                            var t1432 int = exponent__103 * 10
                                            var t1433 uint8 = current__106 - 48
                                            var t1434 int = int(uint8(t1433))
                                            var t1435 int = t1432 + t1434
                                            exponent__103 = t1435
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1429 int = compound_old196 + compound_value197
                                        index__85 = t1429
                                        continue
                                    } else {
                                        var t1437 bool = current__106 == 95
                                        if t1437 {
                                            var t1454 bool = !previous_digit__96
                                            var jp1450 bool
                                            if t1454 {
                                                jp1450 = true
                                            } else {
                                                var t1455 int = index__85 + 1
                                                var t1456 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1457 bool = t1455 >= t1456
                                                jp1450 = t1457
                                            }
                                            var jp1445 bool
                                            if jp1450 {
                                                jp1445 = true
                                            } else {
                                                var t1451 int = index__85 + 1
                                                var t1452 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1451)
                                                var t1453 bool = t1452 < 48
                                                jp1445 = t1453
                                            }
                                            var jp1442 bool
                                            if jp1445 {
                                                jp1442 = true
                                            } else {
                                                var t1446 int = index__85 + 1
                                                var t1447 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1446)
                                                var t1448 bool = t1447 > 57
                                                jp1442 = t1448
                                            }
                                            if jp1442 {
                                                var t1443 ParsedFloat = invalid_parsed_float()
                                                return t1443
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1439 int = compound_old201 + compound_value202
                                                index__85 = t1439
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1422
                                        }
                                    }
                                } else {
                                    break Loop_loop1422
                                }
                            }
                            var t1420 bool = !exponent_digits__105
                            if t1420 {
                                var t1421 ParsedFloat = invalid_parsed_float()
                                return t1421
                            } else {
                                var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1411 bool = index__85 != t1410
                                if t1411 {
                                    var t1412 ParsedFloat = invalid_parsed_float()
                                    return t1412
                                } else {
                                    if exponent_negative__104 {
                                        var t1409 int = 0 - exponent__103
                                        exponent__103 = t1409
                                    } else {}
                                    var jp1402 int
                                    if jp1390 {
                                        jp1402 = 0
                                    } else {
                                        var t1408 int = exponent__103 - fraction_digits__94
                                        jp1402 = t1408
                                    }
                                    var jp1404 int
                                    if jp1390 {
                                        var t1406 int = fraction_digits__94 * 4
                                        var t1407 int = exponent__103 - t1406
                                        jp1404 = t1407
                                    } else {
                                        jp1404 = 0
                                    }
                                    var t1405 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1402,
                                        binary_exponent: jp1404,
                                        hexadecimal: jp1390,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1405
                                }
                            }
                        } else {
                            if jp1390 {
                                var t1475 ParsedFloat = invalid_parsed_float()
                                return t1475
                            } else {
                                var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1411 bool = index__85 != t1410
                                if t1411 {
                                    var t1412 ParsedFloat = invalid_parsed_float()
                                    return t1412
                                } else {
                                    if exponent_negative__104 {
                                        var t1409 int = 0 - exponent__103
                                        exponent__103 = t1409
                                    } else {}
                                    var jp1402 int
                                    if jp1390 {
                                        jp1402 = 0
                                    } else {
                                        var t1408 int = exponent__103 - fraction_digits__94
                                        jp1402 = t1408
                                    }
                                    var jp1404 int
                                    if jp1390 {
                                        var t1406 int = fraction_digits__94 * 4
                                        var t1407 int = exponent__103 - t1406
                                        jp1404 = t1407
                                    } else {
                                        jp1404 = 0
                                    }
                                    var t1405 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1402,
                                        binary_exponent: jp1404,
                                        hexadecimal: jp1390,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1405
                                }
                            }
                        }
                    }
                } else {
                    t1540 = t1539
                    var t1541 bool = t1540 == 120
                    jp1390 = t1541
                    if jp1390 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1534 int = compound_old145 + compound_value146
                        index__85 = t1534
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1393 int
                    if jp1390 {
                        jp1393 = 16
                    } else {
                        jp1393 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1487 uint32 = uint32(int(jp1393))
                    Loop_loop1483__2:
                    for {
                        var t1484 int
                        var inline2211 int = _goml_runtime_core_string_len(value__84)
                        t1484 = inline2211
                        var t1485 bool = index__85 < t1484
                        if t1485 {
                            var current__97 uint8
                            var inline2209 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2209
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1393)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1487)
                                var t1488 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1488)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1499 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1499
                                } else {}
                                var t1497 bool = significant_digits__95 > 0
                                var jp1494 bool
                                if t1497 {
                                    jp1494 = true
                                } else {
                                    var t1498 bool = x151 != 0
                                    jp1494 = t1498
                                }
                                if jp1494 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1495 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1495
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1491 int = compound_old164 + compound_value165
                                index__85 = t1491
                                continue
                            } else {
                                var t1502 bool = current__97 == 95
                                if t1502 {
                                    var t1523 int = index__85 + 1
                                    var t1524 int
                                    var inline2207 int = _goml_runtime_core_string_len(value__84)
                                    t1524 = inline2207
                                    var t1525 bool = t1523 >= t1524
                                    if t1525 {
                                        var inline2199 FloatNatural = float_natural_zero()
                                        var inline2200 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2199,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2200
                                    } else {
                                        var t1504 int = index__85 + 1
                                        var t1505 uint8
                                        var inline2205 uint8 = _goml_runtime_core_string_byte_get(value__84, t1504)
                                        t1505 = inline2205
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1505, jp1393)
                                        var x169 bool = mtmp168._0
                                        var jp1520 bool
                                        if jp1390 {
                                            var t1522 bool = !saw_digit__92
                                            jp1520 = t1522
                                        } else {
                                            jp1520 = false
                                        }
                                        var jp1507 bool
                                        if jp1520 {
                                            var t1521 bool = index__85 == mantissa_start__89
                                            jp1507 = t1521
                                        } else {
                                            jp1507 = false
                                        }
                                        var t1517 bool = !previous_digit__96
                                        var jp1515 bool
                                        if t1517 {
                                            var t1518 bool = !jp1507
                                            jp1515 = t1518
                                        } else {
                                            jp1515 = false
                                        }
                                        var jp1512 bool
                                        if jp1515 {
                                            jp1512 = true
                                        } else {
                                            var t1516 bool = !x169
                                            jp1512 = t1516
                                        }
                                        if jp1512 {
                                            var inline2202 FloatNatural = float_natural_zero()
                                            var inline2203 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2202,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2203
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1509 int = compound_old173 + compound_value174
                                            index__85 = t1509
                                            continue
                                        }
                                    }
                                } else {
                                    var t1532 bool = current__97 == 46
                                    var jp1529 bool
                                    if t1532 {
                                        var t1533 bool = !saw_dot__93
                                        jp1529 = t1533
                                    } else {
                                        jp1529 = false
                                    }
                                    if jp1529 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1530 int = compound_old178 + compound_value179
                                        index__85 = t1530
                                        continue
                                    } else {
                                        break Loop_loop1483__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1483__2
                        }
                    }
                    var t1481 bool = !saw_digit__92
                    if t1481 {
                        var inline2213 FloatNatural = float_natural_zero()
                        var inline2214 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2213,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2214
                    } else {
                        var jp1397 uint8
                        if jp1390 {
                            jp1397 = 112
                        } else {
                            jp1397 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1476 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1477 bool = index__85 < t1476
                        var jp1414 bool
                        if t1477 {
                            var t1478 uint8
                            var inline2216 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1478 = inline2216
                            var t1479 uint8 = ascii_lower(t1478)
                            var t1480 bool = t1479 == jp1397
                            jp1414 = t1480
                        } else {
                            jp1414 = false
                        }
                        if jp1414 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1415 int = compound_old183 + compound_value184
                            index__85 = t1415
                            var t1466 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1467 bool = index__85 < t1466
                            var jp1461 bool
                            if t1467 {
                                var t1470 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1471 bool = t1470 == 43
                                if t1471 {
                                    jp1461 = true
                                } else {
                                    var t1472 uint8
                                    var inline2218 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1472 = inline2218
                                    var t1473 bool = t1472 == 45
                                    jp1461 = t1473
                                }
                            } else {
                                jp1461 = false
                            }
                            if jp1461 {
                                var t1462 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1463 bool = t1462 == 45
                                exponent_negative__104 = t1463
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1464 int = compound_old187 + compound_value188
                                index__85 = t1464
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1422__2:
                            for {
                                var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1424 bool = index__85 < t1423
                                if t1424 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1458 bool = current__106 >= 48
                                    var jp1427 bool
                                    if t1458 {
                                        var t1459 bool = current__106 <= 57
                                        jp1427 = t1459
                                    } else {
                                        jp1427 = false
                                    }
                                    if jp1427 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1431 bool = exponent__103 < 1000000
                                        if t1431 {
                                            var t1432 int = exponent__103 * 10
                                            var t1433 uint8 = current__106 - 48
                                            var t1434 int = int(uint8(t1433))
                                            var t1435 int = t1432 + t1434
                                            exponent__103 = t1435
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1429 int = compound_old196 + compound_value197
                                        index__85 = t1429
                                        continue
                                    } else {
                                        var t1437 bool = current__106 == 95
                                        if t1437 {
                                            var t1454 bool = !previous_digit__96
                                            var jp1450 bool
                                            if t1454 {
                                                jp1450 = true
                                            } else {
                                                var t1455 int = index__85 + 1
                                                var t1456 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1457 bool = t1455 >= t1456
                                                jp1450 = t1457
                                            }
                                            var jp1445 bool
                                            if jp1450 {
                                                jp1445 = true
                                            } else {
                                                var t1451 int = index__85 + 1
                                                var t1452 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1451)
                                                var t1453 bool = t1452 < 48
                                                jp1445 = t1453
                                            }
                                            var jp1442 bool
                                            if jp1445 {
                                                jp1442 = true
                                            } else {
                                                var t1446 int = index__85 + 1
                                                var t1447 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1446)
                                                var t1448 bool = t1447 > 57
                                                jp1442 = t1448
                                            }
                                            if jp1442 {
                                                var t1443 ParsedFloat = invalid_parsed_float()
                                                return t1443
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1439 int = compound_old201 + compound_value202
                                                index__85 = t1439
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1422__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1422__2
                                }
                            }
                            var t1420 bool = !exponent_digits__105
                            if t1420 {
                                var t1421 ParsedFloat = invalid_parsed_float()
                                return t1421
                            } else {
                                var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1411 bool = index__85 != t1410
                                if t1411 {
                                    var t1412 ParsedFloat = invalid_parsed_float()
                                    return t1412
                                } else {
                                    if exponent_negative__104 {
                                        var t1409 int = 0 - exponent__103
                                        exponent__103 = t1409
                                    } else {}
                                    var jp1402 int
                                    if jp1390 {
                                        jp1402 = 0
                                    } else {
                                        var t1408 int = exponent__103 - fraction_digits__94
                                        jp1402 = t1408
                                    }
                                    var jp1404 int
                                    if jp1390 {
                                        var t1406 int = fraction_digits__94 * 4
                                        var t1407 int = exponent__103 - t1406
                                        jp1404 = t1407
                                    } else {
                                        jp1404 = 0
                                    }
                                    var t1405 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1402,
                                        binary_exponent: jp1404,
                                        hexadecimal: jp1390,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1405
                                }
                            }
                        } else {
                            if jp1390 {
                                var t1475 ParsedFloat = invalid_parsed_float()
                                return t1475
                            } else {
                                var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1411 bool = index__85 != t1410
                                if t1411 {
                                    var t1412 ParsedFloat = invalid_parsed_float()
                                    return t1412
                                } else {
                                    if exponent_negative__104 {
                                        var t1409 int = 0 - exponent__103
                                        exponent__103 = t1409
                                    } else {}
                                    var jp1402 int
                                    if jp1390 {
                                        jp1402 = 0
                                    } else {
                                        var t1408 int = exponent__103 - fraction_digits__94
                                        jp1402 = t1408
                                    }
                                    var jp1404 int
                                    if jp1390 {
                                        var t1406 int = fraction_digits__94 * 4
                                        var t1407 int = exponent__103 - t1406
                                        jp1404 = t1407
                                    } else {
                                        jp1404 = 0
                                    }
                                    var t1405 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1402,
                                        binary_exponent: jp1404,
                                        hexadecimal: jp1390,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1405
                                }
                            }
                        }
                    }
                }
            } else {
                jp1390 = false
                if jp1390 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1534 int = compound_old145 + compound_value146
                    index__85 = t1534
                } else {}
                var mantissa_start__89 int = index__85
                var jp1393 int
                if jp1390 {
                    jp1393 = 16
                } else {
                    jp1393 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1487 uint32 = uint32(int(jp1393))
                Loop_loop1483__3:
                for {
                    var t1484 int
                    var inline2211 int = _goml_runtime_core_string_len(value__84)
                    t1484 = inline2211
                    var t1485 bool = index__85 < t1484
                    if t1485 {
                        var current__97 uint8
                        var inline2209 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2209
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1393)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1487)
                            var t1488 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1488)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1499 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1499
                            } else {}
                            var t1497 bool = significant_digits__95 > 0
                            var jp1494 bool
                            if t1497 {
                                jp1494 = true
                            } else {
                                var t1498 bool = x151 != 0
                                jp1494 = t1498
                            }
                            if jp1494 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1495 int = compound_old160 + compound_value161
                                significant_digits__95 = t1495
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1491 int = compound_old164 + compound_value165
                            index__85 = t1491
                            continue
                        } else {
                            var t1502 bool = current__97 == 95
                            if t1502 {
                                var t1523 int = index__85 + 1
                                var t1524 int
                                var inline2207 int = _goml_runtime_core_string_len(value__84)
                                t1524 = inline2207
                                var t1525 bool = t1523 >= t1524
                                if t1525 {
                                    var inline2199 FloatNatural = float_natural_zero()
                                    var inline2200 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2199,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2200
                                } else {
                                    var t1504 int = index__85 + 1
                                    var t1505 uint8
                                    var inline2205 uint8 = _goml_runtime_core_string_byte_get(value__84, t1504)
                                    t1505 = inline2205
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1505, jp1393)
                                    var x169 bool = mtmp168._0
                                    var jp1520 bool
                                    if jp1390 {
                                        var t1522 bool = !saw_digit__92
                                        jp1520 = t1522
                                    } else {
                                        jp1520 = false
                                    }
                                    var jp1507 bool
                                    if jp1520 {
                                        var t1521 bool = index__85 == mantissa_start__89
                                        jp1507 = t1521
                                    } else {
                                        jp1507 = false
                                    }
                                    var t1517 bool = !previous_digit__96
                                    var jp1515 bool
                                    if t1517 {
                                        var t1518 bool = !jp1507
                                        jp1515 = t1518
                                    } else {
                                        jp1515 = false
                                    }
                                    var jp1512 bool
                                    if jp1515 {
                                        jp1512 = true
                                    } else {
                                        var t1516 bool = !x169
                                        jp1512 = t1516
                                    }
                                    if jp1512 {
                                        var inline2202 FloatNatural = float_natural_zero()
                                        var inline2203 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2202,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2203
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1509 int = compound_old173 + compound_value174
                                        index__85 = t1509
                                        continue
                                    }
                                }
                            } else {
                                var t1532 bool = current__97 == 46
                                var jp1529 bool
                                if t1532 {
                                    var t1533 bool = !saw_dot__93
                                    jp1529 = t1533
                                } else {
                                    jp1529 = false
                                }
                                if jp1529 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1530 int = compound_old178 + compound_value179
                                    index__85 = t1530
                                    continue
                                } else {
                                    break Loop_loop1483__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1483__3
                    }
                }
                var t1481 bool = !saw_digit__92
                if t1481 {
                    var inline2213 FloatNatural = float_natural_zero()
                    var inline2214 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2213,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2214
                } else {
                    var jp1397 uint8
                    if jp1390 {
                        jp1397 = 112
                    } else {
                        jp1397 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1476 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1477 bool = index__85 < t1476
                    var jp1414 bool
                    if t1477 {
                        var t1478 uint8
                        var inline2216 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1478 = inline2216
                        var t1479 uint8 = ascii_lower(t1478)
                        var t1480 bool = t1479 == jp1397
                        jp1414 = t1480
                    } else {
                        jp1414 = false
                    }
                    if jp1414 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1415 int = compound_old183 + compound_value184
                        index__85 = t1415
                        var t1466 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1467 bool = index__85 < t1466
                        var jp1461 bool
                        if t1467 {
                            var t1470 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1471 bool = t1470 == 43
                            if t1471 {
                                jp1461 = true
                            } else {
                                var t1472 uint8
                                var inline2218 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1472 = inline2218
                                var t1473 bool = t1472 == 45
                                jp1461 = t1473
                            }
                        } else {
                            jp1461 = false
                        }
                        if jp1461 {
                            var t1462 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1463 bool = t1462 == 45
                            exponent_negative__104 = t1463
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1464 int = compound_old187 + compound_value188
                            index__85 = t1464
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1422__3:
                        for {
                            var t1423 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1424 bool = index__85 < t1423
                            if t1424 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1458 bool = current__106 >= 48
                                var jp1427 bool
                                if t1458 {
                                    var t1459 bool = current__106 <= 57
                                    jp1427 = t1459
                                } else {
                                    jp1427 = false
                                }
                                if jp1427 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1431 bool = exponent__103 < 1000000
                                    if t1431 {
                                        var t1432 int = exponent__103 * 10
                                        var t1433 uint8 = current__106 - 48
                                        var t1434 int = int(uint8(t1433))
                                        var t1435 int = t1432 + t1434
                                        exponent__103 = t1435
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1429 int = compound_old196 + compound_value197
                                    index__85 = t1429
                                    continue
                                } else {
                                    var t1437 bool = current__106 == 95
                                    if t1437 {
                                        var t1454 bool = !previous_digit__96
                                        var jp1450 bool
                                        if t1454 {
                                            jp1450 = true
                                        } else {
                                            var t1455 int = index__85 + 1
                                            var t1456 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1457 bool = t1455 >= t1456
                                            jp1450 = t1457
                                        }
                                        var jp1445 bool
                                        if jp1450 {
                                            jp1445 = true
                                        } else {
                                            var t1451 int = index__85 + 1
                                            var t1452 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1451)
                                            var t1453 bool = t1452 < 48
                                            jp1445 = t1453
                                        }
                                        var jp1442 bool
                                        if jp1445 {
                                            jp1442 = true
                                        } else {
                                            var t1446 int = index__85 + 1
                                            var t1447 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1446)
                                            var t1448 bool = t1447 > 57
                                            jp1442 = t1448
                                        }
                                        if jp1442 {
                                            var t1443 ParsedFloat = invalid_parsed_float()
                                            return t1443
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1439 int = compound_old201 + compound_value202
                                            index__85 = t1439
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1422__3
                                    }
                                }
                            } else {
                                break Loop_loop1422__3
                            }
                        }
                        var t1420 bool = !exponent_digits__105
                        if t1420 {
                            var t1421 ParsedFloat = invalid_parsed_float()
                            return t1421
                        } else {
                            var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1411 bool = index__85 != t1410
                            if t1411 {
                                var t1412 ParsedFloat = invalid_parsed_float()
                                return t1412
                            } else {
                                if exponent_negative__104 {
                                    var t1409 int = 0 - exponent__103
                                    exponent__103 = t1409
                                } else {}
                                var jp1402 int
                                if jp1390 {
                                    jp1402 = 0
                                } else {
                                    var t1408 int = exponent__103 - fraction_digits__94
                                    jp1402 = t1408
                                }
                                var jp1404 int
                                if jp1390 {
                                    var t1406 int = fraction_digits__94 * 4
                                    var t1407 int = exponent__103 - t1406
                                    jp1404 = t1407
                                } else {
                                    jp1404 = 0
                                }
                                var t1405 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1402,
                                    binary_exponent: jp1404,
                                    hexadecimal: jp1390,
                                    significant_digits: significant_digits__95,
                                }
                                return t1405
                            }
                        }
                    } else {
                        if jp1390 {
                            var t1475 ParsedFloat = invalid_parsed_float()
                            return t1475
                        } else {
                            var t1410 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1411 bool = index__85 != t1410
                            if t1411 {
                                var t1412 ParsedFloat = invalid_parsed_float()
                                return t1412
                            } else {
                                if exponent_negative__104 {
                                    var t1409 int = 0 - exponent__103
                                    exponent__103 = t1409
                                } else {}
                                var jp1402 int
                                if jp1390 {
                                    jp1402 = 0
                                } else {
                                    var t1408 int = exponent__103 - fraction_digits__94
                                    jp1402 = t1408
                                }
                                var jp1404 int
                                if jp1390 {
                                    var t1406 int = fraction_digits__94 * 4
                                    var t1407 int = exponent__103 - t1406
                                    jp1404 = t1407
                                } else {
                                    jp1404 = 0
                                }
                                var t1405 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1402,
                                    binary_exponent: jp1404,
                                    hexadecimal: jp1390,
                                    significant_digits: significant_digits__95,
                                }
                                return t1405
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
    var inline2220 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2220, 1)
    var inline2222 FloatNatural = FloatNatural{
        words: inline2220,
    }
    result__26 = inline2222
    var count__27 int = 0
    Loop_loop1573:
    for {
        var t1574 bool = count__27 < exponent__25
        if t1574 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1575 int = compound_old46 + compound_value47
            count__27 = t1575
            continue
        } else {
            break Loop_loop1573
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1662 bool
    var inline2224 *_goml_vec_uint32 = numerator__65.words
    var inline2225 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2224)
    t1662 = inline2225
    if t1662 {
        var t1663 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1663
    } else {
        var t1659 bool = binary_shift__67 >= 0
        var jp1584 FloatNatural
        if t1659 {
            var t1660 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1584 = t1660
        } else {
            var t1661 FloatNatural = float_natural_copy(numerator__65)
            jp1584 = t1661
        }
        var t1655 bool = binary_shift__67 >= 0
        var jp1586 FloatNatural
        if t1655 {
            var t1656 FloatNatural = float_natural_copy(denominator__66)
            jp1586 = t1656
        } else {
            var t1657 int = 0 - binary_shift__67
            var t1658 FloatNatural = float_natural_shift_left(denominator__66, t1657)
            jp1586 = t1658
        }
        var t1587 int = float_natural_bit_length(jp1584)
        var t1588 int = float_natural_bit_length(jp1586)
        var exponent__72 int = t1587 - t1588
        var t1649 bool = exponent__72 >= 0
        var jp1590 int
        if t1649 {
            var t1650 FloatNatural = float_natural_shift_left(jp1586, exponent__72)
            var t1651 int = float_natural_compare(jp1584, t1650)
            jp1590 = t1651
        } else {
            var t1652 int = 0 - exponent__72
            var t1653 FloatNatural = float_natural_shift_left(jp1584, t1652)
            var t1654 int = float_natural_compare(t1653, jp1586)
            jp1590 = t1654
        }
        var t1646 bool = jp1590 < 0
        if t1646 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1647 int = compound_old120 - compound_value121
            exponent__72 = t1647
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1640 bool = exponent__72 > exponent_bias__69
        if t1640 {
            var t1641 int = exponent_bias__69 + exponent_bias__69
            var t1642 int = t1641 + 1
            var t1643 uint64 = uint64(int(t1642))
            var t1644 uint64 = t1643 << mantissa_bits__68
            var t1645 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1644,
                _1: true,
            }
            return t1645
        } else {
            var t1635 bool = exponent__72 < minimum_exponent__74
            var jp1594 uint64
            if t1635 {
                var t1636 int = mantissa_bits__68 - minimum_exponent__74
                var t1637 uint64 = float_rational_quotient(jp1584, jp1586, t1636)
                jp1594 = t1637
            } else {
                var t1638 int = mantissa_bits__68 - exponent__72
                var t1639 uint64 = float_rational_quotient(jp1584, jp1586, t1638)
                jp1594 = t1639
            }
            var mantissa__76 uint64 = jp1594
            var t1597 bool = exponent__72 < minimum_exponent__74
            if t1597 {
                var t1600 bool = mantissa__76 == 0
                if t1600 {
                    var t1601 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1601
                } else {
                    var t1604_lhs uint64 = 1
                    var t1604 uint64 = t1604_lhs << mantissa_bits__68
                    var t1605 bool = mantissa__76 >= t1604
                    if t1605 {
                        var t1606_lhs uint64 = 1
                        var t1606 uint64 = t1606_lhs << mantissa_bits__68
                        var t1607_lhs uint64 = 1
                        var t1607 uint64 = t1607_lhs << mantissa_bits__68
                        var t1608 uint64 = mantissa__76 - t1607
                        var t1609 uint64 = t1606 | t1608
                        var t1610 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1609,
                            _1: false,
                        }
                        return t1610
                    } else {
                        var t1611 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1611
                    }
                }
            } else {
                var t1628 int = mantissa_bits__68 + 1
                var t1629_lhs uint64 = 1
                var t1629 uint64 = t1629_lhs << t1628
                var t1630 bool = mantissa__76 >= t1629
                if t1630 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1631 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1631
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1633 int = compound_old128 + compound_value129
                    exponent__72 = t1633
                } else {}
                var t1615 bool = exponent__72 > exponent_bias__69
                if t1615 {
                    var t1616 int = exponent_bias__69 + exponent_bias__69
                    var t1617 int = t1616 + 1
                    var t1618 uint64 = uint64(int(t1617))
                    var t1619 uint64 = t1618 << mantissa_bits__68
                    var t1620 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1619,
                        _1: true,
                    }
                    return t1620
                } else {
                    var t1621 int = exponent__72 + exponent_bias__69
                    var t1622 uint64 = uint64(int(t1621))
                    var t1623 uint64 = t1622 << mantissa_bits__68
                    var t1624_lhs uint64 = 1
                    var t1624 uint64 = t1624_lhs << mantissa_bits__68
                    var t1625 uint64 = mantissa__76 - t1624
                    var t1626 uint64 = t1623 | t1625
                    var t1627 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1626,
                        _1: false,
                    }
                    return t1627
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1668 int = vec_len__Vec_6uint32(self__528)
    var t1669 bool = t1668 == 0
    return t1669
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1672:
    for {
        var t1680 *_goml_vec_uint32 = value__7.words
        var t1681 bool
        var inline2236 int = vec_len__Vec_6uint32(t1680)
        var inline2237 bool = inline2236 == 0
        t1681 = inline2237
        var t1682 bool = !t1681
        var jp1674 bool
        if t1682 {
            var t1683 *_goml_vec_uint32 = value__7.words
            var t1684 *_goml_vec_uint32 = value__7.words
            var t1685 int
            var inline2230 int = vec_len__Vec_6uint32(t1684)
            t1685 = inline2230
            var t1686 int = t1685 - 1
            var t1687 uint32 = vec_get__Vec_6uint32(t1683, t1686)
            var t1688 bool = t1687 == 0
            jp1674 = t1688
        } else {
            jp1674 = false
        }
        if jp1674 {
            var t1675 *_goml_vec_uint32 = value__7.words
            var t1676 *_goml_vec_uint32 = value__7.words
            var t1677 int
            var inline2234 int = vec_len__Vec_6uint32(t1676)
            t1677 = inline2234
            var t1678 int = t1677 - 1
            vec_truncate__Vec_6uint32(t1675, t1678)
            continue
        } else {
            break Loop_loop1672
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1697 bool = string_is_char_boundary(value__274, start__275)
    var jp1694 bool
    if t1697 {
        var t1698 bool = string_is_char_boundary(value__274, end__276)
        jp1694 = t1698
    } else {
        jp1694 = false
    }
    if jp1694 {
        var t1695 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1695
    } else {
        var t1696 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1696
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1713 int
    var inline2254 int = _goml_runtime_core_string_len(value__78)
    t1713 = inline2254
    var t1714 int
    var inline2252 int = _goml_runtime_core_string_len(expected__79)
    t1714 = inline2252
    var t1715 bool = t1713 != t1714
    if t1715 {
        return false
    } else {
        var index__80 int = 0
        var inline2244 uint8 = 97 - 65
        Loop_loop1703:
        for {
            var t1704 int
            var inline2250 int = _goml_runtime_core_string_len(value__78)
            t1704 = inline2250
            var t1705 bool = index__80 < t1704
            if t1705 {
                var t1709 uint8
                var inline2248 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1709 = inline2248
                var t1710 uint8
                var inline2241 bool = t1709 >= 65
                var inline2243 bool
                if inline2241 {
                    var inline2246 bool = t1709 <= 90
                    inline2243 = inline2246
                } else {
                    inline2243 = false
                }
                if inline2243 {
                    var inline2245 uint8 = t1709 + inline2244
                    t1710 = inline2245
                    var t1711 uint8
                    var inline2239 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1711 = inline2239
                    var t1712 bool = t1710 != t1711
                    if t1712 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1707 int = compound_old134 + compound_value135
                        index__80 = t1707
                        continue
                    }
                } else {
                    t1710 = t1709
                    var t1711 uint8
                    var inline2239 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1711 = inline2239
                    var t1712 bool = t1710 != t1711
                    if t1712 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1707 int = compound_old134 + compound_value135
                        index__80 = t1707
                        continue
                    }
                }
            } else {
                break Loop_loop1703
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1724 bool = value__77 >= 65
    var jp1721 bool
    if t1724 {
        var t1725 bool = value__77 <= 90
        jp1721 = t1725
    } else {
        jp1721 = false
    }
    if jp1721 {
        var t1722 uint8 = 97 - 65
        var t1723 uint8 = value__77 + t1722
        return t1723
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1752 bool = value__81 >= 48
    var jp1736 bool
    if t1752 {
        var t1753 bool = value__81 <= 57
        jp1736 = t1753
    } else {
        jp1736 = false
    }
    var jp1729 int
    if jp1736 {
        var t1737 uint8 = value__81 - 48
        var t1738 int = int(uint8(t1737))
        jp1729 = t1738
        var t1732 bool = jp1729 < base__82
        if t1732 {
            var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1729,
            }
            return t1733
        } else {
            var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1734
        }
    } else {
        var t1748 uint8
        var inline2270 bool = value__81 >= 65
        var inline2272 bool
        if inline2270 {
            var inline2275 bool = value__81 <= 90
            inline2272 = inline2275
        } else {
            inline2272 = false
        }
        if inline2272 {
            var inline2273 uint8 = 97 - 65
            var inline2274 uint8 = value__81 + inline2273
            t1748 = inline2274
            var t1749 bool = t1748 >= 97
            var jp1742 bool
            if t1749 {
                var t1750 uint8
                var inline2256 bool = value__81 >= 65
                var inline2258 bool
                if inline2256 {
                    var inline2261 bool = value__81 <= 90
                    inline2258 = inline2261
                } else {
                    inline2258 = false
                }
                if inline2258 {
                    var inline2259 uint8 = 97 - 65
                    var inline2260 uint8 = value__81 + inline2259
                    t1750 = inline2260
                    var t1751 bool = t1750 <= 102
                    jp1742 = t1751
                    if jp1742 {
                        var t1743 uint8
                        var inline2263 bool = value__81 >= 65
                        var inline2265 bool
                        if inline2263 {
                            var inline2268 bool = value__81 <= 90
                            inline2265 = inline2268
                        } else {
                            inline2265 = false
                        }
                        if inline2265 {
                            var inline2266 uint8 = 97 - 65
                            var inline2267 uint8 = value__81 + inline2266
                            t1743 = inline2267
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        } else {
                            t1743 = value__81
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        }
                    } else {
                        var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1747
                    }
                } else {
                    t1750 = value__81
                    var t1751 bool = t1750 <= 102
                    jp1742 = t1751
                    if jp1742 {
                        var t1743 uint8
                        var inline2263 bool = value__81 >= 65
                        var inline2265 bool
                        if inline2263 {
                            var inline2268 bool = value__81 <= 90
                            inline2265 = inline2268
                        } else {
                            inline2265 = false
                        }
                        if inline2265 {
                            var inline2266 uint8 = 97 - 65
                            var inline2267 uint8 = value__81 + inline2266
                            t1743 = inline2267
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        } else {
                            t1743 = value__81
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        }
                    } else {
                        var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1747
                    }
                }
            } else {
                jp1742 = false
                if jp1742 {
                    var t1743 uint8
                    var inline2263 bool = value__81 >= 65
                    var inline2265 bool
                    if inline2263 {
                        var inline2268 bool = value__81 <= 90
                        inline2265 = inline2268
                    } else {
                        inline2265 = false
                    }
                    if inline2265 {
                        var inline2266 uint8 = 97 - 65
                        var inline2267 uint8 = value__81 + inline2266
                        t1743 = inline2267
                        var t1744 uint8 = t1743 - 97
                        var t1745 uint8 = t1744 + 10
                        var t1746 int = int(uint8(t1745))
                        jp1729 = t1746
                        var t1732 bool = jp1729 < base__82
                        if t1732 {
                            var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1729,
                            }
                            return t1733
                        } else {
                            var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1734
                        }
                    } else {
                        t1743 = value__81
                        var t1744 uint8 = t1743 - 97
                        var t1745 uint8 = t1744 + 10
                        var t1746 int = int(uint8(t1745))
                        jp1729 = t1746
                        var t1732 bool = jp1729 < base__82
                        if t1732 {
                            var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1729,
                            }
                            return t1733
                        } else {
                            var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1734
                        }
                    }
                } else {
                    var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1747
                }
            }
        } else {
            t1748 = value__81
            var t1749 bool = t1748 >= 97
            var jp1742 bool
            if t1749 {
                var t1750 uint8
                var inline2256 bool = value__81 >= 65
                var inline2258 bool
                if inline2256 {
                    var inline2261 bool = value__81 <= 90
                    inline2258 = inline2261
                } else {
                    inline2258 = false
                }
                if inline2258 {
                    var inline2259 uint8 = 97 - 65
                    var inline2260 uint8 = value__81 + inline2259
                    t1750 = inline2260
                    var t1751 bool = t1750 <= 102
                    jp1742 = t1751
                    if jp1742 {
                        var t1743 uint8
                        var inline2263 bool = value__81 >= 65
                        var inline2265 bool
                        if inline2263 {
                            var inline2268 bool = value__81 <= 90
                            inline2265 = inline2268
                        } else {
                            inline2265 = false
                        }
                        if inline2265 {
                            var inline2266 uint8 = 97 - 65
                            var inline2267 uint8 = value__81 + inline2266
                            t1743 = inline2267
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        } else {
                            t1743 = value__81
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        }
                    } else {
                        var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1747
                    }
                } else {
                    t1750 = value__81
                    var t1751 bool = t1750 <= 102
                    jp1742 = t1751
                    if jp1742 {
                        var t1743 uint8
                        var inline2263 bool = value__81 >= 65
                        var inline2265 bool
                        if inline2263 {
                            var inline2268 bool = value__81 <= 90
                            inline2265 = inline2268
                        } else {
                            inline2265 = false
                        }
                        if inline2265 {
                            var inline2266 uint8 = 97 - 65
                            var inline2267 uint8 = value__81 + inline2266
                            t1743 = inline2267
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        } else {
                            t1743 = value__81
                            var t1744 uint8 = t1743 - 97
                            var t1745 uint8 = t1744 + 10
                            var t1746 int = int(uint8(t1745))
                            jp1729 = t1746
                            var t1732 bool = jp1729 < base__82
                            if t1732 {
                                var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1729,
                                }
                                return t1733
                            } else {
                                var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1734
                            }
                        }
                    } else {
                        var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1747
                    }
                }
            } else {
                jp1742 = false
                if jp1742 {
                    var t1743 uint8
                    var inline2263 bool = value__81 >= 65
                    var inline2265 bool
                    if inline2263 {
                        var inline2268 bool = value__81 <= 90
                        inline2265 = inline2268
                    } else {
                        inline2265 = false
                    }
                    if inline2265 {
                        var inline2266 uint8 = 97 - 65
                        var inline2267 uint8 = value__81 + inline2266
                        t1743 = inline2267
                        var t1744 uint8 = t1743 - 97
                        var t1745 uint8 = t1744 + 10
                        var t1746 int = int(uint8(t1745))
                        jp1729 = t1746
                        var t1732 bool = jp1729 < base__82
                        if t1732 {
                            var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1729,
                            }
                            return t1733
                        } else {
                            var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1734
                        }
                    } else {
                        t1743 = value__81
                        var t1744 uint8 = t1743 - 97
                        var t1745 uint8 = t1744 + 10
                        var t1746 int = int(uint8(t1745))
                        jp1729 = t1746
                        var t1732 bool = jp1729 < base__82
                        if t1732 {
                            var t1733 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1729,
                            }
                            return t1733
                        } else {
                            var t1734 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1734
                        }
                    }
                } else {
                    var t1747 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1747
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1756:
    for {
        var t1757 bool = carry__22 != 0
        if t1757 {
            var t1766 *_goml_vec_uint32 = value__20.words
            var t1767 int
            var inline2280 int = vec_len__Vec_6uint32(t1766)
            t1767 = inline2280
            var t1768 bool = index__23 == t1767
            if t1768 {
                var t1769 *_goml_vec_uint32 = value__20.words
                var inline2277 uint32 = 0
                vec_push__Vec_6uint32(t1769, inline2277)
            } else {}
            var t1759 *_goml_vec_uint32 = value__20.words
            var t1760 uint32 = vec_get__Vec_6uint32(t1759, index__23)
            var t1761 uint64 = uint64(uint32(t1760))
            var sum__24 uint64 = t1761 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1763_rhs int = 32
            var t1763 uint64 = sum__24 >> t1763_rhs
            carry__22 = t1763
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1764 int = compound_old42 + compound_value43
            index__23 = t1764
            continue
        } else {
            break Loop_loop1756
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1773 FloatNatural
    var inline2282 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2283 FloatNatural = FloatNatural{
        words: inline2282,
    }
    t1773 = inline2283
    var t1774 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1773,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1774
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1794 *_goml_vec_uint32 = value__9.words
    var t1795 bool
    var inline2289 int = vec_len__Vec_6uint32(t1794)
    var inline2290 bool = inline2289 == 0
    t1795 = inline2290
    if t1795 {
        return 0
    } else {
        var t1778 *_goml_vec_uint32 = value__9.words
        var t1779 *_goml_vec_uint32 = value__9.words
        var t1780 int
        var inline2287 int = vec_len__Vec_6uint32(t1779)
        t1780 = inline2287
        var t1781 int = t1780 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1778, t1781)
        var bits__11 int = 0
        Loop_loop1788:
        for {
            var t1789 bool = high__10 != 0
            if t1789 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1790 uint32 = compound_old9 >> compound_value10
                high__10 = t1790
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1792 int = compound_old12 + compound_value13
                bits__11 = t1792
                continue
            } else {
                break Loop_loop1788
            }
        }
        var t1783 *_goml_vec_uint32 = value__9.words
        var t1784 int
        var inline2285 int = vec_len__Vec_6uint32(t1783)
        t1784 = inline2285
        var t1785 int = t1784 - 1
        var t1786 int = t1785 * 32
        var t1787 int = t1786 + bits__11
        return t1787
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1817 *_goml_vec_uint32 = left__12.words
    var t1818 int
    var inline2300 int = vec_len__Vec_6uint32(t1817)
    t1818 = inline2300
    var t1819 *_goml_vec_uint32 = right__13.words
    var t1820 int
    var inline2298 int = vec_len__Vec_6uint32(t1819)
    t1820 = inline2298
    var t1821 bool = t1818 < t1820
    if t1821 {
        return -1
    } else {
        var t1823 *_goml_vec_uint32 = left__12.words
        var t1824 int
        var inline2294 int = vec_len__Vec_6uint32(t1823)
        t1824 = inline2294
        var t1825 *_goml_vec_uint32 = right__13.words
        var t1826 int
        var inline2292 int = vec_len__Vec_6uint32(t1825)
        t1826 = inline2292
        var t1827 bool = t1824 > t1826
        if t1827 {
            return 1
        } else {
            var t1799 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2296 int = vec_len__Vec_6uint32(t1799)
            index__14 = inline2296
            Loop_loop1801:
            for {
                var t1802 bool = index__14 > 0
                if t1802 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1803 int = compound_old17 - compound_value18
                    index__14 = t1803
                    var t1806 *_goml_vec_uint32 = left__12.words
                    var t1807 uint32 = vec_get__Vec_6uint32(t1806, index__14)
                    var t1808 *_goml_vec_uint32 = right__13.words
                    var t1809 uint32 = vec_get__Vec_6uint32(t1808, index__14)
                    var t1810 bool = t1807 < t1809
                    if t1810 {
                        return -1
                    } else {
                        var t1812 *_goml_vec_uint32 = left__12.words
                        var t1813 uint32 = vec_get__Vec_6uint32(t1812, index__14)
                        var t1814 *_goml_vec_uint32 = right__13.words
                        var t1815 uint32 = vec_get__Vec_6uint32(t1814, index__14)
                        var t1816 bool = t1813 > t1815
                        if t1816 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1801
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1863 bool = shift__57 >= 0
    var jp1831 FloatNatural
    if t1863 {
        var t1864 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1831 = t1864
    } else {
        var t1865 FloatNatural = float_natural_copy(numerator__55)
        jp1831 = t1865
    }
    var t1859 bool = shift__57 >= 0
    var jp1833 FloatNatural
    if t1859 {
        var t1860 FloatNatural = float_natural_copy(denominator__56)
        jp1833 = t1860
    } else {
        var t1861 int = 0 - shift__57
        var t1862 FloatNatural = float_natural_shift_left(denominator__56, t1861)
        jp1833 = t1862
    }
    var quotient__60 uint64 = 0
    Loop_loop1846:
    for {
        var t1847 int = float_natural_compare(jp1831, jp1833)
        var t1848 bool = t1847 >= 0
        if t1848 {
            var t1849 int = float_natural_bit_length(jp1831)
            var t1850 int = float_natural_bit_length(jp1833)
            var offset__61 int = t1849 - t1850
            var part__62 FloatNatural = float_natural_shift_left(jp1833, offset__61)
            var t1854 int = float_natural_compare(jp1831, part__62)
            var t1855 bool = t1854 < 0
            if t1855 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1856 int = compound_old105 - compound_value106
                offset__61 = t1856
                var t1858 FloatNatural = float_natural_shift_left(jp1833, offset__61)
                part__62 = t1858
            } else {}
            float_natural_subtract(jp1831, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1852 uint64 = compound_old111 | compound_value112
            quotient__60 = t1852
            continue
        } else {
            break Loop_loop1846
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1831, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1833)
    var t1840 bool = rounding__64 > 0
    var jp1837 bool
    if t1840 {
        jp1837 = true
    } else {
        var t1843 bool = rounding__64 == 0
        if t1843 {
            var t1844_rhs uint64 = 1
            var t1844 uint64 = quotient__60 & t1844_rhs
            var t1845 bool = t1844 == 1
            jp1837 = t1845
        } else {
            jp1837 = false
        }
    }
    if jp1837 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1838 uint64 = compound_old115 + compound_value116
        quotient__60 = t1838
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1881 bool = index__269 < 0
    var jp1873 bool
    if t1881 {
        jp1873 = true
    } else {
        var t1882 int
        var inline2302 int = _goml_runtime_core_string_len(value__268)
        t1882 = inline2302
        var t1883 bool = index__269 > t1882
        jp1873 = t1883
    }
    if jp1873 {
        return false
    } else {
        var t1876 int
        var inline2306 int = _goml_runtime_core_string_len(value__268)
        t1876 = inline2306
        var t1877 bool = index__269 == t1876
        if t1877 {
            return true
        } else {
            var t1878 uint8
            var inline2304 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1878 = inline2304
            var t1879_rhs uint8 = 192
            var t1879 uint8 = t1878 & t1879_rhs
            var t1880 bool = t1879 != 128
            return t1880
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1887:
    for {
        var t1888 *_goml_vec_uint32 = value__37.words
        var t1889 int
        var inline2310 int = vec_len__Vec_6uint32(t1888)
        t1889 = inline2310
        var t1890 bool = index__41 < t1889
        if t1890 {
            var t1904 *_goml_vec_uint32 = other__38.words
            var t1905 int
            var inline2308 int = vec_len__Vec_6uint32(t1904)
            t1905 = inline2308
            var t1906 bool = index__41 < t1905
            var jp1892 uint64
            if t1906 {
                var t1907 *_goml_vec_uint32 = other__38.words
                var t1908 uint32 = vec_get__Vec_6uint32(t1907, index__41)
                var t1909 uint64 = uint64(uint32(t1908))
                jp1892 = t1909
            } else {
                jp1892 = 0
            }
            var right__42 uint64 = jp1892 + borrow__40
            var t1893 *_goml_vec_uint32 = value__37.words
            var t1894 uint32 = vec_get__Vec_6uint32(t1893, index__41)
            var left__43 uint64 = uint64(uint32(t1894))
            var t1898 bool = left__43 >= right__42
            if t1898 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1899 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1899))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1901 uint64 = base__39 + left__43
                var t1902 uint64 = t1901 - right__42
                var value75 uint32 = uint32(uint64(t1902))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1896 int = compound_old79 + compound_value80
            index__41 = t1896
            continue
        } else {
            break Loop_loop1887
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
