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

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_4bool_3int struct {
    _0 bool
    _1 int
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
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
    var mtmp796 Tuple2_4bool_7float64
    var inline1902 string = "3.125"
    var inline1903 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline1902)
    mtmp796 = inline1903
    var x797 bool = mtmp796._0
    var x798 float64 = mtmp796._1
    var t811 string
    var inline1900 string = _goml_runtime_core_bool_to_string(x797)
    t811 = inline1900
    var inline1897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline1897)
    var t812 string
    var inline1895 string = __goml_builtin_float64_to_string(x798)
    t812 = inline1895
    var inline1892 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline1892)
    var mtmp801 Tuple2_4bool_7float64
    var inline1889 string = "3.14"
    var inline1890 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float32(inline1889)
    mtmp801 = inline1890
    var x802 bool = mtmp801._0
    var x803 float64 = mtmp801._1
    var t813 string
    var inline1887 string = _goml_runtime_core_bool_to_string(x802)
    t813 = inline1887
    var inline1884 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline1884)
    var t814 string
    var inline1882 string = __goml_builtin_float64_to_string(x803)
    t814 = inline1882
    var inline1879 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline1879)
    var mtmp806 Tuple2_4bool_7float64
    var inline1876 string = "not-a-number"
    var inline1877 Tuple2_4bool_7float64 = __goml_builtin_string_parse_float64(inline1876)
    mtmp806 = inline1877
    var x807 bool = mtmp806._0
    var x808 float64 = mtmp806._1
    var t815 string
    var inline1874 string = _goml_runtime_core_bool_to_string(x807)
    t815 = inline1874
    var inline1871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline1871)
    var t816 string
    var inline1869 string = __goml_builtin_float64_to_string(x808)
    t816 = inline1869
    var inline1866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline1866)
    return struct{}{}
}

func __goml_builtin_string_parse_float64(value__199 string) Tuple2_4bool_7float64 {
    var mtmp348 Tuple2_4bool_6uint64 = parsed_float_bits(value__199, 52, 1023)
    var x349 bool = mtmp348._0
    var x350 uint64 = mtmp348._1
    var t835 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(x350)
    var t836 Tuple2_4bool_7float64 = Tuple2_4bool_7float64{
        _0: x349,
        _1: t835,
    }
    return t836
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_float64_to_string(value__195 float64) string {
    var t841 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__195)
    var t842 string = format_float_bits(t841, 52, 11, 1023)
    return t842
}

func __goml_builtin_string_parse_float32(value__196 string) Tuple2_4bool_7float64 {
    var mtmp345 Tuple2_4bool_6uint64 = parsed_float_bits(value__196, 23, 127)
    var x346 bool = mtmp345._0
    var x347 uint64 = mtmp345._1
    var t845 uint32 = uint32(uint64(x347))
    var t846 uint64 = float32_bits_to_float64_bits(t845)
    var t847 float64 = _goml_ffi_math_x00_Float64frombit__q__m__z_f64_hf87502f5f85b19627186df087af51f83(t846)
    var t848 Tuple2_4bool_7float64 = Tuple2_4bool_7float64{
        _0: x346,
        _1: t847,
    }
    return t848
}

func parsed_float_bits(value__107 string, mantissa_bits__108 int, exponent_bias__109 int) Tuple2_4bool_6uint64 {
    var parsed__110 ParsedFloat = parse_float_text(value__107)
    var t942 bool = parsed__110.valid
    var t943 bool = !t942
    if t943 {
        var t944 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t944
    } else {
        var t936 bool = parsed__110.negative
        var jp853 uint64
        if t936 {
            var t941 bool = mantissa_bits__108 == 23
            var jp938 int
            if t941 {
                jp938 = 8
            } else {
                jp938 = 11
            }
            var t939 int = mantissa_bits__108 + jp938
            var t940_lhs uint64 = 1
            var t940 uint64 = t940_lhs << t939
            jp853 = t940
        } else {
            jp853 = 0
        }
        var t935 bool = mantissa_bits__108 == 23
        var jp855 int
        if t935 {
            jp855 = 8
        } else {
            jp855 = 11
        }
        var t856_lhs uint64 = 1
        var t856 uint64 = t856_lhs << jp855
        var t857 uint64 = t856 - 1
        var exponent_mask__112 uint64 = t857 << mantissa_bits__108
        var t913 int = parsed__110.special
        var t914 bool = t913 == 1
        if t914 {
            var t915 uint64 = jp853 | exponent_mask__112
            var t916 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t915,
            }
            return t916
        } else {
            var t918 int = parsed__110.special
            var t919 bool = t918 == 2
            if t919 {
                var t923 int = mantissa_bits__108 - 1
                var t924_lhs uint64 = 1
                var t924 uint64 = t924_lhs << t923
                var t925 uint64 = exponent_mask__112 | t924
                var t930 bool = mantissa_bits__108 == 52
                var jp927 uint64
                if t930 {
                    jp927 = 1
                } else {
                    jp927 = 0
                }
                var t928 uint64 = t925 | jp927
                var t929 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t928,
                }
                return t929
            } else {
                var t932 FloatNatural = parsed__110.numerator
                var t933 bool
                var inline1927 *_goml_vec_uint32 = t932.words
                var inline1928 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1927)
                t933 = inline1928
                if t933 {
                    var t934 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp853,
                    }
                    return t934
                } else {
                    var t896 bool = parsed__110.hexadecimal
                    var t897 bool = !t896
                    if t897 {
                        var t898 int = parsed__110.significant_digits
                        var t899 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t898 + t899
                        var t912 bool = mantissa_bits__108 == 23
                        var jp901 int
                        if t912 {
                            jp901 = 40
                        } else {
                            jp901 = 310
                        }
                        var t911 bool = mantissa_bits__108 == 23
                        var jp903 int
                        if t911 {
                            jp903 = -46
                        } else {
                            jp903 = -325
                        }
                        var t905 bool = decimal_position__113 > jp901
                        if t905 {
                            var t906 uint64 = jp853 | exponent_mask__112
                            var t907 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t906,
                            }
                            return t907
                        } else {
                            var t909 bool = decimal_position__113 < jp903
                            if t909 {
                                var t910 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp853,
                                }
                                return t910
                            } else {
                                var t892 bool = parsed__110.hexadecimal
                                var t893 bool = !t892
                                var jp887 bool
                                if t893 {
                                    var t894 int = parsed__110.decimal_exponent
                                    var t895 bool = t894 < 0
                                    jp887 = t895
                                } else {
                                    jp887 = false
                                }
                                var jp861 FloatNatural
                                if jp887 {
                                    var t888 int = parsed__110.decimal_exponent
                                    var t889 int = 0 - t888
                                    var t890 FloatNatural = float_natural_power5(t889)
                                    jp861 = t890
                                } else {
                                    var inline1930 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline1930, 1)
                                    var inline1932 FloatNatural = FloatNatural{
                                        words: inline1930,
                                    }
                                    jp861 = inline1932
                                }
                                var t882 bool = parsed__110.hexadecimal
                                var t883 bool = !t882
                                var jp873 bool
                                if t883 {
                                    var t884 int = parsed__110.decimal_exponent
                                    var t885 bool = t884 > 0
                                    jp873 = t885
                                } else {
                                    jp873 = false
                                }
                                var jp863 FloatNatural
                                if jp873 {
                                    var t874 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t874)
                                    var count__118 int = 0
                                    Loop_loop876:
                                    for {
                                        var t877 int = parsed__110.decimal_exponent
                                        var t878 bool = count__118 < t877
                                        if t878 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t879 int = compound_old213 + compound_value214
                                            count__118 = t879
                                            continue
                                        } else {
                                            break Loop_loop876
                                        }
                                    }
                                    jp863 = result__117
                                    var t869 bool = parsed__110.hexadecimal
                                    var jp865 int
                                    if t869 {
                                        var t870 int = parsed__110.binary_exponent
                                        jp865 = t870
                                    } else {
                                        var t871 int = parsed__110.decimal_exponent
                                        jp865 = t871
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp863, jp861, jp865, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t866 bool = !x219
                                    var t867 uint64 = jp853 | x218
                                    var t868 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t866,
                                        _1: t867,
                                    }
                                    return t868
                                } else {
                                    var t881 FloatNatural = parsed__110.numerator
                                    jp863 = t881
                                    var t869 bool = parsed__110.hexadecimal
                                    var jp865 int
                                    if t869 {
                                        var t870 int = parsed__110.binary_exponent
                                        jp865 = t870
                                    } else {
                                        var t871 int = parsed__110.decimal_exponent
                                        jp865 = t871
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp863, jp861, jp865, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t866 bool = !x219
                                    var t867 uint64 = jp853 | x218
                                    var t868 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t866,
                                        _1: t867,
                                    }
                                    return t868
                                }
                            }
                        }
                    } else {
                        var t892 bool = parsed__110.hexadecimal
                        var t893 bool = !t892
                        var jp887 bool
                        if t893 {
                            var t894 int = parsed__110.decimal_exponent
                            var t895 bool = t894 < 0
                            jp887 = t895
                        } else {
                            jp887 = false
                        }
                        var jp861 FloatNatural
                        if jp887 {
                            var t888 int = parsed__110.decimal_exponent
                            var t889 int = 0 - t888
                            var t890 FloatNatural = float_natural_power5(t889)
                            jp861 = t890
                        } else {
                            var inline1930 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline1930, 1)
                            var inline1932 FloatNatural = FloatNatural{
                                words: inline1930,
                            }
                            jp861 = inline1932
                        }
                        var t882 bool = parsed__110.hexadecimal
                        var t883 bool = !t882
                        var jp873 bool
                        if t883 {
                            var t884 int = parsed__110.decimal_exponent
                            var t885 bool = t884 > 0
                            jp873 = t885
                        } else {
                            jp873 = false
                        }
                        var jp863 FloatNatural
                        if jp873 {
                            var t874 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t874)
                            var count__118 int = 0
                            Loop_loop876__2:
                            for {
                                var t877 int = parsed__110.decimal_exponent
                                var t878 bool = count__118 < t877
                                if t878 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t879 int = compound_old213 + compound_value214
                                    count__118 = t879
                                    continue
                                } else {
                                    break Loop_loop876__2
                                }
                            }
                            jp863 = result__117
                            var t869 bool = parsed__110.hexadecimal
                            var jp865 int
                            if t869 {
                                var t870 int = parsed__110.binary_exponent
                                jp865 = t870
                            } else {
                                var t871 int = parsed__110.decimal_exponent
                                jp865 = t871
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp863, jp861, jp865, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t866 bool = !x219
                            var t867 uint64 = jp853 | x218
                            var t868 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t866,
                                _1: t867,
                            }
                            return t868
                        } else {
                            var t881 FloatNatural = parsed__110.numerator
                            jp863 = t881
                            var t869 bool = parsed__110.hexadecimal
                            var jp865 int
                            if t869 {
                                var t870 int = parsed__110.binary_exponent
                                jp865 = t870
                            } else {
                                var t871 int = parsed__110.decimal_exponent
                                jp865 = t871
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp863, jp861, jp865, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t866 bool = !x219
                            var t867 uint64 = jp853 | x218
                            var t868 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t866,
                                _1: t867,
                            }
                            return t868
                        }
                    }
                }
            }
        }
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t947 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t947
    var t948 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t948 != 0
    var t949_lhs uint64 = 1
    var t949 uint64 = t949_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t949 - 1
    var t950 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t950 & exponent_mask__166
    var t951_lhs uint64 = 1
    var t951 uint64 = t951_lhs << mantissa_bits__161
    var t952 uint64 = t951 - 1
    var fraction__168 uint64 = bits__160 & t952
    var t1016 bool = exponent__167 == exponent_mask__166
    if t1016 {
        var t1018 bool = fraction__168 == 0
        if t1018 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t1024 bool = exponent__167 == 0
        var jp1022 bool
        if t1024 {
            var t1025 bool = fraction__168 == 0
            jp1022 = t1025
        } else {
            jp1022 = false
        }
        if jp1022 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t1013 bool = exponent__167 == 0
            var jp955 uint64
            if t1013 {
                jp955 = fraction__168
            } else {
                var t1014_lhs uint64 = 1
                var t1014 uint64 = t1014_lhs << mantissa_bits__161
                var t1015 uint64 = fraction__168 | t1014
                jp955 = t1015
            }
            var t1007 bool = exponent__167 == 0
            var jp957 int
            if t1007 {
                var t1008 int = 1 - exponent_bias__163
                var t1009 int = t1008 - mantissa_bits__161
                jp957 = t1009
            } else {
                var t1010 int = int(uint64(exponent__167))
                var t1011 int = t1010 - exponent_bias__163
                var t1012 int = t1011 - mantissa_bits__161
                jp957 = t1012
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp955)
            var t962 bool = jp957 >= 0
            var jp959 int
            if t962 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp957)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t981 bool = mantissa_bits__161 == 23
                var jp964 int
                if t981 {
                    jp964 = 9
                } else {
                    jp964 = 17
                }
                var t978 int
                var inline1940 int = _goml_runtime_core_string_len(digits__173)
                t978 = inline1940
                var t979 bool = t978 < jp964
                var jp966 int
                if t979 {
                    var inline1934 int = _goml_runtime_core_string_len(digits__173)
                    jp966 = inline1934
                } else {
                    jp966 = jp964
                }
                var count__176 int = 1
                Loop_loop969:
                for {
                    var t970 bool = count__176 <= jp966
                    if t970 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t971 int
                        var inline1936 int = _goml_runtime_core_string_len(digits__173)
                        t971 = inline1936
                        var jp973 int
                        if x319 {
                            jp973 = 1
                        } else {
                            jp973 = 0
                        }
                        var point__180 int = t971 + jp973
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t977 bool = x322 == bits__160
                        if t977 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t975 int = compound_old324 + compound_value325
                            count__176 = t975
                            continue
                        }
                    } else {
                        break Loop_loop969
                    }
                }
                var inline1938 int = _goml_runtime_core_string_len(digits__173)
                jp959 = inline1938
                var t960 string = float_natural_decimal(exact_value__171)
                var t961 string = fixed_float_text(t960, jp959, negative__165)
                return t961
            } else {
                var count__183 int = 0
                var t1003 int = 0 - jp957
                Loop_loop1002:
                for {
                    var t1004 bool = count__183 < t1003
                    if t1004 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t1005 int = compound_old329 + compound_value330
                        count__183 = t1005
                        continue
                    } else {
                        break Loop_loop1002
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t983 int
                var inline1946 int = _goml_runtime_core_string_len(digits__184)
                t983 = inline1946
                var point__185 int = t983 + jp957
                var t1001 bool = mantissa_bits__161 == 23
                var jp985 int
                if t1001 {
                    jp985 = 9
                } else {
                    jp985 = 17
                }
                var t998 int
                var inline1944 int = _goml_runtime_core_string_len(digits__184)
                t998 = inline1944
                var t999 bool = t998 < jp985
                var jp987 int
                if t999 {
                    var inline1942 int = _goml_runtime_core_string_len(digits__184)
                    jp987 = inline1942
                } else {
                    jp987 = jp985
                }
                count__183 = 1
                Loop_loop989:
                for {
                    var t990 bool = count__183 <= jp987
                    if t990 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp992 int
                        if x336 {
                            jp992 = 1
                        } else {
                            jp992 = 0
                        }
                        var t993 int = point__185 + jp992
                        var candidate__191 string = fixed_float_text(rounded__190, t993, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t997 bool = x339 == bits__160
                        if t997 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t995 int = compound_old341 + compound_value342
                            count__183 = t995
                            continue
                        }
                    } else {
                        break Loop_loop989
                    }
                }
                jp959 = point__185
                var t960 string = float_natural_decimal(exact_value__171)
                var t961 string = fixed_float_text(t960, jp959, negative__165)
                return t961
            }
        }
    }
}

func float32_bits_to_float64_bits(value__123 uint32) uint64 {
    var t1028 uint64 = uint64(uint32(value__123))
    var t1029_rhs uint64 = 2147483648
    var t1029 uint64 = t1028 & t1029_rhs
    var sign__124_rhs int = 32
    var sign__124 uint64 = t1029 << sign__124_rhs
    var t1030_rhs int = 23
    var t1030 uint32 = value__123 >> t1030_rhs
    var exponent__125_rhs uint32 = 255
    var exponent__125 uint32 = t1030 & exponent__125_rhs
    var fraction__126_rhs uint32 = 8388607
    var fraction__126 uint32 = value__123 & fraction__126_rhs
    var t1033 bool = exponent__125 == 255
    if t1033 {
        var t1036 bool = fraction__126 == 0
        if t1036 {
            var t1037_rhs uint64 = 9218868437227405312
            var t1037 uint64 = sign__124 | t1037_rhs
            return t1037
        } else {
            return 9221120237041090560
        }
    } else {
        var t1040 bool = exponent__125 == 0
        if t1040 {
            var t1043 bool = fraction__126 == 0
            if t1043 {
                return sign__124
            } else {
                var high__127 int = 0
                var remaining__128 uint32 = fraction__126
                Loop_loop1055:
                for {
                    var t1056 bool = remaining__128 > 1
                    if t1056 {
                        var compound_old220 uint32 = remaining__128
                        var compound_value221 int = 1
                        var t1057 uint32 = compound_old220 >> compound_value221
                        remaining__128 = t1057
                        var compound_old223 int = high__127
                        var compound_value224 int = 1
                        var t1059 int = compound_old223 + compound_value224
                        high__127 = t1059
                        continue
                    } else {
                        break Loop_loop1055
                    }
                }
                var unbiased__129 int = high__127 - 149
                var t1045 int = unbiased__129 + 1023
                var t1046 uint64 = uint64(int(t1045))
                var t1047_rhs int = 52
                var t1047 uint64 = t1046 << t1047_rhs
                var t1048 uint64 = sign__124 | t1047
                var t1049 uint64 = uint64(uint32(fraction__126))
                var t1050_lhs uint64 = 1
                var t1050 uint64 = t1050_lhs << high__127
                var t1051 uint64 = t1049 - t1050
                var t1052 int = 52 - high__127
                var t1053 uint64 = t1051 << t1052
                var t1054 uint64 = t1048 | t1053
                return t1054
            }
        } else {
            var t1061 int = int(uint32(exponent__125))
            var t1062 int = t1061 - 127
            var t1063 int = t1062 + 1023
            var t1064 uint64 = uint64(int(t1063))
            var t1065_rhs int = 52
            var t1065 uint64 = t1064 << t1065_rhs
            var t1066 uint64 = sign__124 | t1065
            var t1067 uint64 = uint64(uint32(fraction__126))
            var t1068_rhs int = 29
            var t1068 uint64 = t1067 << t1068_rhs
            var t1069 uint64 = t1066 | t1068
            return t1069
        }
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1254 bool = string_equals_ascii_case(value__84, "nan")
    if t1254 {
        var t1255 FloatNatural
        var inline1948 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1949 FloatNatural = FloatNatural{
            words: inline1948,
        }
        t1255 = inline1949
        var t1256 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1255,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1256
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1246 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1247 bool = index__85 < t1246
        var jp1241 bool
        if t1247 {
            var t1250 uint8
            var inline1953 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1250 = inline1953
            var t1251 bool = t1250 == 43
            if t1251 {
                jp1241 = true
            } else {
                var t1252 uint8
                var inline1951 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1252 = inline1951
                var t1253 bool = t1252 == 45
                jp1241 = t1253
            }
        } else {
            jp1241 = false
        }
        if jp1241 {
            var t1242 uint8
            var inline1955 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1242 = inline1955
            var t1243 bool = t1242 == 45
            negative__86 = t1243
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1244 int = compound_old140 + compound_value141
            index__85 = t1244
        } else {}
        var t1074 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1074)
        var t1238 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1235 bool
        if t1238 {
            jp1235 = true
        } else {
            var t1239 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1235 = t1239
        }
        if jp1235 {
            var t1236 FloatNatural
            var inline1957 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1958 FloatNatural = FloatNatural{
                words: inline1957,
            }
            t1236 = inline1958
            var t1237 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1236,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1237
        } else {
            var t1229 int = index__85 + 2
            var t1230 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1231 bool = t1229 <= t1230
            var jp1224 bool
            if t1231 {
                var t1232 uint8
                var inline1960 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1232 = inline1960
                var t1233 bool = t1232 == 48
                jp1224 = t1233
            } else {
                jp1224 = false
            }
            var jp1077 bool
            if jp1224 {
                var t1225 int = index__85 + 1
                var t1226 uint8
                var inline1969 uint8 = _goml_runtime_core_string_byte_get(value__84, t1225)
                t1226 = inline1969
                var t1227 uint8
                var inline1962 bool = t1226 >= 65
                var inline1964 bool
                if inline1962 {
                    var inline1967 bool = t1226 <= 90
                    inline1964 = inline1967
                } else {
                    inline1964 = false
                }
                if inline1964 {
                    var inline1965 uint8 = 97 - 65
                    var inline1966 uint8 = t1226 + inline1965
                    t1227 = inline1966
                    var t1228 bool = t1227 == 120
                    jp1077 = t1228
                    if jp1077 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1221 int = compound_old145 + compound_value146
                        index__85 = t1221
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1080 int
                    if jp1077 {
                        jp1080 = 16
                    } else {
                        jp1080 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1174 uint32 = uint32(int(jp1080))
                    Loop_loop1170:
                    for {
                        var t1171 int
                        var inline1983 int = _goml_runtime_core_string_len(value__84)
                        t1171 = inline1983
                        var t1172 bool = index__85 < t1171
                        if t1172 {
                            var current__97 uint8
                            var inline1981 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline1981
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1080)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1174)
                                var t1175 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1175)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1186 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1186
                                } else {}
                                var t1184 bool = significant_digits__95 > 0
                                var jp1181 bool
                                if t1184 {
                                    jp1181 = true
                                } else {
                                    var t1185 bool = x151 != 0
                                    jp1181 = t1185
                                }
                                if jp1181 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1182 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1182
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1178 int = compound_old164 + compound_value165
                                index__85 = t1178
                                continue
                            } else {
                                var t1189 bool = current__97 == 95
                                if t1189 {
                                    var t1210 int = index__85 + 1
                                    var t1211 int
                                    var inline1979 int = _goml_runtime_core_string_len(value__84)
                                    t1211 = inline1979
                                    var t1212 bool = t1210 >= t1211
                                    if t1212 {
                                        var inline1971 FloatNatural = float_natural_zero()
                                        var inline1972 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline1971,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline1972
                                    } else {
                                        var t1191 int = index__85 + 1
                                        var t1192 uint8
                                        var inline1977 uint8 = _goml_runtime_core_string_byte_get(value__84, t1191)
                                        t1192 = inline1977
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1192, jp1080)
                                        var x169 bool = mtmp168._0
                                        var jp1207 bool
                                        if jp1077 {
                                            var t1209 bool = !saw_digit__92
                                            jp1207 = t1209
                                        } else {
                                            jp1207 = false
                                        }
                                        var jp1194 bool
                                        if jp1207 {
                                            var t1208 bool = index__85 == mantissa_start__89
                                            jp1194 = t1208
                                        } else {
                                            jp1194 = false
                                        }
                                        var t1204 bool = !previous_digit__96
                                        var jp1202 bool
                                        if t1204 {
                                            var t1205 bool = !jp1194
                                            jp1202 = t1205
                                        } else {
                                            jp1202 = false
                                        }
                                        var jp1199 bool
                                        if jp1202 {
                                            jp1199 = true
                                        } else {
                                            var t1203 bool = !x169
                                            jp1199 = t1203
                                        }
                                        if jp1199 {
                                            var inline1974 FloatNatural = float_natural_zero()
                                            var inline1975 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline1974,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline1975
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1196 int = compound_old173 + compound_value174
                                            index__85 = t1196
                                            continue
                                        }
                                    }
                                } else {
                                    var t1219 bool = current__97 == 46
                                    var jp1216 bool
                                    if t1219 {
                                        var t1220 bool = !saw_dot__93
                                        jp1216 = t1220
                                    } else {
                                        jp1216 = false
                                    }
                                    if jp1216 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1217 int = compound_old178 + compound_value179
                                        index__85 = t1217
                                        continue
                                    } else {
                                        break Loop_loop1170
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1170
                        }
                    }
                    var t1168 bool = !saw_digit__92
                    if t1168 {
                        var inline1985 FloatNatural = float_natural_zero()
                        var inline1986 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline1985,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline1986
                    } else {
                        var jp1084 uint8
                        if jp1077 {
                            jp1084 = 112
                        } else {
                            jp1084 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1163 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1164 bool = index__85 < t1163
                        var jp1101 bool
                        if t1164 {
                            var t1165 uint8
                            var inline1988 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1165 = inline1988
                            var t1166 uint8 = ascii_lower(t1165)
                            var t1167 bool = t1166 == jp1084
                            jp1101 = t1167
                        } else {
                            jp1101 = false
                        }
                        if jp1101 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1102 int = compound_old183 + compound_value184
                            index__85 = t1102
                            var t1153 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1154 bool = index__85 < t1153
                            var jp1148 bool
                            if t1154 {
                                var t1157 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1158 bool = t1157 == 43
                                if t1158 {
                                    jp1148 = true
                                } else {
                                    var t1159 uint8
                                    var inline1990 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1159 = inline1990
                                    var t1160 bool = t1159 == 45
                                    jp1148 = t1160
                                }
                            } else {
                                jp1148 = false
                            }
                            if jp1148 {
                                var t1149 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1150 bool = t1149 == 45
                                exponent_negative__104 = t1150
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1151 int = compound_old187 + compound_value188
                                index__85 = t1151
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1109:
                            for {
                                var t1110 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1111 bool = index__85 < t1110
                                if t1111 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1145 bool = current__106 >= 48
                                    var jp1114 bool
                                    if t1145 {
                                        var t1146 bool = current__106 <= 57
                                        jp1114 = t1146
                                    } else {
                                        jp1114 = false
                                    }
                                    if jp1114 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1118 bool = exponent__103 < 1000000
                                        if t1118 {
                                            var t1119 int = exponent__103 * 10
                                            var t1120 uint8 = current__106 - 48
                                            var t1121 int = int(uint8(t1120))
                                            var t1122 int = t1119 + t1121
                                            exponent__103 = t1122
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1116 int = compound_old196 + compound_value197
                                        index__85 = t1116
                                        continue
                                    } else {
                                        var t1124 bool = current__106 == 95
                                        if t1124 {
                                            var t1141 bool = !previous_digit__96
                                            var jp1137 bool
                                            if t1141 {
                                                jp1137 = true
                                            } else {
                                                var t1142 int = index__85 + 1
                                                var t1143 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1144 bool = t1142 >= t1143
                                                jp1137 = t1144
                                            }
                                            var jp1132 bool
                                            if jp1137 {
                                                jp1132 = true
                                            } else {
                                                var t1138 int = index__85 + 1
                                                var t1139 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1138)
                                                var t1140 bool = t1139 < 48
                                                jp1132 = t1140
                                            }
                                            var jp1129 bool
                                            if jp1132 {
                                                jp1129 = true
                                            } else {
                                                var t1133 int = index__85 + 1
                                                var t1134 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1133)
                                                var t1135 bool = t1134 > 57
                                                jp1129 = t1135
                                            }
                                            if jp1129 {
                                                var t1130 ParsedFloat = invalid_parsed_float()
                                                return t1130
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1126 int = compound_old201 + compound_value202
                                                index__85 = t1126
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1109
                                        }
                                    }
                                } else {
                                    break Loop_loop1109
                                }
                            }
                            var t1107 bool = !exponent_digits__105
                            if t1107 {
                                var t1108 ParsedFloat = invalid_parsed_float()
                                return t1108
                            } else {
                                var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1098 bool = index__85 != t1097
                                if t1098 {
                                    var t1099 ParsedFloat = invalid_parsed_float()
                                    return t1099
                                } else {
                                    if exponent_negative__104 {
                                        var t1096 int = 0 - exponent__103
                                        exponent__103 = t1096
                                    } else {}
                                    var jp1089 int
                                    if jp1077 {
                                        jp1089 = 0
                                    } else {
                                        var t1095 int = exponent__103 - fraction_digits__94
                                        jp1089 = t1095
                                    }
                                    var jp1091 int
                                    if jp1077 {
                                        var t1093 int = fraction_digits__94 * 4
                                        var t1094 int = exponent__103 - t1093
                                        jp1091 = t1094
                                    } else {
                                        jp1091 = 0
                                    }
                                    var t1092 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1089,
                                        binary_exponent: jp1091,
                                        hexadecimal: jp1077,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1092
                                }
                            }
                        } else {
                            if jp1077 {
                                var t1162 ParsedFloat = invalid_parsed_float()
                                return t1162
                            } else {
                                var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1098 bool = index__85 != t1097
                                if t1098 {
                                    var t1099 ParsedFloat = invalid_parsed_float()
                                    return t1099
                                } else {
                                    if exponent_negative__104 {
                                        var t1096 int = 0 - exponent__103
                                        exponent__103 = t1096
                                    } else {}
                                    var jp1089 int
                                    if jp1077 {
                                        jp1089 = 0
                                    } else {
                                        var t1095 int = exponent__103 - fraction_digits__94
                                        jp1089 = t1095
                                    }
                                    var jp1091 int
                                    if jp1077 {
                                        var t1093 int = fraction_digits__94 * 4
                                        var t1094 int = exponent__103 - t1093
                                        jp1091 = t1094
                                    } else {
                                        jp1091 = 0
                                    }
                                    var t1092 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1089,
                                        binary_exponent: jp1091,
                                        hexadecimal: jp1077,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1092
                                }
                            }
                        }
                    }
                } else {
                    t1227 = t1226
                    var t1228 bool = t1227 == 120
                    jp1077 = t1228
                    if jp1077 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1221 int = compound_old145 + compound_value146
                        index__85 = t1221
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1080 int
                    if jp1077 {
                        jp1080 = 16
                    } else {
                        jp1080 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1174 uint32 = uint32(int(jp1080))
                    Loop_loop1170__2:
                    for {
                        var t1171 int
                        var inline1983 int = _goml_runtime_core_string_len(value__84)
                        t1171 = inline1983
                        var t1172 bool = index__85 < t1171
                        if t1172 {
                            var current__97 uint8
                            var inline1981 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline1981
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1080)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1174)
                                var t1175 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1175)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1186 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1186
                                } else {}
                                var t1184 bool = significant_digits__95 > 0
                                var jp1181 bool
                                if t1184 {
                                    jp1181 = true
                                } else {
                                    var t1185 bool = x151 != 0
                                    jp1181 = t1185
                                }
                                if jp1181 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1182 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1182
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1178 int = compound_old164 + compound_value165
                                index__85 = t1178
                                continue
                            } else {
                                var t1189 bool = current__97 == 95
                                if t1189 {
                                    var t1210 int = index__85 + 1
                                    var t1211 int
                                    var inline1979 int = _goml_runtime_core_string_len(value__84)
                                    t1211 = inline1979
                                    var t1212 bool = t1210 >= t1211
                                    if t1212 {
                                        var inline1971 FloatNatural = float_natural_zero()
                                        var inline1972 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline1971,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline1972
                                    } else {
                                        var t1191 int = index__85 + 1
                                        var t1192 uint8
                                        var inline1977 uint8 = _goml_runtime_core_string_byte_get(value__84, t1191)
                                        t1192 = inline1977
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1192, jp1080)
                                        var x169 bool = mtmp168._0
                                        var jp1207 bool
                                        if jp1077 {
                                            var t1209 bool = !saw_digit__92
                                            jp1207 = t1209
                                        } else {
                                            jp1207 = false
                                        }
                                        var jp1194 bool
                                        if jp1207 {
                                            var t1208 bool = index__85 == mantissa_start__89
                                            jp1194 = t1208
                                        } else {
                                            jp1194 = false
                                        }
                                        var t1204 bool = !previous_digit__96
                                        var jp1202 bool
                                        if t1204 {
                                            var t1205 bool = !jp1194
                                            jp1202 = t1205
                                        } else {
                                            jp1202 = false
                                        }
                                        var jp1199 bool
                                        if jp1202 {
                                            jp1199 = true
                                        } else {
                                            var t1203 bool = !x169
                                            jp1199 = t1203
                                        }
                                        if jp1199 {
                                            var inline1974 FloatNatural = float_natural_zero()
                                            var inline1975 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline1974,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline1975
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1196 int = compound_old173 + compound_value174
                                            index__85 = t1196
                                            continue
                                        }
                                    }
                                } else {
                                    var t1219 bool = current__97 == 46
                                    var jp1216 bool
                                    if t1219 {
                                        var t1220 bool = !saw_dot__93
                                        jp1216 = t1220
                                    } else {
                                        jp1216 = false
                                    }
                                    if jp1216 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1217 int = compound_old178 + compound_value179
                                        index__85 = t1217
                                        continue
                                    } else {
                                        break Loop_loop1170__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1170__2
                        }
                    }
                    var t1168 bool = !saw_digit__92
                    if t1168 {
                        var inline1985 FloatNatural = float_natural_zero()
                        var inline1986 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline1985,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline1986
                    } else {
                        var jp1084 uint8
                        if jp1077 {
                            jp1084 = 112
                        } else {
                            jp1084 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1163 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1164 bool = index__85 < t1163
                        var jp1101 bool
                        if t1164 {
                            var t1165 uint8
                            var inline1988 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1165 = inline1988
                            var t1166 uint8 = ascii_lower(t1165)
                            var t1167 bool = t1166 == jp1084
                            jp1101 = t1167
                        } else {
                            jp1101 = false
                        }
                        if jp1101 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1102 int = compound_old183 + compound_value184
                            index__85 = t1102
                            var t1153 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1154 bool = index__85 < t1153
                            var jp1148 bool
                            if t1154 {
                                var t1157 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1158 bool = t1157 == 43
                                if t1158 {
                                    jp1148 = true
                                } else {
                                    var t1159 uint8
                                    var inline1990 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1159 = inline1990
                                    var t1160 bool = t1159 == 45
                                    jp1148 = t1160
                                }
                            } else {
                                jp1148 = false
                            }
                            if jp1148 {
                                var t1149 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1150 bool = t1149 == 45
                                exponent_negative__104 = t1150
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1151 int = compound_old187 + compound_value188
                                index__85 = t1151
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1109__2:
                            for {
                                var t1110 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1111 bool = index__85 < t1110
                                if t1111 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1145 bool = current__106 >= 48
                                    var jp1114 bool
                                    if t1145 {
                                        var t1146 bool = current__106 <= 57
                                        jp1114 = t1146
                                    } else {
                                        jp1114 = false
                                    }
                                    if jp1114 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1118 bool = exponent__103 < 1000000
                                        if t1118 {
                                            var t1119 int = exponent__103 * 10
                                            var t1120 uint8 = current__106 - 48
                                            var t1121 int = int(uint8(t1120))
                                            var t1122 int = t1119 + t1121
                                            exponent__103 = t1122
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1116 int = compound_old196 + compound_value197
                                        index__85 = t1116
                                        continue
                                    } else {
                                        var t1124 bool = current__106 == 95
                                        if t1124 {
                                            var t1141 bool = !previous_digit__96
                                            var jp1137 bool
                                            if t1141 {
                                                jp1137 = true
                                            } else {
                                                var t1142 int = index__85 + 1
                                                var t1143 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1144 bool = t1142 >= t1143
                                                jp1137 = t1144
                                            }
                                            var jp1132 bool
                                            if jp1137 {
                                                jp1132 = true
                                            } else {
                                                var t1138 int = index__85 + 1
                                                var t1139 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1138)
                                                var t1140 bool = t1139 < 48
                                                jp1132 = t1140
                                            }
                                            var jp1129 bool
                                            if jp1132 {
                                                jp1129 = true
                                            } else {
                                                var t1133 int = index__85 + 1
                                                var t1134 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1133)
                                                var t1135 bool = t1134 > 57
                                                jp1129 = t1135
                                            }
                                            if jp1129 {
                                                var t1130 ParsedFloat = invalid_parsed_float()
                                                return t1130
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1126 int = compound_old201 + compound_value202
                                                index__85 = t1126
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1109__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1109__2
                                }
                            }
                            var t1107 bool = !exponent_digits__105
                            if t1107 {
                                var t1108 ParsedFloat = invalid_parsed_float()
                                return t1108
                            } else {
                                var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1098 bool = index__85 != t1097
                                if t1098 {
                                    var t1099 ParsedFloat = invalid_parsed_float()
                                    return t1099
                                } else {
                                    if exponent_negative__104 {
                                        var t1096 int = 0 - exponent__103
                                        exponent__103 = t1096
                                    } else {}
                                    var jp1089 int
                                    if jp1077 {
                                        jp1089 = 0
                                    } else {
                                        var t1095 int = exponent__103 - fraction_digits__94
                                        jp1089 = t1095
                                    }
                                    var jp1091 int
                                    if jp1077 {
                                        var t1093 int = fraction_digits__94 * 4
                                        var t1094 int = exponent__103 - t1093
                                        jp1091 = t1094
                                    } else {
                                        jp1091 = 0
                                    }
                                    var t1092 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1089,
                                        binary_exponent: jp1091,
                                        hexadecimal: jp1077,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1092
                                }
                            }
                        } else {
                            if jp1077 {
                                var t1162 ParsedFloat = invalid_parsed_float()
                                return t1162
                            } else {
                                var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1098 bool = index__85 != t1097
                                if t1098 {
                                    var t1099 ParsedFloat = invalid_parsed_float()
                                    return t1099
                                } else {
                                    if exponent_negative__104 {
                                        var t1096 int = 0 - exponent__103
                                        exponent__103 = t1096
                                    } else {}
                                    var jp1089 int
                                    if jp1077 {
                                        jp1089 = 0
                                    } else {
                                        var t1095 int = exponent__103 - fraction_digits__94
                                        jp1089 = t1095
                                    }
                                    var jp1091 int
                                    if jp1077 {
                                        var t1093 int = fraction_digits__94 * 4
                                        var t1094 int = exponent__103 - t1093
                                        jp1091 = t1094
                                    } else {
                                        jp1091 = 0
                                    }
                                    var t1092 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1089,
                                        binary_exponent: jp1091,
                                        hexadecimal: jp1077,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1092
                                }
                            }
                        }
                    }
                }
            } else {
                jp1077 = false
                if jp1077 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1221 int = compound_old145 + compound_value146
                    index__85 = t1221
                } else {}
                var mantissa_start__89 int = index__85
                var jp1080 int
                if jp1077 {
                    jp1080 = 16
                } else {
                    jp1080 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1174 uint32 = uint32(int(jp1080))
                Loop_loop1170__3:
                for {
                    var t1171 int
                    var inline1983 int = _goml_runtime_core_string_len(value__84)
                    t1171 = inline1983
                    var t1172 bool = index__85 < t1171
                    if t1172 {
                        var current__97 uint8
                        var inline1981 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline1981
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1080)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1174)
                            var t1175 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1175)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1186 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1186
                            } else {}
                            var t1184 bool = significant_digits__95 > 0
                            var jp1181 bool
                            if t1184 {
                                jp1181 = true
                            } else {
                                var t1185 bool = x151 != 0
                                jp1181 = t1185
                            }
                            if jp1181 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1182 int = compound_old160 + compound_value161
                                significant_digits__95 = t1182
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1178 int = compound_old164 + compound_value165
                            index__85 = t1178
                            continue
                        } else {
                            var t1189 bool = current__97 == 95
                            if t1189 {
                                var t1210 int = index__85 + 1
                                var t1211 int
                                var inline1979 int = _goml_runtime_core_string_len(value__84)
                                t1211 = inline1979
                                var t1212 bool = t1210 >= t1211
                                if t1212 {
                                    var inline1971 FloatNatural = float_natural_zero()
                                    var inline1972 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline1971,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline1972
                                } else {
                                    var t1191 int = index__85 + 1
                                    var t1192 uint8
                                    var inline1977 uint8 = _goml_runtime_core_string_byte_get(value__84, t1191)
                                    t1192 = inline1977
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1192, jp1080)
                                    var x169 bool = mtmp168._0
                                    var jp1207 bool
                                    if jp1077 {
                                        var t1209 bool = !saw_digit__92
                                        jp1207 = t1209
                                    } else {
                                        jp1207 = false
                                    }
                                    var jp1194 bool
                                    if jp1207 {
                                        var t1208 bool = index__85 == mantissa_start__89
                                        jp1194 = t1208
                                    } else {
                                        jp1194 = false
                                    }
                                    var t1204 bool = !previous_digit__96
                                    var jp1202 bool
                                    if t1204 {
                                        var t1205 bool = !jp1194
                                        jp1202 = t1205
                                    } else {
                                        jp1202 = false
                                    }
                                    var jp1199 bool
                                    if jp1202 {
                                        jp1199 = true
                                    } else {
                                        var t1203 bool = !x169
                                        jp1199 = t1203
                                    }
                                    if jp1199 {
                                        var inline1974 FloatNatural = float_natural_zero()
                                        var inline1975 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline1974,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline1975
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1196 int = compound_old173 + compound_value174
                                        index__85 = t1196
                                        continue
                                    }
                                }
                            } else {
                                var t1219 bool = current__97 == 46
                                var jp1216 bool
                                if t1219 {
                                    var t1220 bool = !saw_dot__93
                                    jp1216 = t1220
                                } else {
                                    jp1216 = false
                                }
                                if jp1216 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1217 int = compound_old178 + compound_value179
                                    index__85 = t1217
                                    continue
                                } else {
                                    break Loop_loop1170__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1170__3
                    }
                }
                var t1168 bool = !saw_digit__92
                if t1168 {
                    var inline1985 FloatNatural = float_natural_zero()
                    var inline1986 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline1985,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline1986
                } else {
                    var jp1084 uint8
                    if jp1077 {
                        jp1084 = 112
                    } else {
                        jp1084 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1163 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1164 bool = index__85 < t1163
                    var jp1101 bool
                    if t1164 {
                        var t1165 uint8
                        var inline1988 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1165 = inline1988
                        var t1166 uint8 = ascii_lower(t1165)
                        var t1167 bool = t1166 == jp1084
                        jp1101 = t1167
                    } else {
                        jp1101 = false
                    }
                    if jp1101 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1102 int = compound_old183 + compound_value184
                        index__85 = t1102
                        var t1153 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1154 bool = index__85 < t1153
                        var jp1148 bool
                        if t1154 {
                            var t1157 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1158 bool = t1157 == 43
                            if t1158 {
                                jp1148 = true
                            } else {
                                var t1159 uint8
                                var inline1990 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1159 = inline1990
                                var t1160 bool = t1159 == 45
                                jp1148 = t1160
                            }
                        } else {
                            jp1148 = false
                        }
                        if jp1148 {
                            var t1149 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1150 bool = t1149 == 45
                            exponent_negative__104 = t1150
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1151 int = compound_old187 + compound_value188
                            index__85 = t1151
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1109__3:
                        for {
                            var t1110 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1111 bool = index__85 < t1110
                            if t1111 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1145 bool = current__106 >= 48
                                var jp1114 bool
                                if t1145 {
                                    var t1146 bool = current__106 <= 57
                                    jp1114 = t1146
                                } else {
                                    jp1114 = false
                                }
                                if jp1114 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1118 bool = exponent__103 < 1000000
                                    if t1118 {
                                        var t1119 int = exponent__103 * 10
                                        var t1120 uint8 = current__106 - 48
                                        var t1121 int = int(uint8(t1120))
                                        var t1122 int = t1119 + t1121
                                        exponent__103 = t1122
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1116 int = compound_old196 + compound_value197
                                    index__85 = t1116
                                    continue
                                } else {
                                    var t1124 bool = current__106 == 95
                                    if t1124 {
                                        var t1141 bool = !previous_digit__96
                                        var jp1137 bool
                                        if t1141 {
                                            jp1137 = true
                                        } else {
                                            var t1142 int = index__85 + 1
                                            var t1143 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1144 bool = t1142 >= t1143
                                            jp1137 = t1144
                                        }
                                        var jp1132 bool
                                        if jp1137 {
                                            jp1132 = true
                                        } else {
                                            var t1138 int = index__85 + 1
                                            var t1139 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1138)
                                            var t1140 bool = t1139 < 48
                                            jp1132 = t1140
                                        }
                                        var jp1129 bool
                                        if jp1132 {
                                            jp1129 = true
                                        } else {
                                            var t1133 int = index__85 + 1
                                            var t1134 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1133)
                                            var t1135 bool = t1134 > 57
                                            jp1129 = t1135
                                        }
                                        if jp1129 {
                                            var t1130 ParsedFloat = invalid_parsed_float()
                                            return t1130
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1126 int = compound_old201 + compound_value202
                                            index__85 = t1126
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1109__3
                                    }
                                }
                            } else {
                                break Loop_loop1109__3
                            }
                        }
                        var t1107 bool = !exponent_digits__105
                        if t1107 {
                            var t1108 ParsedFloat = invalid_parsed_float()
                            return t1108
                        } else {
                            var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1098 bool = index__85 != t1097
                            if t1098 {
                                var t1099 ParsedFloat = invalid_parsed_float()
                                return t1099
                            } else {
                                if exponent_negative__104 {
                                    var t1096 int = 0 - exponent__103
                                    exponent__103 = t1096
                                } else {}
                                var jp1089 int
                                if jp1077 {
                                    jp1089 = 0
                                } else {
                                    var t1095 int = exponent__103 - fraction_digits__94
                                    jp1089 = t1095
                                }
                                var jp1091 int
                                if jp1077 {
                                    var t1093 int = fraction_digits__94 * 4
                                    var t1094 int = exponent__103 - t1093
                                    jp1091 = t1094
                                } else {
                                    jp1091 = 0
                                }
                                var t1092 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1089,
                                    binary_exponent: jp1091,
                                    hexadecimal: jp1077,
                                    significant_digits: significant_digits__95,
                                }
                                return t1092
                            }
                        }
                    } else {
                        if jp1077 {
                            var t1162 ParsedFloat = invalid_parsed_float()
                            return t1162
                        } else {
                            var t1097 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1098 bool = index__85 != t1097
                            if t1098 {
                                var t1099 ParsedFloat = invalid_parsed_float()
                                return t1099
                            } else {
                                if exponent_negative__104 {
                                    var t1096 int = 0 - exponent__103
                                    exponent__103 = t1096
                                } else {}
                                var jp1089 int
                                if jp1077 {
                                    jp1089 = 0
                                } else {
                                    var t1095 int = exponent__103 - fraction_digits__94
                                    jp1089 = t1095
                                }
                                var jp1091 int
                                if jp1077 {
                                    var t1093 int = fraction_digits__94 * 4
                                    var t1094 int = exponent__103 - t1093
                                    jp1091 = t1094
                                } else {
                                    jp1091 = 0
                                }
                                var t1092 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1089,
                                    binary_exponent: jp1091,
                                    hexadecimal: jp1077,
                                    significant_digits: significant_digits__95,
                                }
                                return t1092
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
    var inline1995 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline1995, 1)
    var inline1997 FloatNatural = FloatNatural{
        words: inline1995,
    }
    result__26 = inline1997
    var count__27 int = 0
    Loop_loop1264:
    for {
        var t1265 bool = count__27 < exponent__25
        if t1265 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1266 int = compound_old46 + compound_value47
            count__27 = t1266
            continue
        } else {
            break Loop_loop1264
        }
    }
    return result__26
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2003 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2004 FloatNatural = FloatNatural{
        words: inline2003,
    }
    result__5 = inline2004
    var index__6 int = 0
    Loop_loop1274:
    for {
        var t1275 *_goml_vec_uint32 = value__4.words
        var t1276 int
        var inline2001 int = vec_len__Vec_6uint32(t1275)
        t1276 = inline2001
        var t1277 bool = index__6 < t1276
        if t1277 {
            var t1278 *_goml_vec_uint32 = result__5.words
            var t1279 *_goml_vec_uint32 = value__4.words
            var t1280 uint32 = vec_get__Vec_6uint32(t1279, index__6)
            vec_push__Vec_6uint32(t1278, t1280)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1281 int = compound_old4 + compound_value5
            index__6 = t1281
            continue
        } else {
            break Loop_loop1274
        }
    }
    return result__5
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1304 bool = factor__16 == 0
    if t1304 {
        var t1305 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1305, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1298 uint64 = uint64(uint32(factor__16))
        Loop_loop1291:
        for {
            var t1292 *_goml_vec_uint32 = value__15.words
            var t1293 int
            var inline2008 int = vec_len__Vec_6uint32(t1292)
            t1293 = inline2008
            var t1294 bool = index__18 < t1293
            if t1294 {
                var t1295 *_goml_vec_uint32 = value__15.words
                var t1296 uint32 = vec_get__Vec_6uint32(t1295, index__18)
                var t1297 uint64 = uint64(uint32(t1296))
                var t1299 uint64 = t1297 * t1298
                var product__19 uint64 = t1299 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1301_rhs int = 32
                var t1301 uint64 = product__19 >> t1301_rhs
                carry__17 = t1301
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1302 int = compound_old30 + compound_value31
                index__18 = t1302
                continue
            } else {
                break Loop_loop1291
            }
        }
        var t1287 bool = carry__17 != 0
        if t1287 {
            var t1288 *_goml_vec_uint32 = value__15.words
            var t1289 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1288, t1289)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1388 bool
    var inline2012 *_goml_vec_uint32 = numerator__65.words
    var inline2013 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2012)
    t1388 = inline2013
    if t1388 {
        var t1389 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1389
    } else {
        var t1385 bool = binary_shift__67 >= 0
        var jp1310 FloatNatural
        if t1385 {
            var t1386 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1310 = t1386
        } else {
            var t1387 FloatNatural = float_natural_copy(numerator__65)
            jp1310 = t1387
        }
        var t1381 bool = binary_shift__67 >= 0
        var jp1312 FloatNatural
        if t1381 {
            var t1382 FloatNatural = float_natural_copy(denominator__66)
            jp1312 = t1382
        } else {
            var t1383 int = 0 - binary_shift__67
            var t1384 FloatNatural = float_natural_shift_left(denominator__66, t1383)
            jp1312 = t1384
        }
        var t1313 int = float_natural_bit_length(jp1310)
        var t1314 int = float_natural_bit_length(jp1312)
        var exponent__72 int = t1313 - t1314
        var t1375 bool = exponent__72 >= 0
        var jp1316 int
        if t1375 {
            var t1376 FloatNatural = float_natural_shift_left(jp1312, exponent__72)
            var t1377 int = float_natural_compare(jp1310, t1376)
            jp1316 = t1377
        } else {
            var t1378 int = 0 - exponent__72
            var t1379 FloatNatural = float_natural_shift_left(jp1310, t1378)
            var t1380 int = float_natural_compare(t1379, jp1312)
            jp1316 = t1380
        }
        var t1372 bool = jp1316 < 0
        if t1372 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1373 int = compound_old120 - compound_value121
            exponent__72 = t1373
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1366 bool = exponent__72 > exponent_bias__69
        if t1366 {
            var t1367 int = exponent_bias__69 + exponent_bias__69
            var t1368 int = t1367 + 1
            var t1369 uint64 = uint64(int(t1368))
            var t1370 uint64 = t1369 << mantissa_bits__68
            var t1371 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1370,
                _1: true,
            }
            return t1371
        } else {
            var t1361 bool = exponent__72 < minimum_exponent__74
            var jp1320 uint64
            if t1361 {
                var t1362 int = mantissa_bits__68 - minimum_exponent__74
                var t1363 uint64 = float_rational_quotient(jp1310, jp1312, t1362)
                jp1320 = t1363
            } else {
                var t1364 int = mantissa_bits__68 - exponent__72
                var t1365 uint64 = float_rational_quotient(jp1310, jp1312, t1364)
                jp1320 = t1365
            }
            var mantissa__76 uint64 = jp1320
            var t1323 bool = exponent__72 < minimum_exponent__74
            if t1323 {
                var t1326 bool = mantissa__76 == 0
                if t1326 {
                    var t1327 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1327
                } else {
                    var t1330_lhs uint64 = 1
                    var t1330 uint64 = t1330_lhs << mantissa_bits__68
                    var t1331 bool = mantissa__76 >= t1330
                    if t1331 {
                        var t1332_lhs uint64 = 1
                        var t1332 uint64 = t1332_lhs << mantissa_bits__68
                        var t1333_lhs uint64 = 1
                        var t1333 uint64 = t1333_lhs << mantissa_bits__68
                        var t1334 uint64 = mantissa__76 - t1333
                        var t1335 uint64 = t1332 | t1334
                        var t1336 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1335,
                            _1: false,
                        }
                        return t1336
                    } else {
                        var t1337 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1337
                    }
                }
            } else {
                var t1354 int = mantissa_bits__68 + 1
                var t1355_lhs uint64 = 1
                var t1355 uint64 = t1355_lhs << t1354
                var t1356 bool = mantissa__76 >= t1355
                if t1356 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1357 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1357
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1359 int = compound_old128 + compound_value129
                    exponent__72 = t1359
                } else {}
                var t1341 bool = exponent__72 > exponent_bias__69
                if t1341 {
                    var t1342 int = exponent_bias__69 + exponent_bias__69
                    var t1343 int = t1342 + 1
                    var t1344 uint64 = uint64(int(t1343))
                    var t1345 uint64 = t1344 << mantissa_bits__68
                    var t1346 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1345,
                        _1: true,
                    }
                    return t1346
                } else {
                    var t1347 int = exponent__72 + exponent_bias__69
                    var t1348 uint64 = uint64(int(t1347))
                    var t1349 uint64 = t1348 << mantissa_bits__68
                    var t1350_lhs uint64 = 1
                    var t1350 uint64 = t1350_lhs << mantissa_bits__68
                    var t1351 uint64 = mantissa__76 - t1350
                    var t1352 uint64 = t1349 | t1351
                    var t1353 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1352,
                        _1: false,
                    }
                    return t1353
                }
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline2019 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2020 FloatNatural = FloatNatural{
        words: inline2019,
    }
    result__2 = inline2020
    var t1393 bool = value__1 != 0
    if t1393 {
        var t1394 *_goml_vec_uint32 = result__2.words
        var t1395 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t1394, t1395)
        var t1396_rhs int = 32
        var t1396 uint64 = value__1 >> t1396_rhs
        var high__3 uint32 = uint32(uint64(t1396))
        var t1398 bool = high__3 != 0
        if t1398 {
            var t1399 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t1399, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1428 bool
    var inline2037 *_goml_vec_uint32 = value__28.words
    var inline2038 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2037)
    t1428 = inline2038
    if t1428 {
        var inline2022 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2023 FloatNatural = FloatNatural{
            words: inline2022,
        }
        return inline2023
    } else {
        var t1431 bool = bits__29 == 0
        if t1431 {
            var t1432 FloatNatural = float_natural_copy(value__28)
            return t1432
        } else {
            var result__30 FloatNatural
            var inline2034 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2035 FloatNatural = FloatNatural{
                words: inline2034,
            }
            result__30 = inline2035
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1423:
            for {
                var t1424 bool = index__33 < word_shift__31
                if t1424 {
                    var t1425 *_goml_vec_uint32 = result__30.words
                    var inline2025 uint32 = 0
                    vec_push__Vec_6uint32(t1425, inline2025)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1426 int = compound_old52 + compound_value53
                    index__33 = t1426
                    continue
                } else {
                    break Loop_loop1423
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop1411:
            for {
                var t1412 *_goml_vec_uint32 = value__28.words
                var t1413 int
                var inline2030 int = vec_len__Vec_6uint32(t1412)
                t1413 = inline2030
                var t1414 bool = index__33 < t1413
                if t1414 {
                    var t1415 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1415, index__33)
                    var t1416 uint64 = uint64(uint32(word__35))
                    var t1417 uint64 = t1416 << bit_shift__32
                    var shifted__36 uint64 = t1417 | carry__34
                    var t1418 *_goml_vec_uint32 = result__30.words
                    var t1419 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1418, t1419)
                    var t1420_rhs int = 32
                    var t1420 uint64 = shifted__36 >> t1420_rhs
                    carry__34 = t1420
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1421 int = compound_old59 + compound_value60
                    index__33 = t1421
                    continue
                } else {
                    break Loop_loop1411
                }
            }
            var t1407 bool = carry__34 != 0
            if t1407 {
                var t1408 *_goml_vec_uint32 = result__30.words
                var t1409 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t1408, t1409)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1455 bool
    var inline2053 *_goml_vec_uint32 = value__49.words
    var inline2054 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2053)
    t1455 = inline2054
    if t1455 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1448:
        for {
            var t1449 bool
            var inline2042 *_goml_vec_uint32 = current__50.words
            var inline2043 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2042)
            t1449 = inline2043
            var t1450 bool = !t1449
            if t1450 {
                var t1451 uint32 = float_natural_divide_small(current__50, 10)
                var t1452 uint8 = uint8(uint32(t1451))
                var t1453 uint8 = t1452 + 48
                vec_push__Vec_5uint8(reversed__51, t1453)
                continue
            } else {
                break Loop_loop1448
            }
        }
        var t1437 int
        var inline2051 int = vec_len__Vec_5uint8(reversed__51)
        t1437 = inline2051
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1437)
        var offset__53 int = 0
        Loop_loop1439:
        for {
            var t1440 int
            var inline2049 int = vec_len__Vec_5uint8(reversed__51)
            t1440 = inline2049
            var t1441 bool = offset__53 < t1440
            if t1441 {
                var t1442 int
                var inline2047 int = vec_len__Vec_5uint8(reversed__51)
                t1442 = inline2047
                var t1443 int = t1442 - offset__53
                var t1444 int = t1443 - 1
                var t1445 uint8 = vec_get__Vec_5uint8(reversed__51, t1444)
                vec_push__Vec_5uint8(output__52, t1445)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1446 int = compound_old98 + compound_value99
                offset__53 = t1446
                continue
            } else {
                break Loop_loop1439
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1458 int = _goml_runtime_core_string_len(self__289)
    return t1458
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1461 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1461)
    var index__148 int = 0
    Loop_loop1516:
    for {
        var t1517 bool = index__148 < count__146
        if t1517 {
            var t1518 uint8
            var inline2058 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1518 = inline2058
            vec_push__Vec_5uint8(output__147, t1518)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1519 int = compound_old267 + compound_value268
            index__148 = t1519
            continue
        } else {
            break Loop_loop1516
        }
    }
    var t1513 int
    var inline2079 int = _goml_runtime_core_string_len(exact__145)
    t1513 = inline2079
    var t1514 bool = count__146 == t1513
    if t1514 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1515 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1515
    } else {
        var next__150 uint8
        var inline2077 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2077
        var trailing__151 bool = false
        var t1464 int = count__146 + 1
        index__148 = t1464
        Loop_loop1505:
        for {
            var t1506 int
            var inline2062 int = _goml_runtime_core_string_len(exact__145)
            t1506 = inline2062
            var t1507 bool = index__148 < t1506
            if t1507 {
                var t1511 uint8
                var inline2060 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1511 = inline2060
                var t1512 bool = t1511 != 48
                if t1512 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1509 int = compound_old278 + compound_value279
                index__148 = t1509
                continue
            } else {
                break Loop_loop1505
            }
        }
        var t1493 bool = next__150 > 53
        var jp1467 bool
        if t1493 {
            jp1467 = true
        } else {
            var t1496 bool = next__150 == 53
            if t1496 {
                if trailing__151 {
                    jp1467 = true
                } else {
                    var t1499 int
                    var inline2064 int = vec_len__Vec_5uint8(output__147)
                    t1499 = inline2064
                    var t1500 int = t1499 - 1
                    var t1501 uint8 = vec_get__Vec_5uint8(output__147, t1500)
                    var t1502 uint8 = t1501 - 48
                    var t1503_rhs uint8 = 2
                    var t1503 uint8 = t1502 % t1503_rhs
                    var t1504 bool = t1503 == 1
                    jp1467 = t1504
                }
            } else {
                jp1467 = false
            }
        }
        if jp1467 {
            var index__153 int
            var inline2075 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2075
            Loop_loop1481:
            for {
                var t1482 bool = index__153 > 0
                if t1482 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1483 int = compound_old282 - compound_value283
                    index__153 = t1483
                    var t1486 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1487 bool = t1486 < 57
                    if t1487 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1488 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1488)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1490 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1490
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1481
                }
            }
            var t1471 int
            var inline2073 int = vec_len__Vec_5uint8(output__147)
            t1471 = inline2073
            var t1472 int = t1471 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1472)
            var inline2070 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2070)
            index__153 = 0
            Loop_loop1475:
            for {
                var t1476 int
                var inline2068 int = vec_len__Vec_5uint8(output__147)
                t1476 = inline2068
                var t1477 bool = index__153 < t1476
                if t1477 {
                    var t1478 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1478)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1479 int = compound_old302 + compound_value303
                    index__153 = t1479
                    continue
                } else {
                    break Loop_loop1475
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1474 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1474
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1492 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1492
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2086 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2086
    Loop_loop1525:
    for {
        var t1530 bool = length__159 > 1
        var jp1527 bool
        if t1530 {
            var t1531 int = length__159 - 1
            var t1532 uint8
            var inline2081 uint8 = _goml_runtime_core_string_byte_get(value__158, t1531)
            t1532 = inline2081
            var t1533 bool = t1532 == 48
            jp1527 = t1533
        } else {
            jp1527 = false
        }
        if jp1527 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1528 int = compound_old312 - compound_value313
            length__159 = t1528
            continue
        } else {
            break Loop_loop1525
        }
    }
    var inline2083 int = 0
    var inline2084 string = string_byte_slice(value__158, inline2083, length__159)
    return inline2084
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2088 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2088)
    } else {}
    var t1538 bool = decimal_point__138 <= 0
    if t1538 {
        var inline2103 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2103)
        var inline2100 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2100)
        var index__141 int = 0
        var t1548 int = 0 - decimal_point__138
        Loop_loop1547:
        for {
            var t1549 bool = index__141 < t1548
            if t1549 {
                var inline2091 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2091)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1550 int = compound_old234 + compound_value235
                index__141 = t1550
                continue
            } else {
                break Loop_loop1547
            }
        }
        index__141 = 0
        Loop_loop1541:
        for {
            var t1542 int
            var inline2098 int = _goml_runtime_core_string_len(digits__137)
            t1542 = inline2098
            var t1543 bool = index__141 < t1542
            if t1543 {
                var t1544 uint8
                var inline2096 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1544 = inline2096
                vec_push__Vec_5uint8(bytes__140, t1544)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1545 int = compound_old240 + compound_value241
                index__141 = t1545
                continue
            } else {
                break Loop_loop1541
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1553 int
        var inline2128 int = _goml_runtime_core_string_len(digits__137)
        t1553 = inline2128
        var t1554 bool = decimal_point__138 >= t1553
        if t1554 {
            var index__142 int = 0
            Loop_loop1561:
            for {
                var t1562 int
                var inline2110 int = _goml_runtime_core_string_len(digits__137)
                t1562 = inline2110
                var t1563 bool = index__142 < t1562
                if t1563 {
                    var t1564 uint8
                    var inline2108 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1564 = inline2108
                    vec_push__Vec_5uint8(bytes__140, t1564)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1565 int = compound_old244 + compound_value245
                    index__142 = t1565
                    continue
                } else {
                    break Loop_loop1561
                }
            }
            Loop_loop1557:
            for {
                var t1558 bool = index__142 < decimal_point__138
                if t1558 {
                    var inline2112 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2112)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1559 int = compound_old249 + compound_value250
                    index__142 = t1559
                    continue
                } else {
                    break Loop_loop1557
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1575:
            for {
                var t1576 bool = index__143 < decimal_point__138
                if t1576 {
                    var t1577 uint8
                    var inline2117 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1577 = inline2117
                    vec_push__Vec_5uint8(bytes__140, t1577)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1578 int = compound_old253 + compound_value254
                    index__143 = t1578
                    continue
                } else {
                    break Loop_loop1575
                }
            }
            var inline2125 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2125)
            Loop_loop1569:
            for {
                var t1570 int
                var inline2123 int = _goml_runtime_core_string_len(digits__137)
                t1570 = inline2123
                var t1571 bool = index__143 < t1570
                if t1571 {
                    var t1572 uint8
                    var inline2121 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1572 = inline2121
                    vec_push__Vec_5uint8(bytes__140, t1572)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1573 int = compound_old259 + compound_value260
                    index__143 = t1573
                    continue
                } else {
                    break Loop_loop1569
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        }
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1595 int
    var inline2145 int = _goml_runtime_core_string_len(value__78)
    t1595 = inline2145
    var t1596 int
    var inline2143 int = _goml_runtime_core_string_len(expected__79)
    t1596 = inline2143
    var t1597 bool = t1595 != t1596
    if t1597 {
        return false
    } else {
        var index__80 int = 0
        var inline2135 uint8 = 97 - 65
        Loop_loop1585:
        for {
            var t1586 int
            var inline2141 int = _goml_runtime_core_string_len(value__78)
            t1586 = inline2141
            var t1587 bool = index__80 < t1586
            if t1587 {
                var t1591 uint8
                var inline2139 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1591 = inline2139
                var t1592 uint8
                var inline2132 bool = t1591 >= 65
                var inline2134 bool
                if inline2132 {
                    var inline2137 bool = t1591 <= 90
                    inline2134 = inline2137
                } else {
                    inline2134 = false
                }
                if inline2134 {
                    var inline2136 uint8 = t1591 + inline2135
                    t1592 = inline2136
                    var t1593 uint8
                    var inline2130 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1593 = inline2130
                    var t1594 bool = t1592 != t1593
                    if t1594 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1589 int = compound_old134 + compound_value135
                        index__80 = t1589
                        continue
                    }
                } else {
                    t1592 = t1591
                    var t1593 uint8
                    var inline2130 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1593 = inline2130
                    var t1594 bool = t1592 != t1593
                    if t1594 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1589 int = compound_old134 + compound_value135
                        index__80 = t1589
                        continue
                    }
                }
            } else {
                break Loop_loop1585
            }
        }
        return true
    }
}

func float_natural_zero() FloatNatural {
    var t1600 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1601 FloatNatural = FloatNatural{
        words: t1600,
    }
    return t1601
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1604 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1604
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2147 bool = string_is_char_boundary(self__294, start__295)
    var inline2149 bool
    if inline2147 {
        var inline2152 bool = string_is_char_boundary(self__294, end__296)
        inline2149 = inline2152
    } else {
        inline2149 = false
    }
    if inline2149 {
        var inline2150 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2150
    } else {
        var inline2151 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2151
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1616 bool = value__77 >= 65
    var jp1613 bool
    if t1616 {
        var t1617 bool = value__77 <= 90
        jp1613 = t1617
    } else {
        jp1613 = false
    }
    if jp1613 {
        var t1614 uint8 = 97 - 65
        var t1615 uint8 = value__77 + t1614
        return t1615
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1644 bool = value__81 >= 48
    var jp1628 bool
    if t1644 {
        var t1645 bool = value__81 <= 57
        jp1628 = t1645
    } else {
        jp1628 = false
    }
    var jp1621 int
    if jp1628 {
        var t1629 uint8 = value__81 - 48
        var t1630 int = int(uint8(t1629))
        jp1621 = t1630
        var t1624 bool = jp1621 < base__82
        if t1624 {
            var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1621,
            }
            return t1625
        } else {
            var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1626
        }
    } else {
        var t1640 uint8
        var inline2168 bool = value__81 >= 65
        var inline2170 bool
        if inline2168 {
            var inline2173 bool = value__81 <= 90
            inline2170 = inline2173
        } else {
            inline2170 = false
        }
        if inline2170 {
            var inline2171 uint8 = 97 - 65
            var inline2172 uint8 = value__81 + inline2171
            t1640 = inline2172
            var t1641 bool = t1640 >= 97
            var jp1634 bool
            if t1641 {
                var t1642 uint8
                var inline2154 bool = value__81 >= 65
                var inline2156 bool
                if inline2154 {
                    var inline2159 bool = value__81 <= 90
                    inline2156 = inline2159
                } else {
                    inline2156 = false
                }
                if inline2156 {
                    var inline2157 uint8 = 97 - 65
                    var inline2158 uint8 = value__81 + inline2157
                    t1642 = inline2158
                    var t1643 bool = t1642 <= 102
                    jp1634 = t1643
                    if jp1634 {
                        var t1635 uint8
                        var inline2161 bool = value__81 >= 65
                        var inline2163 bool
                        if inline2161 {
                            var inline2166 bool = value__81 <= 90
                            inline2163 = inline2166
                        } else {
                            inline2163 = false
                        }
                        if inline2163 {
                            var inline2164 uint8 = 97 - 65
                            var inline2165 uint8 = value__81 + inline2164
                            t1635 = inline2165
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        } else {
                            t1635 = value__81
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        }
                    } else {
                        var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1639
                    }
                } else {
                    t1642 = value__81
                    var t1643 bool = t1642 <= 102
                    jp1634 = t1643
                    if jp1634 {
                        var t1635 uint8
                        var inline2161 bool = value__81 >= 65
                        var inline2163 bool
                        if inline2161 {
                            var inline2166 bool = value__81 <= 90
                            inline2163 = inline2166
                        } else {
                            inline2163 = false
                        }
                        if inline2163 {
                            var inline2164 uint8 = 97 - 65
                            var inline2165 uint8 = value__81 + inline2164
                            t1635 = inline2165
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        } else {
                            t1635 = value__81
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        }
                    } else {
                        var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1639
                    }
                }
            } else {
                jp1634 = false
                if jp1634 {
                    var t1635 uint8
                    var inline2161 bool = value__81 >= 65
                    var inline2163 bool
                    if inline2161 {
                        var inline2166 bool = value__81 <= 90
                        inline2163 = inline2166
                    } else {
                        inline2163 = false
                    }
                    if inline2163 {
                        var inline2164 uint8 = 97 - 65
                        var inline2165 uint8 = value__81 + inline2164
                        t1635 = inline2165
                        var t1636 uint8 = t1635 - 97
                        var t1637 uint8 = t1636 + 10
                        var t1638 int = int(uint8(t1637))
                        jp1621 = t1638
                        var t1624 bool = jp1621 < base__82
                        if t1624 {
                            var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1621,
                            }
                            return t1625
                        } else {
                            var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1626
                        }
                    } else {
                        t1635 = value__81
                        var t1636 uint8 = t1635 - 97
                        var t1637 uint8 = t1636 + 10
                        var t1638 int = int(uint8(t1637))
                        jp1621 = t1638
                        var t1624 bool = jp1621 < base__82
                        if t1624 {
                            var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1621,
                            }
                            return t1625
                        } else {
                            var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1626
                        }
                    }
                } else {
                    var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1639
                }
            }
        } else {
            t1640 = value__81
            var t1641 bool = t1640 >= 97
            var jp1634 bool
            if t1641 {
                var t1642 uint8
                var inline2154 bool = value__81 >= 65
                var inline2156 bool
                if inline2154 {
                    var inline2159 bool = value__81 <= 90
                    inline2156 = inline2159
                } else {
                    inline2156 = false
                }
                if inline2156 {
                    var inline2157 uint8 = 97 - 65
                    var inline2158 uint8 = value__81 + inline2157
                    t1642 = inline2158
                    var t1643 bool = t1642 <= 102
                    jp1634 = t1643
                    if jp1634 {
                        var t1635 uint8
                        var inline2161 bool = value__81 >= 65
                        var inline2163 bool
                        if inline2161 {
                            var inline2166 bool = value__81 <= 90
                            inline2163 = inline2166
                        } else {
                            inline2163 = false
                        }
                        if inline2163 {
                            var inline2164 uint8 = 97 - 65
                            var inline2165 uint8 = value__81 + inline2164
                            t1635 = inline2165
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        } else {
                            t1635 = value__81
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        }
                    } else {
                        var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1639
                    }
                } else {
                    t1642 = value__81
                    var t1643 bool = t1642 <= 102
                    jp1634 = t1643
                    if jp1634 {
                        var t1635 uint8
                        var inline2161 bool = value__81 >= 65
                        var inline2163 bool
                        if inline2161 {
                            var inline2166 bool = value__81 <= 90
                            inline2163 = inline2166
                        } else {
                            inline2163 = false
                        }
                        if inline2163 {
                            var inline2164 uint8 = 97 - 65
                            var inline2165 uint8 = value__81 + inline2164
                            t1635 = inline2165
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        } else {
                            t1635 = value__81
                            var t1636 uint8 = t1635 - 97
                            var t1637 uint8 = t1636 + 10
                            var t1638 int = int(uint8(t1637))
                            jp1621 = t1638
                            var t1624 bool = jp1621 < base__82
                            if t1624 {
                                var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1621,
                                }
                                return t1625
                            } else {
                                var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1626
                            }
                        }
                    } else {
                        var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1639
                    }
                }
            } else {
                jp1634 = false
                if jp1634 {
                    var t1635 uint8
                    var inline2161 bool = value__81 >= 65
                    var inline2163 bool
                    if inline2161 {
                        var inline2166 bool = value__81 <= 90
                        inline2163 = inline2166
                    } else {
                        inline2163 = false
                    }
                    if inline2163 {
                        var inline2164 uint8 = 97 - 65
                        var inline2165 uint8 = value__81 + inline2164
                        t1635 = inline2165
                        var t1636 uint8 = t1635 - 97
                        var t1637 uint8 = t1636 + 10
                        var t1638 int = int(uint8(t1637))
                        jp1621 = t1638
                        var t1624 bool = jp1621 < base__82
                        if t1624 {
                            var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1621,
                            }
                            return t1625
                        } else {
                            var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1626
                        }
                    } else {
                        t1635 = value__81
                        var t1636 uint8 = t1635 - 97
                        var t1637 uint8 = t1636 + 10
                        var t1638 int = int(uint8(t1637))
                        jp1621 = t1638
                        var t1624 bool = jp1621 < base__82
                        if t1624 {
                            var t1625 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1621,
                            }
                            return t1625
                        } else {
                            var t1626 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1626
                        }
                    }
                } else {
                    var t1639 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1639
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1648:
    for {
        var t1649 bool = carry__22 != 0
        if t1649 {
            var t1658 *_goml_vec_uint32 = value__20.words
            var t1659 int
            var inline2178 int = vec_len__Vec_6uint32(t1658)
            t1659 = inline2178
            var t1660 bool = index__23 == t1659
            if t1660 {
                var t1661 *_goml_vec_uint32 = value__20.words
                var inline2175 uint32 = 0
                vec_push__Vec_6uint32(t1661, inline2175)
            } else {}
            var t1651 *_goml_vec_uint32 = value__20.words
            var t1652 uint32 = vec_get__Vec_6uint32(t1651, index__23)
            var t1653 uint64 = uint64(uint32(t1652))
            var sum__24 uint64 = t1653 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1655_rhs int = 32
            var t1655 uint64 = sum__24 >> t1655_rhs
            carry__22 = t1655
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1656 int = compound_old42 + compound_value43
            index__23 = t1656
            continue
        } else {
            break Loop_loop1648
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1665 FloatNatural
    var inline2180 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2181 FloatNatural = FloatNatural{
        words: inline2180,
    }
    t1665 = inline2181
    var t1666 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1665,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1666
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1669 int = vec_len__Vec_6uint32(self__528)
    var t1670 bool = t1669 == 0
    return t1670
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1697 *_goml_vec_uint32 = value__9.words
    var t1698 bool
    var inline2190 int = vec_len__Vec_6uint32(t1697)
    var inline2191 bool = inline2190 == 0
    t1698 = inline2191
    if t1698 {
        return 0
    } else {
        var t1681 *_goml_vec_uint32 = value__9.words
        var t1682 *_goml_vec_uint32 = value__9.words
        var t1683 int
        var inline2188 int = vec_len__Vec_6uint32(t1682)
        t1683 = inline2188
        var t1684 int = t1683 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1681, t1684)
        var bits__11 int = 0
        Loop_loop1691:
        for {
            var t1692 bool = high__10 != 0
            if t1692 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1693 uint32 = compound_old9 >> compound_value10
                high__10 = t1693
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1695 int = compound_old12 + compound_value13
                bits__11 = t1695
                continue
            } else {
                break Loop_loop1691
            }
        }
        var t1686 *_goml_vec_uint32 = value__9.words
        var t1687 int
        var inline2186 int = vec_len__Vec_6uint32(t1686)
        t1687 = inline2186
        var t1688 int = t1687 - 1
        var t1689 int = t1688 * 32
        var t1690 int = t1689 + bits__11
        return t1690
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1720 *_goml_vec_uint32 = left__12.words
    var t1721 int
    var inline2201 int = vec_len__Vec_6uint32(t1720)
    t1721 = inline2201
    var t1722 *_goml_vec_uint32 = right__13.words
    var t1723 int
    var inline2199 int = vec_len__Vec_6uint32(t1722)
    t1723 = inline2199
    var t1724 bool = t1721 < t1723
    if t1724 {
        return -1
    } else {
        var t1726 *_goml_vec_uint32 = left__12.words
        var t1727 int
        var inline2195 int = vec_len__Vec_6uint32(t1726)
        t1727 = inline2195
        var t1728 *_goml_vec_uint32 = right__13.words
        var t1729 int
        var inline2193 int = vec_len__Vec_6uint32(t1728)
        t1729 = inline2193
        var t1730 bool = t1727 > t1729
        if t1730 {
            return 1
        } else {
            var t1702 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2197 int = vec_len__Vec_6uint32(t1702)
            index__14 = inline2197
            Loop_loop1704:
            for {
                var t1705 bool = index__14 > 0
                if t1705 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1706 int = compound_old17 - compound_value18
                    index__14 = t1706
                    var t1709 *_goml_vec_uint32 = left__12.words
                    var t1710 uint32 = vec_get__Vec_6uint32(t1709, index__14)
                    var t1711 *_goml_vec_uint32 = right__13.words
                    var t1712 uint32 = vec_get__Vec_6uint32(t1711, index__14)
                    var t1713 bool = t1710 < t1712
                    if t1713 {
                        return -1
                    } else {
                        var t1715 *_goml_vec_uint32 = left__12.words
                        var t1716 uint32 = vec_get__Vec_6uint32(t1715, index__14)
                        var t1717 *_goml_vec_uint32 = right__13.words
                        var t1718 uint32 = vec_get__Vec_6uint32(t1717, index__14)
                        var t1719 bool = t1716 > t1718
                        if t1719 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1704
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1766 bool = shift__57 >= 0
    var jp1734 FloatNatural
    if t1766 {
        var t1767 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1734 = t1767
    } else {
        var t1768 FloatNatural = float_natural_copy(numerator__55)
        jp1734 = t1768
    }
    var t1762 bool = shift__57 >= 0
    var jp1736 FloatNatural
    if t1762 {
        var t1763 FloatNatural = float_natural_copy(denominator__56)
        jp1736 = t1763
    } else {
        var t1764 int = 0 - shift__57
        var t1765 FloatNatural = float_natural_shift_left(denominator__56, t1764)
        jp1736 = t1765
    }
    var quotient__60 uint64 = 0
    Loop_loop1749:
    for {
        var t1750 int = float_natural_compare(jp1734, jp1736)
        var t1751 bool = t1750 >= 0
        if t1751 {
            var t1752 int = float_natural_bit_length(jp1734)
            var t1753 int = float_natural_bit_length(jp1736)
            var offset__61 int = t1752 - t1753
            var part__62 FloatNatural = float_natural_shift_left(jp1736, offset__61)
            var t1757 int = float_natural_compare(jp1734, part__62)
            var t1758 bool = t1757 < 0
            if t1758 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1759 int = compound_old105 - compound_value106
                offset__61 = t1759
                var t1761 FloatNatural = float_natural_shift_left(jp1736, offset__61)
                part__62 = t1761
            } else {}
            float_natural_subtract(jp1734, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1755 uint64 = compound_old111 | compound_value112
            quotient__60 = t1755
            continue
        } else {
            break Loop_loop1749
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1734, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1736)
    var t1743 bool = rounding__64 > 0
    var jp1740 bool
    if t1743 {
        jp1740 = true
    } else {
        var t1746 bool = rounding__64 == 0
        if t1746 {
            var t1747_rhs uint64 = 1
            var t1747 uint64 = quotient__60 & t1747_rhs
            var t1748 bool = t1747 == 1
            jp1740 = t1748
        } else {
            jp1740 = false
        }
    }
    if jp1740 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1741 uint64 = compound_old115 + compound_value116
        quotient__60 = t1741
    } else {}
    return quotient__60
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1771 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2203 int = vec_len__Vec_6uint32(t1771)
    index__47 = inline2203
    var t1782 uint64 = uint64(uint32(divisor__45))
    var t1785 uint64 = uint64(uint32(divisor__45))
    Loop_loop1774:
    for {
        var t1775 bool = index__47 > 0
        if t1775 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1776 int = compound_old83 - compound_value84
            index__47 = t1776
            var t1778_rhs int = 32
            var t1778 uint64 = remainder__46 << t1778_rhs
            var t1779 *_goml_vec_uint32 = value__44.words
            var t1780 uint32 = vec_get__Vec_6uint32(t1779, index__47)
            var t1781 uint64 = uint64(uint32(t1780))
            var current__48 uint64 = t1778 | t1781
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1783 uint64 = current__48 / t1782
            var value90 uint32 = uint32(uint64(t1783))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1786 uint64 = current__48 % t1785
            remainder__46 = t1786
            continue
        } else {
            break Loop_loop1774
        }
    }
    float_natural_trim(value__44)
    var t1773 uint32 = uint32(uint64(remainder__46))
    return t1773
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1800 bool = string_is_char_boundary(value__274, start__275)
    var jp1797 bool
    if t1800 {
        var t1801 bool = string_is_char_boundary(value__274, end__276)
        jp1797 = t1801
    } else {
        jp1797 = false
    }
    if jp1797 {
        var t1798 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1798
    } else {
        var t1799 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1799
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
    Loop_loop1807:
    for {
        var t1808 *_goml_vec_uint32 = value__37.words
        var t1809 int
        var inline2207 int = vec_len__Vec_6uint32(t1808)
        t1809 = inline2207
        var t1810 bool = index__41 < t1809
        if t1810 {
            var t1824 *_goml_vec_uint32 = other__38.words
            var t1825 int
            var inline2205 int = vec_len__Vec_6uint32(t1824)
            t1825 = inline2205
            var t1826 bool = index__41 < t1825
            var jp1812 uint64
            if t1826 {
                var t1827 *_goml_vec_uint32 = other__38.words
                var t1828 uint32 = vec_get__Vec_6uint32(t1827, index__41)
                var t1829 uint64 = uint64(uint32(t1828))
                jp1812 = t1829
            } else {
                jp1812 = 0
            }
            var right__42 uint64 = jp1812 + borrow__40
            var t1813 *_goml_vec_uint32 = value__37.words
            var t1814 uint32 = vec_get__Vec_6uint32(t1813, index__41)
            var left__43 uint64 = uint64(uint32(t1814))
            var t1818 bool = left__43 >= right__42
            if t1818 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1819 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1819))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1821 uint64 = base__39 + left__43
                var t1822 uint64 = t1821 - right__42
                var value75 uint32 = uint32(uint64(t1822))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1816 int = compound_old79 + compound_value80
            index__41 = t1816
            continue
        } else {
            break Loop_loop1807
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1832:
    for {
        var t1840 *_goml_vec_uint32 = value__7.words
        var t1841 bool
        var inline2215 int = vec_len__Vec_6uint32(t1840)
        var inline2216 bool = inline2215 == 0
        t1841 = inline2216
        var t1842 bool = !t1841
        var jp1834 bool
        if t1842 {
            var t1843 *_goml_vec_uint32 = value__7.words
            var t1844 *_goml_vec_uint32 = value__7.words
            var t1845 int
            var inline2209 int = vec_len__Vec_6uint32(t1844)
            t1845 = inline2209
            var t1846 int = t1845 - 1
            var t1847 uint32 = vec_get__Vec_6uint32(t1843, t1846)
            var t1848 bool = t1847 == 0
            jp1834 = t1848
        } else {
            jp1834 = false
        }
        if jp1834 {
            var t1835 *_goml_vec_uint32 = value__7.words
            var t1836 *_goml_vec_uint32 = value__7.words
            var t1837 int
            var inline2213 int = vec_len__Vec_6uint32(t1836)
            t1837 = inline2213
            var t1838 int = t1837 - 1
            vec_truncate__Vec_6uint32(t1835, t1838)
            continue
        } else {
            break Loop_loop1832
        }
    }
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1862 bool = index__269 < 0
    var jp1854 bool
    if t1862 {
        jp1854 = true
    } else {
        var t1863 int
        var inline2218 int = _goml_runtime_core_string_len(value__268)
        t1863 = inline2218
        var t1864 bool = index__269 > t1863
        jp1854 = t1864
    }
    if jp1854 {
        return false
    } else {
        var t1857 int
        var inline2222 int = _goml_runtime_core_string_len(value__268)
        t1857 = inline2222
        var t1858 bool = index__269 == t1857
        if t1858 {
            return true
        } else {
            var t1859 uint8
            var inline2220 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1859 = inline2220
            var t1860_rhs uint8 = 192
            var t1860 uint8 = t1859 & t1860_rhs
            var t1861 bool = t1860 != 128
            return t1861
        }
    }
}

func main() {
    main0()
}
