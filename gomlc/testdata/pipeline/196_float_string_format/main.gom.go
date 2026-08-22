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

func main0() struct{} {
    var t803 string
    var inline1835 float64 = 18318654708.7
    var inline1836 string = __goml_builtin_float64_to_string(inline1835)
    t803 = inline1836
    var inline1832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
    _goml_runtime_core_string_println(inline1832)
    var t804 string
    var inline1829 float64 = 0.0000001
    var inline1830 string = __goml_builtin_float64_to_string(inline1829)
    t804 = inline1830
    var inline1826 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t804)
    _goml_runtime_core_string_println(inline1826)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t805 float64 = negative_one__1 * zero__0
    var t806 string
    var inline1824 string = __goml_builtin_float64_to_string(t805)
    t806 = inline1824
    var inline1821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline1821)
    var t807 float64 = 1 / zero__0
    var t808 string
    var inline1819 string = __goml_builtin_float64_to_string(t807)
    t808 = inline1819
    var inline1816 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline1816)
    var t809 float64 = -1
    var t810 float64 = t809 / zero__0
    var t811 string
    var inline1814 string = __goml_builtin_float64_to_string(t810)
    t811 = inline1814
    var inline1811 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline1811)
    var t812 float64 = zero__0 / zero__0
    var t813 string
    var inline1809 string = __goml_builtin_float64_to_string(t812)
    t813 = inline1809
    var inline1806 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline1806)
    var wide__2 float64 = 12345678
    var t814 string
    var inline1804 string = __goml_builtin_float64_to_string(wide__2)
    t814 = inline1804
    var inline1801 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline1801)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_float64_to_string(value__195 float64) string {
    var t826 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(value__195)
    var t827 string = format_float_bits(t826, 52, 11, 1023)
    return t827
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t830 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t830
    var t831 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t831 != 0
    var t832_lhs uint64 = 1
    var t832 uint64 = t832_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t832 - 1
    var t833 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t833 & exponent_mask__166
    var t834_lhs uint64 = 1
    var t834 uint64 = t834_lhs << mantissa_bits__161
    var t835 uint64 = t834 - 1
    var fraction__168 uint64 = bits__160 & t835
    var t899 bool = exponent__167 == exponent_mask__166
    if t899 {
        var t901 bool = fraction__168 == 0
        if t901 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t907 bool = exponent__167 == 0
        var jp905 bool
        if t907 {
            var t908 bool = fraction__168 == 0
            jp905 = t908
        } else {
            jp905 = false
        }
        if jp905 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t896 bool = exponent__167 == 0
            var jp838 uint64
            if t896 {
                jp838 = fraction__168
            } else {
                var t897_lhs uint64 = 1
                var t897 uint64 = t897_lhs << mantissa_bits__161
                var t898 uint64 = fraction__168 | t897
                jp838 = t898
            }
            var t890 bool = exponent__167 == 0
            var jp840 int
            if t890 {
                var t891 int = 1 - exponent_bias__163
                var t892 int = t891 - mantissa_bits__161
                jp840 = t892
            } else {
                var t893 int = int(uint64(exponent__167))
                var t894 int = t893 - exponent_bias__163
                var t895 int = t894 - mantissa_bits__161
                jp840 = t895
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp838)
            var t845 bool = jp840 >= 0
            var jp842 int
            if t845 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp840)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t864 bool = mantissa_bits__161 == 23
                var jp847 int
                if t864 {
                    jp847 = 9
                } else {
                    jp847 = 17
                }
                var t861 int
                var inline1848 int = _goml_runtime_core_string_len(digits__173)
                t861 = inline1848
                var t862 bool = t861 < jp847
                var jp849 int
                if t862 {
                    var inline1842 int = _goml_runtime_core_string_len(digits__173)
                    jp849 = inline1842
                } else {
                    jp849 = jp847
                }
                var count__176 int = 1
                Loop_loop852:
                for {
                    var t853 bool = count__176 <= jp849
                    if t853 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t854 int
                        var inline1844 int = _goml_runtime_core_string_len(digits__173)
                        t854 = inline1844
                        var jp856 int
                        if x319 {
                            jp856 = 1
                        } else {
                            jp856 = 0
                        }
                        var point__180 int = t854 + jp856
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t860 bool = x322 == bits__160
                        if t860 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t858 int = compound_old324 + compound_value325
                            count__176 = t858
                            continue
                        }
                    } else {
                        break Loop_loop852
                    }
                }
                var inline1846 int = _goml_runtime_core_string_len(digits__173)
                jp842 = inline1846
                var t843 string = float_natural_decimal(exact_value__171)
                var t844 string = fixed_float_text(t843, jp842, negative__165)
                return t844
            } else {
                var count__183 int = 0
                var t886 int = 0 - jp840
                Loop_loop885:
                for {
                    var t887 bool = count__183 < t886
                    if t887 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t888 int = compound_old329 + compound_value330
                        count__183 = t888
                        continue
                    } else {
                        break Loop_loop885
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t866 int
                var inline1854 int = _goml_runtime_core_string_len(digits__184)
                t866 = inline1854
                var point__185 int = t866 + jp840
                var t884 bool = mantissa_bits__161 == 23
                var jp868 int
                if t884 {
                    jp868 = 9
                } else {
                    jp868 = 17
                }
                var t881 int
                var inline1852 int = _goml_runtime_core_string_len(digits__184)
                t881 = inline1852
                var t882 bool = t881 < jp868
                var jp870 int
                if t882 {
                    var inline1850 int = _goml_runtime_core_string_len(digits__184)
                    jp870 = inline1850
                } else {
                    jp870 = jp868
                }
                count__183 = 1
                Loop_loop872:
                for {
                    var t873 bool = count__183 <= jp870
                    if t873 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp875 int
                        if x336 {
                            jp875 = 1
                        } else {
                            jp875 = 0
                        }
                        var t876 int = point__185 + jp875
                        var candidate__191 string = fixed_float_text(rounded__190, t876, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t880 bool = x339 == bits__160
                        if t880 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t878 int = compound_old341 + compound_value342
                            count__183 = t878
                            continue
                        }
                    } else {
                        break Loop_loop872
                    }
                }
                jp842 = point__185
                var t843 string = float_natural_decimal(exact_value__171)
                var t844 string = fixed_float_text(t843, jp842, negative__165)
                return t844
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1860 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1861 FloatNatural = FloatNatural{
        words: inline1860,
    }
    result__2 = inline1861
    var t912 bool = value__1 != 0
    if t912 {
        var t913 *_goml_vec_uint32 = result__2.words
        var t914 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t913, t914)
        var t915_rhs int = 32
        var t915 uint64 = value__1 >> t915_rhs
        var high__3 uint32 = uint32(uint64(t915))
        var t917 bool = high__3 != 0
        if t917 {
            var t918 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t918, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t947 bool
    var inline1878 *_goml_vec_uint32 = value__28.words
    var inline1879 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1878)
    t947 = inline1879
    if t947 {
        var inline1863 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1864 FloatNatural = FloatNatural{
            words: inline1863,
        }
        return inline1864
    } else {
        var t950 bool = bits__29 == 0
        if t950 {
            var t951 FloatNatural = float_natural_copy(value__28)
            return t951
        } else {
            var result__30 FloatNatural
            var inline1875 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1876 FloatNatural = FloatNatural{
                words: inline1875,
            }
            result__30 = inline1876
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop942:
            for {
                var t943 bool = index__33 < word_shift__31
                if t943 {
                    var t944 *_goml_vec_uint32 = result__30.words
                    var inline1866 uint32 = 0
                    vec_push__Vec_6uint32(t944, inline1866)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t945 int = compound_old52 + compound_value53
                    index__33 = t945
                    continue
                } else {
                    break Loop_loop942
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop930:
            for {
                var t931 *_goml_vec_uint32 = value__28.words
                var t932 int
                var inline1871 int = vec_len__Vec_6uint32(t931)
                t932 = inline1871
                var t933 bool = index__33 < t932
                if t933 {
                    var t934 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t934, index__33)
                    var t935 uint64 = uint64(uint32(word__35))
                    var t936 uint64 = t935 << bit_shift__32
                    var shifted__36 uint64 = t936 | carry__34
                    var t937 *_goml_vec_uint32 = result__30.words
                    var t938 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t937, t938)
                    var t939_rhs int = 32
                    var t939 uint64 = shifted__36 >> t939_rhs
                    carry__34 = t939
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t940 int = compound_old59 + compound_value60
                    index__33 = t940
                    continue
                } else {
                    break Loop_loop930
                }
            }
            var t926 bool = carry__34 != 0
            if t926 {
                var t927 *_goml_vec_uint32 = result__30.words
                var t928 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t927, t928)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t974 bool
    var inline1894 *_goml_vec_uint32 = value__49.words
    var inline1895 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1894)
    t974 = inline1895
    if t974 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop967:
        for {
            var t968 bool
            var inline1883 *_goml_vec_uint32 = current__50.words
            var inline1884 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1883)
            t968 = inline1884
            var t969 bool = !t968
            if t969 {
                var t970 uint32 = float_natural_divide_small(current__50, 10)
                var t971 uint8 = uint8(uint32(t970))
                var t972 uint8 = t971 + 48
                vec_push__Vec_5uint8(reversed__51, t972)
                continue
            } else {
                break Loop_loop967
            }
        }
        var t956 int
        var inline1892 int = vec_len__Vec_5uint8(reversed__51)
        t956 = inline1892
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t956)
        var offset__53 int = 0
        Loop_loop958:
        for {
            var t959 int
            var inline1890 int = vec_len__Vec_5uint8(reversed__51)
            t959 = inline1890
            var t960 bool = offset__53 < t959
            if t960 {
                var t961 int
                var inline1888 int = vec_len__Vec_5uint8(reversed__51)
                t961 = inline1888
                var t962 int = t961 - offset__53
                var t963 int = t962 - 1
                var t964 uint8 = vec_get__Vec_5uint8(reversed__51, t963)
                vec_push__Vec_5uint8(output__52, t964)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t965 int = compound_old98 + compound_value99
                offset__53 = t965
                continue
            } else {
                break Loop_loop958
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t977 int = _goml_runtime_core_string_len(self__289)
    return t977
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t980 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t980)
    var index__148 int = 0
    Loop_loop1035:
    for {
        var t1036 bool = index__148 < count__146
        if t1036 {
            var t1037 uint8
            var inline1899 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1037 = inline1899
            vec_push__Vec_5uint8(output__147, t1037)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1038 int = compound_old267 + compound_value268
            index__148 = t1038
            continue
        } else {
            break Loop_loop1035
        }
    }
    var t1032 int
    var inline1920 int = _goml_runtime_core_string_len(exact__145)
    t1032 = inline1920
    var t1033 bool = count__146 == t1032
    if t1033 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1034 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1034
    } else {
        var next__150 uint8
        var inline1918 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1918
        var trailing__151 bool = false
        var t983 int = count__146 + 1
        index__148 = t983
        Loop_loop1024:
        for {
            var t1025 int
            var inline1903 int = _goml_runtime_core_string_len(exact__145)
            t1025 = inline1903
            var t1026 bool = index__148 < t1025
            if t1026 {
                var t1030 uint8
                var inline1901 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1030 = inline1901
                var t1031 bool = t1030 != 48
                if t1031 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1028 int = compound_old278 + compound_value279
                index__148 = t1028
                continue
            } else {
                break Loop_loop1024
            }
        }
        var t1012 bool = next__150 > 53
        var jp986 bool
        if t1012 {
            jp986 = true
        } else {
            var t1015 bool = next__150 == 53
            if t1015 {
                if trailing__151 {
                    jp986 = true
                } else {
                    var t1018 int
                    var inline1905 int = vec_len__Vec_5uint8(output__147)
                    t1018 = inline1905
                    var t1019 int = t1018 - 1
                    var t1020 uint8 = vec_get__Vec_5uint8(output__147, t1019)
                    var t1021 uint8 = t1020 - 48
                    var t1022_rhs uint8 = 2
                    var t1022 uint8 = t1021 % t1022_rhs
                    var t1023 bool = t1022 == 1
                    jp986 = t1023
                }
            } else {
                jp986 = false
            }
        }
        if jp986 {
            var index__153 int
            var inline1916 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1916
            Loop_loop1000:
            for {
                var t1001 bool = index__153 > 0
                if t1001 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1002 int = compound_old282 - compound_value283
                    index__153 = t1002
                    var t1005 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1006 bool = t1005 < 57
                    if t1006 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1007 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1007)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1009 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1009
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1000
                }
            }
            var t990 int
            var inline1914 int = vec_len__Vec_5uint8(output__147)
            t990 = inline1914
            var t991 int = t990 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t991)
            var inline1911 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1911)
            index__153 = 0
            Loop_loop994:
            for {
                var t995 int
                var inline1909 int = vec_len__Vec_5uint8(output__147)
                t995 = inline1909
                var t996 bool = index__153 < t995
                if t996 {
                    var t997 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t997)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t998 int = compound_old302 + compound_value303
                    index__153 = t998
                    continue
                } else {
                    break Loop_loop994
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t993 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t993
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1011 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1011
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline1927 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline1927
    Loop_loop1044:
    for {
        var t1049 bool = length__159 > 1
        var jp1046 bool
        if t1049 {
            var t1050 int = length__159 - 1
            var t1051 uint8
            var inline1922 uint8 = _goml_runtime_core_string_byte_get(value__158, t1050)
            t1051 = inline1922
            var t1052 bool = t1051 == 48
            jp1046 = t1052
        } else {
            jp1046 = false
        }
        if jp1046 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1047 int = compound_old312 - compound_value313
            length__159 = t1047
            continue
        } else {
            break Loop_loop1044
        }
    }
    var inline1924 int = 0
    var inline1925 string = string_byte_slice(value__158, inline1924, length__159)
    return inline1925
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline1929 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline1929)
    } else {}
    var t1057 bool = decimal_point__138 <= 0
    if t1057 {
        var inline1944 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline1944)
        var inline1941 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline1941)
        var index__141 int = 0
        var t1067 int = 0 - decimal_point__138
        Loop_loop1066:
        for {
            var t1068 bool = index__141 < t1067
            if t1068 {
                var inline1932 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline1932)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1069 int = compound_old234 + compound_value235
                index__141 = t1069
                continue
            } else {
                break Loop_loop1066
            }
        }
        index__141 = 0
        Loop_loop1060:
        for {
            var t1061 int
            var inline1939 int = _goml_runtime_core_string_len(digits__137)
            t1061 = inline1939
            var t1062 bool = index__141 < t1061
            if t1062 {
                var t1063 uint8
                var inline1937 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1063 = inline1937
                vec_push__Vec_5uint8(bytes__140, t1063)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1064 int = compound_old240 + compound_value241
                index__141 = t1064
                continue
            } else {
                break Loop_loop1060
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1072 int
        var inline1969 int = _goml_runtime_core_string_len(digits__137)
        t1072 = inline1969
        var t1073 bool = decimal_point__138 >= t1072
        if t1073 {
            var index__142 int = 0
            Loop_loop1080:
            for {
                var t1081 int
                var inline1951 int = _goml_runtime_core_string_len(digits__137)
                t1081 = inline1951
                var t1082 bool = index__142 < t1081
                if t1082 {
                    var t1083 uint8
                    var inline1949 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1083 = inline1949
                    vec_push__Vec_5uint8(bytes__140, t1083)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1084 int = compound_old244 + compound_value245
                    index__142 = t1084
                    continue
                } else {
                    break Loop_loop1080
                }
            }
            Loop_loop1076:
            for {
                var t1077 bool = index__142 < decimal_point__138
                if t1077 {
                    var inline1953 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline1953)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1078 int = compound_old249 + compound_value250
                    index__142 = t1078
                    continue
                } else {
                    break Loop_loop1076
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1094:
            for {
                var t1095 bool = index__143 < decimal_point__138
                if t1095 {
                    var t1096 uint8
                    var inline1958 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1096 = inline1958
                    vec_push__Vec_5uint8(bytes__140, t1096)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1097 int = compound_old253 + compound_value254
                    index__143 = t1097
                    continue
                } else {
                    break Loop_loop1094
                }
            }
            var inline1966 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline1966)
            Loop_loop1088:
            for {
                var t1089 int
                var inline1964 int = _goml_runtime_core_string_len(digits__137)
                t1089 = inline1964
                var t1090 bool = index__143 < t1089
                if t1090 {
                    var t1091 uint8
                    var inline1962 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1091 = inline1962
                    vec_push__Vec_5uint8(bytes__140, t1091)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1092 int = compound_old259 + compound_value260
                    index__143 = t1092
                    continue
                } else {
                    break Loop_loop1088
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
    var t1193 bool = parsed__110.valid
    var t1194 bool = !t1193
    if t1194 {
        var t1195 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1195
    } else {
        var t1187 bool = parsed__110.negative
        var jp1104 uint64
        if t1187 {
            var t1192 bool = mantissa_bits__108 == 23
            var jp1189 int
            if t1192 {
                jp1189 = 8
            } else {
                jp1189 = 11
            }
            var t1190 int = mantissa_bits__108 + jp1189
            var t1191_lhs uint64 = 1
            var t1191 uint64 = t1191_lhs << t1190
            jp1104 = t1191
        } else {
            jp1104 = 0
        }
        var t1186 bool = mantissa_bits__108 == 23
        var jp1106 int
        if t1186 {
            jp1106 = 8
        } else {
            jp1106 = 11
        }
        var t1107_lhs uint64 = 1
        var t1107 uint64 = t1107_lhs << jp1106
        var t1108 uint64 = t1107 - 1
        var exponent_mask__112 uint64 = t1108 << mantissa_bits__108
        var t1164 int = parsed__110.special
        var t1165 bool = t1164 == 1
        if t1165 {
            var t1166 uint64 = jp1104 | exponent_mask__112
            var t1167 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1166,
            }
            return t1167
        } else {
            var t1169 int = parsed__110.special
            var t1170 bool = t1169 == 2
            if t1170 {
                var t1174 int = mantissa_bits__108 - 1
                var t1175_lhs uint64 = 1
                var t1175 uint64 = t1175_lhs << t1174
                var t1176 uint64 = exponent_mask__112 | t1175
                var t1181 bool = mantissa_bits__108 == 52
                var jp1178 uint64
                if t1181 {
                    jp1178 = 1
                } else {
                    jp1178 = 0
                }
                var t1179 uint64 = t1176 | jp1178
                var t1180 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1179,
                }
                return t1180
            } else {
                var t1183 FloatNatural = parsed__110.numerator
                var t1184 bool
                var inline1971 *_goml_vec_uint32 = t1183.words
                var inline1972 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1971)
                t1184 = inline1972
                if t1184 {
                    var t1185 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1104,
                    }
                    return t1185
                } else {
                    var t1147 bool = parsed__110.hexadecimal
                    var t1148 bool = !t1147
                    if t1148 {
                        var t1149 int = parsed__110.significant_digits
                        var t1150 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1149 + t1150
                        var t1163 bool = mantissa_bits__108 == 23
                        var jp1152 int
                        if t1163 {
                            jp1152 = 40
                        } else {
                            jp1152 = 310
                        }
                        var t1162 bool = mantissa_bits__108 == 23
                        var jp1154 int
                        if t1162 {
                            jp1154 = -46
                        } else {
                            jp1154 = -325
                        }
                        var t1156 bool = decimal_position__113 > jp1152
                        if t1156 {
                            var t1157 uint64 = jp1104 | exponent_mask__112
                            var t1158 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1157,
                            }
                            return t1158
                        } else {
                            var t1160 bool = decimal_position__113 < jp1154
                            if t1160 {
                                var t1161 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1104,
                                }
                                return t1161
                            } else {
                                var t1143 bool = parsed__110.hexadecimal
                                var t1144 bool = !t1143
                                var jp1138 bool
                                if t1144 {
                                    var t1145 int = parsed__110.decimal_exponent
                                    var t1146 bool = t1145 < 0
                                    jp1138 = t1146
                                } else {
                                    jp1138 = false
                                }
                                var jp1112 FloatNatural
                                if jp1138 {
                                    var t1139 int = parsed__110.decimal_exponent
                                    var t1140 int = 0 - t1139
                                    var t1141 FloatNatural = float_natural_power5(t1140)
                                    jp1112 = t1141
                                } else {
                                    var inline1974 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline1974, 1)
                                    var inline1976 FloatNatural = FloatNatural{
                                        words: inline1974,
                                    }
                                    jp1112 = inline1976
                                }
                                var t1133 bool = parsed__110.hexadecimal
                                var t1134 bool = !t1133
                                var jp1124 bool
                                if t1134 {
                                    var t1135 int = parsed__110.decimal_exponent
                                    var t1136 bool = t1135 > 0
                                    jp1124 = t1136
                                } else {
                                    jp1124 = false
                                }
                                var jp1114 FloatNatural
                                if jp1124 {
                                    var t1125 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1125)
                                    var count__118 int = 0
                                    Loop_loop1127:
                                    for {
                                        var t1128 int = parsed__110.decimal_exponent
                                        var t1129 bool = count__118 < t1128
                                        if t1129 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1130 int = compound_old213 + compound_value214
                                            count__118 = t1130
                                            continue
                                        } else {
                                            break Loop_loop1127
                                        }
                                    }
                                    jp1114 = result__117
                                    var t1120 bool = parsed__110.hexadecimal
                                    var jp1116 int
                                    if t1120 {
                                        var t1121 int = parsed__110.binary_exponent
                                        jp1116 = t1121
                                    } else {
                                        var t1122 int = parsed__110.decimal_exponent
                                        jp1116 = t1122
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1114, jp1112, jp1116, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1117 bool = !x219
                                    var t1118 uint64 = jp1104 | x218
                                    var t1119 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1117,
                                        _1: t1118,
                                    }
                                    return t1119
                                } else {
                                    var t1132 FloatNatural = parsed__110.numerator
                                    jp1114 = t1132
                                    var t1120 bool = parsed__110.hexadecimal
                                    var jp1116 int
                                    if t1120 {
                                        var t1121 int = parsed__110.binary_exponent
                                        jp1116 = t1121
                                    } else {
                                        var t1122 int = parsed__110.decimal_exponent
                                        jp1116 = t1122
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1114, jp1112, jp1116, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1117 bool = !x219
                                    var t1118 uint64 = jp1104 | x218
                                    var t1119 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1117,
                                        _1: t1118,
                                    }
                                    return t1119
                                }
                            }
                        }
                    } else {
                        var t1143 bool = parsed__110.hexadecimal
                        var t1144 bool = !t1143
                        var jp1138 bool
                        if t1144 {
                            var t1145 int = parsed__110.decimal_exponent
                            var t1146 bool = t1145 < 0
                            jp1138 = t1146
                        } else {
                            jp1138 = false
                        }
                        var jp1112 FloatNatural
                        if jp1138 {
                            var t1139 int = parsed__110.decimal_exponent
                            var t1140 int = 0 - t1139
                            var t1141 FloatNatural = float_natural_power5(t1140)
                            jp1112 = t1141
                        } else {
                            var inline1974 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline1974, 1)
                            var inline1976 FloatNatural = FloatNatural{
                                words: inline1974,
                            }
                            jp1112 = inline1976
                        }
                        var t1133 bool = parsed__110.hexadecimal
                        var t1134 bool = !t1133
                        var jp1124 bool
                        if t1134 {
                            var t1135 int = parsed__110.decimal_exponent
                            var t1136 bool = t1135 > 0
                            jp1124 = t1136
                        } else {
                            jp1124 = false
                        }
                        var jp1114 FloatNatural
                        if jp1124 {
                            var t1125 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1125)
                            var count__118 int = 0
                            Loop_loop1127__2:
                            for {
                                var t1128 int = parsed__110.decimal_exponent
                                var t1129 bool = count__118 < t1128
                                if t1129 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1130 int = compound_old213 + compound_value214
                                    count__118 = t1130
                                    continue
                                } else {
                                    break Loop_loop1127__2
                                }
                            }
                            jp1114 = result__117
                            var t1120 bool = parsed__110.hexadecimal
                            var jp1116 int
                            if t1120 {
                                var t1121 int = parsed__110.binary_exponent
                                jp1116 = t1121
                            } else {
                                var t1122 int = parsed__110.decimal_exponent
                                jp1116 = t1122
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1114, jp1112, jp1116, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1117 bool = !x219
                            var t1118 uint64 = jp1104 | x218
                            var t1119 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1117,
                                _1: t1118,
                            }
                            return t1119
                        } else {
                            var t1132 FloatNatural = parsed__110.numerator
                            jp1114 = t1132
                            var t1120 bool = parsed__110.hexadecimal
                            var jp1116 int
                            if t1120 {
                                var t1121 int = parsed__110.binary_exponent
                                jp1116 = t1121
                            } else {
                                var t1122 int = parsed__110.decimal_exponent
                                jp1116 = t1122
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1114, jp1112, jp1116, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1117 bool = !x219
                            var t1118 uint64 = jp1104 | x218
                            var t1119 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1117,
                                _1: t1118,
                            }
                            return t1119
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1217 bool = factor__16 == 0
    if t1217 {
        var t1218 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1218, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1211 uint64 = uint64(uint32(factor__16))
        Loop_loop1204:
        for {
            var t1205 *_goml_vec_uint32 = value__15.words
            var t1206 int
            var inline1980 int = vec_len__Vec_6uint32(t1205)
            t1206 = inline1980
            var t1207 bool = index__18 < t1206
            if t1207 {
                var t1208 *_goml_vec_uint32 = value__15.words
                var t1209 uint32 = vec_get__Vec_6uint32(t1208, index__18)
                var t1210 uint64 = uint64(uint32(t1209))
                var t1212 uint64 = t1210 * t1211
                var product__19 uint64 = t1212 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1214_rhs int = 32
                var t1214 uint64 = product__19 >> t1214_rhs
                carry__17 = t1214
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1215 int = compound_old30 + compound_value31
                index__18 = t1215
                continue
            } else {
                break Loop_loop1204
            }
        }
        var t1200 bool = carry__17 != 0
        if t1200 {
            var t1201 *_goml_vec_uint32 = value__15.words
            var t1202 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1201, t1202)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1221 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1222 FloatNatural = FloatNatural{
        words: t1221,
    }
    return t1222
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline1991 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1992 FloatNatural = FloatNatural{
        words: inline1991,
    }
    result__5 = inline1992
    var index__6 int = 0
    Loop_loop1232:
    for {
        var t1233 *_goml_vec_uint32 = value__4.words
        var t1234 int
        var inline1989 int = vec_len__Vec_6uint32(t1233)
        t1234 = inline1989
        var t1235 bool = index__6 < t1234
        if t1235 {
            var t1236 *_goml_vec_uint32 = result__5.words
            var t1237 *_goml_vec_uint32 = value__4.words
            var t1238 uint32 = vec_get__Vec_6uint32(t1237, index__6)
            vec_push__Vec_6uint32(t1236, t1238)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1239 int = compound_old4 + compound_value5
            index__6 = t1239
            continue
        } else {
            break Loop_loop1232
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1246 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline1994 int = vec_len__Vec_6uint32(t1246)
    index__47 = inline1994
    var t1257 uint64 = uint64(uint32(divisor__45))
    var t1260 uint64 = uint64(uint32(divisor__45))
    Loop_loop1249:
    for {
        var t1250 bool = index__47 > 0
        if t1250 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1251 int = compound_old83 - compound_value84
            index__47 = t1251
            var t1253_rhs int = 32
            var t1253 uint64 = remainder__46 << t1253_rhs
            var t1254 *_goml_vec_uint32 = value__44.words
            var t1255 uint32 = vec_get__Vec_6uint32(t1254, index__47)
            var t1256 uint64 = uint64(uint32(t1255))
            var current__48 uint64 = t1253 | t1256
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1258 uint64 = current__48 / t1257
            var value90 uint32 = uint32(uint64(t1258))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1261 uint64 = current__48 % t1260
            remainder__46 = t1261
            continue
        } else {
            break Loop_loop1249
        }
    }
    float_natural_trim(value__44)
    var t1248 uint32 = uint32(uint64(remainder__46))
    return t1248
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1269 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1269
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline1996 bool = string_is_char_boundary(self__294, start__295)
    var inline1998 bool
    if inline1996 {
        var inline2001 bool = string_is_char_boundary(self__294, end__296)
        inline1998 = inline2001
    } else {
        inline1998 = false
    }
    if inline1998 {
        var inline1999 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline1999
    } else {
        var inline2000 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2000
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1457 bool = string_equals_ascii_case(value__84, "nan")
    if t1457 {
        var t1458 FloatNatural
        var inline2003 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2004 FloatNatural = FloatNatural{
            words: inline2003,
        }
        t1458 = inline2004
        var t1459 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1458,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1459
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1449 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1450 bool = index__85 < t1449
        var jp1444 bool
        if t1450 {
            var t1453 uint8
            var inline2008 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1453 = inline2008
            var t1454 bool = t1453 == 43
            if t1454 {
                jp1444 = true
            } else {
                var t1455 uint8
                var inline2006 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1455 = inline2006
                var t1456 bool = t1455 == 45
                jp1444 = t1456
            }
        } else {
            jp1444 = false
        }
        if jp1444 {
            var t1445 uint8
            var inline2010 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1445 = inline2010
            var t1446 bool = t1445 == 45
            negative__86 = t1446
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1447 int = compound_old140 + compound_value141
            index__85 = t1447
        } else {}
        var t1277 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1277)
        var t1441 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1438 bool
        if t1441 {
            jp1438 = true
        } else {
            var t1442 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1438 = t1442
        }
        if jp1438 {
            var t1439 FloatNatural
            var inline2012 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2013 FloatNatural = FloatNatural{
                words: inline2012,
            }
            t1439 = inline2013
            var t1440 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1439,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1440
        } else {
            var t1432 int = index__85 + 2
            var t1433 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1434 bool = t1432 <= t1433
            var jp1427 bool
            if t1434 {
                var t1435 uint8
                var inline2015 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1435 = inline2015
                var t1436 bool = t1435 == 48
                jp1427 = t1436
            } else {
                jp1427 = false
            }
            var jp1280 bool
            if jp1427 {
                var t1428 int = index__85 + 1
                var t1429 uint8
                var inline2024 uint8 = _goml_runtime_core_string_byte_get(value__84, t1428)
                t1429 = inline2024
                var t1430 uint8
                var inline2017 bool = t1429 >= 65
                var inline2019 bool
                if inline2017 {
                    var inline2022 bool = t1429 <= 90
                    inline2019 = inline2022
                } else {
                    inline2019 = false
                }
                if inline2019 {
                    var inline2020 uint8 = 97 - 65
                    var inline2021 uint8 = t1429 + inline2020
                    t1430 = inline2021
                    var t1431 bool = t1430 == 120
                    jp1280 = t1431
                    if jp1280 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1424 int = compound_old145 + compound_value146
                        index__85 = t1424
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1283 int
                    if jp1280 {
                        jp1283 = 16
                    } else {
                        jp1283 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1377 uint32 = uint32(int(jp1283))
                    Loop_loop1373:
                    for {
                        var t1374 int
                        var inline2038 int = _goml_runtime_core_string_len(value__84)
                        t1374 = inline2038
                        var t1375 bool = index__85 < t1374
                        if t1375 {
                            var current__97 uint8
                            var inline2036 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2036
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1283)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1377)
                                var t1378 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1378)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1389 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1389
                                } else {}
                                var t1387 bool = significant_digits__95 > 0
                                var jp1384 bool
                                if t1387 {
                                    jp1384 = true
                                } else {
                                    var t1388 bool = x151 != 0
                                    jp1384 = t1388
                                }
                                if jp1384 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1385 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1385
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1381 int = compound_old164 + compound_value165
                                index__85 = t1381
                                continue
                            } else {
                                var t1392 bool = current__97 == 95
                                if t1392 {
                                    var t1413 int = index__85 + 1
                                    var t1414 int
                                    var inline2034 int = _goml_runtime_core_string_len(value__84)
                                    t1414 = inline2034
                                    var t1415 bool = t1413 >= t1414
                                    if t1415 {
                                        var inline2026 FloatNatural = float_natural_zero()
                                        var inline2027 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2026,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2027
                                    } else {
                                        var t1394 int = index__85 + 1
                                        var t1395 uint8
                                        var inline2032 uint8 = _goml_runtime_core_string_byte_get(value__84, t1394)
                                        t1395 = inline2032
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1395, jp1283)
                                        var x169 bool = mtmp168._0
                                        var jp1410 bool
                                        if jp1280 {
                                            var t1412 bool = !saw_digit__92
                                            jp1410 = t1412
                                        } else {
                                            jp1410 = false
                                        }
                                        var jp1397 bool
                                        if jp1410 {
                                            var t1411 bool = index__85 == mantissa_start__89
                                            jp1397 = t1411
                                        } else {
                                            jp1397 = false
                                        }
                                        var t1407 bool = !previous_digit__96
                                        var jp1405 bool
                                        if t1407 {
                                            var t1408 bool = !jp1397
                                            jp1405 = t1408
                                        } else {
                                            jp1405 = false
                                        }
                                        var jp1402 bool
                                        if jp1405 {
                                            jp1402 = true
                                        } else {
                                            var t1406 bool = !x169
                                            jp1402 = t1406
                                        }
                                        if jp1402 {
                                            var inline2029 FloatNatural = float_natural_zero()
                                            var inline2030 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2029,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2030
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1399 int = compound_old173 + compound_value174
                                            index__85 = t1399
                                            continue
                                        }
                                    }
                                } else {
                                    var t1422 bool = current__97 == 46
                                    var jp1419 bool
                                    if t1422 {
                                        var t1423 bool = !saw_dot__93
                                        jp1419 = t1423
                                    } else {
                                        jp1419 = false
                                    }
                                    if jp1419 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1420 int = compound_old178 + compound_value179
                                        index__85 = t1420
                                        continue
                                    } else {
                                        break Loop_loop1373
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1373
                        }
                    }
                    var t1371 bool = !saw_digit__92
                    if t1371 {
                        var inline2040 FloatNatural = float_natural_zero()
                        var inline2041 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2040,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2041
                    } else {
                        var jp1287 uint8
                        if jp1280 {
                            jp1287 = 112
                        } else {
                            jp1287 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1366 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1367 bool = index__85 < t1366
                        var jp1304 bool
                        if t1367 {
                            var t1368 uint8
                            var inline2043 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1368 = inline2043
                            var t1369 uint8 = ascii_lower(t1368)
                            var t1370 bool = t1369 == jp1287
                            jp1304 = t1370
                        } else {
                            jp1304 = false
                        }
                        if jp1304 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1305 int = compound_old183 + compound_value184
                            index__85 = t1305
                            var t1356 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1357 bool = index__85 < t1356
                            var jp1351 bool
                            if t1357 {
                                var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1361 bool = t1360 == 43
                                if t1361 {
                                    jp1351 = true
                                } else {
                                    var t1362 uint8
                                    var inline2045 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1362 = inline2045
                                    var t1363 bool = t1362 == 45
                                    jp1351 = t1363
                                }
                            } else {
                                jp1351 = false
                            }
                            if jp1351 {
                                var t1352 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1353 bool = t1352 == 45
                                exponent_negative__104 = t1353
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1354 int = compound_old187 + compound_value188
                                index__85 = t1354
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1312:
                            for {
                                var t1313 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1314 bool = index__85 < t1313
                                if t1314 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1348 bool = current__106 >= 48
                                    var jp1317 bool
                                    if t1348 {
                                        var t1349 bool = current__106 <= 57
                                        jp1317 = t1349
                                    } else {
                                        jp1317 = false
                                    }
                                    if jp1317 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1321 bool = exponent__103 < 1000000
                                        if t1321 {
                                            var t1322 int = exponent__103 * 10
                                            var t1323 uint8 = current__106 - 48
                                            var t1324 int = int(uint8(t1323))
                                            var t1325 int = t1322 + t1324
                                            exponent__103 = t1325
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1319 int = compound_old196 + compound_value197
                                        index__85 = t1319
                                        continue
                                    } else {
                                        var t1327 bool = current__106 == 95
                                        if t1327 {
                                            var t1344 bool = !previous_digit__96
                                            var jp1340 bool
                                            if t1344 {
                                                jp1340 = true
                                            } else {
                                                var t1345 int = index__85 + 1
                                                var t1346 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1347 bool = t1345 >= t1346
                                                jp1340 = t1347
                                            }
                                            var jp1335 bool
                                            if jp1340 {
                                                jp1335 = true
                                            } else {
                                                var t1341 int = index__85 + 1
                                                var t1342 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1341)
                                                var t1343 bool = t1342 < 48
                                                jp1335 = t1343
                                            }
                                            var jp1332 bool
                                            if jp1335 {
                                                jp1332 = true
                                            } else {
                                                var t1336 int = index__85 + 1
                                                var t1337 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1336)
                                                var t1338 bool = t1337 > 57
                                                jp1332 = t1338
                                            }
                                            if jp1332 {
                                                var t1333 ParsedFloat = invalid_parsed_float()
                                                return t1333
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1329 int = compound_old201 + compound_value202
                                                index__85 = t1329
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1312
                                        }
                                    }
                                } else {
                                    break Loop_loop1312
                                }
                            }
                            var t1310 bool = !exponent_digits__105
                            if t1310 {
                                var t1311 ParsedFloat = invalid_parsed_float()
                                return t1311
                            } else {
                                var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1301 bool = index__85 != t1300
                                if t1301 {
                                    var t1302 ParsedFloat = invalid_parsed_float()
                                    return t1302
                                } else {
                                    if exponent_negative__104 {
                                        var t1299 int = 0 - exponent__103
                                        exponent__103 = t1299
                                    } else {}
                                    var jp1292 int
                                    if jp1280 {
                                        jp1292 = 0
                                    } else {
                                        var t1298 int = exponent__103 - fraction_digits__94
                                        jp1292 = t1298
                                    }
                                    var jp1294 int
                                    if jp1280 {
                                        var t1296 int = fraction_digits__94 * 4
                                        var t1297 int = exponent__103 - t1296
                                        jp1294 = t1297
                                    } else {
                                        jp1294 = 0
                                    }
                                    var t1295 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1292,
                                        binary_exponent: jp1294,
                                        hexadecimal: jp1280,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1295
                                }
                            }
                        } else {
                            if jp1280 {
                                var t1365 ParsedFloat = invalid_parsed_float()
                                return t1365
                            } else {
                                var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1301 bool = index__85 != t1300
                                if t1301 {
                                    var t1302 ParsedFloat = invalid_parsed_float()
                                    return t1302
                                } else {
                                    if exponent_negative__104 {
                                        var t1299 int = 0 - exponent__103
                                        exponent__103 = t1299
                                    } else {}
                                    var jp1292 int
                                    if jp1280 {
                                        jp1292 = 0
                                    } else {
                                        var t1298 int = exponent__103 - fraction_digits__94
                                        jp1292 = t1298
                                    }
                                    var jp1294 int
                                    if jp1280 {
                                        var t1296 int = fraction_digits__94 * 4
                                        var t1297 int = exponent__103 - t1296
                                        jp1294 = t1297
                                    } else {
                                        jp1294 = 0
                                    }
                                    var t1295 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1292,
                                        binary_exponent: jp1294,
                                        hexadecimal: jp1280,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1295
                                }
                            }
                        }
                    }
                } else {
                    t1430 = t1429
                    var t1431 bool = t1430 == 120
                    jp1280 = t1431
                    if jp1280 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1424 int = compound_old145 + compound_value146
                        index__85 = t1424
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1283 int
                    if jp1280 {
                        jp1283 = 16
                    } else {
                        jp1283 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1377 uint32 = uint32(int(jp1283))
                    Loop_loop1373__2:
                    for {
                        var t1374 int
                        var inline2038 int = _goml_runtime_core_string_len(value__84)
                        t1374 = inline2038
                        var t1375 bool = index__85 < t1374
                        if t1375 {
                            var current__97 uint8
                            var inline2036 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2036
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1283)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1377)
                                var t1378 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1378)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1389 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1389
                                } else {}
                                var t1387 bool = significant_digits__95 > 0
                                var jp1384 bool
                                if t1387 {
                                    jp1384 = true
                                } else {
                                    var t1388 bool = x151 != 0
                                    jp1384 = t1388
                                }
                                if jp1384 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1385 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1385
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1381 int = compound_old164 + compound_value165
                                index__85 = t1381
                                continue
                            } else {
                                var t1392 bool = current__97 == 95
                                if t1392 {
                                    var t1413 int = index__85 + 1
                                    var t1414 int
                                    var inline2034 int = _goml_runtime_core_string_len(value__84)
                                    t1414 = inline2034
                                    var t1415 bool = t1413 >= t1414
                                    if t1415 {
                                        var inline2026 FloatNatural = float_natural_zero()
                                        var inline2027 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2026,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2027
                                    } else {
                                        var t1394 int = index__85 + 1
                                        var t1395 uint8
                                        var inline2032 uint8 = _goml_runtime_core_string_byte_get(value__84, t1394)
                                        t1395 = inline2032
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1395, jp1283)
                                        var x169 bool = mtmp168._0
                                        var jp1410 bool
                                        if jp1280 {
                                            var t1412 bool = !saw_digit__92
                                            jp1410 = t1412
                                        } else {
                                            jp1410 = false
                                        }
                                        var jp1397 bool
                                        if jp1410 {
                                            var t1411 bool = index__85 == mantissa_start__89
                                            jp1397 = t1411
                                        } else {
                                            jp1397 = false
                                        }
                                        var t1407 bool = !previous_digit__96
                                        var jp1405 bool
                                        if t1407 {
                                            var t1408 bool = !jp1397
                                            jp1405 = t1408
                                        } else {
                                            jp1405 = false
                                        }
                                        var jp1402 bool
                                        if jp1405 {
                                            jp1402 = true
                                        } else {
                                            var t1406 bool = !x169
                                            jp1402 = t1406
                                        }
                                        if jp1402 {
                                            var inline2029 FloatNatural = float_natural_zero()
                                            var inline2030 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2029,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2030
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1399 int = compound_old173 + compound_value174
                                            index__85 = t1399
                                            continue
                                        }
                                    }
                                } else {
                                    var t1422 bool = current__97 == 46
                                    var jp1419 bool
                                    if t1422 {
                                        var t1423 bool = !saw_dot__93
                                        jp1419 = t1423
                                    } else {
                                        jp1419 = false
                                    }
                                    if jp1419 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1420 int = compound_old178 + compound_value179
                                        index__85 = t1420
                                        continue
                                    } else {
                                        break Loop_loop1373__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1373__2
                        }
                    }
                    var t1371 bool = !saw_digit__92
                    if t1371 {
                        var inline2040 FloatNatural = float_natural_zero()
                        var inline2041 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2040,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2041
                    } else {
                        var jp1287 uint8
                        if jp1280 {
                            jp1287 = 112
                        } else {
                            jp1287 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1366 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1367 bool = index__85 < t1366
                        var jp1304 bool
                        if t1367 {
                            var t1368 uint8
                            var inline2043 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1368 = inline2043
                            var t1369 uint8 = ascii_lower(t1368)
                            var t1370 bool = t1369 == jp1287
                            jp1304 = t1370
                        } else {
                            jp1304 = false
                        }
                        if jp1304 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1305 int = compound_old183 + compound_value184
                            index__85 = t1305
                            var t1356 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1357 bool = index__85 < t1356
                            var jp1351 bool
                            if t1357 {
                                var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1361 bool = t1360 == 43
                                if t1361 {
                                    jp1351 = true
                                } else {
                                    var t1362 uint8
                                    var inline2045 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1362 = inline2045
                                    var t1363 bool = t1362 == 45
                                    jp1351 = t1363
                                }
                            } else {
                                jp1351 = false
                            }
                            if jp1351 {
                                var t1352 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1353 bool = t1352 == 45
                                exponent_negative__104 = t1353
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1354 int = compound_old187 + compound_value188
                                index__85 = t1354
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1312__2:
                            for {
                                var t1313 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1314 bool = index__85 < t1313
                                if t1314 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1348 bool = current__106 >= 48
                                    var jp1317 bool
                                    if t1348 {
                                        var t1349 bool = current__106 <= 57
                                        jp1317 = t1349
                                    } else {
                                        jp1317 = false
                                    }
                                    if jp1317 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1321 bool = exponent__103 < 1000000
                                        if t1321 {
                                            var t1322 int = exponent__103 * 10
                                            var t1323 uint8 = current__106 - 48
                                            var t1324 int = int(uint8(t1323))
                                            var t1325 int = t1322 + t1324
                                            exponent__103 = t1325
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1319 int = compound_old196 + compound_value197
                                        index__85 = t1319
                                        continue
                                    } else {
                                        var t1327 bool = current__106 == 95
                                        if t1327 {
                                            var t1344 bool = !previous_digit__96
                                            var jp1340 bool
                                            if t1344 {
                                                jp1340 = true
                                            } else {
                                                var t1345 int = index__85 + 1
                                                var t1346 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1347 bool = t1345 >= t1346
                                                jp1340 = t1347
                                            }
                                            var jp1335 bool
                                            if jp1340 {
                                                jp1335 = true
                                            } else {
                                                var t1341 int = index__85 + 1
                                                var t1342 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1341)
                                                var t1343 bool = t1342 < 48
                                                jp1335 = t1343
                                            }
                                            var jp1332 bool
                                            if jp1335 {
                                                jp1332 = true
                                            } else {
                                                var t1336 int = index__85 + 1
                                                var t1337 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1336)
                                                var t1338 bool = t1337 > 57
                                                jp1332 = t1338
                                            }
                                            if jp1332 {
                                                var t1333 ParsedFloat = invalid_parsed_float()
                                                return t1333
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1329 int = compound_old201 + compound_value202
                                                index__85 = t1329
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1312__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1312__2
                                }
                            }
                            var t1310 bool = !exponent_digits__105
                            if t1310 {
                                var t1311 ParsedFloat = invalid_parsed_float()
                                return t1311
                            } else {
                                var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1301 bool = index__85 != t1300
                                if t1301 {
                                    var t1302 ParsedFloat = invalid_parsed_float()
                                    return t1302
                                } else {
                                    if exponent_negative__104 {
                                        var t1299 int = 0 - exponent__103
                                        exponent__103 = t1299
                                    } else {}
                                    var jp1292 int
                                    if jp1280 {
                                        jp1292 = 0
                                    } else {
                                        var t1298 int = exponent__103 - fraction_digits__94
                                        jp1292 = t1298
                                    }
                                    var jp1294 int
                                    if jp1280 {
                                        var t1296 int = fraction_digits__94 * 4
                                        var t1297 int = exponent__103 - t1296
                                        jp1294 = t1297
                                    } else {
                                        jp1294 = 0
                                    }
                                    var t1295 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1292,
                                        binary_exponent: jp1294,
                                        hexadecimal: jp1280,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1295
                                }
                            }
                        } else {
                            if jp1280 {
                                var t1365 ParsedFloat = invalid_parsed_float()
                                return t1365
                            } else {
                                var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1301 bool = index__85 != t1300
                                if t1301 {
                                    var t1302 ParsedFloat = invalid_parsed_float()
                                    return t1302
                                } else {
                                    if exponent_negative__104 {
                                        var t1299 int = 0 - exponent__103
                                        exponent__103 = t1299
                                    } else {}
                                    var jp1292 int
                                    if jp1280 {
                                        jp1292 = 0
                                    } else {
                                        var t1298 int = exponent__103 - fraction_digits__94
                                        jp1292 = t1298
                                    }
                                    var jp1294 int
                                    if jp1280 {
                                        var t1296 int = fraction_digits__94 * 4
                                        var t1297 int = exponent__103 - t1296
                                        jp1294 = t1297
                                    } else {
                                        jp1294 = 0
                                    }
                                    var t1295 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1292,
                                        binary_exponent: jp1294,
                                        hexadecimal: jp1280,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1295
                                }
                            }
                        }
                    }
                }
            } else {
                jp1280 = false
                if jp1280 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1424 int = compound_old145 + compound_value146
                    index__85 = t1424
                } else {}
                var mantissa_start__89 int = index__85
                var jp1283 int
                if jp1280 {
                    jp1283 = 16
                } else {
                    jp1283 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1377 uint32 = uint32(int(jp1283))
                Loop_loop1373__3:
                for {
                    var t1374 int
                    var inline2038 int = _goml_runtime_core_string_len(value__84)
                    t1374 = inline2038
                    var t1375 bool = index__85 < t1374
                    if t1375 {
                        var current__97 uint8
                        var inline2036 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2036
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1283)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1377)
                            var t1378 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1378)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1389 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1389
                            } else {}
                            var t1387 bool = significant_digits__95 > 0
                            var jp1384 bool
                            if t1387 {
                                jp1384 = true
                            } else {
                                var t1388 bool = x151 != 0
                                jp1384 = t1388
                            }
                            if jp1384 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1385 int = compound_old160 + compound_value161
                                significant_digits__95 = t1385
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1381 int = compound_old164 + compound_value165
                            index__85 = t1381
                            continue
                        } else {
                            var t1392 bool = current__97 == 95
                            if t1392 {
                                var t1413 int = index__85 + 1
                                var t1414 int
                                var inline2034 int = _goml_runtime_core_string_len(value__84)
                                t1414 = inline2034
                                var t1415 bool = t1413 >= t1414
                                if t1415 {
                                    var inline2026 FloatNatural = float_natural_zero()
                                    var inline2027 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2026,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2027
                                } else {
                                    var t1394 int = index__85 + 1
                                    var t1395 uint8
                                    var inline2032 uint8 = _goml_runtime_core_string_byte_get(value__84, t1394)
                                    t1395 = inline2032
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1395, jp1283)
                                    var x169 bool = mtmp168._0
                                    var jp1410 bool
                                    if jp1280 {
                                        var t1412 bool = !saw_digit__92
                                        jp1410 = t1412
                                    } else {
                                        jp1410 = false
                                    }
                                    var jp1397 bool
                                    if jp1410 {
                                        var t1411 bool = index__85 == mantissa_start__89
                                        jp1397 = t1411
                                    } else {
                                        jp1397 = false
                                    }
                                    var t1407 bool = !previous_digit__96
                                    var jp1405 bool
                                    if t1407 {
                                        var t1408 bool = !jp1397
                                        jp1405 = t1408
                                    } else {
                                        jp1405 = false
                                    }
                                    var jp1402 bool
                                    if jp1405 {
                                        jp1402 = true
                                    } else {
                                        var t1406 bool = !x169
                                        jp1402 = t1406
                                    }
                                    if jp1402 {
                                        var inline2029 FloatNatural = float_natural_zero()
                                        var inline2030 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2029,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2030
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1399 int = compound_old173 + compound_value174
                                        index__85 = t1399
                                        continue
                                    }
                                }
                            } else {
                                var t1422 bool = current__97 == 46
                                var jp1419 bool
                                if t1422 {
                                    var t1423 bool = !saw_dot__93
                                    jp1419 = t1423
                                } else {
                                    jp1419 = false
                                }
                                if jp1419 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1420 int = compound_old178 + compound_value179
                                    index__85 = t1420
                                    continue
                                } else {
                                    break Loop_loop1373__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1373__3
                    }
                }
                var t1371 bool = !saw_digit__92
                if t1371 {
                    var inline2040 FloatNatural = float_natural_zero()
                    var inline2041 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2040,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2041
                } else {
                    var jp1287 uint8
                    if jp1280 {
                        jp1287 = 112
                    } else {
                        jp1287 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1366 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1367 bool = index__85 < t1366
                    var jp1304 bool
                    if t1367 {
                        var t1368 uint8
                        var inline2043 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1368 = inline2043
                        var t1369 uint8 = ascii_lower(t1368)
                        var t1370 bool = t1369 == jp1287
                        jp1304 = t1370
                    } else {
                        jp1304 = false
                    }
                    if jp1304 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1305 int = compound_old183 + compound_value184
                        index__85 = t1305
                        var t1356 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1357 bool = index__85 < t1356
                        var jp1351 bool
                        if t1357 {
                            var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1361 bool = t1360 == 43
                            if t1361 {
                                jp1351 = true
                            } else {
                                var t1362 uint8
                                var inline2045 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1362 = inline2045
                                var t1363 bool = t1362 == 45
                                jp1351 = t1363
                            }
                        } else {
                            jp1351 = false
                        }
                        if jp1351 {
                            var t1352 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1353 bool = t1352 == 45
                            exponent_negative__104 = t1353
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1354 int = compound_old187 + compound_value188
                            index__85 = t1354
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1312__3:
                        for {
                            var t1313 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1314 bool = index__85 < t1313
                            if t1314 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1348 bool = current__106 >= 48
                                var jp1317 bool
                                if t1348 {
                                    var t1349 bool = current__106 <= 57
                                    jp1317 = t1349
                                } else {
                                    jp1317 = false
                                }
                                if jp1317 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1321 bool = exponent__103 < 1000000
                                    if t1321 {
                                        var t1322 int = exponent__103 * 10
                                        var t1323 uint8 = current__106 - 48
                                        var t1324 int = int(uint8(t1323))
                                        var t1325 int = t1322 + t1324
                                        exponent__103 = t1325
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1319 int = compound_old196 + compound_value197
                                    index__85 = t1319
                                    continue
                                } else {
                                    var t1327 bool = current__106 == 95
                                    if t1327 {
                                        var t1344 bool = !previous_digit__96
                                        var jp1340 bool
                                        if t1344 {
                                            jp1340 = true
                                        } else {
                                            var t1345 int = index__85 + 1
                                            var t1346 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1347 bool = t1345 >= t1346
                                            jp1340 = t1347
                                        }
                                        var jp1335 bool
                                        if jp1340 {
                                            jp1335 = true
                                        } else {
                                            var t1341 int = index__85 + 1
                                            var t1342 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1341)
                                            var t1343 bool = t1342 < 48
                                            jp1335 = t1343
                                        }
                                        var jp1332 bool
                                        if jp1335 {
                                            jp1332 = true
                                        } else {
                                            var t1336 int = index__85 + 1
                                            var t1337 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1336)
                                            var t1338 bool = t1337 > 57
                                            jp1332 = t1338
                                        }
                                        if jp1332 {
                                            var t1333 ParsedFloat = invalid_parsed_float()
                                            return t1333
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1329 int = compound_old201 + compound_value202
                                            index__85 = t1329
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1312__3
                                    }
                                }
                            } else {
                                break Loop_loop1312__3
                            }
                        }
                        var t1310 bool = !exponent_digits__105
                        if t1310 {
                            var t1311 ParsedFloat = invalid_parsed_float()
                            return t1311
                        } else {
                            var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1301 bool = index__85 != t1300
                            if t1301 {
                                var t1302 ParsedFloat = invalid_parsed_float()
                                return t1302
                            } else {
                                if exponent_negative__104 {
                                    var t1299 int = 0 - exponent__103
                                    exponent__103 = t1299
                                } else {}
                                var jp1292 int
                                if jp1280 {
                                    jp1292 = 0
                                } else {
                                    var t1298 int = exponent__103 - fraction_digits__94
                                    jp1292 = t1298
                                }
                                var jp1294 int
                                if jp1280 {
                                    var t1296 int = fraction_digits__94 * 4
                                    var t1297 int = exponent__103 - t1296
                                    jp1294 = t1297
                                } else {
                                    jp1294 = 0
                                }
                                var t1295 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1292,
                                    binary_exponent: jp1294,
                                    hexadecimal: jp1280,
                                    significant_digits: significant_digits__95,
                                }
                                return t1295
                            }
                        }
                    } else {
                        if jp1280 {
                            var t1365 ParsedFloat = invalid_parsed_float()
                            return t1365
                        } else {
                            var t1300 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1301 bool = index__85 != t1300
                            if t1301 {
                                var t1302 ParsedFloat = invalid_parsed_float()
                                return t1302
                            } else {
                                if exponent_negative__104 {
                                    var t1299 int = 0 - exponent__103
                                    exponent__103 = t1299
                                } else {}
                                var jp1292 int
                                if jp1280 {
                                    jp1292 = 0
                                } else {
                                    var t1298 int = exponent__103 - fraction_digits__94
                                    jp1292 = t1298
                                }
                                var jp1294 int
                                if jp1280 {
                                    var t1296 int = fraction_digits__94 * 4
                                    var t1297 int = exponent__103 - t1296
                                    jp1294 = t1297
                                } else {
                                    jp1294 = 0
                                }
                                var t1295 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1292,
                                    binary_exponent: jp1294,
                                    hexadecimal: jp1280,
                                    significant_digits: significant_digits__95,
                                }
                                return t1295
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
    var inline2047 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2047, 1)
    var inline2049 FloatNatural = FloatNatural{
        words: inline2047,
    }
    result__26 = inline2049
    var count__27 int = 0
    Loop_loop1463:
    for {
        var t1464 bool = count__27 < exponent__25
        if t1464 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1465 int = compound_old46 + compound_value47
            count__27 = t1465
            continue
        } else {
            break Loop_loop1463
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1552 bool
    var inline2051 *_goml_vec_uint32 = numerator__65.words
    var inline2052 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2051)
    t1552 = inline2052
    if t1552 {
        var t1553 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1553
    } else {
        var t1549 bool = binary_shift__67 >= 0
        var jp1474 FloatNatural
        if t1549 {
            var t1550 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1474 = t1550
        } else {
            var t1551 FloatNatural = float_natural_copy(numerator__65)
            jp1474 = t1551
        }
        var t1545 bool = binary_shift__67 >= 0
        var jp1476 FloatNatural
        if t1545 {
            var t1546 FloatNatural = float_natural_copy(denominator__66)
            jp1476 = t1546
        } else {
            var t1547 int = 0 - binary_shift__67
            var t1548 FloatNatural = float_natural_shift_left(denominator__66, t1547)
            jp1476 = t1548
        }
        var t1477 int = float_natural_bit_length(jp1474)
        var t1478 int = float_natural_bit_length(jp1476)
        var exponent__72 int = t1477 - t1478
        var t1539 bool = exponent__72 >= 0
        var jp1480 int
        if t1539 {
            var t1540 FloatNatural = float_natural_shift_left(jp1476, exponent__72)
            var t1541 int = float_natural_compare(jp1474, t1540)
            jp1480 = t1541
        } else {
            var t1542 int = 0 - exponent__72
            var t1543 FloatNatural = float_natural_shift_left(jp1474, t1542)
            var t1544 int = float_natural_compare(t1543, jp1476)
            jp1480 = t1544
        }
        var t1536 bool = jp1480 < 0
        if t1536 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1537 int = compound_old120 - compound_value121
            exponent__72 = t1537
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1530 bool = exponent__72 > exponent_bias__69
        if t1530 {
            var t1531 int = exponent_bias__69 + exponent_bias__69
            var t1532 int = t1531 + 1
            var t1533 uint64 = uint64(int(t1532))
            var t1534 uint64 = t1533 << mantissa_bits__68
            var t1535 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1534,
                _1: true,
            }
            return t1535
        } else {
            var t1525 bool = exponent__72 < minimum_exponent__74
            var jp1484 uint64
            if t1525 {
                var t1526 int = mantissa_bits__68 - minimum_exponent__74
                var t1527 uint64 = float_rational_quotient(jp1474, jp1476, t1526)
                jp1484 = t1527
            } else {
                var t1528 int = mantissa_bits__68 - exponent__72
                var t1529 uint64 = float_rational_quotient(jp1474, jp1476, t1528)
                jp1484 = t1529
            }
            var mantissa__76 uint64 = jp1484
            var t1487 bool = exponent__72 < minimum_exponent__74
            if t1487 {
                var t1490 bool = mantissa__76 == 0
                if t1490 {
                    var t1491 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1491
                } else {
                    var t1494_lhs uint64 = 1
                    var t1494 uint64 = t1494_lhs << mantissa_bits__68
                    var t1495 bool = mantissa__76 >= t1494
                    if t1495 {
                        var t1496_lhs uint64 = 1
                        var t1496 uint64 = t1496_lhs << mantissa_bits__68
                        var t1497_lhs uint64 = 1
                        var t1497 uint64 = t1497_lhs << mantissa_bits__68
                        var t1498 uint64 = mantissa__76 - t1497
                        var t1499 uint64 = t1496 | t1498
                        var t1500 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1499,
                            _1: false,
                        }
                        return t1500
                    } else {
                        var t1501 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1501
                    }
                }
            } else {
                var t1518 int = mantissa_bits__68 + 1
                var t1519_lhs uint64 = 1
                var t1519 uint64 = t1519_lhs << t1518
                var t1520 bool = mantissa__76 >= t1519
                if t1520 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1521 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1521
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1523 int = compound_old128 + compound_value129
                    exponent__72 = t1523
                } else {}
                var t1505 bool = exponent__72 > exponent_bias__69
                if t1505 {
                    var t1506 int = exponent_bias__69 + exponent_bias__69
                    var t1507 int = t1506 + 1
                    var t1508 uint64 = uint64(int(t1507))
                    var t1509 uint64 = t1508 << mantissa_bits__68
                    var t1510 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1509,
                        _1: true,
                    }
                    return t1510
                } else {
                    var t1511 int = exponent__72 + exponent_bias__69
                    var t1512 uint64 = uint64(int(t1511))
                    var t1513 uint64 = t1512 << mantissa_bits__68
                    var t1514_lhs uint64 = 1
                    var t1514 uint64 = t1514_lhs << mantissa_bits__68
                    var t1515 uint64 = mantissa__76 - t1514
                    var t1516 uint64 = t1513 | t1515
                    var t1517 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1516,
                        _1: false,
                    }
                    return t1517
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1558 int = vec_len__Vec_6uint32(self__528)
    var t1559 bool = t1558 == 0
    return t1559
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1562:
    for {
        var t1570 *_goml_vec_uint32 = value__7.words
        var t1571 bool
        var inline2063 int = vec_len__Vec_6uint32(t1570)
        var inline2064 bool = inline2063 == 0
        t1571 = inline2064
        var t1572 bool = !t1571
        var jp1564 bool
        if t1572 {
            var t1573 *_goml_vec_uint32 = value__7.words
            var t1574 *_goml_vec_uint32 = value__7.words
            var t1575 int
            var inline2057 int = vec_len__Vec_6uint32(t1574)
            t1575 = inline2057
            var t1576 int = t1575 - 1
            var t1577 uint32 = vec_get__Vec_6uint32(t1573, t1576)
            var t1578 bool = t1577 == 0
            jp1564 = t1578
        } else {
            jp1564 = false
        }
        if jp1564 {
            var t1565 *_goml_vec_uint32 = value__7.words
            var t1566 *_goml_vec_uint32 = value__7.words
            var t1567 int
            var inline2061 int = vec_len__Vec_6uint32(t1566)
            t1567 = inline2061
            var t1568 int = t1567 - 1
            vec_truncate__Vec_6uint32(t1565, t1568)
            continue
        } else {
            break Loop_loop1562
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1587 bool = string_is_char_boundary(value__274, start__275)
    var jp1584 bool
    if t1587 {
        var t1588 bool = string_is_char_boundary(value__274, end__276)
        jp1584 = t1588
    } else {
        jp1584 = false
    }
    if jp1584 {
        var t1585 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1585
    } else {
        var t1586 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1586
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1603 int
    var inline2081 int = _goml_runtime_core_string_len(value__78)
    t1603 = inline2081
    var t1604 int
    var inline2079 int = _goml_runtime_core_string_len(expected__79)
    t1604 = inline2079
    var t1605 bool = t1603 != t1604
    if t1605 {
        return false
    } else {
        var index__80 int = 0
        var inline2071 uint8 = 97 - 65
        Loop_loop1593:
        for {
            var t1594 int
            var inline2077 int = _goml_runtime_core_string_len(value__78)
            t1594 = inline2077
            var t1595 bool = index__80 < t1594
            if t1595 {
                var t1599 uint8
                var inline2075 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1599 = inline2075
                var t1600 uint8
                var inline2068 bool = t1599 >= 65
                var inline2070 bool
                if inline2068 {
                    var inline2073 bool = t1599 <= 90
                    inline2070 = inline2073
                } else {
                    inline2070 = false
                }
                if inline2070 {
                    var inline2072 uint8 = t1599 + inline2071
                    t1600 = inline2072
                    var t1601 uint8
                    var inline2066 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1601 = inline2066
                    var t1602 bool = t1600 != t1601
                    if t1602 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1597 int = compound_old134 + compound_value135
                        index__80 = t1597
                        continue
                    }
                } else {
                    t1600 = t1599
                    var t1601 uint8
                    var inline2066 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1601 = inline2066
                    var t1602 bool = t1600 != t1601
                    if t1602 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1597 int = compound_old134 + compound_value135
                        index__80 = t1597
                        continue
                    }
                }
            } else {
                break Loop_loop1593
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1614 bool = value__77 >= 65
    var jp1611 bool
    if t1614 {
        var t1615 bool = value__77 <= 90
        jp1611 = t1615
    } else {
        jp1611 = false
    }
    if jp1611 {
        var t1612 uint8 = 97 - 65
        var t1613 uint8 = value__77 + t1612
        return t1613
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1642 bool = value__81 >= 48
    var jp1626 bool
    if t1642 {
        var t1643 bool = value__81 <= 57
        jp1626 = t1643
    } else {
        jp1626 = false
    }
    var jp1619 int
    if jp1626 {
        var t1627 uint8 = value__81 - 48
        var t1628 int = int(uint8(t1627))
        jp1619 = t1628
        var t1622 bool = jp1619 < base__82
        if t1622 {
            var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1619,
            }
            return t1623
        } else {
            var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1624
        }
    } else {
        var t1638 uint8
        var inline2097 bool = value__81 >= 65
        var inline2099 bool
        if inline2097 {
            var inline2102 bool = value__81 <= 90
            inline2099 = inline2102
        } else {
            inline2099 = false
        }
        if inline2099 {
            var inline2100 uint8 = 97 - 65
            var inline2101 uint8 = value__81 + inline2100
            t1638 = inline2101
            var t1639 bool = t1638 >= 97
            var jp1632 bool
            if t1639 {
                var t1640 uint8
                var inline2083 bool = value__81 >= 65
                var inline2085 bool
                if inline2083 {
                    var inline2088 bool = value__81 <= 90
                    inline2085 = inline2088
                } else {
                    inline2085 = false
                }
                if inline2085 {
                    var inline2086 uint8 = 97 - 65
                    var inline2087 uint8 = value__81 + inline2086
                    t1640 = inline2087
                    var t1641 bool = t1640 <= 102
                    jp1632 = t1641
                    if jp1632 {
                        var t1633 uint8
                        var inline2090 bool = value__81 >= 65
                        var inline2092 bool
                        if inline2090 {
                            var inline2095 bool = value__81 <= 90
                            inline2092 = inline2095
                        } else {
                            inline2092 = false
                        }
                        if inline2092 {
                            var inline2093 uint8 = 97 - 65
                            var inline2094 uint8 = value__81 + inline2093
                            t1633 = inline2094
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        } else {
                            t1633 = value__81
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        }
                    } else {
                        var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1637
                    }
                } else {
                    t1640 = value__81
                    var t1641 bool = t1640 <= 102
                    jp1632 = t1641
                    if jp1632 {
                        var t1633 uint8
                        var inline2090 bool = value__81 >= 65
                        var inline2092 bool
                        if inline2090 {
                            var inline2095 bool = value__81 <= 90
                            inline2092 = inline2095
                        } else {
                            inline2092 = false
                        }
                        if inline2092 {
                            var inline2093 uint8 = 97 - 65
                            var inline2094 uint8 = value__81 + inline2093
                            t1633 = inline2094
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        } else {
                            t1633 = value__81
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        }
                    } else {
                        var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1637
                    }
                }
            } else {
                jp1632 = false
                if jp1632 {
                    var t1633 uint8
                    var inline2090 bool = value__81 >= 65
                    var inline2092 bool
                    if inline2090 {
                        var inline2095 bool = value__81 <= 90
                        inline2092 = inline2095
                    } else {
                        inline2092 = false
                    }
                    if inline2092 {
                        var inline2093 uint8 = 97 - 65
                        var inline2094 uint8 = value__81 + inline2093
                        t1633 = inline2094
                        var t1634 uint8 = t1633 - 97
                        var t1635 uint8 = t1634 + 10
                        var t1636 int = int(uint8(t1635))
                        jp1619 = t1636
                        var t1622 bool = jp1619 < base__82
                        if t1622 {
                            var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1619,
                            }
                            return t1623
                        } else {
                            var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1624
                        }
                    } else {
                        t1633 = value__81
                        var t1634 uint8 = t1633 - 97
                        var t1635 uint8 = t1634 + 10
                        var t1636 int = int(uint8(t1635))
                        jp1619 = t1636
                        var t1622 bool = jp1619 < base__82
                        if t1622 {
                            var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1619,
                            }
                            return t1623
                        } else {
                            var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1624
                        }
                    }
                } else {
                    var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1637
                }
            }
        } else {
            t1638 = value__81
            var t1639 bool = t1638 >= 97
            var jp1632 bool
            if t1639 {
                var t1640 uint8
                var inline2083 bool = value__81 >= 65
                var inline2085 bool
                if inline2083 {
                    var inline2088 bool = value__81 <= 90
                    inline2085 = inline2088
                } else {
                    inline2085 = false
                }
                if inline2085 {
                    var inline2086 uint8 = 97 - 65
                    var inline2087 uint8 = value__81 + inline2086
                    t1640 = inline2087
                    var t1641 bool = t1640 <= 102
                    jp1632 = t1641
                    if jp1632 {
                        var t1633 uint8
                        var inline2090 bool = value__81 >= 65
                        var inline2092 bool
                        if inline2090 {
                            var inline2095 bool = value__81 <= 90
                            inline2092 = inline2095
                        } else {
                            inline2092 = false
                        }
                        if inline2092 {
                            var inline2093 uint8 = 97 - 65
                            var inline2094 uint8 = value__81 + inline2093
                            t1633 = inline2094
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        } else {
                            t1633 = value__81
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        }
                    } else {
                        var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1637
                    }
                } else {
                    t1640 = value__81
                    var t1641 bool = t1640 <= 102
                    jp1632 = t1641
                    if jp1632 {
                        var t1633 uint8
                        var inline2090 bool = value__81 >= 65
                        var inline2092 bool
                        if inline2090 {
                            var inline2095 bool = value__81 <= 90
                            inline2092 = inline2095
                        } else {
                            inline2092 = false
                        }
                        if inline2092 {
                            var inline2093 uint8 = 97 - 65
                            var inline2094 uint8 = value__81 + inline2093
                            t1633 = inline2094
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        } else {
                            t1633 = value__81
                            var t1634 uint8 = t1633 - 97
                            var t1635 uint8 = t1634 + 10
                            var t1636 int = int(uint8(t1635))
                            jp1619 = t1636
                            var t1622 bool = jp1619 < base__82
                            if t1622 {
                                var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1619,
                                }
                                return t1623
                            } else {
                                var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1624
                            }
                        }
                    } else {
                        var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1637
                    }
                }
            } else {
                jp1632 = false
                if jp1632 {
                    var t1633 uint8
                    var inline2090 bool = value__81 >= 65
                    var inline2092 bool
                    if inline2090 {
                        var inline2095 bool = value__81 <= 90
                        inline2092 = inline2095
                    } else {
                        inline2092 = false
                    }
                    if inline2092 {
                        var inline2093 uint8 = 97 - 65
                        var inline2094 uint8 = value__81 + inline2093
                        t1633 = inline2094
                        var t1634 uint8 = t1633 - 97
                        var t1635 uint8 = t1634 + 10
                        var t1636 int = int(uint8(t1635))
                        jp1619 = t1636
                        var t1622 bool = jp1619 < base__82
                        if t1622 {
                            var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1619,
                            }
                            return t1623
                        } else {
                            var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1624
                        }
                    } else {
                        t1633 = value__81
                        var t1634 uint8 = t1633 - 97
                        var t1635 uint8 = t1634 + 10
                        var t1636 int = int(uint8(t1635))
                        jp1619 = t1636
                        var t1622 bool = jp1619 < base__82
                        if t1622 {
                            var t1623 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1619,
                            }
                            return t1623
                        } else {
                            var t1624 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1624
                        }
                    }
                } else {
                    var t1637 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1637
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1646:
    for {
        var t1647 bool = carry__22 != 0
        if t1647 {
            var t1656 *_goml_vec_uint32 = value__20.words
            var t1657 int
            var inline2107 int = vec_len__Vec_6uint32(t1656)
            t1657 = inline2107
            var t1658 bool = index__23 == t1657
            if t1658 {
                var t1659 *_goml_vec_uint32 = value__20.words
                var inline2104 uint32 = 0
                vec_push__Vec_6uint32(t1659, inline2104)
            } else {}
            var t1649 *_goml_vec_uint32 = value__20.words
            var t1650 uint32 = vec_get__Vec_6uint32(t1649, index__23)
            var t1651 uint64 = uint64(uint32(t1650))
            var sum__24 uint64 = t1651 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1653_rhs int = 32
            var t1653 uint64 = sum__24 >> t1653_rhs
            carry__22 = t1653
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1654 int = compound_old42 + compound_value43
            index__23 = t1654
            continue
        } else {
            break Loop_loop1646
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1663 FloatNatural
    var inline2109 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2110 FloatNatural = FloatNatural{
        words: inline2109,
    }
    t1663 = inline2110
    var t1664 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1663,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1664
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1684 *_goml_vec_uint32 = value__9.words
    var t1685 bool
    var inline2116 int = vec_len__Vec_6uint32(t1684)
    var inline2117 bool = inline2116 == 0
    t1685 = inline2117
    if t1685 {
        return 0
    } else {
        var t1668 *_goml_vec_uint32 = value__9.words
        var t1669 *_goml_vec_uint32 = value__9.words
        var t1670 int
        var inline2114 int = vec_len__Vec_6uint32(t1669)
        t1670 = inline2114
        var t1671 int = t1670 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1668, t1671)
        var bits__11 int = 0
        Loop_loop1678:
        for {
            var t1679 bool = high__10 != 0
            if t1679 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1680 uint32 = compound_old9 >> compound_value10
                high__10 = t1680
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1682 int = compound_old12 + compound_value13
                bits__11 = t1682
                continue
            } else {
                break Loop_loop1678
            }
        }
        var t1673 *_goml_vec_uint32 = value__9.words
        var t1674 int
        var inline2112 int = vec_len__Vec_6uint32(t1673)
        t1674 = inline2112
        var t1675 int = t1674 - 1
        var t1676 int = t1675 * 32
        var t1677 int = t1676 + bits__11
        return t1677
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1707 *_goml_vec_uint32 = left__12.words
    var t1708 int
    var inline2127 int = vec_len__Vec_6uint32(t1707)
    t1708 = inline2127
    var t1709 *_goml_vec_uint32 = right__13.words
    var t1710 int
    var inline2125 int = vec_len__Vec_6uint32(t1709)
    t1710 = inline2125
    var t1711 bool = t1708 < t1710
    if t1711 {
        return -1
    } else {
        var t1713 *_goml_vec_uint32 = left__12.words
        var t1714 int
        var inline2121 int = vec_len__Vec_6uint32(t1713)
        t1714 = inline2121
        var t1715 *_goml_vec_uint32 = right__13.words
        var t1716 int
        var inline2119 int = vec_len__Vec_6uint32(t1715)
        t1716 = inline2119
        var t1717 bool = t1714 > t1716
        if t1717 {
            return 1
        } else {
            var t1689 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2123 int = vec_len__Vec_6uint32(t1689)
            index__14 = inline2123
            Loop_loop1691:
            for {
                var t1692 bool = index__14 > 0
                if t1692 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1693 int = compound_old17 - compound_value18
                    index__14 = t1693
                    var t1696 *_goml_vec_uint32 = left__12.words
                    var t1697 uint32 = vec_get__Vec_6uint32(t1696, index__14)
                    var t1698 *_goml_vec_uint32 = right__13.words
                    var t1699 uint32 = vec_get__Vec_6uint32(t1698, index__14)
                    var t1700 bool = t1697 < t1699
                    if t1700 {
                        return -1
                    } else {
                        var t1702 *_goml_vec_uint32 = left__12.words
                        var t1703 uint32 = vec_get__Vec_6uint32(t1702, index__14)
                        var t1704 *_goml_vec_uint32 = right__13.words
                        var t1705 uint32 = vec_get__Vec_6uint32(t1704, index__14)
                        var t1706 bool = t1703 > t1705
                        if t1706 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1691
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1753 bool = shift__57 >= 0
    var jp1721 FloatNatural
    if t1753 {
        var t1754 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1721 = t1754
    } else {
        var t1755 FloatNatural = float_natural_copy(numerator__55)
        jp1721 = t1755
    }
    var t1749 bool = shift__57 >= 0
    var jp1723 FloatNatural
    if t1749 {
        var t1750 FloatNatural = float_natural_copy(denominator__56)
        jp1723 = t1750
    } else {
        var t1751 int = 0 - shift__57
        var t1752 FloatNatural = float_natural_shift_left(denominator__56, t1751)
        jp1723 = t1752
    }
    var quotient__60 uint64 = 0
    Loop_loop1736:
    for {
        var t1737 int = float_natural_compare(jp1721, jp1723)
        var t1738 bool = t1737 >= 0
        if t1738 {
            var t1739 int = float_natural_bit_length(jp1721)
            var t1740 int = float_natural_bit_length(jp1723)
            var offset__61 int = t1739 - t1740
            var part__62 FloatNatural = float_natural_shift_left(jp1723, offset__61)
            var t1744 int = float_natural_compare(jp1721, part__62)
            var t1745 bool = t1744 < 0
            if t1745 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1746 int = compound_old105 - compound_value106
                offset__61 = t1746
                var t1748 FloatNatural = float_natural_shift_left(jp1723, offset__61)
                part__62 = t1748
            } else {}
            float_natural_subtract(jp1721, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1742 uint64 = compound_old111 | compound_value112
            quotient__60 = t1742
            continue
        } else {
            break Loop_loop1736
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1721, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1723)
    var t1730 bool = rounding__64 > 0
    var jp1727 bool
    if t1730 {
        jp1727 = true
    } else {
        var t1733 bool = rounding__64 == 0
        if t1733 {
            var t1734_rhs uint64 = 1
            var t1734 uint64 = quotient__60 & t1734_rhs
            var t1735 bool = t1734 == 1
            jp1727 = t1735
        } else {
            jp1727 = false
        }
    }
    if jp1727 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1728 uint64 = compound_old115 + compound_value116
        quotient__60 = t1728
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1771 bool = index__269 < 0
    var jp1763 bool
    if t1771 {
        jp1763 = true
    } else {
        var t1772 int
        var inline2129 int = _goml_runtime_core_string_len(value__268)
        t1772 = inline2129
        var t1773 bool = index__269 > t1772
        jp1763 = t1773
    }
    if jp1763 {
        return false
    } else {
        var t1766 int
        var inline2133 int = _goml_runtime_core_string_len(value__268)
        t1766 = inline2133
        var t1767 bool = index__269 == t1766
        if t1767 {
            return true
        } else {
            var t1768 uint8
            var inline2131 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1768 = inline2131
            var t1769_rhs uint8 = 192
            var t1769 uint8 = t1768 & t1769_rhs
            var t1770 bool = t1769 != 128
            return t1770
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1777:
    for {
        var t1778 *_goml_vec_uint32 = value__37.words
        var t1779 int
        var inline2137 int = vec_len__Vec_6uint32(t1778)
        t1779 = inline2137
        var t1780 bool = index__41 < t1779
        if t1780 {
            var t1794 *_goml_vec_uint32 = other__38.words
            var t1795 int
            var inline2135 int = vec_len__Vec_6uint32(t1794)
            t1795 = inline2135
            var t1796 bool = index__41 < t1795
            var jp1782 uint64
            if t1796 {
                var t1797 *_goml_vec_uint32 = other__38.words
                var t1798 uint32 = vec_get__Vec_6uint32(t1797, index__41)
                var t1799 uint64 = uint64(uint32(t1798))
                jp1782 = t1799
            } else {
                jp1782 = 0
            }
            var right__42 uint64 = jp1782 + borrow__40
            var t1783 *_goml_vec_uint32 = value__37.words
            var t1784 uint32 = vec_get__Vec_6uint32(t1783, index__41)
            var left__43 uint64 = uint64(uint32(t1784))
            var t1788 bool = left__43 >= right__42
            if t1788 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1789 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1789))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1791 uint64 = base__39 + left__43
                var t1792 uint64 = t1791 - right__42
                var value75 uint32 = uint32(uint64(t1792))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1786 int = compound_old79 + compound_value80
            index__41 = t1786
            continue
        } else {
            break Loop_loop1777
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
