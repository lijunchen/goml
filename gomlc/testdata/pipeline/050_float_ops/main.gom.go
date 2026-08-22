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
    var start32__13 float32 = 1.25
    var end32__14 float32 = 5.75
    var half__15 float32 = 0.5
    var scale__16 float32 = 2
    var mid32__17 float32
    var inline1877 float32 = end32__14 - start32__13
    var inline1878 float32 = inline1877 * half__15
    var inline1879 float32 = start32__13 + inline1878
    mid32__17 = inline1879
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline1872 float64 = dx__21 * dx__21
    var inline1873 float64 = dy__22 * dy__22
    var inline1874 float64 = inline1872 + inline1873
    var inline1875 float64 = inline1874 / 2
    energy__24 = inline1875
    var neg_dx__25 float64 = -dx__21
    var t820 float64 = energy__24 + dy__22
    var t821 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t820 - t821
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline1867 string = "mid32="
    var inline1868 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(mid32__17)
    var inline1869 string = inline1867 + inline1868
    println__T_string(inline1869)
    var inline1862 string = "neg_end32="
    var inline1863 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(neg_end32__18)
    var inline1864 string = inline1862 + inline1863
    println__T_string(inline1864)
    var inline1857 string = "ratio32="
    var inline1858 string = _goml_m_trait__impl_i_ToString_i_f32_i_to__string(ratio32__19)
    var inline1859 string = inline1857 + inline1858
    println__T_string(inline1859)
    var t822 string
    var inline1855 string = _goml_runtime_core_bool_to_string(less32__20)
    t822 = inline1855
    var t823 string = "less32=" + t822
    var inline1852 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline1852)
    var inline1847 string = "energy="
    var inline1848 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(energy__24)
    var inline1849 string = inline1847 + inline1848
    println__T_string(inline1849)
    var inline1842 string = "neg_dx="
    var inline1843 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(neg_dx__25)
    var inline1844 string = inline1842 + inline1843
    println__T_string(inline1844)
    var inline1837 string = "adjusted="
    var inline1838 string = _goml_m_trait__impl_i_ToString_i_f64_i_to__string(adjusted__26)
    var inline1839 string = inline1837 + inline1838
    println__T_string(inline1839)
    var t824 string
    var inline1835 string = _goml_runtime_core_bool_to_string(less64__28)
    t824 = inline1835
    var t825 string = "less64=" + t824
    var inline1832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
    _goml_runtime_core_string_println(inline1832)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_f32_i_to__string(self__413 float32) string {
    var inline1881 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(self__413)
    var inline1882 uint64 = uint64(uint32(inline1881))
    var inline1883 string = format_float_bits(inline1882, 23, 8, 127)
    return inline1883
}

func println__T_string(value__1 string) struct{} {
    var t830 string
    t830 = value__1
    _goml_runtime_core_string_println(t830)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_f64_i_to__string(self__414 float64) string {
    var inline1886 uint64 = _goml_ffi_math_x00_Float64bits_x0__q__m__z_u64_h771d143dab10df93b09bfe0f407ff63e(self__414)
    var inline1887 string = format_float_bits(inline1886, 52, 11, 1023)
    return inline1887
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t851 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t851
    var t852 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t852 != 0
    var t853_lhs uint64 = 1
    var t853 uint64 = t853_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t853 - 1
    var t854 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t854 & exponent_mask__166
    var t855_lhs uint64 = 1
    var t855 uint64 = t855_lhs << mantissa_bits__161
    var t856 uint64 = t855 - 1
    var fraction__168 uint64 = bits__160 & t856
    var t920 bool = exponent__167 == exponent_mask__166
    if t920 {
        var t922 bool = fraction__168 == 0
        if t922 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t928 bool = exponent__167 == 0
        var jp926 bool
        if t928 {
            var t929 bool = fraction__168 == 0
            jp926 = t929
        } else {
            jp926 = false
        }
        if jp926 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t917 bool = exponent__167 == 0
            var jp859 uint64
            if t917 {
                jp859 = fraction__168
            } else {
                var t918_lhs uint64 = 1
                var t918 uint64 = t918_lhs << mantissa_bits__161
                var t919 uint64 = fraction__168 | t918
                jp859 = t919
            }
            var t911 bool = exponent__167 == 0
            var jp861 int
            if t911 {
                var t912 int = 1 - exponent_bias__163
                var t913 int = t912 - mantissa_bits__161
                jp861 = t913
            } else {
                var t914 int = int(uint64(exponent__167))
                var t915 int = t914 - exponent_bias__163
                var t916 int = t915 - mantissa_bits__161
                jp861 = t916
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp859)
            var t866 bool = jp861 >= 0
            var jp863 int
            if t866 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp861)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t885 bool = mantissa_bits__161 == 23
                var jp868 int
                if t885 {
                    jp868 = 9
                } else {
                    jp868 = 17
                }
                var t882 int
                var inline1895 int = _goml_runtime_core_string_len(digits__173)
                t882 = inline1895
                var t883 bool = t882 < jp868
                var jp870 int
                if t883 {
                    var inline1889 int = _goml_runtime_core_string_len(digits__173)
                    jp870 = inline1889
                } else {
                    jp870 = jp868
                }
                var count__176 int = 1
                Loop_loop873:
                for {
                    var t874 bool = count__176 <= jp870
                    if t874 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t875 int
                        var inline1891 int = _goml_runtime_core_string_len(digits__173)
                        t875 = inline1891
                        var jp877 int
                        if x319 {
                            jp877 = 1
                        } else {
                            jp877 = 0
                        }
                        var point__180 int = t875 + jp877
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t881 bool = x322 == bits__160
                        if t881 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t879 int = compound_old324 + compound_value325
                            count__176 = t879
                            continue
                        }
                    } else {
                        break Loop_loop873
                    }
                }
                var inline1893 int = _goml_runtime_core_string_len(digits__173)
                jp863 = inline1893
                var t864 string = float_natural_decimal(exact_value__171)
                var t865 string = fixed_float_text(t864, jp863, negative__165)
                return t865
            } else {
                var count__183 int = 0
                var t907 int = 0 - jp861
                Loop_loop906:
                for {
                    var t908 bool = count__183 < t907
                    if t908 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t909 int = compound_old329 + compound_value330
                        count__183 = t909
                        continue
                    } else {
                        break Loop_loop906
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t887 int
                var inline1901 int = _goml_runtime_core_string_len(digits__184)
                t887 = inline1901
                var point__185 int = t887 + jp861
                var t905 bool = mantissa_bits__161 == 23
                var jp889 int
                if t905 {
                    jp889 = 9
                } else {
                    jp889 = 17
                }
                var t902 int
                var inline1899 int = _goml_runtime_core_string_len(digits__184)
                t902 = inline1899
                var t903 bool = t902 < jp889
                var jp891 int
                if t903 {
                    var inline1897 int = _goml_runtime_core_string_len(digits__184)
                    jp891 = inline1897
                } else {
                    jp891 = jp889
                }
                count__183 = 1
                Loop_loop893:
                for {
                    var t894 bool = count__183 <= jp891
                    if t894 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp896 int
                        if x336 {
                            jp896 = 1
                        } else {
                            jp896 = 0
                        }
                        var t897 int = point__185 + jp896
                        var candidate__191 string = fixed_float_text(rounded__190, t897, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t901 bool = x339 == bits__160
                        if t901 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t899 int = compound_old341 + compound_value342
                            count__183 = t899
                            continue
                        }
                    } else {
                        break Loop_loop893
                    }
                }
                jp863 = point__185
                var t864 string = float_natural_decimal(exact_value__171)
                var t865 string = fixed_float_text(t864, jp863, negative__165)
                return t865
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1907 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1908 FloatNatural = FloatNatural{
        words: inline1907,
    }
    result__2 = inline1908
    var t933 bool = value__1 != 0
    if t933 {
        var t934 *_goml_vec_uint32 = result__2.words
        var t935 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t934, t935)
        var t936_rhs int = 32
        var t936 uint64 = value__1 >> t936_rhs
        var high__3 uint32 = uint32(uint64(t936))
        var t938 bool = high__3 != 0
        if t938 {
            var t939 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t939, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t968 bool
    var inline1925 *_goml_vec_uint32 = value__28.words
    var inline1926 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1925)
    t968 = inline1926
    if t968 {
        var inline1910 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1911 FloatNatural = FloatNatural{
            words: inline1910,
        }
        return inline1911
    } else {
        var t971 bool = bits__29 == 0
        if t971 {
            var t972 FloatNatural = float_natural_copy(value__28)
            return t972
        } else {
            var result__30 FloatNatural
            var inline1922 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1923 FloatNatural = FloatNatural{
                words: inline1922,
            }
            result__30 = inline1923
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop963:
            for {
                var t964 bool = index__33 < word_shift__31
                if t964 {
                    var t965 *_goml_vec_uint32 = result__30.words
                    var inline1913 uint32 = 0
                    vec_push__Vec_6uint32(t965, inline1913)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t966 int = compound_old52 + compound_value53
                    index__33 = t966
                    continue
                } else {
                    break Loop_loop963
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop951:
            for {
                var t952 *_goml_vec_uint32 = value__28.words
                var t953 int
                var inline1918 int = vec_len__Vec_6uint32(t952)
                t953 = inline1918
                var t954 bool = index__33 < t953
                if t954 {
                    var t955 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t955, index__33)
                    var t956 uint64 = uint64(uint32(word__35))
                    var t957 uint64 = t956 << bit_shift__32
                    var shifted__36 uint64 = t957 | carry__34
                    var t958 *_goml_vec_uint32 = result__30.words
                    var t959 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t958, t959)
                    var t960_rhs int = 32
                    var t960 uint64 = shifted__36 >> t960_rhs
                    carry__34 = t960
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t961 int = compound_old59 + compound_value60
                    index__33 = t961
                    continue
                } else {
                    break Loop_loop951
                }
            }
            var t947 bool = carry__34 != 0
            if t947 {
                var t948 *_goml_vec_uint32 = result__30.words
                var t949 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t948, t949)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t995 bool
    var inline1941 *_goml_vec_uint32 = value__49.words
    var inline1942 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1941)
    t995 = inline1942
    if t995 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop988:
        for {
            var t989 bool
            var inline1930 *_goml_vec_uint32 = current__50.words
            var inline1931 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1930)
            t989 = inline1931
            var t990 bool = !t989
            if t990 {
                var t991 uint32 = float_natural_divide_small(current__50, 10)
                var t992 uint8 = uint8(uint32(t991))
                var t993 uint8 = t992 + 48
                vec_push__Vec_5uint8(reversed__51, t993)
                continue
            } else {
                break Loop_loop988
            }
        }
        var t977 int
        var inline1939 int = vec_len__Vec_5uint8(reversed__51)
        t977 = inline1939
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t977)
        var offset__53 int = 0
        Loop_loop979:
        for {
            var t980 int
            var inline1937 int = vec_len__Vec_5uint8(reversed__51)
            t980 = inline1937
            var t981 bool = offset__53 < t980
            if t981 {
                var t982 int
                var inline1935 int = vec_len__Vec_5uint8(reversed__51)
                t982 = inline1935
                var t983 int = t982 - offset__53
                var t984 int = t983 - 1
                var t985 uint8 = vec_get__Vec_5uint8(reversed__51, t984)
                vec_push__Vec_5uint8(output__52, t985)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t986 int = compound_old98 + compound_value99
                offset__53 = t986
                continue
            } else {
                break Loop_loop979
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t998 int = _goml_runtime_core_string_len(self__289)
    return t998
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1001 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1001)
    var index__148 int = 0
    Loop_loop1056:
    for {
        var t1057 bool = index__148 < count__146
        if t1057 {
            var t1058 uint8
            var inline1946 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1058 = inline1946
            vec_push__Vec_5uint8(output__147, t1058)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1059 int = compound_old267 + compound_value268
            index__148 = t1059
            continue
        } else {
            break Loop_loop1056
        }
    }
    var t1053 int
    var inline1967 int = _goml_runtime_core_string_len(exact__145)
    t1053 = inline1967
    var t1054 bool = count__146 == t1053
    if t1054 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1055 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1055
    } else {
        var next__150 uint8
        var inline1965 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline1965
        var trailing__151 bool = false
        var t1004 int = count__146 + 1
        index__148 = t1004
        Loop_loop1045:
        for {
            var t1046 int
            var inline1950 int = _goml_runtime_core_string_len(exact__145)
            t1046 = inline1950
            var t1047 bool = index__148 < t1046
            if t1047 {
                var t1051 uint8
                var inline1948 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1051 = inline1948
                var t1052 bool = t1051 != 48
                if t1052 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1049 int = compound_old278 + compound_value279
                index__148 = t1049
                continue
            } else {
                break Loop_loop1045
            }
        }
        var t1033 bool = next__150 > 53
        var jp1007 bool
        if t1033 {
            jp1007 = true
        } else {
            var t1036 bool = next__150 == 53
            if t1036 {
                if trailing__151 {
                    jp1007 = true
                } else {
                    var t1039 int
                    var inline1952 int = vec_len__Vec_5uint8(output__147)
                    t1039 = inline1952
                    var t1040 int = t1039 - 1
                    var t1041 uint8 = vec_get__Vec_5uint8(output__147, t1040)
                    var t1042 uint8 = t1041 - 48
                    var t1043_rhs uint8 = 2
                    var t1043 uint8 = t1042 % t1043_rhs
                    var t1044 bool = t1043 == 1
                    jp1007 = t1044
                }
            } else {
                jp1007 = false
            }
        }
        if jp1007 {
            var index__153 int
            var inline1963 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline1963
            Loop_loop1021:
            for {
                var t1022 bool = index__153 > 0
                if t1022 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1023 int = compound_old282 - compound_value283
                    index__153 = t1023
                    var t1026 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1027 bool = t1026 < 57
                    if t1027 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1028 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1028)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1030 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1030
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1021
                }
            }
            var t1011 int
            var inline1961 int = vec_len__Vec_5uint8(output__147)
            t1011 = inline1961
            var t1012 int = t1011 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1012)
            var inline1958 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline1958)
            index__153 = 0
            Loop_loop1015:
            for {
                var t1016 int
                var inline1956 int = vec_len__Vec_5uint8(output__147)
                t1016 = inline1956
                var t1017 bool = index__153 < t1016
                if t1017 {
                    var t1018 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1018)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1019 int = compound_old302 + compound_value303
                    index__153 = t1019
                    continue
                } else {
                    break Loop_loop1015
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1014 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1014
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1032 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1032
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline1974 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline1974
    Loop_loop1065:
    for {
        var t1070 bool = length__159 > 1
        var jp1067 bool
        if t1070 {
            var t1071 int = length__159 - 1
            var t1072 uint8
            var inline1969 uint8 = _goml_runtime_core_string_byte_get(value__158, t1071)
            t1072 = inline1969
            var t1073 bool = t1072 == 48
            jp1067 = t1073
        } else {
            jp1067 = false
        }
        if jp1067 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1068 int = compound_old312 - compound_value313
            length__159 = t1068
            continue
        } else {
            break Loop_loop1065
        }
    }
    var inline1971 int = 0
    var inline1972 string = string_byte_slice(value__158, inline1971, length__159)
    return inline1972
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline1976 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline1976)
    } else {}
    var t1078 bool = decimal_point__138 <= 0
    if t1078 {
        var inline1991 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline1991)
        var inline1988 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline1988)
        var index__141 int = 0
        var t1088 int = 0 - decimal_point__138
        Loop_loop1087:
        for {
            var t1089 bool = index__141 < t1088
            if t1089 {
                var inline1979 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline1979)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1090 int = compound_old234 + compound_value235
                index__141 = t1090
                continue
            } else {
                break Loop_loop1087
            }
        }
        index__141 = 0
        Loop_loop1081:
        for {
            var t1082 int
            var inline1986 int = _goml_runtime_core_string_len(digits__137)
            t1082 = inline1986
            var t1083 bool = index__141 < t1082
            if t1083 {
                var t1084 uint8
                var inline1984 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1084 = inline1984
                vec_push__Vec_5uint8(bytes__140, t1084)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1085 int = compound_old240 + compound_value241
                index__141 = t1085
                continue
            } else {
                break Loop_loop1081
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1093 int
        var inline2016 int = _goml_runtime_core_string_len(digits__137)
        t1093 = inline2016
        var t1094 bool = decimal_point__138 >= t1093
        if t1094 {
            var index__142 int = 0
            Loop_loop1101:
            for {
                var t1102 int
                var inline1998 int = _goml_runtime_core_string_len(digits__137)
                t1102 = inline1998
                var t1103 bool = index__142 < t1102
                if t1103 {
                    var t1104 uint8
                    var inline1996 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1104 = inline1996
                    vec_push__Vec_5uint8(bytes__140, t1104)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1105 int = compound_old244 + compound_value245
                    index__142 = t1105
                    continue
                } else {
                    break Loop_loop1101
                }
            }
            Loop_loop1097:
            for {
                var t1098 bool = index__142 < decimal_point__138
                if t1098 {
                    var inline2000 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2000)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1099 int = compound_old249 + compound_value250
                    index__142 = t1099
                    continue
                } else {
                    break Loop_loop1097
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1115:
            for {
                var t1116 bool = index__143 < decimal_point__138
                if t1116 {
                    var t1117 uint8
                    var inline2005 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1117 = inline2005
                    vec_push__Vec_5uint8(bytes__140, t1117)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1118 int = compound_old253 + compound_value254
                    index__143 = t1118
                    continue
                } else {
                    break Loop_loop1115
                }
            }
            var inline2013 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2013)
            Loop_loop1109:
            for {
                var t1110 int
                var inline2011 int = _goml_runtime_core_string_len(digits__137)
                t1110 = inline2011
                var t1111 bool = index__143 < t1110
                if t1111 {
                    var t1112 uint8
                    var inline2009 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1112 = inline2009
                    vec_push__Vec_5uint8(bytes__140, t1112)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1113 int = compound_old259 + compound_value260
                    index__143 = t1113
                    continue
                } else {
                    break Loop_loop1109
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
    var t1214 bool = parsed__110.valid
    var t1215 bool = !t1214
    if t1215 {
        var t1216 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1216
    } else {
        var t1208 bool = parsed__110.negative
        var jp1125 uint64
        if t1208 {
            var t1213 bool = mantissa_bits__108 == 23
            var jp1210 int
            if t1213 {
                jp1210 = 8
            } else {
                jp1210 = 11
            }
            var t1211 int = mantissa_bits__108 + jp1210
            var t1212_lhs uint64 = 1
            var t1212 uint64 = t1212_lhs << t1211
            jp1125 = t1212
        } else {
            jp1125 = 0
        }
        var t1207 bool = mantissa_bits__108 == 23
        var jp1127 int
        if t1207 {
            jp1127 = 8
        } else {
            jp1127 = 11
        }
        var t1128_lhs uint64 = 1
        var t1128 uint64 = t1128_lhs << jp1127
        var t1129 uint64 = t1128 - 1
        var exponent_mask__112 uint64 = t1129 << mantissa_bits__108
        var t1185 int = parsed__110.special
        var t1186 bool = t1185 == 1
        if t1186 {
            var t1187 uint64 = jp1125 | exponent_mask__112
            var t1188 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1187,
            }
            return t1188
        } else {
            var t1190 int = parsed__110.special
            var t1191 bool = t1190 == 2
            if t1191 {
                var t1195 int = mantissa_bits__108 - 1
                var t1196_lhs uint64 = 1
                var t1196 uint64 = t1196_lhs << t1195
                var t1197 uint64 = exponent_mask__112 | t1196
                var t1202 bool = mantissa_bits__108 == 52
                var jp1199 uint64
                if t1202 {
                    jp1199 = 1
                } else {
                    jp1199 = 0
                }
                var t1200 uint64 = t1197 | jp1199
                var t1201 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1200,
                }
                return t1201
            } else {
                var t1204 FloatNatural = parsed__110.numerator
                var t1205 bool
                var inline2018 *_goml_vec_uint32 = t1204.words
                var inline2019 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2018)
                t1205 = inline2019
                if t1205 {
                    var t1206 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1125,
                    }
                    return t1206
                } else {
                    var t1168 bool = parsed__110.hexadecimal
                    var t1169 bool = !t1168
                    if t1169 {
                        var t1170 int = parsed__110.significant_digits
                        var t1171 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1170 + t1171
                        var t1184 bool = mantissa_bits__108 == 23
                        var jp1173 int
                        if t1184 {
                            jp1173 = 40
                        } else {
                            jp1173 = 310
                        }
                        var t1183 bool = mantissa_bits__108 == 23
                        var jp1175 int
                        if t1183 {
                            jp1175 = -46
                        } else {
                            jp1175 = -325
                        }
                        var t1177 bool = decimal_position__113 > jp1173
                        if t1177 {
                            var t1178 uint64 = jp1125 | exponent_mask__112
                            var t1179 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1178,
                            }
                            return t1179
                        } else {
                            var t1181 bool = decimal_position__113 < jp1175
                            if t1181 {
                                var t1182 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1125,
                                }
                                return t1182
                            } else {
                                var t1164 bool = parsed__110.hexadecimal
                                var t1165 bool = !t1164
                                var jp1159 bool
                                if t1165 {
                                    var t1166 int = parsed__110.decimal_exponent
                                    var t1167 bool = t1166 < 0
                                    jp1159 = t1167
                                } else {
                                    jp1159 = false
                                }
                                var jp1133 FloatNatural
                                if jp1159 {
                                    var t1160 int = parsed__110.decimal_exponent
                                    var t1161 int = 0 - t1160
                                    var t1162 FloatNatural = float_natural_power5(t1161)
                                    jp1133 = t1162
                                } else {
                                    var inline2021 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2021, 1)
                                    var inline2023 FloatNatural = FloatNatural{
                                        words: inline2021,
                                    }
                                    jp1133 = inline2023
                                }
                                var t1154 bool = parsed__110.hexadecimal
                                var t1155 bool = !t1154
                                var jp1145 bool
                                if t1155 {
                                    var t1156 int = parsed__110.decimal_exponent
                                    var t1157 bool = t1156 > 0
                                    jp1145 = t1157
                                } else {
                                    jp1145 = false
                                }
                                var jp1135 FloatNatural
                                if jp1145 {
                                    var t1146 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1146)
                                    var count__118 int = 0
                                    Loop_loop1148:
                                    for {
                                        var t1149 int = parsed__110.decimal_exponent
                                        var t1150 bool = count__118 < t1149
                                        if t1150 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1151 int = compound_old213 + compound_value214
                                            count__118 = t1151
                                            continue
                                        } else {
                                            break Loop_loop1148
                                        }
                                    }
                                    jp1135 = result__117
                                    var t1141 bool = parsed__110.hexadecimal
                                    var jp1137 int
                                    if t1141 {
                                        var t1142 int = parsed__110.binary_exponent
                                        jp1137 = t1142
                                    } else {
                                        var t1143 int = parsed__110.decimal_exponent
                                        jp1137 = t1143
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1135, jp1133, jp1137, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1138 bool = !x219
                                    var t1139 uint64 = jp1125 | x218
                                    var t1140 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1138,
                                        _1: t1139,
                                    }
                                    return t1140
                                } else {
                                    var t1153 FloatNatural = parsed__110.numerator
                                    jp1135 = t1153
                                    var t1141 bool = parsed__110.hexadecimal
                                    var jp1137 int
                                    if t1141 {
                                        var t1142 int = parsed__110.binary_exponent
                                        jp1137 = t1142
                                    } else {
                                        var t1143 int = parsed__110.decimal_exponent
                                        jp1137 = t1143
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1135, jp1133, jp1137, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1138 bool = !x219
                                    var t1139 uint64 = jp1125 | x218
                                    var t1140 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1138,
                                        _1: t1139,
                                    }
                                    return t1140
                                }
                            }
                        }
                    } else {
                        var t1164 bool = parsed__110.hexadecimal
                        var t1165 bool = !t1164
                        var jp1159 bool
                        if t1165 {
                            var t1166 int = parsed__110.decimal_exponent
                            var t1167 bool = t1166 < 0
                            jp1159 = t1167
                        } else {
                            jp1159 = false
                        }
                        var jp1133 FloatNatural
                        if jp1159 {
                            var t1160 int = parsed__110.decimal_exponent
                            var t1161 int = 0 - t1160
                            var t1162 FloatNatural = float_natural_power5(t1161)
                            jp1133 = t1162
                        } else {
                            var inline2021 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2021, 1)
                            var inline2023 FloatNatural = FloatNatural{
                                words: inline2021,
                            }
                            jp1133 = inline2023
                        }
                        var t1154 bool = parsed__110.hexadecimal
                        var t1155 bool = !t1154
                        var jp1145 bool
                        if t1155 {
                            var t1156 int = parsed__110.decimal_exponent
                            var t1157 bool = t1156 > 0
                            jp1145 = t1157
                        } else {
                            jp1145 = false
                        }
                        var jp1135 FloatNatural
                        if jp1145 {
                            var t1146 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1146)
                            var count__118 int = 0
                            Loop_loop1148__2:
                            for {
                                var t1149 int = parsed__110.decimal_exponent
                                var t1150 bool = count__118 < t1149
                                if t1150 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1151 int = compound_old213 + compound_value214
                                    count__118 = t1151
                                    continue
                                } else {
                                    break Loop_loop1148__2
                                }
                            }
                            jp1135 = result__117
                            var t1141 bool = parsed__110.hexadecimal
                            var jp1137 int
                            if t1141 {
                                var t1142 int = parsed__110.binary_exponent
                                jp1137 = t1142
                            } else {
                                var t1143 int = parsed__110.decimal_exponent
                                jp1137 = t1143
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1135, jp1133, jp1137, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1138 bool = !x219
                            var t1139 uint64 = jp1125 | x218
                            var t1140 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1138,
                                _1: t1139,
                            }
                            return t1140
                        } else {
                            var t1153 FloatNatural = parsed__110.numerator
                            jp1135 = t1153
                            var t1141 bool = parsed__110.hexadecimal
                            var jp1137 int
                            if t1141 {
                                var t1142 int = parsed__110.binary_exponent
                                jp1137 = t1142
                            } else {
                                var t1143 int = parsed__110.decimal_exponent
                                jp1137 = t1143
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1135, jp1133, jp1137, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1138 bool = !x219
                            var t1139 uint64 = jp1125 | x218
                            var t1140 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1138,
                                _1: t1139,
                            }
                            return t1140
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1238 bool = factor__16 == 0
    if t1238 {
        var t1239 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1239, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1232 uint64 = uint64(uint32(factor__16))
        Loop_loop1225:
        for {
            var t1226 *_goml_vec_uint32 = value__15.words
            var t1227 int
            var inline2027 int = vec_len__Vec_6uint32(t1226)
            t1227 = inline2027
            var t1228 bool = index__18 < t1227
            if t1228 {
                var t1229 *_goml_vec_uint32 = value__15.words
                var t1230 uint32 = vec_get__Vec_6uint32(t1229, index__18)
                var t1231 uint64 = uint64(uint32(t1230))
                var t1233 uint64 = t1231 * t1232
                var product__19 uint64 = t1233 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1235_rhs int = 32
                var t1235 uint64 = product__19 >> t1235_rhs
                carry__17 = t1235
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1236 int = compound_old30 + compound_value31
                index__18 = t1236
                continue
            } else {
                break Loop_loop1225
            }
        }
        var t1221 bool = carry__17 != 0
        if t1221 {
            var t1222 *_goml_vec_uint32 = value__15.words
            var t1223 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1222, t1223)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1242 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1243 FloatNatural = FloatNatural{
        words: t1242,
    }
    return t1243
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2038 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2039 FloatNatural = FloatNatural{
        words: inline2038,
    }
    result__5 = inline2039
    var index__6 int = 0
    Loop_loop1253:
    for {
        var t1254 *_goml_vec_uint32 = value__4.words
        var t1255 int
        var inline2036 int = vec_len__Vec_6uint32(t1254)
        t1255 = inline2036
        var t1256 bool = index__6 < t1255
        if t1256 {
            var t1257 *_goml_vec_uint32 = result__5.words
            var t1258 *_goml_vec_uint32 = value__4.words
            var t1259 uint32 = vec_get__Vec_6uint32(t1258, index__6)
            vec_push__Vec_6uint32(t1257, t1259)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1260 int = compound_old4 + compound_value5
            index__6 = t1260
            continue
        } else {
            break Loop_loop1253
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1267 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2041 int = vec_len__Vec_6uint32(t1267)
    index__47 = inline2041
    var t1278 uint64 = uint64(uint32(divisor__45))
    var t1281 uint64 = uint64(uint32(divisor__45))
    Loop_loop1270:
    for {
        var t1271 bool = index__47 > 0
        if t1271 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1272 int = compound_old83 - compound_value84
            index__47 = t1272
            var t1274_rhs int = 32
            var t1274 uint64 = remainder__46 << t1274_rhs
            var t1275 *_goml_vec_uint32 = value__44.words
            var t1276 uint32 = vec_get__Vec_6uint32(t1275, index__47)
            var t1277 uint64 = uint64(uint32(t1276))
            var current__48 uint64 = t1274 | t1277
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1279 uint64 = current__48 / t1278
            var value90 uint32 = uint32(uint64(t1279))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1282 uint64 = current__48 % t1281
            remainder__46 = t1282
            continue
        } else {
            break Loop_loop1270
        }
    }
    float_natural_trim(value__44)
    var t1269 uint32 = uint32(uint64(remainder__46))
    return t1269
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1290 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1290
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2043 bool = string_is_char_boundary(self__294, start__295)
    var inline2045 bool
    if inline2043 {
        var inline2048 bool = string_is_char_boundary(self__294, end__296)
        inline2045 = inline2048
    } else {
        inline2045 = false
    }
    if inline2045 {
        var inline2046 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2046
    } else {
        var inline2047 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2047
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1478 bool = string_equals_ascii_case(value__84, "nan")
    if t1478 {
        var t1479 FloatNatural
        var inline2050 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2051 FloatNatural = FloatNatural{
            words: inline2050,
        }
        t1479 = inline2051
        var t1480 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1479,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1480
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1470 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1471 bool = index__85 < t1470
        var jp1465 bool
        if t1471 {
            var t1474 uint8
            var inline2055 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1474 = inline2055
            var t1475 bool = t1474 == 43
            if t1475 {
                jp1465 = true
            } else {
                var t1476 uint8
                var inline2053 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1476 = inline2053
                var t1477 bool = t1476 == 45
                jp1465 = t1477
            }
        } else {
            jp1465 = false
        }
        if jp1465 {
            var t1466 uint8
            var inline2057 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1466 = inline2057
            var t1467 bool = t1466 == 45
            negative__86 = t1467
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1468 int = compound_old140 + compound_value141
            index__85 = t1468
        } else {}
        var t1298 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1298)
        var t1462 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1459 bool
        if t1462 {
            jp1459 = true
        } else {
            var t1463 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1459 = t1463
        }
        if jp1459 {
            var t1460 FloatNatural
            var inline2059 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2060 FloatNatural = FloatNatural{
                words: inline2059,
            }
            t1460 = inline2060
            var t1461 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1460,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1461
        } else {
            var t1453 int = index__85 + 2
            var t1454 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1455 bool = t1453 <= t1454
            var jp1448 bool
            if t1455 {
                var t1456 uint8
                var inline2062 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1456 = inline2062
                var t1457 bool = t1456 == 48
                jp1448 = t1457
            } else {
                jp1448 = false
            }
            var jp1301 bool
            if jp1448 {
                var t1449 int = index__85 + 1
                var t1450 uint8
                var inline2071 uint8 = _goml_runtime_core_string_byte_get(value__84, t1449)
                t1450 = inline2071
                var t1451 uint8
                var inline2064 bool = t1450 >= 65
                var inline2066 bool
                if inline2064 {
                    var inline2069 bool = t1450 <= 90
                    inline2066 = inline2069
                } else {
                    inline2066 = false
                }
                if inline2066 {
                    var inline2067 uint8 = 97 - 65
                    var inline2068 uint8 = t1450 + inline2067
                    t1451 = inline2068
                    var t1452 bool = t1451 == 120
                    jp1301 = t1452
                    if jp1301 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1445 int = compound_old145 + compound_value146
                        index__85 = t1445
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1304 int
                    if jp1301 {
                        jp1304 = 16
                    } else {
                        jp1304 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1398 uint32 = uint32(int(jp1304))
                    Loop_loop1394:
                    for {
                        var t1395 int
                        var inline2085 int = _goml_runtime_core_string_len(value__84)
                        t1395 = inline2085
                        var t1396 bool = index__85 < t1395
                        if t1396 {
                            var current__97 uint8
                            var inline2083 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2083
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1304)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1398)
                                var t1399 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1399)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1410 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1410
                                } else {}
                                var t1408 bool = significant_digits__95 > 0
                                var jp1405 bool
                                if t1408 {
                                    jp1405 = true
                                } else {
                                    var t1409 bool = x151 != 0
                                    jp1405 = t1409
                                }
                                if jp1405 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1406 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1406
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1402 int = compound_old164 + compound_value165
                                index__85 = t1402
                                continue
                            } else {
                                var t1413 bool = current__97 == 95
                                if t1413 {
                                    var t1434 int = index__85 + 1
                                    var t1435 int
                                    var inline2081 int = _goml_runtime_core_string_len(value__84)
                                    t1435 = inline2081
                                    var t1436 bool = t1434 >= t1435
                                    if t1436 {
                                        var inline2073 FloatNatural = float_natural_zero()
                                        var inline2074 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2073,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2074
                                    } else {
                                        var t1415 int = index__85 + 1
                                        var t1416 uint8
                                        var inline2079 uint8 = _goml_runtime_core_string_byte_get(value__84, t1415)
                                        t1416 = inline2079
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1416, jp1304)
                                        var x169 bool = mtmp168._0
                                        var jp1431 bool
                                        if jp1301 {
                                            var t1433 bool = !saw_digit__92
                                            jp1431 = t1433
                                        } else {
                                            jp1431 = false
                                        }
                                        var jp1418 bool
                                        if jp1431 {
                                            var t1432 bool = index__85 == mantissa_start__89
                                            jp1418 = t1432
                                        } else {
                                            jp1418 = false
                                        }
                                        var t1428 bool = !previous_digit__96
                                        var jp1426 bool
                                        if t1428 {
                                            var t1429 bool = !jp1418
                                            jp1426 = t1429
                                        } else {
                                            jp1426 = false
                                        }
                                        var jp1423 bool
                                        if jp1426 {
                                            jp1423 = true
                                        } else {
                                            var t1427 bool = !x169
                                            jp1423 = t1427
                                        }
                                        if jp1423 {
                                            var inline2076 FloatNatural = float_natural_zero()
                                            var inline2077 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2076,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2077
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1420 int = compound_old173 + compound_value174
                                            index__85 = t1420
                                            continue
                                        }
                                    }
                                } else {
                                    var t1443 bool = current__97 == 46
                                    var jp1440 bool
                                    if t1443 {
                                        var t1444 bool = !saw_dot__93
                                        jp1440 = t1444
                                    } else {
                                        jp1440 = false
                                    }
                                    if jp1440 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1441 int = compound_old178 + compound_value179
                                        index__85 = t1441
                                        continue
                                    } else {
                                        break Loop_loop1394
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1394
                        }
                    }
                    var t1392 bool = !saw_digit__92
                    if t1392 {
                        var inline2087 FloatNatural = float_natural_zero()
                        var inline2088 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2087,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2088
                    } else {
                        var jp1308 uint8
                        if jp1301 {
                            jp1308 = 112
                        } else {
                            jp1308 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1388 bool = index__85 < t1387
                        var jp1325 bool
                        if t1388 {
                            var t1389 uint8
                            var inline2090 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1389 = inline2090
                            var t1390 uint8 = ascii_lower(t1389)
                            var t1391 bool = t1390 == jp1308
                            jp1325 = t1391
                        } else {
                            jp1325 = false
                        }
                        if jp1325 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1326 int = compound_old183 + compound_value184
                            index__85 = t1326
                            var t1377 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1378 bool = index__85 < t1377
                            var jp1372 bool
                            if t1378 {
                                var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1382 bool = t1381 == 43
                                if t1382 {
                                    jp1372 = true
                                } else {
                                    var t1383 uint8
                                    var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1383 = inline2092
                                    var t1384 bool = t1383 == 45
                                    jp1372 = t1384
                                }
                            } else {
                                jp1372 = false
                            }
                            if jp1372 {
                                var t1373 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1374 bool = t1373 == 45
                                exponent_negative__104 = t1374
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1375 int = compound_old187 + compound_value188
                                index__85 = t1375
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1333:
                            for {
                                var t1334 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1335 bool = index__85 < t1334
                                if t1335 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1369 bool = current__106 >= 48
                                    var jp1338 bool
                                    if t1369 {
                                        var t1370 bool = current__106 <= 57
                                        jp1338 = t1370
                                    } else {
                                        jp1338 = false
                                    }
                                    if jp1338 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1342 bool = exponent__103 < 1000000
                                        if t1342 {
                                            var t1343 int = exponent__103 * 10
                                            var t1344 uint8 = current__106 - 48
                                            var t1345 int = int(uint8(t1344))
                                            var t1346 int = t1343 + t1345
                                            exponent__103 = t1346
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1340 int = compound_old196 + compound_value197
                                        index__85 = t1340
                                        continue
                                    } else {
                                        var t1348 bool = current__106 == 95
                                        if t1348 {
                                            var t1365 bool = !previous_digit__96
                                            var jp1361 bool
                                            if t1365 {
                                                jp1361 = true
                                            } else {
                                                var t1366 int = index__85 + 1
                                                var t1367 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1368 bool = t1366 >= t1367
                                                jp1361 = t1368
                                            }
                                            var jp1356 bool
                                            if jp1361 {
                                                jp1356 = true
                                            } else {
                                                var t1362 int = index__85 + 1
                                                var t1363 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1362)
                                                var t1364 bool = t1363 < 48
                                                jp1356 = t1364
                                            }
                                            var jp1353 bool
                                            if jp1356 {
                                                jp1353 = true
                                            } else {
                                                var t1357 int = index__85 + 1
                                                var t1358 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1357)
                                                var t1359 bool = t1358 > 57
                                                jp1353 = t1359
                                            }
                                            if jp1353 {
                                                var t1354 ParsedFloat = invalid_parsed_float()
                                                return t1354
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1350 int = compound_old201 + compound_value202
                                                index__85 = t1350
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1333
                                        }
                                    }
                                } else {
                                    break Loop_loop1333
                                }
                            }
                            var t1331 bool = !exponent_digits__105
                            if t1331 {
                                var t1332 ParsedFloat = invalid_parsed_float()
                                return t1332
                            } else {
                                var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1322 bool = index__85 != t1321
                                if t1322 {
                                    var t1323 ParsedFloat = invalid_parsed_float()
                                    return t1323
                                } else {
                                    if exponent_negative__104 {
                                        var t1320 int = 0 - exponent__103
                                        exponent__103 = t1320
                                    } else {}
                                    var jp1313 int
                                    if jp1301 {
                                        jp1313 = 0
                                    } else {
                                        var t1319 int = exponent__103 - fraction_digits__94
                                        jp1313 = t1319
                                    }
                                    var jp1315 int
                                    if jp1301 {
                                        var t1317 int = fraction_digits__94 * 4
                                        var t1318 int = exponent__103 - t1317
                                        jp1315 = t1318
                                    } else {
                                        jp1315 = 0
                                    }
                                    var t1316 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1313,
                                        binary_exponent: jp1315,
                                        hexadecimal: jp1301,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1316
                                }
                            }
                        } else {
                            if jp1301 {
                                var t1386 ParsedFloat = invalid_parsed_float()
                                return t1386
                            } else {
                                var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1322 bool = index__85 != t1321
                                if t1322 {
                                    var t1323 ParsedFloat = invalid_parsed_float()
                                    return t1323
                                } else {
                                    if exponent_negative__104 {
                                        var t1320 int = 0 - exponent__103
                                        exponent__103 = t1320
                                    } else {}
                                    var jp1313 int
                                    if jp1301 {
                                        jp1313 = 0
                                    } else {
                                        var t1319 int = exponent__103 - fraction_digits__94
                                        jp1313 = t1319
                                    }
                                    var jp1315 int
                                    if jp1301 {
                                        var t1317 int = fraction_digits__94 * 4
                                        var t1318 int = exponent__103 - t1317
                                        jp1315 = t1318
                                    } else {
                                        jp1315 = 0
                                    }
                                    var t1316 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1313,
                                        binary_exponent: jp1315,
                                        hexadecimal: jp1301,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1316
                                }
                            }
                        }
                    }
                } else {
                    t1451 = t1450
                    var t1452 bool = t1451 == 120
                    jp1301 = t1452
                    if jp1301 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1445 int = compound_old145 + compound_value146
                        index__85 = t1445
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1304 int
                    if jp1301 {
                        jp1304 = 16
                    } else {
                        jp1304 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1398 uint32 = uint32(int(jp1304))
                    Loop_loop1394__2:
                    for {
                        var t1395 int
                        var inline2085 int = _goml_runtime_core_string_len(value__84)
                        t1395 = inline2085
                        var t1396 bool = index__85 < t1395
                        if t1396 {
                            var current__97 uint8
                            var inline2083 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2083
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1304)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1398)
                                var t1399 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1399)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1410 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1410
                                } else {}
                                var t1408 bool = significant_digits__95 > 0
                                var jp1405 bool
                                if t1408 {
                                    jp1405 = true
                                } else {
                                    var t1409 bool = x151 != 0
                                    jp1405 = t1409
                                }
                                if jp1405 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1406 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1406
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1402 int = compound_old164 + compound_value165
                                index__85 = t1402
                                continue
                            } else {
                                var t1413 bool = current__97 == 95
                                if t1413 {
                                    var t1434 int = index__85 + 1
                                    var t1435 int
                                    var inline2081 int = _goml_runtime_core_string_len(value__84)
                                    t1435 = inline2081
                                    var t1436 bool = t1434 >= t1435
                                    if t1436 {
                                        var inline2073 FloatNatural = float_natural_zero()
                                        var inline2074 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2073,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2074
                                    } else {
                                        var t1415 int = index__85 + 1
                                        var t1416 uint8
                                        var inline2079 uint8 = _goml_runtime_core_string_byte_get(value__84, t1415)
                                        t1416 = inline2079
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1416, jp1304)
                                        var x169 bool = mtmp168._0
                                        var jp1431 bool
                                        if jp1301 {
                                            var t1433 bool = !saw_digit__92
                                            jp1431 = t1433
                                        } else {
                                            jp1431 = false
                                        }
                                        var jp1418 bool
                                        if jp1431 {
                                            var t1432 bool = index__85 == mantissa_start__89
                                            jp1418 = t1432
                                        } else {
                                            jp1418 = false
                                        }
                                        var t1428 bool = !previous_digit__96
                                        var jp1426 bool
                                        if t1428 {
                                            var t1429 bool = !jp1418
                                            jp1426 = t1429
                                        } else {
                                            jp1426 = false
                                        }
                                        var jp1423 bool
                                        if jp1426 {
                                            jp1423 = true
                                        } else {
                                            var t1427 bool = !x169
                                            jp1423 = t1427
                                        }
                                        if jp1423 {
                                            var inline2076 FloatNatural = float_natural_zero()
                                            var inline2077 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2076,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2077
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1420 int = compound_old173 + compound_value174
                                            index__85 = t1420
                                            continue
                                        }
                                    }
                                } else {
                                    var t1443 bool = current__97 == 46
                                    var jp1440 bool
                                    if t1443 {
                                        var t1444 bool = !saw_dot__93
                                        jp1440 = t1444
                                    } else {
                                        jp1440 = false
                                    }
                                    if jp1440 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1441 int = compound_old178 + compound_value179
                                        index__85 = t1441
                                        continue
                                    } else {
                                        break Loop_loop1394__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1394__2
                        }
                    }
                    var t1392 bool = !saw_digit__92
                    if t1392 {
                        var inline2087 FloatNatural = float_natural_zero()
                        var inline2088 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2087,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2088
                    } else {
                        var jp1308 uint8
                        if jp1301 {
                            jp1308 = 112
                        } else {
                            jp1308 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1388 bool = index__85 < t1387
                        var jp1325 bool
                        if t1388 {
                            var t1389 uint8
                            var inline2090 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1389 = inline2090
                            var t1390 uint8 = ascii_lower(t1389)
                            var t1391 bool = t1390 == jp1308
                            jp1325 = t1391
                        } else {
                            jp1325 = false
                        }
                        if jp1325 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1326 int = compound_old183 + compound_value184
                            index__85 = t1326
                            var t1377 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1378 bool = index__85 < t1377
                            var jp1372 bool
                            if t1378 {
                                var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1382 bool = t1381 == 43
                                if t1382 {
                                    jp1372 = true
                                } else {
                                    var t1383 uint8
                                    var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1383 = inline2092
                                    var t1384 bool = t1383 == 45
                                    jp1372 = t1384
                                }
                            } else {
                                jp1372 = false
                            }
                            if jp1372 {
                                var t1373 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1374 bool = t1373 == 45
                                exponent_negative__104 = t1374
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1375 int = compound_old187 + compound_value188
                                index__85 = t1375
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1333__2:
                            for {
                                var t1334 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1335 bool = index__85 < t1334
                                if t1335 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1369 bool = current__106 >= 48
                                    var jp1338 bool
                                    if t1369 {
                                        var t1370 bool = current__106 <= 57
                                        jp1338 = t1370
                                    } else {
                                        jp1338 = false
                                    }
                                    if jp1338 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1342 bool = exponent__103 < 1000000
                                        if t1342 {
                                            var t1343 int = exponent__103 * 10
                                            var t1344 uint8 = current__106 - 48
                                            var t1345 int = int(uint8(t1344))
                                            var t1346 int = t1343 + t1345
                                            exponent__103 = t1346
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1340 int = compound_old196 + compound_value197
                                        index__85 = t1340
                                        continue
                                    } else {
                                        var t1348 bool = current__106 == 95
                                        if t1348 {
                                            var t1365 bool = !previous_digit__96
                                            var jp1361 bool
                                            if t1365 {
                                                jp1361 = true
                                            } else {
                                                var t1366 int = index__85 + 1
                                                var t1367 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1368 bool = t1366 >= t1367
                                                jp1361 = t1368
                                            }
                                            var jp1356 bool
                                            if jp1361 {
                                                jp1356 = true
                                            } else {
                                                var t1362 int = index__85 + 1
                                                var t1363 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1362)
                                                var t1364 bool = t1363 < 48
                                                jp1356 = t1364
                                            }
                                            var jp1353 bool
                                            if jp1356 {
                                                jp1353 = true
                                            } else {
                                                var t1357 int = index__85 + 1
                                                var t1358 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1357)
                                                var t1359 bool = t1358 > 57
                                                jp1353 = t1359
                                            }
                                            if jp1353 {
                                                var t1354 ParsedFloat = invalid_parsed_float()
                                                return t1354
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1350 int = compound_old201 + compound_value202
                                                index__85 = t1350
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1333__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1333__2
                                }
                            }
                            var t1331 bool = !exponent_digits__105
                            if t1331 {
                                var t1332 ParsedFloat = invalid_parsed_float()
                                return t1332
                            } else {
                                var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1322 bool = index__85 != t1321
                                if t1322 {
                                    var t1323 ParsedFloat = invalid_parsed_float()
                                    return t1323
                                } else {
                                    if exponent_negative__104 {
                                        var t1320 int = 0 - exponent__103
                                        exponent__103 = t1320
                                    } else {}
                                    var jp1313 int
                                    if jp1301 {
                                        jp1313 = 0
                                    } else {
                                        var t1319 int = exponent__103 - fraction_digits__94
                                        jp1313 = t1319
                                    }
                                    var jp1315 int
                                    if jp1301 {
                                        var t1317 int = fraction_digits__94 * 4
                                        var t1318 int = exponent__103 - t1317
                                        jp1315 = t1318
                                    } else {
                                        jp1315 = 0
                                    }
                                    var t1316 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1313,
                                        binary_exponent: jp1315,
                                        hexadecimal: jp1301,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1316
                                }
                            }
                        } else {
                            if jp1301 {
                                var t1386 ParsedFloat = invalid_parsed_float()
                                return t1386
                            } else {
                                var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1322 bool = index__85 != t1321
                                if t1322 {
                                    var t1323 ParsedFloat = invalid_parsed_float()
                                    return t1323
                                } else {
                                    if exponent_negative__104 {
                                        var t1320 int = 0 - exponent__103
                                        exponent__103 = t1320
                                    } else {}
                                    var jp1313 int
                                    if jp1301 {
                                        jp1313 = 0
                                    } else {
                                        var t1319 int = exponent__103 - fraction_digits__94
                                        jp1313 = t1319
                                    }
                                    var jp1315 int
                                    if jp1301 {
                                        var t1317 int = fraction_digits__94 * 4
                                        var t1318 int = exponent__103 - t1317
                                        jp1315 = t1318
                                    } else {
                                        jp1315 = 0
                                    }
                                    var t1316 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1313,
                                        binary_exponent: jp1315,
                                        hexadecimal: jp1301,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1316
                                }
                            }
                        }
                    }
                }
            } else {
                jp1301 = false
                if jp1301 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1445 int = compound_old145 + compound_value146
                    index__85 = t1445
                } else {}
                var mantissa_start__89 int = index__85
                var jp1304 int
                if jp1301 {
                    jp1304 = 16
                } else {
                    jp1304 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1398 uint32 = uint32(int(jp1304))
                Loop_loop1394__3:
                for {
                    var t1395 int
                    var inline2085 int = _goml_runtime_core_string_len(value__84)
                    t1395 = inline2085
                    var t1396 bool = index__85 < t1395
                    if t1396 {
                        var current__97 uint8
                        var inline2083 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2083
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1304)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1398)
                            var t1399 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1399)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1410 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1410
                            } else {}
                            var t1408 bool = significant_digits__95 > 0
                            var jp1405 bool
                            if t1408 {
                                jp1405 = true
                            } else {
                                var t1409 bool = x151 != 0
                                jp1405 = t1409
                            }
                            if jp1405 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1406 int = compound_old160 + compound_value161
                                significant_digits__95 = t1406
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1402 int = compound_old164 + compound_value165
                            index__85 = t1402
                            continue
                        } else {
                            var t1413 bool = current__97 == 95
                            if t1413 {
                                var t1434 int = index__85 + 1
                                var t1435 int
                                var inline2081 int = _goml_runtime_core_string_len(value__84)
                                t1435 = inline2081
                                var t1436 bool = t1434 >= t1435
                                if t1436 {
                                    var inline2073 FloatNatural = float_natural_zero()
                                    var inline2074 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2073,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2074
                                } else {
                                    var t1415 int = index__85 + 1
                                    var t1416 uint8
                                    var inline2079 uint8 = _goml_runtime_core_string_byte_get(value__84, t1415)
                                    t1416 = inline2079
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1416, jp1304)
                                    var x169 bool = mtmp168._0
                                    var jp1431 bool
                                    if jp1301 {
                                        var t1433 bool = !saw_digit__92
                                        jp1431 = t1433
                                    } else {
                                        jp1431 = false
                                    }
                                    var jp1418 bool
                                    if jp1431 {
                                        var t1432 bool = index__85 == mantissa_start__89
                                        jp1418 = t1432
                                    } else {
                                        jp1418 = false
                                    }
                                    var t1428 bool = !previous_digit__96
                                    var jp1426 bool
                                    if t1428 {
                                        var t1429 bool = !jp1418
                                        jp1426 = t1429
                                    } else {
                                        jp1426 = false
                                    }
                                    var jp1423 bool
                                    if jp1426 {
                                        jp1423 = true
                                    } else {
                                        var t1427 bool = !x169
                                        jp1423 = t1427
                                    }
                                    if jp1423 {
                                        var inline2076 FloatNatural = float_natural_zero()
                                        var inline2077 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2076,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2077
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1420 int = compound_old173 + compound_value174
                                        index__85 = t1420
                                        continue
                                    }
                                }
                            } else {
                                var t1443 bool = current__97 == 46
                                var jp1440 bool
                                if t1443 {
                                    var t1444 bool = !saw_dot__93
                                    jp1440 = t1444
                                } else {
                                    jp1440 = false
                                }
                                if jp1440 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1441 int = compound_old178 + compound_value179
                                    index__85 = t1441
                                    continue
                                } else {
                                    break Loop_loop1394__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1394__3
                    }
                }
                var t1392 bool = !saw_digit__92
                if t1392 {
                    var inline2087 FloatNatural = float_natural_zero()
                    var inline2088 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2087,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2088
                } else {
                    var jp1308 uint8
                    if jp1301 {
                        jp1308 = 112
                    } else {
                        jp1308 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1387 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1388 bool = index__85 < t1387
                    var jp1325 bool
                    if t1388 {
                        var t1389 uint8
                        var inline2090 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1389 = inline2090
                        var t1390 uint8 = ascii_lower(t1389)
                        var t1391 bool = t1390 == jp1308
                        jp1325 = t1391
                    } else {
                        jp1325 = false
                    }
                    if jp1325 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1326 int = compound_old183 + compound_value184
                        index__85 = t1326
                        var t1377 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1378 bool = index__85 < t1377
                        var jp1372 bool
                        if t1378 {
                            var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1382 bool = t1381 == 43
                            if t1382 {
                                jp1372 = true
                            } else {
                                var t1383 uint8
                                var inline2092 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1383 = inline2092
                                var t1384 bool = t1383 == 45
                                jp1372 = t1384
                            }
                        } else {
                            jp1372 = false
                        }
                        if jp1372 {
                            var t1373 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1374 bool = t1373 == 45
                            exponent_negative__104 = t1374
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1375 int = compound_old187 + compound_value188
                            index__85 = t1375
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1333__3:
                        for {
                            var t1334 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1335 bool = index__85 < t1334
                            if t1335 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1369 bool = current__106 >= 48
                                var jp1338 bool
                                if t1369 {
                                    var t1370 bool = current__106 <= 57
                                    jp1338 = t1370
                                } else {
                                    jp1338 = false
                                }
                                if jp1338 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1342 bool = exponent__103 < 1000000
                                    if t1342 {
                                        var t1343 int = exponent__103 * 10
                                        var t1344 uint8 = current__106 - 48
                                        var t1345 int = int(uint8(t1344))
                                        var t1346 int = t1343 + t1345
                                        exponent__103 = t1346
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1340 int = compound_old196 + compound_value197
                                    index__85 = t1340
                                    continue
                                } else {
                                    var t1348 bool = current__106 == 95
                                    if t1348 {
                                        var t1365 bool = !previous_digit__96
                                        var jp1361 bool
                                        if t1365 {
                                            jp1361 = true
                                        } else {
                                            var t1366 int = index__85 + 1
                                            var t1367 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1368 bool = t1366 >= t1367
                                            jp1361 = t1368
                                        }
                                        var jp1356 bool
                                        if jp1361 {
                                            jp1356 = true
                                        } else {
                                            var t1362 int = index__85 + 1
                                            var t1363 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1362)
                                            var t1364 bool = t1363 < 48
                                            jp1356 = t1364
                                        }
                                        var jp1353 bool
                                        if jp1356 {
                                            jp1353 = true
                                        } else {
                                            var t1357 int = index__85 + 1
                                            var t1358 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1357)
                                            var t1359 bool = t1358 > 57
                                            jp1353 = t1359
                                        }
                                        if jp1353 {
                                            var t1354 ParsedFloat = invalid_parsed_float()
                                            return t1354
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1350 int = compound_old201 + compound_value202
                                            index__85 = t1350
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1333__3
                                    }
                                }
                            } else {
                                break Loop_loop1333__3
                            }
                        }
                        var t1331 bool = !exponent_digits__105
                        if t1331 {
                            var t1332 ParsedFloat = invalid_parsed_float()
                            return t1332
                        } else {
                            var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1322 bool = index__85 != t1321
                            if t1322 {
                                var t1323 ParsedFloat = invalid_parsed_float()
                                return t1323
                            } else {
                                if exponent_negative__104 {
                                    var t1320 int = 0 - exponent__103
                                    exponent__103 = t1320
                                } else {}
                                var jp1313 int
                                if jp1301 {
                                    jp1313 = 0
                                } else {
                                    var t1319 int = exponent__103 - fraction_digits__94
                                    jp1313 = t1319
                                }
                                var jp1315 int
                                if jp1301 {
                                    var t1317 int = fraction_digits__94 * 4
                                    var t1318 int = exponent__103 - t1317
                                    jp1315 = t1318
                                } else {
                                    jp1315 = 0
                                }
                                var t1316 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1313,
                                    binary_exponent: jp1315,
                                    hexadecimal: jp1301,
                                    significant_digits: significant_digits__95,
                                }
                                return t1316
                            }
                        }
                    } else {
                        if jp1301 {
                            var t1386 ParsedFloat = invalid_parsed_float()
                            return t1386
                        } else {
                            var t1321 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1322 bool = index__85 != t1321
                            if t1322 {
                                var t1323 ParsedFloat = invalid_parsed_float()
                                return t1323
                            } else {
                                if exponent_negative__104 {
                                    var t1320 int = 0 - exponent__103
                                    exponent__103 = t1320
                                } else {}
                                var jp1313 int
                                if jp1301 {
                                    jp1313 = 0
                                } else {
                                    var t1319 int = exponent__103 - fraction_digits__94
                                    jp1313 = t1319
                                }
                                var jp1315 int
                                if jp1301 {
                                    var t1317 int = fraction_digits__94 * 4
                                    var t1318 int = exponent__103 - t1317
                                    jp1315 = t1318
                                } else {
                                    jp1315 = 0
                                }
                                var t1316 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1313,
                                    binary_exponent: jp1315,
                                    hexadecimal: jp1301,
                                    significant_digits: significant_digits__95,
                                }
                                return t1316
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
    var inline2094 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2094, 1)
    var inline2096 FloatNatural = FloatNatural{
        words: inline2094,
    }
    result__26 = inline2096
    var count__27 int = 0
    Loop_loop1484:
    for {
        var t1485 bool = count__27 < exponent__25
        if t1485 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1486 int = compound_old46 + compound_value47
            count__27 = t1486
            continue
        } else {
            break Loop_loop1484
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1573 bool
    var inline2098 *_goml_vec_uint32 = numerator__65.words
    var inline2099 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2098)
    t1573 = inline2099
    if t1573 {
        var t1574 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1574
    } else {
        var t1570 bool = binary_shift__67 >= 0
        var jp1495 FloatNatural
        if t1570 {
            var t1571 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1495 = t1571
        } else {
            var t1572 FloatNatural = float_natural_copy(numerator__65)
            jp1495 = t1572
        }
        var t1566 bool = binary_shift__67 >= 0
        var jp1497 FloatNatural
        if t1566 {
            var t1567 FloatNatural = float_natural_copy(denominator__66)
            jp1497 = t1567
        } else {
            var t1568 int = 0 - binary_shift__67
            var t1569 FloatNatural = float_natural_shift_left(denominator__66, t1568)
            jp1497 = t1569
        }
        var t1498 int = float_natural_bit_length(jp1495)
        var t1499 int = float_natural_bit_length(jp1497)
        var exponent__72 int = t1498 - t1499
        var t1560 bool = exponent__72 >= 0
        var jp1501 int
        if t1560 {
            var t1561 FloatNatural = float_natural_shift_left(jp1497, exponent__72)
            var t1562 int = float_natural_compare(jp1495, t1561)
            jp1501 = t1562
        } else {
            var t1563 int = 0 - exponent__72
            var t1564 FloatNatural = float_natural_shift_left(jp1495, t1563)
            var t1565 int = float_natural_compare(t1564, jp1497)
            jp1501 = t1565
        }
        var t1557 bool = jp1501 < 0
        if t1557 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1558 int = compound_old120 - compound_value121
            exponent__72 = t1558
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1551 bool = exponent__72 > exponent_bias__69
        if t1551 {
            var t1552 int = exponent_bias__69 + exponent_bias__69
            var t1553 int = t1552 + 1
            var t1554 uint64 = uint64(int(t1553))
            var t1555 uint64 = t1554 << mantissa_bits__68
            var t1556 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1555,
                _1: true,
            }
            return t1556
        } else {
            var t1546 bool = exponent__72 < minimum_exponent__74
            var jp1505 uint64
            if t1546 {
                var t1547 int = mantissa_bits__68 - minimum_exponent__74
                var t1548 uint64 = float_rational_quotient(jp1495, jp1497, t1547)
                jp1505 = t1548
            } else {
                var t1549 int = mantissa_bits__68 - exponent__72
                var t1550 uint64 = float_rational_quotient(jp1495, jp1497, t1549)
                jp1505 = t1550
            }
            var mantissa__76 uint64 = jp1505
            var t1508 bool = exponent__72 < minimum_exponent__74
            if t1508 {
                var t1511 bool = mantissa__76 == 0
                if t1511 {
                    var t1512 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1512
                } else {
                    var t1515_lhs uint64 = 1
                    var t1515 uint64 = t1515_lhs << mantissa_bits__68
                    var t1516 bool = mantissa__76 >= t1515
                    if t1516 {
                        var t1517_lhs uint64 = 1
                        var t1517 uint64 = t1517_lhs << mantissa_bits__68
                        var t1518_lhs uint64 = 1
                        var t1518 uint64 = t1518_lhs << mantissa_bits__68
                        var t1519 uint64 = mantissa__76 - t1518
                        var t1520 uint64 = t1517 | t1519
                        var t1521 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1520,
                            _1: false,
                        }
                        return t1521
                    } else {
                        var t1522 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1522
                    }
                }
            } else {
                var t1539 int = mantissa_bits__68 + 1
                var t1540_lhs uint64 = 1
                var t1540 uint64 = t1540_lhs << t1539
                var t1541 bool = mantissa__76 >= t1540
                if t1541 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1542 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1542
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1544 int = compound_old128 + compound_value129
                    exponent__72 = t1544
                } else {}
                var t1526 bool = exponent__72 > exponent_bias__69
                if t1526 {
                    var t1527 int = exponent_bias__69 + exponent_bias__69
                    var t1528 int = t1527 + 1
                    var t1529 uint64 = uint64(int(t1528))
                    var t1530 uint64 = t1529 << mantissa_bits__68
                    var t1531 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1530,
                        _1: true,
                    }
                    return t1531
                } else {
                    var t1532 int = exponent__72 + exponent_bias__69
                    var t1533 uint64 = uint64(int(t1532))
                    var t1534 uint64 = t1533 << mantissa_bits__68
                    var t1535_lhs uint64 = 1
                    var t1535 uint64 = t1535_lhs << mantissa_bits__68
                    var t1536 uint64 = mantissa__76 - t1535
                    var t1537 uint64 = t1534 | t1536
                    var t1538 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1537,
                        _1: false,
                    }
                    return t1538
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1579 int = vec_len__Vec_6uint32(self__528)
    var t1580 bool = t1579 == 0
    return t1580
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1583:
    for {
        var t1591 *_goml_vec_uint32 = value__7.words
        var t1592 bool
        var inline2110 int = vec_len__Vec_6uint32(t1591)
        var inline2111 bool = inline2110 == 0
        t1592 = inline2111
        var t1593 bool = !t1592
        var jp1585 bool
        if t1593 {
            var t1594 *_goml_vec_uint32 = value__7.words
            var t1595 *_goml_vec_uint32 = value__7.words
            var t1596 int
            var inline2104 int = vec_len__Vec_6uint32(t1595)
            t1596 = inline2104
            var t1597 int = t1596 - 1
            var t1598 uint32 = vec_get__Vec_6uint32(t1594, t1597)
            var t1599 bool = t1598 == 0
            jp1585 = t1599
        } else {
            jp1585 = false
        }
        if jp1585 {
            var t1586 *_goml_vec_uint32 = value__7.words
            var t1587 *_goml_vec_uint32 = value__7.words
            var t1588 int
            var inline2108 int = vec_len__Vec_6uint32(t1587)
            t1588 = inline2108
            var t1589 int = t1588 - 1
            vec_truncate__Vec_6uint32(t1586, t1589)
            continue
        } else {
            break Loop_loop1583
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1608 bool = string_is_char_boundary(value__274, start__275)
    var jp1605 bool
    if t1608 {
        var t1609 bool = string_is_char_boundary(value__274, end__276)
        jp1605 = t1609
    } else {
        jp1605 = false
    }
    if jp1605 {
        var t1606 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1606
    } else {
        var t1607 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1607
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1624 int
    var inline2128 int = _goml_runtime_core_string_len(value__78)
    t1624 = inline2128
    var t1625 int
    var inline2126 int = _goml_runtime_core_string_len(expected__79)
    t1625 = inline2126
    var t1626 bool = t1624 != t1625
    if t1626 {
        return false
    } else {
        var index__80 int = 0
        var inline2118 uint8 = 97 - 65
        Loop_loop1614:
        for {
            var t1615 int
            var inline2124 int = _goml_runtime_core_string_len(value__78)
            t1615 = inline2124
            var t1616 bool = index__80 < t1615
            if t1616 {
                var t1620 uint8
                var inline2122 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1620 = inline2122
                var t1621 uint8
                var inline2115 bool = t1620 >= 65
                var inline2117 bool
                if inline2115 {
                    var inline2120 bool = t1620 <= 90
                    inline2117 = inline2120
                } else {
                    inline2117 = false
                }
                if inline2117 {
                    var inline2119 uint8 = t1620 + inline2118
                    t1621 = inline2119
                    var t1622 uint8
                    var inline2113 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1622 = inline2113
                    var t1623 bool = t1621 != t1622
                    if t1623 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1618 int = compound_old134 + compound_value135
                        index__80 = t1618
                        continue
                    }
                } else {
                    t1621 = t1620
                    var t1622 uint8
                    var inline2113 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1622 = inline2113
                    var t1623 bool = t1621 != t1622
                    if t1623 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1618 int = compound_old134 + compound_value135
                        index__80 = t1618
                        continue
                    }
                }
            } else {
                break Loop_loop1614
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1635 bool = value__77 >= 65
    var jp1632 bool
    if t1635 {
        var t1636 bool = value__77 <= 90
        jp1632 = t1636
    } else {
        jp1632 = false
    }
    if jp1632 {
        var t1633 uint8 = 97 - 65
        var t1634 uint8 = value__77 + t1633
        return t1634
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1663 bool = value__81 >= 48
    var jp1647 bool
    if t1663 {
        var t1664 bool = value__81 <= 57
        jp1647 = t1664
    } else {
        jp1647 = false
    }
    var jp1640 int
    if jp1647 {
        var t1648 uint8 = value__81 - 48
        var t1649 int = int(uint8(t1648))
        jp1640 = t1649
        var t1643 bool = jp1640 < base__82
        if t1643 {
            var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1640,
            }
            return t1644
        } else {
            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1645
        }
    } else {
        var t1659 uint8
        var inline2144 bool = value__81 >= 65
        var inline2146 bool
        if inline2144 {
            var inline2149 bool = value__81 <= 90
            inline2146 = inline2149
        } else {
            inline2146 = false
        }
        if inline2146 {
            var inline2147 uint8 = 97 - 65
            var inline2148 uint8 = value__81 + inline2147
            t1659 = inline2148
            var t1660 bool = t1659 >= 97
            var jp1653 bool
            if t1660 {
                var t1661 uint8
                var inline2130 bool = value__81 >= 65
                var inline2132 bool
                if inline2130 {
                    var inline2135 bool = value__81 <= 90
                    inline2132 = inline2135
                } else {
                    inline2132 = false
                }
                if inline2132 {
                    var inline2133 uint8 = 97 - 65
                    var inline2134 uint8 = value__81 + inline2133
                    t1661 = inline2134
                    var t1662 bool = t1661 <= 102
                    jp1653 = t1662
                    if jp1653 {
                        var t1654 uint8
                        var inline2137 bool = value__81 >= 65
                        var inline2139 bool
                        if inline2137 {
                            var inline2142 bool = value__81 <= 90
                            inline2139 = inline2142
                        } else {
                            inline2139 = false
                        }
                        if inline2139 {
                            var inline2140 uint8 = 97 - 65
                            var inline2141 uint8 = value__81 + inline2140
                            t1654 = inline2141
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        } else {
                            t1654 = value__81
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        }
                    } else {
                        var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1658
                    }
                } else {
                    t1661 = value__81
                    var t1662 bool = t1661 <= 102
                    jp1653 = t1662
                    if jp1653 {
                        var t1654 uint8
                        var inline2137 bool = value__81 >= 65
                        var inline2139 bool
                        if inline2137 {
                            var inline2142 bool = value__81 <= 90
                            inline2139 = inline2142
                        } else {
                            inline2139 = false
                        }
                        if inline2139 {
                            var inline2140 uint8 = 97 - 65
                            var inline2141 uint8 = value__81 + inline2140
                            t1654 = inline2141
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        } else {
                            t1654 = value__81
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        }
                    } else {
                        var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1658
                    }
                }
            } else {
                jp1653 = false
                if jp1653 {
                    var t1654 uint8
                    var inline2137 bool = value__81 >= 65
                    var inline2139 bool
                    if inline2137 {
                        var inline2142 bool = value__81 <= 90
                        inline2139 = inline2142
                    } else {
                        inline2139 = false
                    }
                    if inline2139 {
                        var inline2140 uint8 = 97 - 65
                        var inline2141 uint8 = value__81 + inline2140
                        t1654 = inline2141
                        var t1655 uint8 = t1654 - 97
                        var t1656 uint8 = t1655 + 10
                        var t1657 int = int(uint8(t1656))
                        jp1640 = t1657
                        var t1643 bool = jp1640 < base__82
                        if t1643 {
                            var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1640,
                            }
                            return t1644
                        } else {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1645
                        }
                    } else {
                        t1654 = value__81
                        var t1655 uint8 = t1654 - 97
                        var t1656 uint8 = t1655 + 10
                        var t1657 int = int(uint8(t1656))
                        jp1640 = t1657
                        var t1643 bool = jp1640 < base__82
                        if t1643 {
                            var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1640,
                            }
                            return t1644
                        } else {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1645
                        }
                    }
                } else {
                    var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1658
                }
            }
        } else {
            t1659 = value__81
            var t1660 bool = t1659 >= 97
            var jp1653 bool
            if t1660 {
                var t1661 uint8
                var inline2130 bool = value__81 >= 65
                var inline2132 bool
                if inline2130 {
                    var inline2135 bool = value__81 <= 90
                    inline2132 = inline2135
                } else {
                    inline2132 = false
                }
                if inline2132 {
                    var inline2133 uint8 = 97 - 65
                    var inline2134 uint8 = value__81 + inline2133
                    t1661 = inline2134
                    var t1662 bool = t1661 <= 102
                    jp1653 = t1662
                    if jp1653 {
                        var t1654 uint8
                        var inline2137 bool = value__81 >= 65
                        var inline2139 bool
                        if inline2137 {
                            var inline2142 bool = value__81 <= 90
                            inline2139 = inline2142
                        } else {
                            inline2139 = false
                        }
                        if inline2139 {
                            var inline2140 uint8 = 97 - 65
                            var inline2141 uint8 = value__81 + inline2140
                            t1654 = inline2141
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        } else {
                            t1654 = value__81
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        }
                    } else {
                        var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1658
                    }
                } else {
                    t1661 = value__81
                    var t1662 bool = t1661 <= 102
                    jp1653 = t1662
                    if jp1653 {
                        var t1654 uint8
                        var inline2137 bool = value__81 >= 65
                        var inline2139 bool
                        if inline2137 {
                            var inline2142 bool = value__81 <= 90
                            inline2139 = inline2142
                        } else {
                            inline2139 = false
                        }
                        if inline2139 {
                            var inline2140 uint8 = 97 - 65
                            var inline2141 uint8 = value__81 + inline2140
                            t1654 = inline2141
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        } else {
                            t1654 = value__81
                            var t1655 uint8 = t1654 - 97
                            var t1656 uint8 = t1655 + 10
                            var t1657 int = int(uint8(t1656))
                            jp1640 = t1657
                            var t1643 bool = jp1640 < base__82
                            if t1643 {
                                var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1640,
                                }
                                return t1644
                            } else {
                                var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1645
                            }
                        }
                    } else {
                        var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1658
                    }
                }
            } else {
                jp1653 = false
                if jp1653 {
                    var t1654 uint8
                    var inline2137 bool = value__81 >= 65
                    var inline2139 bool
                    if inline2137 {
                        var inline2142 bool = value__81 <= 90
                        inline2139 = inline2142
                    } else {
                        inline2139 = false
                    }
                    if inline2139 {
                        var inline2140 uint8 = 97 - 65
                        var inline2141 uint8 = value__81 + inline2140
                        t1654 = inline2141
                        var t1655 uint8 = t1654 - 97
                        var t1656 uint8 = t1655 + 10
                        var t1657 int = int(uint8(t1656))
                        jp1640 = t1657
                        var t1643 bool = jp1640 < base__82
                        if t1643 {
                            var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1640,
                            }
                            return t1644
                        } else {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1645
                        }
                    } else {
                        t1654 = value__81
                        var t1655 uint8 = t1654 - 97
                        var t1656 uint8 = t1655 + 10
                        var t1657 int = int(uint8(t1656))
                        jp1640 = t1657
                        var t1643 bool = jp1640 < base__82
                        if t1643 {
                            var t1644 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1640,
                            }
                            return t1644
                        } else {
                            var t1645 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1645
                        }
                    }
                } else {
                    var t1658 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1658
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1667:
    for {
        var t1668 bool = carry__22 != 0
        if t1668 {
            var t1677 *_goml_vec_uint32 = value__20.words
            var t1678 int
            var inline2154 int = vec_len__Vec_6uint32(t1677)
            t1678 = inline2154
            var t1679 bool = index__23 == t1678
            if t1679 {
                var t1680 *_goml_vec_uint32 = value__20.words
                var inline2151 uint32 = 0
                vec_push__Vec_6uint32(t1680, inline2151)
            } else {}
            var t1670 *_goml_vec_uint32 = value__20.words
            var t1671 uint32 = vec_get__Vec_6uint32(t1670, index__23)
            var t1672 uint64 = uint64(uint32(t1671))
            var sum__24 uint64 = t1672 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1674_rhs int = 32
            var t1674 uint64 = sum__24 >> t1674_rhs
            carry__22 = t1674
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1675 int = compound_old42 + compound_value43
            index__23 = t1675
            continue
        } else {
            break Loop_loop1667
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1684 FloatNatural
    var inline2156 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2157 FloatNatural = FloatNatural{
        words: inline2156,
    }
    t1684 = inline2157
    var t1685 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1684,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1685
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1705 *_goml_vec_uint32 = value__9.words
    var t1706 bool
    var inline2163 int = vec_len__Vec_6uint32(t1705)
    var inline2164 bool = inline2163 == 0
    t1706 = inline2164
    if t1706 {
        return 0
    } else {
        var t1689 *_goml_vec_uint32 = value__9.words
        var t1690 *_goml_vec_uint32 = value__9.words
        var t1691 int
        var inline2161 int = vec_len__Vec_6uint32(t1690)
        t1691 = inline2161
        var t1692 int = t1691 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1689, t1692)
        var bits__11 int = 0
        Loop_loop1699:
        for {
            var t1700 bool = high__10 != 0
            if t1700 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1701 uint32 = compound_old9 >> compound_value10
                high__10 = t1701
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1703 int = compound_old12 + compound_value13
                bits__11 = t1703
                continue
            } else {
                break Loop_loop1699
            }
        }
        var t1694 *_goml_vec_uint32 = value__9.words
        var t1695 int
        var inline2159 int = vec_len__Vec_6uint32(t1694)
        t1695 = inline2159
        var t1696 int = t1695 - 1
        var t1697 int = t1696 * 32
        var t1698 int = t1697 + bits__11
        return t1698
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1728 *_goml_vec_uint32 = left__12.words
    var t1729 int
    var inline2174 int = vec_len__Vec_6uint32(t1728)
    t1729 = inline2174
    var t1730 *_goml_vec_uint32 = right__13.words
    var t1731 int
    var inline2172 int = vec_len__Vec_6uint32(t1730)
    t1731 = inline2172
    var t1732 bool = t1729 < t1731
    if t1732 {
        return -1
    } else {
        var t1734 *_goml_vec_uint32 = left__12.words
        var t1735 int
        var inline2168 int = vec_len__Vec_6uint32(t1734)
        t1735 = inline2168
        var t1736 *_goml_vec_uint32 = right__13.words
        var t1737 int
        var inline2166 int = vec_len__Vec_6uint32(t1736)
        t1737 = inline2166
        var t1738 bool = t1735 > t1737
        if t1738 {
            return 1
        } else {
            var t1710 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2170 int = vec_len__Vec_6uint32(t1710)
            index__14 = inline2170
            Loop_loop1712:
            for {
                var t1713 bool = index__14 > 0
                if t1713 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1714 int = compound_old17 - compound_value18
                    index__14 = t1714
                    var t1717 *_goml_vec_uint32 = left__12.words
                    var t1718 uint32 = vec_get__Vec_6uint32(t1717, index__14)
                    var t1719 *_goml_vec_uint32 = right__13.words
                    var t1720 uint32 = vec_get__Vec_6uint32(t1719, index__14)
                    var t1721 bool = t1718 < t1720
                    if t1721 {
                        return -1
                    } else {
                        var t1723 *_goml_vec_uint32 = left__12.words
                        var t1724 uint32 = vec_get__Vec_6uint32(t1723, index__14)
                        var t1725 *_goml_vec_uint32 = right__13.words
                        var t1726 uint32 = vec_get__Vec_6uint32(t1725, index__14)
                        var t1727 bool = t1724 > t1726
                        if t1727 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1712
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1774 bool = shift__57 >= 0
    var jp1742 FloatNatural
    if t1774 {
        var t1775 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1742 = t1775
    } else {
        var t1776 FloatNatural = float_natural_copy(numerator__55)
        jp1742 = t1776
    }
    var t1770 bool = shift__57 >= 0
    var jp1744 FloatNatural
    if t1770 {
        var t1771 FloatNatural = float_natural_copy(denominator__56)
        jp1744 = t1771
    } else {
        var t1772 int = 0 - shift__57
        var t1773 FloatNatural = float_natural_shift_left(denominator__56, t1772)
        jp1744 = t1773
    }
    var quotient__60 uint64 = 0
    Loop_loop1757:
    for {
        var t1758 int = float_natural_compare(jp1742, jp1744)
        var t1759 bool = t1758 >= 0
        if t1759 {
            var t1760 int = float_natural_bit_length(jp1742)
            var t1761 int = float_natural_bit_length(jp1744)
            var offset__61 int = t1760 - t1761
            var part__62 FloatNatural = float_natural_shift_left(jp1744, offset__61)
            var t1765 int = float_natural_compare(jp1742, part__62)
            var t1766 bool = t1765 < 0
            if t1766 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1767 int = compound_old105 - compound_value106
                offset__61 = t1767
                var t1769 FloatNatural = float_natural_shift_left(jp1744, offset__61)
                part__62 = t1769
            } else {}
            float_natural_subtract(jp1742, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1763 uint64 = compound_old111 | compound_value112
            quotient__60 = t1763
            continue
        } else {
            break Loop_loop1757
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1742, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1744)
    var t1751 bool = rounding__64 > 0
    var jp1748 bool
    if t1751 {
        jp1748 = true
    } else {
        var t1754 bool = rounding__64 == 0
        if t1754 {
            var t1755_rhs uint64 = 1
            var t1755 uint64 = quotient__60 & t1755_rhs
            var t1756 bool = t1755 == 1
            jp1748 = t1756
        } else {
            jp1748 = false
        }
    }
    if jp1748 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1749 uint64 = compound_old115 + compound_value116
        quotient__60 = t1749
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1792 bool = index__269 < 0
    var jp1784 bool
    if t1792 {
        jp1784 = true
    } else {
        var t1793 int
        var inline2176 int = _goml_runtime_core_string_len(value__268)
        t1793 = inline2176
        var t1794 bool = index__269 > t1793
        jp1784 = t1794
    }
    if jp1784 {
        return false
    } else {
        var t1787 int
        var inline2180 int = _goml_runtime_core_string_len(value__268)
        t1787 = inline2180
        var t1788 bool = index__269 == t1787
        if t1788 {
            return true
        } else {
            var t1789 uint8
            var inline2178 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1789 = inline2178
            var t1790_rhs uint8 = 192
            var t1790 uint8 = t1789 & t1790_rhs
            var t1791 bool = t1790 != 128
            return t1791
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1798:
    for {
        var t1799 *_goml_vec_uint32 = value__37.words
        var t1800 int
        var inline2184 int = vec_len__Vec_6uint32(t1799)
        t1800 = inline2184
        var t1801 bool = index__41 < t1800
        if t1801 {
            var t1815 *_goml_vec_uint32 = other__38.words
            var t1816 int
            var inline2182 int = vec_len__Vec_6uint32(t1815)
            t1816 = inline2182
            var t1817 bool = index__41 < t1816
            var jp1803 uint64
            if t1817 {
                var t1818 *_goml_vec_uint32 = other__38.words
                var t1819 uint32 = vec_get__Vec_6uint32(t1818, index__41)
                var t1820 uint64 = uint64(uint32(t1819))
                jp1803 = t1820
            } else {
                jp1803 = 0
            }
            var right__42 uint64 = jp1803 + borrow__40
            var t1804 *_goml_vec_uint32 = value__37.words
            var t1805 uint32 = vec_get__Vec_6uint32(t1804, index__41)
            var left__43 uint64 = uint64(uint32(t1805))
            var t1809 bool = left__43 >= right__42
            if t1809 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1810 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1810))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1812 uint64 = base__39 + left__43
                var t1813 uint64 = t1812 - right__42
                var value75 uint32 = uint32(uint64(t1813))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1807 int = compound_old79 + compound_value80
            index__41 = t1807
            continue
        } else {
            break Loop_loop1798
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
