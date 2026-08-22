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

func _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(arg0 float32) uint32 {
    return _goml_ffi_import_math_h0cea0c730c0e331b7d5cc103b112df29.Float32bits(arg0)
}

func array_get__Array_3_5uint8(arr [3]uint8, index int) uint8 {
    return arr[index]
}

func array_get__Array_2_7float32(arr [2]float32, index int) float32 {
    return arr[index]
}

func array_get__Array_2_5int64(arr [2]int64, index int) int64 {
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
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
    var arr__0 [3]uint8 = [3]uint8{10, 20, 30}
    var i__1 *ref_int_x
    var inline1901 int = 0
    var inline1902 *ref_int_x = ref__Ref_3int(inline1901)
    i__1 = inline1902
    Loop_loop813:
    for {
        var t814 int
        var inline1879 int = ref_get__Ref_3int(i__1)
        t814 = inline1879
        var t815 bool = t814 < 3
        if t815 {
            var t816 int
            var inline1877 int = ref_get__Ref_3int(i__1)
            t816 = inline1877
            var t817 uint8 = array_get__Array_3_5uint8(arr__0, t816)
            var t818 string
            var inline1875 string = __goml_builtin_uint8_to_string(t817)
            t818 = inline1875
            var inline1872 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
            _goml_runtime_core_string_println(inline1872)
            var t819 int
            var inline1870 int = ref_get__Ref_3int(i__1)
            t819 = inline1870
            var t820 int = t819 + 1
            ref_set__Ref_3int(i__1, t820)
            continue
        } else {
            break Loop_loop813
        }
    }
    var floats__2 [2]float32 = [2]float32{1.5, 2.5}
    var t805 float32 = array_get__Array_2_7float32(floats__2, 0)
    var t806 string
    var inline1899 string = __goml_builtin_float32_to_string(t805)
    t806 = inline1899
    var inline1896 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline1896)
    var t807 float32 = array_get__Array_2_7float32(floats__2, 1)
    var t808 string
    var inline1894 string = __goml_builtin_float32_to_string(t807)
    t808 = inline1894
    var inline1891 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline1891)
    var longs__3 [2]int64 = [2]int64{100, 200}
    var t809 int64 = array_get__Array_2_5int64(longs__3, 0)
    var t810 string
    var inline1889 string = __goml_builtin_int64_to_string(t809)
    t810 = inline1889
    var inline1886 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline1886)
    var t811 int64 = array_get__Array_2_5int64(longs__3, 1)
    var t812 string
    var inline1884 string = __goml_builtin_int64_to_string(t811)
    t812 = inline1884
    var inline1881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline1881)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t845 uint64 = uint64(uint8(value__228))
    var t846 string = decimal_string(t845)
    return t846
}

func __goml_builtin_float32_to_string(value__194 float32) string {
    var t849 uint32 = _goml_ffi_math_x00_Float32bits_x0__q__m__z_u32_hbbf8d280343f673a9e2fd959393f1495(value__194)
    var t850 uint64 = uint64(uint32(t849))
    var t851 string = format_float_bits(t850, 23, 8, 127)
    return t851
}

func __goml_builtin_int64_to_string(value__226 int64) string {
    var inline1914 bool = value__226 < 0
    if inline1914 {
        var inline1915 uint64 = uint64(int64(value__226))
        var inline1916 uint64 = 0 - inline1915
        var inline1917 string = decimal_string(inline1916)
        var inline1918 string = "-" + inline1917
        return inline1918
    } else {
        var inline1919 uint64 = uint64(int64(value__226))
        var inline1920 string = decimal_string(inline1919)
        return inline1920
    }
}

func decimal_string(value__208 uint64) string {
    var t877 bool = value__208 == 0
    if t877 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop870:
        for {
            var t871 bool = remaining__210 > 0
            if t871 {
                var t872_rhs uint64 = 10
                var t872 uint64 = remaining__210 % t872_rhs
                var t873 uint8 = uint8(uint64(t872))
                var t874 uint8 = t873 + 48
                vec_push__Vec_5uint8(reversed__209, t874)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t875 uint64 = compound_old353 / compound_value354
                remaining__210 = t875
                continue
            } else {
                break Loop_loop870
            }
        }
        var t859 int
        var inline1930 int = vec_len__Vec_5uint8(reversed__209)
        t859 = inline1930
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t859)
        var offset__212 int = 0
        Loop_loop861:
        for {
            var t862 int
            var inline1928 int = vec_len__Vec_5uint8(reversed__209)
            t862 = inline1928
            var t863 bool = offset__212 < t862
            if t863 {
                var t864 int
                var inline1926 int = vec_len__Vec_5uint8(reversed__209)
                t864 = inline1926
                var t865 int = t864 - offset__212
                var t866 int = t865 - 1
                var t867 uint8 = vec_get__Vec_5uint8(reversed__209, t866)
                vec_push__Vec_5uint8(bytes__211, t867)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t868 int = compound_old358 + compound_value359
                offset__212 = t868
                continue
            } else {
                break Loop_loop861
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func format_float_bits(bits__160 uint64, mantissa_bits__161 int, exponent_bits__162 int, exponent_bias__163 int) string {
    var t880 int = mantissa_bits__161 + exponent_bits__162
    var sign_mask__164_lhs uint64 = 1
    var sign_mask__164 uint64 = sign_mask__164_lhs << t880
    var t881 uint64 = bits__160 & sign_mask__164
    var negative__165 bool = t881 != 0
    var t882_lhs uint64 = 1
    var t882 uint64 = t882_lhs << exponent_bits__162
    var exponent_mask__166 uint64 = t882 - 1
    var t883 uint64 = bits__160 >> mantissa_bits__161
    var exponent__167 uint64 = t883 & exponent_mask__166
    var t884_lhs uint64 = 1
    var t884 uint64 = t884_lhs << mantissa_bits__161
    var t885 uint64 = t884 - 1
    var fraction__168 uint64 = bits__160 & t885
    var t949 bool = exponent__167 == exponent_mask__166
    if t949 {
        var t951 bool = fraction__168 == 0
        if t951 {
            if negative__165 {
                return "-inf"
            } else {
                return "inf"
            }
        } else {
            return "NaN"
        }
    } else {
        var t957 bool = exponent__167 == 0
        var jp955 bool
        if t957 {
            var t958 bool = fraction__168 == 0
            jp955 = t958
        } else {
            jp955 = false
        }
        if jp955 {
            if negative__165 {
                return "-0"
            } else {
                return "0"
            }
        } else {
            var t946 bool = exponent__167 == 0
            var jp888 uint64
            if t946 {
                jp888 = fraction__168
            } else {
                var t947_lhs uint64 = 1
                var t947 uint64 = t947_lhs << mantissa_bits__161
                var t948 uint64 = fraction__168 | t947
                jp888 = t948
            }
            var t940 bool = exponent__167 == 0
            var jp890 int
            if t940 {
                var t941 int = 1 - exponent_bias__163
                var t942 int = t941 - mantissa_bits__161
                jp890 = t942
            } else {
                var t943 int = int(uint64(exponent__167))
                var t944 int = t943 - exponent_bias__163
                var t945 int = t944 - mantissa_bits__161
                jp890 = t945
            }
            var exact_value__171 FloatNatural = float_natural_from_u64(jp888)
            var t895 bool = jp890 >= 0
            var jp892 int
            if t895 {
                var shifted__172 FloatNatural = float_natural_shift_left(exact_value__171, jp890)
                var digits__173 string = float_natural_decimal(shifted__172)
                var t914 bool = mantissa_bits__161 == 23
                var jp897 int
                if t914 {
                    jp897 = 9
                } else {
                    jp897 = 17
                }
                var t911 int
                var inline1938 int = _goml_runtime_core_string_len(digits__173)
                t911 = inline1938
                var t912 bool = t911 < jp897
                var jp899 int
                if t912 {
                    var inline1932 int = _goml_runtime_core_string_len(digits__173)
                    jp899 = inline1932
                } else {
                    jp899 = jp897
                }
                var count__176 int = 1
                Loop_loop902:
                for {
                    var t903 bool = count__176 <= jp899
                    if t903 {
                        var mtmp317 Tuple2_6string_4bool = rounded_float_digits(digits__173, count__176)
                        var x318 string = mtmp317._0
                        var x319 bool = mtmp317._1
                        var rounded__179 string = trim_float_digits(x318)
                        var t904 int
                        var inline1934 int = _goml_runtime_core_string_len(digits__173)
                        t904 = inline1934
                        var jp906 int
                        if x319 {
                            jp906 = 1
                        } else {
                            jp906 = 0
                        }
                        var point__180 int = t904 + jp906
                        var candidate__181 string = fixed_float_text(rounded__179, point__180, negative__165)
                        var mtmp320 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__181, mantissa_bits__161, exponent_bias__163)
                        var x322 uint64 = mtmp320._1
                        var t910 bool = x322 == bits__160
                        if t910 {
                            return candidate__181
                        } else {
                            var compound_old324 int = count__176
                            var compound_value325 int = 1
                            var t908 int = compound_old324 + compound_value325
                            count__176 = t908
                            continue
                        }
                    } else {
                        break Loop_loop902
                    }
                }
                var inline1936 int = _goml_runtime_core_string_len(digits__173)
                jp892 = inline1936
                var t893 string = float_natural_decimal(exact_value__171)
                var t894 string = fixed_float_text(t893, jp892, negative__165)
                return t894
            } else {
                var count__183 int = 0
                var t936 int = 0 - jp890
                Loop_loop935:
                for {
                    var t937 bool = count__183 < t936
                    if t937 {
                        float_natural_multiply_small(exact_value__171, 5)
                        var compound_old329 int = count__183
                        var compound_value330 int = 1
                        var t938 int = compound_old329 + compound_value330
                        count__183 = t938
                        continue
                    } else {
                        break Loop_loop935
                    }
                }
                var digits__184 string = float_natural_decimal(exact_value__171)
                var t916 int
                var inline1944 int = _goml_runtime_core_string_len(digits__184)
                t916 = inline1944
                var point__185 int = t916 + jp890
                var t934 bool = mantissa_bits__161 == 23
                var jp918 int
                if t934 {
                    jp918 = 9
                } else {
                    jp918 = 17
                }
                var t931 int
                var inline1942 int = _goml_runtime_core_string_len(digits__184)
                t931 = inline1942
                var t932 bool = t931 < jp918
                var jp920 int
                if t932 {
                    var inline1940 int = _goml_runtime_core_string_len(digits__184)
                    jp920 = inline1940
                } else {
                    jp920 = jp918
                }
                count__183 = 1
                Loop_loop922:
                for {
                    var t923 bool = count__183 <= jp920
                    if t923 {
                        var mtmp334 Tuple2_6string_4bool = rounded_float_digits(digits__184, count__183)
                        var x335 string = mtmp334._0
                        var x336 bool = mtmp334._1
                        var rounded__190 string = trim_float_digits(x335)
                        var jp925 int
                        if x336 {
                            jp925 = 1
                        } else {
                            jp925 = 0
                        }
                        var t926 int = point__185 + jp925
                        var candidate__191 string = fixed_float_text(rounded__190, t926, negative__165)
                        var mtmp337 Tuple2_4bool_6uint64 = parsed_float_bits(candidate__191, mantissa_bits__161, exponent_bias__163)
                        var x339 uint64 = mtmp337._1
                        var t930 bool = x339 == bits__160
                        if t930 {
                            return candidate__191
                        } else {
                            var compound_old341 int = count__183
                            var compound_value342 int = 1
                            var t928 int = compound_old341 + compound_value342
                            count__183 = t928
                            continue
                        }
                    } else {
                        break Loop_loop922
                    }
                }
                jp892 = point__185
                var t893 string = float_natural_decimal(exact_value__171)
                var t894 string = fixed_float_text(t893, jp892, negative__165)
                return t894
            }
        }
    }
}

func float_natural_from_u64(value__1 uint64) FloatNatural {
    var result__2 FloatNatural
    var inline1950 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline1951 FloatNatural = FloatNatural{
        words: inline1950,
    }
    result__2 = inline1951
    var t978 bool = value__1 != 0
    if t978 {
        var t979 *_goml_vec_uint32 = result__2.words
        var t980 uint32 = uint32(uint64(value__1))
        vec_push__Vec_6uint32(t979, t980)
        var t981_rhs int = 32
        var t981 uint64 = value__1 >> t981_rhs
        var high__3 uint32 = uint32(uint64(t981))
        var t983 bool = high__3 != 0
        if t983 {
            var t984 *_goml_vec_uint32 = result__2.words
            vec_push__Vec_6uint32(t984, high__3)
        } else {}
    } else {}
    return result__2
}

func float_natural_shift_left(value__28 FloatNatural, bits__29 int) FloatNatural {
    var t1013 bool
    var inline1968 *_goml_vec_uint32 = value__28.words
    var inline1969 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1968)
    t1013 = inline1969
    if t1013 {
        var inline1953 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline1954 FloatNatural = FloatNatural{
            words: inline1953,
        }
        return inline1954
    } else {
        var t1016 bool = bits__29 == 0
        if t1016 {
            var t1017 FloatNatural = float_natural_copy(value__28)
            return t1017
        } else {
            var result__30 FloatNatural
            var inline1965 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline1966 FloatNatural = FloatNatural{
                words: inline1965,
            }
            result__30 = inline1966
            var word_shift__31 int = bits__29 / 32
            var bit_shift__32_rhs int = 32
            var bit_shift__32 int = bits__29 % bit_shift__32_rhs
            var index__33 int = 0
            Loop_loop1008:
            for {
                var t1009 bool = index__33 < word_shift__31
                if t1009 {
                    var t1010 *_goml_vec_uint32 = result__30.words
                    var inline1956 uint32 = 0
                    vec_push__Vec_6uint32(t1010, inline1956)
                    var compound_old52 int = index__33
                    var compound_value53 int = 1
                    var t1011 int = compound_old52 + compound_value53
                    index__33 = t1011
                    continue
                } else {
                    break Loop_loop1008
                }
            }
            var carry__34 uint64 = 0
            index__33 = 0
            Loop_loop996:
            for {
                var t997 *_goml_vec_uint32 = value__28.words
                var t998 int
                var inline1961 int = vec_len__Vec_6uint32(t997)
                t998 = inline1961
                var t999 bool = index__33 < t998
                if t999 {
                    var t1000 *_goml_vec_uint32 = value__28.words
                    var word__35 uint32 = vec_get__Vec_6uint32(t1000, index__33)
                    var t1001 uint64 = uint64(uint32(word__35))
                    var t1002 uint64 = t1001 << bit_shift__32
                    var shifted__36 uint64 = t1002 | carry__34
                    var t1003 *_goml_vec_uint32 = result__30.words
                    var t1004 uint32 = uint32(uint64(shifted__36))
                    vec_push__Vec_6uint32(t1003, t1004)
                    var t1005_rhs int = 32
                    var t1005 uint64 = shifted__36 >> t1005_rhs
                    carry__34 = t1005
                    var compound_old59 int = index__33
                    var compound_value60 int = 1
                    var t1006 int = compound_old59 + compound_value60
                    index__33 = t1006
                    continue
                } else {
                    break Loop_loop996
                }
            }
            var t992 bool = carry__34 != 0
            if t992 {
                var t993 *_goml_vec_uint32 = result__30.words
                var t994 uint32 = uint32(uint64(carry__34))
                vec_push__Vec_6uint32(t993, t994)
            } else {}
            return result__30
        }
    }
}

func float_natural_decimal(value__49 FloatNatural) string {
    var t1040 bool
    var inline1984 *_goml_vec_uint32 = value__49.words
    var inline1985 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1984)
    t1040 = inline1985
    if t1040 {
        return "0"
    } else {
        var current__50 FloatNatural = float_natural_copy(value__49)
        var reversed__51 *_goml_vec_uint8 = vec_new__Vec_5uint8()
        Loop_loop1033:
        for {
            var t1034 bool
            var inline1973 *_goml_vec_uint32 = current__50.words
            var inline1974 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline1973)
            t1034 = inline1974
            var t1035 bool = !t1034
            if t1035 {
                var t1036 uint32 = float_natural_divide_small(current__50, 10)
                var t1037 uint8 = uint8(uint32(t1036))
                var t1038 uint8 = t1037 + 48
                vec_push__Vec_5uint8(reversed__51, t1038)
                continue
            } else {
                break Loop_loop1033
            }
        }
        var t1022 int
        var inline1982 int = vec_len__Vec_5uint8(reversed__51)
        t1022 = inline1982
        var output__52 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1022)
        var offset__53 int = 0
        Loop_loop1024:
        for {
            var t1025 int
            var inline1980 int = vec_len__Vec_5uint8(reversed__51)
            t1025 = inline1980
            var t1026 bool = offset__53 < t1025
            if t1026 {
                var t1027 int
                var inline1978 int = vec_len__Vec_5uint8(reversed__51)
                t1027 = inline1978
                var t1028 int = t1027 - offset__53
                var t1029 int = t1028 - 1
                var t1030 uint8 = vec_get__Vec_5uint8(reversed__51, t1029)
                vec_push__Vec_5uint8(output__52, t1030)
                var compound_old98 int = offset__53
                var compound_value99 int = 1
                var t1031 int = compound_old98 + compound_value99
                offset__53 = t1031
                continue
            } else {
                break Loop_loop1024
            }
        }
        var mtmp102 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__52)
        var x104 string = mtmp102._1
        return x104
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1043 int = _goml_runtime_core_string_len(self__289)
    return t1043
}

func rounded_float_digits(exact__145 string, count__146 int) Tuple2_6string_4bool {
    var t1046 int = count__146 + 1
    var output__147 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1046)
    var index__148 int = 0
    Loop_loop1101:
    for {
        var t1102 bool = index__148 < count__146
        if t1102 {
            var t1103 uint8
            var inline1989 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
            t1103 = inline1989
            vec_push__Vec_5uint8(output__147, t1103)
            var compound_old267 int = index__148
            var compound_value268 int = 1
            var t1104 int = compound_old267 + compound_value268
            index__148 = t1104
            continue
        } else {
            break Loop_loop1101
        }
    }
    var t1098 int
    var inline2010 int = _goml_runtime_core_string_len(exact__145)
    t1098 = inline2010
    var t1099 bool = count__146 == t1098
    if t1099 {
        var mtmp271 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
        var x273 string = mtmp271._1
        var t1100 Tuple2_6string_4bool = Tuple2_6string_4bool{
            _0: x273,
            _1: false,
        }
        return t1100
    } else {
        var next__150 uint8
        var inline2008 uint8 = _goml_runtime_core_string_byte_get(exact__145, count__146)
        next__150 = inline2008
        var trailing__151 bool = false
        var t1049 int = count__146 + 1
        index__148 = t1049
        Loop_loop1090:
        for {
            var t1091 int
            var inline1993 int = _goml_runtime_core_string_len(exact__145)
            t1091 = inline1993
            var t1092 bool = index__148 < t1091
            if t1092 {
                var t1096 uint8
                var inline1991 uint8 = _goml_runtime_core_string_byte_get(exact__145, index__148)
                t1096 = inline1991
                var t1097 bool = t1096 != 48
                if t1097 {
                    trailing__151 = true
                } else {}
                var compound_old278 int = index__148
                var compound_value279 int = 1
                var t1094 int = compound_old278 + compound_value279
                index__148 = t1094
                continue
            } else {
                break Loop_loop1090
            }
        }
        var t1078 bool = next__150 > 53
        var jp1052 bool
        if t1078 {
            jp1052 = true
        } else {
            var t1081 bool = next__150 == 53
            if t1081 {
                if trailing__151 {
                    jp1052 = true
                } else {
                    var t1084 int
                    var inline1995 int = vec_len__Vec_5uint8(output__147)
                    t1084 = inline1995
                    var t1085 int = t1084 - 1
                    var t1086 uint8 = vec_get__Vec_5uint8(output__147, t1085)
                    var t1087 uint8 = t1086 - 48
                    var t1088_rhs uint8 = 2
                    var t1088 uint8 = t1087 % t1088_rhs
                    var t1089 bool = t1088 == 1
                    jp1052 = t1089
                }
            } else {
                jp1052 = false
            }
        }
        if jp1052 {
            var index__153 int
            var inline2006 int = vec_len__Vec_5uint8(output__147)
            index__153 = inline2006
            Loop_loop1066:
            for {
                var t1067 bool = index__153 > 0
                if t1067 {
                    var compound_old282 int = index__153
                    var compound_value283 int = 1
                    var t1068 int = compound_old282 - compound_value283
                    index__153 = t1068
                    var t1071 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    var t1072 bool = t1071 < 57
                    if t1072 {
                        var index286 int = index__153
                        var place287 uint8 = vec_get__Vec_5uint8(output__147, index286)
                        var value288 uint8 = 1
                        var t1073 uint8 = place287 + value288
                        vec_set__Vec_5uint8(output__147, index286, t1073)
                        var mtmp290 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
                        var x292 string = mtmp290._1
                        var t1075 Tuple2_6string_4bool = Tuple2_6string_4bool{
                            _0: x292,
                            _1: false,
                        }
                        return t1075
                    } else {
                        var index294 int = index__153
                        vec_get__Vec_5uint8(output__147, index294)
                        var value296 uint8 = 48
                        vec_set__Vec_5uint8(output__147, index294, value296)
                        continue
                    }
                } else {
                    break Loop_loop1066
                }
            }
            var t1056 int
            var inline2004 int = vec_len__Vec_5uint8(output__147)
            t1056 = inline2004
            var t1057 int = t1056 + 1
            var carried__155 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1057)
            var inline2001 uint8 = 49
            vec_push__Vec_5uint8(carried__155, inline2001)
            index__153 = 0
            Loop_loop1060:
            for {
                var t1061 int
                var inline1999 int = vec_len__Vec_5uint8(output__147)
                t1061 = inline1999
                var t1062 bool = index__153 < t1061
                if t1062 {
                    var t1063 uint8 = vec_get__Vec_5uint8(output__147, index__153)
                    vec_push__Vec_5uint8(carried__155, t1063)
                    var compound_old302 int = index__153
                    var compound_value303 int = 1
                    var t1064 int = compound_old302 + compound_value303
                    index__153 = t1064
                    continue
                } else {
                    break Loop_loop1060
                }
            }
            var mtmp306 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(carried__155)
            var x308 string = mtmp306._1
            var t1059 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x308,
                _1: true,
            }
            return t1059
        } else {
            var mtmp309 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(output__147)
            var x311 string = mtmp309._1
            var t1077 Tuple2_6string_4bool = Tuple2_6string_4bool{
                _0: x311,
                _1: false,
            }
            return t1077
        }
    }
}

func trim_float_digits(value__158 string) string {
    var length__159 int
    var inline2017 int = _goml_runtime_core_string_len(value__158)
    length__159 = inline2017
    Loop_loop1110:
    for {
        var t1115 bool = length__159 > 1
        var jp1112 bool
        if t1115 {
            var t1116 int = length__159 - 1
            var t1117 uint8
            var inline2012 uint8 = _goml_runtime_core_string_byte_get(value__158, t1116)
            t1117 = inline2012
            var t1118 bool = t1117 == 48
            jp1112 = t1118
        } else {
            jp1112 = false
        }
        if jp1112 {
            var compound_old312 int = length__159
            var compound_value313 int = 1
            var t1113 int = compound_old312 - compound_value313
            length__159 = t1113
            continue
        } else {
            break Loop_loop1110
        }
    }
    var inline2014 int = 0
    var inline2015 string = string_byte_slice(value__158, inline2014, length__159)
    return inline2015
}

func fixed_float_text(digits__137 string, decimal_point__138 int, negative__139 bool) string {
    var bytes__140 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    if negative__139 {
        var inline2019 uint8 = 45
        vec_push__Vec_5uint8(bytes__140, inline2019)
    } else {}
    var t1123 bool = decimal_point__138 <= 0
    if t1123 {
        var inline2034 uint8 = 48
        vec_push__Vec_5uint8(bytes__140, inline2034)
        var inline2031 uint8 = 46
        vec_push__Vec_5uint8(bytes__140, inline2031)
        var index__141 int = 0
        var t1133 int = 0 - decimal_point__138
        Loop_loop1132:
        for {
            var t1134 bool = index__141 < t1133
            if t1134 {
                var inline2022 uint8 = 48
                vec_push__Vec_5uint8(bytes__140, inline2022)
                var compound_old234 int = index__141
                var compound_value235 int = 1
                var t1135 int = compound_old234 + compound_value235
                index__141 = t1135
                continue
            } else {
                break Loop_loop1132
            }
        }
        index__141 = 0
        Loop_loop1126:
        for {
            var t1127 int
            var inline2029 int = _goml_runtime_core_string_len(digits__137)
            t1127 = inline2029
            var t1128 bool = index__141 < t1127
            if t1128 {
                var t1129 uint8
                var inline2027 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__141)
                t1129 = inline2027
                vec_push__Vec_5uint8(bytes__140, t1129)
                var compound_old240 int = index__141
                var compound_value241 int = 1
                var t1130 int = compound_old240 + compound_value241
                index__141 = t1130
                continue
            } else {
                break Loop_loop1126
            }
        }
        var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
        var x265 string = mtmp263._1
        return x265
    } else {
        var t1138 int
        var inline2059 int = _goml_runtime_core_string_len(digits__137)
        t1138 = inline2059
        var t1139 bool = decimal_point__138 >= t1138
        if t1139 {
            var index__142 int = 0
            Loop_loop1146:
            for {
                var t1147 int
                var inline2041 int = _goml_runtime_core_string_len(digits__137)
                t1147 = inline2041
                var t1148 bool = index__142 < t1147
                if t1148 {
                    var t1149 uint8
                    var inline2039 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__142)
                    t1149 = inline2039
                    vec_push__Vec_5uint8(bytes__140, t1149)
                    var compound_old244 int = index__142
                    var compound_value245 int = 1
                    var t1150 int = compound_old244 + compound_value245
                    index__142 = t1150
                    continue
                } else {
                    break Loop_loop1146
                }
            }
            Loop_loop1142:
            for {
                var t1143 bool = index__142 < decimal_point__138
                if t1143 {
                    var inline2043 uint8 = 48
                    vec_push__Vec_5uint8(bytes__140, inline2043)
                    var compound_old249 int = index__142
                    var compound_value250 int = 1
                    var t1144 int = compound_old249 + compound_value250
                    index__142 = t1144
                    continue
                } else {
                    break Loop_loop1142
                }
            }
            var mtmp263 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__140)
            var x265 string = mtmp263._1
            return x265
        } else {
            var index__143 int = 0
            Loop_loop1160:
            for {
                var t1161 bool = index__143 < decimal_point__138
                if t1161 {
                    var t1162 uint8
                    var inline2048 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1162 = inline2048
                    vec_push__Vec_5uint8(bytes__140, t1162)
                    var compound_old253 int = index__143
                    var compound_value254 int = 1
                    var t1163 int = compound_old253 + compound_value254
                    index__143 = t1163
                    continue
                } else {
                    break Loop_loop1160
                }
            }
            var inline2056 uint8 = 46
            vec_push__Vec_5uint8(bytes__140, inline2056)
            Loop_loop1154:
            for {
                var t1155 int
                var inline2054 int = _goml_runtime_core_string_len(digits__137)
                t1155 = inline2054
                var t1156 bool = index__143 < t1155
                if t1156 {
                    var t1157 uint8
                    var inline2052 uint8 = _goml_runtime_core_string_byte_get(digits__137, index__143)
                    t1157 = inline2052
                    vec_push__Vec_5uint8(bytes__140, t1157)
                    var compound_old259 int = index__143
                    var compound_value260 int = 1
                    var t1158 int = compound_old259 + compound_value260
                    index__143 = t1158
                    continue
                } else {
                    break Loop_loop1154
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
    var t1259 bool = parsed__110.valid
    var t1260 bool = !t1259
    if t1260 {
        var t1261 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
            _0: false,
            _1: 0,
        }
        return t1261
    } else {
        var t1253 bool = parsed__110.negative
        var jp1170 uint64
        if t1253 {
            var t1258 bool = mantissa_bits__108 == 23
            var jp1255 int
            if t1258 {
                jp1255 = 8
            } else {
                jp1255 = 11
            }
            var t1256 int = mantissa_bits__108 + jp1255
            var t1257_lhs uint64 = 1
            var t1257 uint64 = t1257_lhs << t1256
            jp1170 = t1257
        } else {
            jp1170 = 0
        }
        var t1252 bool = mantissa_bits__108 == 23
        var jp1172 int
        if t1252 {
            jp1172 = 8
        } else {
            jp1172 = 11
        }
        var t1173_lhs uint64 = 1
        var t1173 uint64 = t1173_lhs << jp1172
        var t1174 uint64 = t1173 - 1
        var exponent_mask__112 uint64 = t1174 << mantissa_bits__108
        var t1230 int = parsed__110.special
        var t1231 bool = t1230 == 1
        if t1231 {
            var t1232 uint64 = jp1170 | exponent_mask__112
            var t1233 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                _0: true,
                _1: t1232,
            }
            return t1233
        } else {
            var t1235 int = parsed__110.special
            var t1236 bool = t1235 == 2
            if t1236 {
                var t1240 int = mantissa_bits__108 - 1
                var t1241_lhs uint64 = 1
                var t1241 uint64 = t1241_lhs << t1240
                var t1242 uint64 = exponent_mask__112 | t1241
                var t1247 bool = mantissa_bits__108 == 52
                var jp1244 uint64
                if t1247 {
                    jp1244 = 1
                } else {
                    jp1244 = 0
                }
                var t1245 uint64 = t1242 | jp1244
                var t1246 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                    _0: true,
                    _1: t1245,
                }
                return t1246
            } else {
                var t1249 FloatNatural = parsed__110.numerator
                var t1250 bool
                var inline2061 *_goml_vec_uint32 = t1249.words
                var inline2062 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2061)
                t1250 = inline2062
                if t1250 {
                    var t1251 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                        _0: true,
                        _1: jp1170,
                    }
                    return t1251
                } else {
                    var t1213 bool = parsed__110.hexadecimal
                    var t1214 bool = !t1213
                    if t1214 {
                        var t1215 int = parsed__110.significant_digits
                        var t1216 int = parsed__110.decimal_exponent
                        var decimal_position__113 int = t1215 + t1216
                        var t1229 bool = mantissa_bits__108 == 23
                        var jp1218 int
                        if t1229 {
                            jp1218 = 40
                        } else {
                            jp1218 = 310
                        }
                        var t1228 bool = mantissa_bits__108 == 23
                        var jp1220 int
                        if t1228 {
                            jp1220 = -46
                        } else {
                            jp1220 = -325
                        }
                        var t1222 bool = decimal_position__113 > jp1218
                        if t1222 {
                            var t1223 uint64 = jp1170 | exponent_mask__112
                            var t1224 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: false,
                                _1: t1223,
                            }
                            return t1224
                        } else {
                            var t1226 bool = decimal_position__113 < jp1220
                            if t1226 {
                                var t1227 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                    _0: true,
                                    _1: jp1170,
                                }
                                return t1227
                            } else {
                                var t1209 bool = parsed__110.hexadecimal
                                var t1210 bool = !t1209
                                var jp1204 bool
                                if t1210 {
                                    var t1211 int = parsed__110.decimal_exponent
                                    var t1212 bool = t1211 < 0
                                    jp1204 = t1212
                                } else {
                                    jp1204 = false
                                }
                                var jp1178 FloatNatural
                                if jp1204 {
                                    var t1205 int = parsed__110.decimal_exponent
                                    var t1206 int = 0 - t1205
                                    var t1207 FloatNatural = float_natural_power5(t1206)
                                    jp1178 = t1207
                                } else {
                                    var inline2064 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                                    vec_push__Vec_6uint32(inline2064, 1)
                                    var inline2066 FloatNatural = FloatNatural{
                                        words: inline2064,
                                    }
                                    jp1178 = inline2066
                                }
                                var t1199 bool = parsed__110.hexadecimal
                                var t1200 bool = !t1199
                                var jp1190 bool
                                if t1200 {
                                    var t1201 int = parsed__110.decimal_exponent
                                    var t1202 bool = t1201 > 0
                                    jp1190 = t1202
                                } else {
                                    jp1190 = false
                                }
                                var jp1180 FloatNatural
                                if jp1190 {
                                    var t1191 FloatNatural = parsed__110.numerator
                                    var result__117 FloatNatural = float_natural_copy(t1191)
                                    var count__118 int = 0
                                    Loop_loop1193:
                                    for {
                                        var t1194 int = parsed__110.decimal_exponent
                                        var t1195 bool = count__118 < t1194
                                        if t1195 {
                                            float_natural_multiply_small(result__117, 5)
                                            var compound_old213 int = count__118
                                            var compound_value214 int = 1
                                            var t1196 int = compound_old213 + compound_value214
                                            count__118 = t1196
                                            continue
                                        } else {
                                            break Loop_loop1193
                                        }
                                    }
                                    jp1180 = result__117
                                    var t1186 bool = parsed__110.hexadecimal
                                    var jp1182 int
                                    if t1186 {
                                        var t1187 int = parsed__110.binary_exponent
                                        jp1182 = t1187
                                    } else {
                                        var t1188 int = parsed__110.decimal_exponent
                                        jp1182 = t1188
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1180, jp1178, jp1182, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1183 bool = !x219
                                    var t1184 uint64 = jp1170 | x218
                                    var t1185 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1183,
                                        _1: t1184,
                                    }
                                    return t1185
                                } else {
                                    var t1198 FloatNatural = parsed__110.numerator
                                    jp1180 = t1198
                                    var t1186 bool = parsed__110.hexadecimal
                                    var jp1182 int
                                    if t1186 {
                                        var t1187 int = parsed__110.binary_exponent
                                        jp1182 = t1187
                                    } else {
                                        var t1188 int = parsed__110.decimal_exponent
                                        jp1182 = t1188
                                    }
                                    var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1180, jp1178, jp1182, mantissa_bits__108, exponent_bias__109)
                                    var x218 uint64 = mtmp217._0
                                    var x219 bool = mtmp217._1
                                    var t1183 bool = !x219
                                    var t1184 uint64 = jp1170 | x218
                                    var t1185 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                        _0: t1183,
                                        _1: t1184,
                                    }
                                    return t1185
                                }
                            }
                        }
                    } else {
                        var t1209 bool = parsed__110.hexadecimal
                        var t1210 bool = !t1209
                        var jp1204 bool
                        if t1210 {
                            var t1211 int = parsed__110.decimal_exponent
                            var t1212 bool = t1211 < 0
                            jp1204 = t1212
                        } else {
                            jp1204 = false
                        }
                        var jp1178 FloatNatural
                        if jp1204 {
                            var t1205 int = parsed__110.decimal_exponent
                            var t1206 int = 0 - t1205
                            var t1207 FloatNatural = float_natural_power5(t1206)
                            jp1178 = t1207
                        } else {
                            var inline2064 *_goml_vec_uint32 = vec_new__Vec_6uint32()
                            vec_push__Vec_6uint32(inline2064, 1)
                            var inline2066 FloatNatural = FloatNatural{
                                words: inline2064,
                            }
                            jp1178 = inline2066
                        }
                        var t1199 bool = parsed__110.hexadecimal
                        var t1200 bool = !t1199
                        var jp1190 bool
                        if t1200 {
                            var t1201 int = parsed__110.decimal_exponent
                            var t1202 bool = t1201 > 0
                            jp1190 = t1202
                        } else {
                            jp1190 = false
                        }
                        var jp1180 FloatNatural
                        if jp1190 {
                            var t1191 FloatNatural = parsed__110.numerator
                            var result__117 FloatNatural = float_natural_copy(t1191)
                            var count__118 int = 0
                            Loop_loop1193__2:
                            for {
                                var t1194 int = parsed__110.decimal_exponent
                                var t1195 bool = count__118 < t1194
                                if t1195 {
                                    float_natural_multiply_small(result__117, 5)
                                    var compound_old213 int = count__118
                                    var compound_value214 int = 1
                                    var t1196 int = compound_old213 + compound_value214
                                    count__118 = t1196
                                    continue
                                } else {
                                    break Loop_loop1193__2
                                }
                            }
                            jp1180 = result__117
                            var t1186 bool = parsed__110.hexadecimal
                            var jp1182 int
                            if t1186 {
                                var t1187 int = parsed__110.binary_exponent
                                jp1182 = t1187
                            } else {
                                var t1188 int = parsed__110.decimal_exponent
                                jp1182 = t1188
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1180, jp1178, jp1182, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1183 bool = !x219
                            var t1184 uint64 = jp1170 | x218
                            var t1185 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1183,
                                _1: t1184,
                            }
                            return t1185
                        } else {
                            var t1198 FloatNatural = parsed__110.numerator
                            jp1180 = t1198
                            var t1186 bool = parsed__110.hexadecimal
                            var jp1182 int
                            if t1186 {
                                var t1187 int = parsed__110.binary_exponent
                                jp1182 = t1187
                            } else {
                                var t1188 int = parsed__110.decimal_exponent
                                jp1182 = t1188
                            }
                            var mtmp217 Tuple2_6uint64_4bool = float_rational_bits(jp1180, jp1178, jp1182, mantissa_bits__108, exponent_bias__109)
                            var x218 uint64 = mtmp217._0
                            var x219 bool = mtmp217._1
                            var t1183 bool = !x219
                            var t1184 uint64 = jp1170 | x218
                            var t1185 Tuple2_4bool_6uint64 = Tuple2_4bool_6uint64{
                                _0: t1183,
                                _1: t1184,
                            }
                            return t1185
                        }
                    }
                }
            }
        }
    }
}

func float_natural_multiply_small(value__15 FloatNatural, factor__16 uint32) struct{} {
    var t1283 bool = factor__16 == 0
    if t1283 {
        var t1284 *_goml_vec_uint32 = value__15.words
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(t1284, 0)
        return struct{}{}
    } else {
        var carry__17 uint64 = 0
        var index__18 int = 0
        var t1277 uint64 = uint64(uint32(factor__16))
        Loop_loop1270:
        for {
            var t1271 *_goml_vec_uint32 = value__15.words
            var t1272 int
            var inline2070 int = vec_len__Vec_6uint32(t1271)
            t1272 = inline2070
            var t1273 bool = index__18 < t1272
            if t1273 {
                var t1274 *_goml_vec_uint32 = value__15.words
                var t1275 uint32 = vec_get__Vec_6uint32(t1274, index__18)
                var t1276 uint64 = uint64(uint32(t1275))
                var t1278 uint64 = t1276 * t1277
                var product__19 uint64 = t1278 + carry__17
                var place24 *_goml_vec_uint32 = value__15.words
                var index25 int = index__18
                vec_get__Vec_6uint32(place24, index25)
                var value27 uint32 = uint32(uint64(product__19))
                vec_set__Vec_6uint32(place24, index25, value27)
                var t1280_rhs int = 32
                var t1280 uint64 = product__19 >> t1280_rhs
                carry__17 = t1280
                var compound_old30 int = index__18
                var compound_value31 int = 1
                var t1281 int = compound_old30 + compound_value31
                index__18 = t1281
                continue
            } else {
                break Loop_loop1270
            }
        }
        var t1266 bool = carry__17 != 0
        if t1266 {
            var t1267 *_goml_vec_uint32 = value__15.words
            var t1268 uint32 = uint32(uint64(carry__17))
            vec_push__Vec_6uint32(t1267, t1268)
            return struct{}{}
        } else {
            return struct{}{}
        }
    }
}

func float_natural_zero() FloatNatural {
    var t1287 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var t1288 FloatNatural = FloatNatural{
        words: t1287,
    }
    return t1288
}

func float_natural_copy(value__4 FloatNatural) FloatNatural {
    var result__5 FloatNatural
    var inline2081 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2082 FloatNatural = FloatNatural{
        words: inline2081,
    }
    result__5 = inline2082
    var index__6 int = 0
    Loop_loop1298:
    for {
        var t1299 *_goml_vec_uint32 = value__4.words
        var t1300 int
        var inline2079 int = vec_len__Vec_6uint32(t1299)
        t1300 = inline2079
        var t1301 bool = index__6 < t1300
        if t1301 {
            var t1302 *_goml_vec_uint32 = result__5.words
            var t1303 *_goml_vec_uint32 = value__4.words
            var t1304 uint32 = vec_get__Vec_6uint32(t1303, index__6)
            vec_push__Vec_6uint32(t1302, t1304)
            var compound_old4 int = index__6
            var compound_value5 int = 1
            var t1305 int = compound_old4 + compound_value5
            index__6 = t1305
            continue
        } else {
            break Loop_loop1298
        }
    }
    return result__5
}

func float_natural_divide_small(value__44 FloatNatural, divisor__45 uint32) uint32 {
    var remainder__46 uint64 = 0
    var t1312 *_goml_vec_uint32 = value__44.words
    var index__47 int
    var inline2084 int = vec_len__Vec_6uint32(t1312)
    index__47 = inline2084
    var t1323 uint64 = uint64(uint32(divisor__45))
    var t1326 uint64 = uint64(uint32(divisor__45))
    Loop_loop1315:
    for {
        var t1316 bool = index__47 > 0
        if t1316 {
            var compound_old83 int = index__47
            var compound_value84 int = 1
            var t1317 int = compound_old83 - compound_value84
            index__47 = t1317
            var t1319_rhs int = 32
            var t1319 uint64 = remainder__46 << t1319_rhs
            var t1320 *_goml_vec_uint32 = value__44.words
            var t1321 uint32 = vec_get__Vec_6uint32(t1320, index__47)
            var t1322 uint64 = uint64(uint32(t1321))
            var current__48 uint64 = t1319 | t1322
            var place87 *_goml_vec_uint32 = value__44.words
            var index88 int = index__47
            vec_get__Vec_6uint32(place87, index88)
            var t1324 uint64 = current__48 / t1323
            var value90 uint32 = uint32(uint64(t1324))
            vec_set__Vec_6uint32(place87, index88, value90)
            var t1327 uint64 = current__48 % t1326
            remainder__46 = t1327
            continue
        } else {
            break Loop_loop1315
        }
    }
    float_natural_trim(value__44)
    var t1314 uint32 = uint32(uint64(remainder__46))
    return t1314
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1330 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1330
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline2086 bool = string_is_char_boundary(self__294, start__295)
    var inline2088 bool
    if inline2086 {
        var inline2091 bool = string_is_char_boundary(self__294, end__296)
        inline2088 = inline2091
    } else {
        inline2088 = false
    }
    if inline2088 {
        var inline2089 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline2089
    } else {
        var inline2090 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline2090
    }
}

func parse_float_text(value__84 string) ParsedFloat {
    var t1518 bool = string_equals_ascii_case(value__84, "nan")
    if t1518 {
        var t1519 FloatNatural
        var inline2093 *_goml_vec_uint32 = vec_new__Vec_6uint32()
        var inline2094 FloatNatural = FloatNatural{
            words: inline2093,
        }
        t1519 = inline2094
        var t1520 ParsedFloat = ParsedFloat{
            valid: true,
            negative: false,
            special: 2,
            numerator: t1519,
            decimal_exponent: 0,
            binary_exponent: 0,
            hexadecimal: false,
            significant_digits: 0,
        }
        return t1520
    } else {
        var index__85 int = 0
        var negative__86 bool = false
        var t1510 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1511 bool = index__85 < t1510
        var jp1505 bool
        if t1511 {
            var t1514 uint8
            var inline2098 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1514 = inline2098
            var t1515 bool = t1514 == 43
            if t1515 {
                jp1505 = true
            } else {
                var t1516 uint8
                var inline2096 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1516 = inline2096
                var t1517 bool = t1516 == 45
                jp1505 = t1517
            }
        } else {
            jp1505 = false
        }
        if jp1505 {
            var t1506 uint8
            var inline2100 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
            t1506 = inline2100
            var t1507 bool = t1506 == 45
            negative__86 = t1507
            var compound_old140 int = index__85
            var compound_value141 int = 1
            var t1508 int = compound_old140 + compound_value141
            index__85 = t1508
        } else {}
        var t1338 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var special_text__87 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__84, index__85, t1338)
        var t1502 bool = string_equals_ascii_case(special_text__87, "inf")
        var jp1499 bool
        if t1502 {
            jp1499 = true
        } else {
            var t1503 bool = string_equals_ascii_case(special_text__87, "infinity")
            jp1499 = t1503
        }
        if jp1499 {
            var t1500 FloatNatural
            var inline2102 *_goml_vec_uint32 = vec_new__Vec_6uint32()
            var inline2103 FloatNatural = FloatNatural{
                words: inline2102,
            }
            t1500 = inline2103
            var t1501 ParsedFloat = ParsedFloat{
                valid: true,
                negative: negative__86,
                special: 1,
                numerator: t1500,
                decimal_exponent: 0,
                binary_exponent: 0,
                hexadecimal: false,
                significant_digits: 0,
            }
            return t1501
        } else {
            var t1493 int = index__85 + 2
            var t1494 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
            var t1495 bool = t1493 <= t1494
            var jp1488 bool
            if t1495 {
                var t1496 uint8
                var inline2105 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                t1496 = inline2105
                var t1497 bool = t1496 == 48
                jp1488 = t1497
            } else {
                jp1488 = false
            }
            var jp1341 bool
            if jp1488 {
                var t1489 int = index__85 + 1
                var t1490 uint8
                var inline2114 uint8 = _goml_runtime_core_string_byte_get(value__84, t1489)
                t1490 = inline2114
                var t1491 uint8
                var inline2107 bool = t1490 >= 65
                var inline2109 bool
                if inline2107 {
                    var inline2112 bool = t1490 <= 90
                    inline2109 = inline2112
                } else {
                    inline2109 = false
                }
                if inline2109 {
                    var inline2110 uint8 = 97 - 65
                    var inline2111 uint8 = t1490 + inline2110
                    t1491 = inline2111
                    var t1492 bool = t1491 == 120
                    jp1341 = t1492
                    if jp1341 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1485 int = compound_old145 + compound_value146
                        index__85 = t1485
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1344 int
                    if jp1341 {
                        jp1344 = 16
                    } else {
                        jp1344 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1438 uint32 = uint32(int(jp1344))
                    Loop_loop1434:
                    for {
                        var t1435 int
                        var inline2128 int = _goml_runtime_core_string_len(value__84)
                        t1435 = inline2128
                        var t1436 bool = index__85 < t1435
                        if t1436 {
                            var current__97 uint8
                            var inline2126 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2126
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1344)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1438)
                                var t1439 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1439)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1450 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1450
                                } else {}
                                var t1448 bool = significant_digits__95 > 0
                                var jp1445 bool
                                if t1448 {
                                    jp1445 = true
                                } else {
                                    var t1449 bool = x151 != 0
                                    jp1445 = t1449
                                }
                                if jp1445 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1446 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1446
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1442 int = compound_old164 + compound_value165
                                index__85 = t1442
                                continue
                            } else {
                                var t1453 bool = current__97 == 95
                                if t1453 {
                                    var t1474 int = index__85 + 1
                                    var t1475 int
                                    var inline2124 int = _goml_runtime_core_string_len(value__84)
                                    t1475 = inline2124
                                    var t1476 bool = t1474 >= t1475
                                    if t1476 {
                                        var inline2116 FloatNatural = float_natural_zero()
                                        var inline2117 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2116,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2117
                                    } else {
                                        var t1455 int = index__85 + 1
                                        var t1456 uint8
                                        var inline2122 uint8 = _goml_runtime_core_string_byte_get(value__84, t1455)
                                        t1456 = inline2122
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1456, jp1344)
                                        var x169 bool = mtmp168._0
                                        var jp1471 bool
                                        if jp1341 {
                                            var t1473 bool = !saw_digit__92
                                            jp1471 = t1473
                                        } else {
                                            jp1471 = false
                                        }
                                        var jp1458 bool
                                        if jp1471 {
                                            var t1472 bool = index__85 == mantissa_start__89
                                            jp1458 = t1472
                                        } else {
                                            jp1458 = false
                                        }
                                        var t1468 bool = !previous_digit__96
                                        var jp1466 bool
                                        if t1468 {
                                            var t1469 bool = !jp1458
                                            jp1466 = t1469
                                        } else {
                                            jp1466 = false
                                        }
                                        var jp1463 bool
                                        if jp1466 {
                                            jp1463 = true
                                        } else {
                                            var t1467 bool = !x169
                                            jp1463 = t1467
                                        }
                                        if jp1463 {
                                            var inline2119 FloatNatural = float_natural_zero()
                                            var inline2120 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2119,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2120
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1460 int = compound_old173 + compound_value174
                                            index__85 = t1460
                                            continue
                                        }
                                    }
                                } else {
                                    var t1483 bool = current__97 == 46
                                    var jp1480 bool
                                    if t1483 {
                                        var t1484 bool = !saw_dot__93
                                        jp1480 = t1484
                                    } else {
                                        jp1480 = false
                                    }
                                    if jp1480 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1481 int = compound_old178 + compound_value179
                                        index__85 = t1481
                                        continue
                                    } else {
                                        break Loop_loop1434
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1434
                        }
                    }
                    var t1432 bool = !saw_digit__92
                    if t1432 {
                        var inline2130 FloatNatural = float_natural_zero()
                        var inline2131 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2130,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2131
                    } else {
                        var jp1348 uint8
                        if jp1341 {
                            jp1348 = 112
                        } else {
                            jp1348 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1427 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1428 bool = index__85 < t1427
                        var jp1365 bool
                        if t1428 {
                            var t1429 uint8
                            var inline2133 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1429 = inline2133
                            var t1430 uint8 = ascii_lower(t1429)
                            var t1431 bool = t1430 == jp1348
                            jp1365 = t1431
                        } else {
                            jp1365 = false
                        }
                        if jp1365 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1366 int = compound_old183 + compound_value184
                            index__85 = t1366
                            var t1417 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1418 bool = index__85 < t1417
                            var jp1412 bool
                            if t1418 {
                                var t1421 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1422 bool = t1421 == 43
                                if t1422 {
                                    jp1412 = true
                                } else {
                                    var t1423 uint8
                                    var inline2135 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1423 = inline2135
                                    var t1424 bool = t1423 == 45
                                    jp1412 = t1424
                                }
                            } else {
                                jp1412 = false
                            }
                            if jp1412 {
                                var t1413 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1414 bool = t1413 == 45
                                exponent_negative__104 = t1414
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1415 int = compound_old187 + compound_value188
                                index__85 = t1415
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1373:
                            for {
                                var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1375 bool = index__85 < t1374
                                if t1375 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1409 bool = current__106 >= 48
                                    var jp1378 bool
                                    if t1409 {
                                        var t1410 bool = current__106 <= 57
                                        jp1378 = t1410
                                    } else {
                                        jp1378 = false
                                    }
                                    if jp1378 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1382 bool = exponent__103 < 1000000
                                        if t1382 {
                                            var t1383 int = exponent__103 * 10
                                            var t1384 uint8 = current__106 - 48
                                            var t1385 int = int(uint8(t1384))
                                            var t1386 int = t1383 + t1385
                                            exponent__103 = t1386
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1380 int = compound_old196 + compound_value197
                                        index__85 = t1380
                                        continue
                                    } else {
                                        var t1388 bool = current__106 == 95
                                        if t1388 {
                                            var t1405 bool = !previous_digit__96
                                            var jp1401 bool
                                            if t1405 {
                                                jp1401 = true
                                            } else {
                                                var t1406 int = index__85 + 1
                                                var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1408 bool = t1406 >= t1407
                                                jp1401 = t1408
                                            }
                                            var jp1396 bool
                                            if jp1401 {
                                                jp1396 = true
                                            } else {
                                                var t1402 int = index__85 + 1
                                                var t1403 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1402)
                                                var t1404 bool = t1403 < 48
                                                jp1396 = t1404
                                            }
                                            var jp1393 bool
                                            if jp1396 {
                                                jp1393 = true
                                            } else {
                                                var t1397 int = index__85 + 1
                                                var t1398 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1397)
                                                var t1399 bool = t1398 > 57
                                                jp1393 = t1399
                                            }
                                            if jp1393 {
                                                var t1394 ParsedFloat = invalid_parsed_float()
                                                return t1394
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1390 int = compound_old201 + compound_value202
                                                index__85 = t1390
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1373
                                        }
                                    }
                                } else {
                                    break Loop_loop1373
                                }
                            }
                            var t1371 bool = !exponent_digits__105
                            if t1371 {
                                var t1372 ParsedFloat = invalid_parsed_float()
                                return t1372
                            } else {
                                var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1362 bool = index__85 != t1361
                                if t1362 {
                                    var t1363 ParsedFloat = invalid_parsed_float()
                                    return t1363
                                } else {
                                    if exponent_negative__104 {
                                        var t1360 int = 0 - exponent__103
                                        exponent__103 = t1360
                                    } else {}
                                    var jp1353 int
                                    if jp1341 {
                                        jp1353 = 0
                                    } else {
                                        var t1359 int = exponent__103 - fraction_digits__94
                                        jp1353 = t1359
                                    }
                                    var jp1355 int
                                    if jp1341 {
                                        var t1357 int = fraction_digits__94 * 4
                                        var t1358 int = exponent__103 - t1357
                                        jp1355 = t1358
                                    } else {
                                        jp1355 = 0
                                    }
                                    var t1356 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1353,
                                        binary_exponent: jp1355,
                                        hexadecimal: jp1341,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1356
                                }
                            }
                        } else {
                            if jp1341 {
                                var t1426 ParsedFloat = invalid_parsed_float()
                                return t1426
                            } else {
                                var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1362 bool = index__85 != t1361
                                if t1362 {
                                    var t1363 ParsedFloat = invalid_parsed_float()
                                    return t1363
                                } else {
                                    if exponent_negative__104 {
                                        var t1360 int = 0 - exponent__103
                                        exponent__103 = t1360
                                    } else {}
                                    var jp1353 int
                                    if jp1341 {
                                        jp1353 = 0
                                    } else {
                                        var t1359 int = exponent__103 - fraction_digits__94
                                        jp1353 = t1359
                                    }
                                    var jp1355 int
                                    if jp1341 {
                                        var t1357 int = fraction_digits__94 * 4
                                        var t1358 int = exponent__103 - t1357
                                        jp1355 = t1358
                                    } else {
                                        jp1355 = 0
                                    }
                                    var t1356 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1353,
                                        binary_exponent: jp1355,
                                        hexadecimal: jp1341,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1356
                                }
                            }
                        }
                    }
                } else {
                    t1491 = t1490
                    var t1492 bool = t1491 == 120
                    jp1341 = t1492
                    if jp1341 {
                        var compound_old145 int = index__85
                        var compound_value146 int = 2
                        var t1485 int = compound_old145 + compound_value146
                        index__85 = t1485
                    } else {}
                    var mantissa_start__89 int = index__85
                    var jp1344 int
                    if jp1341 {
                        jp1344 = 16
                    } else {
                        jp1344 = 10
                    }
                    var numerator__91 FloatNatural = float_natural_zero()
                    var saw_digit__92 bool = false
                    var saw_dot__93 bool = false
                    var fraction_digits__94 int = 0
                    var significant_digits__95 int = 0
                    var previous_digit__96 bool = false
                    var t1438 uint32 = uint32(int(jp1344))
                    Loop_loop1434__2:
                    for {
                        var t1435 int
                        var inline2128 int = _goml_runtime_core_string_len(value__84)
                        t1435 = inline2128
                        var t1436 bool = index__85 < t1435
                        if t1436 {
                            var current__97 uint8
                            var inline2126 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            current__97 = inline2126
                            var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1344)
                            var x150 bool = mtmp149._0
                            var x151 int = mtmp149._1
                            if x150 {
                                float_natural_multiply_small(numerator__91, t1438)
                                var t1439 uint32 = uint32(int(x151))
                                float_natural_add_small(numerator__91, t1439)
                                saw_digit__92 = true
                                previous_digit__96 = true
                                if saw_dot__93 {
                                    var compound_old156 int = fraction_digits__94
                                    var compound_value157 int = 1
                                    var t1450 int = compound_old156 + compound_value157
                                    fraction_digits__94 = t1450
                                } else {}
                                var t1448 bool = significant_digits__95 > 0
                                var jp1445 bool
                                if t1448 {
                                    jp1445 = true
                                } else {
                                    var t1449 bool = x151 != 0
                                    jp1445 = t1449
                                }
                                if jp1445 {
                                    var compound_old160 int = significant_digits__95
                                    var compound_value161 int = 1
                                    var t1446 int = compound_old160 + compound_value161
                                    significant_digits__95 = t1446
                                } else {}
                                var compound_old164 int = index__85
                                var compound_value165 int = 1
                                var t1442 int = compound_old164 + compound_value165
                                index__85 = t1442
                                continue
                            } else {
                                var t1453 bool = current__97 == 95
                                if t1453 {
                                    var t1474 int = index__85 + 1
                                    var t1475 int
                                    var inline2124 int = _goml_runtime_core_string_len(value__84)
                                    t1475 = inline2124
                                    var t1476 bool = t1474 >= t1475
                                    if t1476 {
                                        var inline2116 FloatNatural = float_natural_zero()
                                        var inline2117 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2116,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2117
                                    } else {
                                        var t1455 int = index__85 + 1
                                        var t1456 uint8
                                        var inline2122 uint8 = _goml_runtime_core_string_byte_get(value__84, t1455)
                                        t1456 = inline2122
                                        var mtmp168 Tuple2_4bool_3int = float_digit(t1456, jp1344)
                                        var x169 bool = mtmp168._0
                                        var jp1471 bool
                                        if jp1341 {
                                            var t1473 bool = !saw_digit__92
                                            jp1471 = t1473
                                        } else {
                                            jp1471 = false
                                        }
                                        var jp1458 bool
                                        if jp1471 {
                                            var t1472 bool = index__85 == mantissa_start__89
                                            jp1458 = t1472
                                        } else {
                                            jp1458 = false
                                        }
                                        var t1468 bool = !previous_digit__96
                                        var jp1466 bool
                                        if t1468 {
                                            var t1469 bool = !jp1458
                                            jp1466 = t1469
                                        } else {
                                            jp1466 = false
                                        }
                                        var jp1463 bool
                                        if jp1466 {
                                            jp1463 = true
                                        } else {
                                            var t1467 bool = !x169
                                            jp1463 = t1467
                                        }
                                        if jp1463 {
                                            var inline2119 FloatNatural = float_natural_zero()
                                            var inline2120 ParsedFloat = ParsedFloat{
                                                valid: false,
                                                negative: false,
                                                special: 0,
                                                numerator: inline2119,
                                                decimal_exponent: 0,
                                                binary_exponent: 0,
                                                hexadecimal: false,
                                                significant_digits: 0,
                                            }
                                            return inline2120
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old173 int = index__85
                                            var compound_value174 int = 1
                                            var t1460 int = compound_old173 + compound_value174
                                            index__85 = t1460
                                            continue
                                        }
                                    }
                                } else {
                                    var t1483 bool = current__97 == 46
                                    var jp1480 bool
                                    if t1483 {
                                        var t1484 bool = !saw_dot__93
                                        jp1480 = t1484
                                    } else {
                                        jp1480 = false
                                    }
                                    if jp1480 {
                                        saw_dot__93 = true
                                        previous_digit__96 = false
                                        var compound_old178 int = index__85
                                        var compound_value179 int = 1
                                        var t1481 int = compound_old178 + compound_value179
                                        index__85 = t1481
                                        continue
                                    } else {
                                        break Loop_loop1434__2
                                    }
                                }
                            }
                        } else {
                            break Loop_loop1434__2
                        }
                    }
                    var t1432 bool = !saw_digit__92
                    if t1432 {
                        var inline2130 FloatNatural = float_natural_zero()
                        var inline2131 ParsedFloat = ParsedFloat{
                            valid: false,
                            negative: false,
                            special: 0,
                            numerator: inline2130,
                            decimal_exponent: 0,
                            binary_exponent: 0,
                            hexadecimal: false,
                            significant_digits: 0,
                        }
                        return inline2131
                    } else {
                        var jp1348 uint8
                        if jp1341 {
                            jp1348 = 112
                        } else {
                            jp1348 = 101
                        }
                        var exponent__103 int = 0
                        var exponent_negative__104 bool = false
                        var t1427 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1428 bool = index__85 < t1427
                        var jp1365 bool
                        if t1428 {
                            var t1429 uint8
                            var inline2133 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                            t1429 = inline2133
                            var t1430 uint8 = ascii_lower(t1429)
                            var t1431 bool = t1430 == jp1348
                            jp1365 = t1431
                        } else {
                            jp1365 = false
                        }
                        if jp1365 {
                            var compound_old183 int = index__85
                            var compound_value184 int = 1
                            var t1366 int = compound_old183 + compound_value184
                            index__85 = t1366
                            var t1417 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1418 bool = index__85 < t1417
                            var jp1412 bool
                            if t1418 {
                                var t1421 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1422 bool = t1421 == 43
                                if t1422 {
                                    jp1412 = true
                                } else {
                                    var t1423 uint8
                                    var inline2135 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                    t1423 = inline2135
                                    var t1424 bool = t1423 == 45
                                    jp1412 = t1424
                                }
                            } else {
                                jp1412 = false
                            }
                            if jp1412 {
                                var t1413 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1414 bool = t1413 == 45
                                exponent_negative__104 = t1414
                                var compound_old187 int = index__85
                                var compound_value188 int = 1
                                var t1415 int = compound_old187 + compound_value188
                                index__85 = t1415
                            } else {}
                            var exponent_digits__105 bool = false
                            previous_digit__96 = false
                            Loop_loop1373__2:
                            for {
                                var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1375 bool = index__85 < t1374
                                if t1375 {
                                    var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                    var t1409 bool = current__106 >= 48
                                    var jp1378 bool
                                    if t1409 {
                                        var t1410 bool = current__106 <= 57
                                        jp1378 = t1410
                                    } else {
                                        jp1378 = false
                                    }
                                    if jp1378 {
                                        exponent_digits__105 = true
                                        previous_digit__96 = true
                                        var t1382 bool = exponent__103 < 1000000
                                        if t1382 {
                                            var t1383 int = exponent__103 * 10
                                            var t1384 uint8 = current__106 - 48
                                            var t1385 int = int(uint8(t1384))
                                            var t1386 int = t1383 + t1385
                                            exponent__103 = t1386
                                        } else {}
                                        var compound_old196 int = index__85
                                        var compound_value197 int = 1
                                        var t1380 int = compound_old196 + compound_value197
                                        index__85 = t1380
                                        continue
                                    } else {
                                        var t1388 bool = current__106 == 95
                                        if t1388 {
                                            var t1405 bool = !previous_digit__96
                                            var jp1401 bool
                                            if t1405 {
                                                jp1401 = true
                                            } else {
                                                var t1406 int = index__85 + 1
                                                var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                                var t1408 bool = t1406 >= t1407
                                                jp1401 = t1408
                                            }
                                            var jp1396 bool
                                            if jp1401 {
                                                jp1396 = true
                                            } else {
                                                var t1402 int = index__85 + 1
                                                var t1403 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1402)
                                                var t1404 bool = t1403 < 48
                                                jp1396 = t1404
                                            }
                                            var jp1393 bool
                                            if jp1396 {
                                                jp1393 = true
                                            } else {
                                                var t1397 int = index__85 + 1
                                                var t1398 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1397)
                                                var t1399 bool = t1398 > 57
                                                jp1393 = t1399
                                            }
                                            if jp1393 {
                                                var t1394 ParsedFloat = invalid_parsed_float()
                                                return t1394
                                            } else {
                                                previous_digit__96 = false
                                                var compound_old201 int = index__85
                                                var compound_value202 int = 1
                                                var t1390 int = compound_old201 + compound_value202
                                                index__85 = t1390
                                                continue
                                            }
                                        } else {
                                            break Loop_loop1373__2
                                        }
                                    }
                                } else {
                                    break Loop_loop1373__2
                                }
                            }
                            var t1371 bool = !exponent_digits__105
                            if t1371 {
                                var t1372 ParsedFloat = invalid_parsed_float()
                                return t1372
                            } else {
                                var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1362 bool = index__85 != t1361
                                if t1362 {
                                    var t1363 ParsedFloat = invalid_parsed_float()
                                    return t1363
                                } else {
                                    if exponent_negative__104 {
                                        var t1360 int = 0 - exponent__103
                                        exponent__103 = t1360
                                    } else {}
                                    var jp1353 int
                                    if jp1341 {
                                        jp1353 = 0
                                    } else {
                                        var t1359 int = exponent__103 - fraction_digits__94
                                        jp1353 = t1359
                                    }
                                    var jp1355 int
                                    if jp1341 {
                                        var t1357 int = fraction_digits__94 * 4
                                        var t1358 int = exponent__103 - t1357
                                        jp1355 = t1358
                                    } else {
                                        jp1355 = 0
                                    }
                                    var t1356 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1353,
                                        binary_exponent: jp1355,
                                        hexadecimal: jp1341,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1356
                                }
                            }
                        } else {
                            if jp1341 {
                                var t1426 ParsedFloat = invalid_parsed_float()
                                return t1426
                            } else {
                                var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                var t1362 bool = index__85 != t1361
                                if t1362 {
                                    var t1363 ParsedFloat = invalid_parsed_float()
                                    return t1363
                                } else {
                                    if exponent_negative__104 {
                                        var t1360 int = 0 - exponent__103
                                        exponent__103 = t1360
                                    } else {}
                                    var jp1353 int
                                    if jp1341 {
                                        jp1353 = 0
                                    } else {
                                        var t1359 int = exponent__103 - fraction_digits__94
                                        jp1353 = t1359
                                    }
                                    var jp1355 int
                                    if jp1341 {
                                        var t1357 int = fraction_digits__94 * 4
                                        var t1358 int = exponent__103 - t1357
                                        jp1355 = t1358
                                    } else {
                                        jp1355 = 0
                                    }
                                    var t1356 ParsedFloat = ParsedFloat{
                                        valid: true,
                                        negative: negative__86,
                                        special: 0,
                                        numerator: numerator__91,
                                        decimal_exponent: jp1353,
                                        binary_exponent: jp1355,
                                        hexadecimal: jp1341,
                                        significant_digits: significant_digits__95,
                                    }
                                    return t1356
                                }
                            }
                        }
                    }
                }
            } else {
                jp1341 = false
                if jp1341 {
                    var compound_old145 int = index__85
                    var compound_value146 int = 2
                    var t1485 int = compound_old145 + compound_value146
                    index__85 = t1485
                } else {}
                var mantissa_start__89 int = index__85
                var jp1344 int
                if jp1341 {
                    jp1344 = 16
                } else {
                    jp1344 = 10
                }
                var numerator__91 FloatNatural = float_natural_zero()
                var saw_digit__92 bool = false
                var saw_dot__93 bool = false
                var fraction_digits__94 int = 0
                var significant_digits__95 int = 0
                var previous_digit__96 bool = false
                var t1438 uint32 = uint32(int(jp1344))
                Loop_loop1434__3:
                for {
                    var t1435 int
                    var inline2128 int = _goml_runtime_core_string_len(value__84)
                    t1435 = inline2128
                    var t1436 bool = index__85 < t1435
                    if t1436 {
                        var current__97 uint8
                        var inline2126 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        current__97 = inline2126
                        var mtmp149 Tuple2_4bool_3int = float_digit(current__97, jp1344)
                        var x150 bool = mtmp149._0
                        var x151 int = mtmp149._1
                        if x150 {
                            float_natural_multiply_small(numerator__91, t1438)
                            var t1439 uint32 = uint32(int(x151))
                            float_natural_add_small(numerator__91, t1439)
                            saw_digit__92 = true
                            previous_digit__96 = true
                            if saw_dot__93 {
                                var compound_old156 int = fraction_digits__94
                                var compound_value157 int = 1
                                var t1450 int = compound_old156 + compound_value157
                                fraction_digits__94 = t1450
                            } else {}
                            var t1448 bool = significant_digits__95 > 0
                            var jp1445 bool
                            if t1448 {
                                jp1445 = true
                            } else {
                                var t1449 bool = x151 != 0
                                jp1445 = t1449
                            }
                            if jp1445 {
                                var compound_old160 int = significant_digits__95
                                var compound_value161 int = 1
                                var t1446 int = compound_old160 + compound_value161
                                significant_digits__95 = t1446
                            } else {}
                            var compound_old164 int = index__85
                            var compound_value165 int = 1
                            var t1442 int = compound_old164 + compound_value165
                            index__85 = t1442
                            continue
                        } else {
                            var t1453 bool = current__97 == 95
                            if t1453 {
                                var t1474 int = index__85 + 1
                                var t1475 int
                                var inline2124 int = _goml_runtime_core_string_len(value__84)
                                t1475 = inline2124
                                var t1476 bool = t1474 >= t1475
                                if t1476 {
                                    var inline2116 FloatNatural = float_natural_zero()
                                    var inline2117 ParsedFloat = ParsedFloat{
                                        valid: false,
                                        negative: false,
                                        special: 0,
                                        numerator: inline2116,
                                        decimal_exponent: 0,
                                        binary_exponent: 0,
                                        hexadecimal: false,
                                        significant_digits: 0,
                                    }
                                    return inline2117
                                } else {
                                    var t1455 int = index__85 + 1
                                    var t1456 uint8
                                    var inline2122 uint8 = _goml_runtime_core_string_byte_get(value__84, t1455)
                                    t1456 = inline2122
                                    var mtmp168 Tuple2_4bool_3int = float_digit(t1456, jp1344)
                                    var x169 bool = mtmp168._0
                                    var jp1471 bool
                                    if jp1341 {
                                        var t1473 bool = !saw_digit__92
                                        jp1471 = t1473
                                    } else {
                                        jp1471 = false
                                    }
                                    var jp1458 bool
                                    if jp1471 {
                                        var t1472 bool = index__85 == mantissa_start__89
                                        jp1458 = t1472
                                    } else {
                                        jp1458 = false
                                    }
                                    var t1468 bool = !previous_digit__96
                                    var jp1466 bool
                                    if t1468 {
                                        var t1469 bool = !jp1458
                                        jp1466 = t1469
                                    } else {
                                        jp1466 = false
                                    }
                                    var jp1463 bool
                                    if jp1466 {
                                        jp1463 = true
                                    } else {
                                        var t1467 bool = !x169
                                        jp1463 = t1467
                                    }
                                    if jp1463 {
                                        var inline2119 FloatNatural = float_natural_zero()
                                        var inline2120 ParsedFloat = ParsedFloat{
                                            valid: false,
                                            negative: false,
                                            special: 0,
                                            numerator: inline2119,
                                            decimal_exponent: 0,
                                            binary_exponent: 0,
                                            hexadecimal: false,
                                            significant_digits: 0,
                                        }
                                        return inline2120
                                    } else {
                                        previous_digit__96 = false
                                        var compound_old173 int = index__85
                                        var compound_value174 int = 1
                                        var t1460 int = compound_old173 + compound_value174
                                        index__85 = t1460
                                        continue
                                    }
                                }
                            } else {
                                var t1483 bool = current__97 == 46
                                var jp1480 bool
                                if t1483 {
                                    var t1484 bool = !saw_dot__93
                                    jp1480 = t1484
                                } else {
                                    jp1480 = false
                                }
                                if jp1480 {
                                    saw_dot__93 = true
                                    previous_digit__96 = false
                                    var compound_old178 int = index__85
                                    var compound_value179 int = 1
                                    var t1481 int = compound_old178 + compound_value179
                                    index__85 = t1481
                                    continue
                                } else {
                                    break Loop_loop1434__3
                                }
                            }
                        }
                    } else {
                        break Loop_loop1434__3
                    }
                }
                var t1432 bool = !saw_digit__92
                if t1432 {
                    var inline2130 FloatNatural = float_natural_zero()
                    var inline2131 ParsedFloat = ParsedFloat{
                        valid: false,
                        negative: false,
                        special: 0,
                        numerator: inline2130,
                        decimal_exponent: 0,
                        binary_exponent: 0,
                        hexadecimal: false,
                        significant_digits: 0,
                    }
                    return inline2131
                } else {
                    var jp1348 uint8
                    if jp1341 {
                        jp1348 = 112
                    } else {
                        jp1348 = 101
                    }
                    var exponent__103 int = 0
                    var exponent_negative__104 bool = false
                    var t1427 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                    var t1428 bool = index__85 < t1427
                    var jp1365 bool
                    if t1428 {
                        var t1429 uint8
                        var inline2133 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                        t1429 = inline2133
                        var t1430 uint8 = ascii_lower(t1429)
                        var t1431 bool = t1430 == jp1348
                        jp1365 = t1431
                    } else {
                        jp1365 = false
                    }
                    if jp1365 {
                        var compound_old183 int = index__85
                        var compound_value184 int = 1
                        var t1366 int = compound_old183 + compound_value184
                        index__85 = t1366
                        var t1417 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                        var t1418 bool = index__85 < t1417
                        var jp1412 bool
                        if t1418 {
                            var t1421 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1422 bool = t1421 == 43
                            if t1422 {
                                jp1412 = true
                            } else {
                                var t1423 uint8
                                var inline2135 uint8 = _goml_runtime_core_string_byte_get(value__84, index__85)
                                t1423 = inline2135
                                var t1424 bool = t1423 == 45
                                jp1412 = t1424
                            }
                        } else {
                            jp1412 = false
                        }
                        if jp1412 {
                            var t1413 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                            var t1414 bool = t1413 == 45
                            exponent_negative__104 = t1414
                            var compound_old187 int = index__85
                            var compound_value188 int = 1
                            var t1415 int = compound_old187 + compound_value188
                            index__85 = t1415
                        } else {}
                        var exponent_digits__105 bool = false
                        previous_digit__96 = false
                        Loop_loop1373__3:
                        for {
                            var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1375 bool = index__85 < t1374
                            if t1375 {
                                var current__106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, index__85)
                                var t1409 bool = current__106 >= 48
                                var jp1378 bool
                                if t1409 {
                                    var t1410 bool = current__106 <= 57
                                    jp1378 = t1410
                                } else {
                                    jp1378 = false
                                }
                                if jp1378 {
                                    exponent_digits__105 = true
                                    previous_digit__96 = true
                                    var t1382 bool = exponent__103 < 1000000
                                    if t1382 {
                                        var t1383 int = exponent__103 * 10
                                        var t1384 uint8 = current__106 - 48
                                        var t1385 int = int(uint8(t1384))
                                        var t1386 int = t1383 + t1385
                                        exponent__103 = t1386
                                    } else {}
                                    var compound_old196 int = index__85
                                    var compound_value197 int = 1
                                    var t1380 int = compound_old196 + compound_value197
                                    index__85 = t1380
                                    continue
                                } else {
                                    var t1388 bool = current__106 == 95
                                    if t1388 {
                                        var t1405 bool = !previous_digit__96
                                        var jp1401 bool
                                        if t1405 {
                                            jp1401 = true
                                        } else {
                                            var t1406 int = index__85 + 1
                                            var t1407 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                                            var t1408 bool = t1406 >= t1407
                                            jp1401 = t1408
                                        }
                                        var jp1396 bool
                                        if jp1401 {
                                            jp1396 = true
                                        } else {
                                            var t1402 int = index__85 + 1
                                            var t1403 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1402)
                                            var t1404 bool = t1403 < 48
                                            jp1396 = t1404
                                        }
                                        var jp1393 bool
                                        if jp1396 {
                                            jp1393 = true
                                        } else {
                                            var t1397 int = index__85 + 1
                                            var t1398 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1397)
                                            var t1399 bool = t1398 > 57
                                            jp1393 = t1399
                                        }
                                        if jp1393 {
                                            var t1394 ParsedFloat = invalid_parsed_float()
                                            return t1394
                                        } else {
                                            previous_digit__96 = false
                                            var compound_old201 int = index__85
                                            var compound_value202 int = 1
                                            var t1390 int = compound_old201 + compound_value202
                                            index__85 = t1390
                                            continue
                                        }
                                    } else {
                                        break Loop_loop1373__3
                                    }
                                }
                            } else {
                                break Loop_loop1373__3
                            }
                        }
                        var t1371 bool = !exponent_digits__105
                        if t1371 {
                            var t1372 ParsedFloat = invalid_parsed_float()
                            return t1372
                        } else {
                            var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1362 bool = index__85 != t1361
                            if t1362 {
                                var t1363 ParsedFloat = invalid_parsed_float()
                                return t1363
                            } else {
                                if exponent_negative__104 {
                                    var t1360 int = 0 - exponent__103
                                    exponent__103 = t1360
                                } else {}
                                var jp1353 int
                                if jp1341 {
                                    jp1353 = 0
                                } else {
                                    var t1359 int = exponent__103 - fraction_digits__94
                                    jp1353 = t1359
                                }
                                var jp1355 int
                                if jp1341 {
                                    var t1357 int = fraction_digits__94 * 4
                                    var t1358 int = exponent__103 - t1357
                                    jp1355 = t1358
                                } else {
                                    jp1355 = 0
                                }
                                var t1356 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1353,
                                    binary_exponent: jp1355,
                                    hexadecimal: jp1341,
                                    significant_digits: significant_digits__95,
                                }
                                return t1356
                            }
                        }
                    } else {
                        if jp1341 {
                            var t1426 ParsedFloat = invalid_parsed_float()
                            return t1426
                        } else {
                            var t1361 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                            var t1362 bool = index__85 != t1361
                            if t1362 {
                                var t1363 ParsedFloat = invalid_parsed_float()
                                return t1363
                            } else {
                                if exponent_negative__104 {
                                    var t1360 int = 0 - exponent__103
                                    exponent__103 = t1360
                                } else {}
                                var jp1353 int
                                if jp1341 {
                                    jp1353 = 0
                                } else {
                                    var t1359 int = exponent__103 - fraction_digits__94
                                    jp1353 = t1359
                                }
                                var jp1355 int
                                if jp1341 {
                                    var t1357 int = fraction_digits__94 * 4
                                    var t1358 int = exponent__103 - t1357
                                    jp1355 = t1358
                                } else {
                                    jp1355 = 0
                                }
                                var t1356 ParsedFloat = ParsedFloat{
                                    valid: true,
                                    negative: negative__86,
                                    special: 0,
                                    numerator: numerator__91,
                                    decimal_exponent: jp1353,
                                    binary_exponent: jp1355,
                                    hexadecimal: jp1341,
                                    significant_digits: significant_digits__95,
                                }
                                return t1356
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
    var inline2137 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    vec_push__Vec_6uint32(inline2137, 1)
    var inline2139 FloatNatural = FloatNatural{
        words: inline2137,
    }
    result__26 = inline2139
    var count__27 int = 0
    Loop_loop1524:
    for {
        var t1525 bool = count__27 < exponent__25
        if t1525 {
            float_natural_multiply_small(result__26, 5)
            var compound_old46 int = count__27
            var compound_value47 int = 1
            var t1526 int = compound_old46 + compound_value47
            count__27 = t1526
            continue
        } else {
            break Loop_loop1524
        }
    }
    return result__26
}

func float_rational_bits(numerator__65 FloatNatural, denominator__66 FloatNatural, binary_shift__67 int, mantissa_bits__68 int, exponent_bias__69 int) Tuple2_6uint64_4bool {
    var t1613 bool
    var inline2141 *_goml_vec_uint32 = numerator__65.words
    var inline2142 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(inline2141)
    t1613 = inline2142
    if t1613 {
        var t1614 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
            _0: 0,
            _1: false,
        }
        return t1614
    } else {
        var t1610 bool = binary_shift__67 >= 0
        var jp1535 FloatNatural
        if t1610 {
            var t1611 FloatNatural = float_natural_shift_left(numerator__65, binary_shift__67)
            jp1535 = t1611
        } else {
            var t1612 FloatNatural = float_natural_copy(numerator__65)
            jp1535 = t1612
        }
        var t1606 bool = binary_shift__67 >= 0
        var jp1537 FloatNatural
        if t1606 {
            var t1607 FloatNatural = float_natural_copy(denominator__66)
            jp1537 = t1607
        } else {
            var t1608 int = 0 - binary_shift__67
            var t1609 FloatNatural = float_natural_shift_left(denominator__66, t1608)
            jp1537 = t1609
        }
        var t1538 int = float_natural_bit_length(jp1535)
        var t1539 int = float_natural_bit_length(jp1537)
        var exponent__72 int = t1538 - t1539
        var t1600 bool = exponent__72 >= 0
        var jp1541 int
        if t1600 {
            var t1601 FloatNatural = float_natural_shift_left(jp1537, exponent__72)
            var t1602 int = float_natural_compare(jp1535, t1601)
            jp1541 = t1602
        } else {
            var t1603 int = 0 - exponent__72
            var t1604 FloatNatural = float_natural_shift_left(jp1535, t1603)
            var t1605 int = float_natural_compare(t1604, jp1537)
            jp1541 = t1605
        }
        var t1597 bool = jp1541 < 0
        if t1597 {
            var compound_old120 int = exponent__72
            var compound_value121 int = 1
            var t1598 int = compound_old120 - compound_value121
            exponent__72 = t1598
        } else {}
        var minimum_exponent__74 int = 1 - exponent_bias__69
        var t1591 bool = exponent__72 > exponent_bias__69
        if t1591 {
            var t1592 int = exponent_bias__69 + exponent_bias__69
            var t1593 int = t1592 + 1
            var t1594 uint64 = uint64(int(t1593))
            var t1595 uint64 = t1594 << mantissa_bits__68
            var t1596 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                _0: t1595,
                _1: true,
            }
            return t1596
        } else {
            var t1586 bool = exponent__72 < minimum_exponent__74
            var jp1545 uint64
            if t1586 {
                var t1587 int = mantissa_bits__68 - minimum_exponent__74
                var t1588 uint64 = float_rational_quotient(jp1535, jp1537, t1587)
                jp1545 = t1588
            } else {
                var t1589 int = mantissa_bits__68 - exponent__72
                var t1590 uint64 = float_rational_quotient(jp1535, jp1537, t1589)
                jp1545 = t1590
            }
            var mantissa__76 uint64 = jp1545
            var t1548 bool = exponent__72 < minimum_exponent__74
            if t1548 {
                var t1551 bool = mantissa__76 == 0
                if t1551 {
                    var t1552 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: 0,
                        _1: false,
                    }
                    return t1552
                } else {
                    var t1555_lhs uint64 = 1
                    var t1555 uint64 = t1555_lhs << mantissa_bits__68
                    var t1556 bool = mantissa__76 >= t1555
                    if t1556 {
                        var t1557_lhs uint64 = 1
                        var t1557 uint64 = t1557_lhs << mantissa_bits__68
                        var t1558_lhs uint64 = 1
                        var t1558 uint64 = t1558_lhs << mantissa_bits__68
                        var t1559 uint64 = mantissa__76 - t1558
                        var t1560 uint64 = t1557 | t1559
                        var t1561 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: t1560,
                            _1: false,
                        }
                        return t1561
                    } else {
                        var t1562 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                            _0: mantissa__76,
                            _1: false,
                        }
                        return t1562
                    }
                }
            } else {
                var t1579 int = mantissa_bits__68 + 1
                var t1580_lhs uint64 = 1
                var t1580 uint64 = t1580_lhs << t1579
                var t1581 bool = mantissa__76 >= t1580
                if t1581 {
                    var compound_old125 uint64 = mantissa__76
                    var compound_value126 int = 1
                    var t1582 uint64 = compound_old125 >> compound_value126
                    mantissa__76 = t1582
                    var compound_old128 int = exponent__72
                    var compound_value129 int = 1
                    var t1584 int = compound_old128 + compound_value129
                    exponent__72 = t1584
                } else {}
                var t1566 bool = exponent__72 > exponent_bias__69
                if t1566 {
                    var t1567 int = exponent_bias__69 + exponent_bias__69
                    var t1568 int = t1567 + 1
                    var t1569 uint64 = uint64(int(t1568))
                    var t1570 uint64 = t1569 << mantissa_bits__68
                    var t1571 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1570,
                        _1: true,
                    }
                    return t1571
                } else {
                    var t1572 int = exponent__72 + exponent_bias__69
                    var t1573 uint64 = uint64(int(t1572))
                    var t1574 uint64 = t1573 << mantissa_bits__68
                    var t1575_lhs uint64 = 1
                    var t1575 uint64 = t1575_lhs << mantissa_bits__68
                    var t1576 uint64 = mantissa__76 - t1575
                    var t1577 uint64 = t1574 | t1576
                    var t1578 Tuple2_6uint64_4bool = Tuple2_6uint64_4bool{
                        _0: t1577,
                        _1: false,
                    }
                    return t1578
                }
            }
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__u32(self__528 *_goml_vec_uint32) bool {
    var t1619 int = vec_len__Vec_6uint32(self__528)
    var t1620 bool = t1619 == 0
    return t1620
}

func float_natural_trim(value__7 FloatNatural) struct{} {
    Loop_loop1623:
    for {
        var t1631 *_goml_vec_uint32 = value__7.words
        var t1632 bool
        var inline2153 int = vec_len__Vec_6uint32(t1631)
        var inline2154 bool = inline2153 == 0
        t1632 = inline2154
        var t1633 bool = !t1632
        var jp1625 bool
        if t1633 {
            var t1634 *_goml_vec_uint32 = value__7.words
            var t1635 *_goml_vec_uint32 = value__7.words
            var t1636 int
            var inline2147 int = vec_len__Vec_6uint32(t1635)
            t1636 = inline2147
            var t1637 int = t1636 - 1
            var t1638 uint32 = vec_get__Vec_6uint32(t1634, t1637)
            var t1639 bool = t1638 == 0
            jp1625 = t1639
        } else {
            jp1625 = false
        }
        if jp1625 {
            var t1626 *_goml_vec_uint32 = value__7.words
            var t1627 *_goml_vec_uint32 = value__7.words
            var t1628 int
            var inline2151 int = vec_len__Vec_6uint32(t1627)
            t1628 = inline2151
            var t1629 int = t1628 - 1
            vec_truncate__Vec_6uint32(t1626, t1629)
            continue
        } else {
            break Loop_loop1623
        }
    }
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1648 bool = string_is_char_boundary(value__274, start__275)
    var jp1645 bool
    if t1648 {
        var t1649 bool = string_is_char_boundary(value__274, end__276)
        jp1645 = t1649
    } else {
        jp1645 = false
    }
    if jp1645 {
        var t1646 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1646
    } else {
        var t1647 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1647
    }
}

func string_equals_ascii_case(value__78 string, expected__79 string) bool {
    var t1664 int
    var inline2171 int = _goml_runtime_core_string_len(value__78)
    t1664 = inline2171
    var t1665 int
    var inline2169 int = _goml_runtime_core_string_len(expected__79)
    t1665 = inline2169
    var t1666 bool = t1664 != t1665
    if t1666 {
        return false
    } else {
        var index__80 int = 0
        var inline2161 uint8 = 97 - 65
        Loop_loop1654:
        for {
            var t1655 int
            var inline2167 int = _goml_runtime_core_string_len(value__78)
            t1655 = inline2167
            var t1656 bool = index__80 < t1655
            if t1656 {
                var t1660 uint8
                var inline2165 uint8 = _goml_runtime_core_string_byte_get(value__78, index__80)
                t1660 = inline2165
                var t1661 uint8
                var inline2158 bool = t1660 >= 65
                var inline2160 bool
                if inline2158 {
                    var inline2163 bool = t1660 <= 90
                    inline2160 = inline2163
                } else {
                    inline2160 = false
                }
                if inline2160 {
                    var inline2162 uint8 = t1660 + inline2161
                    t1661 = inline2162
                    var t1662 uint8
                    var inline2156 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1662 = inline2156
                    var t1663 bool = t1661 != t1662
                    if t1663 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1658 int = compound_old134 + compound_value135
                        index__80 = t1658
                        continue
                    }
                } else {
                    t1661 = t1660
                    var t1662 uint8
                    var inline2156 uint8 = _goml_runtime_core_string_byte_get(expected__79, index__80)
                    t1662 = inline2156
                    var t1663 bool = t1661 != t1662
                    if t1663 {
                        return false
                    } else {
                        var compound_old134 int = index__80
                        var compound_value135 int = 1
                        var t1658 int = compound_old134 + compound_value135
                        index__80 = t1658
                        continue
                    }
                }
            } else {
                break Loop_loop1654
            }
        }
        return true
    }
}

func ascii_lower(value__77 uint8) uint8 {
    var t1675 bool = value__77 >= 65
    var jp1672 bool
    if t1675 {
        var t1676 bool = value__77 <= 90
        jp1672 = t1676
    } else {
        jp1672 = false
    }
    if jp1672 {
        var t1673 uint8 = 97 - 65
        var t1674 uint8 = value__77 + t1673
        return t1674
    } else {
        return value__77
    }
}

func float_digit(value__81 uint8, base__82 int) Tuple2_4bool_3int {
    var t1703 bool = value__81 >= 48
    var jp1687 bool
    if t1703 {
        var t1704 bool = value__81 <= 57
        jp1687 = t1704
    } else {
        jp1687 = false
    }
    var jp1680 int
    if jp1687 {
        var t1688 uint8 = value__81 - 48
        var t1689 int = int(uint8(t1688))
        jp1680 = t1689
        var t1683 bool = jp1680 < base__82
        if t1683 {
            var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: true,
                _1: jp1680,
            }
            return t1684
        } else {
            var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                _0: false,
                _1: 0,
            }
            return t1685
        }
    } else {
        var t1699 uint8
        var inline2187 bool = value__81 >= 65
        var inline2189 bool
        if inline2187 {
            var inline2192 bool = value__81 <= 90
            inline2189 = inline2192
        } else {
            inline2189 = false
        }
        if inline2189 {
            var inline2190 uint8 = 97 - 65
            var inline2191 uint8 = value__81 + inline2190
            t1699 = inline2191
            var t1700 bool = t1699 >= 97
            var jp1693 bool
            if t1700 {
                var t1701 uint8
                var inline2173 bool = value__81 >= 65
                var inline2175 bool
                if inline2173 {
                    var inline2178 bool = value__81 <= 90
                    inline2175 = inline2178
                } else {
                    inline2175 = false
                }
                if inline2175 {
                    var inline2176 uint8 = 97 - 65
                    var inline2177 uint8 = value__81 + inline2176
                    t1701 = inline2177
                    var t1702 bool = t1701 <= 102
                    jp1693 = t1702
                    if jp1693 {
                        var t1694 uint8
                        var inline2180 bool = value__81 >= 65
                        var inline2182 bool
                        if inline2180 {
                            var inline2185 bool = value__81 <= 90
                            inline2182 = inline2185
                        } else {
                            inline2182 = false
                        }
                        if inline2182 {
                            var inline2183 uint8 = 97 - 65
                            var inline2184 uint8 = value__81 + inline2183
                            t1694 = inline2184
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        } else {
                            t1694 = value__81
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        }
                    } else {
                        var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1698
                    }
                } else {
                    t1701 = value__81
                    var t1702 bool = t1701 <= 102
                    jp1693 = t1702
                    if jp1693 {
                        var t1694 uint8
                        var inline2180 bool = value__81 >= 65
                        var inline2182 bool
                        if inline2180 {
                            var inline2185 bool = value__81 <= 90
                            inline2182 = inline2185
                        } else {
                            inline2182 = false
                        }
                        if inline2182 {
                            var inline2183 uint8 = 97 - 65
                            var inline2184 uint8 = value__81 + inline2183
                            t1694 = inline2184
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        } else {
                            t1694 = value__81
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        }
                    } else {
                        var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1698
                    }
                }
            } else {
                jp1693 = false
                if jp1693 {
                    var t1694 uint8
                    var inline2180 bool = value__81 >= 65
                    var inline2182 bool
                    if inline2180 {
                        var inline2185 bool = value__81 <= 90
                        inline2182 = inline2185
                    } else {
                        inline2182 = false
                    }
                    if inline2182 {
                        var inline2183 uint8 = 97 - 65
                        var inline2184 uint8 = value__81 + inline2183
                        t1694 = inline2184
                        var t1695 uint8 = t1694 - 97
                        var t1696 uint8 = t1695 + 10
                        var t1697 int = int(uint8(t1696))
                        jp1680 = t1697
                        var t1683 bool = jp1680 < base__82
                        if t1683 {
                            var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1680,
                            }
                            return t1684
                        } else {
                            var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1685
                        }
                    } else {
                        t1694 = value__81
                        var t1695 uint8 = t1694 - 97
                        var t1696 uint8 = t1695 + 10
                        var t1697 int = int(uint8(t1696))
                        jp1680 = t1697
                        var t1683 bool = jp1680 < base__82
                        if t1683 {
                            var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1680,
                            }
                            return t1684
                        } else {
                            var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1685
                        }
                    }
                } else {
                    var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1698
                }
            }
        } else {
            t1699 = value__81
            var t1700 bool = t1699 >= 97
            var jp1693 bool
            if t1700 {
                var t1701 uint8
                var inline2173 bool = value__81 >= 65
                var inline2175 bool
                if inline2173 {
                    var inline2178 bool = value__81 <= 90
                    inline2175 = inline2178
                } else {
                    inline2175 = false
                }
                if inline2175 {
                    var inline2176 uint8 = 97 - 65
                    var inline2177 uint8 = value__81 + inline2176
                    t1701 = inline2177
                    var t1702 bool = t1701 <= 102
                    jp1693 = t1702
                    if jp1693 {
                        var t1694 uint8
                        var inline2180 bool = value__81 >= 65
                        var inline2182 bool
                        if inline2180 {
                            var inline2185 bool = value__81 <= 90
                            inline2182 = inline2185
                        } else {
                            inline2182 = false
                        }
                        if inline2182 {
                            var inline2183 uint8 = 97 - 65
                            var inline2184 uint8 = value__81 + inline2183
                            t1694 = inline2184
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        } else {
                            t1694 = value__81
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        }
                    } else {
                        var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1698
                    }
                } else {
                    t1701 = value__81
                    var t1702 bool = t1701 <= 102
                    jp1693 = t1702
                    if jp1693 {
                        var t1694 uint8
                        var inline2180 bool = value__81 >= 65
                        var inline2182 bool
                        if inline2180 {
                            var inline2185 bool = value__81 <= 90
                            inline2182 = inline2185
                        } else {
                            inline2182 = false
                        }
                        if inline2182 {
                            var inline2183 uint8 = 97 - 65
                            var inline2184 uint8 = value__81 + inline2183
                            t1694 = inline2184
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        } else {
                            t1694 = value__81
                            var t1695 uint8 = t1694 - 97
                            var t1696 uint8 = t1695 + 10
                            var t1697 int = int(uint8(t1696))
                            jp1680 = t1697
                            var t1683 bool = jp1680 < base__82
                            if t1683 {
                                var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: true,
                                    _1: jp1680,
                                }
                                return t1684
                            } else {
                                var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                    _0: false,
                                    _1: 0,
                                }
                                return t1685
                            }
                        }
                    } else {
                        var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                            _0: false,
                            _1: 0,
                        }
                        return t1698
                    }
                }
            } else {
                jp1693 = false
                if jp1693 {
                    var t1694 uint8
                    var inline2180 bool = value__81 >= 65
                    var inline2182 bool
                    if inline2180 {
                        var inline2185 bool = value__81 <= 90
                        inline2182 = inline2185
                    } else {
                        inline2182 = false
                    }
                    if inline2182 {
                        var inline2183 uint8 = 97 - 65
                        var inline2184 uint8 = value__81 + inline2183
                        t1694 = inline2184
                        var t1695 uint8 = t1694 - 97
                        var t1696 uint8 = t1695 + 10
                        var t1697 int = int(uint8(t1696))
                        jp1680 = t1697
                        var t1683 bool = jp1680 < base__82
                        if t1683 {
                            var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1680,
                            }
                            return t1684
                        } else {
                            var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1685
                        }
                    } else {
                        t1694 = value__81
                        var t1695 uint8 = t1694 - 97
                        var t1696 uint8 = t1695 + 10
                        var t1697 int = int(uint8(t1696))
                        jp1680 = t1697
                        var t1683 bool = jp1680 < base__82
                        if t1683 {
                            var t1684 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: true,
                                _1: jp1680,
                            }
                            return t1684
                        } else {
                            var t1685 Tuple2_4bool_3int = Tuple2_4bool_3int{
                                _0: false,
                                _1: 0,
                            }
                            return t1685
                        }
                    }
                } else {
                    var t1698 Tuple2_4bool_3int = Tuple2_4bool_3int{
                        _0: false,
                        _1: 0,
                    }
                    return t1698
                }
            }
        }
    }
}

func float_natural_add_small(value__20 FloatNatural, addition__21 uint32) struct{} {
    var carry__22 uint64 = uint64(uint32(addition__21))
    var index__23 int = 0
    Loop_loop1707:
    for {
        var t1708 bool = carry__22 != 0
        if t1708 {
            var t1717 *_goml_vec_uint32 = value__20.words
            var t1718 int
            var inline2197 int = vec_len__Vec_6uint32(t1717)
            t1718 = inline2197
            var t1719 bool = index__23 == t1718
            if t1719 {
                var t1720 *_goml_vec_uint32 = value__20.words
                var inline2194 uint32 = 0
                vec_push__Vec_6uint32(t1720, inline2194)
            } else {}
            var t1710 *_goml_vec_uint32 = value__20.words
            var t1711 uint32 = vec_get__Vec_6uint32(t1710, index__23)
            var t1712 uint64 = uint64(uint32(t1711))
            var sum__24 uint64 = t1712 + carry__22
            var place36 *_goml_vec_uint32 = value__20.words
            var index37 int = index__23
            vec_get__Vec_6uint32(place36, index37)
            var value39 uint32 = uint32(uint64(sum__24))
            vec_set__Vec_6uint32(place36, index37, value39)
            var t1714_rhs int = 32
            var t1714 uint64 = sum__24 >> t1714_rhs
            carry__22 = t1714
            var compound_old42 int = index__23
            var compound_value43 int = 1
            var t1715 int = compound_old42 + compound_value43
            index__23 = t1715
            continue
        } else {
            break Loop_loop1707
        }
    }
    return struct{}{}
}

func invalid_parsed_float() ParsedFloat {
    var t1724 FloatNatural
    var inline2199 *_goml_vec_uint32 = vec_new__Vec_6uint32()
    var inline2200 FloatNatural = FloatNatural{
        words: inline2199,
    }
    t1724 = inline2200
    var t1725 ParsedFloat = ParsedFloat{
        valid: false,
        negative: false,
        special: 0,
        numerator: t1724,
        decimal_exponent: 0,
        binary_exponent: 0,
        hexadecimal: false,
        significant_digits: 0,
    }
    return t1725
}

func float_natural_bit_length(value__9 FloatNatural) int {
    var t1745 *_goml_vec_uint32 = value__9.words
    var t1746 bool
    var inline2206 int = vec_len__Vec_6uint32(t1745)
    var inline2207 bool = inline2206 == 0
    t1746 = inline2207
    if t1746 {
        return 0
    } else {
        var t1729 *_goml_vec_uint32 = value__9.words
        var t1730 *_goml_vec_uint32 = value__9.words
        var t1731 int
        var inline2204 int = vec_len__Vec_6uint32(t1730)
        t1731 = inline2204
        var t1732 int = t1731 - 1
        var high__10 uint32 = vec_get__Vec_6uint32(t1729, t1732)
        var bits__11 int = 0
        Loop_loop1739:
        for {
            var t1740 bool = high__10 != 0
            if t1740 {
                var compound_old9 uint32 = high__10
                var compound_value10 int = 1
                var t1741 uint32 = compound_old9 >> compound_value10
                high__10 = t1741
                var compound_old12 int = bits__11
                var compound_value13 int = 1
                var t1743 int = compound_old12 + compound_value13
                bits__11 = t1743
                continue
            } else {
                break Loop_loop1739
            }
        }
        var t1734 *_goml_vec_uint32 = value__9.words
        var t1735 int
        var inline2202 int = vec_len__Vec_6uint32(t1734)
        t1735 = inline2202
        var t1736 int = t1735 - 1
        var t1737 int = t1736 * 32
        var t1738 int = t1737 + bits__11
        return t1738
    }
}

func float_natural_compare(left__12 FloatNatural, right__13 FloatNatural) int {
    var t1768 *_goml_vec_uint32 = left__12.words
    var t1769 int
    var inline2217 int = vec_len__Vec_6uint32(t1768)
    t1769 = inline2217
    var t1770 *_goml_vec_uint32 = right__13.words
    var t1771 int
    var inline2215 int = vec_len__Vec_6uint32(t1770)
    t1771 = inline2215
    var t1772 bool = t1769 < t1771
    if t1772 {
        return -1
    } else {
        var t1774 *_goml_vec_uint32 = left__12.words
        var t1775 int
        var inline2211 int = vec_len__Vec_6uint32(t1774)
        t1775 = inline2211
        var t1776 *_goml_vec_uint32 = right__13.words
        var t1777 int
        var inline2209 int = vec_len__Vec_6uint32(t1776)
        t1777 = inline2209
        var t1778 bool = t1775 > t1777
        if t1778 {
            return 1
        } else {
            var t1750 *_goml_vec_uint32 = left__12.words
            var index__14 int
            var inline2213 int = vec_len__Vec_6uint32(t1750)
            index__14 = inline2213
            Loop_loop1752:
            for {
                var t1753 bool = index__14 > 0
                if t1753 {
                    var compound_old17 int = index__14
                    var compound_value18 int = 1
                    var t1754 int = compound_old17 - compound_value18
                    index__14 = t1754
                    var t1757 *_goml_vec_uint32 = left__12.words
                    var t1758 uint32 = vec_get__Vec_6uint32(t1757, index__14)
                    var t1759 *_goml_vec_uint32 = right__13.words
                    var t1760 uint32 = vec_get__Vec_6uint32(t1759, index__14)
                    var t1761 bool = t1758 < t1760
                    if t1761 {
                        return -1
                    } else {
                        var t1763 *_goml_vec_uint32 = left__12.words
                        var t1764 uint32 = vec_get__Vec_6uint32(t1763, index__14)
                        var t1765 *_goml_vec_uint32 = right__13.words
                        var t1766 uint32 = vec_get__Vec_6uint32(t1765, index__14)
                        var t1767 bool = t1764 > t1766
                        if t1767 {
                            return 1
                        } else {
                            continue
                        }
                    }
                } else {
                    break Loop_loop1752
                }
            }
            return 0
        }
    }
}

func float_rational_quotient(numerator__55 FloatNatural, denominator__56 FloatNatural, shift__57 int) uint64 {
    var t1814 bool = shift__57 >= 0
    var jp1782 FloatNatural
    if t1814 {
        var t1815 FloatNatural = float_natural_shift_left(numerator__55, shift__57)
        jp1782 = t1815
    } else {
        var t1816 FloatNatural = float_natural_copy(numerator__55)
        jp1782 = t1816
    }
    var t1810 bool = shift__57 >= 0
    var jp1784 FloatNatural
    if t1810 {
        var t1811 FloatNatural = float_natural_copy(denominator__56)
        jp1784 = t1811
    } else {
        var t1812 int = 0 - shift__57
        var t1813 FloatNatural = float_natural_shift_left(denominator__56, t1812)
        jp1784 = t1813
    }
    var quotient__60 uint64 = 0
    Loop_loop1797:
    for {
        var t1798 int = float_natural_compare(jp1782, jp1784)
        var t1799 bool = t1798 >= 0
        if t1799 {
            var t1800 int = float_natural_bit_length(jp1782)
            var t1801 int = float_natural_bit_length(jp1784)
            var offset__61 int = t1800 - t1801
            var part__62 FloatNatural = float_natural_shift_left(jp1784, offset__61)
            var t1805 int = float_natural_compare(jp1782, part__62)
            var t1806 bool = t1805 < 0
            if t1806 {
                var compound_old105 int = offset__61
                var compound_value106 int = 1
                var t1807 int = compound_old105 - compound_value106
                offset__61 = t1807
                var t1809 FloatNatural = float_natural_shift_left(jp1784, offset__61)
                part__62 = t1809
            } else {}
            float_natural_subtract(jp1782, part__62)
            var compound_old111 uint64 = quotient__60
            var compound_value112_lhs uint64 = 1
            var compound_value112 uint64 = compound_value112_lhs << offset__61
            var t1803 uint64 = compound_old111 | compound_value112
            quotient__60 = t1803
            continue
        } else {
            break Loop_loop1797
        }
    }
    var doubled__63 FloatNatural = float_natural_shift_left(jp1782, 1)
    var rounding__64 int = float_natural_compare(doubled__63, jp1784)
    var t1791 bool = rounding__64 > 0
    var jp1788 bool
    if t1791 {
        jp1788 = true
    } else {
        var t1794 bool = rounding__64 == 0
        if t1794 {
            var t1795_rhs uint64 = 1
            var t1795 uint64 = quotient__60 & t1795_rhs
            var t1796 bool = t1795 == 1
            jp1788 = t1796
        } else {
            jp1788 = false
        }
    }
    if jp1788 {
        var compound_old115 uint64 = quotient__60
        var compound_value116 uint64 = 1
        var t1789 uint64 = compound_old115 + compound_value116
        quotient__60 = t1789
    } else {}
    return quotient__60
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__u32(self__531 *_goml_vec_uint32, len__532 int) struct{} {
    vec_truncate__Vec_6uint32(self__531, len__532)
    return struct{}{}
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1832 bool = index__269 < 0
    var jp1824 bool
    if t1832 {
        jp1824 = true
    } else {
        var t1833 int
        var inline2219 int = _goml_runtime_core_string_len(value__268)
        t1833 = inline2219
        var t1834 bool = index__269 > t1833
        jp1824 = t1834
    }
    if jp1824 {
        return false
    } else {
        var t1827 int
        var inline2223 int = _goml_runtime_core_string_len(value__268)
        t1827 = inline2223
        var t1828 bool = index__269 == t1827
        if t1828 {
            return true
        } else {
            var t1829 uint8
            var inline2221 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1829 = inline2221
            var t1830_rhs uint8 = 192
            var t1830 uint8 = t1829 & t1830_rhs
            var t1831 bool = t1830 != 128
            return t1831
        }
    }
}

func float_natural_subtract(value__37 FloatNatural, other__38 FloatNatural) struct{} {
    var base__39 uint64 = 4294967296
    var borrow__40 uint64 = 0
    var index__41 int = 0
    Loop_loop1838:
    for {
        var t1839 *_goml_vec_uint32 = value__37.words
        var t1840 int
        var inline2227 int = vec_len__Vec_6uint32(t1839)
        t1840 = inline2227
        var t1841 bool = index__41 < t1840
        if t1841 {
            var t1855 *_goml_vec_uint32 = other__38.words
            var t1856 int
            var inline2225 int = vec_len__Vec_6uint32(t1855)
            t1856 = inline2225
            var t1857 bool = index__41 < t1856
            var jp1843 uint64
            if t1857 {
                var t1858 *_goml_vec_uint32 = other__38.words
                var t1859 uint32 = vec_get__Vec_6uint32(t1858, index__41)
                var t1860 uint64 = uint64(uint32(t1859))
                jp1843 = t1860
            } else {
                jp1843 = 0
            }
            var right__42 uint64 = jp1843 + borrow__40
            var t1844 *_goml_vec_uint32 = value__37.words
            var t1845 uint32 = vec_get__Vec_6uint32(t1844, index__41)
            var left__43 uint64 = uint64(uint32(t1845))
            var t1849 bool = left__43 >= right__42
            if t1849 {
                var place65 *_goml_vec_uint32 = value__37.words
                var index66 int = index__41
                vec_get__Vec_6uint32(place65, index66)
                var t1850 uint64 = left__43 - right__42
                var value68 uint32 = uint32(uint64(t1850))
                vec_set__Vec_6uint32(place65, index66, value68)
                borrow__40 = 0
            } else {
                var place72 *_goml_vec_uint32 = value__37.words
                var index73 int = index__41
                vec_get__Vec_6uint32(place72, index73)
                var t1852 uint64 = base__39 + left__43
                var t1853 uint64 = t1852 - right__42
                var value75 uint32 = uint32(uint64(t1853))
                vec_set__Vec_6uint32(place72, index73, value75)
                borrow__40 = 1
            }
            var compound_old79 int = index__41
            var compound_value80 int = 1
            var t1847 int = compound_old79 + compound_value80
            index__41 = t1847
            continue
        } else {
            break Loop_loop1838
        }
    }
    float_natural_trim(value__37)
    return struct{}{}
}

func main() {
    main0()
}
