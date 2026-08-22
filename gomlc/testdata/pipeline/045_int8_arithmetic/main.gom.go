package main

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

type _goml_vec_uint8 struct {
    items []uint8
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

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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
    var a__4 int8
    a__4 = 90
    var b__5 int8
    b__5 = -20
    var c__6 int8
    c__6 = 3
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    var inline925 string = "a="
    var inline926 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(a__4)
    var inline927 string = inline925 + inline926
    println__T_string(inline927)
    var inline920 string = "b="
    var inline921 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(b__5)
    var inline922 string = inline920 + inline921
    println__T_string(inline922)
    var inline915 string = "c="
    var inline916 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(c__6)
    var inline917 string = inline915 + inline916
    println__T_string(inline917)
    var inline910 string = "sum="
    var inline911 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(sum__7)
    var inline912 string = inline910 + inline911
    println__T_string(inline912)
    var inline905 string = "diff="
    var inline906 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(diff__8)
    var inline907 string = inline905 + inline906
    println__T_string(inline907)
    var inline900 string = "prod="
    var inline901 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(prod__9)
    var inline902 string = inline900 + inline901
    println__T_string(inline902)
    var inline895 string = "quot="
    var inline896 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(quot__10)
    var inline897 string = inline895 + inline896
    println__T_string(inline897)
    var inline890 string = "neg="
    var inline891 string = _goml_m_trait__impl_i_ToString_i_i8_i_to__string(neg__11)
    var inline892 string = inline890 + inline891
    println__T_string(inline892)
    var inline885 string = "b<a="
    var inline886 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline887 string = inline885 + inline886
    println__T_string(inline887)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t821 string
    t821 = value__1
    _goml_runtime_core_string_println(t821)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i8_i_to__string(self__405 int8) string {
    var inline934 int64 = int64(int8(self__405))
    var inline935 string = signed_decimal_string(inline934)
    return inline935
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t828 string = _goml_runtime_core_bool_to_string(self__401)
    return t828
}

func signed_decimal_string(value__214 int64) string {
    var t839 bool = value__214 < 0
    if t839 {
        var t840 uint64 = uint64(int64(value__214))
        var t841 uint64 = 0 - t840
        var t842 string = decimal_string(t841)
        var t843 string = "-" + t842
        return t843
    } else {
        var t844 uint64 = uint64(int64(value__214))
        var t845 string = decimal_string(t844)
        return t845
    }
}

func decimal_string(value__208 uint64) string {
    var t868 bool = value__208 == 0
    if t868 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop861:
        for {
            var t862 bool = remaining__210 > 0
            if t862 {
                var t863_rhs uint64 = 10
                var t863 uint64 = remaining__210 % t863_rhs
                var t864 uint8 = uint8(uint64(t863))
                var t865 uint8 = t864 + 48
                vec_push__Vec_5uint8(reversed__209, t865)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t866 uint64 = compound_old353 / compound_value354
                remaining__210 = t866
                continue
            } else {
                break Loop_loop861
            }
        }
        var t850 int
        var inline953 int = vec_len__Vec_5uint8(reversed__209)
        t850 = inline953
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t850)
        var offset__212 int = 0
        Loop_loop852:
        for {
            var t853 int
            var inline951 int = vec_len__Vec_5uint8(reversed__209)
            t853 = inline951
            var t854 bool = offset__212 < t853
            if t854 {
                var t855 int
                var inline949 int = vec_len__Vec_5uint8(reversed__209)
                t855 = inline949
                var t856 int = t855 - offset__212
                var t857 int = t856 - 1
                var t858 uint8 = vec_get__Vec_5uint8(reversed__209, t857)
                vec_push__Vec_5uint8(bytes__211, t858)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t859 int = compound_old358 + compound_value359
                offset__212 = t859
                continue
            } else {
                break Loop_loop852
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
