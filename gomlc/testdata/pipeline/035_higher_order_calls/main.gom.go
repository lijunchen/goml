package main

import (
    _goml_os "os"
)

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

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int) func(int32) int32 {
    return arr[index]
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

func double(x__0 int32) int32 {
    var t801 int32 = x__0 * 2
    return t801
}

func increment(x__1 int32) int32 {
    var t804 int32 = x__1 + 1
    return t804
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t810 int32 = f__4(10)
    var t811 int32 = g__5(t810)
    var inline887 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t811)
    _goml_runtime_core_string_println(inline887)
    var chosen__6 func(int32) int32
    var inline885 bool = true
    if inline885 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t812 func(int32) int32
    var inline883 bool = false
    if inline883 {
        t812 = double
    } else {
        t812 = increment
    }
    var direct__8 int32 = t812(5)
    var t813 string
    var inline881 string = __goml_builtin_int32_to_string(applied__7)
    t813 = inline881
    var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline878)
    var t814 string
    var inline876 string = __goml_builtin_int32_to_string(direct__8)
    t814 = inline876
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline896 int64 = int64(int32(self__407))
    var inline897 string = signed_decimal_string(inline896)
    return inline897
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t831 int64 = int64(int32(value__225))
    var inline899 bool = t831 < 0
    if inline899 {
        var inline900 uint64 = uint64(int64(t831))
        var inline901 uint64 = 0 - inline900
        var inline902 string = decimal_string(inline901)
        var inline903 string = "-" + inline902
        return inline903
    } else {
        var inline904 uint64 = uint64(int64(t831))
        var inline905 string = decimal_string(inline904)
        return inline905
    }
}

func signed_decimal_string(value__214 int64) string {
    var t837 bool = value__214 < 0
    if t837 {
        var t838 uint64 = uint64(int64(value__214))
        var t839 uint64 = 0 - t838
        var t840 string = decimal_string(t839)
        var t841 string = "-" + t840
        return t841
    } else {
        var t842 uint64 = uint64(int64(value__214))
        var t843 string = decimal_string(t842)
        return t843
    }
}

func decimal_string(value__208 uint64) string {
    var t866 bool = value__208 == 0
    if t866 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop859:
        for {
            var t860 bool = remaining__210 > 0
            if t860 {
                var t861_rhs uint64 = 10
                var t861 uint64 = remaining__210 % t861_rhs
                var t862 uint8 = uint8(uint64(t861))
                var t863 uint8 = t862 + 48
                vec_push__Vec_5uint8(reversed__209, t863)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t864 uint64 = compound_old353 / compound_value354
                remaining__210 = t864
                continue
            } else {
                break Loop_loop859
            }
        }
        var t848 int
        var inline915 int = vec_len__Vec_5uint8(reversed__209)
        t848 = inline915
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t848)
        var offset__212 int = 0
        Loop_loop850:
        for {
            var t851 int
            var inline913 int = vec_len__Vec_5uint8(reversed__209)
            t851 = inline913
            var t852 bool = offset__212 < t851
            if t852 {
                var t853 int
                var inline911 int = vec_len__Vec_5uint8(reversed__209)
                t853 = inline911
                var t854 int = t853 - offset__212
                var t855 int = t854 - 1
                var t856 uint8 = vec_get__Vec_5uint8(reversed__209, t855)
                vec_push__Vec_5uint8(bytes__211, t856)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t857 int = compound_old358 + compound_value359
                offset__212 = t857
                continue
            } else {
                break Loop_loop850
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
