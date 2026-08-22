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

func optimized(base__0 int, count__1 int) int {
    var index__2 int = 0
    var result__3 int = 0
    var square__4 int = base__0 * base__0
    var scaled__5 int = square__4 * 17
    var offset__6 int = scaled__5 + base__0
    Loop_loop812:
    for {
        var t813 bool = index__2 < count__1
        if t813 {
            var t814 int = result__3 + offset__6
            result__3 = t814
            var t815 int = index__2 + 1
            index__2 = t815
            continue
        } else {
            break Loop_loop812
        }
    }
    return result__3
}

func guarded(divisor__7 int, count__8 int) int {
    var index__9 int = 0
    var result__10 int = 0
    Loop_loop819:
    for {
        var t820 bool = index__9 < count__8
        if t820 {
            var quotient__11 int = 100 / divisor__7
            var t821 int = result__10 + quotient__11
            result__10 = t821
            var t822 int = index__9 + 1
            index__9 = t822
            continue
        } else {
            break Loop_loop819
        }
    }
    return result__10
}

func changing(count__12 int) int {
    var index__13 int = 0
    var value__14 int = 1
    var result__15 int = 0
    Loop_loop826:
    for {
        var t827 bool = index__13 < count__12
        if t827 {
            var derived__16 int = value__14 + 1
            var t828 int = result__15 + derived__16
            result__15 = t828
            var t829 int = value__14 + 1
            value__14 = t829
            var t830 int = index__13 + 1
            index__13 = t830
            continue
        } else {
            break Loop_loop826
        }
    }
    return result__15
}

func main0() struct{} {
    var t832 int = optimized(3, 4)
    var inline891 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t832)
    _goml_runtime_core_string_println(inline891)
    var t833 int = guarded(0, 0)
    var inline888 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t833)
    _goml_runtime_core_string_println(inline888)
    var t834 int = changing(3)
    var inline885 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t834)
    _goml_runtime_core_string_println(inline885)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline896 int64 = int64(int(self__404))
    var inline897 string = signed_decimal_string(inline896)
    return inline897
}

func signed_decimal_string(value__214 int64) string {
    var t849 bool = value__214 < 0
    if t849 {
        var t850 uint64 = uint64(int64(value__214))
        var t851 uint64 = 0 - t850
        var t852 string = decimal_string(t851)
        var t853 string = "-" + t852
        return t853
    } else {
        var t854 uint64 = uint64(int64(value__214))
        var t855 string = decimal_string(t854)
        return t855
    }
}

func decimal_string(value__208 uint64) string {
    var t878 bool = value__208 == 0
    if t878 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop871:
        for {
            var t872 bool = remaining__210 > 0
            if t872 {
                var t873_rhs uint64 = 10
                var t873 uint64 = remaining__210 % t873_rhs
                var t874 uint8 = uint8(uint64(t873))
                var t875 uint8 = t874 + 48
                vec_push__Vec_5uint8(reversed__209, t875)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t876 uint64 = compound_old353 / compound_value354
                remaining__210 = t876
                continue
            } else {
                break Loop_loop871
            }
        }
        var t860 int
        var inline915 int = vec_len__Vec_5uint8(reversed__209)
        t860 = inline915
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t860)
        var offset__212 int = 0
        Loop_loop862:
        for {
            var t863 int
            var inline913 int = vec_len__Vec_5uint8(reversed__209)
            t863 = inline913
            var t864 bool = offset__212 < t863
            if t864 {
                var t865 int
                var inline911 int = vec_len__Vec_5uint8(reversed__209)
                t865 = inline911
                var t866 int = t865 - offset__212
                var t867 int = t866 - 1
                var t868 uint8 = vec_get__Vec_5uint8(reversed__209, t867)
                vec_push__Vec_5uint8(bytes__211, t868)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t869 int = compound_old358 + compound_value359
                offset__212 = t869
                continue
            } else {
                break Loop_loop862
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
