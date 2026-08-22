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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

func _goml_m_trait__impl_i_Measure_i_Vec_l_i32_r__i_measure(self__0 *_goml_vec_int32) int {
    var inline861 int = vec_len__Vec_5int32(self__0)
    return inline861
}

func main0() struct{} {
    var values__2 *_goml_vec_int32 = vec_new__Vec_5int32()
    var t800 int
    var inline868 int = _goml_m_trait__impl_i_Measure_i_Vec_l_i32_r__i_measure(values__2)
    t800 = inline868
    var t801 string
    var inline866 string = __goml_builtin_int_to_string(t800)
    t801 = inline866
    var inline863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
    _goml_runtime_core_string_println(inline863)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t819 int64 = int64(int(value__222))
    var inline876 bool = t819 < 0
    if inline876 {
        var inline877 uint64 = uint64(int64(t819))
        var inline878 uint64 = 0 - inline877
        var inline879 string = decimal_string(inline878)
        var inline880 string = "-" + inline879
        return inline880
    } else {
        var inline881 uint64 = uint64(int64(t819))
        var inline882 string = decimal_string(inline881)
        return inline882
    }
}

func decimal_string(value__208 uint64) string {
    var t854 bool = value__208 == 0
    if t854 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop847:
        for {
            var t848 bool = remaining__210 > 0
            if t848 {
                var t849_rhs uint64 = 10
                var t849 uint64 = remaining__210 % t849_rhs
                var t850 uint8 = uint8(uint64(t849))
                var t851 uint8 = t850 + 48
                vec_push__Vec_5uint8(reversed__209, t851)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t852 uint64 = compound_old353 / compound_value354
                remaining__210 = t852
                continue
            } else {
                break Loop_loop847
            }
        }
        var t836 int
        var inline892 int = vec_len__Vec_5uint8(reversed__209)
        t836 = inline892
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t836)
        var offset__212 int = 0
        Loop_loop838:
        for {
            var t839 int
            var inline890 int = vec_len__Vec_5uint8(reversed__209)
            t839 = inline890
            var t840 bool = offset__212 < t839
            if t840 {
                var t841 int
                var inline888 int = vec_len__Vec_5uint8(reversed__209)
                t841 = inline888
                var t842 int = t841 - offset__212
                var t843 int = t842 - 1
                var t844 uint8 = vec_get__Vec_5uint8(reversed__209, t843)
                vec_push__Vec_5uint8(bytes__211, t844)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t845 int = compound_old358 + compound_value359
                offset__212 = t845
                continue
            } else {
                break Loop_loop838
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
