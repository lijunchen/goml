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

func main0() struct{} {
    var x__0 int = 1
    var t799 int = x__0 + 1
    x__0 = t799
    var t800 string
    var inline856 string = __goml_builtin_int_to_string(x__0)
    t800 = inline856
    var inline853 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline853)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t811 int64 = int64(int(value__222))
    var inline862 bool = t811 < 0
    if inline862 {
        var inline863 uint64 = uint64(int64(t811))
        var inline864 uint64 = 0 - inline863
        var inline865 string = decimal_string(inline864)
        var inline866 string = "-" + inline865
        return inline866
    } else {
        var inline867 uint64 = uint64(int64(t811))
        var inline868 string = decimal_string(inline867)
        return inline868
    }
}

func decimal_string(value__208 uint64) string {
    var t846 bool = value__208 == 0
    if t846 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop839:
        for {
            var t840 bool = remaining__210 > 0
            if t840 {
                var t841_rhs uint64 = 10
                var t841 uint64 = remaining__210 % t841_rhs
                var t842 uint8 = uint8(uint64(t841))
                var t843 uint8 = t842 + 48
                vec_push__Vec_5uint8(reversed__209, t843)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t844 uint64 = compound_old353 / compound_value354
                remaining__210 = t844
                continue
            } else {
                break Loop_loop839
            }
        }
        var t828 int
        var inline878 int = vec_len__Vec_5uint8(reversed__209)
        t828 = inline878
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t828)
        var offset__212 int = 0
        Loop_loop830:
        for {
            var t831 int
            var inline876 int = vec_len__Vec_5uint8(reversed__209)
            t831 = inline876
            var t832 bool = offset__212 < t831
            if t832 {
                var t833 int
                var inline874 int = vec_len__Vec_5uint8(reversed__209)
                t833 = inline874
                var t834 int = t833 - offset__212
                var t835 int = t834 - 1
                var t836 uint8 = vec_get__Vec_5uint8(reversed__209, t835)
                vec_push__Vec_5uint8(bytes__211, t836)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t837 int = compound_old358 + compound_value359
                offset__212 = t837
                continue
            } else {
                break Loop_loop830
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
