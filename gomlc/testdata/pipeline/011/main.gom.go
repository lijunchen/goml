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
    var a__0 int = 1
    var a__1 int = a__0 + 2
    var a__2 int = a__1 + 3
    var a__3 int = a__2 + 4
    var t797 string
    var inline854 string = __goml_builtin_int_to_string(a__3)
    t797 = inline854
    var inline851 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t797)
    _goml_runtime_core_string_println(inline851)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t809 int64 = int64(int(value__222))
    var inline860 bool = t809 < 0
    if inline860 {
        var inline861 uint64 = uint64(int64(t809))
        var inline862 uint64 = 0 - inline861
        var inline863 string = decimal_string(inline862)
        var inline864 string = "-" + inline863
        return inline864
    } else {
        var inline865 uint64 = uint64(int64(t809))
        var inline866 string = decimal_string(inline865)
        return inline866
    }
}

func decimal_string(value__208 uint64) string {
    var t844 bool = value__208 == 0
    if t844 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop837:
        for {
            var t838 bool = remaining__210 > 0
            if t838 {
                var t839_rhs uint64 = 10
                var t839 uint64 = remaining__210 % t839_rhs
                var t840 uint8 = uint8(uint64(t839))
                var t841 uint8 = t840 + 48
                vec_push__Vec_5uint8(reversed__209, t841)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t842 uint64 = compound_old353 / compound_value354
                remaining__210 = t842
                continue
            } else {
                break Loop_loop837
            }
        }
        var t826 int
        var inline876 int = vec_len__Vec_5uint8(reversed__209)
        t826 = inline876
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t826)
        var offset__212 int = 0
        Loop_loop828:
        for {
            var t829 int
            var inline874 int = vec_len__Vec_5uint8(reversed__209)
            t829 = inline874
            var t830 bool = offset__212 < t829
            if t830 {
                var t831 int
                var inline872 int = vec_len__Vec_5uint8(reversed__209)
                t831 = inline872
                var t832 int = t831 - offset__212
                var t833 int = t832 - 1
                var t834 uint8 = vec_get__Vec_5uint8(reversed__209, t833)
                vec_push__Vec_5uint8(bytes__211, t834)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t835 int = compound_old358 + compound_value359
                offset__212 = t835
                continue
            } else {
                break Loop_loop828
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
