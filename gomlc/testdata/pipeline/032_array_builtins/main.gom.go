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

func array_get__Array_3_5int32(arr [3]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_3_5int32(arr [3]int32, index int, value int32) [3]int32 {
    arr[index] = value
    return arr
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
    var arr__3 [3]int32 = [3]int32{1, 2, 3}
    var updated__4 [3]int32
    var inline870 [3]int32 = arr__3
    var inline871 [3]int32 = inline870
    var inline872 int = 1
    array_get__Array_3_5int32(inline871, inline872)
    var inline874 int32 = 42
    var inline875 [3]int32 = array_set__Array_3_5int32(inline871, inline872, inline874)
    inline870 = inline875
    updated__4 = inline870
    var value__5 int32
    var inline868 int32 = array_get__Array_3_5int32(updated__4, 1)
    value__5 = inline868
    var t810 string
    var inline866 string = __goml_builtin_int32_to_string(value__5)
    t810 = inline866
    var inline863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline863)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t821 int64 = int64(int32(value__225))
    var inline883 bool = t821 < 0
    if inline883 {
        var inline884 uint64 = uint64(int64(t821))
        var inline885 uint64 = 0 - inline884
        var inline886 string = decimal_string(inline885)
        var inline887 string = "-" + inline886
        return inline887
    } else {
        var inline888 uint64 = uint64(int64(t821))
        var inline889 string = decimal_string(inline888)
        return inline889
    }
}

func decimal_string(value__208 uint64) string {
    var t856 bool = value__208 == 0
    if t856 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop849:
        for {
            var t850 bool = remaining__210 > 0
            if t850 {
                var t851_rhs uint64 = 10
                var t851 uint64 = remaining__210 % t851_rhs
                var t852 uint8 = uint8(uint64(t851))
                var t853 uint8 = t852 + 48
                vec_push__Vec_5uint8(reversed__209, t853)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t854 uint64 = compound_old353 / compound_value354
                remaining__210 = t854
                continue
            } else {
                break Loop_loop849
            }
        }
        var t838 int
        var inline899 int = vec_len__Vec_5uint8(reversed__209)
        t838 = inline899
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t838)
        var offset__212 int = 0
        Loop_loop840:
        for {
            var t841 int
            var inline897 int = vec_len__Vec_5uint8(reversed__209)
            t841 = inline897
            var t842 bool = offset__212 < t841
            if t842 {
                var t843 int
                var inline895 int = vec_len__Vec_5uint8(reversed__209)
                t843 = inline895
                var t844 int = t843 - offset__212
                var t845 int = t844 - 1
                var t846 uint8 = vec_get__Vec_5uint8(reversed__209, t845)
                vec_push__Vec_5uint8(bytes__211, t846)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t847 int = compound_old358 + compound_value359
                offset__212 = t847
                continue
            } else {
                break Loop_loop840
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
