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

type Wrap__isize struct {
    value int
}

type Wrap__string struct {
    value string
}

type Ordering int32

func main0() struct{} {
    var t799 int32
    t799 = 1
    var t800 string
    var inline868 string = __goml_builtin_int32_to_string(t799)
    t800 = inline868
    var inline865 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline865)
    var t801 int32
    t801 = 1
    var t802 string
    var inline862 string = __goml_builtin_int32_to_string(t801)
    t802 = inline862
    var inline859 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline859)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t817 int64 = int64(int32(value__225))
    var inline875 bool = t817 < 0
    if inline875 {
        var inline876 uint64 = uint64(int64(t817))
        var inline877 uint64 = 0 - inline876
        var inline878 string = decimal_string(inline877)
        var inline879 string = "-" + inline878
        return inline879
    } else {
        var inline880 uint64 = uint64(int64(t817))
        var inline881 string = decimal_string(inline880)
        return inline881
    }
}

func decimal_string(value__208 uint64) string {
    var t852 bool = value__208 == 0
    if t852 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop845:
        for {
            var t846 bool = remaining__210 > 0
            if t846 {
                var t847_rhs uint64 = 10
                var t847 uint64 = remaining__210 % t847_rhs
                var t848 uint8 = uint8(uint64(t847))
                var t849 uint8 = t848 + 48
                vec_push__Vec_5uint8(reversed__209, t849)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t850 uint64 = compound_old353 / compound_value354
                remaining__210 = t850
                continue
            } else {
                break Loop_loop845
            }
        }
        var t834 int
        var inline891 int = vec_len__Vec_5uint8(reversed__209)
        t834 = inline891
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t834)
        var offset__212 int = 0
        Loop_loop836:
        for {
            var t837 int
            var inline889 int = vec_len__Vec_5uint8(reversed__209)
            t837 = inline889
            var t838 bool = offset__212 < t837
            if t838 {
                var t839 int
                var inline887 int = vec_len__Vec_5uint8(reversed__209)
                t839 = inline887
                var t840 int = t839 - offset__212
                var t841 int = t840 - 1
                var t842 uint8 = vec_get__Vec_5uint8(reversed__209, t841)
                vec_push__Vec_5uint8(bytes__211, t842)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t843 int = compound_old358 + compound_value359
                offset__212 = t843
                continue
            } else {
                break Loop_loop836
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
