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
    var t798 string
    var inline868 string = "direct"
    t798 = inline868
    var inline865 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t798)
    _goml_runtime_core_string_println(inline865)
    var t799 int32
    var inline863 int32 = 42
    t799 = inline863
    var t800 string
    var inline861 string = __goml_builtin_int32_to_string(t799)
    t800 = inline861
    var inline858 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline858)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t816 int64 = int64(int32(value__225))
    var inline874 bool = t816 < 0
    if inline874 {
        var inline875 uint64 = uint64(int64(t816))
        var inline876 uint64 = 0 - inline875
        var inline877 string = decimal_string(inline876)
        var inline878 string = "-" + inline877
        return inline878
    } else {
        var inline879 uint64 = uint64(int64(t816))
        var inline880 string = decimal_string(inline879)
        return inline880
    }
}

func decimal_string(value__208 uint64) string {
    var t851 bool = value__208 == 0
    if t851 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop844:
        for {
            var t845 bool = remaining__210 > 0
            if t845 {
                var t846_rhs uint64 = 10
                var t846 uint64 = remaining__210 % t846_rhs
                var t847 uint8 = uint8(uint64(t846))
                var t848 uint8 = t847 + 48
                vec_push__Vec_5uint8(reversed__209, t848)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t849 uint64 = compound_old353 / compound_value354
                remaining__210 = t849
                continue
            } else {
                break Loop_loop844
            }
        }
        var t833 int
        var inline890 int = vec_len__Vec_5uint8(reversed__209)
        t833 = inline890
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t833)
        var offset__212 int = 0
        Loop_loop835:
        for {
            var t836 int
            var inline888 int = vec_len__Vec_5uint8(reversed__209)
            t836 = inline888
            var t837 bool = offset__212 < t836
            if t837 {
                var t838 int
                var inline886 int = vec_len__Vec_5uint8(reversed__209)
                t838 = inline886
                var t839 int = t838 - offset__212
                var t840 int = t839 - 1
                var t841 uint8 = vec_get__Vec_5uint8(reversed__209, t840)
                vec_push__Vec_5uint8(bytes__211, t841)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t842 int = compound_old358 + compound_value359
                offset__212 = t842
                continue
            } else {
                break Loop_loop835
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
