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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
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
    var value__3 Tuple2_5int32_6string
    var inline862 int32 = 1
    var inline863 string = "x"
    var inline864 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: inline862,
        _1: inline863,
    }
    value__3 = inline864
    var t800 int32 = value__3._0
    var t801 string
    var inline860 string = __goml_builtin_int32_to_string(t800)
    t801 = inline860
    var t802 string = value__3._1
    var t803 string = t801 + t802
    var inline857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
    _goml_runtime_core_string_println(inline857)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t815 int64 = int64(int32(value__225))
    var inline870 bool = t815 < 0
    if inline870 {
        var inline871 uint64 = uint64(int64(t815))
        var inline872 uint64 = 0 - inline871
        var inline873 string = decimal_string(inline872)
        var inline874 string = "-" + inline873
        return inline874
    } else {
        var inline875 uint64 = uint64(int64(t815))
        var inline876 string = decimal_string(inline875)
        return inline876
    }
}

func decimal_string(value__208 uint64) string {
    var t850 bool = value__208 == 0
    if t850 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop843:
        for {
            var t844 bool = remaining__210 > 0
            if t844 {
                var t845_rhs uint64 = 10
                var t845 uint64 = remaining__210 % t845_rhs
                var t846 uint8 = uint8(uint64(t845))
                var t847 uint8 = t846 + 48
                vec_push__Vec_5uint8(reversed__209, t847)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t848 uint64 = compound_old353 / compound_value354
                remaining__210 = t848
                continue
            } else {
                break Loop_loop843
            }
        }
        var t832 int
        var inline886 int = vec_len__Vec_5uint8(reversed__209)
        t832 = inline886
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t832)
        var offset__212 int = 0
        Loop_loop834:
        for {
            var t835 int
            var inline884 int = vec_len__Vec_5uint8(reversed__209)
            t835 = inline884
            var t836 bool = offset__212 < t835
            if t836 {
                var t837 int
                var inline882 int = vec_len__Vec_5uint8(reversed__209)
                t837 = inline882
                var t838 int = t837 - offset__212
                var t839 int = t838 - 1
                var t840 uint8 = vec_get__Vec_5uint8(reversed__209, t839)
                vec_push__Vec_5uint8(bytes__211, t840)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t841 int = compound_old358 + compound_value359
                offset__212 = t841
                continue
            } else {
                break Loop_loop834
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
