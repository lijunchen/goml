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

type Light int32

const (
    Light_Red Light = 0
    Yellow Light = 1
    Green Light = 2
)

type Paint int32

const (
    Paint_Red Paint = 0
    Blue Paint = 1
)

func main0() struct{} {
    var light__2 Light = Light_Red
    var paint__3 Paint = Paint_Red
    var t808 int32
    switch light__2 {
    case Light_Red:
        t808 = 10
    case Yellow:
        t808 = 20
    case Green:
        t808 = 30
    default:
        panic("non-exhaustive match")
    }
    var t809 string
    var inline890 string = __goml_builtin_int32_to_string(t808)
    t809 = inline890
    var inline887 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline887)
    var t810 int32
    switch paint__3 {
    case Paint_Red:
        t810 = 1
    case Blue:
        t810 = 2
    default:
        panic("non-exhaustive match")
    }
    var t811 string
    var inline884 string = __goml_builtin_int32_to_string(t810)
    t811 = inline884
    var inline881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline881)
    var t812 int32
    t812 = 30
    var t813 string
    var inline878 string = __goml_builtin_int32_to_string(t812)
    t813 = inline878
    var inline875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline875)
    var t814 int32
    t814 = 2
    var t815 string
    var inline872 string = __goml_builtin_int32_to_string(t814)
    t815 = inline872
    var inline869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline869)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t827 int64 = int64(int32(value__225))
    var inline897 bool = t827 < 0
    if inline897 {
        var inline898 uint64 = uint64(int64(t827))
        var inline899 uint64 = 0 - inline898
        var inline900 string = decimal_string(inline899)
        var inline901 string = "-" + inline900
        return inline901
    } else {
        var inline902 uint64 = uint64(int64(t827))
        var inline903 string = decimal_string(inline902)
        return inline903
    }
}

func decimal_string(value__208 uint64) string {
    var t862 bool = value__208 == 0
    if t862 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop855:
        for {
            var t856 bool = remaining__210 > 0
            if t856 {
                var t857_rhs uint64 = 10
                var t857 uint64 = remaining__210 % t857_rhs
                var t858 uint8 = uint8(uint64(t857))
                var t859 uint8 = t858 + 48
                vec_push__Vec_5uint8(reversed__209, t859)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t860 uint64 = compound_old353 / compound_value354
                remaining__210 = t860
                continue
            } else {
                break Loop_loop855
            }
        }
        var t844 int
        var inline913 int = vec_len__Vec_5uint8(reversed__209)
        t844 = inline913
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t844)
        var offset__212 int = 0
        Loop_loop846:
        for {
            var t847 int
            var inline911 int = vec_len__Vec_5uint8(reversed__209)
            t847 = inline911
            var t848 bool = offset__212 < t847
            if t848 {
                var t849 int
                var inline909 int = vec_len__Vec_5uint8(reversed__209)
                t849 = inline909
                var t850 int = t849 - offset__212
                var t851 int = t850 - 1
                var t852 uint8 = vec_get__Vec_5uint8(reversed__209, t851)
                vec_push__Vec_5uint8(bytes__211, t852)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t853 int = compound_old358 + compound_value359
                offset__212 = t853
                continue
            } else {
                break Loop_loop846
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
