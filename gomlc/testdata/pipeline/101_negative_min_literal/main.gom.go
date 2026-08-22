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
    var a__0 int8 = -128
    var t799 string
    var inline868 string = __goml_builtin_int8_to_string(a__0)
    t799 = inline868
    var inline865 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t799)
    _goml_runtime_core_string_println(inline865)
    var b__1 int16 = -32768
    var t800 string
    var inline863 string = __goml_builtin_int16_to_string(b__1)
    t800 = inline863
    var inline860 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline860)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int8_to_string(value__223 int8) string {
    var t814 int64 = int64(int8(value__223))
    var inline877 bool = t814 < 0
    if inline877 {
        var inline878 uint64 = uint64(int64(t814))
        var inline879 uint64 = 0 - inline878
        var inline880 string = decimal_string(inline879)
        var inline881 string = "-" + inline880
        return inline881
    } else {
        var inline882 uint64 = uint64(int64(t814))
        var inline883 string = decimal_string(inline882)
        return inline883
    }
}

func __goml_builtin_int16_to_string(value__224 int16) string {
    var t818 int64 = int64(int16(value__224))
    var inline885 bool = t818 < 0
    if inline885 {
        var inline886 uint64 = uint64(int64(t818))
        var inline887 uint64 = 0 - inline886
        var inline888 string = decimal_string(inline887)
        var inline889 string = "-" + inline888
        return inline889
    } else {
        var inline890 uint64 = uint64(int64(t818))
        var inline891 string = decimal_string(inline890)
        return inline891
    }
}

func decimal_string(value__208 uint64) string {
    var t853 bool = value__208 == 0
    if t853 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop846:
        for {
            var t847 bool = remaining__210 > 0
            if t847 {
                var t848_rhs uint64 = 10
                var t848 uint64 = remaining__210 % t848_rhs
                var t849 uint8 = uint8(uint64(t848))
                var t850 uint8 = t849 + 48
                vec_push__Vec_5uint8(reversed__209, t850)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t851 uint64 = compound_old353 / compound_value354
                remaining__210 = t851
                continue
            } else {
                break Loop_loop846
            }
        }
        var t835 int
        var inline901 int = vec_len__Vec_5uint8(reversed__209)
        t835 = inline901
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t835)
        var offset__212 int = 0
        Loop_loop837:
        for {
            var t838 int
            var inline899 int = vec_len__Vec_5uint8(reversed__209)
            t838 = inline899
            var t839 bool = offset__212 < t838
            if t839 {
                var t840 int
                var inline897 int = vec_len__Vec_5uint8(reversed__209)
                t840 = inline897
                var t841 int = t840 - offset__212
                var t842 int = t841 - 1
                var t843 uint8 = vec_get__Vec_5uint8(reversed__209, t842)
                vec_push__Vec_5uint8(bytes__211, t843)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t844 int = compound_old358 + compound_value359
                offset__212 = t844
                continue
            } else {
                break Loop_loop837
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
