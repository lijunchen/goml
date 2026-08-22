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

type NumberBox struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_i32_i_mark(self__0 int32) string {
    var t799 string
    var inline863 string = __goml_builtin_int32_to_string(self__0)
    t799 = inline863
    var t800 string = "marked:" + t799
    return t800
}

func _goml_m_trait__impl_i_Container_i__l_i32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t803 int32 = self__1.value
    return t803
}

func main0() struct{} {
    var t805 NumberBox = NumberBox{
        value: 7,
    }
    var t806 string
    var inline868 int32 = _goml_m_trait__impl_i_Container_i__l_i32_r__x40_NumberBox_i_value(t805)
    var inline869 string = _goml_m_trait__impl_i_Mark_i_i32_i_mark(inline868)
    t806 = inline869
    var inline865 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline865)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t819 int64 = int64(int32(value__225))
    var inline880 bool = t819 < 0
    if inline880 {
        var inline881 uint64 = uint64(int64(t819))
        var inline882 uint64 = 0 - inline881
        var inline883 string = decimal_string(inline882)
        var inline884 string = "-" + inline883
        return inline884
    } else {
        var inline885 uint64 = uint64(int64(t819))
        var inline886 string = decimal_string(inline885)
        return inline886
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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
        var inline896 int = vec_len__Vec_5uint8(reversed__209)
        t838 = inline896
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t838)
        var offset__212 int = 0
        Loop_loop840:
        for {
            var t841 int
            var inline894 int = vec_len__Vec_5uint8(reversed__209)
            t841 = inline894
            var t842 bool = offset__212 < t841
            if t842 {
                var t843 int
                var inline892 int = vec_len__Vec_5uint8(reversed__209)
                t843 = inline892
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
