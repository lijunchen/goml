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

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
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

type Wrap__i32 struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Render_i_Vec_l_i32_r__i_render(self__0 *_goml_vec_int32) string {
    var t803 int
    var inline878 int = vec_len__Vec_5int32(self__0)
    t803 = inline878
    var t804 string
    var inline876 string = __goml_builtin_int_to_string(t803)
    t804 = inline876
    var t805 string = "items=" + t804
    return t805
}

func main0() struct{} {
    var values__5 *_goml_vec_int32 = vec_new__Vec_5int32()
    var inline898 int32 = 1
    vec_push__Vec_5int32(values__5, inline898)
    var inline895 int32 = 2
    vec_push__Vec_5int32(values__5, inline895)
    var t807 string
    var inline893 string = _goml_m_trait__impl_i_Render_i_Vec_l_i32_r__i_render(values__5)
    t807 = inline893
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline890)
    var text__6 string
    var inline888 string = "equal"
    text__6 = inline888
    var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__6)
    _goml_runtime_core_string_println(inline885)
    var selected__7 Wrap__i32 = Wrap__i32{
        value: 7,
    }
    var t808 string
    var inline883 string = _goml_m_trait__impl_i_Selected_i_Wrap____i32_i_selected(selected__7)
    t808 = inline883
    var inline880 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline880)
    return struct{}{}
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t830 int64 = int64(int(value__222))
    var inline910 bool = t830 < 0
    if inline910 {
        var inline911 uint64 = uint64(int64(t830))
        var inline912 uint64 = 0 - inline911
        var inline913 string = decimal_string(inline912)
        var inline914 string = "-" + inline913
        return inline914
    } else {
        var inline915 uint64 = uint64(int64(t830))
        var inline916 string = decimal_string(inline915)
        return inline916
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_Selected_i_Wrap____i32_i_selected(self__1 Wrap__i32) string {
    return "selected"
}

func decimal_string(value__208 uint64) string {
    var t869 bool = value__208 == 0
    if t869 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop862:
        for {
            var t863 bool = remaining__210 > 0
            if t863 {
                var t864_rhs uint64 = 10
                var t864 uint64 = remaining__210 % t864_rhs
                var t865 uint8 = uint8(uint64(t864))
                var t866 uint8 = t865 + 48
                vec_push__Vec_5uint8(reversed__209, t866)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t867 uint64 = compound_old353 / compound_value354
                remaining__210 = t867
                continue
            } else {
                break Loop_loop862
            }
        }
        var t851 int
        var inline926 int = vec_len__Vec_5uint8(reversed__209)
        t851 = inline926
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t851)
        var offset__212 int = 0
        Loop_loop853:
        for {
            var t854 int
            var inline924 int = vec_len__Vec_5uint8(reversed__209)
            t854 = inline924
            var t855 bool = offset__212 < t854
            if t855 {
                var t856 int
                var inline922 int = vec_len__Vec_5uint8(reversed__209)
                t856 = inline922
                var t857 int = t856 - offset__212
                var t858 int = t857 - 1
                var t859 uint8 = vec_get__Vec_5uint8(reversed__209, t858)
                vec_push__Vec_5uint8(bytes__211, t859)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t860 int = compound_old358 + compound_value359
                offset__212 = t860
                continue
            } else {
                break Loop_loop853
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
