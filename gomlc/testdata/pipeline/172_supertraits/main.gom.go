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

type Box__isize struct {
    value int
}

type Box__i32 struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t798 Box__isize = Box__isize{
        value: 5,
    }
    var t799 string
    var inline890 int = _goml_m_trait__impl_i_Parent_i__l_isize_r__x40_Box____isize_i_parent(t798)
    var inline891 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline890)
    var inline892 string = _goml_m_trait__impl_i_Render_i_Box____isize_i_render(t798)
    var inline893 string = inline891 + inline892
    var inline894 string = _goml_m_trait__impl_i_Child_i__l_isize_r__x40_Box____isize_i_child(t798)
    var inline895 string = inline893 + inline894
    t799 = inline895
    var inline887 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t799)
    _goml_runtime_core_string_println(inline887)
    var t800 int32
    var inline885 int32 = 6
    t800 = inline885
    var t801 string
    var inline883 string = __goml_builtin_int32_to_string(t800)
    t801 = inline883
    var inline880 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
    _goml_runtime_core_string_println(inline880)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_Parent_i__l_isize_r__x40_Box____isize_i_parent(self__0 Box__isize) int {
    var t824 int = self__0.value
    return t824
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline907 int64 = int64(int(self__404))
    var inline908 string = signed_decimal_string(inline907)
    return inline908
}

func _goml_m_trait__impl_i_Render_i_Box____isize_i_render(self__1 Box__isize) string {
    return ":render"
}

func _goml_m_trait__impl_i_Child_i__l_isize_r__x40_Box____isize_i_child(self__2 Box__isize) string {
    return ":child"
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t834 int64 = int64(int32(value__225))
    var inline910 bool = t834 < 0
    if inline910 {
        var inline911 uint64 = uint64(int64(t834))
        var inline912 uint64 = 0 - inline911
        var inline913 string = decimal_string(inline912)
        var inline914 string = "-" + inline913
        return inline914
    } else {
        var inline915 uint64 = uint64(int64(t834))
        var inline916 string = decimal_string(inline915)
        return inline916
    }
}

func signed_decimal_string(value__214 int64) string {
    var t844 bool = value__214 < 0
    if t844 {
        var t845 uint64 = uint64(int64(value__214))
        var t846 uint64 = 0 - t845
        var t847 string = decimal_string(t846)
        var t848 string = "-" + t847
        return t848
    } else {
        var t849 uint64 = uint64(int64(value__214))
        var t850 string = decimal_string(t849)
        return t850
    }
}

func decimal_string(value__208 uint64) string {
    var t873 bool = value__208 == 0
    if t873 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop866:
        for {
            var t867 bool = remaining__210 > 0
            if t867 {
                var t868_rhs uint64 = 10
                var t868 uint64 = remaining__210 % t868_rhs
                var t869 uint8 = uint8(uint64(t868))
                var t870 uint8 = t869 + 48
                vec_push__Vec_5uint8(reversed__209, t870)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t871 uint64 = compound_old353 / compound_value354
                remaining__210 = t871
                continue
            } else {
                break Loop_loop866
            }
        }
        var t855 int
        var inline934 int = vec_len__Vec_5uint8(reversed__209)
        t855 = inline934
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t855)
        var offset__212 int = 0
        Loop_loop857:
        for {
            var t858 int
            var inline932 int = vec_len__Vec_5uint8(reversed__209)
            t858 = inline932
            var t859 bool = offset__212 < t858
            if t859 {
                var t860 int
                var inline930 int = vec_len__Vec_5uint8(reversed__209)
                t860 = inline930
                var t861 int = t860 - offset__212
                var t862 int = t861 - 1
                var t863 uint8 = vec_get__Vec_5uint8(reversed__209, t862)
                vec_push__Vec_5uint8(bytes__211, t863)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t864 int = compound_old358 + compound_value359
                offset__212 = t864
                continue
            } else {
                break Loop_loop857
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
