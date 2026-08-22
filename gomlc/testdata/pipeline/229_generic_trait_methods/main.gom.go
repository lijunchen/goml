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

type Prefix struct {
    value string
}

type Ordering int32

func main0() struct{} {
    var direct__3 string
    var inline884 string = "ok"
    var inline885 string = "direct:"
    var inline886 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline884)
    var inline887 string = inline885 + inline886
    direct__3 = inline887
    var inline881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(direct__3)
    _goml_runtime_core_string_println(inline881)
    var t801 Prefix = Prefix{
        value: "generic:",
    }
    var generic__4 string
    var inline879 string = _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(t801, 11)
    generic__4 = inline879
    var inline876 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(generic__4)
    _goml_runtime_core_string_println(inline876)
    var ufcs__5 string
    var inline871 int = 12
    var inline872 string = "ufcs:"
    var inline873 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline871)
    var inline874 string = inline872 + inline873
    ufcs__5 = inline874
    var inline868 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(ufcs__5)
    _goml_runtime_core_string_println(inline868)
    return struct{}{}
}

func _goml_m_trait__impl_i_RenderValue_i_Prefix_i_render____mono2(self__0 Prefix, value__1 int) string {
    var t816 string = self__0.value
    var t817 string
    var inline896 string = __goml_builtin_int_to_string(value__1)
    t817 = inline896
    var t818 string = t816 + t817
    return t818
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline898 int64 = int64(int(self__404))
    var inline899 string = signed_decimal_string(inline898)
    return inline899
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t826 int64 = int64(int(value__222))
    var inline901 bool = t826 < 0
    if inline901 {
        var inline902 uint64 = uint64(int64(t826))
        var inline903 uint64 = 0 - inline902
        var inline904 string = decimal_string(inline903)
        var inline905 string = "-" + inline904
        return inline905
    } else {
        var inline906 uint64 = uint64(int64(t826))
        var inline907 string = decimal_string(inline906)
        return inline907
    }
}

func signed_decimal_string(value__214 int64) string {
    var t832 bool = value__214 < 0
    if t832 {
        var t833 uint64 = uint64(int64(value__214))
        var t834 uint64 = 0 - t833
        var t835 string = decimal_string(t834)
        var t836 string = "-" + t835
        return t836
    } else {
        var t837 uint64 = uint64(int64(value__214))
        var t838 string = decimal_string(t837)
        return t838
    }
}

func decimal_string(value__208 uint64) string {
    var t861 bool = value__208 == 0
    if t861 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop854:
        for {
            var t855 bool = remaining__210 > 0
            if t855 {
                var t856_rhs uint64 = 10
                var t856 uint64 = remaining__210 % t856_rhs
                var t857 uint8 = uint8(uint64(t856))
                var t858 uint8 = t857 + 48
                vec_push__Vec_5uint8(reversed__209, t858)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t859 uint64 = compound_old353 / compound_value354
                remaining__210 = t859
                continue
            } else {
                break Loop_loop854
            }
        }
        var t843 int
        var inline917 int = vec_len__Vec_5uint8(reversed__209)
        t843 = inline917
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t843)
        var offset__212 int = 0
        Loop_loop845:
        for {
            var t846 int
            var inline915 int = vec_len__Vec_5uint8(reversed__209)
            t846 = inline915
            var t847 bool = offset__212 < t846
            if t847 {
                var t848 int
                var inline913 int = vec_len__Vec_5uint8(reversed__209)
                t848 = inline913
                var t849 int = t848 - offset__212
                var t850 int = t849 - 1
                var t851 uint8 = vec_get__Vec_5uint8(reversed__209, t850)
                vec_push__Vec_5uint8(bytes__211, t851)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t852 int = compound_old358 + compound_value359
                offset__212 = t852
                continue
            } else {
                break Loop_loop845
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
