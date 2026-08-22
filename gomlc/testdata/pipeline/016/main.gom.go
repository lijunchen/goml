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

type List__i32 interface {
    isList__i32()
}

type List__i32_Nil struct {}

func (_ List__i32_Nil) isList__i32() {}

type List__i32_Cons struct {
    _0 int32
    _1 List__i32
}

func (_ List__i32_Cons) isList__i32() {}

type List__isize interface {
    isList__isize()
}

type List__isize_Nil struct {}

func (_ List__isize_Nil) isList__isize() {}

type List__isize_Cons struct {
    _0 int
    _1 List__isize
}

func (_ List__isize_Cons) isList__isize() {}

type List__unit interface {
    isList__unit()
}

type List__unit_Nil struct {}

func (_ List__unit_Nil) isList__unit() {}

type List__unit_Cons struct {
    _0 struct{}
    _1 List__unit
}

func (_ List__unit_Cons) isList__unit() {}

type List__bool interface {
    isList__bool()
}

type List__bool_Nil struct {}

func (_ List__bool_Nil) isList__bool() {}

type List__bool_Cons struct {
    _0 bool
    _1 List__bool
}

func (_ List__bool_Cons) isList__bool() {}

func int_list_length(xs__2 List__i32) int32 {
    switch xs__2.(type) {
    case List__i32_Nil:
        return 0
    case List__i32_Cons:
        var x799 List__i32 = xs__2.(List__i32_Cons)._1
        var t810 int32 = int_list_length(x799)
        var t811 int32 = 1 + t810
        return t811
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__4 List__isize = List__isize_Cons{
        _0: 1,
        _1: List__isize_Nil{},
    }
    var length__5 int32 = list_length__T_isize(x__4)
    var inline901 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__5)
    _goml_runtime_core_string_println(inline901)
    var t813 List__isize = List__isize_Cons{
        _0: 2,
        _1: List__isize_Nil{},
    }
    var x__6 List__isize = List__isize_Cons{
        _0: 1,
        _1: t813,
    }
    var length__7 int32 = list_length__T_isize(x__6)
    var inline898 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__7)
    _goml_runtime_core_string_println(inline898)
    var t814 List__i32 = List__i32_Cons{
        _0: 2,
        _1: List__i32_Nil{},
    }
    var t815 List__i32 = List__i32_Cons{
        _0: 1,
        _1: t814,
    }
    var x__8 List__i32 = List__i32_Cons{
        _0: 0,
        _1: t815,
    }
    var length__9 int32 = int_list_length(x__8)
    var inline895 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__9)
    _goml_runtime_core_string_println(inline895)
    var x__10 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var length__11 int32 = list_length__T_unit(x__10)
    var inline892 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__11)
    _goml_runtime_core_string_println(inline892)
    var t816 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: List__unit_Nil{},
    }
    var x__12 List__unit = List__unit_Cons{
        _0: struct{}{},
        _1: t816,
    }
    var length__13 int32 = list_length__T_unit(x__12)
    var inline889 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__13)
    _goml_runtime_core_string_println(inline889)
    var t817 List__bool = List__bool_Cons{
        _0: false,
        _1: List__bool_Nil{},
    }
    var x__14 List__bool = List__bool_Cons{
        _0: true,
        _1: t817,
    }
    var length__15 int32 = list_length__T_bool(x__14)
    var inline886 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(length__15)
    _goml_runtime_core_string_println(inline886)
    return struct{}{}
}

func list_length__T_isize(xs__0 List__isize) int32 {
    switch xs__0.(type) {
    case List__isize_Nil:
        return 0
    case List__isize_Cons:
        var x797 List__isize = xs__0.(List__isize_Cons)._1
        var t822 int32 = list_length__T_isize(x797)
        var t823 int32 = 1 + t822
        return t823
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_unit(xs__0 List__unit) int32 {
    switch xs__0.(type) {
    case List__unit_Nil:
        return 0
    case List__unit_Cons:
        var x797 List__unit = xs__0.(List__unit_Cons)._1
        var t831 int32 = list_length__T_unit(x797)
        var t832 int32 = 1 + t831
        return t832
    default:
        panic("non-exhaustive match")
    }
}

func list_length__T_bool(xs__0 List__bool) int32 {
    switch xs__0.(type) {
    case List__bool_Nil:
        return 0
    case List__bool_Cons:
        var x797 List__bool = xs__0.(List__bool_Cons)._1
        var t837 int32 = list_length__T_bool(x797)
        var t838 int32 = 1 + t837
        return t838
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline906 int64 = int64(int32(self__407))
    var inline907 string = signed_decimal_string(inline906)
    return inline907
}

func signed_decimal_string(value__214 int64) string {
    var t850 bool = value__214 < 0
    if t850 {
        var t851 uint64 = uint64(int64(value__214))
        var t852 uint64 = 0 - t851
        var t853 string = decimal_string(t852)
        var t854 string = "-" + t853
        return t854
    } else {
        var t855 uint64 = uint64(int64(value__214))
        var t856 string = decimal_string(t855)
        return t856
    }
}

func decimal_string(value__208 uint64) string {
    var t879 bool = value__208 == 0
    if t879 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop872:
        for {
            var t873 bool = remaining__210 > 0
            if t873 {
                var t874_rhs uint64 = 10
                var t874 uint64 = remaining__210 % t874_rhs
                var t875 uint8 = uint8(uint64(t874))
                var t876 uint8 = t875 + 48
                vec_push__Vec_5uint8(reversed__209, t876)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t877 uint64 = compound_old353 / compound_value354
                remaining__210 = t877
                continue
            } else {
                break Loop_loop872
            }
        }
        var t861 int
        var inline925 int = vec_len__Vec_5uint8(reversed__209)
        t861 = inline925
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t861)
        var offset__212 int = 0
        Loop_loop863:
        for {
            var t864 int
            var inline923 int = vec_len__Vec_5uint8(reversed__209)
            t864 = inline923
            var t865 bool = offset__212 < t864
            if t865 {
                var t866 int
                var inline921 int = vec_len__Vec_5uint8(reversed__209)
                t866 = inline921
                var t867 int = t866 - offset__212
                var t868 int = t867 - 1
                var t869 uint8 = vec_get__Vec_5uint8(reversed__209, t868)
                vec_push__Vec_5uint8(bytes__211, t869)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t870 int = compound_old358 + compound_value359
                offset__212 = t870
                continue
            } else {
                break Loop_loop863
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
