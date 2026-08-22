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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func maybe_total(flag__2 bool) Option__i32 {
    var mtmp796 Option__i32
    if flag__2 {
        var inline890 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 3,
        }
        mtmp796 = inline890
    } else {
        mtmp796 = Option__i32{
            _tag: 0,
        }
    }
    var jp818 int32
    switch mtmp796._tag {
    case 0:
        return Option__i32{
            _tag: 0,
        }
    case 1:
        var x797 int32 = mtmp796._v1_0
        jp818 = x797
        var mtmp798 Option__i32
        var inline886 bool = jp818 > 0
        if inline886 {
            var inline887 int32 = jp818 * 2
            var inline888 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: inline887,
            }
            mtmp798 = inline888
        } else {
            mtmp798 = Option__i32{
                _tag: 0,
            }
        }
        var jp820 int32
        switch mtmp798._tag {
        case 0:
            return Option__i32{
                _tag: 0,
            }
        case 1:
            var x799 int32 = mtmp798._v1_0
            jp820 = x799
            var t821 int32 = jp818 + jp820
            var t822 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: t821,
            }
            return t822
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t830 Option__i32 = maybe_total(true)
    var t831 string
    switch t830._tag {
    case 0:
        t831 = "none"
    case 1:
        var inline905 int32 = t830._v1_0
        var inline907 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline905)
        var inline908 string = "some=" + inline907
        t831 = inline908
    default:
        panic("non-exhaustive match")
    }
    var inline902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline902)
    var t832 Option__i32 = maybe_total(false)
    var t833 string
    switch t832._tag {
    case 0:
        t833 = "none"
    case 1:
        var inline897 int32 = t832._v1_0
        var inline899 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline897)
        var inline900 string = "some=" + inline899
        t833 = inline900
    default:
        panic("non-exhaustive match")
    }
    var inline894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t833)
    _goml_runtime_core_string_println(inline894)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline910 int64 = int64(int32(self__286))
    var inline911 string = signed_decimal_string(inline910)
    return inline911
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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
        var inline930 int = vec_len__Vec_5uint8(reversed__209)
        t861 = inline930
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t861)
        var offset__212 int = 0
        Loop_loop863:
        for {
            var t864 int
            var inline928 int = vec_len__Vec_5uint8(reversed__209)
            t864 = inline928
            var t865 bool = offset__212 < t864
            if t865 {
                var t866 int
                var inline926 int = vec_len__Vec_5uint8(reversed__209)
                t866 = inline926
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
