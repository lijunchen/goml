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

type Mode int32

const (
    Take Mode = 0
    Skip Mode = 1
)

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func nested(top__1 bool, mode__2 Mode, inner_flag__3 bool) Option__i32 {
    var jp812 int32
    if top__1 {
        switch mode__2 {
        case Take:
            var mtmp796 Option__i32
            if inner_flag__3 {
                var inline887 Option__i32 = Option__i32{
                    _tag: 1,
                    _v1_0: 8,
                }
                mtmp796 = inline887
            } else {
                mtmp796 = Option__i32{
                    _tag: 0,
                }
            }
            var jp817 int32
            switch mtmp796._tag {
            case 0:
                return Option__i32{
                    _tag: 0,
                }
            case 1:
                var x797 int32 = mtmp796._v1_0
                jp817 = x797
                var t818 int32 = jp817 + 1
                jp812 = t818
                var t813 Option__i32 = Option__i32{
                    _tag: 1,
                    _v1_0: jp812,
                }
                return t813
            default:
                panic("non-exhaustive match")
            }
        case Skip:
            jp812 = 20
            var t813 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: jp812,
            }
            return t813
        default:
            panic("non-exhaustive match")
        }
    } else {
        var mtmp798 Option__i32
        if inner_flag__3 {
            var inline889 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: 8,
            }
            mtmp798 = inline889
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
            var t821 int32 = jp820 + 2
            jp812 = t821
            var t813 Option__i32 = Option__i32{
                _tag: 1,
                _v1_0: jp812,
            }
            return t813
        default:
            panic("non-exhaustive match")
        }
    }
}

func main0() struct{} {
    var t829 Option__i32 = nested(true, Take, true)
    var t830 string
    switch t829._tag {
    case 0:
        t830 = "none"
    case 1:
        var inline912 int32 = t829._v1_0
        var inline914 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline912)
        var inline915 string = "some=" + inline914
        t830 = inline915
    default:
        panic("non-exhaustive match")
    }
    var inline909 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t830)
    _goml_runtime_core_string_println(inline909)
    var t831 Option__i32 = nested(true, Skip, false)
    var t832 string
    switch t831._tag {
    case 0:
        t832 = "none"
    case 1:
        var inline904 int32 = t831._v1_0
        var inline906 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline904)
        var inline907 string = "some=" + inline906
        t832 = inline907
    default:
        panic("non-exhaustive match")
    }
    var inline901 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t832)
    _goml_runtime_core_string_println(inline901)
    var t833 Option__i32 = nested(false, Take, false)
    var t834 string
    switch t833._tag {
    case 0:
        t834 = "none"
    case 1:
        var inline896 int32 = t833._v1_0
        var inline898 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline896)
        var inline899 string = "some=" + inline898
        t834 = inline899
    default:
        panic("non-exhaustive match")
    }
    var inline893 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t834)
    _goml_runtime_core_string_println(inline893)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline917 int64 = int64(int32(self__286))
    var inline918 string = signed_decimal_string(inline917)
    return inline918
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t851 bool = value__214 < 0
    if t851 {
        var t852 uint64 = uint64(int64(value__214))
        var t853 uint64 = 0 - t852
        var t854 string = decimal_string(t853)
        var t855 string = "-" + t854
        return t855
    } else {
        var t856 uint64 = uint64(int64(value__214))
        var t857 string = decimal_string(t856)
        return t857
    }
}

func decimal_string(value__208 uint64) string {
    var t880 bool = value__208 == 0
    if t880 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop873:
        for {
            var t874 bool = remaining__210 > 0
            if t874 {
                var t875_rhs uint64 = 10
                var t875 uint64 = remaining__210 % t875_rhs
                var t876 uint8 = uint8(uint64(t875))
                var t877 uint8 = t876 + 48
                vec_push__Vec_5uint8(reversed__209, t877)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t878 uint64 = compound_old353 / compound_value354
                remaining__210 = t878
                continue
            } else {
                break Loop_loop873
            }
        }
        var t862 int
        var inline937 int = vec_len__Vec_5uint8(reversed__209)
        t862 = inline937
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t862)
        var offset__212 int = 0
        Loop_loop864:
        for {
            var t865 int
            var inline935 int = vec_len__Vec_5uint8(reversed__209)
            t865 = inline935
            var t866 bool = offset__212 < t865
            if t866 {
                var t867 int
                var inline933 int = vec_len__Vec_5uint8(reversed__209)
                t867 = inline933
                var t868 int = t867 - offset__212
                var t869 int = t868 - 1
                var t870 uint8 = vec_get__Vec_5uint8(reversed__209, t869)
                vec_push__Vec_5uint8(bytes__211, t870)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t871 int = compound_old358 + compound_value359
                offset__212 = t871
                continue
            } else {
                break Loop_loop864
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
