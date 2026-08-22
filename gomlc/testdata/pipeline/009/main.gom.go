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

type T struct {
    _tag int32
    _v1_0 bool
    _v1_1 bool
}

func test(t__0 T) struct{} {
    switch t__0._tag {
    case 0:
        var t804 string
        var inline876 int = 1
        var inline877 string = __goml_builtin_int_to_string(inline876)
        t804 = inline877
        var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t804)
        _goml_runtime_core_string_println(inline873)
        return struct{}{}
    case 1:
        var x796 bool = t__0._v1_0
        var x797 bool = t__0._v1_1
        switch x797 {
        case true:
            switch x796 {
            case true:
                var t808 string
                var inline882 int = 4
                var inline883 string = __goml_builtin_int_to_string(inline882)
                t808 = inline883
                var inline879 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
                _goml_runtime_core_string_println(inline879)
                return struct{}{}
            case false:
                var t810 string
                var inline888 int = 3
                var inline889 string = __goml_builtin_int_to_string(inline888)
                t810 = inline889
                var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
                _goml_runtime_core_string_println(inline885)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x796 {
            case true:
                var t813 string
                var inline894 int = 4
                var inline895 string = __goml_builtin_int_to_string(inline894)
                t813 = inline895
                var inline891 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
                _goml_runtime_core_string_println(inline891)
                return struct{}{}
            case false:
                var t815 string
                var inline900 int = 2
                var inline901 string = __goml_builtin_int_to_string(inline900)
                t815 = inline901
                var inline897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
                _goml_runtime_core_string_println(inline897)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t818 T = T{
        _tag: 1,
        _v1_0: true,
        _v1_1: true,
    }
    test(t818)
    var t819 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: true,
    }
    test(t819)
    var t820 T = T{
        _tag: 1,
        _v1_0: false,
        _v1_1: false,
    }
    test(t820)
    test(T{
        _tag: 0,
    })
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t831 int64 = int64(int(value__222))
    var inline907 bool = t831 < 0
    if inline907 {
        var inline908 uint64 = uint64(int64(t831))
        var inline909 uint64 = 0 - inline908
        var inline910 string = decimal_string(inline909)
        var inline911 string = "-" + inline910
        return inline911
    } else {
        var inline912 uint64 = uint64(int64(t831))
        var inline913 string = decimal_string(inline912)
        return inline913
    }
}

func decimal_string(value__208 uint64) string {
    var t866 bool = value__208 == 0
    if t866 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop859:
        for {
            var t860 bool = remaining__210 > 0
            if t860 {
                var t861_rhs uint64 = 10
                var t861 uint64 = remaining__210 % t861_rhs
                var t862 uint8 = uint8(uint64(t861))
                var t863 uint8 = t862 + 48
                vec_push__Vec_5uint8(reversed__209, t863)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t864 uint64 = compound_old353 / compound_value354
                remaining__210 = t864
                continue
            } else {
                break Loop_loop859
            }
        }
        var t848 int
        var inline923 int = vec_len__Vec_5uint8(reversed__209)
        t848 = inline923
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t848)
        var offset__212 int = 0
        Loop_loop850:
        for {
            var t851 int
            var inline921 int = vec_len__Vec_5uint8(reversed__209)
            t851 = inline921
            var t852 bool = offset__212 < t851
            if t852 {
                var t853 int
                var inline919 int = vec_len__Vec_5uint8(reversed__209)
                t853 = inline919
                var t854 int = t853 - offset__212
                var t855 int = t854 - 1
                var t856 uint8 = vec_get__Vec_5uint8(reversed__209, t855)
                vec_push__Vec_5uint8(bytes__211, t856)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t857 int = compound_old358 + compound_value359
                offset__212 = t857
                continue
            } else {
                break Loop_loop850
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
