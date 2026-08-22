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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_os.Stdout.WriteString(s)
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

type Color int32

const (
    Red Color = 0
    Green Color = 1
    Blue Color = 2
)

func main0() struct{} {
    var x796 Color = Blue
    var x797 Color = Red
    switch x797 {
    case Red:
        switch x796 {
        case Red:
            var inline859 int = 1
            var inline860 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline859)
            _goml_runtime_core_string_print(inline860)
            return struct{}{}
        default:
            var inline863 int = 3
            var inline864 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline863)
            _goml_runtime_core_string_print(inline864)
            return struct{}{}
        }
    case Green:
        switch x796 {
        case Red:
            var inline867 int = 0
            var inline868 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline867)
            _goml_runtime_core_string_print(inline868)
            return struct{}{}
        default:
            var inline871 int = 3
            var inline872 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline871)
            _goml_runtime_core_string_print(inline872)
            return struct{}{}
        }
    case Blue:
        switch x796 {
        case Blue:
            var inline875 int = 2
            var inline876 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline875)
            _goml_runtime_core_string_print(inline876)
            return struct{}{}
        default:
            var inline879 int = 3
            var inline880 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline879)
            _goml_runtime_core_string_print(inline880)
            return struct{}{}
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline885 int64 = int64(int(self__404))
    var inline886 string = signed_decimal_string(inline885)
    return inline886
}

func signed_decimal_string(value__214 int64) string {
    var t823 bool = value__214 < 0
    if t823 {
        var t824 uint64 = uint64(int64(value__214))
        var t825 uint64 = 0 - t824
        var t826 string = decimal_string(t825)
        var t827 string = "-" + t826
        return t827
    } else {
        var t828 uint64 = uint64(int64(value__214))
        var t829 string = decimal_string(t828)
        return t829
    }
}

func decimal_string(value__208 uint64) string {
    var t852 bool = value__208 == 0
    if t852 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop845:
        for {
            var t846 bool = remaining__210 > 0
            if t846 {
                var t847_rhs uint64 = 10
                var t847 uint64 = remaining__210 % t847_rhs
                var t848 uint8 = uint8(uint64(t847))
                var t849 uint8 = t848 + 48
                vec_push__Vec_5uint8(reversed__209, t849)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t850 uint64 = compound_old353 / compound_value354
                remaining__210 = t850
                continue
            } else {
                break Loop_loop845
            }
        }
        var t834 int
        var inline904 int = vec_len__Vec_5uint8(reversed__209)
        t834 = inline904
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t834)
        var offset__212 int = 0
        Loop_loop836:
        for {
            var t837 int
            var inline902 int = vec_len__Vec_5uint8(reversed__209)
            t837 = inline902
            var t838 bool = offset__212 < t837
            if t838 {
                var t839 int
                var inline900 int = vec_len__Vec_5uint8(reversed__209)
                t839 = inline900
                var t840 int = t839 - offset__212
                var t841 int = t840 - 1
                var t842 uint8 = vec_get__Vec_5uint8(reversed__209, t841)
                vec_push__Vec_5uint8(bytes__211, t842)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t843 int = compound_old358 + compound_value359
                offset__212 = t843
                continue
            } else {
                break Loop_loop836
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
