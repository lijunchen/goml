package main

import (
    _goml_os "os"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
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
    var x796 bool = true
    var x797 bool = false
    var jp805 Tuple2_4bool_4bool
    switch x797 {
    case true:
        switch x796 {
        case true:
            var t821 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp805 = t821
        case false:
            var t822 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp805 = t822
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x796 {
        case true:
            var t825 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp805 = t825
        case false:
            var t826 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp805 = t826
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x799 bool = jp805._1
    var x801 bool = true
    switch x799 {
    case true:
        switch x801 {
        case true:
            var t810 string
            var inline885 int = 3
            var inline886 string = __goml_builtin_int_to_string(inline885)
            t810 = inline886
            var inline882 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
            _goml_runtime_core_string_println(inline882)
        case false:
            var t812 string
            var inline891 int = 1
            var inline892 string = __goml_builtin_int_to_string(inline891)
            t812 = inline892
            var inline888 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
            _goml_runtime_core_string_println(inline888)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x801 {
        case true:
            var t815 string
            var inline897 int = 2
            var inline898 string = __goml_builtin_int_to_string(inline897)
            t815 = inline898
            var inline894 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
            _goml_runtime_core_string_println(inline894)
        case false:
            var t817 string
            var inline903 int = 0
            var inline904 string = __goml_builtin_int_to_string(inline903)
            t817 = inline904
            var inline900 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t817)
            _goml_runtime_core_string_println(inline900)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t807 string
    var inline909 string = _goml_runtime_core_unit_to_string(c__4)
    t807 = inline909
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline906)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t840 int64 = int64(int(value__222))
    var inline915 bool = t840 < 0
    if inline915 {
        var inline916 uint64 = uint64(int64(t840))
        var inline917 uint64 = 0 - inline916
        var inline918 string = decimal_string(inline917)
        var inline919 string = "-" + inline918
        return inline919
    } else {
        var inline920 uint64 = uint64(int64(t840))
        var inline921 string = decimal_string(inline920)
        return inline921
    }
}

func decimal_string(value__208 uint64) string {
    var t875 bool = value__208 == 0
    if t875 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop868:
        for {
            var t869 bool = remaining__210 > 0
            if t869 {
                var t870_rhs uint64 = 10
                var t870 uint64 = remaining__210 % t870_rhs
                var t871 uint8 = uint8(uint64(t870))
                var t872 uint8 = t871 + 48
                vec_push__Vec_5uint8(reversed__209, t872)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t873 uint64 = compound_old353 / compound_value354
                remaining__210 = t873
                continue
            } else {
                break Loop_loop868
            }
        }
        var t857 int
        var inline931 int = vec_len__Vec_5uint8(reversed__209)
        t857 = inline931
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t857)
        var offset__212 int = 0
        Loop_loop859:
        for {
            var t860 int
            var inline929 int = vec_len__Vec_5uint8(reversed__209)
            t860 = inline929
            var t861 bool = offset__212 < t860
            if t861 {
                var t862 int
                var inline927 int = vec_len__Vec_5uint8(reversed__209)
                t862 = inline927
                var t863 int = t862 - offset__212
                var t864 int = t863 - 1
                var t865 uint8 = vec_get__Vec_5uint8(reversed__209, t864)
                vec_push__Vec_5uint8(bytes__211, t865)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t866 int = compound_old358 + compound_value359
                offset__212 = t866
                continue
            } else {
                break Loop_loop859
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
