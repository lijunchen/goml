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

type Boxed interface {
    isBoxed()
}

type One struct {
    _0 dyn__Display
}

func (_ One) isBoxed() {}

type Pair struct {
    _0 dyn__Display
    _1 dyn__Display
}

func (_ Pair) isBoxed() {}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_i32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_i32_i_show(self__0 int32) string {
    var inline873 string = __goml_builtin_int32_to_string(self__0)
    return inline873
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t814 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t816 string
    var inline895 string = t814.vtable.show(t814.data)
    t816 = inline895
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline890)
    var t817 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t818 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t820 string
    var inline885 string = t817.vtable.show(t817.data)
    var inline886 string = inline885 + "-"
    var inline887 string = t818.vtable.show(t818.data)
    var inline888 string = inline886 + inline887
    t820 = inline888
    var inline875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline875)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t829 int64 = int64(int32(value__225))
    var inline909 bool = t829 < 0
    if inline909 {
        var inline910 uint64 = uint64(int64(t829))
        var inline911 uint64 = 0 - inline910
        var inline912 string = decimal_string(inline911)
        var inline913 string = "-" + inline912
        return inline913
    } else {
        var inline914 uint64 = uint64(int64(t829))
        var inline915 string = decimal_string(inline914)
        return inline915
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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
        var inline925 int = vec_len__Vec_5uint8(reversed__209)
        t848 = inline925
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t848)
        var offset__212 int = 0
        Loop_loop850:
        for {
            var t851 int
            var inline923 int = vec_len__Vec_5uint8(reversed__209)
            t851 = inline923
            var t852 bool = offset__212 < t851
            if t852 {
                var t853 int
                var inline921 int = vec_len__Vec_5uint8(reversed__209)
                t853 = inline921
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
