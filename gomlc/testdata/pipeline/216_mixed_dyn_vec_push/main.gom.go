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

type _goml_vec_Dyn_Show struct {
    items []dyn__Show
}

func vec_new__Vec_8Dyn_Show() *_goml_vec_Dyn_Show {
    return &_goml_vec_Dyn_Show{
        items: nil,
    }
}

func vec_push__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, elem dyn__Show) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_8Dyn_Show(vec *_goml_vec_Dyn_Show, index int) dyn__Show {
    return vec.items[index]
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

type Wrap struct {
    value string
}

type Ordering int32

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__Wrap__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_Wrap_i_show(self.(Wrap))
}

func dyn__Show__vtable__Wrap() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__Wrap__show,
    }
}

func dyn__Show__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Show_i_i32_i_show(self.(int32))
}

func dyn__Show__vtable__int32() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Show_i_i32_i_show(self__0 int32) string {
    var inline871 string = __goml_builtin_int32_to_string(self__0)
    return inline871
}

func _goml_m_trait__impl_i_Show_i_Wrap_i_show(self__1 Wrap) string {
    var t804 string = self__1.value
    return t804
}

func main0() struct{} {
    var values__2 *_goml_vec_Dyn_Show
    var inline883 *_goml_vec_Dyn_Show = vec_new__Vec_8Dyn_Show()
    values__2 = inline883
    var value__3 int32 = 10
    var t806 dyn__Show = dyn__Show{
        data: int32(value__3),
        vtable: dyn__Show__vtable__int32(),
    }
    vec_push__Vec_8Dyn_Show(values__2, t806)
    var t807 Wrap = Wrap{
        value: "ok",
    }
    var t808 dyn__Show = dyn__Show{
        data: t807,
        vtable: dyn__Show__vtable__Wrap(),
    }
    vec_push__Vec_8Dyn_Show(values__2, t808)
    var t809 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 0)
    var t810 string = t809.vtable.show(t809.data)
    var inline876 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline876)
    var t811 dyn__Show = vec_get__Vec_8Dyn_Show(values__2, 1)
    var t812 string = t811.vtable.show(t811.data)
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t827 int64 = int64(int32(value__225))
    var inline889 bool = t827 < 0
    if inline889 {
        var inline890 uint64 = uint64(int64(t827))
        var inline891 uint64 = 0 - inline890
        var inline892 string = decimal_string(inline891)
        var inline893 string = "-" + inline892
        return inline893
    } else {
        var inline894 uint64 = uint64(int64(t827))
        var inline895 string = decimal_string(inline894)
        return inline895
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t864 bool = value__208 == 0
    if t864 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop857:
        for {
            var t858 bool = remaining__210 > 0
            if t858 {
                var t859_rhs uint64 = 10
                var t859 uint64 = remaining__210 % t859_rhs
                var t860 uint8 = uint8(uint64(t859))
                var t861 uint8 = t860 + 48
                vec_push__Vec_5uint8(reversed__209, t861)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t862 uint64 = compound_old353 / compound_value354
                remaining__210 = t862
                continue
            } else {
                break Loop_loop857
            }
        }
        var t846 int
        var inline905 int = vec_len__Vec_5uint8(reversed__209)
        t846 = inline905
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t846)
        var offset__212 int = 0
        Loop_loop848:
        for {
            var t849 int
            var inline903 int = vec_len__Vec_5uint8(reversed__209)
            t849 = inline903
            var t850 bool = offset__212 < t849
            if t850 {
                var t851 int
                var inline901 int = vec_len__Vec_5uint8(reversed__209)
                t851 = inline901
                var t852 int = t851 - offset__212
                var t853 int = t852 - 1
                var t854 uint8 = vec_get__Vec_5uint8(reversed__209, t853)
                vec_push__Vec_5uint8(bytes__211, t854)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t855 int = compound_old358 + compound_value359
                offset__212 = t855
                continue
            } else {
                break Loop_loop848
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
