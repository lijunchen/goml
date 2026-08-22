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

type Holder__dynDisplay struct {
    value dyn__Display
}

type Ordering int32

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
    var inline859 string = __goml_builtin_int32_to_string(self__0)
    return inline859
}

func main0() struct{} {
    var value__2 int32 = 42
    var t804 dyn__Display = dyn__Display{
        data: int32(value__2),
        vtable: dyn__Display__vtable__int32(),
    }
    var t806 string
    var inline864 string = t804.vtable.show(t804.data)
    t806 = inline864
    var inline861 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline861)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t815 int64 = int64(int32(value__225))
    var inline870 bool = t815 < 0
    if inline870 {
        var inline871 uint64 = uint64(int64(t815))
        var inline872 uint64 = 0 - inline871
        var inline873 string = decimal_string(inline872)
        var inline874 string = "-" + inline873
        return inline874
    } else {
        var inline875 uint64 = uint64(int64(t815))
        var inline876 string = decimal_string(inline875)
        return inline876
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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
        var inline886 int = vec_len__Vec_5uint8(reversed__209)
        t834 = inline886
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t834)
        var offset__212 int = 0
        Loop_loop836:
        for {
            var t837 int
            var inline884 int = vec_len__Vec_5uint8(reversed__209)
            t837 = inline884
            var t838 bool = offset__212 < t837
            if t838 {
                var t839 int
                var inline882 int = vec_len__Vec_5uint8(reversed__209)
                t839 = inline882
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
