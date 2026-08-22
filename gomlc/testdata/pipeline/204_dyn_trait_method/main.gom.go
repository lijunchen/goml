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

type Point struct {
    value int32
}

type Ordering int32

type dyn__Display_vtable struct {
    show func(any, string) string
    name func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Point__show(self any, p0 string) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show(self.(Point), p0)
}

func dyn__Display__wrap__Point__name(self any) string {
    return _goml_m_trait__impl_i_Named_i_Point_i_name(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
        name: dyn__Display__wrap__Point__name,
    }
}

func _goml_m_trait__impl_i_Named_i_Point_i_name(self__0 Point) string {
    return "point"
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__1 Point, prefix__2 string) string {
    var t804 int32 = self__1.value
    var t805 string
    var inline865 string = __goml_builtin_int32_to_string(t804)
    t805 = inline865
    var t806 string = prefix__2 + t805
    return t806
}

func main0() struct{} {
    var t812 Point = Point{
        value: 7,
    }
    var value__4 dyn__Display = dyn__Display{
        data: t812,
        vtable: dyn__Display__vtable__Point(),
    }
    var inline876 string = value__4.vtable.show(value__4.data, "value=")
    println__T_string(inline876)
    var inline878 string = value__4.vtable.name(value__4.data)
    println__T_string(inline878)
    var inline880 string = value__4.vtable.show(value__4.data, "again=")
    println__T_string(inline880)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t817 string
    t817 = value__1
    _goml_runtime_core_string_println(t817)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t821 int64 = int64(int32(value__225))
    var inline887 bool = t821 < 0
    if inline887 {
        var inline888 uint64 = uint64(int64(t821))
        var inline889 uint64 = 0 - inline888
        var inline890 string = decimal_string(inline889)
        var inline891 string = "-" + inline890
        return inline891
    } else {
        var inline892 uint64 = uint64(int64(t821))
        var inline893 string = decimal_string(inline892)
        return inline893
    }
}

func decimal_string(value__208 uint64) string {
    var t858 bool = value__208 == 0
    if t858 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop851:
        for {
            var t852 bool = remaining__210 > 0
            if t852 {
                var t853_rhs uint64 = 10
                var t853 uint64 = remaining__210 % t853_rhs
                var t854 uint8 = uint8(uint64(t853))
                var t855 uint8 = t854 + 48
                vec_push__Vec_5uint8(reversed__209, t855)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t856 uint64 = compound_old353 / compound_value354
                remaining__210 = t856
                continue
            } else {
                break Loop_loop851
            }
        }
        var t840 int
        var inline903 int = vec_len__Vec_5uint8(reversed__209)
        t840 = inline903
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t840)
        var offset__212 int = 0
        Loop_loop842:
        for {
            var t843 int
            var inline901 int = vec_len__Vec_5uint8(reversed__209)
            t843 = inline901
            var t844 bool = offset__212 < t843
            if t844 {
                var t845 int
                var inline899 int = vec_len__Vec_5uint8(reversed__209)
                t845 = inline899
                var t846 int = t845 - offset__212
                var t847 int = t846 - 1
                var t848 uint8 = vec_get__Vec_5uint8(reversed__209, t847)
                vec_push__Vec_5uint8(bytes__211, t848)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t849 int = compound_old358 + compound_value359
                offset__212 = t849
                continue
            } else {
                break Loop_loop842
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
