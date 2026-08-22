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
    x int32
    y int32
}

type Flag struct {
    value bool
}

type Ordering int32

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Flag_i_show(self.(Flag))
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
    }
}

func dyn__Display__wrap__Point__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show(self.(Point))
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
    }
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__0 Point) string {
    var t800 int32 = self__0.x
    var t801 string
    var inline870 string = __goml_builtin_int32_to_string(t800)
    t801 = inline870
    var t802 string = "Point(" + t801
    var t803 string = t802 + ","
    var t804 int32 = self__0.y
    var t805 string
    var inline868 string = __goml_builtin_int32_to_string(t804)
    t805 = inline868
    var t806 string = t803 + t805
    var t807 string = t806 + ")"
    return t807
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__1 Flag) string {
    var t812 bool = self__1.value
    if t812 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func main0() struct{} {
    var p__2 Point = Point{
        x: 1,
        y: 2,
    }
    var t__3 Flag = Flag{
        value: true,
    }
    var dp__4 dyn__Display = dyn__Display{
        data: p__2,
        vtable: dyn__Display__vtable__Point(),
    }
    var dt__5 dyn__Display = dyn__Display{
        data: t__3,
        vtable: dyn__Display__vtable__Flag(),
    }
    var t814 string = dp__4.vtable.show(dp__4.data)
    var inline875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline875)
    var t815 string = dt__5.vtable.show(dt__5.data)
    var inline872 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline872)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t824 int64 = int64(int32(value__225))
    var inline882 bool = t824 < 0
    if inline882 {
        var inline883 uint64 = uint64(int64(t824))
        var inline884 uint64 = 0 - inline883
        var inline885 string = decimal_string(inline884)
        var inline886 string = "-" + inline885
        return inline886
    } else {
        var inline887 uint64 = uint64(int64(t824))
        var inline888 string = decimal_string(inline887)
        return inline888
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t861 bool = value__208 == 0
    if t861 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop854:
        for {
            var t855 bool = remaining__210 > 0
            if t855 {
                var t856_rhs uint64 = 10
                var t856 uint64 = remaining__210 % t856_rhs
                var t857 uint8 = uint8(uint64(t856))
                var t858 uint8 = t857 + 48
                vec_push__Vec_5uint8(reversed__209, t858)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t859 uint64 = compound_old353 / compound_value354
                remaining__210 = t859
                continue
            } else {
                break Loop_loop854
            }
        }
        var t843 int
        var inline898 int = vec_len__Vec_5uint8(reversed__209)
        t843 = inline898
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t843)
        var offset__212 int = 0
        Loop_loop845:
        for {
            var t846 int
            var inline896 int = vec_len__Vec_5uint8(reversed__209)
            t846 = inline896
            var t847 bool = offset__212 < t846
            if t847 {
                var t848 int
                var inline894 int = vec_len__Vec_5uint8(reversed__209)
                t848 = inline894
                var t849 int = t848 - offset__212
                var t850 int = t849 - 1
                var t851 uint8 = vec_get__Vec_5uint8(reversed__209, t850)
                vec_push__Vec_5uint8(bytes__211, t851)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t852 int = compound_old358 + compound_value359
                offset__212 = t852
                continue
            } else {
                break Loop_loop845
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
