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

type _goml_vec_Dyn_Display struct {
    items []dyn__Display
}

func vec_new__Vec_11Dyn_Display() *_goml_vec_Dyn_Display {
    return &_goml_vec_Dyn_Display{
        items: nil,
    }
}

func vec_push__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display, elem dyn__Display) struct{} {
    vec.items = append(vec.items, elem)
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
    var inline876 string = __goml_builtin_int32_to_string(self__0)
    return inline876
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline900 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline900
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t807 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t807)
    var t808 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t808)
    var s__5 []dyn__Display
    var inline892 int = 0
    var inline893 int = 2
    var inline894 []dyn__Display = v__2.items[inline892:inline893]
    s__5 = inline894
    var t809 dyn__Display = s__5[0]
    var t810 string
    var inline890 string = t809.vtable.show(t809.data)
    t810 = inline890
    var inline887 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline887)
    var t__6 []dyn__Display
    var inline883 int = 1
    var inline884 int = 2
    var inline885 []dyn__Display = s__5[inline883:inline884]
    t__6 = inline885
    var t811 dyn__Display = t__6[0]
    var t812 string
    var inline881 string = t811.vtable.show(t811.data)
    t812 = inline881
    var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline878)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t832 int64 = int64(int32(value__225))
    var inline906 bool = t832 < 0
    if inline906 {
        var inline907 uint64 = uint64(int64(t832))
        var inline908 uint64 = 0 - inline907
        var inline909 string = decimal_string(inline908)
        var inline910 string = "-" + inline909
        return inline910
    } else {
        var inline911 uint64 = uint64(int64(t832))
        var inline912 string = decimal_string(inline911)
        return inline912
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t869 bool = value__208 == 0
    if t869 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop862:
        for {
            var t863 bool = remaining__210 > 0
            if t863 {
                var t864_rhs uint64 = 10
                var t864 uint64 = remaining__210 % t864_rhs
                var t865 uint8 = uint8(uint64(t864))
                var t866 uint8 = t865 + 48
                vec_push__Vec_5uint8(reversed__209, t866)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t867 uint64 = compound_old353 / compound_value354
                remaining__210 = t867
                continue
            } else {
                break Loop_loop862
            }
        }
        var t851 int
        var inline922 int = vec_len__Vec_5uint8(reversed__209)
        t851 = inline922
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t851)
        var offset__212 int = 0
        Loop_loop853:
        for {
            var t854 int
            var inline920 int = vec_len__Vec_5uint8(reversed__209)
            t854 = inline920
            var t855 bool = offset__212 < t854
            if t855 {
                var t856 int
                var inline918 int = vec_len__Vec_5uint8(reversed__209)
                t856 = inline918
                var t857 int = t856 - offset__212
                var t858 int = t857 - 1
                var t859 uint8 = vec_get__Vec_5uint8(reversed__209, t858)
                vec_push__Vec_5uint8(bytes__211, t859)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t860 int = compound_old358 + compound_value359
                offset__212 = t860
                continue
            } else {
                break Loop_loop853
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
