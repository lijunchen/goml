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

type Number struct {
    value int
}

type Ordering int32

type dyn__Display_vtable struct {
    display func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

type dyn__Source_vtable struct {
    get func(any) int
}

type dyn__Source struct {
    data any
    vtable *dyn__Source_vtable
}

func dyn__Display__wrap__Number__display(self any) string {
    return _goml_m_trait__impl_i_Display_i_Number_i_display(self.(Number))
}

func dyn__Display__vtable__Number() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        display: dyn__Display__wrap__Number__display,
    }
}

func dyn__Source__wrap__Number__get(self any) int {
    return _goml_m_trait__impl_i_Source_i_Number_i_get(self.(Number))
}

func dyn__Source__vtable__Number() *dyn__Source_vtable {
    return &dyn__Source_vtable{
        get: dyn__Source__wrap__Number__get,
    }
}

func _goml_m_trait__impl_i_Display_i_Number_i_display(self__0 Number) string {
    var t801 int = self__0.value
    var inline877 string = __goml_builtin_int_to_string(t801)
    return inline877
}

func _goml_m_trait__impl_i_Source_i_Number_i_get(self__1 Number) int {
    var t805 int = self__1.value
    return t805
}

func main0() struct{} {
    var t807 Number = Number{
        value: 42,
    }
    var display__3 dyn__Display = dyn__Display{
        data: t807,
        vtable: dyn__Display__vtable__Number(),
    }
    var t808 string = display__3.vtable.display(display__3.data)
    var inline890 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline890)
    var t809 Number = Number{
        value: 7,
    }
    var erased__4 dyn__Display
    var inline888 dyn__Display = dyn__Display{
        data: t809,
        vtable: dyn__Display__vtable__Number(),
    }
    erased__4 = inline888
    var t810 string = erased__4.vtable.display(erased__4.data)
    var inline885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline885)
    var t811 Number = Number{
        value: 11,
    }
    var source__5 dyn__Source = dyn__Source{
        data: t811,
        vtable: dyn__Source__vtable__Number(),
    }
    var t812 int = source__5.vtable.get(source__5.data)
    var inline882 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t812)
    _goml_runtime_core_string_println(inline882)
    var t813 Number = Number{
        value: 13,
    }
    var same__6 dyn__Display = dyn__Display{
        data: t813,
        vtable: dyn__Display__vtable__Number(),
    }
    var t814 string = same__6.vtable.display(same__6.data)
    var inline879 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline879)
    return struct{}{}
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t830 int64 = int64(int(value__222))
    var inline899 bool = t830 < 0
    if inline899 {
        var inline900 uint64 = uint64(int64(t830))
        var inline901 uint64 = 0 - inline900
        var inline902 string = decimal_string(inline901)
        var inline903 string = "-" + inline902
        return inline903
    } else {
        var inline904 uint64 = uint64(int64(t830))
        var inline905 string = decimal_string(inline904)
        return inline905
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline907 int64 = int64(int(self__404))
    var inline908 string = signed_decimal_string(inline907)
    return inline908
}

func signed_decimal_string(value__214 int64) string {
    var t841 bool = value__214 < 0
    if t841 {
        var t842 uint64 = uint64(int64(value__214))
        var t843 uint64 = 0 - t842
        var t844 string = decimal_string(t843)
        var t845 string = "-" + t844
        return t845
    } else {
        var t846 uint64 = uint64(int64(value__214))
        var t847 string = decimal_string(t846)
        return t847
    }
}

func decimal_string(value__208 uint64) string {
    var t870 bool = value__208 == 0
    if t870 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop863:
        for {
            var t864 bool = remaining__210 > 0
            if t864 {
                var t865_rhs uint64 = 10
                var t865 uint64 = remaining__210 % t865_rhs
                var t866 uint8 = uint8(uint64(t865))
                var t867 uint8 = t866 + 48
                vec_push__Vec_5uint8(reversed__209, t867)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t868 uint64 = compound_old353 / compound_value354
                remaining__210 = t868
                continue
            } else {
                break Loop_loop863
            }
        }
        var t852 int
        var inline918 int = vec_len__Vec_5uint8(reversed__209)
        t852 = inline918
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t852)
        var offset__212 int = 0
        Loop_loop854:
        for {
            var t855 int
            var inline916 int = vec_len__Vec_5uint8(reversed__209)
            t855 = inline916
            var t856 bool = offset__212 < t855
            if t856 {
                var t857 int
                var inline914 int = vec_len__Vec_5uint8(reversed__209)
                t857 = inline914
                var t858 int = t857 - offset__212
                var t859 int = t858 - 1
                var t860 uint8 = vec_get__Vec_5uint8(reversed__209, t859)
                vec_push__Vec_5uint8(bytes__211, t860)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t861 int = compound_old358 + compound_value359
                offset__212 = t861
                continue
            } else {
                break Loop_loop854
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
