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

type closure_env_f_0 struct {
    y_0 int32
    z_1 int32
}

type closure_env_add_base_1 struct {
    base_0 int32
}

type closure_env_printer_2 struct {}

type closure_env_no_capture_3 struct {}

type closure_env_play_list_and_point_4 struct {
    list123_0 IntList
    point_1 Point
}

type Ordering int32

type IntList interface {
    isIntList()
}

type Nil struct {}

func (_ Nil) isIntList() {}

type Cons struct {
    _0 int32
    _1 IntList
}

func (_ Cons) isIntList() {}

func main0() struct{} {
    var base__6 int32 = 5
    var t823 closure_env_add_base_1 = closure_env_add_base_1{
        base_0: base__6,
    }
    var add_base__8 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(t823, p0)
    }
    var result__9 int32 = add_base__8(7)
    var t824 closure_env_printer_2 = closure_env_printer_2{}
    var printer__13 func(string, int32) struct{} = func(p0 string, p1 int32) struct{} {
        return _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(t824, p0, p1)
    }
    printer__13("result: ", result__9)
    var t825 closure_env_no_capture_3 = closure_env_no_capture_3{}
    var no_capture__17 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(t825, p0)
    }
    var doubled__18 int32 = no_capture__17(3)
    var t826 string
    var inline929 string = __goml_builtin_int32_to_string(doubled__18)
    t826 = inline929
    var inline926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline926)
    var inline915 int32 = 3
    var inline916 int32 = 5
    var inline917 closure_env_f_0 = closure_env_f_0{
        y_0: inline915,
        z_1: inline916,
    }
    var inline918 func(int32) int32 = func(p0 int32) int32 {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline917, p0)
    }
    var inline919 int32 = inline918(2)
    var inline920 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline919)
    println__T_string(inline920)
    var inline922 int32 = inline918(3)
    var inline923 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline922)
    println__T_string(inline923)
    var t827 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t828 IntList = Cons{
        _0: 2,
        _1: t827,
    }
    var list123__19 IntList = Cons{
        _0: 1,
        _1: t828,
    }
    var point__20 Point = Point{
        x: 10,
        y: 20,
    }
    var t829 closure_env_play_list_and_point_4 = closure_env_play_list_and_point_4{
        list123_0: list123__19,
        point_1: point__20,
    }
    var play_list_and_point__25 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(t829)
    }
    play_list_and_point__25()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t831 string
    t831 = value__1
    _goml_runtime_core_string_println(t831)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline932 int64 = int64(int32(self__286))
    var inline933 string = signed_decimal_string(inline932)
    return inline933
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t840 int64 = int64(int32(value__225))
    var inline935 bool = t840 < 0
    if inline935 {
        var inline936 uint64 = uint64(int64(t840))
        var inline937 uint64 = 0 - inline936
        var inline938 string = decimal_string(inline937)
        var inline939 string = "-" + inline938
        return inline939
    } else {
        var inline940 uint64 = uint64(int64(t840))
        var inline941 string = decimal_string(inline940)
        return inline941
    }
}

func signed_decimal_string(value__214 int64) string {
    var t846 bool = value__214 < 0
    if t846 {
        var t847 uint64 = uint64(int64(value__214))
        var t848 uint64 = 0 - t847
        var t849 string = decimal_string(t848)
        var t850 string = "-" + t849
        return t850
    } else {
        var t851 uint64 = uint64(int64(value__214))
        var t852 string = decimal_string(t851)
        return t852
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
        var inline951 int = vec_len__Vec_5uint8(reversed__209)
        t857 = inline951
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t857)
        var offset__212 int = 0
        Loop_loop859:
        for {
            var t860 int
            var inline949 int = vec_len__Vec_5uint8(reversed__209)
            t860 = inline949
            var t861 bool = offset__212 < t860
            if t861 {
                var t862 int
                var inline947 int = vec_len__Vec_5uint8(reversed__209)
                t862 = inline947
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

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env807 closure_env_f_0, x__2 int32) int32 {
    var y__0 int32 = env807.y_0
    var z__1 int32 = env807.z_1
    var t883 int32 = x__2 * y__0
    var t884 int32 = t883 * z__1
    return t884
}

func _goml_m_inherent_i_closure__en_he0443db01d9642f998bbe31aa65b7a79_base__1_i_apply(env808 closure_env_add_base_1, x__7 int32) int32 {
    var base__6 int32 = env808.base_0
    var t887 int32 = x__7 + base__6
    return t887
}

func _goml_m_inherent_i_closure__env__printer__2_i_closure__env__printer__2_i_apply(env809 closure_env_printer_2, prefix__10 string, value__11 int32) struct{} {
    var t889 string
    var inline956 string = __goml_builtin_int32_to_string(value__11)
    t889 = inline956
    var message__12 string = prefix__10 + t889
    var inline953 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__12)
    _goml_runtime_core_string_println(inline953)
    return struct{}{}
}

func _goml_m_inherent_i_closure__en_ha1a1d2e736bef56b17edec01979b6eae_ture__3_i_apply(env810 closure_env_no_capture_3, z__16 int32) int32 {
    var t893 int32 = z__16 * 2
    return t893
}

func _goml_m_inherent_i_closure__en_h905154c8b1f2c223fea35335436a9056_oint__4_i_apply(env811 closure_env_play_list_and_point_4) struct{} {
    var list123__19 IntList = env811.list123_0
    var point__20 Point = env811.point_1
    switch list123__19.(type) {
    case Nil:
        var inline958 string = "Empty list"
        var inline959 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline958)
        _goml_runtime_core_string_println(inline959)
        return struct{}{}
    case Cons:
        var x800 int32 = list123__19.(Cons)._0
        var t897 string
        var inline972 string = __goml_builtin_int32_to_string(x800)
        t897 = inline972
        var inline969 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t897)
        _goml_runtime_core_string_println(inline969)
        var x803 int32 = point__20.x
        var x804 int32 = point__20.y
        var t898 string
        var inline967 string = __goml_builtin_int32_to_string(x803)
        t898 = inline967
        var t899 string = "Point: (" + t898
        var t900 string = t899 + ", "
        var t901 string
        var inline965 string = __goml_builtin_int32_to_string(x804)
        t901 = inline965
        var t902 string = t900 + t901
        var t903 string = t902 + ")"
        var inline962 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t903)
        _goml_runtime_core_string_println(inline962)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
