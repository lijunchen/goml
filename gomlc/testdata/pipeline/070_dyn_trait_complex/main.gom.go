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

func vec_len__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display) int {
    return int(len(vec.items))
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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
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

type Counter struct {
    cell *ref_int32_x
}

type closure_env_f_0 struct {}

type closure_env_make_renderer_1 struct {
    tag_0 string
}

type Ordering int32

type dyn__Display_vtable struct {
    show func(any) string
    show_with func(any, string, string) string
    tick func(any) struct{}
    bump func(any, int32) int32
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__Counter__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Counter_i_show(self.(Counter))
}

func dyn__Display__wrap__Counter__show_with(self any, p0 string, p1 string) string {
    return _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self.(Counter), p0, p1)
}

func dyn__Display__wrap__Counter__tick(self any) struct{} {
    return _goml_m_trait__impl_i_Display_i_Counter_i_tick(self.(Counter))
}

func dyn__Display__wrap__Counter__bump(self any, p0 int32) int32 {
    return _goml_m_trait__impl_i_Display_i_Counter_i_bump(self.(Counter), p0)
}

func dyn__Display__vtable__Counter() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Counter__show,
        show_with: dyn__Display__wrap__Counter__show_with,
        tick: dyn__Display__wrap__Counter__tick,
        bump: dyn__Display__wrap__Counter__bump,
    }
}

func dyn__Display__wrap__Flag__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Flag_i_show(self.(Flag))
}

func dyn__Display__wrap__Flag__show_with(self any, p0 string, p1 string) string {
    return _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self.(Flag), p0, p1)
}

func dyn__Display__wrap__Flag__tick(self any) struct{} {
    return _goml_m_trait__impl_i_Display_i_Flag_i_tick(self.(Flag))
}

func dyn__Display__wrap__Flag__bump(self any, p0 int32) int32 {
    return _goml_m_trait__impl_i_Display_i_Flag_i_bump(self.(Flag), p0)
}

func dyn__Display__vtable__Flag() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Flag__show,
        show_with: dyn__Display__wrap__Flag__show_with,
        tick: dyn__Display__wrap__Flag__tick,
        bump: dyn__Display__wrap__Flag__bump,
    }
}

func dyn__Display__wrap__Point__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show(self.(Point))
}

func dyn__Display__wrap__Point__show_with(self any, p0 string, p1 string) string {
    return _goml_m_trait__impl_i_Display_i_Point_i_show__with(self.(Point), p0, p1)
}

func dyn__Display__wrap__Point__tick(self any) struct{} {
    return _goml_m_trait__impl_i_Display_i_Point_i_tick(self.(Point))
}

func dyn__Display__wrap__Point__bump(self any, p0 int32) int32 {
    return _goml_m_trait__impl_i_Display_i_Point_i_bump(self.(Point), p0)
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
        show_with: dyn__Display__wrap__Point__show_with,
        tick: dyn__Display__wrap__Point__tick,
        bump: dyn__Display__wrap__Point__bump,
    }
}

func _goml_m_trait__impl_i_Display_i_Point_i_show(self__0 Point) string {
    var t814 int32 = self__0.x
    var t815 string
    var inline1011 string = __goml_builtin_int32_to_string(t814)
    t815 = inline1011
    var t816 string = "Point(" + t815
    var t817 string = t816 + ","
    var t818 int32 = self__0.y
    var t819 string
    var inline1009 string = __goml_builtin_int32_to_string(t818)
    t819 = inline1009
    var t820 string = t817 + t819
    var t821 string = t820 + ")"
    return t821
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t824 string = prefix__2 + "Point("
    var t825 int32 = self__1.x
    var t826 string
    var inline1015 string = __goml_builtin_int32_to_string(t825)
    t826 = inline1015
    var t827 string = t824 + t826
    var t828 string = t827 + ","
    var t829 int32 = self__1.y
    var t830 string
    var inline1013 string = __goml_builtin_int32_to_string(t829)
    t830 = inline1013
    var t831 string = t828 + t830
    var t832 string = t831 + ")"
    var t833 string = t832 + suffix__3
    return t833
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t837 int32 = self__5.x
    var t838 int32 = self__5.y
    var t839 int32 = t837 + t838
    var t840 int32 = t839 + delta__6
    return t840
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t845 bool = self__7.value
    if t845 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t850 bool = self__8.value
    if t850 {
        var t851 string = prefix__9 + "Flag(true)"
        var t852 string = t851 + suffix__10
        return t852
    } else {
        var t853 string = prefix__9 + "Flag(false)"
        var t854 string = t853 + suffix__10
        return t854
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t860 bool = self__12.value
    if t860 {
        return delta__13
    } else {
        var t861 int32 = -delta__13
        return t861
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t864 *ref_int32_x = self__14.cell
    var t865 int32
    var inline1019 int32 = ref_get__Ref_5int32(t864)
    t865 = inline1019
    var t866 string
    var inline1017 string = __goml_builtin_int32_to_string(t865)
    t866 = inline1017
    var t867 string = "Counter(" + t866
    var t868 string = t867 + ")"
    return t868
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t871 string = prefix__16 + "Counter("
    var t872 *ref_int32_x = self__15.cell
    var t873 int32
    var inline1023 int32 = ref_get__Ref_5int32(t872)
    t873 = inline1023
    var t874 string
    var inline1021 string = __goml_builtin_int32_to_string(t873)
    t874 = inline1021
    var t875 string = t871 + t874
    var t876 string = t875 + ")"
    var t877 string = t876 + suffix__17
    return t877
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t879 *ref_int32_x = self__18.cell
    var t880 int32
    var inline1027 int32 = ref_get__Ref_5int32(t879)
    t880 = inline1027
    var next__19 int32 = t880 + 1
    var t881 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t881, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t884 *ref_int32_x = self__20.cell
    var t885 int32
    var inline1031 int32 = ref_get__Ref_5int32(t884)
    t885 = inline1031
    var next__22 int32 = t885 + delta__21
    var t886 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t886, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t889 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t889
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t892 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t892, p0, p1)
    }
    var t893 string = f__28(x__24, tag__25)
    return t893
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t896 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t897 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t896, p0)
    }
    return t897
}

func main0() struct{} {
    var p1__33 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__34 Point = Point{
        x: 3,
        y: 4,
    }
    var f1__35 Flag = Flag{
        value: true,
    }
    var f2__36 Flag = Flag{
        value: false,
    }
    var t906 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(10)
    var c__37 Counter = Counter{
        cell: t906,
    }
    var dp1__38 dyn__Display = dyn__Display{
        data: p1__33,
        vtable: dyn__Display__vtable__Point(),
    }
    var dp2__39 dyn__Display = dyn__Display{
        data: p2__34,
        vtable: dyn__Display__vtable__Point(),
    }
    var df1__40 dyn__Display = dyn__Display{
        data: f1__35,
        vtable: dyn__Display__vtable__Flag(),
    }
    var df2__41 dyn__Display = dyn__Display{
        data: f2__36,
        vtable: dyn__Display__vtable__Flag(),
    }
    var dc__42 dyn__Display = dyn__Display{
        data: c__37,
        vtable: dyn__Display__vtable__Counter(),
    }
    var render_star__43 func(dyn__Display) string = make_renderer("*")
    var render_angle__44 func(dyn__Display) string = make_renderer("<")
    var s0__45 string = show_dyn(dp2__39)
    var s1__46 string = call_via_closure(df2__41, "*")
    var t907 string = render_star__43(dp1__38)
    var t908 string = t907 + "|"
    var t909 string = render_angle__44(df1__40)
    var s2__47 string = t908 + t909
    var v__48 *_goml_vec_Dyn_Display
    var inline1081 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline1081
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline1073 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline1073
    var jp911 int32
    switch vlen__49 {
    case 2:
        jp911 = 3
    default:
        jp911 = 5
    }
    var inline1070 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline1070)
    var inline1067 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline1067)
    var inline1064 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline1064)
    var i__51 *ref_int32_x
    var inline1061 int32 = 0
    var inline1062 *ref_int32_x = ref__Ref_5int32(inline1061)
    i__51 = inline1062
    Loop_loop917:
    for {
        var t918 int32
        var inline1049 int32 = ref_get__Ref_5int32(i__51)
        t918 = inline1049
        var t919 bool = t918 < 3
        if t919 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline1043 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline1044 string = inline1043 + ":"
            var inline1045 int32 = dc__42.vtable.bump(dc__42.data, jp911)
            var inline1046 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1045)
            var inline1047 string = inline1044 + inline1046
            line__52 = inline1047
            var inline1039 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline1039)
            var t920 int32
            var inline1037 int32 = ref_get__Ref_5int32(i__51)
            t920 = inline1037
            var t921 int32 = t920 + 1
            ref_set__Ref_5int32(i__51, t921)
            continue
        } else {
            break Loop_loop917
        }
    }
    var t913 string
    var inline1059 string = __goml_builtin_int_to_string(vlen__49)
    t913 = inline1059
    var t914 string = "len:" + t913
    var inline1056 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t914)
    _goml_runtime_core_string_println(inline1056)
    var t915 string
    var inline1054 string = __goml_builtin_int32_to_string(jp911)
    t915 = inline1054
    var t916 string = "delta:" + t915
    var inline1051 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t916)
    _goml_runtime_core_string_println(inline1051)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline1083 int64 = int64(int32(self__286))
    var inline1084 string = signed_decimal_string(inline1083)
    return inline1084
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__i32(value__684 int32) *ref_int32_x {
    var t932 *ref_int32_x = ref__Ref_5int32(value__684)
    return t932
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t949 int64 = int64(int32(value__225))
    var inline1090 bool = t949 < 0
    if inline1090 {
        var inline1091 uint64 = uint64(int64(t949))
        var inline1092 uint64 = 0 - inline1091
        var inline1093 string = decimal_string(inline1092)
        var inline1094 string = "-" + inline1093
        return inline1094
    } else {
        var inline1095 uint64 = uint64(int64(t949))
        var inline1096 string = decimal_string(inline1095)
        return inline1096
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t955 int64 = int64(int(value__222))
    var inline1098 bool = t955 < 0
    if inline1098 {
        var inline1099 uint64 = uint64(int64(t955))
        var inline1100 uint64 = 0 - inline1099
        var inline1101 string = decimal_string(inline1100)
        var inline1102 string = "-" + inline1101
        return inline1102
    } else {
        var inline1103 uint64 = uint64(int64(t955))
        var inline1104 string = decimal_string(inline1103)
        return inline1104
    }
}

func signed_decimal_string(value__214 int64) string {
    var t961 bool = value__214 < 0
    if t961 {
        var t962 uint64 = uint64(int64(value__214))
        var t963 uint64 = 0 - t962
        var t964 string = decimal_string(t963)
        var t965 string = "-" + t964
        return t965
    } else {
        var t966 uint64 = uint64(int64(value__214))
        var t967 string = decimal_string(t966)
        return t967
    }
}

func decimal_string(value__208 uint64) string {
    var t990 bool = value__208 == 0
    if t990 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop983:
        for {
            var t984 bool = remaining__210 > 0
            if t984 {
                var t985_rhs uint64 = 10
                var t985 uint64 = remaining__210 % t985_rhs
                var t986 uint8 = uint8(uint64(t985))
                var t987 uint8 = t986 + 48
                vec_push__Vec_5uint8(reversed__209, t987)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t988 uint64 = compound_old353 / compound_value354
                remaining__210 = t988
                continue
            } else {
                break Loop_loop983
            }
        }
        var t972 int
        var inline1114 int = vec_len__Vec_5uint8(reversed__209)
        t972 = inline1114
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t972)
        var offset__212 int = 0
        Loop_loop974:
        for {
            var t975 int
            var inline1112 int = vec_len__Vec_5uint8(reversed__209)
            t975 = inline1112
            var t976 bool = offset__212 < t975
            if t976 {
                var t977 int
                var inline1110 int = vec_len__Vec_5uint8(reversed__209)
                t977 = inline1110
                var t978 int = t977 - offset__212
                var t979 int = t978 - 1
                var t980 uint8 = vec_get__Vec_5uint8(reversed__209, t979)
                vec_push__Vec_5uint8(bytes__211, t980)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t981 int = compound_old358 + compound_value359
                offset__212 = t981
                continue
            } else {
                break Loop_loop974
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env810 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t1004 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t1004
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env811 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env811.tag_0
    var t1007 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t1007
}

func main() {
    main0()
}
