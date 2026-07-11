package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func vec_len__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display) int32 {
    return int32(len(vec.items))
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
    var retv24 string
    var t25 int32 = self__0.x
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    var t27 string = "Point(" + t26
    var t28 string = t27 + ","
    var t29 int32 = self__0.y
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t29)
    var t31 string = t28 + t30
    var t32 string = t31 + ")"
    retv24 = t32
    return retv24
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv34 string
    var t35 string = prefix__2 + "Point("
    var t36 int32 = self__1.x
    var t37 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t36)
    var t38 string = t35 + t37
    var t39 string = t38 + ","
    var t40 int32 = self__1.y
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    var t42 string = t39 + t41
    var t43 string = t42 + ")"
    var t44 string = t43 + suffix__3
    retv34 = t44
    return retv34
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv47 int32
    var t48 int32 = self__5.x
    var t49 int32 = self__5.y
    var t50 int32 = t48 + t49
    var t51 int32 = t50 + delta__6
    retv47 = t51
    return retv47
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv53 string
    var t56 bool = self__7.value
    var jp55 string
    if t56 {
        jp55 = "Flag(true)"
    } else {
        jp55 = "Flag(false)"
    }
    retv53 = jp55
    return retv53
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv58 string
    var t61 bool = self__8.value
    var jp60 string
    if t61 {
        var t62 string = prefix__9 + "Flag(true)"
        var t63 string = t62 + suffix__10
        jp60 = t63
    } else {
        var t64 string = prefix__9 + "Flag(false)"
        var t65 string = t64 + suffix__10
        jp60 = t65
    }
    retv58 = jp60
    return retv58
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv68 int32
    var t71 bool = self__12.value
    var jp70 int32
    if t71 {
        jp70 = delta__13
    } else {
        var t72 int32 = -delta__13
        jp70 = t72
    }
    retv68 = jp70
    return retv68
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv74 string
    var t75 *ref_int32_x = self__14.cell
    var t76 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t75)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    var t78 string = "Counter(" + t77
    var t79 string = t78 + ")"
    retv74 = t79
    return retv74
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv81 string
    var t82 string = prefix__16 + "Counter("
    var t83 *ref_int32_x = self__15.cell
    var t84 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t83)
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t84)
    var t86 string = t82 + t85
    var t87 string = t86 + ")"
    var t88 string = t87 + suffix__17
    retv81 = t88
    return retv81
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t90 *ref_int32_x = self__18.cell
    var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t90)
    var next__19 int32 = t91 + 1
    var t92 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t92, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv94 int32
    var t95 *ref_int32_x = self__20.cell
    var t96 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t95)
    var next__22 int32 = t96 + delta__21
    var t97 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t97, next__22)
    retv94 = next__22
    return retv94
}

func show_dyn(x__23 dyn__Display) string {
    var retv99 string
    var t100 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv99 = t100
    return retv99
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv102 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t103 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv102 = t103
    return retv102
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv105 func(dyn__Display) string
    var t106 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv105 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t106, p0)
    }
    return retv105
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv108 string
    x__31.vtable.tick(x__31.data)
    var t109 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t110 string = t109 + ":"
    var t111 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    var t113 string = t110 + t112
    retv108 = t113
    return retv108
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
    var t115 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t115,
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
    var t116 string = render_star__43(dp1__38)
    var t117 string = t116 + "|"
    var t118 string = render_angle__44(df1__40)
    var s2__47 string = t117 + t118
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp120 int32
    switch vlen__49 {
    case 2:
        jp120 = 3
    default:
        jp120 = 5
    }
    var delta__50 int32 = jp120
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop126:
    for {
        var t127 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t128 bool = t127 < 3
        if t128 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t129 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t130 int32 = t129 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t130)
            continue
        } else {
            break Loop_loop126
        }
    }
    var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(vlen__49)
    var t123 string = "len:" + t122
    println__T_string(t123)
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t125 string = "delta:" + t124
    println__T_string(t125)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv132 string
    var t133 string = _goml_runtime_core_int32_to_string(self__2)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv135 int32
    var t136 int32 = ref_get__Ref_5int32(self__115)
    retv135 = t136
    return retv135
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv140 *ref_int32_x
    var t141 *ref_int32_x = ref__Ref_5int32(value__114)
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv143 *_goml_vec_Dyn_Display
    var t144 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__73 *_goml_vec_Dyn_Display, elem__74 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__84 *_goml_vec_Dyn_Display) int32 {
    var retv148 int32
    var t149 int32 = vec_len__Vec_11Dyn_Display(self__84)
    retv148 = t149
    return retv148
}

func println__T_string(value__1 string) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t151)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv154 string
    retv154 = self__9
    return retv154
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env21 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv162 string
    var t163 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv162 = t163
    return retv162
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env22 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv165 string
    var tag__29 string = env22.tag_0
    var t166 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv165 = t166
    return retv165
}

func main() {
    main0()
}
