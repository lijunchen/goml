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
    var retv39 string
    var t40 int32 = self__0.x
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    var t42 string = "Point(" + t41
    var t43 string = t42 + ","
    var t44 int32 = self__0.y
    var t45 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t44)
    var t46 string = t43 + t45
    var t47 string = t46 + ")"
    retv39 = t47
    return retv39
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv49 string
    var t50 string = prefix__2 + "Point("
    var t51 int32 = self__1.x
    var t52 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t51)
    var t53 string = t50 + t52
    var t54 string = t53 + ","
    var t55 int32 = self__1.y
    var t56 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t55)
    var t57 string = t54 + t56
    var t58 string = t57 + ")"
    var t59 string = t58 + suffix__3
    retv49 = t59
    return retv49
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv62 int32
    var t63 int32 = self__5.x
    var t64 int32 = self__5.y
    var t65 int32 = t63 + t64
    var t66 int32 = t65 + delta__6
    retv62 = t66
    return retv62
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv68 string
    var t71 bool = self__7.value
    var jp70 string
    if t71 {
        jp70 = "Flag(true)"
    } else {
        jp70 = "Flag(false)"
    }
    retv68 = jp70
    return retv68
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv73 string
    var t76 bool = self__8.value
    var jp75 string
    if t76 {
        var t77 string = prefix__9 + "Flag(true)"
        var t78 string = t77 + suffix__10
        jp75 = t78
    } else {
        var t79 string = prefix__9 + "Flag(false)"
        var t80 string = t79 + suffix__10
        jp75 = t80
    }
    retv73 = jp75
    return retv73
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv83 int32
    var t86 bool = self__12.value
    var jp85 int32
    if t86 {
        jp85 = delta__13
    } else {
        var t87 int32 = -delta__13
        jp85 = t87
    }
    retv83 = jp85
    return retv83
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv89 string
    var t90 *ref_int32_x = self__14.cell
    var t91 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t90)
    var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t91)
    var t93 string = "Counter(" + t92
    var t94 string = t93 + ")"
    retv89 = t94
    return retv89
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv96 string
    var t97 string = prefix__16 + "Counter("
    var t98 *ref_int32_x = self__15.cell
    var t99 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t98)
    var t100 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t99)
    var t101 string = t97 + t100
    var t102 string = t101 + ")"
    var t103 string = t102 + suffix__17
    retv96 = t103
    return retv96
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t105 *ref_int32_x = self__18.cell
    var t106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t105)
    var next__19 int32 = t106 + 1
    var t107 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t107, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv109 int32
    var t110 *ref_int32_x = self__20.cell
    var t111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t110)
    var next__22 int32 = t111 + delta__21
    var t112 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t112, next__22)
    retv109 = next__22
    return retv109
}

func show_dyn(x__23 dyn__Display) string {
    var retv114 string
    var t115 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv114 = t115
    return retv114
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv117 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t118 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv117 = t118
    return retv117
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv120 func(dyn__Display) string
    var t121 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv120 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t121, p0)
    }
    return retv120
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv123 string
    x__31.vtable.tick(x__31.data)
    var t124 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t125 string = t124 + ":"
    var t126 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    var t128 string = t125 + t127
    retv123 = t128
    return retv123
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
    var t130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t130,
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
    var t131 string = render_star__43(dp1__38)
    var t132 string = t131 + "|"
    var t133 string = render_angle__44(df1__40)
    var s2__47 string = t132 + t133
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp135 int32
    switch vlen__49 {
    case 2:
        jp135 = 3
    default:
        jp135 = 5
    }
    var delta__50 int32 = jp135
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop141:
    for {
        var t142 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t143 bool = t142 < 3
        if t143 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t145 int32 = t144 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t145)
            continue
        } else {
            break Loop_loop141
        }
    }
    var t137 string = _goml_m_inherent_i_int32_i_int32_i_to__string(vlen__49)
    var t138 string = "len:" + t137
    println__T_string(t138)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t140 string = "delta:" + t139
    println__T_string(t140)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv147 string
    var t148 string = _goml_runtime_core_int32_to_string(self__2)
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv150 int32
    var t151 int32 = ref_get__Ref_5int32(self__138)
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv155 *ref_int32_x
    var t156 *ref_int32_x = ref__Ref_5int32(value__137)
    retv155 = t156
    return retv155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv158 *_goml_vec_Dyn_Display
    var t159 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv158 = t159
    return retv158
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__96 *_goml_vec_Dyn_Display, elem__97 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__107 *_goml_vec_Dyn_Display) int32 {
    var retv163 int32
    var t164 int32 = vec_len__Vec_11Dyn_Display(self__107)
    retv163 = t164
    return retv163
}

func println__T_string(value__1 string) struct{} {
    var t166 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t166)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv169 string
    retv169 = self__9
    return retv169
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env36 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv177 string
    var t178 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv177 = t178
    return retv177
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env37 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv180 string
    var tag__29 string = env37.tag_0
    var t181 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv180 = t181
    return retv180
}

func main() {
    main0()
}
