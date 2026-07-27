package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

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

func vec_len__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display) int {
    return int(len(vec.items))
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
    var retv81 string
    var t82 int32 = self__0.x
    var t83 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t82)
    var t84 string = "Point(" + t83
    var t85 string = t84 + ","
    var t86 int32 = self__0.y
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    var t88 string = t85 + t87
    var t89 string = t88 + ")"
    retv81 = t89
    return retv81
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv91 string
    var t92 string = prefix__2 + "Point("
    var t93 int32 = self__1.x
    var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t93)
    var t95 string = t92 + t94
    var t96 string = t95 + ","
    var t97 int32 = self__1.y
    var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t97)
    var t99 string = t96 + t98
    var t100 string = t99 + ")"
    var t101 string = t100 + suffix__3
    retv91 = t101
    return retv91
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv104 int32
    var t105 int32 = self__5.x
    var t106 int32 = self__5.y
    var t107 int32 = t105 + t106
    var t108 int32 = t107 + delta__6
    retv104 = t108
    return retv104
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv110 string
    var t113 bool = self__7.value
    var jp112 string
    if t113 {
        jp112 = "Flag(true)"
    } else {
        jp112 = "Flag(false)"
    }
    retv110 = jp112
    return retv110
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv115 string
    var t118 bool = self__8.value
    var jp117 string
    if t118 {
        var t119 string = prefix__9 + "Flag(true)"
        var t120 string = t119 + suffix__10
        jp117 = t120
    } else {
        var t121 string = prefix__9 + "Flag(false)"
        var t122 string = t121 + suffix__10
        jp117 = t122
    }
    retv115 = jp117
    return retv115
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv125 int32
    var t128 bool = self__12.value
    var jp127 int32
    if t128 {
        jp127 = delta__13
    } else {
        var t129 int32 = -delta__13
        jp127 = t129
    }
    retv125 = jp127
    return retv125
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv131 string
    var t132 *ref_int32_x = self__14.cell
    var t133 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t132)
    var t134 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t133)
    var t135 string = "Counter(" + t134
    var t136 string = t135 + ")"
    retv131 = t136
    return retv131
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv138 string
    var t139 string = prefix__16 + "Counter("
    var t140 *ref_int32_x = self__15.cell
    var t141 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t140)
    var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t141)
    var t143 string = t139 + t142
    var t144 string = t143 + ")"
    var t145 string = t144 + suffix__17
    retv138 = t145
    return retv138
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t147 *ref_int32_x = self__18.cell
    var t148 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t147)
    var next__19 int32 = t148 + 1
    var t149 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t149, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv151 int32
    var t152 *ref_int32_x = self__20.cell
    var t153 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t152)
    var next__22 int32 = t153 + delta__21
    var t154 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t154, next__22)
    retv151 = next__22
    return retv151
}

func show_dyn(x__23 dyn__Display) string {
    var retv156 string
    var t157 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv156 = t157
    return retv156
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv159 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t160 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv159 = t160
    return retv159
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv162 func(dyn__Display) string
    var t163 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv162 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t163, p0)
    }
    return retv162
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv165 string
    x__31.vtable.tick(x__31.data)
    var t166 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t167 string = t166 + ":"
    var t168 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t168)
    var t170 string = t167 + t169
    retv165 = t170
    return retv165
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
    var t172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t172,
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
    var t173 string = render_star__43(dp1__38)
    var t174 string = t173 + "|"
    var t175 string = render_angle__44(df1__40)
    var s2__47 string = t174 + t175
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp177 int32
    switch vlen__49 {
    case 2:
        jp177 = 3
    default:
        jp177 = 5
    }
    var delta__50 int32 = jp177
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop183:
    for {
        var t184 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t185 bool = t184 < 3
        if t185 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t186 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t187 int32 = t186 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t187)
            continue
        } else {
            break Loop_loop183
        }
    }
    var t179 string = _goml_m_inherent_i_int_i_int_i_to__string(vlen__49)
    var t180 string = "len:" + t179
    println__T_string(t180)
    var t181 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t182 string = "delta:" + t181
    println__T_string(t182)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv189 string
    var t190 string = _goml_runtime_core_int32_to_string(self__6)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv192 int32
    var t193 int32 = ref_get__Ref_5int32(self__210)
    retv192 = t193
    return retv192
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv197 *ref_int32_x
    var t198 *ref_int32_x = ref__Ref_5int32(value__209)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv200 *_goml_vec_Dyn_Display
    var t201 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__128 *_goml_vec_Dyn_Display, elem__129 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__139 *_goml_vec_Dyn_Display) int {
    var retv205 int
    var t206 int = vec_len__Vec_11Dyn_Display(self__139)
    retv205 = t206
    return retv205
}

func println__T_string(value__1 string) struct{} {
    var t208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t208)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv211 string
    var t212 string = _goml_runtime_core_int_to_string(self__5)
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv214 string
    retv214 = self__38
    return retv214
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env78 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv222 string
    var t223 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env79 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv225 string
    var tag__29 string = env79.tag_0
    var t226 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv225 = t226
    return retv225
}

func main() {
    main0()
}
