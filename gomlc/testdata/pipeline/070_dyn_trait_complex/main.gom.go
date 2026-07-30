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
    var retv85 string
    var t86 int32 = self__0.x
    var t87 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t86)
    var t88 string = "Point(" + t87
    var t89 string = t88 + ","
    var t90 int32 = self__0.y
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    var t92 string = t89 + t91
    var t93 string = t92 + ")"
    retv85 = t93
    return retv85
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv95 string
    var t96 string = prefix__2 + "Point("
    var t97 int32 = self__1.x
    var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t97)
    var t99 string = t96 + t98
    var t100 string = t99 + ","
    var t101 int32 = self__1.y
    var t102 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t101)
    var t103 string = t100 + t102
    var t104 string = t103 + ")"
    var t105 string = t104 + suffix__3
    retv95 = t105
    return retv95
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv108 int32
    var t109 int32 = self__5.x
    var t110 int32 = self__5.y
    var t111 int32 = t109 + t110
    var t112 int32 = t111 + delta__6
    retv108 = t112
    return retv108
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv114 string
    var t117 bool = self__7.value
    var jp116 string
    if t117 {
        jp116 = "Flag(true)"
    } else {
        jp116 = "Flag(false)"
    }
    retv114 = jp116
    return retv114
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv119 string
    var t122 bool = self__8.value
    var jp121 string
    if t122 {
        var t123 string = prefix__9 + "Flag(true)"
        var t124 string = t123 + suffix__10
        jp121 = t124
    } else {
        var t125 string = prefix__9 + "Flag(false)"
        var t126 string = t125 + suffix__10
        jp121 = t126
    }
    retv119 = jp121
    return retv119
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv129 int32
    var t132 bool = self__12.value
    var jp131 int32
    if t132 {
        jp131 = delta__13
    } else {
        var t133 int32 = -delta__13
        jp131 = t133
    }
    retv129 = jp131
    return retv129
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv135 string
    var t136 *ref_int32_x = self__14.cell
    var t137 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t136)
    var t138 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t137)
    var t139 string = "Counter(" + t138
    var t140 string = t139 + ")"
    retv135 = t140
    return retv135
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv142 string
    var t143 string = prefix__16 + "Counter("
    var t144 *ref_int32_x = self__15.cell
    var t145 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t144)
    var t146 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t145)
    var t147 string = t143 + t146
    var t148 string = t147 + ")"
    var t149 string = t148 + suffix__17
    retv142 = t149
    return retv142
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t151 *ref_int32_x = self__18.cell
    var t152 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t151)
    var next__19 int32 = t152 + 1
    var t153 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t153, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv155 int32
    var t156 *ref_int32_x = self__20.cell
    var t157 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t156)
    var next__22 int32 = t157 + delta__21
    var t158 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t158, next__22)
    retv155 = next__22
    return retv155
}

func show_dyn(x__23 dyn__Display) string {
    var retv160 string
    var t161 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv160 = t161
    return retv160
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv163 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t164 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv163 = t164
    return retv163
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv166 func(dyn__Display) string
    var t167 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv166 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t167, p0)
    }
    return retv166
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv169 string
    x__31.vtable.tick(x__31.data)
    var t170 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t171 string = t170 + ":"
    var t172 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t172)
    var t174 string = t171 + t173
    retv169 = t174
    return retv169
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
    var t176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t176,
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
    var t177 string = render_star__43(dp1__38)
    var t178 string = t177 + "|"
    var t179 string = render_angle__44(df1__40)
    var s2__47 string = t178 + t179
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp181 int32
    switch vlen__49 {
    case 2:
        jp181 = 3
    default:
        jp181 = 5
    }
    var delta__50 int32 = jp181
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop187:
    for {
        var t188 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t189 bool = t188 < 3
        if t189 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t190 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t191 int32 = t190 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t191)
            continue
        } else {
            break Loop_loop187
        }
    }
    var t183 string = _goml_m_inherent_i_int_i_int_i_to__string(vlen__49)
    var t184 string = "len:" + t183
    println__T_string(t184)
    var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t186 string = "delta:" + t185
    println__T_string(t186)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv193 string
    var t194 string = _goml_runtime_core_int32_to_string(self__6)
    retv193 = t194
    return retv193
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv196 int32
    var t197 int32 = ref_get__Ref_5int32(self__208)
    retv196 = t197
    return retv196
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv201 *ref_int32_x
    var t202 *ref_int32_x = ref__Ref_5int32(value__207)
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv204 *_goml_vec_Dyn_Display
    var t205 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv204 = t205
    return retv204
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__137 *_goml_vec_Dyn_Display) int {
    var retv209 int
    var t210 int = vec_len__Vec_11Dyn_Display(self__137)
    retv209 = t210
    return retv209
}

func println__T_string(value__1 string) struct{} {
    var t212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv215 string
    var t216 string = _goml_runtime_core_int_to_string(self__5)
    retv215 = t216
    return retv215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv218 string
    retv218 = self__38
    return retv218
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env82 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv226 string
    var t227 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env83 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv229 string
    var tag__29 string = env83.tag_0
    var t230 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv229 = t230
    return retv229
}

func main() {
    main0()
}
