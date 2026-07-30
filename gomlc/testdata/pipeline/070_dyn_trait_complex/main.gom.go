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
    var retv125 string
    var t126 int32 = self__0.x
    var t127 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t126)
    var t128 string = "Point(" + t127
    var t129 string = t128 + ","
    var t130 int32 = self__0.y
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    var t132 string = t129 + t131
    var t133 string = t132 + ")"
    retv125 = t133
    return retv125
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv135 string
    var t136 string = prefix__2 + "Point("
    var t137 int32 = self__1.x
    var t138 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t137)
    var t139 string = t136 + t138
    var t140 string = t139 + ","
    var t141 int32 = self__1.y
    var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t141)
    var t143 string = t140 + t142
    var t144 string = t143 + ")"
    var t145 string = t144 + suffix__3
    retv135 = t145
    return retv135
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv148 int32
    var t149 int32 = self__5.x
    var t150 int32 = self__5.y
    var t151 int32 = t149 + t150
    var t152 int32 = t151 + delta__6
    retv148 = t152
    return retv148
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv154 string
    var t157 bool = self__7.value
    var jp156 string
    if t157 {
        jp156 = "Flag(true)"
    } else {
        jp156 = "Flag(false)"
    }
    retv154 = jp156
    return retv154
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv159 string
    var t162 bool = self__8.value
    var jp161 string
    if t162 {
        var t163 string = prefix__9 + "Flag(true)"
        var t164 string = t163 + suffix__10
        jp161 = t164
    } else {
        var t165 string = prefix__9 + "Flag(false)"
        var t166 string = t165 + suffix__10
        jp161 = t166
    }
    retv159 = jp161
    return retv159
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv169 int32
    var t172 bool = self__12.value
    var jp171 int32
    if t172 {
        jp171 = delta__13
    } else {
        var t173 int32 = -delta__13
        jp171 = t173
    }
    retv169 = jp171
    return retv169
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv175 string
    var t176 *ref_int32_x = self__14.cell
    var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t176)
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t177)
    var t179 string = "Counter(" + t178
    var t180 string = t179 + ")"
    retv175 = t180
    return retv175
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv182 string
    var t183 string = prefix__16 + "Counter("
    var t184 *ref_int32_x = self__15.cell
    var t185 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t184)
    var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t185)
    var t187 string = t183 + t186
    var t188 string = t187 + ")"
    var t189 string = t188 + suffix__17
    retv182 = t189
    return retv182
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t191 *ref_int32_x = self__18.cell
    var t192 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t191)
    var next__19 int32 = t192 + 1
    var t193 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t193, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv195 int32
    var t196 *ref_int32_x = self__20.cell
    var t197 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t196)
    var next__22 int32 = t197 + delta__21
    var t198 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t198, next__22)
    retv195 = next__22
    return retv195
}

func show_dyn(x__23 dyn__Display) string {
    var retv200 string
    var t201 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv200 = t201
    return retv200
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv203 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t204 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv203 = t204
    return retv203
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv206 func(dyn__Display) string
    var t207 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv206 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t207, p0)
    }
    return retv206
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv209 string
    x__31.vtable.tick(x__31.data)
    var t210 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t211 string = t210 + ":"
    var t212 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t213 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t212)
    var t214 string = t211 + t213
    retv209 = t214
    return retv209
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
    var t216 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t216,
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
    var t217 string = render_star__43(dp1__38)
    var t218 string = t217 + "|"
    var t219 string = render_angle__44(df1__40)
    var s2__47 string = t218 + t219
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp221 int32
    switch vlen__49 {
    case 2:
        jp221 = 3
    default:
        jp221 = 5
    }
    var delta__50 int32 = jp221
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop227:
    for {
        var t228 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t229 bool = t228 < 3
        if t229 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t230 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t231 int32 = t230 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t231)
            continue
        } else {
            break Loop_loop227
        }
    }
    var t223 string = _goml_m_inherent_i_int_i_int_i_to__string(vlen__49)
    var t224 string = "len:" + t223
    println__T_string(t224)
    var t225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t226 string = "delta:" + t225
    println__T_string(t226)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv233 string
    var t234 string = _goml_runtime_core_int32_to_string(self__6)
    retv233 = t234
    return retv233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv236 int32
    var t237 int32 = ref_get__Ref_5int32(self__208)
    retv236 = t237
    return retv236
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv241 *ref_int32_x
    var t242 *ref_int32_x = ref__Ref_5int32(value__207)
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv244 *_goml_vec_Dyn_Display
    var t245 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv244 = t245
    return retv244
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__137 *_goml_vec_Dyn_Display) int {
    var retv249 int
    var t250 int = vec_len__Vec_11Dyn_Display(self__137)
    retv249 = t250
    return retv249
}

func println__T_string(value__1 string) struct{} {
    var t252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t252)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv255 string
    var t256 string = _goml_runtime_core_int_to_string(self__5)
    retv255 = t256
    return retv255
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv258 string
    retv258 = self__38
    return retv258
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env122 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv266 string
    var t267 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env123 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv269 string
    var tag__29 string = env123.tag_0
    var t270 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv269 = t270
    return retv269
}

func main() {
    main0()
}
