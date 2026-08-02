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
    var t173 int32 = self__0.x
    var t174 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t173)
    var t175 string = "Point(" + t174
    var t176 string = t175 + ","
    var t177 int32 = self__0.y
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t177)
    var t179 string = t176 + t178
    var t180 string = t179 + ")"
    return t180
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t183 string = prefix__2 + "Point("
    var t184 int32 = self__1.x
    var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t184)
    var t186 string = t183 + t185
    var t187 string = t186 + ","
    var t188 int32 = self__1.y
    var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t188)
    var t190 string = t187 + t189
    var t191 string = t190 + ")"
    var t192 string = t191 + suffix__3
    return t192
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t196 int32 = self__5.x
    var t197 int32 = self__5.y
    var t198 int32 = t196 + t197
    var t199 int32 = t198 + delta__6
    return t199
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t204 bool = self__7.value
    if t204 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t209 bool = self__8.value
    if t209 {
        var t210 string = prefix__9 + "Flag(true)"
        var t211 string = t210 + suffix__10
        return t211
    } else {
        var t212 string = prefix__9 + "Flag(false)"
        var t213 string = t212 + suffix__10
        return t213
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t219 bool = self__12.value
    if t219 {
        return delta__13
    } else {
        var t220 int32 = -delta__13
        return t220
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t223 *ref_int32_x = self__14.cell
    var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t223)
    var t225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t224)
    var t226 string = "Counter(" + t225
    var t227 string = t226 + ")"
    return t227
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t230 string = prefix__16 + "Counter("
    var t231 *ref_int32_x = self__15.cell
    var t232 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t231)
    var t233 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t232)
    var t234 string = t230 + t233
    var t235 string = t234 + ")"
    var t236 string = t235 + suffix__17
    return t236
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t238 *ref_int32_x = self__18.cell
    var t239 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t238)
    var next__19 int32 = t239 + 1
    var t240 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t240, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t243 *ref_int32_x = self__20.cell
    var t244 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t243)
    var next__22 int32 = t244 + delta__21
    var t245 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t245, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t248 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t248
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t251 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    return t251
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv253 func(dyn__Display) string
    var t254 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv253 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t254, p0)
    }
    return retv253
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    x__31.vtable.tick(x__31.data)
    var t257 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t258 string = t257 + ":"
    var t259 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t260 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t259)
    var t261 string = t258 + t260
    return t261
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
    var t263 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t263,
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
    var t264 string = render_star__43(dp1__38)
    var t265 string = t264 + "|"
    var t266 string = render_angle__44(df1__40)
    var s2__47 string = t265 + t266
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp268 int32
    switch vlen__49 {
    case 2:
        jp268 = 3
    default:
        jp268 = 5
    }
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop274:
    for {
        var t275 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t276 bool = t275 < 3
        if t276 {
            var line__52 string = bump_and_show(dc__42, jp268)
            println__T_string(line__52)
            var t277 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t278 int32 = t277 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t278)
            continue
        } else {
            break Loop_loop274
        }
    }
    var t270 string = _goml_m_inherent_i_int_i_int_i_to__string(vlen__49)
    var t271 string = "len:" + t270
    println__T_string(t271)
    var t272 string = _goml_m_inherent_i_int32_i_int32_i_to__string(jp268)
    var t273 string = "delta:" + t272
    println__T_string(t273)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t281 string = _goml_runtime_core_int32_to_string(self__6)
    return t281
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t284 int32 = ref_get__Ref_5int32(self__208)
    return t284
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t289 *ref_int32_x = ref__Ref_5int32(value__207)
    return t289
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var t292 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    return t292
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__137 *_goml_vec_Dyn_Display) int {
    var t297 int = vec_len__Vec_11Dyn_Display(self__137)
    return t297
}

func println__T_string(value__1 string) struct{} {
    var t299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t299)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t303 string = _goml_runtime_core_int_to_string(self__5)
    return t303
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env169 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t314 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t314
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env170 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env170.tag_0
    var t317 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t317
}

func main() {
    main0()
}
