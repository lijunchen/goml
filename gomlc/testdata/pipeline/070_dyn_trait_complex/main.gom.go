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
    var retv169 string
    var t170 int32 = self__0.x
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t170)
    var t172 string = "Point(" + t171
    var t173 string = t172 + ","
    var t174 int32 = self__0.y
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t174)
    var t176 string = t173 + t175
    var t177 string = t176 + ")"
    retv169 = t177
    return retv169
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv179 string
    var t180 string = prefix__2 + "Point("
    var t181 int32 = self__1.x
    var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t181)
    var t183 string = t180 + t182
    var t184 string = t183 + ","
    var t185 int32 = self__1.y
    var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t185)
    var t187 string = t184 + t186
    var t188 string = t187 + ")"
    var t189 string = t188 + suffix__3
    retv179 = t189
    return retv179
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv192 int32
    var t193 int32 = self__5.x
    var t194 int32 = self__5.y
    var t195 int32 = t193 + t194
    var t196 int32 = t195 + delta__6
    retv192 = t196
    return retv192
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv198 string
    var t201 bool = self__7.value
    var jp200 string
    if t201 {
        jp200 = "Flag(true)"
    } else {
        jp200 = "Flag(false)"
    }
    retv198 = jp200
    return retv198
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv203 string
    var t206 bool = self__8.value
    var jp205 string
    if t206 {
        var t207 string = prefix__9 + "Flag(true)"
        var t208 string = t207 + suffix__10
        jp205 = t208
    } else {
        var t209 string = prefix__9 + "Flag(false)"
        var t210 string = t209 + suffix__10
        jp205 = t210
    }
    retv203 = jp205
    return retv203
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv213 int32
    var t216 bool = self__12.value
    var jp215 int32
    if t216 {
        jp215 = delta__13
    } else {
        var t217 int32 = -delta__13
        jp215 = t217
    }
    retv213 = jp215
    return retv213
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv219 string
    var t220 *ref_int32_x = self__14.cell
    var t221 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t220)
    var t222 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t221)
    var t223 string = "Counter(" + t222
    var t224 string = t223 + ")"
    retv219 = t224
    return retv219
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv226 string
    var t227 string = prefix__16 + "Counter("
    var t228 *ref_int32_x = self__15.cell
    var t229 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t228)
    var t230 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t229)
    var t231 string = t227 + t230
    var t232 string = t231 + ")"
    var t233 string = t232 + suffix__17
    retv226 = t233
    return retv226
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t235 *ref_int32_x = self__18.cell
    var t236 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t235)
    var next__19 int32 = t236 + 1
    var t237 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t237, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv239 int32
    var t240 *ref_int32_x = self__20.cell
    var t241 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t240)
    var next__22 int32 = t241 + delta__21
    var t242 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t242, next__22)
    retv239 = next__22
    return retv239
}

func show_dyn(x__23 dyn__Display) string {
    var retv244 string
    var t245 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv244 = t245
    return retv244
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv247 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t248 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv247 = t248
    return retv247
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv250 func(dyn__Display) string
    var t251 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv250 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t251, p0)
    }
    return retv250
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv253 string
    x__31.vtable.tick(x__31.data)
    var t254 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t255 string = t254 + ":"
    var t256 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t256)
    var t258 string = t255 + t257
    retv253 = t258
    return retv253
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
    var t260 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t260,
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
    var t261 string = render_star__43(dp1__38)
    var t262 string = t261 + "|"
    var t263 string = render_angle__44(df1__40)
    var s2__47 string = t262 + t263
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp265 int32
    switch vlen__49 {
    case 2:
        jp265 = 3
    default:
        jp265 = 5
    }
    var delta__50 int32 = jp265
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop271:
    for {
        var t272 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t273 bool = t272 < 3
        if t273 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t274 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t275 int32 = t274 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t275)
            continue
        } else {
            break Loop_loop271
        }
    }
    var t267 string = _goml_m_inherent_i_int_i_int_i_to__string(vlen__49)
    var t268 string = "len:" + t267
    println__T_string(t268)
    var t269 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t270 string = "delta:" + t269
    println__T_string(t270)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv277 string
    var t278 string = _goml_runtime_core_int32_to_string(self__6)
    retv277 = t278
    return retv277
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv280 int32
    var t281 int32 = ref_get__Ref_5int32(self__208)
    retv280 = t281
    return retv280
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv285 *ref_int32_x
    var t286 *ref_int32_x = ref__Ref_5int32(value__207)
    retv285 = t286
    return retv285
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv288 *_goml_vec_Dyn_Display
    var t289 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv288 = t289
    return retv288
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__126 *_goml_vec_Dyn_Display, elem__127 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__137 *_goml_vec_Dyn_Display) int {
    var retv293 int
    var t294 int = vec_len__Vec_11Dyn_Display(self__137)
    retv293 = t294
    return retv293
}

func println__T_string(value__1 string) struct{} {
    var t296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t296)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv299 string
    var t300 string = _goml_runtime_core_int_to_string(self__5)
    retv299 = t300
    return retv299
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv302 string
    retv302 = self__38
    return retv302
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env166 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv310 string
    var t311 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv310 = t311
    return retv310
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env167 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv313 string
    var tag__29 string = env167.tag_0
    var t314 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv313 = t314
    return retv313
}

func main() {
    main0()
}
