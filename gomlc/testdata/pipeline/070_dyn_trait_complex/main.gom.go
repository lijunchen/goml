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
    var t205 int32 = self__0.x
    var t206 string
    var inline355 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline355
    var t207 string = "Point(" + t206
    var t208 string = t207 + ","
    var t209 int32 = self__0.y
    var t210 string
    var inline353 string = _goml_runtime_core_int32_to_string(t209)
    t210 = inline353
    var t211 string = t208 + t210
    var t212 string = t211 + ")"
    return t212
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t215 string = prefix__2 + "Point("
    var t216 int32 = self__1.x
    var t217 string
    var inline359 string = _goml_runtime_core_int32_to_string(t216)
    t217 = inline359
    var t218 string = t215 + t217
    var t219 string = t218 + ","
    var t220 int32 = self__1.y
    var t221 string
    var inline357 string = _goml_runtime_core_int32_to_string(t220)
    t221 = inline357
    var t222 string = t219 + t221
    var t223 string = t222 + ")"
    var t224 string = t223 + suffix__3
    return t224
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t228 int32 = self__5.x
    var t229 int32 = self__5.y
    var t230 int32 = t228 + t229
    var t231 int32 = t230 + delta__6
    return t231
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t236 bool = self__7.value
    if t236 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t241 bool = self__8.value
    if t241 {
        var t242 string = prefix__9 + "Flag(true)"
        var t243 string = t242 + suffix__10
        return t243
    } else {
        var t244 string = prefix__9 + "Flag(false)"
        var t245 string = t244 + suffix__10
        return t245
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t251 bool = self__12.value
    if t251 {
        return delta__13
    } else {
        var t252 int32 = -delta__13
        return t252
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t255 *ref_int32_x = self__14.cell
    var t256 int32
    var inline363 int32 = ref_get__Ref_5int32(t255)
    t256 = inline363
    var t257 string
    var inline361 string = _goml_runtime_core_int32_to_string(t256)
    t257 = inline361
    var t258 string = "Counter(" + t257
    var t259 string = t258 + ")"
    return t259
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t262 string = prefix__16 + "Counter("
    var t263 *ref_int32_x = self__15.cell
    var t264 int32
    var inline367 int32 = ref_get__Ref_5int32(t263)
    t264 = inline367
    var t265 string
    var inline365 string = _goml_runtime_core_int32_to_string(t264)
    t265 = inline365
    var t266 string = t262 + t265
    var t267 string = t266 + ")"
    var t268 string = t267 + suffix__17
    return t268
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t270 *ref_int32_x = self__18.cell
    var t271 int32
    var inline371 int32 = ref_get__Ref_5int32(t270)
    t271 = inline371
    var next__19 int32 = t271 + 1
    var t272 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t272, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t275 *ref_int32_x = self__20.cell
    var t276 int32
    var inline375 int32 = ref_get__Ref_5int32(t275)
    t276 = inline375
    var next__22 int32 = t276 + delta__21
    var t277 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t277, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t280 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t280
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t283 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t283, p0, p1)
    }
    var t284 string = f__28(x__24, tag__25)
    return t284
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t287 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t288 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t287, p0)
    }
    return t288
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
    var t297 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t297,
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
    var t298 string = render_star__43(dp1__38)
    var t299 string = t298 + "|"
    var t300 string = render_angle__44(df1__40)
    var s2__47 string = t299 + t300
    var v__48 *_goml_vec_Dyn_Display
    var inline425 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline425
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline417 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline417
    var jp302 int32
    switch vlen__49 {
    case 2:
        jp302 = 3
    default:
        jp302 = 5
    }
    var inline414 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline414)
    var inline411 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline411)
    var inline408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline408)
    var i__51 *ref_int32_x
    var inline405 int32 = 0
    var inline406 *ref_int32_x = ref__Ref_5int32(inline405)
    i__51 = inline406
    Loop_loop308:
    for {
        var t309 int32
        var inline393 int32 = ref_get__Ref_5int32(i__51)
        t309 = inline393
        var t310 bool = t309 < 3
        if t310 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline387 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline388 string = inline387 + ":"
            var inline389 int32 = dc__42.vtable.bump(dc__42.data, jp302)
            var inline390 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline389)
            var inline391 string = inline388 + inline390
            line__52 = inline391
            var inline383 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline383)
            var t311 int32
            var inline381 int32 = ref_get__Ref_5int32(i__51)
            t311 = inline381
            var t312 int32 = t311 + 1
            ref_set__Ref_5int32(i__51, t312)
            continue
        } else {
            break Loop_loop308
        }
    }
    var t304 string
    var inline403 string = _goml_runtime_core_int_to_string(vlen__49)
    t304 = inline403
    var t305 string = "len:" + t304
    var inline400 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t305)
    _goml_runtime_core_string_println(inline400)
    var t306 string
    var inline398 string = _goml_runtime_core_int32_to_string(jp302)
    t306 = inline398
    var t307 string = "delta:" + t306
    var inline395 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t307)
    _goml_runtime_core_string_println(inline395)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t315 string = _goml_runtime_core_int32_to_string(self__33)
    return t315
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__273 int32) *ref_int32_x {
    var t323 *ref_int32_x = ref__Ref_5int32(value__273)
    return t323
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env201 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t348 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t348
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env202 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env202.tag_0
    var t351 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t351
}

func main() {
    main0()
}
