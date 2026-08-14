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
    var t200 int32 = self__0.x
    var t201 string
    var inline350 string = _goml_runtime_core_int32_to_string(t200)
    t201 = inline350
    var t202 string = "Point(" + t201
    var t203 string = t202 + ","
    var t204 int32 = self__0.y
    var t205 string
    var inline348 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline348
    var t206 string = t203 + t205
    var t207 string = t206 + ")"
    return t207
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t210 string = prefix__2 + "Point("
    var t211 int32 = self__1.x
    var t212 string
    var inline354 string = _goml_runtime_core_int32_to_string(t211)
    t212 = inline354
    var t213 string = t210 + t212
    var t214 string = t213 + ","
    var t215 int32 = self__1.y
    var t216 string
    var inline352 string = _goml_runtime_core_int32_to_string(t215)
    t216 = inline352
    var t217 string = t214 + t216
    var t218 string = t217 + ")"
    var t219 string = t218 + suffix__3
    return t219
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t223 int32 = self__5.x
    var t224 int32 = self__5.y
    var t225 int32 = t223 + t224
    var t226 int32 = t225 + delta__6
    return t226
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t231 bool = self__7.value
    if t231 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t236 bool = self__8.value
    if t236 {
        var t237 string = prefix__9 + "Flag(true)"
        var t238 string = t237 + suffix__10
        return t238
    } else {
        var t239 string = prefix__9 + "Flag(false)"
        var t240 string = t239 + suffix__10
        return t240
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t246 bool = self__12.value
    if t246 {
        return delta__13
    } else {
        var t247 int32 = -delta__13
        return t247
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t250 *ref_int32_x = self__14.cell
    var t251 int32
    var inline358 int32 = ref_get__Ref_5int32(t250)
    t251 = inline358
    var t252 string
    var inline356 string = _goml_runtime_core_int32_to_string(t251)
    t252 = inline356
    var t253 string = "Counter(" + t252
    var t254 string = t253 + ")"
    return t254
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t257 string = prefix__16 + "Counter("
    var t258 *ref_int32_x = self__15.cell
    var t259 int32
    var inline362 int32 = ref_get__Ref_5int32(t258)
    t259 = inline362
    var t260 string
    var inline360 string = _goml_runtime_core_int32_to_string(t259)
    t260 = inline360
    var t261 string = t257 + t260
    var t262 string = t261 + ")"
    var t263 string = t262 + suffix__17
    return t263
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t265 *ref_int32_x = self__18.cell
    var t266 int32
    var inline366 int32 = ref_get__Ref_5int32(t265)
    t266 = inline366
    var next__19 int32 = t266 + 1
    var t267 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t267, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t270 *ref_int32_x = self__20.cell
    var t271 int32
    var inline370 int32 = ref_get__Ref_5int32(t270)
    t271 = inline370
    var next__22 int32 = t271 + delta__21
    var t272 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t272, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t275 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t275
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t278 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t278, p0, p1)
    }
    var t279 string = f__28(x__24, tag__25)
    return t279
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t282 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t283 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t282, p0)
    }
    return t283
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
    var t292 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t292,
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
    var t293 string = render_star__43(dp1__38)
    var t294 string = t293 + "|"
    var t295 string = render_angle__44(df1__40)
    var s2__47 string = t294 + t295
    var v__48 *_goml_vec_Dyn_Display
    var inline420 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline420
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline412 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline412
    var jp297 int32
    switch vlen__49 {
    case 2:
        jp297 = 3
    default:
        jp297 = 5
    }
    var inline409 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline409)
    var inline406 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline406)
    var inline403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline403)
    var i__51 *ref_int32_x
    var inline400 int32 = 0
    var inline401 *ref_int32_x = ref__Ref_5int32(inline400)
    i__51 = inline401
    Loop_loop303:
    for {
        var t304 int32
        var inline388 int32 = ref_get__Ref_5int32(i__51)
        t304 = inline388
        var t305 bool = t304 < 3
        if t305 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline382 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline383 string = inline382 + ":"
            var inline384 int32 = dc__42.vtable.bump(dc__42.data, jp297)
            var inline385 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline384)
            var inline386 string = inline383 + inline385
            line__52 = inline386
            var inline378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline378)
            var t306 int32
            var inline376 int32 = ref_get__Ref_5int32(i__51)
            t306 = inline376
            var t307 int32 = t306 + 1
            ref_set__Ref_5int32(i__51, t307)
            continue
        } else {
            break Loop_loop303
        }
    }
    var t299 string
    var inline398 string = _goml_runtime_core_int_to_string(vlen__49)
    t299 = inline398
    var t300 string = "len:" + t299
    var inline395 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t300)
    _goml_runtime_core_string_println(inline395)
    var t301 string
    var inline393 string = _goml_runtime_core_int32_to_string(jp297)
    t301 = inline393
    var t302 string = "delta:" + t301
    var inline390 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t302)
    _goml_runtime_core_string_println(inline390)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t310 string = _goml_runtime_core_int32_to_string(self__33)
    return t310
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__270 int32) *ref_int32_x {
    var t318 *ref_int32_x = ref__Ref_5int32(value__270)
    return t318
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env196 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t343 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t343
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env197 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env197.tag_0
    var t346 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t346
}

func main() {
    main0()
}
