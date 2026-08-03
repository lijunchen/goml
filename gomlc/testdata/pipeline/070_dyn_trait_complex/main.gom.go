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
    var t195 int32 = self__0.x
    var t196 string
    var inline343 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline343
    var t197 string = "Point(" + t196
    var t198 string = t197 + ","
    var t199 int32 = self__0.y
    var t200 string
    var inline341 string = _goml_runtime_core_int32_to_string(t199)
    t200 = inline341
    var t201 string = t198 + t200
    var t202 string = t201 + ")"
    return t202
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t205 string = prefix__2 + "Point("
    var t206 int32 = self__1.x
    var t207 string
    var inline347 string = _goml_runtime_core_int32_to_string(t206)
    t207 = inline347
    var t208 string = t205 + t207
    var t209 string = t208 + ","
    var t210 int32 = self__1.y
    var t211 string
    var inline345 string = _goml_runtime_core_int32_to_string(t210)
    t211 = inline345
    var t212 string = t209 + t211
    var t213 string = t212 + ")"
    var t214 string = t213 + suffix__3
    return t214
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t218 int32 = self__5.x
    var t219 int32 = self__5.y
    var t220 int32 = t218 + t219
    var t221 int32 = t220 + delta__6
    return t221
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t226 bool = self__7.value
    if t226 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t231 bool = self__8.value
    if t231 {
        var t232 string = prefix__9 + "Flag(true)"
        var t233 string = t232 + suffix__10
        return t233
    } else {
        var t234 string = prefix__9 + "Flag(false)"
        var t235 string = t234 + suffix__10
        return t235
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t241 bool = self__12.value
    if t241 {
        return delta__13
    } else {
        var t242 int32 = -delta__13
        return t242
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t245 *ref_int32_x = self__14.cell
    var t246 int32
    var inline351 int32 = ref_get__Ref_5int32(t245)
    t246 = inline351
    var t247 string
    var inline349 string = _goml_runtime_core_int32_to_string(t246)
    t247 = inline349
    var t248 string = "Counter(" + t247
    var t249 string = t248 + ")"
    return t249
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t252 string = prefix__16 + "Counter("
    var t253 *ref_int32_x = self__15.cell
    var t254 int32
    var inline355 int32 = ref_get__Ref_5int32(t253)
    t254 = inline355
    var t255 string
    var inline353 string = _goml_runtime_core_int32_to_string(t254)
    t255 = inline353
    var t256 string = t252 + t255
    var t257 string = t256 + ")"
    var t258 string = t257 + suffix__17
    return t258
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t260 *ref_int32_x = self__18.cell
    var t261 int32
    var inline359 int32 = ref_get__Ref_5int32(t260)
    t261 = inline359
    var next__19 int32 = t261 + 1
    var t262 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t262, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t265 *ref_int32_x = self__20.cell
    var t266 int32
    var inline363 int32 = ref_get__Ref_5int32(t265)
    t266 = inline363
    var next__22 int32 = t266 + delta__21
    var t267 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t267, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t270 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t270
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var inline365 string = x__24.vtable.show_with(x__24.data, tag__25, tag__25)
    return inline365
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv275 func(dyn__Display) string
    var t276 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv275 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t276, p0)
    }
    return retv275
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
    var t285 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t285,
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
    var t286 string = render_star__43(dp1__38)
    var t287 string = t286 + "|"
    var t288 string = render_angle__44(df1__40)
    var s2__47 string = t287 + t288
    var v__48 *_goml_vec_Dyn_Display
    var inline415 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline415
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline407 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline407
    var jp290 int32
    switch vlen__49 {
    case 2:
        jp290 = 3
    default:
        jp290 = 5
    }
    var inline404 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline404)
    var inline401 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline401)
    var inline398 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline398)
    var i__51 *ref_int32_x
    var inline395 int32 = 0
    var inline396 *ref_int32_x = ref__Ref_5int32(inline395)
    i__51 = inline396
    Loop_loop296:
    for {
        var t297 int32
        var inline383 int32 = ref_get__Ref_5int32(i__51)
        t297 = inline383
        var t298 bool = t297 < 3
        if t298 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline377 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline378 string = inline377 + ":"
            var inline379 int32 = dc__42.vtable.bump(dc__42.data, jp290)
            var inline380 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline379)
            var inline381 string = inline378 + inline380
            line__52 = inline381
            var inline373 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline373)
            var t299 int32
            var inline371 int32 = ref_get__Ref_5int32(i__51)
            t299 = inline371
            var t300 int32 = t299 + 1
            ref_set__Ref_5int32(i__51, t300)
            continue
        } else {
            break Loop_loop296
        }
    }
    var t292 string
    var inline393 string = _goml_runtime_core_int_to_string(vlen__49)
    t292 = inline393
    var t293 string = "len:" + t292
    var inline390 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t293)
    _goml_runtime_core_string_println(inline390)
    var t294 string
    var inline388 string = _goml_runtime_core_int32_to_string(jp290)
    t294 = inline388
    var t295 string = "delta:" + t294
    var inline385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t295)
    _goml_runtime_core_string_println(inline385)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t303 string = _goml_runtime_core_int32_to_string(self__35)
    return t303
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t311 *ref_int32_x = ref__Ref_5int32(value__236)
    return t311
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env192 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env192.tag_0
    var t339 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t339
}

func main() {
    main0()
}
