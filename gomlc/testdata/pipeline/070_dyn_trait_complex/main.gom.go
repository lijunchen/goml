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
    var t190 int32 = self__0.x
    var t191 string
    var inline340 string = _goml_runtime_core_int32_to_string(t190)
    t191 = inline340
    var t192 string = "Point(" + t191
    var t193 string = t192 + ","
    var t194 int32 = self__0.y
    var t195 string
    var inline338 string = _goml_runtime_core_int32_to_string(t194)
    t195 = inline338
    var t196 string = t193 + t195
    var t197 string = t196 + ")"
    return t197
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t200 string = prefix__2 + "Point("
    var t201 int32 = self__1.x
    var t202 string
    var inline344 string = _goml_runtime_core_int32_to_string(t201)
    t202 = inline344
    var t203 string = t200 + t202
    var t204 string = t203 + ","
    var t205 int32 = self__1.y
    var t206 string
    var inline342 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline342
    var t207 string = t204 + t206
    var t208 string = t207 + ")"
    var t209 string = t208 + suffix__3
    return t209
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t213 int32 = self__5.x
    var t214 int32 = self__5.y
    var t215 int32 = t213 + t214
    var t216 int32 = t215 + delta__6
    return t216
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t221 bool = self__7.value
    if t221 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t226 bool = self__8.value
    if t226 {
        var t227 string = prefix__9 + "Flag(true)"
        var t228 string = t227 + suffix__10
        return t228
    } else {
        var t229 string = prefix__9 + "Flag(false)"
        var t230 string = t229 + suffix__10
        return t230
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t236 bool = self__12.value
    if t236 {
        return delta__13
    } else {
        var t237 int32 = -delta__13
        return t237
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t240 *ref_int32_x = self__14.cell
    var t241 int32
    var inline348 int32 = ref_get__Ref_5int32(t240)
    t241 = inline348
    var t242 string
    var inline346 string = _goml_runtime_core_int32_to_string(t241)
    t242 = inline346
    var t243 string = "Counter(" + t242
    var t244 string = t243 + ")"
    return t244
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t247 string = prefix__16 + "Counter("
    var t248 *ref_int32_x = self__15.cell
    var t249 int32
    var inline352 int32 = ref_get__Ref_5int32(t248)
    t249 = inline352
    var t250 string
    var inline350 string = _goml_runtime_core_int32_to_string(t249)
    t250 = inline350
    var t251 string = t247 + t250
    var t252 string = t251 + ")"
    var t253 string = t252 + suffix__17
    return t253
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t255 *ref_int32_x = self__18.cell
    var t256 int32
    var inline356 int32 = ref_get__Ref_5int32(t255)
    t256 = inline356
    var next__19 int32 = t256 + 1
    var t257 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t257, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t260 *ref_int32_x = self__20.cell
    var t261 int32
    var inline360 int32 = ref_get__Ref_5int32(t260)
    t261 = inline360
    var next__22 int32 = t261 + delta__21
    var t262 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t262, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t265 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t265
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t268 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t268, p0, p1)
    }
    var t269 string = f__28(x__24, tag__25)
    return t269
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t272 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t273 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t272, p0)
    }
    return t273
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
    var t282 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t282,
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
    var t283 string = render_star__43(dp1__38)
    var t284 string = t283 + "|"
    var t285 string = render_angle__44(df1__40)
    var s2__47 string = t284 + t285
    var v__48 *_goml_vec_Dyn_Display
    var inline410 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline410
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline402 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline402
    var jp287 int32
    switch vlen__49 {
    case 2:
        jp287 = 3
    default:
        jp287 = 5
    }
    var inline399 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline399)
    var inline396 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline396)
    var inline393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline393)
    var i__51 *ref_int32_x
    var inline390 int32 = 0
    var inline391 *ref_int32_x = ref__Ref_5int32(inline390)
    i__51 = inline391
    Loop_loop293:
    for {
        var t294 int32
        var inline378 int32 = ref_get__Ref_5int32(i__51)
        t294 = inline378
        var t295 bool = t294 < 3
        if t295 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline372 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline373 string = inline372 + ":"
            var inline374 int32 = dc__42.vtable.bump(dc__42.data, jp287)
            var inline375 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline374)
            var inline376 string = inline373 + inline375
            line__52 = inline376
            var inline368 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline368)
            var t296 int32
            var inline366 int32 = ref_get__Ref_5int32(i__51)
            t296 = inline366
            var t297 int32 = t296 + 1
            ref_set__Ref_5int32(i__51, t297)
            continue
        } else {
            break Loop_loop293
        }
    }
    var t289 string
    var inline388 string = _goml_runtime_core_int_to_string(vlen__49)
    t289 = inline388
    var t290 string = "len:" + t289
    var inline385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t290)
    _goml_runtime_core_string_println(inline385)
    var t291 string
    var inline383 string = _goml_runtime_core_int32_to_string(jp287)
    t291 = inline383
    var t292 string = "delta:" + t291
    var inline380 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t292)
    _goml_runtime_core_string_println(inline380)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t300 string = _goml_runtime_core_int32_to_string(self__33)
    return t300
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__255 int32) *ref_int32_x {
    var t308 *ref_int32_x = ref__Ref_5int32(value__255)
    return t308
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env186 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t333 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t333
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env187 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env187.tag_0
    var t336 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t336
}

func main() {
    main0()
}
