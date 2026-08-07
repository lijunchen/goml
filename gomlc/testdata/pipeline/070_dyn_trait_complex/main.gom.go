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
    var t154 int32 = self__0.x
    var t155 string
    var inline302 string = _goml_runtime_core_int32_to_string(t154)
    t155 = inline302
    var t156 string = "Point(" + t155
    var t157 string = t156 + ","
    var t158 int32 = self__0.y
    var t159 string
    var inline300 string = _goml_runtime_core_int32_to_string(t158)
    t159 = inline300
    var t160 string = t157 + t159
    var t161 string = t160 + ")"
    return t161
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t164 string = prefix__2 + "Point("
    var t165 int32 = self__1.x
    var t166 string
    var inline306 string = _goml_runtime_core_int32_to_string(t165)
    t166 = inline306
    var t167 string = t164 + t166
    var t168 string = t167 + ","
    var t169 int32 = self__1.y
    var t170 string
    var inline304 string = _goml_runtime_core_int32_to_string(t169)
    t170 = inline304
    var t171 string = t168 + t170
    var t172 string = t171 + ")"
    var t173 string = t172 + suffix__3
    return t173
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t177 int32 = self__5.x
    var t178 int32 = self__5.y
    var t179 int32 = t177 + t178
    var t180 int32 = t179 + delta__6
    return t180
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t185 bool = self__7.value
    if t185 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t190 bool = self__8.value
    if t190 {
        var t191 string = prefix__9 + "Flag(true)"
        var t192 string = t191 + suffix__10
        return t192
    } else {
        var t193 string = prefix__9 + "Flag(false)"
        var t194 string = t193 + suffix__10
        return t194
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t200 bool = self__12.value
    if t200 {
        return delta__13
    } else {
        var t201 int32 = -delta__13
        return t201
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t204 *ref_int32_x = self__14.cell
    var t205 int32
    var inline310 int32 = ref_get__Ref_5int32(t204)
    t205 = inline310
    var t206 string
    var inline308 string = _goml_runtime_core_int32_to_string(t205)
    t206 = inline308
    var t207 string = "Counter(" + t206
    var t208 string = t207 + ")"
    return t208
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t211 string = prefix__16 + "Counter("
    var t212 *ref_int32_x = self__15.cell
    var t213 int32
    var inline314 int32 = ref_get__Ref_5int32(t212)
    t213 = inline314
    var t214 string
    var inline312 string = _goml_runtime_core_int32_to_string(t213)
    t214 = inline312
    var t215 string = t211 + t214
    var t216 string = t215 + ")"
    var t217 string = t216 + suffix__17
    return t217
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t219 *ref_int32_x = self__18.cell
    var t220 int32
    var inline318 int32 = ref_get__Ref_5int32(t219)
    t220 = inline318
    var next__19 int32 = t220 + 1
    var t221 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t221, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t224 *ref_int32_x = self__20.cell
    var t225 int32
    var inline322 int32 = ref_get__Ref_5int32(t224)
    t225 = inline322
    var next__22 int32 = t225 + delta__21
    var t226 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t226, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t229 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t229
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var inline324 string = x__24.vtable.show_with(x__24.data, tag__25, tag__25)
    return inline324
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv234 func(dyn__Display) string
    var t235 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv234 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t235, p0)
    }
    return retv234
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
    var t244 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t244,
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
    var t245 string = render_star__43(dp1__38)
    var t246 string = t245 + "|"
    var t247 string = render_angle__44(df1__40)
    var s2__47 string = t246 + t247
    var v__48 *_goml_vec_Dyn_Display
    var inline374 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline374
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline366 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline366
    var jp249 int32
    switch vlen__49 {
    case 2:
        jp249 = 3
    default:
        jp249 = 5
    }
    var inline363 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline363)
    var inline360 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline360)
    var inline357 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline357)
    var i__51 *ref_int32_x
    var inline354 int32 = 0
    var inline355 *ref_int32_x = ref__Ref_5int32(inline354)
    i__51 = inline355
    Loop_loop255:
    for {
        var t256 int32
        var inline342 int32 = ref_get__Ref_5int32(i__51)
        t256 = inline342
        var t257 bool = t256 < 3
        if t257 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline336 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline337 string = inline336 + ":"
            var inline338 int32 = dc__42.vtable.bump(dc__42.data, jp249)
            var inline339 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline338)
            var inline340 string = inline337 + inline339
            line__52 = inline340
            var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline332)
            var t258 int32
            var inline330 int32 = ref_get__Ref_5int32(i__51)
            t258 = inline330
            var t259 int32 = t258 + 1
            ref_set__Ref_5int32(i__51, t259)
            continue
        } else {
            break Loop_loop255
        }
    }
    var t251 string
    var inline352 string = _goml_runtime_core_int_to_string(vlen__49)
    t251 = inline352
    var t252 string = "len:" + t251
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline349)
    var t253 string
    var inline347 string = _goml_runtime_core_int32_to_string(jp249)
    t253 = inline347
    var t254 string = "delta:" + t253
    var inline344 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline344)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t262 string = _goml_runtime_core_int32_to_string(self__35)
    return t262
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__232 int32) *ref_int32_x {
    var t270 *ref_int32_x = ref__Ref_5int32(value__232)
    return t270
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env151 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env151.tag_0
    var t298 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t298
}

func main() {
    main0()
}
