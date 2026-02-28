package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
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
    return _goml_trait_impl_Display_Counter_show(self.(Counter))
}

func dyn__Display__wrap__Counter__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_impl_Display_Counter_show_with(self.(Counter), p0, p1)
}

func dyn__Display__wrap__Counter__tick(self any) struct{} {
    return _goml_trait_impl_Display_Counter_tick(self.(Counter))
}

func dyn__Display__wrap__Counter__bump(self any, p0 int32) int32 {
    return _goml_trait_impl_Display_Counter_bump(self.(Counter), p0)
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
    return _goml_trait_impl_Display_Flag_show(self.(Flag))
}

func dyn__Display__wrap__Flag__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_impl_Display_Flag_show_with(self.(Flag), p0, p1)
}

func dyn__Display__wrap__Flag__tick(self any) struct{} {
    return _goml_trait_impl_Display_Flag_tick(self.(Flag))
}

func dyn__Display__wrap__Flag__bump(self any, p0 int32) int32 {
    return _goml_trait_impl_Display_Flag_bump(self.(Flag), p0)
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
    return _goml_trait_impl_Display_Point_show(self.(Point))
}

func dyn__Display__wrap__Point__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_impl_Display_Point_show_with(self.(Point), p0, p1)
}

func dyn__Display__wrap__Point__tick(self any) struct{} {
    return _goml_trait_impl_Display_Point_tick(self.(Point))
}

func dyn__Display__wrap__Point__bump(self any, p0 int32) int32 {
    return _goml_trait_impl_Display_Point_bump(self.(Point), p0)
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
        show_with: dyn__Display__wrap__Point__show_with,
        tick: dyn__Display__wrap__Point__tick,
        bump: dyn__Display__wrap__Point__bump,
    }
}

func _goml_trait_impl_Display_Point_show(self__0 Point) string {
    var t13 int32 = self__0.x
    var t14 string = int32_to_string(t13)
    var t15 string = "Point(" + t14
    var t16 string = t15 + ","
    var t17 int32 = self__0.y
    var t18 string = int32_to_string(t17)
    var t19 string = t16 + t18
    var t20 string = t19 + ")"
    return t20
}

func _goml_trait_impl_Display_Point_show_with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t21 string = prefix__2 + "Point("
    var t22 int32 = self__1.x
    var t23 string = int32_to_string(t22)
    var t24 string = t21 + t23
    var t25 string = t24 + ","
    var t26 int32 = self__1.y
    var t27 string = int32_to_string(t26)
    var t28 string = t25 + t27
    var t29 string = t28 + ")"
    var t30 string = t29 + suffix__3
    return t30
}

func _goml_trait_impl_Display_Point_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_trait_impl_Display_Point_bump(self__5 Point, delta__6 int32) int32 {
    var t31 int32 = self__5.x
    var t32 int32 = self__5.y
    var t33 int32 = t31 + t32
    var t34 int32 = t33 + delta__6
    return t34
}

func _goml_trait_impl_Display_Flag_show(self__7 Flag) string {
    var t37 bool = self__7.value
    var jp36 string
    if t37 {
        jp36 = "Flag(true)"
    } else {
        jp36 = "Flag(false)"
    }
    return jp36
}

func _goml_trait_impl_Display_Flag_show_with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t40 bool = self__8.value
    var jp39 string
    if t40 {
        var t41 string = prefix__9 + "Flag(true)"
        var t42 string = t41 + suffix__10
        jp39 = t42
    } else {
        var t43 string = prefix__9 + "Flag(false)"
        var t44 string = t43 + suffix__10
        jp39 = t44
    }
    return jp39
}

func _goml_trait_impl_Display_Flag_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_trait_impl_Display_Flag_bump(self__12 Flag, delta__13 int32) int32 {
    var t47 bool = self__12.value
    var jp46 int32
    if t47 {
        jp46 = delta__13
    } else {
        var t48 int32 = -delta__13
        jp46 = t48
    }
    return jp46
}

func _goml_trait_impl_Display_Counter_show(self__14 Counter) string {
    var t49 *ref_int32_x = self__14.cell
    var t50 int32 = ref_get__Ref_int32(t49)
    var t51 string = int32_to_string(t50)
    var t52 string = "Counter(" + t51
    var t53 string = t52 + ")"
    return t53
}

func _goml_trait_impl_Display_Counter_show_with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t54 string = prefix__16 + "Counter("
    var t55 *ref_int32_x = self__15.cell
    var t56 int32 = ref_get__Ref_int32(t55)
    var t57 string = int32_to_string(t56)
    var t58 string = t54 + t57
    var t59 string = t58 + ")"
    var t60 string = t59 + suffix__17
    return t60
}

func _goml_trait_impl_Display_Counter_tick(self__18 Counter) struct{} {
    var t61 *ref_int32_x = self__18.cell
    var t62 int32 = ref_get__Ref_int32(t61)
    var next__19 int32 = t62 + 1
    var t63 *ref_int32_x = self__18.cell
    ref_set__Ref_int32(t63, next__19)
    return struct{}{}
}

func _goml_trait_impl_Display_Counter_bump(self__20 Counter, delta__21 int32) int32 {
    var t64 *ref_int32_x = self__20.cell
    var t65 int32 = ref_get__Ref_int32(t64)
    var next__22 int32 = t65 + delta__21
    var t66 *ref_int32_x = self__20.cell
    ref_set__Ref_int32(t66, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t67 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t67
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t68 string = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__28, x__24, tag__25)
    return t68
}

func make_renderer(tag__29 string) closure_env_make_renderer_1 {
    var t69 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    return t69
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    x__31.vtable.tick(x__31.data)
    var t70 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t71 string = t70 + ":"
    var t72 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t73 string = int32_to_string(t72)
    var t74 string = t71 + t73
    return t74
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
    var t75 *ref_int32_x = ref__Ref_int32(10)
    var c__37 Counter = Counter{
        cell: t75,
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
    var render_star__43 closure_env_make_renderer_1 = make_renderer("*")
    var render_angle__44 closure_env_make_renderer_1 = make_renderer("<")
    var s0__45 string = show_dyn(dp2__39)
    var s1__46 string = call_via_closure(df2__41, "*")
    var t76 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_star__43, dp1__38)
    var t77 string = t76 + "|"
    var t78 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_angle__44, df1__40)
    var s2__47 string = t77 + t78
    var v__48 []dyn__Display = nil
    var v__49 []dyn__Display = append(v__48, dp1__38)
    var v__50 []dyn__Display = append(v__49, df1__40)
    var v__51 []dyn__Display = append(v__50, dc__42)
    var vlen__52 int32 = int32(len(v__51))
    var jp80 int32
    switch vlen__52 {
    case 2:
        jp80 = 3
    default:
        jp80 = 5
    }
    var delta__53 int32 = jp80
    string_println(s0__45)
    string_println(s1__46)
    string_println(s2__47)
    var i__54 *ref_int32_x = ref__Ref_int32(0)
    for {
        var t87 int32 = ref_get__Ref_int32(i__54)
        var t88 bool = t87 < 3
        if !t88 {
            break
        }
        var line__55 string = bump_and_show(dc__42, delta__53)
        string_println(line__55)
        var t89 int32 = ref_get__Ref_int32(i__54)
        var t90 int32 = t89 + 1
        ref_set__Ref_int32(i__54, t90)
    }
    var t82 string = int32_to_string(vlen__52)
    var t83 string = "len:" + t82
    string_println(t83)
    var t84 string = int32_to_string(delta__53)
    var t85 string = "delta:" + t84
    string_println(t85)
    return struct{}{}
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env11 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t91 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t91
}

func _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(env12 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env12.tag_0
    var t92 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t92
}

func main() {
    main0()
}
