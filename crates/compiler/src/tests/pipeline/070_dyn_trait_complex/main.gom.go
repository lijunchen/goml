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
    var ret68 string
    var t17 int32 = self__0.x
    var t16 string = int32_to_string(t17)
    var t15 string = "Point(" + t16
    var t14 string = t15 + ","
    var t19 int32 = self__0.y
    var t18 string = int32_to_string(t19)
    var t13 string = t14 + t18
    ret68 = t13 + ")"
    return ret68
}

func _goml_trait_impl_Display_Point_show_with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var ret69 string
    var t24 string = prefix__2 + "Point("
    var t26 int32 = self__1.x
    var t25 string = int32_to_string(t26)
    var t23 string = t24 + t25
    var t22 string = t23 + ","
    var t28 int32 = self__1.y
    var t27 string = int32_to_string(t28)
    var t21 string = t22 + t27
    var t20 string = t21 + ")"
    ret69 = t20 + suffix__3
    return ret69
}

func _goml_trait_impl_Display_Point_tick(self__4 Point) struct{} {
    var ret70 struct{}
    ret70 = struct{}{}
    return ret70
}

func _goml_trait_impl_Display_Point_bump(self__5 Point, delta__6 int32) int32 {
    var ret71 int32
    var t30 int32 = self__5.x
    var t31 int32 = self__5.y
    var t29 int32 = t30 + t31
    ret71 = t29 + delta__6
    return ret71
}

func _goml_trait_impl_Display_Flag_show(self__7 Flag) string {
    var ret72 string
    var t32 bool = self__7.value
    if t32 {
        ret72 = "Flag(true)"
    } else {
        ret72 = "Flag(false)"
    }
    return ret72
}

func _goml_trait_impl_Display_Flag_show_with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var ret73 string
    var t33 bool = self__8.value
    if t33 {
        var t34 string = prefix__9 + "Flag(true)"
        ret73 = t34 + suffix__10
    } else {
        var t35 string = prefix__9 + "Flag(false)"
        ret73 = t35 + suffix__10
    }
    return ret73
}

func _goml_trait_impl_Display_Flag_tick(self__11 Flag) struct{} {
    var ret74 struct{}
    ret74 = struct{}{}
    return ret74
}

func _goml_trait_impl_Display_Flag_bump(self__12 Flag, delta__13 int32) int32 {
    var ret75 int32
    var t36 bool = self__12.value
    if t36 {
        ret75 = delta__13
    } else {
        ret75 = -delta__13
    }
    return ret75
}

func _goml_trait_impl_Display_Counter_show(self__14 Counter) string {
    var ret76 string
    var t40 *ref_int32_x = self__14.cell
    var t39 int32 = ref_get__Ref_int32(t40)
    var t38 string = int32_to_string(t39)
    var t37 string = "Counter(" + t38
    ret76 = t37 + ")"
    return ret76
}

func _goml_trait_impl_Display_Counter_show_with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var ret77 string
    var t43 string = prefix__16 + "Counter("
    var t46 *ref_int32_x = self__15.cell
    var t45 int32 = ref_get__Ref_int32(t46)
    var t44 string = int32_to_string(t45)
    var t42 string = t43 + t44
    var t41 string = t42 + ")"
    ret77 = t41 + suffix__17
    return ret77
}

func _goml_trait_impl_Display_Counter_tick(self__18 Counter) struct{} {
    var ret78 struct{}
    var t48 *ref_int32_x = self__18.cell
    var t47 int32 = ref_get__Ref_int32(t48)
    var next__19 int32 = t47 + 1
    var t49 *ref_int32_x = self__18.cell
    ref_set__Ref_int32(t49, next__19)
    ret78 = struct{}{}
    return ret78
}

func _goml_trait_impl_Display_Counter_bump(self__20 Counter, delta__21 int32) int32 {
    var ret79 int32
    var t51 *ref_int32_x = self__20.cell
    var t50 int32 = ref_get__Ref_int32(t51)
    var next__22 int32 = t50 + delta__21
    var t52 *ref_int32_x = self__20.cell
    ref_set__Ref_int32(t52, next__22)
    ret79 = next__22
    return ret79
}

func show_dyn(x__23 dyn__Display) string {
    var ret80 string
    ret80 = x__23.vtable.show_with(x__23.data, "<", ">")
    return ret80
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var ret81 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    ret81 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__28, x__24, tag__25)
    return ret81
}

func make_renderer(tag__29 string) closure_env_make_renderer_1 {
    var ret82 closure_env_make_renderer_1
    ret82 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    return ret82
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var ret83 string
    x__31.vtable.tick(x__31.data)
    var t54 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t53 string = t54 + ":"
    var t56 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t55 string = int32_to_string(t56)
    ret83 = t53 + t55
    return ret83
}

func main0() struct{} {
    var ret84 struct{}
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
    var t57 *ref_int32_x = ref__Ref_int32(10)
    var c__37 Counter = Counter{
        cell: t57,
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
    var t59 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_star__43, dp1__38)
    var t58 string = t59 + "|"
    var t60 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_angle__44, df1__40)
    var s2__47 string = t58 + t60
    var v__48 []dyn__Display = nil
    var v__49 []dyn__Display = append(v__48, dp1__38)
    var v__50 []dyn__Display = append(v__49, df1__40)
    var v__51 []dyn__Display = append(v__50, dc__42)
    var vlen__52 int32 = int32(len(v__51))
    var delta__53 int32
    switch vlen__52 {
    case 2:
        delta__53 = 3
    default:
        delta__53 = 5
    }
    string_println(s0__45)
    string_println(s1__46)
    string_println(s2__47)
    var i__54 *ref_int32_x = ref__Ref_int32(0)
    var cond85 bool
    for {
        var t61 int32 = ref_get__Ref_int32(i__54)
        cond85 = t61 < 3
        if !cond85 {
            break
        }
        var line__55 string = bump_and_show(dc__42, delta__53)
        string_println(line__55)
        var t63 int32 = ref_get__Ref_int32(i__54)
        var t62 int32 = t63 + 1
        ref_set__Ref_int32(i__54, t62)
    }
    var t65 string = int32_to_string(vlen__52)
    var t64 string = "len:" + t65
    string_println(t64)
    var t67 string = int32_to_string(delta__53)
    var t66 string = "delta:" + t67
    string_println(t66)
    ret84 = struct{}{}
    return ret84
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env11 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var ret86 string
    ret86 = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return ret86
}

func _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(env12 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var ret87 string
    var tag__29 string = env12.tag_0
    ret87 = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return ret87
}

func main() {
    main0()
}
