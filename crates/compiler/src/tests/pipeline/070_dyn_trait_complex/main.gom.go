package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type GoError = error

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
    return _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_show(self.(Counter))
}

func dyn__Display__wrap__Counter__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_show_x5f_with(self.(Counter), p0, p1)
}

func dyn__Display__wrap__Counter__tick(self any) struct{} {
    return _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_tick(self.(Counter))
}

func dyn__Display__wrap__Counter__bump(self any, p0 int32) int32 {
    return _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_bump(self.(Counter), p0)
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
    return _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show(self.(Flag))
}

func dyn__Display__wrap__Flag__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show_x5f_with(self.(Flag), p0, p1)
}

func dyn__Display__wrap__Flag__tick(self any) struct{} {
    return _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_tick(self.(Flag))
}

func dyn__Display__wrap__Flag__bump(self any, p0 int32) int32 {
    return _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_bump(self.(Flag), p0)
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
    return _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show(self.(Point))
}

func dyn__Display__wrap__Point__show_with(self any, p0 string, p1 string) string {
    return _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show_x5f_with(self.(Point), p0, p1)
}

func dyn__Display__wrap__Point__tick(self any) struct{} {
    return _goml_trait_x5f_impl_x23_Display_x23_Point_x23_tick(self.(Point))
}

func dyn__Display__wrap__Point__bump(self any, p0 int32) int32 {
    return _goml_trait_x5f_impl_x23_Display_x23_Point_x23_bump(self.(Point), p0)
}

func dyn__Display__vtable__Point() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__Point__show,
        show_with: dyn__Display__wrap__Point__show_with,
        tick: dyn__Display__wrap__Point__tick,
        bump: dyn__Display__wrap__Point__bump,
    }
}

func _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show(self__0 Point) string {
    var retv14 string
    var t15 int32 = self__0.x
    var t16 string = int32_to_string(t15)
    var t17 string = "Point(" + t16
    var t18 string = t17 + ","
    var t19 int32 = self__0.y
    var t20 string = int32_to_string(t19)
    var t21 string = t18 + t20
    var t22 string = t21 + ")"
    retv14 = t22
    return retv14
}

func _goml_trait_x5f_impl_x23_Display_x23_Point_x23_show_x5f_with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv24 string
    var t25 string = prefix__2 + "Point("
    var t26 int32 = self__1.x
    var t27 string = int32_to_string(t26)
    var t28 string = t25 + t27
    var t29 string = t28 + ","
    var t30 int32 = self__1.y
    var t31 string = int32_to_string(t30)
    var t32 string = t29 + t31
    var t33 string = t32 + ")"
    var t34 string = t33 + suffix__3
    retv24 = t34
    return retv24
}

func _goml_trait_x5f_impl_x23_Display_x23_Point_x23_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Display_x23_Point_x23_bump(self__5 Point, delta__6 int32) int32 {
    var retv37 int32
    var t38 int32 = self__5.x
    var t39 int32 = self__5.y
    var t40 int32 = t38 + t39
    var t41 int32 = t40 + delta__6
    retv37 = t41
    return retv37
}

func _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show(self__7 Flag) string {
    var retv43 string
    var t46 bool = self__7.value
    var jp45 string
    if t46 {
        jp45 = "Flag(true)"
    } else {
        jp45 = "Flag(false)"
    }
    retv43 = jp45
    return retv43
}

func _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_show_x5f_with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv48 string
    var t51 bool = self__8.value
    var jp50 string
    if t51 {
        var t52 string = prefix__9 + "Flag(true)"
        var t53 string = t52 + suffix__10
        jp50 = t53
    } else {
        var t54 string = prefix__9 + "Flag(false)"
        var t55 string = t54 + suffix__10
        jp50 = t55
    }
    retv48 = jp50
    return retv48
}

func _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Display_x23_Flag_x23_bump(self__12 Flag, delta__13 int32) int32 {
    var retv58 int32
    var t61 bool = self__12.value
    var jp60 int32
    if t61 {
        jp60 = delta__13
    } else {
        var t62 int32 = -delta__13
        jp60 = t62
    }
    retv58 = jp60
    return retv58
}

func _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_show(self__14 Counter) string {
    var retv64 string
    var t65 *ref_int32_x = self__14.cell
    var t66 int32 = ref_get__Ref_5int32(t65)
    var t67 string = int32_to_string(t66)
    var t68 string = "Counter(" + t67
    var t69 string = t68 + ")"
    retv64 = t69
    return retv64
}

func _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_show_x5f_with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv71 string
    var t72 string = prefix__16 + "Counter("
    var t73 *ref_int32_x = self__15.cell
    var t74 int32 = ref_get__Ref_5int32(t73)
    var t75 string = int32_to_string(t74)
    var t76 string = t72 + t75
    var t77 string = t76 + ")"
    var t78 string = t77 + suffix__17
    retv71 = t78
    return retv71
}

func _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_tick(self__18 Counter) struct{} {
    var t80 *ref_int32_x = self__18.cell
    var t81 int32 = ref_get__Ref_5int32(t80)
    var next__19 int32 = t81 + 1
    var t82 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t82, next__19)
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Display_x23_Counter_x23_bump(self__20 Counter, delta__21 int32) int32 {
    var retv84 int32
    var t85 *ref_int32_x = self__20.cell
    var t86 int32 = ref_get__Ref_5int32(t85)
    var next__22 int32 = t86 + delta__21
    var t87 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t87, next__22)
    retv84 = next__22
    return retv84
}

func show_dyn(x__23 dyn__Display) string {
    var retv89 string
    var t90 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv89 = t90
    return retv89
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv92 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t93 string = _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(f__28, x__24, tag__25)
    retv92 = t93
    return retv92
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv95 func(dyn__Display) string
    var t96 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv95 = func(p0 dyn__Display) string {
        return _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_renderer_x5f_1_x23_closure_x5f_env_x5f_make_x5f_renderer_x5f_1_x23_apply(t96, p0)
    }
    return retv95
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv98 string
    x__31.vtable.tick(x__31.data)
    var t99 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t100 string = t99 + ":"
    var t101 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t102 string = int32_to_string(t101)
    var t103 string = t100 + t102
    retv98 = t103
    return retv98
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
    var t105 *ref_int32_x = ref__Ref_5int32(10)
    var c__37 Counter = Counter{
        cell: t105,
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
    var t106 string = render_star__43(dp1__38)
    var t107 string = t106 + "|"
    var t108 string = render_angle__44(df1__40)
    var s2__47 string = t107 + t108
    var v__48 []dyn__Display = nil
    var v__49 []dyn__Display = append(v__48, dp1__38)
    var v__50 []dyn__Display = append(v__49, df1__40)
    var v__51 []dyn__Display = append(v__50, dc__42)
    var vlen__52 int32 = int32(len(v__51))
    var jp110 int32
    switch vlen__52 {
    case 2:
        jp110 = 3
    default:
        jp110 = 5
    }
    var delta__53 int32 = jp110
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__54 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop116:
    for {
        var t117 int32 = ref_get__Ref_5int32(i__54)
        var t118 bool = t117 < 3
        if t118 {
            var line__55 string = bump_and_show(dc__42, delta__53)
            println__T_string(line__55)
            var t119 int32 = ref_get__Ref_5int32(i__54)
            var t120 int32 = t119 + 1
            ref_set__Ref_5int32(i__54, t120)
            continue
        } else {
            break Loop_loop116
        }
    }
    var t112 string = int32_to_string(vlen__52)
    var t113 string = "len:" + t112
    println__T_string(t113)
    var t114 string = int32_to_string(delta__53)
    var t115 string = "delta:" + t114
    println__T_string(t115)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_x23_closure_x5f_env_x5f_f_x5f_0_x23_closure_x5f_env_x5f_f_x5f_0_x23_apply(env11 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv124 string
    var t125 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv124 = t125
    return retv124
}

func _goml_inherent_x23_closure_x5f_env_x5f_make_x5f_renderer_x5f_1_x23_closure_x5f_env_x5f_make_x5f_renderer_x5f_1_x23_apply(env12 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv127 string
    var tag__29 string = env12.tag_0
    var t128 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv127 = t128
    return retv127
}

func main() {
    main0()
}
