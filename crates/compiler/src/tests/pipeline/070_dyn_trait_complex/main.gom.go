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
    var t13 int32
    var t14 string
    var t15 string
    var t16 string
    var t17 int32
    var t18 string
    var t19 string
    var t20 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t13 = self__0.x
            t14 = int32_to_string(t13)
            t15 = "Point(" + t14
            t16 = t15 + ","
            t17 = self__0.y
            t18 = int32_to_string(t17)
            t19 = t16 + t18
            t20 = t19 + ")"
            return t20
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Point_show_with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t21 string
    var t22 int32
    var t23 string
    var t24 string
    var t25 string
    var t26 int32
    var t27 string
    var t28 string
    var t29 string
    var t30 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t21 = prefix__2 + "Point("
            t22 = self__1.x
            t23 = int32_to_string(t22)
            t24 = t21 + t23
            t25 = t24 + ","
            t26 = self__1.y
            t27 = int32_to_string(t26)
            t28 = t25 + t27
            t29 = t28 + ")"
            t30 = t29 + suffix__3
            return t30
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Point_tick(self__4 Point) struct{} {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Point_bump(self__5 Point, delta__6 int32) int32 {
    var t31 int32
    var t32 int32
    var t33 int32
    var t34 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t31 = self__5.x
            t32 = self__5.y
            t33 = t31 + t32
            t34 = t33 + delta__6
            return t34
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Flag_show(self__7 Flag) string {
    var t37 bool
    var jp36 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t37 = self__7.value
            if t37 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp36
        case 2:
            jp36 = "Flag(true)"
            pc = 1
        case 3:
            jp36 = "Flag(false)"
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Flag_show_with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t40 bool
    var jp39 string
    var t41 string
    var t42 string
    var t43 string
    var t44 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t40 = self__8.value
            if t40 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp39
        case 2:
            t41 = prefix__9 + "Flag(true)"
            t42 = t41 + suffix__10
            jp39 = t42
            pc = 1
        case 3:
            t43 = prefix__9 + "Flag(false)"
            t44 = t43 + suffix__10
            jp39 = t44
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Flag_tick(self__11 Flag) struct{} {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Flag_bump(self__12 Flag, delta__13 int32) int32 {
    var t47 bool
    var jp46 int32
    var t48 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t47 = self__12.value
            if t47 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp46
        case 2:
            jp46 = delta__13
            pc = 1
        case 3:
            t48 = -delta__13
            jp46 = t48
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Counter_show(self__14 Counter) string {
    var t49 *ref_int32_x
    var t50 int32
    var t51 string
    var t52 string
    var t53 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t49 = self__14.cell
            t50 = ref_get__Ref_int32(t49)
            t51 = int32_to_string(t50)
            t52 = "Counter(" + t51
            t53 = t52 + ")"
            return t53
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Counter_show_with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t54 string
    var t55 *ref_int32_x
    var t56 int32
    var t57 string
    var t58 string
    var t59 string
    var t60 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t54 = prefix__16 + "Counter("
            t55 = self__15.cell
            t56 = ref_get__Ref_int32(t55)
            t57 = int32_to_string(t56)
            t58 = t54 + t57
            t59 = t58 + ")"
            t60 = t59 + suffix__17
            return t60
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Counter_tick(self__18 Counter) struct{} {
    var t61 *ref_int32_x
    var t62 int32
    var next__19 int32
    var t63 *ref_int32_x
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t61 = self__18.cell
            t62 = ref_get__Ref_int32(t61)
            next__19 = t62 + 1
            t63 = self__18.cell
            ref_set__Ref_int32(t63, next__19)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Counter_bump(self__20 Counter, delta__21 int32) int32 {
    var t64 *ref_int32_x
    var t65 int32
    var next__22 int32
    var t66 *ref_int32_x
    var mtmp1 struct{}
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t64 = self__20.cell
            t65 = ref_get__Ref_int32(t64)
            next__22 = t65 + delta__21
            t66 = self__20.cell
            ref_set__Ref_int32(t66, next__22)
            return next__22
        default:
            panic("invalid pc")
        }
    }
}

func show_dyn(x__23 dyn__Display) string {
    var t67 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t67 = x__23.vtable.show_with(x__23.data, "<", ">")
            return t67
        default:
            panic("invalid pc")
        }
    }
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var f__28 closure_env_f_0
    var t68 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            f__28 = closure_env_f_0{}
            t68 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__28, x__24, tag__25)
            return t68
        default:
            panic("invalid pc")
        }
    }
}

func make_renderer(tag__29 string) closure_env_make_renderer_1 {
    var t69 closure_env_make_renderer_1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t69 = closure_env_make_renderer_1{
                tag_0: tag__29,
            }
            return t69
        default:
            panic("invalid pc")
        }
    }
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var mtmp2 struct{}
    var t70 string
    var t71 string
    var t72 int32
    var t73 string
    var t74 string
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x__31.vtable.tick(x__31.data)
            t70 = x__31.vtable.show_with(x__31.data, "[", "]")
            t71 = t70 + ":"
            t72 = x__31.vtable.bump(x__31.data, delta__32)
            t73 = int32_to_string(t72)
            t74 = t71 + t73
            return t74
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var p1__33 Point
    var p2__34 Point
    var f1__35 Flag
    var f2__36 Flag
    var t75 *ref_int32_x
    var c__37 Counter
    var dp1__38 dyn__Display
    var dp2__39 dyn__Display
    var df1__40 dyn__Display
    var df2__41 dyn__Display
    var dc__42 dyn__Display
    var render_star__43 closure_env_make_renderer_1
    var render_angle__44 closure_env_make_renderer_1
    var s0__45 string
    var s1__46 string
    var t76 string
    var t77 string
    var t78 string
    var s2__47 string
    var v__48 []dyn__Display
    var v__49 []dyn__Display
    var v__50 []dyn__Display
    var v__51 []dyn__Display
    var vlen__52 int32
    var jp80 int32
    var delta__53 int32
    var mtmp3 struct{}
    var mtmp4 struct{}
    var mtmp5 struct{}
    var i__54 *ref_int32_x
    var mtmp8 struct{}
    var t82 string
    var t83 string
    var mtmp9 struct{}
    var t84 string
    var t85 string
    var mtmp10 struct{}
    var t87 int32
    var t88 bool
    var line__55 string
    var _wild6 struct{}
    var t89 int32
    var t90 int32
    var mtmp7 struct{}
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    _ = mtmp8
    _ = mtmp9
    _ = mtmp10
    _ = _wild6
    _ = mtmp7
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            p1__33 = Point{
                x: 1,
                y: 2,
            }
            p2__34 = Point{
                x: 3,
                y: 4,
            }
            f1__35 = Flag{
                value: true,
            }
            f2__36 = Flag{
                value: false,
            }
            t75 = ref__Ref_int32(10)
            c__37 = Counter{
                cell: t75,
            }
            dp1__38 = dyn__Display{
                data: p1__33,
                vtable: dyn__Display__vtable__Point(),
            }
            dp2__39 = dyn__Display{
                data: p2__34,
                vtable: dyn__Display__vtable__Point(),
            }
            df1__40 = dyn__Display{
                data: f1__35,
                vtable: dyn__Display__vtable__Flag(),
            }
            df2__41 = dyn__Display{
                data: f2__36,
                vtable: dyn__Display__vtable__Flag(),
            }
            dc__42 = dyn__Display{
                data: c__37,
                vtable: dyn__Display__vtable__Counter(),
            }
            render_star__43 = make_renderer("*")
            render_angle__44 = make_renderer("<")
            s0__45 = show_dyn(dp2__39)
            s1__46 = call_via_closure(df2__41, "*")
            t76 = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_star__43, dp1__38)
            t77 = t76 + "|"
            t78 = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_angle__44, df1__40)
            s2__47 = t77 + t78
            v__48 = nil
            v__49 = append(v__48, dp1__38)
            v__50 = append(v__49, df1__40)
            v__51 = append(v__50, dc__42)
            vlen__52 = int32(len(v__51))
            switch vlen__52 {
            case 2:
                pc = 6
            default:
                pc = 7
            }
        case 1:
            delta__53 = jp80
            string_println(s0__45)
            string_println(s1__46)
            string_println(s2__47)
            i__54 = ref__Ref_int32(0)
            pc = 3
        case 2:
            t82 = int32_to_string(vlen__52)
            t83 = "len:" + t82
            string_println(t83)
            t84 = int32_to_string(delta__53)
            t85 = "delta:" + t84
            string_println(t85)
            return struct{}{}
        case 3:
            t87 = ref_get__Ref_int32(i__54)
            t88 = t87 < 3
            if t88 {
                pc = 4
            } else {
                pc = 5
            }
        case 4:
            line__55 = bump_and_show(dc__42, delta__53)
            string_println(line__55)
            t89 = ref_get__Ref_int32(i__54)
            t90 = t89 + 1
            ref_set__Ref_int32(i__54, t90)
            pc = 3
        case 5:
            pc = 2
        case 6:
            jp80 = 3
            pc = 1
        case 7:
            jp80 = 5
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env11 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t91 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t91 = v__26.vtable.show_with(v__26.data, t__27, t__27)
            return t91
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(env12 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string
    var t92 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            tag__29 = env12.tag_0
            t92 = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
            return t92
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
