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
    var ret139 string
    var t88 int32 = self__0.x
    var t87 string = int32_to_string(t88)
    var t86 string = "Point(" + t87
    var t85 string = t86 + ","
    var t90 int32 = self__0.y
    var t89 string = int32_to_string(t90)
    var t84 string = t85 + t89
    ret139 = t84 + ")"
    return ret139
}

func _goml_trait_impl_Display_Point_show_with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var ret140 string
    var t95 string = prefix__2 + "Point("
    var t97 int32 = self__1.x
    var t96 string = int32_to_string(t97)
    var t94 string = t95 + t96
    var t93 string = t94 + ","
    var t99 int32 = self__1.y
    var t98 string = int32_to_string(t99)
    var t92 string = t93 + t98
    var t91 string = t92 + ")"
    ret140 = t91 + suffix__3
    return ret140
}

func _goml_trait_impl_Display_Point_tick(self__4 Point) struct{} {
    var ret141 struct{}
    ret141 = struct{}{}
    return ret141
}

func _goml_trait_impl_Display_Point_bump(self__5 Point, delta__6 int32) int32 {
    var ret142 int32
    var t101 int32 = self__5.x
    var t102 int32 = self__5.y
    var t100 int32 = t101 + t102
    ret142 = t100 + delta__6
    return ret142
}

func _goml_trait_impl_Display_Flag_show(self__7 Flag) string {
    var ret143 string
    var t103 bool = self__7.value
    if t103 {
        ret143 = "Flag(true)"
    } else {
        ret143 = "Flag(false)"
    }
    return ret143
}

func _goml_trait_impl_Display_Flag_show_with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var ret144 string
    var t104 bool = self__8.value
    if t104 {
        var t105 string = prefix__9 + "Flag(true)"
        ret144 = t105 + suffix__10
    } else {
        var t106 string = prefix__9 + "Flag(false)"
        ret144 = t106 + suffix__10
    }
    return ret144
}

func _goml_trait_impl_Display_Flag_tick(self__11 Flag) struct{} {
    var ret145 struct{}
    ret145 = struct{}{}
    return ret145
}

func _goml_trait_impl_Display_Flag_bump(self__12 Flag, delta__13 int32) int32 {
    var ret146 int32
    var t107 bool = self__12.value
    if t107 {
        ret146 = delta__13
    } else {
        ret146 = -delta__13
    }
    return ret146
}

func _goml_trait_impl_Display_Counter_show(self__14 Counter) string {
    var ret147 string
    var t111 *ref_int32_x = self__14.cell
    var t110 int32 = ref_get__Ref_int32(t111)
    var t109 string = int32_to_string(t110)
    var t108 string = "Counter(" + t109
    ret147 = t108 + ")"
    return ret147
}

func _goml_trait_impl_Display_Counter_show_with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var ret148 string
    var t114 string = prefix__16 + "Counter("
    var t117 *ref_int32_x = self__15.cell
    var t116 int32 = ref_get__Ref_int32(t117)
    var t115 string = int32_to_string(t116)
    var t113 string = t114 + t115
    var t112 string = t113 + ")"
    ret148 = t112 + suffix__17
    return ret148
}

func _goml_trait_impl_Display_Counter_tick(self__18 Counter) struct{} {
    var ret149 struct{}
    var t119 *ref_int32_x = self__18.cell
    var t118 int32 = ref_get__Ref_int32(t119)
    var next__19 int32 = t118 + 1
    var t120 *ref_int32_x = self__18.cell
    ref_set__Ref_int32(t120, next__19)
    ret149 = struct{}{}
    return ret149
}

func _goml_trait_impl_Display_Counter_bump(self__20 Counter, delta__21 int32) int32 {
    var ret150 int32
    var t122 *ref_int32_x = self__20.cell
    var t121 int32 = ref_get__Ref_int32(t122)
    var next__22 int32 = t121 + delta__21
    var t123 *ref_int32_x = self__20.cell
    ref_set__Ref_int32(t123, next__22)
    ret150 = next__22
    return ret150
}

func show_dyn(x__23 dyn__Display) string {
    var ret151 string
    ret151 = x__23.vtable.show_with(x__23.data, "<", ">")
    return ret151
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var ret152 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    ret152 = _goml_inherent_closure_env_f_0_closure_env_f_0_apply(f__28, x__24, tag__25)
    return ret152
}

func make_renderer(tag__29 string) closure_env_make_renderer_1 {
    var ret153 closure_env_make_renderer_1
    ret153 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    return ret153
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var ret154 string
    x__31.vtable.tick(x__31.data)
    var t125 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t124 string = t125 + ":"
    var t127 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t126 string = int32_to_string(t127)
    ret154 = t124 + t126
    return ret154
}

func main0() struct{} {
    var ret155 struct{}
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
    var t128 *ref_int32_x = ref__Ref_int32(10)
    var c__37 Counter = Counter{
        cell: t128,
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
    var t130 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_star__43, dp1__38)
    var t129 string = t130 + "|"
    var t131 string = _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(render_angle__44, df1__40)
    var s2__47 string = t129 + t131
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
    var cond156 bool
    for {
        var t132 int32 = ref_get__Ref_int32(i__54)
        cond156 = t132 < 3
        if !cond156 {
            break
        }
        var line__55 string = bump_and_show(dc__42, delta__53)
        string_println(line__55)
        var t134 int32 = ref_get__Ref_int32(i__54)
        var t133 int32 = t134 + 1
        ref_set__Ref_int32(i__54, t133)
    }
    var t136 string = int32_to_string(vlen__52)
    var t135 string = "len:" + t136
    string_println(t135)
    var t138 string = int32_to_string(delta__53)
    var t137 string = "delta:" + t138
    string_println(t137)
    ret155 = struct{}{}
    return ret155
}

func _goml_inherent_closure_env_f_0_closure_env_f_0_apply(env82 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var ret157 string
    ret157 = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return ret157
}

func _goml_inherent_closure_env_make_renderer_1_closure_env_make_renderer_1_apply(env83 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var ret158 string
    var tag__29 string = env83.tag_0
    ret158 = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return ret158
}

func main() {
    main0()
}
