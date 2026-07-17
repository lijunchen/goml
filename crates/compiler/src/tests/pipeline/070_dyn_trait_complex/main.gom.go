package main

import (
    _goml_fmt "fmt"
)

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

func vec_len__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display) int32 {
    return int32(len(vec.items))
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
    var retv78 string
    var t79 int32 = self__0.x
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t79)
    var t81 string = "Point(" + t80
    var t82 string = t81 + ","
    var t83 int32 = self__0.y
    var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t83)
    var t85 string = t82 + t84
    var t86 string = t85 + ")"
    retv78 = t86
    return retv78
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv88 string
    var t89 string = prefix__2 + "Point("
    var t90 int32 = self__1.x
    var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t90)
    var t92 string = t89 + t91
    var t93 string = t92 + ","
    var t94 int32 = self__1.y
    var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t94)
    var t96 string = t93 + t95
    var t97 string = t96 + ")"
    var t98 string = t97 + suffix__3
    retv88 = t98
    return retv88
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv101 int32
    var t102 int32 = self__5.x
    var t103 int32 = self__5.y
    var t104 int32 = t102 + t103
    var t105 int32 = t104 + delta__6
    retv101 = t105
    return retv101
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv107 string
    var t110 bool = self__7.value
    var jp109 string
    if t110 {
        jp109 = "Flag(true)"
    } else {
        jp109 = "Flag(false)"
    }
    retv107 = jp109
    return retv107
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv112 string
    var t115 bool = self__8.value
    var jp114 string
    if t115 {
        var t116 string = prefix__9 + "Flag(true)"
        var t117 string = t116 + suffix__10
        jp114 = t117
    } else {
        var t118 string = prefix__9 + "Flag(false)"
        var t119 string = t118 + suffix__10
        jp114 = t119
    }
    retv112 = jp114
    return retv112
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv122 int32
    var t125 bool = self__12.value
    var jp124 int32
    if t125 {
        jp124 = delta__13
    } else {
        var t126 int32 = -delta__13
        jp124 = t126
    }
    retv122 = jp124
    return retv122
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv128 string
    var t129 *ref_int32_x = self__14.cell
    var t130 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t129)
    var t131 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t130)
    var t132 string = "Counter(" + t131
    var t133 string = t132 + ")"
    retv128 = t133
    return retv128
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv135 string
    var t136 string = prefix__16 + "Counter("
    var t137 *ref_int32_x = self__15.cell
    var t138 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t137)
    var t139 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t138)
    var t140 string = t136 + t139
    var t141 string = t140 + ")"
    var t142 string = t141 + suffix__17
    retv135 = t142
    return retv135
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t144 *ref_int32_x = self__18.cell
    var t145 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t144)
    var next__19 int32 = t145 + 1
    var t146 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t146, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv148 int32
    var t149 *ref_int32_x = self__20.cell
    var t150 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t149)
    var next__22 int32 = t150 + delta__21
    var t151 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t151, next__22)
    retv148 = next__22
    return retv148
}

func show_dyn(x__23 dyn__Display) string {
    var retv153 string
    var t154 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv153 = t154
    return retv153
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv156 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t157 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv156 = t157
    return retv156
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv159 func(dyn__Display) string
    var t160 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv159 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t160, p0)
    }
    return retv159
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv162 string
    x__31.vtable.tick(x__31.data)
    var t163 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t164 string = t163 + ":"
    var t165 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t165)
    var t167 string = t164 + t166
    retv162 = t167
    return retv162
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
    var t169 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t169,
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
    var t170 string = render_star__43(dp1__38)
    var t171 string = t170 + "|"
    var t172 string = render_angle__44(df1__40)
    var s2__47 string = t171 + t172
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp174 int32
    switch vlen__49 {
    case 2:
        jp174 = 3
    default:
        jp174 = 5
    }
    var delta__50 int32 = jp174
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop180:
    for {
        var t181 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t182 bool = t181 < 3
        if t182 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t183 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t184 int32 = t183 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t184)
            continue
        } else {
            break Loop_loop180
        }
    }
    var t176 string = _goml_m_inherent_i_int32_i_int32_i_to__string(vlen__49)
    var t177 string = "len:" + t176
    println__T_string(t177)
    var t178 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t179 string = "delta:" + t178
    println__T_string(t179)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv186 string
    var t187 string = _goml_runtime_core_int32_to_string(self__5)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv189 int32
    var t190 int32 = ref_get__Ref_5int32(self__205)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv194 *ref_int32_x
    var t195 *ref_int32_x = ref__Ref_5int32(value__204)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv197 *_goml_vec_Dyn_Display
    var t198 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__123 *_goml_vec_Dyn_Display, elem__124 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__134 *_goml_vec_Dyn_Display) int32 {
    var retv202 int32
    var t203 int32 = vec_len__Vec_11Dyn_Display(self__134)
    retv202 = t203
    return retv202
}

func println__T_string(value__1 string) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv208 string
    retv208 = self__37
    return retv208
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env75 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv216 string
    var t217 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv216 = t217
    return retv216
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env76 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv219 string
    var tag__29 string = env76.tag_0
    var t220 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv219 = t220
    return retv219
}

func main() {
    main0()
}
