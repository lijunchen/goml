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
    var retv75 string
    var t76 int32 = self__0.x
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    var t78 string = "Point(" + t77
    var t79 string = t78 + ","
    var t80 int32 = self__0.y
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t80)
    var t82 string = t79 + t81
    var t83 string = t82 + ")"
    retv75 = t83
    return retv75
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv85 string
    var t86 string = prefix__2 + "Point("
    var t87 int32 = self__1.x
    var t88 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t87)
    var t89 string = t86 + t88
    var t90 string = t89 + ","
    var t91 int32 = self__1.y
    var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t91)
    var t93 string = t90 + t92
    var t94 string = t93 + ")"
    var t95 string = t94 + suffix__3
    retv85 = t95
    return retv85
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv98 int32
    var t99 int32 = self__5.x
    var t100 int32 = self__5.y
    var t101 int32 = t99 + t100
    var t102 int32 = t101 + delta__6
    retv98 = t102
    return retv98
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv104 string
    var t107 bool = self__7.value
    var jp106 string
    if t107 {
        jp106 = "Flag(true)"
    } else {
        jp106 = "Flag(false)"
    }
    retv104 = jp106
    return retv104
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv109 string
    var t112 bool = self__8.value
    var jp111 string
    if t112 {
        var t113 string = prefix__9 + "Flag(true)"
        var t114 string = t113 + suffix__10
        jp111 = t114
    } else {
        var t115 string = prefix__9 + "Flag(false)"
        var t116 string = t115 + suffix__10
        jp111 = t116
    }
    retv109 = jp111
    return retv109
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv119 int32
    var t122 bool = self__12.value
    var jp121 int32
    if t122 {
        jp121 = delta__13
    } else {
        var t123 int32 = -delta__13
        jp121 = t123
    }
    retv119 = jp121
    return retv119
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv125 string
    var t126 *ref_int32_x = self__14.cell
    var t127 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t126)
    var t128 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t127)
    var t129 string = "Counter(" + t128
    var t130 string = t129 + ")"
    retv125 = t130
    return retv125
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv132 string
    var t133 string = prefix__16 + "Counter("
    var t134 *ref_int32_x = self__15.cell
    var t135 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t134)
    var t136 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t135)
    var t137 string = t133 + t136
    var t138 string = t137 + ")"
    var t139 string = t138 + suffix__17
    retv132 = t139
    return retv132
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t141 *ref_int32_x = self__18.cell
    var t142 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t141)
    var next__19 int32 = t142 + 1
    var t143 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t143, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv145 int32
    var t146 *ref_int32_x = self__20.cell
    var t147 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t146)
    var next__22 int32 = t147 + delta__21
    var t148 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t148, next__22)
    retv145 = next__22
    return retv145
}

func show_dyn(x__23 dyn__Display) string {
    var retv150 string
    var t151 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv150 = t151
    return retv150
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv153 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t154 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv153 = t154
    return retv153
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv156 func(dyn__Display) string
    var t157 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv156 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t157, p0)
    }
    return retv156
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv159 string
    x__31.vtable.tick(x__31.data)
    var t160 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t161 string = t160 + ":"
    var t162 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t162)
    var t164 string = t161 + t163
    retv159 = t164
    return retv159
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
    var t166 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t166,
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
    var t167 string = render_star__43(dp1__38)
    var t168 string = t167 + "|"
    var t169 string = render_angle__44(df1__40)
    var s2__47 string = t168 + t169
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp171 int32
    switch vlen__49 {
    case 2:
        jp171 = 3
    default:
        jp171 = 5
    }
    var delta__50 int32 = jp171
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop177:
    for {
        var t178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t179 bool = t178 < 3
        if t179 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t180 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t181 int32 = t180 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t181)
            continue
        } else {
            break Loop_loop177
        }
    }
    var t173 string = _goml_m_inherent_i_int32_i_int32_i_to__string(vlen__49)
    var t174 string = "len:" + t173
    println__T_string(t174)
    var t175 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t176 string = "delta:" + t175
    println__T_string(t176)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv183 string
    var t184 string = _goml_runtime_core_int32_to_string(self__2)
    retv183 = t184
    return retv183
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv186 int32
    var t187 int32 = ref_get__Ref_5int32(self__201)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv191 *ref_int32_x
    var t192 *ref_int32_x = ref__Ref_5int32(value__200)
    retv191 = t192
    return retv191
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv194 *_goml_vec_Dyn_Display
    var t195 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__120 *_goml_vec_Dyn_Display, elem__121 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__131 *_goml_vec_Dyn_Display) int32 {
    var retv199 int32
    var t200 int32 = vec_len__Vec_11Dyn_Display(self__131)
    retv199 = t200
    return retv199
}

func println__T_string(value__1 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv205 string
    retv205 = self__34
    return retv205
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env72 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv213 string
    var t214 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env73 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv216 string
    var tag__29 string = env73.tag_0
    var t217 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv216 = t217
    return retv216
}

func main() {
    main0()
}
