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
    var retv21 string
    var t22 int32 = self__0.x
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t22)
    var t24 string = "Point(" + t23
    var t25 string = t24 + ","
    var t26 int32 = self__0.y
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    var t28 string = t25 + t27
    var t29 string = t28 + ")"
    retv21 = t29
    return retv21
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var retv31 string
    var t32 string = prefix__2 + "Point("
    var t33 int32 = self__1.x
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t33)
    var t35 string = t32 + t34
    var t36 string = t35 + ","
    var t37 int32 = self__1.y
    var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t37)
    var t39 string = t36 + t38
    var t40 string = t39 + ")"
    var t41 string = t40 + suffix__3
    retv31 = t41
    return retv31
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var retv44 int32
    var t45 int32 = self__5.x
    var t46 int32 = self__5.y
    var t47 int32 = t45 + t46
    var t48 int32 = t47 + delta__6
    retv44 = t48
    return retv44
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var retv50 string
    var t53 bool = self__7.value
    var jp52 string
    if t53 {
        jp52 = "Flag(true)"
    } else {
        jp52 = "Flag(false)"
    }
    retv50 = jp52
    return retv50
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var retv55 string
    var t58 bool = self__8.value
    var jp57 string
    if t58 {
        var t59 string = prefix__9 + "Flag(true)"
        var t60 string = t59 + suffix__10
        jp57 = t60
    } else {
        var t61 string = prefix__9 + "Flag(false)"
        var t62 string = t61 + suffix__10
        jp57 = t62
    }
    retv55 = jp57
    return retv55
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var retv65 int32
    var t68 bool = self__12.value
    var jp67 int32
    if t68 {
        jp67 = delta__13
    } else {
        var t69 int32 = -delta__13
        jp67 = t69
    }
    retv65 = jp67
    return retv65
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var retv71 string
    var t72 *ref_int32_x = self__14.cell
    var t73 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t72)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    var t75 string = "Counter(" + t74
    var t76 string = t75 + ")"
    retv71 = t76
    return retv71
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var retv78 string
    var t79 string = prefix__16 + "Counter("
    var t80 *ref_int32_x = self__15.cell
    var t81 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t80)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    var t83 string = t79 + t82
    var t84 string = t83 + ")"
    var t85 string = t84 + suffix__17
    retv78 = t85
    return retv78
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t87 *ref_int32_x = self__18.cell
    var t88 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t87)
    var next__19 int32 = t88 + 1
    var t89 *ref_int32_x = self__18.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t89, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var retv91 int32
    var t92 *ref_int32_x = self__20.cell
    var t93 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t92)
    var next__22 int32 = t93 + delta__21
    var t94 *ref_int32_x = self__20.cell
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t94, next__22)
    retv91 = next__22
    return retv91
}

func show_dyn(x__23 dyn__Display) string {
    var retv96 string
    var t97 string = x__23.vtable.show_with(x__23.data, "<", ">")
    retv96 = t97
    return retv96
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var retv99 string
    var f__28 closure_env_f_0 = closure_env_f_0{}
    var t100 string = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(f__28, x__24, tag__25)
    retv99 = t100
    return retv99
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var retv102 func(dyn__Display) string
    var t103 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    retv102 = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t103, p0)
    }
    return retv102
}

func bump_and_show(x__31 dyn__Display, delta__32 int32) string {
    var retv105 string
    x__31.vtable.tick(x__31.data)
    var t106 string = x__31.vtable.show_with(x__31.data, "[", "]")
    var t107 string = t106 + ":"
    var t108 int32 = x__31.vtable.bump(x__31.data, delta__32)
    var t109 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t108)
    var t110 string = t107 + t109
    retv105 = t110
    return retv105
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
    var t112 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t112,
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
    var t113 string = render_star__43(dp1__38)
    var t114 string = t113 + "|"
    var t115 string = render_angle__44(df1__40)
    var s2__47 string = t114 + t115
    var v__48 *_goml_vec_Dyn_Display = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dp1__38)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, df1__40)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(v__48, dc__42)
    var vlen__49 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(v__48)
    var jp117 int32
    switch vlen__49 {
    case 2:
        jp117 = 3
    default:
        jp117 = 5
    }
    var delta__50 int32 = jp117
    println__T_string(s0__45)
    println__T_string(s1__46)
    println__T_string(s2__47)
    var i__51 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop123:
    for {
        var t124 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
        var t125 bool = t124 < 3
        if t125 {
            var line__52 string = bump_and_show(dc__42, delta__50)
            println__T_string(line__52)
            var t126 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__51)
            var t127 int32 = t126 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__51, t127)
            continue
        } else {
            break Loop_loop123
        }
    }
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(vlen__49)
    var t120 string = "len:" + t119
    println__T_string(t120)
    var t121 string = _goml_m_inherent_i_int32_i_int32_i_to__string(delta__50)
    var t122 string = "delta:" + t121
    println__T_string(t122)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int32_to_string(self__2)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv132 int32
    var t133 int32 = ref_get__Ref_5int32(self__103)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv137 *ref_int32_x
    var t138 *ref_int32_x = ref__Ref_5int32(value__102)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__dynDisplay() *_goml_vec_Dyn_Display {
    var retv140 *_goml_vec_Dyn_Display
    var t141 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__dynDisplay(self__71 *_goml_vec_Dyn_Display, elem__72 dyn__Display) struct{} {
    vec_push__Vec_11Dyn_Display(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__dynDisplay(self__82 *_goml_vec_Dyn_Display) int32 {
    var retv145 int32
    var t146 int32 = vec_len__Vec_11Dyn_Display(self__82)
    retv145 = t146
    return retv145
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv151 string
    retv151 = self__9
    return retv151
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env18 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var retv159 string
    var t160 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env19 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var retv162 string
    var tag__29 string = env19.tag_0
    var t163 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    retv162 = t163
    return retv162
}

func main() {
    main0()
}
