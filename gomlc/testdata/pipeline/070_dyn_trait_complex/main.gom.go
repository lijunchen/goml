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

type Ordering int32

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
    var t429 int32 = self__0.x
    var t430 string
    var inline579 string = _goml_runtime_core_int32_to_string(t429)
    t430 = inline579
    var t431 string = "Point(" + t430
    var t432 string = t431 + ","
    var t433 int32 = self__0.y
    var t434 string
    var inline577 string = _goml_runtime_core_int32_to_string(t433)
    t434 = inline577
    var t435 string = t432 + t434
    var t436 string = t435 + ")"
    return t436
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t439 string = prefix__2 + "Point("
    var t440 int32 = self__1.x
    var t441 string
    var inline583 string = _goml_runtime_core_int32_to_string(t440)
    t441 = inline583
    var t442 string = t439 + t441
    var t443 string = t442 + ","
    var t444 int32 = self__1.y
    var t445 string
    var inline581 string = _goml_runtime_core_int32_to_string(t444)
    t445 = inline581
    var t446 string = t443 + t445
    var t447 string = t446 + ")"
    var t448 string = t447 + suffix__3
    return t448
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t452 int32 = self__5.x
    var t453 int32 = self__5.y
    var t454 int32 = t452 + t453
    var t455 int32 = t454 + delta__6
    return t455
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t460 bool = self__7.value
    if t460 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t465 bool = self__8.value
    if t465 {
        var t466 string = prefix__9 + "Flag(true)"
        var t467 string = t466 + suffix__10
        return t467
    } else {
        var t468 string = prefix__9 + "Flag(false)"
        var t469 string = t468 + suffix__10
        return t469
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t475 bool = self__12.value
    if t475 {
        return delta__13
    } else {
        var t476 int32 = -delta__13
        return t476
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t479 *ref_int32_x = self__14.cell
    var t480 int32
    var inline587 int32 = ref_get__Ref_5int32(t479)
    t480 = inline587
    var t481 string
    var inline585 string = _goml_runtime_core_int32_to_string(t480)
    t481 = inline585
    var t482 string = "Counter(" + t481
    var t483 string = t482 + ")"
    return t483
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t486 string = prefix__16 + "Counter("
    var t487 *ref_int32_x = self__15.cell
    var t488 int32
    var inline591 int32 = ref_get__Ref_5int32(t487)
    t488 = inline591
    var t489 string
    var inline589 string = _goml_runtime_core_int32_to_string(t488)
    t489 = inline589
    var t490 string = t486 + t489
    var t491 string = t490 + ")"
    var t492 string = t491 + suffix__17
    return t492
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t494 *ref_int32_x = self__18.cell
    var t495 int32
    var inline595 int32 = ref_get__Ref_5int32(t494)
    t495 = inline595
    var next__19 int32 = t495 + 1
    var t496 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t496, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t499 *ref_int32_x = self__20.cell
    var t500 int32
    var inline599 int32 = ref_get__Ref_5int32(t499)
    t500 = inline599
    var next__22 int32 = t500 + delta__21
    var t501 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t501, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t504 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t504
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t507 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t507, p0, p1)
    }
    var t508 string = f__28(x__24, tag__25)
    return t508
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t511 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t512 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t511, p0)
    }
    return t512
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
    var t521 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t521,
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
    var t522 string = render_star__43(dp1__38)
    var t523 string = t522 + "|"
    var t524 string = render_angle__44(df1__40)
    var s2__47 string = t523 + t524
    var v__48 *_goml_vec_Dyn_Display
    var inline649 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline649
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline641 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline641
    var jp526 int32
    switch vlen__49 {
    case 2:
        jp526 = 3
    default:
        jp526 = 5
    }
    var inline638 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline638)
    var inline635 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline635)
    var inline632 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline632)
    var i__51 *ref_int32_x
    var inline629 int32 = 0
    var inline630 *ref_int32_x = ref__Ref_5int32(inline629)
    i__51 = inline630
    Loop_loop532:
    for {
        var t533 int32
        var inline617 int32 = ref_get__Ref_5int32(i__51)
        t533 = inline617
        var t534 bool = t533 < 3
        if t534 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline611 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline612 string = inline611 + ":"
            var inline613 int32 = dc__42.vtable.bump(dc__42.data, jp526)
            var inline614 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline613)
            var inline615 string = inline612 + inline614
            line__52 = inline615
            var inline607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline607)
            var t535 int32
            var inline605 int32 = ref_get__Ref_5int32(i__51)
            t535 = inline605
            var t536 int32 = t535 + 1
            ref_set__Ref_5int32(i__51, t536)
            continue
        } else {
            break Loop_loop532
        }
    }
    var t528 string
    var inline627 string = _goml_runtime_core_int_to_string(vlen__49)
    t528 = inline627
    var t529 string = "len:" + t528
    var inline624 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t529)
    _goml_runtime_core_string_println(inline624)
    var t530 string
    var inline622 string = _goml_runtime_core_int32_to_string(jp526)
    t530 = inline622
    var t531 string = "delta:" + t530
    var inline619 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t531)
    _goml_runtime_core_string_println(inline619)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t539 string = _goml_runtime_core_int32_to_string(self__33)
    return t539
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t547 *ref_int32_x = ref__Ref_5int32(value__431)
    return t547
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env425 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t572 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t572
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env426 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env426.tag_0
    var t575 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t575
}

func main() {
    main0()
}
