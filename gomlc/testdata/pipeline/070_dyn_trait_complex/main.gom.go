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
    var t426 int32 = self__0.x
    var t427 string
    var inline576 string = _goml_runtime_core_int32_to_string(t426)
    t427 = inline576
    var t428 string = "Point(" + t427
    var t429 string = t428 + ","
    var t430 int32 = self__0.y
    var t431 string
    var inline574 string = _goml_runtime_core_int32_to_string(t430)
    t431 = inline574
    var t432 string = t429 + t431
    var t433 string = t432 + ")"
    return t433
}

func _goml_m_trait__impl_i_Display_i_Point_i_show__with(self__1 Point, prefix__2 string, suffix__3 string) string {
    var t436 string = prefix__2 + "Point("
    var t437 int32 = self__1.x
    var t438 string
    var inline580 string = _goml_runtime_core_int32_to_string(t437)
    t438 = inline580
    var t439 string = t436 + t438
    var t440 string = t439 + ","
    var t441 int32 = self__1.y
    var t442 string
    var inline578 string = _goml_runtime_core_int32_to_string(t441)
    t442 = inline578
    var t443 string = t440 + t442
    var t444 string = t443 + ")"
    var t445 string = t444 + suffix__3
    return t445
}

func _goml_m_trait__impl_i_Display_i_Point_i_tick(self__4 Point) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Point_i_bump(self__5 Point, delta__6 int32) int32 {
    var t449 int32 = self__5.x
    var t450 int32 = self__5.y
    var t451 int32 = t449 + t450
    var t452 int32 = t451 + delta__6
    return t452
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show(self__7 Flag) string {
    var t457 bool = self__7.value
    if t457 {
        return "Flag(true)"
    } else {
        return "Flag(false)"
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_show__with(self__8 Flag, prefix__9 string, suffix__10 string) string {
    var t462 bool = self__8.value
    if t462 {
        var t463 string = prefix__9 + "Flag(true)"
        var t464 string = t463 + suffix__10
        return t464
    } else {
        var t465 string = prefix__9 + "Flag(false)"
        var t466 string = t465 + suffix__10
        return t466
    }
}

func _goml_m_trait__impl_i_Display_i_Flag_i_tick(self__11 Flag) struct{} {
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Flag_i_bump(self__12 Flag, delta__13 int32) int32 {
    var t472 bool = self__12.value
    if t472 {
        return delta__13
    } else {
        var t473 int32 = -delta__13
        return t473
    }
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show(self__14 Counter) string {
    var t476 *ref_int32_x = self__14.cell
    var t477 int32
    var inline584 int32 = ref_get__Ref_5int32(t476)
    t477 = inline584
    var t478 string
    var inline582 string = _goml_runtime_core_int32_to_string(t477)
    t478 = inline582
    var t479 string = "Counter(" + t478
    var t480 string = t479 + ")"
    return t480
}

func _goml_m_trait__impl_i_Display_i_Counter_i_show__with(self__15 Counter, prefix__16 string, suffix__17 string) string {
    var t483 string = prefix__16 + "Counter("
    var t484 *ref_int32_x = self__15.cell
    var t485 int32
    var inline588 int32 = ref_get__Ref_5int32(t484)
    t485 = inline588
    var t486 string
    var inline586 string = _goml_runtime_core_int32_to_string(t485)
    t486 = inline586
    var t487 string = t483 + t486
    var t488 string = t487 + ")"
    var t489 string = t488 + suffix__17
    return t489
}

func _goml_m_trait__impl_i_Display_i_Counter_i_tick(self__18 Counter) struct{} {
    var t491 *ref_int32_x = self__18.cell
    var t492 int32
    var inline592 int32 = ref_get__Ref_5int32(t491)
    t492 = inline592
    var next__19 int32 = t492 + 1
    var t493 *ref_int32_x = self__18.cell
    ref_set__Ref_5int32(t493, next__19)
    return struct{}{}
}

func _goml_m_trait__impl_i_Display_i_Counter_i_bump(self__20 Counter, delta__21 int32) int32 {
    var t496 *ref_int32_x = self__20.cell
    var t497 int32
    var inline596 int32 = ref_get__Ref_5int32(t496)
    t497 = inline596
    var next__22 int32 = t497 + delta__21
    var t498 *ref_int32_x = self__20.cell
    ref_set__Ref_5int32(t498, next__22)
    return next__22
}

func show_dyn(x__23 dyn__Display) string {
    var t501 string = x__23.vtable.show_with(x__23.data, "<", ">")
    return t501
}

func call_via_closure(x__24 dyn__Display, tag__25 string) string {
    var t504 closure_env_f_0 = closure_env_f_0{}
    var f__28 func(dyn__Display, string) string = func(p0 dyn__Display, p1 string) string {
        return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(t504, p0, p1)
    }
    var t505 string = f__28(x__24, tag__25)
    return t505
}

func make_renderer(tag__29 string) func(dyn__Display) string {
    var t508 closure_env_make_renderer_1 = closure_env_make_renderer_1{
        tag_0: tag__29,
    }
    var t509 func(dyn__Display) string = func(p0 dyn__Display) string {
        return _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(t508, p0)
    }
    return t509
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
    var t518 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(10)
    var c__37 Counter = Counter{
        cell: t518,
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
    var t519 string = render_star__43(dp1__38)
    var t520 string = t519 + "|"
    var t521 string = render_angle__44(df1__40)
    var s2__47 string = t520 + t521
    var v__48 *_goml_vec_Dyn_Display
    var inline646 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__48 = inline646
    vec_push__Vec_11Dyn_Display(v__48, dp1__38)
    vec_push__Vec_11Dyn_Display(v__48, df1__40)
    vec_push__Vec_11Dyn_Display(v__48, dc__42)
    var vlen__49 int
    var inline638 int = vec_len__Vec_11Dyn_Display(v__48)
    vlen__49 = inline638
    var jp523 int32
    switch vlen__49 {
    case 2:
        jp523 = 3
    default:
        jp523 = 5
    }
    var inline635 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s0__45)
    _goml_runtime_core_string_println(inline635)
    var inline632 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s1__46)
    _goml_runtime_core_string_println(inline632)
    var inline629 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(s2__47)
    _goml_runtime_core_string_println(inline629)
    var i__51 *ref_int32_x
    var inline626 int32 = 0
    var inline627 *ref_int32_x = ref__Ref_5int32(inline626)
    i__51 = inline627
    Loop_loop529:
    for {
        var t530 int32
        var inline614 int32 = ref_get__Ref_5int32(i__51)
        t530 = inline614
        var t531 bool = t530 < 3
        if t531 {
            var line__52 string
            dc__42.vtable.tick(dc__42.data)
            var inline608 string = dc__42.vtable.show_with(dc__42.data, "[", "]")
            var inline609 string = inline608 + ":"
            var inline610 int32 = dc__42.vtable.bump(dc__42.data, jp523)
            var inline611 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline610)
            var inline612 string = inline609 + inline611
            line__52 = inline612
            var inline604 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(line__52)
            _goml_runtime_core_string_println(inline604)
            var t532 int32
            var inline602 int32 = ref_get__Ref_5int32(i__51)
            t532 = inline602
            var t533 int32 = t532 + 1
            ref_set__Ref_5int32(i__51, t533)
            continue
        } else {
            break Loop_loop529
        }
    }
    var t525 string
    var inline624 string = _goml_runtime_core_int_to_string(vlen__49)
    t525 = inline624
    var t526 string = "len:" + t525
    var inline621 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t526)
    _goml_runtime_core_string_println(inline621)
    var t527 string
    var inline619 string = _goml_runtime_core_int32_to_string(jp523)
    t527 = inline619
    var t528 string = "delta:" + t527
    var inline616 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t528)
    _goml_runtime_core_string_println(inline616)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t536 string = _goml_runtime_core_int32_to_string(self__33)
    return t536
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__431 int32) *ref_int32_x {
    var t544 *ref_int32_x = ref__Ref_5int32(value__431)
    return t544
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env422 closure_env_f_0, v__26 dyn__Display, t__27 string) string {
    var t569 string = v__26.vtable.show_with(v__26.data, t__27, t__27)
    return t569
}

func _goml_m_inherent_i_closure__en_h5c3741356b48d7360bc79df27842b70e_erer__1_i_apply(env423 closure_env_make_renderer_1, x__30 dyn__Display) string {
    var tag__29 string = env423.tag_0
    var t572 string = x__30.vtable.show_with(x__30.data, tag__29, tag__29)
    return t572
}

func main() {
    main0()
}
