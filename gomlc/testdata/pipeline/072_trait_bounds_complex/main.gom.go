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

type Boxed struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var inline584 string = _goml_runtime_core_int32_to_string(self__0)
    return inline584
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t420 string
    var inline586 string = _goml_runtime_core_int32_to_string(self__1)
    t420 = inline586
    var t421 string = "i32(" + t420
    var t422 string = t421 + ")"
    return t422
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t428 int32 = self__4 * 16777619
    var t429 int32 = t428 + 216613626
    return t429
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t432 int32 = self__5 + other__6
    return t432
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t438 string
    var inline588 string = _goml_runtime_core_int32_to_string(self__9)
    t438 = inline588
    var t439 string = "<" + t438
    var t440 string = t439 + ">"
    return t440
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t443 int32 = self__10.value
    var t444 string
    var inline590 string = _goml_runtime_core_int32_to_string(t443)
    t444 = inline590
    var t445 string = "Boxed(" + t444
    var t446 string = t445 + ")"
    return t446
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t449 int32 = self__11.value
    var t450 string
    var inline592 string = _goml_runtime_core_int32_to_string(t449)
    t450 = inline592
    var t451 string = "Boxed{value=" + t450
    var t452 string = t451 + "}"
    return t452
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t460 int32 = self__14.value
    var t461 int32 = t460 * 31
    var t462 int32 = t461 + 7
    var t463 int32 = t462 * 1315423911
    return t463
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t466 int32 = self__15.value
    var t467 int32 = other__16.value
    var t468 int32 = t466 + t467
    var t469 Boxed = Boxed{
        value: t468,
    }
    return t469
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t477 int32 = self__19.value
    var t478 string
    var inline594 string = _goml_runtime_core_int32_to_string(t477)
    t478 = inline594
    var t479 string = "[" + t478
    var t480 string = t479 + "]"
    return t480
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t486 string
    var inline633 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline634 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline633)
    t486 = inline634
    var inline630 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t486)
    _goml_runtime_core_string_println(inline630)
    var t487 Boxed = Boxed{
        value: 99,
    }
    var t488 Boxed = Boxed{
        value: 3,
    }
    var t489 Boxed = Boxed{
        value: 4,
    }
    var t490 string
    var inline627 Boxed = combine_scaled__T_Boxed(t488, t489, 2)
    var inline628 string = report_pair__Q_Boxed__T_Boxed(t487, t488, t489, inline627)
    t490 = inline628
    var inline624 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t490)
    _goml_runtime_core_string_println(inline624)
    var t491 string
    var inline613 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline614 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline613, third__51)
    var inline615 string = tag_text__Q_int32(sum_tag__48)
    var inline616 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline614)
    var inline617 string = inline615 + " "
    var inline618 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline614)
    var inline619 string = inline617 + inline618
    var inline620 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline616)
    var inline621 string = " @" + inline620
    var inline622 string = inline619 + inline621
    t491 = inline622
    var inline610 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t491)
    _goml_runtime_core_string_println(inline610)
    var t492 Boxed = Boxed{
        value: 1,
    }
    var t493 Boxed = Boxed{
        value: 5,
    }
    var t494 Boxed = Boxed{
        value: 6,
    }
    var t495 Boxed = Boxed{
        value: 7,
    }
    var t496 string
    var inline599 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t493, t494)
    var inline600 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline599, t495)
    var inline601 string = tag_text__Q_Boxed(t492)
    var inline602 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline600)
    var inline603 string = inline601 + " "
    var inline604 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline600)
    var inline605 string = inline603 + inline604
    var inline606 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline602)
    var inline607 string = " @" + inline606
    var inline608 string = inline605 + inline607
    t496 = inline608
    var inline596 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t496)
    _goml_runtime_core_string_println(inline596)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t499 string = _goml_runtime_core_int32_to_string(self__33)
    return t499
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t531 int32
    var inline720 int32 = a__23 + b__24
    t531 = inline720
    var inline718 int32 = t531 * factor__25
    return inline718
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline739 bool = a__27 == b__28
    same__30 = inline739
    var header__31 string
    var inline733 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline734 string = inline733 + "#"
    var inline735 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline736 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline735)
    var inline737 string = inline734 + inline736
    header__31 = inline737
    var repr__32 string
    var inline728 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline729 string = inline728 + " / "
    var inline730 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline731 string = inline729 + inline730
    repr__32 = inline731
    var h__33 int32
    var inline725 int32 = combined__29 * 16777619
    var inline726 int32 = inline725 + 216613626
    h__33 = inline726
    var t535 string = header__31 + " "
    var t536 string = t535 + repr__32
    var t537 string
    if same__30 {
        t537 = "true"
    } else {
        t537 = "false"
    }
    var t538 string = " | eq=" + t537
    var t539 string
    var inline722 string = _goml_runtime_core_int32_to_string(h__33)
    t539 = inline722
    var t540 string = " | hash=" + t539
    var t541 string = t538 + t540
    var t542 string = t536 + t541
    return t542
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t545 Boxed
    var inline745 int32 = a__23.value
    var inline746 int32 = b__24.value
    var inline747 int32 = inline745 + inline746
    var inline748 Boxed = Boxed{
        value: inline747,
    }
    t545 = inline748
    var inline741 int32 = t545.value
    var inline742 int32 = inline741 * factor__25
    var inline743 Boxed = Boxed{
        value: inline742,
    }
    return inline743
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline769 int32 = a__27.value
    var inline770 int32 = b__28.value
    var inline771 bool = inline769 == inline770
    same__30 = inline771
    var header__31 string
    var inline763 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline764 string = inline763 + "#"
    var inline765 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline766 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline765)
    var inline767 string = inline764 + inline766
    header__31 = inline767
    var repr__32 string
    var inline758 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline759 string = inline758 + " / "
    var inline760 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline761 string = inline759 + inline760
    repr__32 = inline761
    var h__33 int32
    var inline753 int32 = combined__29.value
    var inline754 int32 = inline753 * 31
    var inline755 int32 = inline754 + 7
    var inline756 int32 = inline755 * 1315423911
    h__33 = inline756
    var t549 string = header__31 + " "
    var t550 string = t549 + repr__32
    var t551 string
    if same__30 {
        t551 = "true"
    } else {
        t551 = "false"
    }
    var t552 string = " | eq=" + t551
    var t553 string
    var inline750 string = _goml_runtime_core_int32_to_string(h__33)
    t553 = inline750
    var t554 string = " | hash=" + t553
    var t555 string = t552 + t554
    var t556 string = t550 + t555
    return t556
}

func tag_text__Q_int32(tag__22 int32) string {
    var t559 string
    var inline778 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline779 string = "i32(" + inline778
    var inline780 string = inline779 + ")"
    t559 = inline780
    var t560 string = t559 + "#"
    var t561 int32
    var inline775 int32 = tag__22 * 16777619
    var inline776 int32 = inline775 + 216613626
    t561 = inline776
    var t562 string
    var inline773 string = _goml_runtime_core_int32_to_string(t561)
    t562 = inline773
    var t563 string = t560 + t562
    return t563
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t566 string
    var inline789 int32 = tag__22.value
    var inline790 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline789)
    var inline791 string = "Boxed{value=" + inline790
    var inline792 string = inline791 + "}"
    t566 = inline792
    var t567 string = t566 + "#"
    var t568 int32
    var inline784 int32 = tag__22.value
    var inline785 int32 = inline784 * 31
    var inline786 int32 = inline785 + 7
    var inline787 int32 = inline786 * 1315423911
    t568 = inline787
    var t569 string
    var inline782 string = _goml_runtime_core_int32_to_string(t568)
    t569 = inline782
    var t570 string = t567 + t569
    return t570
}

func main() {
    main0()
}
