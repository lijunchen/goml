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
    var inline581 string = _goml_runtime_core_int32_to_string(self__0)
    return inline581
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t417 string
    var inline583 string = _goml_runtime_core_int32_to_string(self__1)
    t417 = inline583
    var t418 string = "i32(" + t417
    var t419 string = t418 + ")"
    return t419
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t425 int32 = self__4 * 16777619
    var t426 int32 = t425 + 216613626
    return t426
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t429 int32 = self__5 + other__6
    return t429
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t435 string
    var inline585 string = _goml_runtime_core_int32_to_string(self__9)
    t435 = inline585
    var t436 string = "<" + t435
    var t437 string = t436 + ">"
    return t437
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t440 int32 = self__10.value
    var t441 string
    var inline587 string = _goml_runtime_core_int32_to_string(t440)
    t441 = inline587
    var t442 string = "Boxed(" + t441
    var t443 string = t442 + ")"
    return t443
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t446 int32 = self__11.value
    var t447 string
    var inline589 string = _goml_runtime_core_int32_to_string(t446)
    t447 = inline589
    var t448 string = "Boxed{value=" + t447
    var t449 string = t448 + "}"
    return t449
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t457 int32 = self__14.value
    var t458 int32 = t457 * 31
    var t459 int32 = t458 + 7
    var t460 int32 = t459 * 1315423911
    return t460
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t463 int32 = self__15.value
    var t464 int32 = other__16.value
    var t465 int32 = t463 + t464
    var t466 Boxed = Boxed{
        value: t465,
    }
    return t466
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t474 int32 = self__19.value
    var t475 string
    var inline591 string = _goml_runtime_core_int32_to_string(t474)
    t475 = inline591
    var t476 string = "[" + t475
    var t477 string = t476 + "]"
    return t477
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t483 string
    var inline630 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline631 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline630)
    t483 = inline631
    var inline627 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t483)
    _goml_runtime_core_string_println(inline627)
    var t484 Boxed = Boxed{
        value: 99,
    }
    var t485 Boxed = Boxed{
        value: 3,
    }
    var t486 Boxed = Boxed{
        value: 4,
    }
    var t487 string
    var inline624 Boxed = combine_scaled__T_Boxed(t485, t486, 2)
    var inline625 string = report_pair__Q_Boxed__T_Boxed(t484, t485, t486, inline624)
    t487 = inline625
    var inline621 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t487)
    _goml_runtime_core_string_println(inline621)
    var t488 string
    var inline610 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline611 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline610, third__51)
    var inline612 string = tag_text__Q_int32(sum_tag__48)
    var inline613 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline611)
    var inline614 string = inline612 + " "
    var inline615 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline611)
    var inline616 string = inline614 + inline615
    var inline617 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline613)
    var inline618 string = " @" + inline617
    var inline619 string = inline616 + inline618
    t488 = inline619
    var inline607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t488)
    _goml_runtime_core_string_println(inline607)
    var t489 Boxed = Boxed{
        value: 1,
    }
    var t490 Boxed = Boxed{
        value: 5,
    }
    var t491 Boxed = Boxed{
        value: 6,
    }
    var t492 Boxed = Boxed{
        value: 7,
    }
    var t493 string
    var inline596 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t490, t491)
    var inline597 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline596, t492)
    var inline598 string = tag_text__Q_Boxed(t489)
    var inline599 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline597)
    var inline600 string = inline598 + " "
    var inline601 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline597)
    var inline602 string = inline600 + inline601
    var inline603 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline599)
    var inline604 string = " @" + inline603
    var inline605 string = inline602 + inline604
    t493 = inline605
    var inline593 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t493)
    _goml_runtime_core_string_println(inline593)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t496 string = _goml_runtime_core_int32_to_string(self__33)
    return t496
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t528 int32
    var inline717 int32 = a__23 + b__24
    t528 = inline717
    var inline715 int32 = t528 * factor__25
    return inline715
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline736 bool = a__27 == b__28
    same__30 = inline736
    var header__31 string
    var inline730 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline731 string = inline730 + "#"
    var inline732 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline733 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline732)
    var inline734 string = inline731 + inline733
    header__31 = inline734
    var repr__32 string
    var inline725 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline726 string = inline725 + " / "
    var inline727 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline728 string = inline726 + inline727
    repr__32 = inline728
    var h__33 int32
    var inline722 int32 = combined__29 * 16777619
    var inline723 int32 = inline722 + 216613626
    h__33 = inline723
    var t532 string = header__31 + " "
    var t533 string = t532 + repr__32
    var t534 string
    if same__30 {
        t534 = "true"
    } else {
        t534 = "false"
    }
    var t535 string = " | eq=" + t534
    var t536 string
    var inline719 string = _goml_runtime_core_int32_to_string(h__33)
    t536 = inline719
    var t537 string = " | hash=" + t536
    var t538 string = t535 + t537
    var t539 string = t533 + t538
    return t539
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t542 Boxed
    var inline742 int32 = a__23.value
    var inline743 int32 = b__24.value
    var inline744 int32 = inline742 + inline743
    var inline745 Boxed = Boxed{
        value: inline744,
    }
    t542 = inline745
    var inline738 int32 = t542.value
    var inline739 int32 = inline738 * factor__25
    var inline740 Boxed = Boxed{
        value: inline739,
    }
    return inline740
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline766 int32 = a__27.value
    var inline767 int32 = b__28.value
    var inline768 bool = inline766 == inline767
    same__30 = inline768
    var header__31 string
    var inline760 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline761 string = inline760 + "#"
    var inline762 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline763 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline762)
    var inline764 string = inline761 + inline763
    header__31 = inline764
    var repr__32 string
    var inline755 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline756 string = inline755 + " / "
    var inline757 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline758 string = inline756 + inline757
    repr__32 = inline758
    var h__33 int32
    var inline750 int32 = combined__29.value
    var inline751 int32 = inline750 * 31
    var inline752 int32 = inline751 + 7
    var inline753 int32 = inline752 * 1315423911
    h__33 = inline753
    var t546 string = header__31 + " "
    var t547 string = t546 + repr__32
    var t548 string
    if same__30 {
        t548 = "true"
    } else {
        t548 = "false"
    }
    var t549 string = " | eq=" + t548
    var t550 string
    var inline747 string = _goml_runtime_core_int32_to_string(h__33)
    t550 = inline747
    var t551 string = " | hash=" + t550
    var t552 string = t549 + t551
    var t553 string = t547 + t552
    return t553
}

func tag_text__Q_int32(tag__22 int32) string {
    var t556 string
    var inline775 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline776 string = "i32(" + inline775
    var inline777 string = inline776 + ")"
    t556 = inline777
    var t557 string = t556 + "#"
    var t558 int32
    var inline772 int32 = tag__22 * 16777619
    var inline773 int32 = inline772 + 216613626
    t558 = inline773
    var t559 string
    var inline770 string = _goml_runtime_core_int32_to_string(t558)
    t559 = inline770
    var t560 string = t557 + t559
    return t560
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t563 string
    var inline786 int32 = tag__22.value
    var inline787 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline786)
    var inline788 string = "Boxed{value=" + inline787
    var inline789 string = inline788 + "}"
    t563 = inline789
    var t564 string = t563 + "#"
    var t565 int32
    var inline781 int32 = tag__22.value
    var inline782 int32 = inline781 * 31
    var inline783 int32 = inline782 + 7
    var inline784 int32 = inline783 * 1315423911
    t565 = inline784
    var t566 string
    var inline779 string = _goml_runtime_core_int32_to_string(t565)
    t566 = inline779
    var t567 string = t564 + t566
    return t567
}

func main() {
    main0()
}
