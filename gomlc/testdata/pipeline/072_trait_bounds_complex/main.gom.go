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

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var inline355 string = _goml_runtime_core_int32_to_string(self__0)
    return inline355
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t191 string
    var inline357 string = _goml_runtime_core_int32_to_string(self__1)
    t191 = inline357
    var t192 string = "i32(" + t191
    var t193 string = t192 + ")"
    return t193
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t199 int32 = self__4 * 16777619
    var t200 int32 = t199 + 216613626
    return t200
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t203 int32 = self__5 + other__6
    return t203
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t209 string
    var inline359 string = _goml_runtime_core_int32_to_string(self__9)
    t209 = inline359
    var t210 string = "<" + t209
    var t211 string = t210 + ">"
    return t211
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t214 int32 = self__10.value
    var t215 string
    var inline361 string = _goml_runtime_core_int32_to_string(t214)
    t215 = inline361
    var t216 string = "Boxed(" + t215
    var t217 string = t216 + ")"
    return t217
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t220 int32 = self__11.value
    var t221 string
    var inline363 string = _goml_runtime_core_int32_to_string(t220)
    t221 = inline363
    var t222 string = "Boxed{value=" + t221
    var t223 string = t222 + "}"
    return t223
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t231 int32 = self__14.value
    var t232 int32 = t231 * 31
    var t233 int32 = t232 + 7
    var t234 int32 = t233 * 1315423911
    return t234
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t237 int32 = self__15.value
    var t238 int32 = other__16.value
    var t239 int32 = t237 + t238
    var t240 Boxed = Boxed{
        value: t239,
    }
    return t240
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t248 int32 = self__19.value
    var t249 string
    var inline365 string = _goml_runtime_core_int32_to_string(t248)
    t249 = inline365
    var t250 string = "[" + t249
    var t251 string = t250 + "]"
    return t251
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t257 string
    var inline404 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline405 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline404)
    t257 = inline405
    var inline401 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline401)
    var t258 Boxed = Boxed{
        value: 99,
    }
    var t259 Boxed = Boxed{
        value: 3,
    }
    var t260 Boxed = Boxed{
        value: 4,
    }
    var t261 string
    var inline398 Boxed = combine_scaled__T_Boxed(t259, t260, 2)
    var inline399 string = report_pair__Q_Boxed__T_Boxed(t258, t259, t260, inline398)
    t261 = inline399
    var inline395 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t261)
    _goml_runtime_core_string_println(inline395)
    var t262 string
    var inline384 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline385 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline384, third__51)
    var inline386 string = tag_text__Q_int32(sum_tag__48)
    var inline387 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline385)
    var inline388 string = inline386 + " "
    var inline389 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline385)
    var inline390 string = inline388 + inline389
    var inline391 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline387)
    var inline392 string = " @" + inline391
    var inline393 string = inline390 + inline392
    t262 = inline393
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline381)
    var t263 Boxed = Boxed{
        value: 1,
    }
    var t264 Boxed = Boxed{
        value: 5,
    }
    var t265 Boxed = Boxed{
        value: 6,
    }
    var t266 Boxed = Boxed{
        value: 7,
    }
    var t267 string
    var inline370 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t264, t265)
    var inline371 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline370, t266)
    var inline372 string = tag_text__Q_Boxed(t263)
    var inline373 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline371)
    var inline374 string = inline372 + " "
    var inline375 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline371)
    var inline376 string = inline374 + inline375
    var inline377 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline373)
    var inline378 string = " @" + inline377
    var inline379 string = inline376 + inline378
    t267 = inline379
    var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t267)
    _goml_runtime_core_string_println(inline367)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t270 string = _goml_runtime_core_int32_to_string(self__33)
    return t270
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t302 int32
    var inline491 int32 = a__23 + b__24
    t302 = inline491
    var inline489 int32 = t302 * factor__25
    return inline489
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline510 bool = a__27 == b__28
    same__30 = inline510
    var header__31 string
    var inline504 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline505 string = inline504 + "#"
    var inline506 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline507 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline506)
    var inline508 string = inline505 + inline507
    header__31 = inline508
    var repr__32 string
    var inline499 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline500 string = inline499 + " / "
    var inline501 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline502 string = inline500 + inline501
    repr__32 = inline502
    var h__33 int32
    var inline496 int32 = combined__29 * 16777619
    var inline497 int32 = inline496 + 216613626
    h__33 = inline497
    var t306 string = header__31 + " "
    var t307 string = t306 + repr__32
    var t308 string
    if same__30 {
        t308 = "true"
    } else {
        t308 = "false"
    }
    var t309 string = " | eq=" + t308
    var t310 string
    var inline493 string = _goml_runtime_core_int32_to_string(h__33)
    t310 = inline493
    var t311 string = " | hash=" + t310
    var t312 string = t309 + t311
    var t313 string = t307 + t312
    return t313
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t316 Boxed
    var inline516 int32 = a__23.value
    var inline517 int32 = b__24.value
    var inline518 int32 = inline516 + inline517
    var inline519 Boxed = Boxed{
        value: inline518,
    }
    t316 = inline519
    var inline512 int32 = t316.value
    var inline513 int32 = inline512 * factor__25
    var inline514 Boxed = Boxed{
        value: inline513,
    }
    return inline514
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline540 int32 = a__27.value
    var inline541 int32 = b__28.value
    var inline542 bool = inline540 == inline541
    same__30 = inline542
    var header__31 string
    var inline534 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline535 string = inline534 + "#"
    var inline536 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline537 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline536)
    var inline538 string = inline535 + inline537
    header__31 = inline538
    var repr__32 string
    var inline529 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline530 string = inline529 + " / "
    var inline531 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline532 string = inline530 + inline531
    repr__32 = inline532
    var h__33 int32
    var inline524 int32 = combined__29.value
    var inline525 int32 = inline524 * 31
    var inline526 int32 = inline525 + 7
    var inline527 int32 = inline526 * 1315423911
    h__33 = inline527
    var t320 string = header__31 + " "
    var t321 string = t320 + repr__32
    var t322 string
    if same__30 {
        t322 = "true"
    } else {
        t322 = "false"
    }
    var t323 string = " | eq=" + t322
    var t324 string
    var inline521 string = _goml_runtime_core_int32_to_string(h__33)
    t324 = inline521
    var t325 string = " | hash=" + t324
    var t326 string = t323 + t325
    var t327 string = t321 + t326
    return t327
}

func tag_text__Q_int32(tag__22 int32) string {
    var t330 string
    var inline549 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline550 string = "i32(" + inline549
    var inline551 string = inline550 + ")"
    t330 = inline551
    var t331 string = t330 + "#"
    var t332 int32
    var inline546 int32 = tag__22 * 16777619
    var inline547 int32 = inline546 + 216613626
    t332 = inline547
    var t333 string
    var inline544 string = _goml_runtime_core_int32_to_string(t332)
    t333 = inline544
    var t334 string = t331 + t333
    return t334
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t337 string
    var inline560 int32 = tag__22.value
    var inline561 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline560)
    var inline562 string = "Boxed{value=" + inline561
    var inline563 string = inline562 + "}"
    t337 = inline563
    var t338 string = t337 + "#"
    var t339 int32
    var inline555 int32 = tag__22.value
    var inline556 int32 = inline555 * 31
    var inline557 int32 = inline556 + 7
    var inline558 int32 = inline557 * 1315423911
    t339 = inline558
    var t340 string
    var inline553 string = _goml_runtime_core_int32_to_string(t339)
    t340 = inline553
    var t341 string = t338 + t340
    return t341
}

func main() {
    main0()
}
