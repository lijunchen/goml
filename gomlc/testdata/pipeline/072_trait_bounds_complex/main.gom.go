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
    var inline348 string = _goml_runtime_core_int32_to_string(self__0)
    return inline348
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t181 string
    var inline350 string = _goml_runtime_core_int32_to_string(self__1)
    t181 = inline350
    var t182 string = "i32(" + t181
    var t183 string = t182 + ")"
    return t183
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t189 int32 = self__4 * 16777619
    var t190 int32 = t189 + 216613626
    return t190
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t193 int32 = self__5 + other__6
    return t193
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t199 string
    var inline354 string = _goml_runtime_core_int32_to_string(self__9)
    t199 = inline354
    var t200 string = "<" + t199
    var t201 string = t200 + ">"
    return t201
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t204 int32 = self__10.value
    var t205 string
    var inline356 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline356
    var t206 string = "Boxed(" + t205
    var t207 string = t206 + ")"
    return t207
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t210 int32 = self__11.value
    var t211 string
    var inline358 string = _goml_runtime_core_int32_to_string(t210)
    t211 = inline358
    var t212 string = "Boxed{value=" + t211
    var t213 string = t212 + "}"
    return t213
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t221 int32 = self__14.value
    var t222 int32 = t221 * 31
    var t223 int32 = t222 + 7
    var t224 int32 = t223 * 1315423911
    return t224
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t227 int32 = self__15.value
    var t228 int32 = other__16.value
    var t229 int32 = t227 + t228
    var t230 Boxed = Boxed{
        value: t229,
    }
    return t230
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t238 int32 = self__19.value
    var t239 string
    var inline362 string = _goml_runtime_core_int32_to_string(t238)
    t239 = inline362
    var t240 string = "[" + t239
    var t241 string = t240 + "]"
    return t241
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t247 string
    var inline401 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline402 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline401)
    t247 = inline402
    var inline398 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline398)
    var t248 Boxed = Boxed{
        value: 99,
    }
    var t249 Boxed = Boxed{
        value: 3,
    }
    var t250 Boxed = Boxed{
        value: 4,
    }
    var t251 string
    var inline395 Boxed = combine_scaled__T_Boxed(t249, t250, 2)
    var inline396 string = report_pair__Q_Boxed__T_Boxed(t248, t249, t250, inline395)
    t251 = inline396
    var inline392 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline392)
    var t252 string
    var inline381 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline382 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline381, third__51)
    var inline383 string = tag_text__Q_int32(sum_tag__48)
    var inline384 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline382)
    var inline385 string = inline383 + " "
    var inline386 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline382)
    var inline387 string = inline385 + inline386
    var inline388 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline384)
    var inline389 string = " @" + inline388
    var inline390 string = inline387 + inline389
    t252 = inline390
    var inline378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline378)
    var t253 Boxed = Boxed{
        value: 1,
    }
    var t254 Boxed = Boxed{
        value: 5,
    }
    var t255 Boxed = Boxed{
        value: 6,
    }
    var t256 Boxed = Boxed{
        value: 7,
    }
    var t257 string
    var inline367 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t254, t255)
    var inline368 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline367, t256)
    var inline369 string = tag_text__Q_Boxed(t253)
    var inline370 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline368)
    var inline371 string = inline369 + " "
    var inline372 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline368)
    var inline373 string = inline371 + inline372
    var inline374 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline370)
    var inline375 string = " @" + inline374
    var inline376 string = inline373 + inline375
    t257 = inline376
    var inline364 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline364)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t260 string = _goml_runtime_core_int32_to_string(self__35)
    return t260
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t263 bool = self__109 == other__110
    return t263
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t295 int32
    var inline488 int32 = a__23 + b__24
    t295 = inline488
    var inline486 int32 = t295 * factor__25
    return inline486
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline507 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(a__27, b__28)
    same__30 = inline507
    var header__31 string
    var inline501 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline502 string = inline501 + "#"
    var inline503 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline504 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline503)
    var inline505 string = inline502 + inline504
    header__31 = inline505
    var repr__32 string
    var inline496 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline497 string = inline496 + " / "
    var inline498 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline499 string = inline497 + inline498
    repr__32 = inline499
    var h__33 int32
    var inline493 int32 = combined__29 * 16777619
    var inline494 int32 = inline493 + 216613626
    h__33 = inline494
    var t299 string = header__31 + " "
    var t300 string = t299 + repr__32
    var t301 string
    if same__30 {
        t301 = "true"
    } else {
        t301 = "false"
    }
    var t302 string = " | eq=" + t301
    var t303 string
    var inline490 string = _goml_runtime_core_int32_to_string(h__33)
    t303 = inline490
    var t304 string = " | hash=" + t303
    var t305 string = t302 + t304
    var t306 string = t300 + t305
    return t306
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t309 Boxed
    var inline513 int32 = a__23.value
    var inline514 int32 = b__24.value
    var inline515 int32 = inline513 + inline514
    var inline516 Boxed = Boxed{
        value: inline515,
    }
    t309 = inline516
    var inline509 int32 = t309.value
    var inline510 int32 = inline509 * factor__25
    var inline511 Boxed = Boxed{
        value: inline510,
    }
    return inline511
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline537 int32 = a__27.value
    var inline538 int32 = b__28.value
    var inline539 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline537, inline538)
    same__30 = inline539
    var header__31 string
    var inline531 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline532 string = inline531 + "#"
    var inline533 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline534 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline533)
    var inline535 string = inline532 + inline534
    header__31 = inline535
    var repr__32 string
    var inline526 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline527 string = inline526 + " / "
    var inline528 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline529 string = inline527 + inline528
    repr__32 = inline529
    var h__33 int32
    var inline521 int32 = combined__29.value
    var inline522 int32 = inline521 * 31
    var inline523 int32 = inline522 + 7
    var inline524 int32 = inline523 * 1315423911
    h__33 = inline524
    var t313 string = header__31 + " "
    var t314 string = t313 + repr__32
    var t315 string
    if same__30 {
        t315 = "true"
    } else {
        t315 = "false"
    }
    var t316 string = " | eq=" + t315
    var t317 string
    var inline518 string = _goml_runtime_core_int32_to_string(h__33)
    t317 = inline518
    var t318 string = " | hash=" + t317
    var t319 string = t316 + t318
    var t320 string = t314 + t319
    return t320
}

func tag_text__Q_int32(tag__22 int32) string {
    var t323 string
    var inline546 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline547 string = "i32(" + inline546
    var inline548 string = inline547 + ")"
    t323 = inline548
    var t324 string = t323 + "#"
    var t325 int32
    var inline543 int32 = tag__22 * 16777619
    var inline544 int32 = inline543 + 216613626
    t325 = inline544
    var t326 string
    var inline541 string = _goml_runtime_core_int32_to_string(t325)
    t326 = inline541
    var t327 string = t324 + t326
    return t327
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t330 string
    var inline557 int32 = tag__22.value
    var inline558 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline557)
    var inline559 string = "Boxed{value=" + inline558
    var inline560 string = inline559 + "}"
    t330 = inline560
    var t331 string = t330 + "#"
    var t332 int32
    var inline552 int32 = tag__22.value
    var inline553 int32 = inline552 * 31
    var inline554 int32 = inline553 + 7
    var inline555 int32 = inline554 * 1315423911
    t332 = inline555
    var t333 string
    var inline550 string = _goml_runtime_core_int32_to_string(t332)
    t333 = inline550
    var t334 string = t331 + t333
    return t334
}

func main() {
    main0()
}
