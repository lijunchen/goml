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
    var inline353 string = _goml_runtime_core_int32_to_string(self__0)
    return inline353
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t186 string
    var inline355 string = _goml_runtime_core_int32_to_string(self__1)
    t186 = inline355
    var t187 string = "i32(" + t186
    var t188 string = t187 + ")"
    return t188
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t194 int32 = self__4 * 16777619
    var t195 int32 = t194 + 216613626
    return t195
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t198 int32 = self__5 + other__6
    return t198
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t204 string
    var inline359 string = _goml_runtime_core_int32_to_string(self__9)
    t204 = inline359
    var t205 string = "<" + t204
    var t206 string = t205 + ">"
    return t206
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t209 int32 = self__10.value
    var t210 string
    var inline361 string = _goml_runtime_core_int32_to_string(t209)
    t210 = inline361
    var t211 string = "Boxed(" + t210
    var t212 string = t211 + ")"
    return t212
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t215 int32 = self__11.value
    var t216 string
    var inline363 string = _goml_runtime_core_int32_to_string(t215)
    t216 = inline363
    var t217 string = "Boxed{value=" + t216
    var t218 string = t217 + "}"
    return t218
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t226 int32 = self__14.value
    var t227 int32 = t226 * 31
    var t228 int32 = t227 + 7
    var t229 int32 = t228 * 1315423911
    return t229
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t232 int32 = self__15.value
    var t233 int32 = other__16.value
    var t234 int32 = t232 + t233
    var t235 Boxed = Boxed{
        value: t234,
    }
    return t235
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t243 int32 = self__19.value
    var t244 string
    var inline367 string = _goml_runtime_core_int32_to_string(t243)
    t244 = inline367
    var t245 string = "[" + t244
    var t246 string = t245 + "]"
    return t246
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t252 string
    var inline406 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline407 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline406)
    t252 = inline407
    var inline403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline403)
    var t253 Boxed = Boxed{
        value: 99,
    }
    var t254 Boxed = Boxed{
        value: 3,
    }
    var t255 Boxed = Boxed{
        value: 4,
    }
    var t256 string
    var inline400 Boxed = combine_scaled__T_Boxed(t254, t255, 2)
    var inline401 string = report_pair__Q_Boxed__T_Boxed(t253, t254, t255, inline400)
    t256 = inline401
    var inline397 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t256)
    _goml_runtime_core_string_println(inline397)
    var t257 string
    var inline386 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline387 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline386, third__51)
    var inline388 string = tag_text__Q_int32(sum_tag__48)
    var inline389 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline387)
    var inline390 string = inline388 + " "
    var inline391 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline387)
    var inline392 string = inline390 + inline391
    var inline393 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline389)
    var inline394 string = " @" + inline393
    var inline395 string = inline392 + inline394
    t257 = inline395
    var inline383 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline383)
    var t258 Boxed = Boxed{
        value: 1,
    }
    var t259 Boxed = Boxed{
        value: 5,
    }
    var t260 Boxed = Boxed{
        value: 6,
    }
    var t261 Boxed = Boxed{
        value: 7,
    }
    var t262 string
    var inline372 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t259, t260)
    var inline373 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline372, t261)
    var inline374 string = tag_text__Q_Boxed(t258)
    var inline375 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline373)
    var inline376 string = inline374 + " "
    var inline377 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline373)
    var inline378 string = inline376 + inline377
    var inline379 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline375)
    var inline380 string = " @" + inline379
    var inline381 string = inline378 + inline380
    t262 = inline381
    var inline369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline369)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t265 string = _goml_runtime_core_int32_to_string(self__35)
    return t265
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t268 bool = self__94 == other__95
    return t268
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t300 int32
    var inline493 int32 = a__23 + b__24
    t300 = inline493
    var inline491 int32 = t300 * factor__25
    return inline491
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline512 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__27, b__28)
    same__30 = inline512
    var header__31 string
    var inline506 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline507 string = inline506 + "#"
    var inline508 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline509 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline508)
    var inline510 string = inline507 + inline509
    header__31 = inline510
    var repr__32 string
    var inline501 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline502 string = inline501 + " / "
    var inline503 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline504 string = inline502 + inline503
    repr__32 = inline504
    var h__33 int32
    var inline498 int32 = combined__29 * 16777619
    var inline499 int32 = inline498 + 216613626
    h__33 = inline499
    var t304 string = header__31 + " "
    var t305 string = t304 + repr__32
    var t306 string
    if same__30 {
        t306 = "true"
    } else {
        t306 = "false"
    }
    var t307 string = " | eq=" + t306
    var t308 string
    var inline495 string = _goml_runtime_core_int32_to_string(h__33)
    t308 = inline495
    var t309 string = " | hash=" + t308
    var t310 string = t307 + t309
    var t311 string = t305 + t310
    return t311
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t314 Boxed
    var inline518 int32 = a__23.value
    var inline519 int32 = b__24.value
    var inline520 int32 = inline518 + inline519
    var inline521 Boxed = Boxed{
        value: inline520,
    }
    t314 = inline521
    var inline514 int32 = t314.value
    var inline515 int32 = inline514 * factor__25
    var inline516 Boxed = Boxed{
        value: inline515,
    }
    return inline516
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline542 int32 = a__27.value
    var inline543 int32 = b__28.value
    var inline544 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline542, inline543)
    same__30 = inline544
    var header__31 string
    var inline536 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline537 string = inline536 + "#"
    var inline538 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline539 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline538)
    var inline540 string = inline537 + inline539
    header__31 = inline540
    var repr__32 string
    var inline531 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline532 string = inline531 + " / "
    var inline533 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline534 string = inline532 + inline533
    repr__32 = inline534
    var h__33 int32
    var inline526 int32 = combined__29.value
    var inline527 int32 = inline526 * 31
    var inline528 int32 = inline527 + 7
    var inline529 int32 = inline528 * 1315423911
    h__33 = inline529
    var t318 string = header__31 + " "
    var t319 string = t318 + repr__32
    var t320 string
    if same__30 {
        t320 = "true"
    } else {
        t320 = "false"
    }
    var t321 string = " | eq=" + t320
    var t322 string
    var inline523 string = _goml_runtime_core_int32_to_string(h__33)
    t322 = inline523
    var t323 string = " | hash=" + t322
    var t324 string = t321 + t323
    var t325 string = t319 + t324
    return t325
}

func tag_text__Q_int32(tag__22 int32) string {
    var t328 string
    var inline551 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline552 string = "i32(" + inline551
    var inline553 string = inline552 + ")"
    t328 = inline553
    var t329 string = t328 + "#"
    var t330 int32
    var inline548 int32 = tag__22 * 16777619
    var inline549 int32 = inline548 + 216613626
    t330 = inline549
    var t331 string
    var inline546 string = _goml_runtime_core_int32_to_string(t330)
    t331 = inline546
    var t332 string = t329 + t331
    return t332
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t335 string
    var inline562 int32 = tag__22.value
    var inline563 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline562)
    var inline564 string = "Boxed{value=" + inline563
    var inline565 string = inline564 + "}"
    t335 = inline565
    var t336 string = t335 + "#"
    var t337 int32
    var inline557 int32 = tag__22.value
    var inline558 int32 = inline557 * 31
    var inline559 int32 = inline558 + 7
    var inline560 int32 = inline559 * 1315423911
    t337 = inline560
    var t338 string
    var inline555 string = _goml_runtime_core_int32_to_string(t337)
    t338 = inline555
    var t339 string = t336 + t338
    return t339
}

func main() {
    main0()
}
