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
    var inline345 string = _goml_runtime_core_int32_to_string(self__0)
    return inline345
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t181 string
    var inline347 string = _goml_runtime_core_int32_to_string(self__1)
    t181 = inline347
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
    var inline349 string = _goml_runtime_core_int32_to_string(self__9)
    t199 = inline349
    var t200 string = "<" + t199
    var t201 string = t200 + ">"
    return t201
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t204 int32 = self__10.value
    var t205 string
    var inline351 string = _goml_runtime_core_int32_to_string(t204)
    t205 = inline351
    var t206 string = "Boxed(" + t205
    var t207 string = t206 + ")"
    return t207
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t210 int32 = self__11.value
    var t211 string
    var inline353 string = _goml_runtime_core_int32_to_string(t210)
    t211 = inline353
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
    var inline355 string = _goml_runtime_core_int32_to_string(t238)
    t239 = inline355
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
    var inline394 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline395 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline394)
    t247 = inline395
    var inline391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline391)
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
    var inline388 Boxed = combine_scaled__T_Boxed(t249, t250, 2)
    var inline389 string = report_pair__Q_Boxed__T_Boxed(t248, t249, t250, inline388)
    t251 = inline389
    var inline385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline385)
    var t252 string
    var inline374 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline375 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline374, third__51)
    var inline376 string = tag_text__Q_int32(sum_tag__48)
    var inline377 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline375)
    var inline378 string = inline376 + " "
    var inline379 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline375)
    var inline380 string = inline378 + inline379
    var inline381 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline377)
    var inline382 string = " @" + inline381
    var inline383 string = inline380 + inline382
    t252 = inline383
    var inline371 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline371)
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
    var inline360 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t254, t255)
    var inline361 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline360, t256)
    var inline362 string = tag_text__Q_Boxed(t253)
    var inline363 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline361)
    var inline364 string = inline362 + " "
    var inline365 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline361)
    var inline366 string = inline364 + inline365
    var inline367 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline363)
    var inline368 string = " @" + inline367
    var inline369 string = inline366 + inline368
    t257 = inline369
    var inline357 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t257)
    _goml_runtime_core_string_println(inline357)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t260 string = _goml_runtime_core_int32_to_string(self__33)
    return t260
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t292 int32
    var inline481 int32 = a__23 + b__24
    t292 = inline481
    var inline479 int32 = t292 * factor__25
    return inline479
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline500 bool = a__27 == b__28
    same__30 = inline500
    var header__31 string
    var inline494 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline495 string = inline494 + "#"
    var inline496 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline497 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline496)
    var inline498 string = inline495 + inline497
    header__31 = inline498
    var repr__32 string
    var inline489 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline490 string = inline489 + " / "
    var inline491 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline492 string = inline490 + inline491
    repr__32 = inline492
    var h__33 int32
    var inline486 int32 = combined__29 * 16777619
    var inline487 int32 = inline486 + 216613626
    h__33 = inline487
    var t296 string = header__31 + " "
    var t297 string = t296 + repr__32
    var t298 string
    if same__30 {
        t298 = "true"
    } else {
        t298 = "false"
    }
    var t299 string = " | eq=" + t298
    var t300 string
    var inline483 string = _goml_runtime_core_int32_to_string(h__33)
    t300 = inline483
    var t301 string = " | hash=" + t300
    var t302 string = t299 + t301
    var t303 string = t297 + t302
    return t303
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t306 Boxed
    var inline506 int32 = a__23.value
    var inline507 int32 = b__24.value
    var inline508 int32 = inline506 + inline507
    var inline509 Boxed = Boxed{
        value: inline508,
    }
    t306 = inline509
    var inline502 int32 = t306.value
    var inline503 int32 = inline502 * factor__25
    var inline504 Boxed = Boxed{
        value: inline503,
    }
    return inline504
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline530 int32 = a__27.value
    var inline531 int32 = b__28.value
    var inline532 bool = inline530 == inline531
    same__30 = inline532
    var header__31 string
    var inline524 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline525 string = inline524 + "#"
    var inline526 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline527 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline526)
    var inline528 string = inline525 + inline527
    header__31 = inline528
    var repr__32 string
    var inline519 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline520 string = inline519 + " / "
    var inline521 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline522 string = inline520 + inline521
    repr__32 = inline522
    var h__33 int32
    var inline514 int32 = combined__29.value
    var inline515 int32 = inline514 * 31
    var inline516 int32 = inline515 + 7
    var inline517 int32 = inline516 * 1315423911
    h__33 = inline517
    var t310 string = header__31 + " "
    var t311 string = t310 + repr__32
    var t312 string
    if same__30 {
        t312 = "true"
    } else {
        t312 = "false"
    }
    var t313 string = " | eq=" + t312
    var t314 string
    var inline511 string = _goml_runtime_core_int32_to_string(h__33)
    t314 = inline511
    var t315 string = " | hash=" + t314
    var t316 string = t313 + t315
    var t317 string = t311 + t316
    return t317
}

func tag_text__Q_int32(tag__22 int32) string {
    var t320 string
    var inline539 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline540 string = "i32(" + inline539
    var inline541 string = inline540 + ")"
    t320 = inline541
    var t321 string = t320 + "#"
    var t322 int32
    var inline536 int32 = tag__22 * 16777619
    var inline537 int32 = inline536 + 216613626
    t322 = inline537
    var t323 string
    var inline534 string = _goml_runtime_core_int32_to_string(t322)
    t323 = inline534
    var t324 string = t321 + t323
    return t324
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t327 string
    var inline550 int32 = tag__22.value
    var inline551 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline550)
    var inline552 string = "Boxed{value=" + inline551
    var inline553 string = inline552 + "}"
    t327 = inline553
    var t328 string = t327 + "#"
    var t329 int32
    var inline545 int32 = tag__22.value
    var inline546 int32 = inline545 * 31
    var inline547 int32 = inline546 + 7
    var inline548 int32 = inline547 * 1315423911
    t329 = inline548
    var t330 string
    var inline543 string = _goml_runtime_core_int32_to_string(t329)
    t330 = inline543
    var t331 string = t328 + t330
    return t331
}

func main() {
    main0()
}
