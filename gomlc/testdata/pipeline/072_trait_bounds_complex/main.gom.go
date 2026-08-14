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
    var inline360 string = _goml_runtime_core_int32_to_string(self__0)
    return inline360
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t196 string
    var inline362 string = _goml_runtime_core_int32_to_string(self__1)
    t196 = inline362
    var t197 string = "i32(" + t196
    var t198 string = t197 + ")"
    return t198
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t204 int32 = self__4 * 16777619
    var t205 int32 = t204 + 216613626
    return t205
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t208 int32 = self__5 + other__6
    return t208
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t214 string
    var inline364 string = _goml_runtime_core_int32_to_string(self__9)
    t214 = inline364
    var t215 string = "<" + t214
    var t216 string = t215 + ">"
    return t216
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t219 int32 = self__10.value
    var t220 string
    var inline366 string = _goml_runtime_core_int32_to_string(t219)
    t220 = inline366
    var t221 string = "Boxed(" + t220
    var t222 string = t221 + ")"
    return t222
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t225 int32 = self__11.value
    var t226 string
    var inline368 string = _goml_runtime_core_int32_to_string(t225)
    t226 = inline368
    var t227 string = "Boxed{value=" + t226
    var t228 string = t227 + "}"
    return t228
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t236 int32 = self__14.value
    var t237 int32 = t236 * 31
    var t238 int32 = t237 + 7
    var t239 int32 = t238 * 1315423911
    return t239
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t242 int32 = self__15.value
    var t243 int32 = other__16.value
    var t244 int32 = t242 + t243
    var t245 Boxed = Boxed{
        value: t244,
    }
    return t245
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t253 int32 = self__19.value
    var t254 string
    var inline370 string = _goml_runtime_core_int32_to_string(t253)
    t254 = inline370
    var t255 string = "[" + t254
    var t256 string = t255 + "]"
    return t256
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t262 string
    var inline409 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline410 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline409)
    t262 = inline410
    var inline406 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline406)
    var t263 Boxed = Boxed{
        value: 99,
    }
    var t264 Boxed = Boxed{
        value: 3,
    }
    var t265 Boxed = Boxed{
        value: 4,
    }
    var t266 string
    var inline403 Boxed = combine_scaled__T_Boxed(t264, t265, 2)
    var inline404 string = report_pair__Q_Boxed__T_Boxed(t263, t264, t265, inline403)
    t266 = inline404
    var inline400 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t266)
    _goml_runtime_core_string_println(inline400)
    var t267 string
    var inline389 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline390 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline389, third__51)
    var inline391 string = tag_text__Q_int32(sum_tag__48)
    var inline392 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline390)
    var inline393 string = inline391 + " "
    var inline394 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline390)
    var inline395 string = inline393 + inline394
    var inline396 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline392)
    var inline397 string = " @" + inline396
    var inline398 string = inline395 + inline397
    t267 = inline398
    var inline386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t267)
    _goml_runtime_core_string_println(inline386)
    var t268 Boxed = Boxed{
        value: 1,
    }
    var t269 Boxed = Boxed{
        value: 5,
    }
    var t270 Boxed = Boxed{
        value: 6,
    }
    var t271 Boxed = Boxed{
        value: 7,
    }
    var t272 string
    var inline375 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t269, t270)
    var inline376 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline375, t271)
    var inline377 string = tag_text__Q_Boxed(t268)
    var inline378 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline376)
    var inline379 string = inline377 + " "
    var inline380 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline376)
    var inline381 string = inline379 + inline380
    var inline382 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline378)
    var inline383 string = " @" + inline382
    var inline384 string = inline381 + inline383
    t272 = inline384
    var inline372 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t272)
    _goml_runtime_core_string_println(inline372)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t275 string = _goml_runtime_core_int32_to_string(self__33)
    return t275
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t307 int32
    var inline496 int32 = a__23 + b__24
    t307 = inline496
    var inline494 int32 = t307 * factor__25
    return inline494
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline515 bool = a__27 == b__28
    same__30 = inline515
    var header__31 string
    var inline509 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline510 string = inline509 + "#"
    var inline511 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline512 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline511)
    var inline513 string = inline510 + inline512
    header__31 = inline513
    var repr__32 string
    var inline504 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline505 string = inline504 + " / "
    var inline506 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline507 string = inline505 + inline506
    repr__32 = inline507
    var h__33 int32
    var inline501 int32 = combined__29 * 16777619
    var inline502 int32 = inline501 + 216613626
    h__33 = inline502
    var t311 string = header__31 + " "
    var t312 string = t311 + repr__32
    var t313 string
    if same__30 {
        t313 = "true"
    } else {
        t313 = "false"
    }
    var t314 string = " | eq=" + t313
    var t315 string
    var inline498 string = _goml_runtime_core_int32_to_string(h__33)
    t315 = inline498
    var t316 string = " | hash=" + t315
    var t317 string = t314 + t316
    var t318 string = t312 + t317
    return t318
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t321 Boxed
    var inline521 int32 = a__23.value
    var inline522 int32 = b__24.value
    var inline523 int32 = inline521 + inline522
    var inline524 Boxed = Boxed{
        value: inline523,
    }
    t321 = inline524
    var inline517 int32 = t321.value
    var inline518 int32 = inline517 * factor__25
    var inline519 Boxed = Boxed{
        value: inline518,
    }
    return inline519
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline545 int32 = a__27.value
    var inline546 int32 = b__28.value
    var inline547 bool = inline545 == inline546
    same__30 = inline547
    var header__31 string
    var inline539 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline540 string = inline539 + "#"
    var inline541 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline542 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline541)
    var inline543 string = inline540 + inline542
    header__31 = inline543
    var repr__32 string
    var inline534 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline535 string = inline534 + " / "
    var inline536 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline537 string = inline535 + inline536
    repr__32 = inline537
    var h__33 int32
    var inline529 int32 = combined__29.value
    var inline530 int32 = inline529 * 31
    var inline531 int32 = inline530 + 7
    var inline532 int32 = inline531 * 1315423911
    h__33 = inline532
    var t325 string = header__31 + " "
    var t326 string = t325 + repr__32
    var t327 string
    if same__30 {
        t327 = "true"
    } else {
        t327 = "false"
    }
    var t328 string = " | eq=" + t327
    var t329 string
    var inline526 string = _goml_runtime_core_int32_to_string(h__33)
    t329 = inline526
    var t330 string = " | hash=" + t329
    var t331 string = t328 + t330
    var t332 string = t326 + t331
    return t332
}

func tag_text__Q_int32(tag__22 int32) string {
    var t335 string
    var inline554 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline555 string = "i32(" + inline554
    var inline556 string = inline555 + ")"
    t335 = inline556
    var t336 string = t335 + "#"
    var t337 int32
    var inline551 int32 = tag__22 * 16777619
    var inline552 int32 = inline551 + 216613626
    t337 = inline552
    var t338 string
    var inline549 string = _goml_runtime_core_int32_to_string(t337)
    t338 = inline549
    var t339 string = t336 + t338
    return t339
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t342 string
    var inline565 int32 = tag__22.value
    var inline566 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline565)
    var inline567 string = "Boxed{value=" + inline566
    var inline568 string = inline567 + "}"
    t342 = inline568
    var t343 string = t342 + "#"
    var t344 int32
    var inline560 int32 = tag__22.value
    var inline561 int32 = inline560 * 31
    var inline562 int32 = inline561 + 7
    var inline563 int32 = inline562 * 1315423911
    t344 = inline563
    var t345 string
    var inline558 string = _goml_runtime_core_int32_to_string(t344)
    t345 = inline558
    var t346 string = t343 + t345
    return t346
}

func main() {
    main0()
}
