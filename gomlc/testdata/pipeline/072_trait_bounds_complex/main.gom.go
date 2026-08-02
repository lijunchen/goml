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
    var inline331 string = _goml_runtime_core_int32_to_string(self__0)
    return inline331
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t164 string
    var inline333 string = _goml_runtime_core_int32_to_string(self__1)
    t164 = inline333
    var t165 string = "i32(" + t164
    var t166 string = t165 + ")"
    return t166
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t172 int32 = self__4 * 16777619
    var t173 int32 = t172 + 216613626
    return t173
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t176 int32 = self__5 + other__6
    return t176
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t182 string
    var inline337 string = _goml_runtime_core_int32_to_string(self__9)
    t182 = inline337
    var t183 string = "<" + t182
    var t184 string = t183 + ">"
    return t184
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t187 int32 = self__10.value
    var t188 string
    var inline339 string = _goml_runtime_core_int32_to_string(t187)
    t188 = inline339
    var t189 string = "Boxed(" + t188
    var t190 string = t189 + ")"
    return t190
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t193 int32 = self__11.value
    var t194 string
    var inline341 string = _goml_runtime_core_int32_to_string(t193)
    t194 = inline341
    var t195 string = "Boxed{value=" + t194
    var t196 string = t195 + "}"
    return t196
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t204 int32 = self__14.value
    var t205 int32 = t204 * 31
    var t206 int32 = t205 + 7
    var t207 int32 = t206 * 1315423911
    return t207
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t210 int32 = self__15.value
    var t211 int32 = other__16.value
    var t212 int32 = t210 + t211
    var t213 Boxed = Boxed{
        value: t212,
    }
    return t213
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t221 int32 = self__19.value
    var t222 string
    var inline345 string = _goml_runtime_core_int32_to_string(t221)
    t222 = inline345
    var t223 string = "[" + t222
    var t224 string = t223 + "]"
    return t224
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t230 string
    var inline384 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline385 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline384)
    t230 = inline385
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline381)
    var t231 Boxed = Boxed{
        value: 99,
    }
    var t232 Boxed = Boxed{
        value: 3,
    }
    var t233 Boxed = Boxed{
        value: 4,
    }
    var t234 string
    var inline378 Boxed = combine_scaled__T_Boxed(t232, t233, 2)
    var inline379 string = report_pair__Q_Boxed__T_Boxed(t231, t232, t233, inline378)
    t234 = inline379
    var inline375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline375)
    var t235 string
    var inline364 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline365 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline364, third__51)
    var inline366 string = tag_text__Q_int32(sum_tag__48)
    var inline367 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline365)
    var inline368 string = inline366 + " "
    var inline369 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline365)
    var inline370 string = inline368 + inline369
    var inline371 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline367)
    var inline372 string = " @" + inline371
    var inline373 string = inline370 + inline372
    t235 = inline373
    var inline361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline361)
    var t236 Boxed = Boxed{
        value: 1,
    }
    var t237 Boxed = Boxed{
        value: 5,
    }
    var t238 Boxed = Boxed{
        value: 6,
    }
    var t239 Boxed = Boxed{
        value: 7,
    }
    var t240 string
    var inline350 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t237, t238)
    var inline351 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline350, t239)
    var inline352 string = tag_text__Q_Boxed(t236)
    var inline353 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline351)
    var inline354 string = inline352 + " "
    var inline355 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline351)
    var inline356 string = inline354 + inline355
    var inline357 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline353)
    var inline358 string = " @" + inline357
    var inline359 string = inline356 + inline358
    t240 = inline359
    var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline347)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t243 string = _goml_runtime_core_int32_to_string(self__6)
    return t243
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t246 bool = self__65 == other__66
    return t246
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t278 int32
    var inline471 int32 = a__23 + b__24
    t278 = inline471
    var inline469 int32 = t278 * factor__25
    return inline469
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline490 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__27, b__28)
    same__30 = inline490
    var header__31 string
    var inline484 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline485 string = inline484 + "#"
    var inline486 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline487 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline486)
    var inline488 string = inline485 + inline487
    header__31 = inline488
    var repr__32 string
    var inline479 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline480 string = inline479 + " / "
    var inline481 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline482 string = inline480 + inline481
    repr__32 = inline482
    var h__33 int32
    var inline476 int32 = combined__29 * 16777619
    var inline477 int32 = inline476 + 216613626
    h__33 = inline477
    var t282 string = header__31 + " "
    var t283 string = t282 + repr__32
    var t284 string
    if same__30 {
        t284 = "true"
    } else {
        t284 = "false"
    }
    var t285 string = " | eq=" + t284
    var t286 string
    var inline473 string = _goml_runtime_core_int32_to_string(h__33)
    t286 = inline473
    var t287 string = " | hash=" + t286
    var t288 string = t285 + t287
    var t289 string = t283 + t288
    return t289
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t292 Boxed
    var inline496 int32 = a__23.value
    var inline497 int32 = b__24.value
    var inline498 int32 = inline496 + inline497
    var inline499 Boxed = Boxed{
        value: inline498,
    }
    t292 = inline499
    var inline492 int32 = t292.value
    var inline493 int32 = inline492 * factor__25
    var inline494 Boxed = Boxed{
        value: inline493,
    }
    return inline494
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline520 int32 = a__27.value
    var inline521 int32 = b__28.value
    var inline522 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline520, inline521)
    same__30 = inline522
    var header__31 string
    var inline514 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline515 string = inline514 + "#"
    var inline516 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline517 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline516)
    var inline518 string = inline515 + inline517
    header__31 = inline518
    var repr__32 string
    var inline509 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline510 string = inline509 + " / "
    var inline511 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline512 string = inline510 + inline511
    repr__32 = inline512
    var h__33 int32
    var inline504 int32 = combined__29.value
    var inline505 int32 = inline504 * 31
    var inline506 int32 = inline505 + 7
    var inline507 int32 = inline506 * 1315423911
    h__33 = inline507
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
    var inline501 string = _goml_runtime_core_int32_to_string(h__33)
    t300 = inline501
    var t301 string = " | hash=" + t300
    var t302 string = t299 + t301
    var t303 string = t297 + t302
    return t303
}

func tag_text__Q_int32(tag__22 int32) string {
    var t306 string
    var inline529 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline530 string = "i32(" + inline529
    var inline531 string = inline530 + ")"
    t306 = inline531
    var t307 string = t306 + "#"
    var t308 int32
    var inline526 int32 = tag__22 * 16777619
    var inline527 int32 = inline526 + 216613626
    t308 = inline527
    var t309 string
    var inline524 string = _goml_runtime_core_int32_to_string(t308)
    t309 = inline524
    var t310 string = t307 + t309
    return t310
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t313 string
    var inline540 int32 = tag__22.value
    var inline541 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline540)
    var inline542 string = "Boxed{value=" + inline541
    var inline543 string = inline542 + "}"
    t313 = inline543
    var t314 string = t313 + "#"
    var t315 int32
    var inline535 int32 = tag__22.value
    var inline536 int32 = inline535 * 31
    var inline537 int32 = inline536 + 7
    var inline538 int32 = inline537 * 1315423911
    t315 = inline538
    var t316 string
    var inline533 string = _goml_runtime_core_int32_to_string(t315)
    t316 = inline533
    var t317 string = t314 + t316
    return t317
}

func main() {
    main0()
}
