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
    var inline312 string = _goml_runtime_core_int32_to_string(self__0)
    return inline312
}

func _goml_m_trait__impl_i_Debug_i_int32_i_show(self__1 int32) string {
    var t145 string
    var inline314 string = _goml_runtime_core_int32_to_string(self__1)
    t145 = inline314
    var t146 string = "i32(" + t145
    var t147 string = t146 + ")"
    return t147
}

func _goml_m_trait__impl_i_MyHash_i_int32_i_hash(self__4 int32) int32 {
    var t153 int32 = self__4 * 16777619
    var t154 int32 = t153 + 216613626
    return t154
}

func _goml_m_trait__impl_i_Add_i_int32_i_add(self__5 int32, other__6 int32) int32 {
    var t157 int32 = self__5 + other__6
    return t157
}

func _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(self__9 int32) string {
    var t163 string
    var inline318 string = _goml_runtime_core_int32_to_string(self__9)
    t163 = inline318
    var t164 string = "<" + t163
    var t165 string = t164 + ">"
    return t165
}

func _goml_m_trait__impl_i_Display_i_Boxed_i_show(self__10 Boxed) string {
    var t168 int32 = self__10.value
    var t169 string
    var inline320 string = _goml_runtime_core_int32_to_string(t168)
    t169 = inline320
    var t170 string = "Boxed(" + t169
    var t171 string = t170 + ")"
    return t171
}

func _goml_m_trait__impl_i_Debug_i_Boxed_i_show(self__11 Boxed) string {
    var t174 int32 = self__11.value
    var t175 string
    var inline322 string = _goml_runtime_core_int32_to_string(t174)
    t175 = inline322
    var t176 string = "Boxed{value=" + t175
    var t177 string = t176 + "}"
    return t177
}

func _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(self__14 Boxed) int32 {
    var t185 int32 = self__14.value
    var t186 int32 = t185 * 31
    var t187 int32 = t186 + 7
    var t188 int32 = t187 * 1315423911
    return t188
}

func _goml_m_trait__impl_i_Add_i_Boxed_i_add(self__15 Boxed, other__16 Boxed) Boxed {
    var t191 int32 = self__15.value
    var t192 int32 = other__16.value
    var t193 int32 = t191 + t192
    var t194 Boxed = Boxed{
        value: t193,
    }
    return t194
}

func _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(self__19 Boxed) string {
    var t202 int32 = self__19.value
    var t203 string
    var inline326 string = _goml_runtime_core_int32_to_string(t202)
    t203 = inline326
    var t204 string = "[" + t203
    var t205 string = t204 + "]"
    return t205
}

func main0() struct{} {
    var tag__45 int32 = 7
    var left__46 int32 = 10
    var right__47 int32 = 32
    var sum_tag__48 int32 = 0
    var first__49 int32 = 1
    var second__50 int32 = 2
    var third__51 int32 = 3
    var t211 string
    var inline365 int32 = combine_scaled__T_int32(left__46, right__47, 2)
    var inline366 string = report_pair__Q_int32__T_int32(tag__45, left__46, right__47, inline365)
    t211 = inline366
    var inline362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline362)
    var t212 Boxed = Boxed{
        value: 99,
    }
    var t213 Boxed = Boxed{
        value: 3,
    }
    var t214 Boxed = Boxed{
        value: 4,
    }
    var t215 string
    var inline359 Boxed = combine_scaled__T_Boxed(t213, t214, 2)
    var inline360 string = report_pair__Q_Boxed__T_Boxed(t212, t213, t214, inline359)
    t215 = inline360
    var inline356 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline356)
    var t216 string
    var inline345 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(first__49, second__50)
    var inline346 int32 = _goml_m_trait__impl_i_Add_i_int32_i_add(inline345, third__51)
    var inline347 string = tag_text__Q_int32(sum_tag__48)
    var inline348 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(inline346)
    var inline349 string = inline347 + " "
    var inline350 string = _goml_m_trait__impl_i_Inspect_i_int32_i_inspect(inline346)
    var inline351 string = inline349 + inline350
    var inline352 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline348)
    var inline353 string = " @" + inline352
    var inline354 string = inline351 + inline353
    t216 = inline354
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline342)
    var t217 Boxed = Boxed{
        value: 1,
    }
    var t218 Boxed = Boxed{
        value: 5,
    }
    var t219 Boxed = Boxed{
        value: 6,
    }
    var t220 Boxed = Boxed{
        value: 7,
    }
    var t221 string
    var inline331 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(t218, t219)
    var inline332 Boxed = _goml_m_trait__impl_i_Add_i_Boxed_i_add(inline331, t220)
    var inline333 string = tag_text__Q_Boxed(t217)
    var inline334 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(inline332)
    var inline335 string = inline333 + " "
    var inline336 string = _goml_m_trait__impl_i_Inspect_i_Boxed_i_inspect(inline332)
    var inline337 string = inline335 + inline336
    var inline338 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline334)
    var inline339 string = " @" + inline338
    var inline340 string = inline337 + inline339
    t221 = inline340
    var inline328 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline328)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t224 string = _goml_runtime_core_int32_to_string(self__35)
    return t224
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t227 bool = self__109 == other__110
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func combine_scaled__T_int32(a__23 int32, b__24 int32, factor__25 int32) int32 {
    var t259 int32
    var inline452 int32 = a__23 + b__24
    t259 = inline452
    var inline450 int32 = t259 * factor__25
    return inline450
}

func report_pair__Q_int32__T_int32(tag__26 int32, a__27 int32, b__28 int32, combined__29 int32) string {
    var same__30 bool
    var inline471 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__27, b__28)
    same__30 = inline471
    var header__31 string
    var inline465 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(tag__26)
    var inline466 string = inline465 + "#"
    var inline467 int32 = _goml_m_trait__impl_i_MyHash_i_int32_i_hash(tag__26)
    var inline468 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline467)
    var inline469 string = inline466 + inline468
    header__31 = inline469
    var repr__32 string
    var inline460 string = _goml_m_trait__impl_i_Debug_i_int32_i_show(combined__29)
    var inline461 string = inline460 + " / "
    var inline462 string = _goml_m_trait__impl_i_Display_i_int32_i_show(combined__29)
    var inline463 string = inline461 + inline462
    repr__32 = inline463
    var h__33 int32
    var inline457 int32 = combined__29 * 16777619
    var inline458 int32 = inline457 + 216613626
    h__33 = inline458
    var t263 string = header__31 + " "
    var t264 string = t263 + repr__32
    var t265 string
    if same__30 {
        t265 = "true"
    } else {
        t265 = "false"
    }
    var t266 string = " | eq=" + t265
    var t267 string
    var inline454 string = _goml_runtime_core_int32_to_string(h__33)
    t267 = inline454
    var t268 string = " | hash=" + t267
    var t269 string = t266 + t268
    var t270 string = t264 + t269
    return t270
}

func combine_scaled__T_Boxed(a__23 Boxed, b__24 Boxed, factor__25 int32) Boxed {
    var t273 Boxed
    var inline477 int32 = a__23.value
    var inline478 int32 = b__24.value
    var inline479 int32 = inline477 + inline478
    var inline480 Boxed = Boxed{
        value: inline479,
    }
    t273 = inline480
    var inline473 int32 = t273.value
    var inline474 int32 = inline473 * factor__25
    var inline475 Boxed = Boxed{
        value: inline474,
    }
    return inline475
}

func report_pair__Q_Boxed__T_Boxed(tag__26 Boxed, a__27 Boxed, b__28 Boxed, combined__29 Boxed) string {
    var same__30 bool
    var inline501 int32 = a__27.value
    var inline502 int32 = b__28.value
    var inline503 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline501, inline502)
    same__30 = inline503
    var header__31 string
    var inline495 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(tag__26)
    var inline496 string = inline495 + "#"
    var inline497 int32 = _goml_m_trait__impl_i_MyHash_i_Boxed_i_hash(tag__26)
    var inline498 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline497)
    var inline499 string = inline496 + inline498
    header__31 = inline499
    var repr__32 string
    var inline490 string = _goml_m_trait__impl_i_Debug_i_Boxed_i_show(combined__29)
    var inline491 string = inline490 + " / "
    var inline492 string = _goml_m_trait__impl_i_Display_i_Boxed_i_show(combined__29)
    var inline493 string = inline491 + inline492
    repr__32 = inline493
    var h__33 int32
    var inline485 int32 = combined__29.value
    var inline486 int32 = inline485 * 31
    var inline487 int32 = inline486 + 7
    var inline488 int32 = inline487 * 1315423911
    h__33 = inline488
    var t277 string = header__31 + " "
    var t278 string = t277 + repr__32
    var t279 string
    if same__30 {
        t279 = "true"
    } else {
        t279 = "false"
    }
    var t280 string = " | eq=" + t279
    var t281 string
    var inline482 string = _goml_runtime_core_int32_to_string(h__33)
    t281 = inline482
    var t282 string = " | hash=" + t281
    var t283 string = t280 + t282
    var t284 string = t278 + t283
    return t284
}

func tag_text__Q_int32(tag__22 int32) string {
    var t287 string
    var inline510 string = _goml_m_inherent_i_int32_i_int32_i_to__string(tag__22)
    var inline511 string = "i32(" + inline510
    var inline512 string = inline511 + ")"
    t287 = inline512
    var t288 string = t287 + "#"
    var t289 int32
    var inline507 int32 = tag__22 * 16777619
    var inline508 int32 = inline507 + 216613626
    t289 = inline508
    var t290 string
    var inline505 string = _goml_runtime_core_int32_to_string(t289)
    t290 = inline505
    var t291 string = t288 + t290
    return t291
}

func tag_text__Q_Boxed(tag__22 Boxed) string {
    var t294 string
    var inline521 int32 = tag__22.value
    var inline522 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline521)
    var inline523 string = "Boxed{value=" + inline522
    var inline524 string = inline523 + "}"
    t294 = inline524
    var t295 string = t294 + "#"
    var t296 int32
    var inline516 int32 = tag__22.value
    var inline517 int32 = inline516 * 31
    var inline518 int32 = inline517 + 7
    var inline519 int32 = inline518 * 1315423911
    t296 = inline519
    var t297 string
    var inline514 string = _goml_runtime_core_int32_to_string(t296)
    t297 = inline514
    var t298 string = t295 + t297
    return t298
}

func main() {
    main0()
}
