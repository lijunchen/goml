package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_NoDefault_x struct {
    value NoDefault
}

type Settings struct {
    enabled bool
    retries int
    label string
}

type NoDefault struct {
    value int
}

type Empty struct {}

type PairDefaults__int struct {
    first int
    second int
    nested Option__int
}

type State interface {
    isState()
}

type Idle struct {}

func (_ Idle) isState() {}

type Running struct {
    _0 int
}

func (_ Running) isState() {}

type Message interface {
    isMessage()
}

type Message_Data struct {
    _0 string
    _1 int
}

func (_ Message_Data) isMessage() {}

type Message_Empty struct {}

func (_ Message_Empty) isMessage() {}

type Event interface {
    isEvent()
}

type Event_Data struct {
    _0 string
    _1 int
}

func (_ Event_Data) isEvent() {}

type Event_Empty struct {}

func (_ Event_Empty) isEvent() {}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

type Lazy__NoDefault interface {
    isLazy__NoDefault()
}

type Lazy__NoDefault_Empty struct {}

func (_ Lazy__NoDefault_Empty) isLazy__NoDefault() {}

type Value struct {
    _0 NoDefault
}

func (_ Value) isLazy__NoDefault() {}

type Selected__int interface {
    isSelected__int()
}

type Values struct {
    _0 int
    _1 int
}

func (_ Values) isSelected__int() {}

type Ignored struct {
    _0 *ref_NoDefault_x
}

func (_ Ignored) isSelected__int() {}

func _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(self__0 Settings, other__1 Settings) bool {
    var jp204 bool
    var t208 bool = self__0.enabled
    var t209 bool = other__1.enabled
    var inline362 bool = t208 == t209
    jp204 = inline362
    var jp199 bool
    if jp204 {
        var t205 int = self__0.retries
        var t206 int = other__1.retries
        var inline364 bool = t205 == t206
        jp199 = inline364
    } else {
        jp199 = false
    }
    if jp199 {
        var t200 string = self__0.label
        var t201 string = other__1.label
        var inline366 bool = t200 == t201
        return inline366
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t213 bool
    t213 = false
    var t214 int
    t214 = 0
    var t215 string
    t215 = ""
    var t216 Settings = Settings{
        enabled: t213,
        retries: t214,
        label: t215,
    }
    return t216
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x173 bool = self__2.enabled
    var x174 int = self__2.retries
    var x175 string = self__2.label
    var t219 string = "Settings { " + "enabled: "
    var t220 string
    var inline375 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x173)
    t220 = inline375
    var t221 string = t219 + t220
    var t222 string = t221 + ", "
    var t223 string = t222 + "retries: "
    var t224 string
    var inline373 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x174)
    t224 = inline373
    var t225 string = t223 + t224
    var t226 string = t225 + ", "
    var t227 string = t226 + "label: "
    var t228 string
    var inline371 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x175)
    t228 = inline371
    var t229 string = t227 + t228
    var t230 string = t229 + " }"
    return t230
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t233 Empty = Empty{}
    return t233
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7.(type) {
    case Idle:
        return "State::Idle"
    case Running:
        var x176 int = self__7.(Running)._0
        var t242 string
        var inline377 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x176)
        t242 = inline377
        var t243 string = "State::Running(" + t242
        var t244 string = t243 + ")"
        return t244
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9.(type) {
    case Message_Data:
        var x177 string = self__9.(Message_Data)._0
        var x178 int = self__9.(Message_Data)._1
        var t254 string
        var inline383 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x177)
        t254 = inline383
        var t255 string = "Message::Data(" + t254
        var t256 string = t255 + ", "
        var t257 string
        var inline381 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x178)
        t257 = inline381
        var t258 string = t256 + t257
        var t259 string = t258 + ")"
        return t259
    case Message_Empty:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t278 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t278)
    var t279 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t280 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t279)
    var t281 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t280)
    println__T_string(t281)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t282 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t282)
    var pair__17 PairDefaults__int = _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default()
    var t283 int = pair__17.first
    var t284 int = pair__17.second
    var t285 int = t283 + t284
    var t286 string = _goml_m_inherent_i_int_i_int_i_to__string(t285)
    println__T_string(t286)
    var t287 Option__int = pair__17.nested
    var t288 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(t287)
    var t289 string
    var inline435 string = _goml_runtime_core_bool_to_string(t288)
    t289 = inline435
    println__T_string(t289)
    var state__18 State
    state__18 = Idle{}
    var t290 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t290)
    _goml_runtime_core_string_println(inline431)
    var message__19 Message
    var inline427 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline428 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline429 Message = Message_Data{
        _0: inline427,
        _1: inline428,
    }
    message__19 = inline429
    var t291 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline424 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t291)
    _goml_runtime_core_string_println(inline424)
    var event__20 Event
    var inline420 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline421 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline422 Event = Event_Data{
        _0: inline420,
        _1: inline421,
    }
    event__20 = inline422
    var t292 string
    switch event__20.(type) {
    case Event_Data:
        var inline407 string = event__20.(Event_Data)._0
        var inline408 int = event__20.(Event_Data)._1
        var inline411 string = "Event::Data { " + "name: "
        var inline412 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline407)
        var inline413 string = inline411 + inline412
        var inline414 string = inline413 + ", "
        var inline415 string = inline414 + "count: "
        var inline416 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline408)
        var inline417 string = inline415 + inline416
        var inline418 string = inline417 + " }"
        t292 = inline418
    case Event_Empty:
        t292 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline404 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t292)
    _goml_runtime_core_string_println(inline404)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault_Empty{}
    var jp294 string
    switch lazy__21.(type) {
    case Lazy__NoDefault_Empty:
        jp294 = "empty"
    case Value:
        jp294 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline400 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp294)
    _goml_runtime_core_string_println(inline400)
    var selected__22 Selected__int
    var inline396 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline397 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline398 Selected__int = Values{
        _0: inline396,
        _1: inline397,
    }
    selected__22 = inline398
    var jp296 string
    switch selected__22.(type) {
    case Values:
        var x191 int = selected__22.(Values)._0
        var x192 int = selected__22.(Values)._1
        var t298 int = x191 + x192
        var inline391 string = _goml_runtime_core_int_to_string(t298)
        jp296 = inline391
    case Ignored:
        jp296 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp296)
    _goml_runtime_core_string_println(inline393)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_int_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__84 int) string {
    var inline439 string = _goml_runtime_core_int_to_string(self__84)
    return inline439
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__82 string) string {
    return self__82
}

func println__T_string(value__31 string) struct{} {
    var t325 string
    t325 = value__31
    _goml_runtime_core_string_println(t325)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t329 string = _goml_runtime_core_bool_to_string(self__66)
    return t329
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default() PairDefaults__int {
    var t332 int
    t332 = 0
    var t333 int
    t333 = 0
    var t334 Option__int
    t334 = None{}
    var t335 PairDefaults__int = PairDefaults__int{
        first: t332,
        second: t333,
        nested: t334,
    }
    return t335
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t338 string = _goml_runtime_core_int_to_string(self__34)
    return t338
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(self__289 Option__int) bool {
    var t341 bool
    switch self__289.(type) {
    case None:
        t341 = false
    case Some:
        t341 = true
    default:
        panic("non-exhaustive match")
    }
    var t342 bool = !t341
    return t342
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t352 string = _goml_runtime_core_int_to_string(self__69)
    return t352
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
