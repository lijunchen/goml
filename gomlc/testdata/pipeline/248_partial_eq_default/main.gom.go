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
    var jp219 bool
    var t223 bool = self__0.enabled
    var t224 bool = other__1.enabled
    var inline377 bool = t223 == t224
    jp219 = inline377
    var jp214 bool
    if jp219 {
        var t220 int = self__0.retries
        var t221 int = other__1.retries
        var inline379 bool = t220 == t221
        jp214 = inline379
    } else {
        jp214 = false
    }
    if jp214 {
        var t215 string = self__0.label
        var t216 string = other__1.label
        var inline381 bool = t215 == t216
        return inline381
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t228 bool
    t228 = false
    var t229 int
    t229 = 0
    var t230 string
    t230 = ""
    var t231 Settings = Settings{
        enabled: t228,
        retries: t229,
        label: t230,
    }
    return t231
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x188 bool = self__2.enabled
    var x189 int = self__2.retries
    var x190 string = self__2.label
    var t234 string = "Settings { " + "enabled: "
    var t235 string
    var inline390 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x188)
    t235 = inline390
    var t236 string = t234 + t235
    var t237 string = t236 + ", "
    var t238 string = t237 + "retries: "
    var t239 string
    var inline388 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
    t239 = inline388
    var t240 string = t238 + t239
    var t241 string = t240 + ", "
    var t242 string = t241 + "label: "
    var t243 string
    var inline386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x190)
    t243 = inline386
    var t244 string = t242 + t243
    var t245 string = t244 + " }"
    return t245
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t248 Empty = Empty{}
    return t248
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7.(type) {
    case Idle:
        return "State::Idle"
    case Running:
        var x191 int = self__7.(Running)._0
        var t257 string
        var inline392 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x191)
        t257 = inline392
        var t258 string = "State::Running(" + t257
        var t259 string = t258 + ")"
        return t259
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9.(type) {
    case Message_Data:
        var x192 string = self__9.(Message_Data)._0
        var x193 int = self__9.(Message_Data)._1
        var t269 string
        var inline398 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x192)
        t269 = inline398
        var t270 string = "Message::Data(" + t269
        var t271 string = t270 + ", "
        var t272 string
        var inline396 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x193)
        t272 = inline396
        var t273 string = t271 + t272
        var t274 string = t273 + ")"
        return t274
    case Message_Empty:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t293 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t293)
    var t294 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t295 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t294)
    var t296 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t295)
    println__T_string(t296)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t297 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t297)
    var pair__17 PairDefaults__int = _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default()
    var t298 int = pair__17.first
    var t299 int = pair__17.second
    var t300 int = t298 + t299
    var t301 string = _goml_m_inherent_i_int_i_int_i_to__string(t300)
    println__T_string(t301)
    var t302 Option__int = pair__17.nested
    var t303 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(t302)
    var t304 string
    var inline450 string = _goml_runtime_core_bool_to_string(t303)
    t304 = inline450
    println__T_string(t304)
    var state__18 State
    state__18 = Idle{}
    var t305 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline446 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t305)
    _goml_runtime_core_string_println(inline446)
    var message__19 Message
    var inline442 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline443 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline444 Message = Message_Data{
        _0: inline442,
        _1: inline443,
    }
    message__19 = inline444
    var t306 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline439 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t306)
    _goml_runtime_core_string_println(inline439)
    var event__20 Event
    var inline435 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline436 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline437 Event = Event_Data{
        _0: inline435,
        _1: inline436,
    }
    event__20 = inline437
    var t307 string
    switch event__20.(type) {
    case Event_Data:
        var inline422 string = event__20.(Event_Data)._0
        var inline423 int = event__20.(Event_Data)._1
        var inline426 string = "Event::Data { " + "name: "
        var inline427 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline422)
        var inline428 string = inline426 + inline427
        var inline429 string = inline428 + ", "
        var inline430 string = inline429 + "count: "
        var inline431 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline423)
        var inline432 string = inline430 + inline431
        var inline433 string = inline432 + " }"
        t307 = inline433
    case Event_Empty:
        t307 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline419 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t307)
    _goml_runtime_core_string_println(inline419)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault_Empty{}
    var jp309 string
    switch lazy__21.(type) {
    case Lazy__NoDefault_Empty:
        jp309 = "empty"
    case Value:
        jp309 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline415 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp309)
    _goml_runtime_core_string_println(inline415)
    var selected__22 Selected__int
    var inline411 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline412 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline413 Selected__int = Values{
        _0: inline411,
        _1: inline412,
    }
    selected__22 = inline413
    var jp311 string
    switch selected__22.(type) {
    case Values:
        var x206 int = selected__22.(Values)._0
        var x207 int = selected__22.(Values)._1
        var t313 int = x206 + x207
        var inline406 string = _goml_runtime_core_int_to_string(t313)
        jp311 = inline406
    case Ignored:
        jp311 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp311)
    _goml_runtime_core_string_println(inline408)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_int_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__82 int) string {
    var inline454 string = _goml_runtime_core_int_to_string(self__82)
    return inline454
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__80 string) string {
    return self__80
}

func println__T_string(value__1 string) struct{} {
    var t340 string
    t340 = value__1
    _goml_runtime_core_string_println(t340)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t344 string = _goml_runtime_core_bool_to_string(self__64)
    return t344
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default() PairDefaults__int {
    var t347 int
    t347 = 0
    var t348 int
    t348 = 0
    var t349 Option__int
    t349 = None{}
    var t350 PairDefaults__int = PairDefaults__int{
        first: t347,
        second: t348,
        nested: t349,
    }
    return t350
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t353 string = _goml_runtime_core_int_to_string(self__32)
    return t353
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(self__299 Option__int) bool {
    var t356 bool
    switch self__299.(type) {
    case None:
        t356 = false
    case Some:
        t356 = true
    default:
        panic("non-exhaustive match")
    }
    var t357 bool = !t356
    return t357
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t367 string = _goml_runtime_core_int_to_string(self__67)
    return t367
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
