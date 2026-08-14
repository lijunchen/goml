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
    var jp214 bool
    var t218 bool = self__0.enabled
    var t219 bool = other__1.enabled
    var inline372 bool = t218 == t219
    jp214 = inline372
    var jp209 bool
    if jp214 {
        var t215 int = self__0.retries
        var t216 int = other__1.retries
        var inline374 bool = t215 == t216
        jp209 = inline374
    } else {
        jp209 = false
    }
    if jp209 {
        var t210 string = self__0.label
        var t211 string = other__1.label
        var inline376 bool = t210 == t211
        return inline376
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t223 bool
    t223 = false
    var t224 int
    t224 = 0
    var t225 string
    t225 = ""
    var t226 Settings = Settings{
        enabled: t223,
        retries: t224,
        label: t225,
    }
    return t226
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x183 bool = self__2.enabled
    var x184 int = self__2.retries
    var x185 string = self__2.label
    var t229 string = "Settings { " + "enabled: "
    var t230 string
    var inline385 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x183)
    t230 = inline385
    var t231 string = t229 + t230
    var t232 string = t231 + ", "
    var t233 string = t232 + "retries: "
    var t234 string
    var inline383 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
    t234 = inline383
    var t235 string = t233 + t234
    var t236 string = t235 + ", "
    var t237 string = t236 + "label: "
    var t238 string
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x185)
    t238 = inline381
    var t239 string = t237 + t238
    var t240 string = t239 + " }"
    return t240
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t243 Empty = Empty{}
    return t243
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7.(type) {
    case Idle:
        return "State::Idle"
    case Running:
        var x186 int = self__7.(Running)._0
        var t252 string
        var inline387 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x186)
        t252 = inline387
        var t253 string = "State::Running(" + t252
        var t254 string = t253 + ")"
        return t254
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9.(type) {
    case Message_Data:
        var x187 string = self__9.(Message_Data)._0
        var x188 int = self__9.(Message_Data)._1
        var t264 string
        var inline393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x187)
        t264 = inline393
        var t265 string = "Message::Data(" + t264
        var t266 string = t265 + ", "
        var t267 string
        var inline391 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x188)
        t267 = inline391
        var t268 string = t266 + t267
        var t269 string = t268 + ")"
        return t269
    case Message_Empty:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t288 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t288)
    var t289 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t290 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t289)
    var t291 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t290)
    println__T_string(t291)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t292 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t292)
    var pair__17 PairDefaults__int = _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default()
    var t293 int = pair__17.first
    var t294 int = pair__17.second
    var t295 int = t293 + t294
    var t296 string = _goml_m_inherent_i_int_i_int_i_to__string(t295)
    println__T_string(t296)
    var t297 Option__int = pair__17.nested
    var t298 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(t297)
    var t299 string
    var inline445 string = _goml_runtime_core_bool_to_string(t298)
    t299 = inline445
    println__T_string(t299)
    var state__18 State
    state__18 = Idle{}
    var t300 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t300)
    _goml_runtime_core_string_println(inline441)
    var message__19 Message
    var inline437 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline438 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline439 Message = Message_Data{
        _0: inline437,
        _1: inline438,
    }
    message__19 = inline439
    var t301 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t301)
    _goml_runtime_core_string_println(inline434)
    var event__20 Event
    var inline430 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline431 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline432 Event = Event_Data{
        _0: inline430,
        _1: inline431,
    }
    event__20 = inline432
    var t302 string
    switch event__20.(type) {
    case Event_Data:
        var inline417 string = event__20.(Event_Data)._0
        var inline418 int = event__20.(Event_Data)._1
        var inline421 string = "Event::Data { " + "name: "
        var inline422 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline417)
        var inline423 string = inline421 + inline422
        var inline424 string = inline423 + ", "
        var inline425 string = inline424 + "count: "
        var inline426 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline418)
        var inline427 string = inline425 + inline426
        var inline428 string = inline427 + " }"
        t302 = inline428
    case Event_Empty:
        t302 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline414 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t302)
    _goml_runtime_core_string_println(inline414)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault_Empty{}
    var jp304 string
    switch lazy__21.(type) {
    case Lazy__NoDefault_Empty:
        jp304 = "empty"
    case Value:
        jp304 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline410 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp304)
    _goml_runtime_core_string_println(inline410)
    var selected__22 Selected__int
    var inline406 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline407 int = _goml_m_trait__impl_i_Default_i_int_i_default()
    var inline408 Selected__int = Values{
        _0: inline406,
        _1: inline407,
    }
    selected__22 = inline408
    var jp306 string
    switch selected__22.(type) {
    case Values:
        var x201 int = selected__22.(Values)._0
        var x202 int = selected__22.(Values)._1
        var t308 int = x201 + x202
        var inline401 string = _goml_runtime_core_int_to_string(t308)
        jp306 = inline401
    case Ignored:
        jp306 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp306)
    _goml_runtime_core_string_println(inline403)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_int_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__82 int) string {
    var inline449 string = _goml_runtime_core_int_to_string(self__82)
    return inline449
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__80 string) string {
    return self__80
}

func println__T_string(value__1 string) struct{} {
    var t335 string
    t335 = value__1
    _goml_runtime_core_string_println(t335)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t339 string = _goml_runtime_core_bool_to_string(self__64)
    return t339
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____int_i_default() PairDefaults__int {
    var t342 int
    t342 = 0
    var t343 int
    t343 = 0
    var t344 Option__int
    t344 = None{}
    var t345 PairDefaults__int = PairDefaults__int{
        first: t342,
        second: t343,
        nested: t344,
    }
    return t345
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t348 string = _goml_runtime_core_int_to_string(self__32)
    return t348
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__int(self__296 Option__int) bool {
    var t351 bool
    switch self__296.(type) {
    case None:
        t351 = false
    case Some:
        t351 = true
    default:
        panic("non-exhaustive match")
    }
    var t352 bool = !t351
    return t352
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t362 string = _goml_runtime_core_int_to_string(self__67)
    return t362
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
