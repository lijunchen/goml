package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_EnvEntry struct {
    items []EnvEntry
}

func vec_new__Vec_8EnvEntry() *_goml_vec_EnvEntry {
    return &_goml_vec_EnvEntry{
        items: nil,
    }
}

func vec_push__Vec_8EnvEntry(vec *_goml_vec_EnvEntry, elem EnvEntry) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_8EnvEntry(vec *_goml_vec_EnvEntry, index int32) EnvEntry {
    return vec.items[index]
}

func vec_len__Vec_8EnvEntry(vec *_goml_vec_EnvEntry) int32 {
    return int32(len(vec.items))
}

type _goml_vec_SubstEntry struct {
    items []SubstEntry
}

func vec_new__Vec_10SubstEntry() *_goml_vec_SubstEntry {
    return &_goml_vec_SubstEntry{
        items: nil,
    }
}

func vec_push__Vec_10SubstEntry(vec *_goml_vec_SubstEntry, elem SubstEntry) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_10SubstEntry(vec *_goml_vec_SubstEntry, index int32) SubstEntry {
    return vec.items[index]
}

func vec_len__Vec_10SubstEntry(vec *_goml_vec_SubstEntry) int32 {
    return int32(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Tv_x struct {
    value Tv
}

func ref__Ref_2Tv(value Tv) *ref_Tv_x {
    return &ref_Tv_x{
        value: value,
    }
}

func ref_get__Ref_2Tv(reference *ref_Tv_x) Tv {
    return reference.value
}

func ref_set__Ref_2Tv(reference *ref_Tv_x, value Tv) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_2Tv(a *ref_Tv_x, b *ref_Tv_x) bool {
    return a == b
}

type ref_Option__Typ_x struct {
    value Option__Typ
}

func ref__Ref_11Option__Typ(value Option__Typ) *ref_Option__Typ_x {
    return &ref_Option__Typ_x{
        value: value,
    }
}

func ref_get__Ref_11Option__Typ(reference *ref_Option__Typ_x) Option__Typ {
    return reference.value
}

func ref_set__Ref_11Option__Typ(reference *ref_Option__Typ_x, value Option__Typ) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_3Typ_3Typ struct {
    _0 Typ
    _1 Typ
}

type Tuple2_3Typ_16Vec_10SubstEntry struct {
    _0 Typ
    _1 *_goml_vec_SubstEntry
}

type CheckerState struct {
    gensym_counter *ref_int32_x
    current_level *ref_int32_x
}

type EnvEntry struct {
    name string
    ty Typ
}

type SubstEntry struct {
    name string
    ty Typ
}

type Exp interface {
    isExp()
}

type Var struct {
    _0 string
}

func (_ Var) isExp() {}

type App struct {
    _0 Exp
    _1 Exp
}

func (_ App) isExp() {}

type Lam struct {
    _0 string
    _1 Exp
}

func (_ Lam) isExp() {}

type Let struct {
    _0 string
    _1 Exp
    _2 Exp
}

func (_ Let) isExp() {}

type Typ interface {
    isTyp()
}

type TVar struct {
    _0 *ref_Tv_x
}

func (_ TVar) isTyp() {}

type QVar struct {
    _0 string
}

func (_ QVar) isTyp() {}

type TArrow struct {
    _0 Typ
    _1 Typ
}

func (_ TArrow) isTyp() {}

type Tv interface {
    isTv()
}

type Unbound struct {
    _0 string
    _1 int32
}

func (_ Unbound) isTv() {}

type Link struct {
    _0 Typ
}

func (_ Link) isTv() {}

type Option__Typ interface {
    isOption__Typ()
}

type None struct {}

func (_ None) isOption__Typ() {}

type Some struct {
    _0 Typ
}

func (_ Some) isOption__Typ() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__Typ__string interface {
    isResult__Typ__string()
}

type Result__Typ__string_Ok struct {
    _0 Typ
}

func (_ Result__Typ__string_Ok) isResult__Typ__string() {}

type Result__Typ__string_Err struct {
    _0 string
}

func (_ Result__Typ__string_Err) isResult__Typ__string() {}

func state_new() CheckerState {
    var retv209 CheckerState
    var t210 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t211 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t212 CheckerState = CheckerState{
        gensym_counter: t210,
        current_level: t211,
    }
    retv209 = t212
    return retv209
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t214 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t214, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t216 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t216, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t220 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t220)
    var t221 *ref_int32_x = st__3.current_level
    var t222 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t221, t222)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t224 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t224)
    var t225 *ref_int32_x = st__5.current_level
    var t226 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t225, t226)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv228 int32
    var t231 bool = a__7 < b__8
    var jp230 int32
    if t231 {
        jp230 = a__7
    } else {
        jp230 = b__8
    }
    retv228 = jp230
    return retv228
}

func nth_letter(n__9 int32) rune {
    var retv233 rune
    var jp235 rune
    switch n__9 {
    case 0:
        jp235 = 97
    case 1:
        jp235 = 98
    case 2:
        jp235 = 99
    case 3:
        jp235 = 100
    case 4:
        jp235 = 101
    case 5:
        jp235 = 102
    case 6:
        jp235 = 103
    case 7:
        jp235 = 104
    case 8:
        jp235 = 105
    case 9:
        jp235 = 106
    case 10:
        jp235 = 107
    case 11:
        jp235 = 108
    case 12:
        jp235 = 109
    case 13:
        jp235 = 110
    case 14:
        jp235 = 111
    case 15:
        jp235 = 112
    case 16:
        jp235 = 113
    case 17:
        jp235 = 114
    case 18:
        jp235 = 115
    case 19:
        jp235 = 116
    case 20:
        jp235 = 117
    case 21:
        jp235 = 118
    case 22:
        jp235 = 119
    case 23:
        jp235 = 120
    case 24:
        jp235 = 121
    case 25:
        jp235 = 122
    default:
        jp235 = 97
    }
    retv233 = jp235
    return retv233
}

func gensym(st__10 CheckerState) string {
    var retv237 string
    var t238 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t238)
    var t239 *ref_int32_x = st__10.gensym_counter
    var t240 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t239, t240)
    var t243 bool = n__11 < 26
    var jp242 string
    if t243 {
        var t244 rune = nth_letter(n__11)
        var t245 string = _goml_m_inherent_i_char_i_char_i_to__string(t244)
        jp242 = t245
    } else {
        var t246 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t247 string = "t" + t246
        jp242 = t247
    }
    retv237 = jp242
    return retv237
}

func newvar(st__12 CheckerState) Typ {
    var retv249 Typ
    var name__13 string = gensym(st__12)
    var t250 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t250)
    var t251 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t252 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t251)
    var t253 Typ = TVar{
        _0: t252,
    }
    retv249 = t253
    return retv249
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv255 bool
    var jp257 bool
    switch ty__15.(type) {
    case TVar:
        var x10 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x10
        var mtmp14 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp259 bool
        switch mtmp14.(type) {
        case Unbound:
            jp259 = false
        case Link:
            var x17 Typ = mtmp14.(Link)._0
            var inner__17 Typ = x17
            var t260 bool = typ_is_arrow(inner__17)
            jp259 = t260
        default:
            panic("non-exhaustive match")
        }
        jp257 = jp259
    case QVar:
        jp257 = false
    case TArrow:
        jp257 = true
    default:
        panic("non-exhaustive match")
    }
    retv255 = jp257
    return retv255
}

func typ_to_string(ty__18 Typ) string {
    var retv262 string
    var jp264 string
    switch ty__18.(type) {
    case TVar:
        var x18 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x18
        var mtmp22 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp266 string
        switch mtmp22.(type) {
        case Unbound:
            var x23 string = mtmp22.(Unbound)._0
            var name__21 string = x23
            var t267 string = "'" + name__21
            jp266 = t267
        case Link:
            var x25 Typ = mtmp22.(Link)._0
            var inner__22 Typ = x25
            var t268 string = typ_to_string(inner__22)
            jp266 = t268
        default:
            panic("non-exhaustive match")
        }
        jp264 = jp266
    case QVar:
        var x19 string = ty__18.(QVar)._0
        var name__19 string = x19
        var t269 string = "'" + name__19
        jp264 = t269
    case TArrow:
        var x20 Typ = ty__18.(TArrow)._0
        var x21 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x21
        var t1__23 Typ = x20
        var t274 bool = typ_is_arrow(t1__23)
        var jp271 string
        if t274 {
            var t275 string = typ_to_string(t1__23)
            var t276 string = "(" + t275
            var t277 string = t276 + ")"
            jp271 = t277
        } else {
            var t278 string = typ_to_string(t1__23)
            jp271 = t278
        }
        var s1__25 string = jp271
        var s2__26 string = typ_to_string(t2__24)
        var t272 string = s1__25 + " -> "
        var t273 string = t272 + s2__26
        jp264 = t273
    default:
        panic("non-exhaustive match")
    }
    retv262 = jp264
    return retv262
}

func env_empty() *_goml_vec_EnvEntry {
    var retv280 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv280 = env__27
    return retv280
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv282 Option__Typ
    var t283 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t284 int32 = t283 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t284)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop287:
    for {
        var t300 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t301 bool = !t300
        var jp289 bool
        if t301 {
            var t302 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t303 bool = t302 >= 0
            jp289 = t303
        } else {
            jp289 = false
        }
        if jp289 {
            var t290 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t290)
            var t292 string = entry__33.name
            var t293 bool = t292 == name__29
            if t293 {
                var t294 Typ = entry__33.ty
                var t295 Option__Typ = Some{
                    _0: t294,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t295)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t297 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t298 int32 = t297 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t298)
            }
            continue
        } else {
            break Loop_loop287
        }
    }
    var t286 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv282 = t286
    return retv282
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv305 Option__Typ
    var t306 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t307 int32 = t306 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t307)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop310:
    for {
        var t323 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t324 bool = !t323
        var jp312 bool
        if t324 {
            var t325 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t326 bool = t325 >= 0
            jp312 = t326
        } else {
            jp312 = false
        }
        if jp312 {
            var t313 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t313)
            var t315 string = entry__39.name
            var t316 bool = t315 == name__35
            if t316 {
                var t317 Typ = entry__39.ty
                var t318 Option__Typ = Some{
                    _0: t317,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t318)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t320 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t321 int32 = t320 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t321)
            }
            continue
        } else {
            break Loop_loop310
        }
    }
    var t309 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv305 = t309
    return retv305
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv328 Result__unit__string
    var jp330 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x30 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x30
        var t333 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp332 Result__unit__string
        if t333 {
            var t334 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp332 = t334
        } else {
            var mtmp34 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp336 Result__unit__string
            switch mtmp34.(type) {
            case Unbound:
                var x35 string = mtmp34.(Unbound)._0
                var x36 int32 = mtmp34.(Unbound)._1
                var l2__45 int32 = x36
                var name__44 string = x35
                var mtmp38 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp338 int32
                switch mtmp38.(type) {
                case Unbound:
                    var x40 int32 = mtmp38.(Unbound)._1
                    var l__46 int32 = x40
                    var t341 int32 = min_i32(l__46, l2__45)
                    jp338 = t341
                case Link:
                    jp338 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp338
                var t339 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t339)
                var t340 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp336 = t340
            case Link:
                var x37 Typ = mtmp34.(Link)._0
                var inner__48 Typ = x37
                var t342 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp336 = t342
            default:
                panic("non-exhaustive match")
            }
            jp332 = jp336
        }
        jp330 = jp332
    case QVar:
        var t343 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp330 = t343
    case TArrow:
        var x32 Typ = ty__42.(TArrow)._0
        var x33 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x33
        var t1__49 Typ = x32
        var mtmp43 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp345 Result__unit__string
        switch mtmp43.(type) {
        case Result__unit__string_Ok:
            var t346 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp345 = t346
        case Result__unit__string_Err:
            var x45 string = mtmp43.(Result__unit__string_Err)._0
            var e__51 string = x45
            var t347 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp345 = t347
        default:
            panic("non-exhaustive match")
        }
        jp330 = jp345
    default:
        panic("non-exhaustive match")
    }
    retv328 = jp330
    return retv328
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv349 Result__unit__string
    var mtmp46 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x47 Typ = mtmp46._0
    var x48 Typ = mtmp46._1
    var jp351 Result__unit__string
    switch x48.(type) {
    case TVar:
        var x49 *ref_Tv_x = x48.(TVar)._0
        var jp353 Result__unit__string
        switch x47.(type) {
        case TVar:
            var x53 *ref_Tv_x = x47.(TVar)._0
            var r1__55 *ref_Tv_x = x53
            var r2__56 *ref_Tv_x = x49
            var t356 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp355 Result__unit__string
            if t356 {
                var t357 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp355 = t357
            } else {
                var mtmp57 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp359 Result__unit__string
                switch mtmp57.(type) {
                case Unbound:
                    var mtmp61 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp361 Result__unit__string
                    switch mtmp61.(type) {
                    case Unbound:
                        var t362 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp65 Result__unit__string = occurs(st__52, r1__55, t362)
                        var jp364 Result__unit__string
                        switch mtmp65.(type) {
                        case Result__unit__string_Ok:
                            var t365 Typ = TVar{
                                _0: r2__56,
                            }
                            var t366 Tv = Link{
                                _0: t365,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t366)
                            var t367 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp364 = t367
                        case Result__unit__string_Err:
                            var x67 string = mtmp65.(Result__unit__string_Err)._0
                            var e__59 string = x67
                            var t368 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp364 = t368
                        default:
                            panic("non-exhaustive match")
                        }
                        jp361 = jp364
                    case Link:
                        var x64 Typ = mtmp61.(Link)._0
                        var inner__58 Typ = x64
                        var t369 Typ = TVar{
                            _0: r1__55,
                        }
                        var t370 Result__unit__string = unify(st__52, t369, inner__58)
                        jp361 = t370
                    default:
                        panic("non-exhaustive match")
                    }
                    jp359 = jp361
                case Link:
                    var x60 Typ = mtmp57.(Link)._0
                    var inner__57 Typ = x60
                    var t371 Typ = TVar{
                        _0: r2__56,
                    }
                    var t372 Result__unit__string = unify(st__52, inner__57, t371)
                    jp359 = t372
                default:
                    panic("non-exhaustive match")
                }
                jp355 = jp359
            }
            jp353 = jp355
        case QVar:
            var r2__65 *ref_Tv_x = x49
            var other__64 Typ = x47
            var mtmp69 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp374 Result__unit__string
            switch mtmp69.(type) {
            case Unbound:
                var mtmp73 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp376 Result__unit__string
                switch mtmp73.(type) {
                case Result__unit__string_Ok:
                    var t377 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t377)
                    var t378 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp376 = t378
                case Result__unit__string_Err:
                    var x75 string = mtmp73.(Result__unit__string_Err)._0
                    var e__67 string = x75
                    var t379 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp376 = t379
                default:
                    panic("non-exhaustive match")
                }
                jp374 = jp376
            case Link:
                var x72 Typ = mtmp69.(Link)._0
                var inner__66 Typ = x72
                var t380 Result__unit__string = unify(st__52, other__64, inner__66)
                jp374 = t380
            default:
                panic("non-exhaustive match")
            }
            jp353 = jp374
        case TArrow:
            var r2__65 *ref_Tv_x = x49
            var other__64 Typ = x47
            var mtmp77 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp382 Result__unit__string
            switch mtmp77.(type) {
            case Unbound:
                var mtmp81 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp384 Result__unit__string
                switch mtmp81.(type) {
                case Result__unit__string_Ok:
                    var t385 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t385)
                    var t386 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp384 = t386
                case Result__unit__string_Err:
                    var x83 string = mtmp81.(Result__unit__string_Err)._0
                    var e__67 string = x83
                    var t387 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp384 = t387
                default:
                    panic("non-exhaustive match")
                }
                jp382 = jp384
            case Link:
                var x80 Typ = mtmp77.(Link)._0
                var inner__66 Typ = x80
                var t388 Result__unit__string = unify(st__52, other__64, inner__66)
                jp382 = t388
            default:
                panic("non-exhaustive match")
            }
            jp353 = jp382
        default:
            panic("non-exhaustive match")
        }
        jp351 = jp353
    case QVar:
        var jp390 Result__unit__string
        switch x47.(type) {
        case TVar:
            var x85 *ref_Tv_x = x47.(TVar)._0
            var r1__60 *ref_Tv_x = x85
            var other__61 Typ = x48
            var mtmp89 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp392 Result__unit__string
            switch mtmp89.(type) {
            case Unbound:
                var mtmp93 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp394 Result__unit__string
                switch mtmp93.(type) {
                case Result__unit__string_Ok:
                    var t395 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t395)
                    var t396 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp394 = t396
                case Result__unit__string_Err:
                    var x95 string = mtmp93.(Result__unit__string_Err)._0
                    var e__63 string = x95
                    var t397 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp394 = t397
                default:
                    panic("non-exhaustive match")
                }
                jp392 = jp394
            case Link:
                var x92 Typ = mtmp89.(Link)._0
                var inner__62 Typ = x92
                var t398 Result__unit__string = unify(st__52, inner__62, other__61)
                jp392 = t398
            default:
                panic("non-exhaustive match")
            }
            jp390 = jp392
        case QVar:
            var t399 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp390 = t399
        case TArrow:
            var t400 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp390 = t400
        default:
            panic("non-exhaustive match")
        }
        jp351 = jp390
    case TArrow:
        var x51 Typ = x48.(TArrow)._0
        var x52 Typ = x48.(TArrow)._1
        var jp402 Result__unit__string
        switch x47.(type) {
        case TVar:
            var x97 *ref_Tv_x = x47.(TVar)._0
            var r1__60 *ref_Tv_x = x97
            var other__61 Typ = x48
            var mtmp101 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp404 Result__unit__string
            switch mtmp101.(type) {
            case Unbound:
                var mtmp105 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp406 Result__unit__string
                switch mtmp105.(type) {
                case Result__unit__string_Ok:
                    var t407 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t407)
                    var t408 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp406 = t408
                case Result__unit__string_Err:
                    var x107 string = mtmp105.(Result__unit__string_Err)._0
                    var e__63 string = x107
                    var t409 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp406 = t409
                default:
                    panic("non-exhaustive match")
                }
                jp404 = jp406
            case Link:
                var x104 Typ = mtmp101.(Link)._0
                var inner__62 Typ = x104
                var t410 Result__unit__string = unify(st__52, inner__62, other__61)
                jp404 = t410
            default:
                panic("non-exhaustive match")
            }
            jp402 = jp404
        case QVar:
            var t411 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp402 = t411
        case TArrow:
            var x99 Typ = x47.(TArrow)._0
            var x100 Typ = x47.(TArrow)._1
            var a2__69 Typ = x100
            var a1__68 Typ = x99
            var b2__71 Typ = x52
            var b1__70 Typ = x51
            var mtmp109 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp413 Result__unit__string
            switch mtmp109.(type) {
            case Result__unit__string_Ok:
                var t414 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp413 = t414
            case Result__unit__string_Err:
                var x111 string = mtmp109.(Result__unit__string_Err)._0
                var e__72 string = x111
                var t415 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp413 = t415
            default:
                panic("non-exhaustive match")
            }
            jp402 = jp413
        default:
            panic("non-exhaustive match")
        }
        jp351 = jp402
    default:
        panic("non-exhaustive match")
    }
    retv349 = jp351
    return retv349
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv417 Typ
    var jp419 Typ
    switch ty__74.(type) {
    case TVar:
        var x112 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x112
        var mtmp116 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp421 Typ
        switch mtmp116.(type) {
        case Unbound:
            var x117 string = mtmp116.(Unbound)._0
            var x118 int32 = mtmp116.(Unbound)._1
            var l__77 int32 = x118
            var name__76 string = x117
            var t422 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t422)
            var t425 bool = l__77 > cur__78
            var jp424 Typ
            if t425 {
                var t426 Typ = QVar{
                    _0: name__76,
                }
                jp424 = t426
            } else {
                var t427 Typ = TVar{
                    _0: tvref__75,
                }
                jp424 = t427
            }
            jp421 = jp424
        case Link:
            var x119 Typ = mtmp116.(Link)._0
            var inner__79 Typ = x119
            var t428 Typ = gen(st__73, inner__79)
            jp421 = t428
        default:
            panic("non-exhaustive match")
        }
        jp419 = jp421
    case QVar:
        var other__82 Typ = ty__74
        jp419 = other__82
    case TArrow:
        var x114 Typ = ty__74.(TArrow)._0
        var x115 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x115
        var t1__80 Typ = x114
        var t429 Typ = gen(st__73, t1__80)
        var t430 Typ = gen(st__73, t2__81)
        var t431 Typ = TArrow{
            _0: t429,
            _1: t430,
        }
        jp419 = t431
    default:
        panic("non-exhaustive match")
    }
    retv417 = jp419
    return retv417
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv433 Tuple2_3Typ_16Vec_10SubstEntry
    var jp435 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x120 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x120
        var mtmp124 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp437 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp124.(type) {
        case Unbound:
            var t438 Typ = TVar{
                _0: tvref__90,
            }
            var t439 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t438,
                _1: subst__84,
            }
            jp437 = t439
        case Link:
            var x127 Typ = mtmp124.(Link)._0
            var inner__91 Typ = x127
            var t440 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp437 = t440
        default:
            panic("non-exhaustive match")
        }
        jp435 = jp437
    case QVar:
        var x121 string = ty__85.(QVar)._0
        var name__86 string = x121
        var mtmp128 Option__Typ = subst_lookup(subst__84, name__86)
        var jp442 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp128.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t443 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t443)
            var t444 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp442 = t444
        case Some:
            var x129 Typ = mtmp128.(Some)._0
            var t__87 Typ = x129
            var t445 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp442 = t445
        default:
            panic("non-exhaustive match")
        }
        jp435 = jp442
    case TArrow:
        var x122 Typ = ty__85.(TArrow)._0
        var x123 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x123
        var t1__92 Typ = x122
        var mtmp130 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x131 Typ = mtmp130._0
        var x132 *_goml_vec_SubstEntry = mtmp130._1
        var subst1__95 *_goml_vec_SubstEntry = x132
        var ty1__94 Typ = x131
        var mtmp133 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x134 Typ = mtmp133._0
        var x135 *_goml_vec_SubstEntry = mtmp133._1
        var subst2__97 *_goml_vec_SubstEntry = x135
        var ty2__96 Typ = x134
        var t446 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t447 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t446,
            _1: subst2__97,
        }
        jp435 = t447
    default:
        panic("non-exhaustive match")
    }
    retv433 = jp435
    return retv433
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv449 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp136 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x137 Typ = mtmp136._0
    var t__101 Typ = x137
    retv449 = t__101
    return retv449
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv451 Result__Typ__string
    var jp453 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x139 string = e__104.(Var)._0
        var x__105 string = x139
        var mtmp147 Option__Typ = env_lookup(env__103, x__105)
        var jp455 Result__Typ__string
        switch mtmp147.(type) {
        case None:
            var t456 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp455 = t456
        case Some:
            var x148 Typ = mtmp147.(Some)._0
            var ty__106 Typ = x148
            var t457 Typ = inst(st__102, ty__106)
            var t458 Result__Typ__string = Result__Typ__string_Ok{
                _0: t457,
            }
            jp455 = t458
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp455
    case App:
        var x140 Exp = e__104.(App)._0
        var x141 Exp = e__104.(App)._1
        var e2__114 Exp = x141
        var e1__113 Exp = x140
        var mtmp149 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp460 Result__Typ__string
        switch mtmp149.(type) {
        case Result__Typ__string_Ok:
            var x150 Typ = mtmp149.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x150
            var mtmp152 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp462 Result__Typ__string
            switch mtmp152.(type) {
            case Result__Typ__string_Ok:
                var x153 Typ = mtmp152.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x153
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp155 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp464 Result__Typ__string
                switch mtmp155.(type) {
                case Result__unit__string_Ok:
                    var t465 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp464 = t465
                case Result__unit__string_Err:
                    var x157 string = mtmp155.(Result__unit__string_Err)._0
                    var e__121 string = x157
                    var t466 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp464 = t466
                default:
                    panic("non-exhaustive match")
                }
                jp462 = jp464
            case Result__Typ__string_Err:
                var x154 string = mtmp152.(Result__Typ__string_Err)._0
                var e__117 string = x154
                var t467 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp462 = t467
            default:
                panic("non-exhaustive match")
            }
            jp460 = jp462
        case Result__Typ__string_Err:
            var x151 string = mtmp149.(Result__Typ__string_Err)._0
            var e__115 string = x151
            var t468 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp460 = t468
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp460
    case Lam:
        var x142 string = e__104.(Lam)._0
        var x143 Exp = e__104.(Lam)._1
        var body__108 Exp = x143
        var x__107 string = x142
        var ty_x__109 Typ = newvar(st__102)
        var t469 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t469)
        var mtmp158 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp471 Result__Typ__string
        switch mtmp158.(type) {
        case Result__Typ__string_Ok:
            var x159 Typ = mtmp158.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x159
            var t472 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t473 Result__Typ__string = Result__Typ__string_Ok{
                _0: t472,
            }
            jp471 = t473
        case Result__Typ__string_Err:
            var x160 string = mtmp158.(Result__Typ__string_Err)._0
            var e__112 string = x160
            var t474 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp471 = t474
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp471
    case Let:
        var x144 string = e__104.(Let)._0
        var x145 Exp = e__104.(Let)._1
        var x146 Exp = e__104.(Let)._2
        var e2__124 Exp = x146
        var e1__123 Exp = x145
        var x__122 string = x144
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp476 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x163 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x163
            var t477 Typ = gen(st__102, ty1__127)
            var t478 EnvEntry = EnvEntry{
                name: x__122,
                ty: t477,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t478)
            var t479 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp476 = t479
        case Result__Typ__string_Err:
            var x164 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x164
            var t480 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp476 = t480
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp476
    default:
        panic("non-exhaustive match")
    }
    retv451 = jp453
    return retv451
}

func exp_var(name__129 string) Exp {
    var retv482 Exp
    var t483 Exp = Var{
        _0: name__129,
    }
    retv482 = t483
    return retv482
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv485 Exp
    var t486 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv485 = t486
    return retv485
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv488 Exp
    var t489 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv488 = t489
    return retv488
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv491 Exp
    var t492 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv491 = t492
    return retv491
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x165 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x165
        var t495 string = label__137 + ": "
        var t496 string = typ_to_string(ty__139)
        var t497 string = t495 + t496
        println__T_string(t497)
    case Result__Typ__string_Err:
        var x166 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x166
        var t499 string = label__137 + ": "
        var t500 string = t499 + e__140
        println__T_string(t500)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t503 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t503)
    var t504 Exp = exp_var("x")
    var t505 Exp = exp_var("y")
    var t506 Exp = exp_app(t504, t505)
    var t507 Exp = exp_lam("y", t506)
    var c1__143 Exp = exp_lam("x", t507)
    reset_type_variables(st__141)
    var t508 *_goml_vec_EnvEntry = env_empty()
    var t509 Result__Typ__string = typeof(st__141, t508, id__142)
    show_result("id", t509)
    reset_type_variables(st__141)
    var t510 *_goml_vec_EnvEntry = env_empty()
    var t511 Result__Typ__string = typeof(st__141, t510, c1__143)
    show_result("c1", t511)
    reset_type_variables(st__141)
    var t512 *_goml_vec_EnvEntry = env_empty()
    var t513 Exp = exp_var("x")
    var t514 Exp = exp_let("x", c1__143, t513)
    var t515 Result__Typ__string = typeof(st__141, t512, t514)
    show_result("let_x_c1_x", t515)
    reset_type_variables(st__141)
    var t516 *_goml_vec_EnvEntry = env_empty()
    var t517 Exp = exp_var("z")
    var t518 Exp = exp_lam("z", t517)
    var t519 Exp = exp_var("y")
    var t520 Exp = exp_let("y", t518, t519)
    var t521 Result__Typ__string = typeof(st__141, t516, t520)
    show_result("let_y_id_y", t521)
    reset_type_variables(st__141)
    var t522 *_goml_vec_EnvEntry = env_empty()
    var t523 Exp = exp_var("z")
    var t524 Exp = exp_lam("z", t523)
    var t525 Exp = exp_var("y")
    var t526 Exp = exp_let("y", t524, t525)
    var t527 Exp = exp_lam("x", t526)
    var t528 Result__Typ__string = typeof(st__141, t522, t527)
    show_result("lam_x_let_y_id_y", t528)
    reset_type_variables(st__141)
    var t529 *_goml_vec_EnvEntry = env_empty()
    var t530 Exp = exp_var("z")
    var t531 Exp = exp_lam("z", t530)
    var t532 Exp = exp_var("y")
    var t533 Exp = exp_var("x")
    var t534 Exp = exp_app(t532, t533)
    var t535 Exp = exp_let("y", t531, t534)
    var t536 Exp = exp_lam("x", t535)
    var t537 Result__Typ__string = typeof(st__141, t529, t536)
    show_result("lam_x_let_y_id_yx", t537)
    reset_type_variables(st__141)
    var t538 *_goml_vec_EnvEntry = env_empty()
    var t539 Exp = exp_var("x")
    var t540 Exp = exp_var("x")
    var t541 Exp = exp_app(t539, t540)
    var t542 Exp = exp_lam("x", t541)
    var t543 Result__Typ__string = typeof(st__141, t538, t542)
    show_result("self_apply", t543)
    reset_type_variables(st__141)
    var t544 *_goml_vec_EnvEntry = env_empty()
    var t545 Exp = exp_var("x")
    var t546 Exp = exp_var("x")
    var t547 Exp = exp_let("x", t545, t546)
    var t548 Result__Typ__string = typeof(st__141, t544, t547)
    show_result("unbound_var", t548)
    reset_type_variables(st__141)
    var t549 *_goml_vec_EnvEntry = env_empty()
    var t550 Exp = exp_var("y")
    var t551 Exp = exp_var("y")
    var t552 Exp = exp_var("z")
    var t553 Exp = exp_app(t551, t552)
    var t554 Exp = exp_lam("z", t553)
    var t555 Exp = exp_app(t550, t554)
    var t556 Exp = exp_lam("y", t555)
    var t557 Result__Typ__string = typeof(st__141, t549, t556)
    show_result("max_heiber", t557)
    reset_type_variables(st__141)
    var t558 *_goml_vec_EnvEntry = env_empty()
    var t559 Exp = exp_var("k")
    var t560 Exp = exp_var("k")
    var t561 Exp = exp_var("x")
    var t562 Exp = exp_app(t560, t561)
    var t563 Exp = exp_var("y")
    var t564 Exp = exp_app(t562, t563)
    var t565 Exp = exp_app(t559, t564)
    var t566 Exp = exp_var("k")
    var t567 Exp = exp_var("y")
    var t568 Exp = exp_app(t566, t567)
    var t569 Exp = exp_var("x")
    var t570 Exp = exp_app(t568, t569)
    var t571 Exp = exp_app(t565, t570)
    var t572 Exp = exp_lam("k", t571)
    var t573 Exp = exp_lam("y", t572)
    var t574 Exp = exp_lam("x", t573)
    var t575 Result__Typ__string = typeof(st__141, t558, t574)
    show_result("kirang", t575)
    reset_type_variables(st__141)
    var t576 *_goml_vec_EnvEntry = env_empty()
    var t577 Exp = exp_var("id")
    var t578 Exp = exp_var("id")
    var t579 Exp = exp_app(t577, t578)
    var t580 Exp = exp_let("id", id__142, t579)
    var t581 Result__Typ__string = typeof(st__141, t576, t580)
    show_result("let_id_idid", t581)
    reset_type_variables(st__141)
    var t582 *_goml_vec_EnvEntry = env_empty()
    var t583 Exp = exp_var("x")
    var t584 Exp = exp_app(t583, id__142)
    var t585 Exp = exp_var("z")
    var t586 Exp = exp_let("z", t584, t585)
    var t587 Exp = exp_var("y")
    var t588 Exp = exp_let("y", t586, t587)
    var t589 Exp = exp_let("x", c1__143, t588)
    var t590 Result__Typ__string = typeof(st__141, t582, t589)
    show_result("nested_lets", t590)
    reset_type_variables(st__141)
    var t591 *_goml_vec_EnvEntry = env_empty()
    var t592 Exp = exp_var("x")
    var t593 Exp = exp_var("y")
    var t594 Exp = exp_app(t592, t593)
    var t595 Exp = exp_var("y")
    var t596 Exp = exp_var("x")
    var t597 Exp = exp_app(t595, t596)
    var t598 Exp = exp_lam("x", t597)
    var t599 Exp = exp_let("x", t594, t598)
    var t600 Exp = exp_lam("y", t599)
    var t601 Exp = exp_lam("x", t600)
    var t602 Result__Typ__string = typeof(st__141, t591, t601)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t602)
    reset_type_variables(st__141)
    var t603 *_goml_vec_EnvEntry = env_empty()
    var t604 Exp = exp_var("x")
    var t605 Exp = exp_var("y")
    var t606 Exp = exp_let("y", t604, t605)
    var t607 Exp = exp_lam("x", t606)
    var t608 Result__Typ__string = typeof(st__141, t603, t607)
    show_result("sound_gen_1", t608)
    reset_type_variables(st__141)
    var t609 *_goml_vec_EnvEntry = env_empty()
    var t610 Exp = exp_var("x")
    var t611 Exp = exp_lam("z", t610)
    var t612 Exp = exp_var("y")
    var t613 Exp = exp_let("y", t611, t612)
    var t614 Exp = exp_lam("x", t613)
    var t615 Result__Typ__string = typeof(st__141, t609, t614)
    show_result("sound_gen_2", t615)
    reset_type_variables(st__141)
    var t616 *_goml_vec_EnvEntry = env_empty()
    var t617 Exp = exp_var("x")
    var t618 Exp = exp_var("z")
    var t619 Exp = exp_app(t617, t618)
    var t620 Exp = exp_lam("z", t619)
    var t621 Exp = exp_var("y")
    var t622 Exp = exp_let("y", t620, t621)
    var t623 Exp = exp_lam("x", t622)
    var t624 Result__Typ__string = typeof(st__141, t616, t623)
    show_result("sound_gen_3", t624)
    reset_type_variables(st__141)
    var t625 *_goml_vec_EnvEntry = env_empty()
    var t626 Exp = exp_var("x")
    var t627 Exp = exp_var("y")
    var t628 Exp = exp_app(t626, t627)
    var t629 Exp = exp_var("x")
    var t630 Exp = exp_var("y")
    var t631 Exp = exp_app(t629, t630)
    var t632 Exp = exp_let("x", t628, t631)
    var t633 Exp = exp_lam("y", t632)
    var t634 Exp = exp_lam("x", t633)
    var t635 Result__Typ__string = typeof(st__141, t625, t634)
    show_result("double_apply", t635)
    reset_type_variables(st__141)
    var t636 *_goml_vec_EnvEntry = env_empty()
    var t637 Exp = exp_var("x")
    var t638 Exp = exp_var("y")
    var t639 Exp = exp_var("y")
    var t640 Exp = exp_app(t638, t639)
    var t641 Exp = exp_let("y", t637, t640)
    var t642 Exp = exp_lam("x", t641)
    var t643 Result__Typ__string = typeof(st__141, t636, t642)
    show_result("sound_gen_occurs", t643)
    reset_gensym(st__141)
    var t644 *_goml_vec_EnvEntry = env_empty()
    var t645 Exp = exp_var("x")
    var t646 Exp = exp_app(t645, id__142)
    var t647 Exp = exp_var("z")
    var t648 Exp = exp_let("z", t646, t647)
    var t649 Exp = exp_var("y")
    var t650 Exp = exp_let("y", t648, t649)
    var t651 Exp = exp_lam("x", t650)
    var t652 Result__Typ__string = typeof(st__141, t644, t651)
    show_result("fun_x_let_y_let_z_x_id_z_y", t652)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv654 *ref_int32_x
    var t655 *ref_int32_x = ref__Ref_5int32(value__102)
    retv654 = t655
    return retv654
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv659 int32
    var t660 int32 = ref_get__Ref_5int32(self__103)
    retv659 = t660
    return retv659
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv662 string
    var t663 string = _goml_runtime_core_char_to_string(self__3)
    retv662 = t663
    return retv662
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv665 string
    var t666 string = _goml_runtime_core_int32_to_string(self__2)
    retv665 = t666
    return retv665
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__102 Tv) *ref_Tv_x {
    var retv668 *ref_Tv_x
    var t669 *ref_Tv_x = ref__Ref_2Tv(value__102)
    retv668 = t669
    return retv668
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__103 *ref_Tv_x) Tv {
    var retv671 Tv
    var t672 Tv = ref_get__Ref_2Tv(self__103)
    retv671 = t672
    return retv671
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv674 *_goml_vec_EnvEntry
    var t675 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__82 *_goml_vec_EnvEntry) int32 {
    var retv677 int32
    var t678 int32 = vec_len__Vec_8EnvEntry(self__82)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__102 Option__Typ) *ref_Option__Typ_x {
    var retv680 *ref_Option__Typ_x
    var t681 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__102)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__102 bool) *ref_bool_x {
    var retv683 *ref_bool_x
    var t684 *ref_bool_x = ref__Ref_4bool(value__102)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__103 *ref_bool_x) bool {
    var retv686 bool
    var t687 bool = ref_get__Ref_4bool(self__103)
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__104 *ref_Option__Typ_x, value__105 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__104 *ref_bool_x, value__105 bool) struct{} {
    ref_set__Ref_4bool(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__103 *ref_Option__Typ_x) Option__Typ {
    var retv693 Option__Typ
    var t694 Option__Typ = ref_get__Ref_11Option__Typ(self__103)
    retv693 = t694
    return retv693
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__82 *_goml_vec_SubstEntry) int32 {
    var retv696 int32
    var t697 int32 = vec_len__Vec_10SubstEntry(self__82)
    retv696 = t697
    return retv696
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__104 *ref_Tv_x, value__105 Tv) struct{} {
    ref_set__Ref_2Tv(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__73 *_goml_vec_SubstEntry, elem__74 SubstEntry) *_goml_vec_SubstEntry {
    var retv701 *_goml_vec_SubstEntry
    var result__75 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop703:
    for {
        var t704 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t705 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__73)
        var t706 bool = t704 < t705
        if t706 {
            var t707 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t708 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__73, t707)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__75, t708)
            var t709 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t710 int32 = t709 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t710)
            continue
        } else {
            break Loop_loop703
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__75, elem__74)
    retv701 = result__75
    return retv701
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv712 *_goml_vec_SubstEntry
    var t713 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv712 = t713
    return retv712
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__73 *_goml_vec_EnvEntry, elem__74 EnvEntry) *_goml_vec_EnvEntry {
    var retv715 *_goml_vec_EnvEntry
    var result__75 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop717:
    for {
        var t718 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t719 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__73)
        var t720 bool = t718 < t719
        if t720 {
            var t721 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t722 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__73, t721)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__75, t722)
            var t723 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t724 int32 = t723 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t724)
            continue
        } else {
            break Loop_loop717
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__75, elem__74)
    retv715 = result__75
    return retv715
}

func println__T_string(value__1 string) struct{} {
    var t726 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t726)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__71 *_goml_vec_SubstEntry, elem__72 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__77 *_goml_vec_SubstEntry, index__78 int32) SubstEntry {
    var retv731 SubstEntry
    var t732 SubstEntry = vec_get__Vec_10SubstEntry(self__77, index__78)
    retv731 = t732
    return retv731
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__71 *_goml_vec_EnvEntry, elem__72 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__77 *_goml_vec_EnvEntry, index__78 int32) EnvEntry {
    var retv736 EnvEntry
    var t737 EnvEntry = vec_get__Vec_8EnvEntry(self__77, index__78)
    retv736 = t737
    return retv736
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv739 string
    retv739 = self__9
    return retv739
}

func main() {
    main0()
}
