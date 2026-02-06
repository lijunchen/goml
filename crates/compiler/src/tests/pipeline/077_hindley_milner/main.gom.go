package main

import (
    "fmt"
    "unicode/utf8"
)

func string_get(s string, i int32) rune {
    return rune(s[i])
}

func char_to_string(x rune) string {
    if !utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_state_x struct {
    value State
}

func ref__Ref_State(value State) *ref_state_x {
    return &ref_state_x{
        value: value,
    }
}

func ref_get__Ref_State(reference *ref_state_x) State {
    return reference.value
}

type ref_tvcontent_x struct {
    value TvContent
}

func ref__Ref_TvContent(value TvContent) *ref_tvcontent_x {
    return &ref_tvcontent_x{
        value: value,
    }
}

func ref_get__Ref_TvContent(reference *ref_tvcontent_x) TvContent {
    return reference.value
}

func ref_set__Ref_TvContent(reference *ref_tvcontent_x, value TvContent) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_typ_x struct {
    value Typ
}

func ref__Ref_Typ(value Typ) *ref_typ_x {
    return &ref_typ_x{
        value: value,
    }
}

func ref_get__Ref_Typ(reference *ref_typ_x) Typ {
    return reference.value
}

type ref_vec_tuple_string_typ_x struct {
    value []Tuple2_string_Typ
}

func ref__Ref_Vec_Tuple_string_Typ(value []Tuple2_string_Typ) *ref_vec_tuple_string_typ_x {
    return &ref_vec_tuple_string_typ_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Tuple_string_Typ(reference *ref_vec_tuple_string_typ_x) []Tuple2_string_Typ {
    return reference.value
}

func ref_set__Ref_Vec_Tuple_string_Typ(reference *ref_vec_tuple_string_typ_x, value []Tuple2_string_Typ) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_option__typ_x struct {
    value Option__Typ
}

func ref__Ref_Option__Typ(value Option__Typ) *ref_option__typ_x {
    return &ref_option__typ_x{
        value: value,
    }
}

func ref_get__Ref_Option__Typ(reference *ref_option__typ_x) Option__Typ {
    return reference.value
}

func ref_set__Ref_Option__Typ(reference *ref_option__typ_x, value Option__Typ) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_exp_x struct {
    value Exp
}

func ref__Ref_Exp(value Exp) *ref_exp_x {
    return &ref_exp_x{
        value: value,
    }
}

func ref_get__Ref_Exp(reference *ref_exp_x) Exp {
    return reference.value
}

type Tuple2_TvContent_TvContent struct {
    _0 TvContent
    _1 TvContent
}

type Tuple2_Typ_Typ struct {
    _0 Typ
    _1 Typ
}

type Tuple2_Typ_Vec_Tuple2_string_Typ struct {
    _0 Typ
    _1 []Tuple2_string_Typ
}

type Tuple2_string_Typ struct {
    _0 string
    _1 Typ
}

type State struct {
    gensym_counter *ref_int32_x
    current_level *ref_int32_x
}

type Exp interface {
    isExp()
}

type Var struct {
    _0 string
}

func (_ Var) isExp() {}

type App struct {
    _0 *ref_exp_x
    _1 *ref_exp_x
}

func (_ App) isExp() {}

type Lam struct {
    _0 string
    _1 *ref_exp_x
}

func (_ Lam) isExp() {}

type Let struct {
    _0 string
    _1 *ref_exp_x
    _2 *ref_exp_x
}

func (_ Let) isExp() {}

type TvContent interface {
    isTvContent()
}

type Unbound struct {
    _0 int32
    _1 string
    _2 int32
}

func (_ Unbound) isTvContent() {}

type Link struct {
    _0 *ref_typ_x
}

func (_ Link) isTvContent() {}

type Typ interface {
    isTyp()
}

type TVar struct {
    _0 *ref_tvcontent_x
}

func (_ TVar) isTyp() {}

type QVar struct {
    _0 string
}

func (_ QVar) isTyp() {}

type TArrow struct {
    _0 *ref_typ_x
    _1 *ref_typ_x
}

func (_ TArrow) isTyp() {}

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

type Option__Typ interface {
    isOption__Typ()
}

type None struct {}

func (_ None) isOption__Typ() {}

type Some struct {
    _0 Typ
}

func (_ Some) isOption__Typ() {}

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

func state_new() State {
    var ret509 State
    var t234 *ref_int32_x = ref__Ref_int32(0)
    var t235 *ref_int32_x = ref__Ref_int32(1)
    ret509 = State{
        gensym_counter: t234,
        current_level: t235,
    }
    return ret509
}

func reset_gensym(st__0 *ref_state_x) struct{} {
    var ret510 struct{}
    var s__1 State = ref_get__Ref_State(st__0)
    var t236 *ref_int32_x = s__1.gensym_counter
    ret510 = ref_set__Ref_int32(t236, 0)
    return ret510
}

func gensym(st__2 *ref_state_x) string {
    var ret511 string
    var s__3 State = ref_get__Ref_State(st__2)
    var t237 *ref_int32_x = s__3.gensym_counter
    var n__4 int32 = ref_get__Ref_int32(t237)
    var t238 *ref_int32_x = s__3.gensym_counter
    var t239 int32 = n__4 + 1
    ref_set__Ref_int32(t238, t239)
    var t240 bool = n__4 < 26
    if t240 {
        var c__5 rune = string_get("abcdefghijklmnopqrstuvwxyz", n__4)
        ret511 = char_to_string(c__5)
    } else {
        var t241 string = int32_to_string(n__4)
        ret511 = "t" + t241
    }
    return ret511
}

func reset_level(st__6 *ref_state_x) struct{} {
    var ret512 struct{}
    var s__7 State = ref_get__Ref_State(st__6)
    var t242 *ref_int32_x = s__7.current_level
    ret512 = ref_set__Ref_int32(t242, 1)
    return ret512
}

func reset_type_variables(st__8 *ref_state_x) struct{} {
    var ret513 struct{}
    reset_gensym(st__8)
    ret513 = reset_level(st__8)
    return ret513
}

func enter_level(st__9 *ref_state_x) struct{} {
    var ret514 struct{}
    var s__10 State = ref_get__Ref_State(st__9)
    var t243 *ref_int32_x = s__10.current_level
    var lv__11 int32 = ref_get__Ref_int32(t243)
    var t244 *ref_int32_x = s__10.current_level
    var t245 int32 = lv__11 + 1
    ret514 = ref_set__Ref_int32(t244, t245)
    return ret514
}

func leave_level(st__12 *ref_state_x) struct{} {
    var ret515 struct{}
    var s__13 State = ref_get__Ref_State(st__12)
    var t246 *ref_int32_x = s__13.current_level
    var lv__14 int32 = ref_get__Ref_int32(t246)
    var t247 *ref_int32_x = s__13.current_level
    var t248 int32 = lv__14 - 1
    ret515 = ref_set__Ref_int32(t247, t248)
    return ret515
}

func newvar(st__15 *ref_state_x) Typ {
    var ret516 Typ
    var s__16 State = ref_get__Ref_State(st__15)
    var t249 *ref_int32_x = s__16.gensym_counter
    var id__17 int32 = ref_get__Ref_int32(t249)
    var name__18 string = gensym(st__15)
    var t250 *ref_int32_x = s__16.current_level
    var level__19 int32 = ref_get__Ref_int32(t250)
    var t252 TvContent = Unbound{
        _0: id__17,
        _1: name__18,
        _2: level__19,
    }
    var t251 *ref_tvcontent_x = ref__Ref_TvContent(t252)
    ret516 = TVar{
        _0: t251,
    }
    return ret516
}

func min_int32(a__20 int32, b__21 int32) int32 {
    var ret517 int32
    var t253 bool = a__20 < b__21
    if t253 {
        ret517 = a__20
    } else {
        ret517 = b__21
    }
    return ret517
}

func occurs(st__22 *ref_state_x, tvr__23 *ref_tvcontent_x, tvr_id__24 int32, ty__25 Typ) Result__unit__string {
    var ret518 Result__unit__string
    switch ty__25 := ty__25.(type) {
    case TVar:
        var x2 *ref_tvcontent_x = ty__25._0
        var tvr2__26 *ref_tvcontent_x = x2
        var tv_content__27 TvContent = ref_get__Ref_TvContent(tvr__23)
        var tv_content2__28 TvContent = ref_get__Ref_TvContent(tvr2__26)
        var mtmp6 Tuple2_TvContent_TvContent = Tuple2_TvContent_TvContent{
            _0: tv_content__27,
            _1: tv_content2__28,
        }
        var x7 TvContent = mtmp6._0
        var x8 TvContent = mtmp6._1
        switch x8 := x8.(type) {
        case Unbound:
            var x9 int32 = x8._0
            var x10 string = x8._1
            var x11 int32 = x8._2
            switch x7 := x7.(type) {
            case Unbound:
                var x15 int32 = x7._2
                var l__31 int32 = x15
                var l2__34 int32 = x11
                var name2__33 string = x10
                var id2__32 int32 = x9
                var t254 bool = tvr_id__24 == id2__32
                if t254 {
                    ret518 = Result__unit__string_Err{
                        _0: "occurs check",
                    }
                } else {
                    var min_level__35 int32 = min_int32(l__31, l2__34)
                    var t255 TvContent = Unbound{
                        _0: id2__32,
                        _1: name2__33,
                        _2: min_level__35,
                    }
                    ref_set__Ref_TvContent(tvr2__26, t255)
                    ret518 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                }
            case Link:
                var l2__38 int32 = x11
                var name2__37 string = x10
                var id2__36 int32 = x9
                var t256 TvContent = Unbound{
                    _0: id2__36,
                    _1: name2__37,
                    _2: l2__38,
                }
                ref_set__Ref_TvContent(tvr2__26, t256)
                ret518 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            }
        case Link:
            var x12 *ref_typ_x = x8._0
            switch x7.(type) {
            case Unbound:
                var linked_ty__30 *ref_typ_x = x12
                var t257 Typ = ref_get__Ref_Typ(linked_ty__30)
                ret518 = occurs(st__22, tvr__23, tvr_id__24, t257)
            case Link:
                ret518 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            }
        }
    case QVar:
        ret518 = Result__unit__string_Ok{
            _0: struct{}{},
        }
    case TArrow:
        var x4 *ref_typ_x = ty__25._0
        var x5 *ref_typ_x = ty__25._1
        var t2__40 *ref_typ_x = x5
        var t1__39 *ref_typ_x = x4
        var t258 Typ = ref_get__Ref_Typ(t1__39)
        var mtmp23 Result__unit__string = occurs(st__22, tvr__23, tvr_id__24, t258)
        switch mtmp23 := mtmp23.(type) {
        case Result__unit__string_Ok:
            var t259 Typ = ref_get__Ref_Typ(t2__40)
            ret518 = occurs(st__22, tvr__23, tvr_id__24, t259)
        case Result__unit__string_Err:
            var x25 string = mtmp23._0
            var e__41 string = x25
            ret518 = Result__unit__string_Err{
                _0: e__41,
            }
        }
    }
    return ret518
}

func unify(st__42 *ref_state_x, t1__43 Typ, t2__44 Typ) Result__unit__string {
    var ret519 Result__unit__string
    var mtmp26 Tuple2_Typ_Typ = Tuple2_Typ_Typ{
        _0: t1__43,
        _1: t2__44,
    }
    var x27 Typ = mtmp26._0
    var x28 Typ = mtmp26._1
    switch x28 := x28.(type) {
    case TVar:
        var x29 *ref_tvcontent_x = x28._0
        switch x27 := x27.(type) {
        case TVar:
            var x33 *ref_tvcontent_x = x27._0
            var tv1__45 *ref_tvcontent_x = x33
            var tv2__46 *ref_tvcontent_x = x29
            var c1__47 TvContent = ref_get__Ref_TvContent(tv1__45)
            var c2__48 TvContent = ref_get__Ref_TvContent(tv2__46)
            var mtmp37 Tuple2_TvContent_TvContent = Tuple2_TvContent_TvContent{
                _0: c1__47,
                _1: c2__48,
            }
            var x38 TvContent = mtmp37._0
            var x39 TvContent = mtmp37._1
            switch x39 := x39.(type) {
            case Unbound:
                var x40 int32 = x39._0
                switch x38 := x38.(type) {
                case Unbound:
                    var x44 int32 = x38._0
                    var id1__49 int32 = x44
                    var id2__50 int32 = x40
                    var t260 bool = id1__49 == id2__50
                    if t260 {
                        ret519 = Result__unit__string_Ok{
                            _0: struct{}{},
                        }
                    } else {
                        var t261 Typ = TVar{
                            _0: tv2__46,
                        }
                        var mtmp48 Result__unit__string = occurs(st__42, tv1__45, id1__49, t261)
                        switch mtmp48 := mtmp48.(type) {
                        case Result__unit__string_Ok:
                            var t264 Typ = TVar{
                                _0: tv2__46,
                            }
                            var t263 *ref_typ_x = ref__Ref_Typ(t264)
                            var t262 TvContent = Link{
                                _0: t263,
                            }
                            ref_set__Ref_TvContent(tv1__45, t262)
                            ret519 = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                        case Result__unit__string_Err:
                            var x50 string = mtmp48._0
                            var e__51 string = x50
                            ret519 = Result__unit__string_Err{
                                _0: e__51,
                            }
                        }
                    }
                case Link:
                    var x47 *ref_typ_x = x38._0
                    var ty1__52 *ref_typ_x = x47
                    var t265 Typ = ref_get__Ref_Typ(ty1__52)
                    var t266 Typ = TVar{
                        _0: tv2__46,
                    }
                    ret519 = unify(st__42, t265, t266)
                }
            case Link:
                var x43 *ref_typ_x = x39._0
                switch x38 := x38.(type) {
                case Unbound:
                    var ty2__53 *ref_typ_x = x43
                    var t267 Typ = TVar{
                        _0: tv1__45,
                    }
                    var t268 Typ = ref_get__Ref_Typ(ty2__53)
                    ret519 = unify(st__42, t267, t268)
                case Link:
                    var x55 *ref_typ_x = x38._0
                    var ty1__52 *ref_typ_x = x55
                    var t269 Typ = ref_get__Ref_Typ(ty1__52)
                    var t270 Typ = TVar{
                        _0: tv2__46,
                    }
                    ret519 = unify(st__42, t269, t270)
                }
            }
        case QVar:
            var tv__61 *ref_tvcontent_x = x29
            var t__60 Typ = x27
            var c__62 TvContent = ref_get__Ref_TvContent(tv__61)
            switch c__62 := c__62.(type) {
            case Unbound:
                var x56 int32 = c__62._0
                var id__64 int32 = x56
                var mtmp60 Result__unit__string = occurs(st__42, tv__61, id__64, t__60)
                switch mtmp60 := mtmp60.(type) {
                case Result__unit__string_Ok:
                    var t272 *ref_typ_x = ref__Ref_Typ(t__60)
                    var t271 TvContent = Link{
                        _0: t272,
                    }
                    ref_set__Ref_TvContent(tv__61, t271)
                    ret519 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x62 string = mtmp60._0
                    var e__65 string = x62
                    ret519 = Result__unit__string_Err{
                        _0: e__65,
                    }
                }
            case Link:
                var x59 *ref_typ_x = c__62._0
                var ty__63 *ref_typ_x = x59
                var t273 Typ = ref_get__Ref_Typ(ty__63)
                ret519 = unify(st__42, t__60, t273)
            }
        case TArrow:
            var tv__61 *ref_tvcontent_x = x29
            var t__60 Typ = x27
            var c__62 TvContent = ref_get__Ref_TvContent(tv__61)
            switch c__62 := c__62.(type) {
            case Unbound:
                var x64 int32 = c__62._0
                var id__64 int32 = x64
                var mtmp68 Result__unit__string = occurs(st__42, tv__61, id__64, t__60)
                switch mtmp68 := mtmp68.(type) {
                case Result__unit__string_Ok:
                    var t275 *ref_typ_x = ref__Ref_Typ(t__60)
                    var t274 TvContent = Link{
                        _0: t275,
                    }
                    ref_set__Ref_TvContent(tv__61, t274)
                    ret519 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x70 string = mtmp68._0
                    var e__65 string = x70
                    ret519 = Result__unit__string_Err{
                        _0: e__65,
                    }
                }
            case Link:
                var x67 *ref_typ_x = c__62._0
                var ty__63 *ref_typ_x = x67
                var t276 Typ = ref_get__Ref_Typ(ty__63)
                ret519 = unify(st__42, t__60, t276)
            }
        }
    case QVar:
        switch x27 := x27.(type) {
        case TVar:
            var x72 *ref_tvcontent_x = x27._0
            var tv__54 *ref_tvcontent_x = x72
            var t__55 Typ = x28
            var c__56 TvContent = ref_get__Ref_TvContent(tv__54)
            switch c__56 := c__56.(type) {
            case Unbound:
                var x76 int32 = c__56._0
                var id__58 int32 = x76
                var mtmp80 Result__unit__string = occurs(st__42, tv__54, id__58, t__55)
                switch mtmp80 := mtmp80.(type) {
                case Result__unit__string_Ok:
                    var t278 *ref_typ_x = ref__Ref_Typ(t__55)
                    var t277 TvContent = Link{
                        _0: t278,
                    }
                    ref_set__Ref_TvContent(tv__54, t277)
                    ret519 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x82 string = mtmp80._0
                    var e__59 string = x82
                    ret519 = Result__unit__string_Err{
                        _0: e__59,
                    }
                }
            case Link:
                var x79 *ref_typ_x = c__56._0
                var ty__57 *ref_typ_x = x79
                var t279 Typ = ref_get__Ref_Typ(ty__57)
                ret519 = unify(st__42, t279, t__55)
            }
        case QVar:
            ret519 = Result__unit__string_Err{
                _0: "unification error",
            }
        case TArrow:
            ret519 = Result__unit__string_Err{
                _0: "unification error",
            }
        }
    case TArrow:
        var x31 *ref_typ_x = x28._0
        var x32 *ref_typ_x = x28._1
        switch x27 := x27.(type) {
        case TVar:
            var x84 *ref_tvcontent_x = x27._0
            var tv__54 *ref_tvcontent_x = x84
            var t__55 Typ = x28
            var c__56 TvContent = ref_get__Ref_TvContent(tv__54)
            switch c__56 := c__56.(type) {
            case Unbound:
                var x88 int32 = c__56._0
                var id__58 int32 = x88
                var mtmp92 Result__unit__string = occurs(st__42, tv__54, id__58, t__55)
                switch mtmp92 := mtmp92.(type) {
                case Result__unit__string_Ok:
                    var t281 *ref_typ_x = ref__Ref_Typ(t__55)
                    var t280 TvContent = Link{
                        _0: t281,
                    }
                    ref_set__Ref_TvContent(tv__54, t280)
                    ret519 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x94 string = mtmp92._0
                    var e__59 string = x94
                    ret519 = Result__unit__string_Err{
                        _0: e__59,
                    }
                }
            case Link:
                var x91 *ref_typ_x = c__56._0
                var ty__57 *ref_typ_x = x91
                var t282 Typ = ref_get__Ref_Typ(ty__57)
                ret519 = unify(st__42, t282, t__55)
            }
        case QVar:
            ret519 = Result__unit__string_Err{
                _0: "unification error",
            }
        case TArrow:
            var x86 *ref_typ_x = x27._0
            var x87 *ref_typ_x = x27._1
            var t1b__67 *ref_typ_x = x87
            var t1a__66 *ref_typ_x = x86
            var t2b__69 *ref_typ_x = x32
            var t2a__68 *ref_typ_x = x31
            var t283 Typ = ref_get__Ref_Typ(t1a__66)
            var t284 Typ = ref_get__Ref_Typ(t2a__68)
            var mtmp96 Result__unit__string = unify(st__42, t283, t284)
            switch mtmp96 := mtmp96.(type) {
            case Result__unit__string_Ok:
                var t285 Typ = ref_get__Ref_Typ(t1b__67)
                var t286 Typ = ref_get__Ref_Typ(t2b__69)
                ret519 = unify(st__42, t285, t286)
            case Result__unit__string_Err:
                var x98 string = mtmp96._0
                var e__70 string = x98
                ret519 = Result__unit__string_Err{
                    _0: e__70,
                }
            }
        }
    }
    return ret519
}

func gen(st__71 *ref_state_x, ty__72 Typ) Typ {
    var ret520 Typ
    var s__73 State = ref_get__Ref_State(st__71)
    var t287 *ref_int32_x = s__73.current_level
    var curr_level__74 int32 = ref_get__Ref_int32(t287)
    switch ty__72 := ty__72.(type) {
    case TVar:
        var x99 *ref_tvcontent_x = ty__72._0
        var tv__75 *ref_tvcontent_x = x99
        var c__76 TvContent = ref_get__Ref_TvContent(tv__75)
        switch c__76 := c__76.(type) {
        case Unbound:
            var x104 string = c__76._1
            var x105 int32 = c__76._2
            var l__78 int32 = x105
            var name__77 string = x104
            var t288 bool = l__78 > curr_level__74
            if t288 {
                ret520 = QVar{
                    _0: name__77,
                }
            } else {
                ret520 = TVar{
                    _0: tv__75,
                }
            }
        case Link:
            var x106 *ref_typ_x = c__76._0
            var linked_ty__79 *ref_typ_x = x106
            var t289 Typ = ref_get__Ref_Typ(linked_ty__79)
            ret520 = gen(st__71, t289)
        }
    case QVar:
        ret520 = ty__72
    case TArrow:
        var x101 *ref_typ_x = ty__72._0
        var x102 *ref_typ_x = ty__72._1
        var t2__81 *ref_typ_x = x102
        var t1__80 *ref_typ_x = x101
        var t292 Typ = ref_get__Ref_Typ(t1__80)
        var t291 Typ = gen(st__71, t292)
        var t290 *ref_typ_x = ref__Ref_Typ(t291)
        var t295 Typ = ref_get__Ref_Typ(t2__81)
        var t294 Typ = gen(st__71, t295)
        var t293 *ref_typ_x = ref__Ref_Typ(t294)
        ret520 = TArrow{
            _0: t290,
            _1: t293,
        }
    }
    return ret520
}

func inst_helper(st__82 *ref_state_x, ty__83 Typ, subst__84 []Tuple2_string_Typ) Tuple2_Typ_Vec_Tuple2_string_Typ {
    var ret521 Tuple2_Typ_Vec_Tuple2_string_Typ
    switch ty__83 := ty__83.(type) {
    case TVar:
        var x107 *ref_tvcontent_x = ty__83._0
        var tv__90 *ref_tvcontent_x = x107
        var c__91 TvContent = ref_get__Ref_TvContent(tv__90)
        switch c__91 := c__91.(type) {
        case Unbound:
            var t296 Typ = TVar{
                _0: tv__90,
            }
            ret521 = Tuple2_Typ_Vec_Tuple2_string_Typ{
                _0: t296,
                _1: subst__84,
            }
        case Link:
            var x114 *ref_typ_x = c__91._0
            var linked_ty__92 *ref_typ_x = x114
            var t297 Typ = ref_get__Ref_Typ(linked_ty__92)
            ret521 = inst_helper(st__82, t297, subst__84)
        }
    case QVar:
        var x108 string = ty__83._0
        var name__85 string = x108
        var found__86 Option__Typ = vec_find_assoc(subst__84, name__85)
        switch found__86 := found__86.(type) {
        case None:
            var tv__88 Typ = newvar(st__82)
            var t298 Tuple2_string_Typ = Tuple2_string_Typ{
                _0: name__85,
                _1: tv__88,
            }
            var new_subst__89 []Tuple2_string_Typ = vec_cons_subst(t298, subst__84)
            ret521 = Tuple2_Typ_Vec_Tuple2_string_Typ{
                _0: tv__88,
                _1: new_subst__89,
            }
        case Some:
            var x115 Typ = found__86._0
            var t__87 Typ = x115
            ret521 = Tuple2_Typ_Vec_Tuple2_string_Typ{
                _0: t__87,
                _1: subst__84,
            }
        }
    case TArrow:
        var x109 *ref_typ_x = ty__83._0
        var x110 *ref_typ_x = ty__83._1
        var t2__94 *ref_typ_x = x110
        var t1__93 *ref_typ_x = x109
        var t299 Typ = ref_get__Ref_Typ(t1__93)
        var pair1__95 Tuple2_Typ_Vec_Tuple2_string_Typ = inst_helper(st__82, t299, subst__84)
        var mtmp116 Tuple2_Typ_Vec_Tuple2_string_Typ = pair1__95
        var x117 Typ = mtmp116._0
        var x118 []Tuple2_string_Typ = mtmp116._1
        var subst1__97 []Tuple2_string_Typ = x118
        var ty1__96 Typ = x117
        var t300 Typ = ref_get__Ref_Typ(t2__94)
        var pair2__98 Tuple2_Typ_Vec_Tuple2_string_Typ = inst_helper(st__82, t300, subst1__97)
        var mtmp119 Tuple2_Typ_Vec_Tuple2_string_Typ = pair2__98
        var x120 Typ = mtmp119._0
        var x121 []Tuple2_string_Typ = mtmp119._1
        var subst2__100 []Tuple2_string_Typ = x121
        var ty2__99 Typ = x120
        var t302 *ref_typ_x = ref__Ref_Typ(ty1__96)
        var t303 *ref_typ_x = ref__Ref_Typ(ty2__99)
        var t301 Typ = TArrow{
            _0: t302,
            _1: t303,
        }
        ret521 = Tuple2_Typ_Vec_Tuple2_string_Typ{
            _0: t301,
            _1: subst2__100,
        }
    }
    return ret521
}

func inst(st__101 *ref_state_x, ty__102 Typ) Typ {
    var ret522 Typ
    var empty_subst__103 []Tuple2_string_Typ = nil
    var result__104 Tuple2_Typ_Vec_Tuple2_string_Typ = inst_helper(st__101, ty__102, empty_subst__103)
    var mtmp122 Tuple2_Typ_Vec_Tuple2_string_Typ = result__104
    var x123 Typ = mtmp122._0
    var ty_result__105 Typ = x123
    ret522 = ty_result__105
    return ret522
}

func vec_cons_env(elem__106 Tuple2_string_Typ, vec__107 []Tuple2_string_Typ) []Tuple2_string_Typ {
    var ret523 []Tuple2_string_Typ
    var new_vec__108 []Tuple2_string_Typ = nil
    var new_vec__109 []Tuple2_string_Typ = append(new_vec__108, elem__106)
    var i__110 *ref_int32_x = ref__Ref_int32(0)
    var len__111 int32 = int32(len(vec__107))
    var new_vec_ref__112 *ref_vec_tuple_string_typ_x = ref__Ref_Vec_Tuple_string_Typ(new_vec__109)
    var cond524 bool
    for {
        var t304 int32 = ref_get__Ref_int32(i__110)
        cond524 = t304 < len__111
        if !cond524 {
            break
        }
        var nv__113 []Tuple2_string_Typ = ref_get__Ref_Vec_Tuple_string_Typ(new_vec_ref__112)
        var t306 int32 = ref_get__Ref_int32(i__110)
        var t305 Tuple2_string_Typ = vec__107[t306]
        var nv__114 []Tuple2_string_Typ = append(nv__113, t305)
        ref_set__Ref_Vec_Tuple_string_Typ(new_vec_ref__112, nv__114)
        var t308 int32 = ref_get__Ref_int32(i__110)
        var t307 int32 = t308 + 1
        ref_set__Ref_int32(i__110, t307)
    }
    ret523 = ref_get__Ref_Vec_Tuple_string_Typ(new_vec_ref__112)
    return ret523
}

func vec_cons_subst(elem__115 Tuple2_string_Typ, vec__116 []Tuple2_string_Typ) []Tuple2_string_Typ {
    var ret525 []Tuple2_string_Typ
    var new_vec__117 []Tuple2_string_Typ = nil
    var new_vec__118 []Tuple2_string_Typ = append(new_vec__117, elem__115)
    var i__119 *ref_int32_x = ref__Ref_int32(0)
    var len__120 int32 = int32(len(vec__116))
    var new_vec_ref__121 *ref_vec_tuple_string_typ_x = ref__Ref_Vec_Tuple_string_Typ(new_vec__118)
    var cond526 bool
    for {
        var t309 int32 = ref_get__Ref_int32(i__119)
        cond526 = t309 < len__120
        if !cond526 {
            break
        }
        var nv__122 []Tuple2_string_Typ = ref_get__Ref_Vec_Tuple_string_Typ(new_vec_ref__121)
        var t311 int32 = ref_get__Ref_int32(i__119)
        var t310 Tuple2_string_Typ = vec__116[t311]
        var nv__123 []Tuple2_string_Typ = append(nv__122, t310)
        ref_set__Ref_Vec_Tuple_string_Typ(new_vec_ref__121, nv__123)
        var t313 int32 = ref_get__Ref_int32(i__119)
        var t312 int32 = t313 + 1
        ref_set__Ref_int32(i__119, t312)
    }
    ret525 = ref_get__Ref_Vec_Tuple_string_Typ(new_vec_ref__121)
    return ret525
}

func vec_find_assoc(vec__124 []Tuple2_string_Typ, key__125 string) Option__Typ {
    var ret527 Option__Typ
    var i__126 *ref_int32_x = ref__Ref_int32(0)
    var len__127 int32 = int32(len(vec__124))
    var t314 Option__Typ = None{}
    var result__128 *ref_option__typ_x = ref__Ref_Option__Typ(t314)
    var cond528 bool
    for {
        var t315 int32 = ref_get__Ref_int32(i__126)
        cond528 = t315 < len__127
        if !cond528 {
            break
        }
        var t316 int32 = ref_get__Ref_int32(i__126)
        var pair__129 Tuple2_string_Typ = vec__124[t316]
        var mtmp131 Tuple2_string_Typ = pair__129
        var x132 string = mtmp131._0
        var x133 Typ = mtmp131._1
        var v__131 Typ = x133
        var k__130 string = x132
        var t317 bool = k__130 == key__125
        if t317 {
            var t318 Option__Typ = Some{
                _0: v__131,
            }
            ref_set__Ref_Option__Typ(result__128, t318)
        } else {}
        var t320 int32 = ref_get__Ref_int32(i__126)
        var t319 int32 = t320 + 1
        ref_set__Ref_int32(i__126, t319)
    }
    ret527 = ref_get__Ref_Option__Typ(result__128)
    return ret527
}

func env_lookup(env__132 []Tuple2_string_Typ, name__133 string) Option__Typ {
    var ret529 Option__Typ
    ret529 = vec_find_assoc(env__132, name__133)
    return ret529
}

func typeof(st__134 *ref_state_x, env__135 []Tuple2_string_Typ, exp__136 Exp) Result__Typ__string {
    var ret530 Result__Typ__string
    switch exp__136 := exp__136.(type) {
    case Var:
        var x136 string = exp__136._0
        var x__137 string = x136
        var mtmp144 Option__Typ = env_lookup(env__135, x__137)
        switch mtmp144 := mtmp144.(type) {
        case None:
            ret530 = Result__Typ__string_Err{
                _0: "unbound var",
            }
        case Some:
            var x145 Typ = mtmp144._0
            var ty__138 Typ = x145
            var t321 Typ = inst(st__134, ty__138)
            ret530 = Result__Typ__string_Ok{
                _0: t321,
            }
        }
    case App:
        var x137 *ref_exp_x = exp__136._0
        var x138 *ref_exp_x = exp__136._1
        var e2__146 *ref_exp_x = x138
        var e1__145 *ref_exp_x = x137
        var t322 Exp = ref_get__Ref_Exp(e1__145)
        var mtmp146 Result__Typ__string = typeof(st__134, env__135, t322)
        switch mtmp146 := mtmp146.(type) {
        case Result__Typ__string_Ok:
            var x147 Typ = mtmp146._0
            var ty_fun__147 Typ = x147
            var t323 Exp = ref_get__Ref_Exp(e2__146)
            var mtmp149 Result__Typ__string = typeof(st__134, env__135, t323)
            switch mtmp149 := mtmp149.(type) {
            case Result__Typ__string_Ok:
                var x150 Typ = mtmp149._0
                var ty_arg__148 Typ = x150
                var ty_res__149 Typ = newvar(st__134)
                var t325 *ref_typ_x = ref__Ref_Typ(ty_arg__148)
                var t326 *ref_typ_x = ref__Ref_Typ(ty_res__149)
                var t324 Typ = TArrow{
                    _0: t325,
                    _1: t326,
                }
                var mtmp152 Result__unit__string = unify(st__134, ty_fun__147, t324)
                switch mtmp152 := mtmp152.(type) {
                case Result__unit__string_Ok:
                    ret530 = Result__Typ__string_Ok{
                        _0: ty_res__149,
                    }
                case Result__unit__string_Err:
                    var x154 string = mtmp152._0
                    var e__150 string = x154
                    ret530 = Result__Typ__string_Err{
                        _0: e__150,
                    }
                }
            case Result__Typ__string_Err:
                var x151 string = mtmp149._0
                var e__151 string = x151
                ret530 = Result__Typ__string_Err{
                    _0: e__151,
                }
            }
        case Result__Typ__string_Err:
            var x148 string = mtmp146._0
            var e__152 string = x148
            ret530 = Result__Typ__string_Err{
                _0: e__152,
            }
        }
    case Lam:
        var x139 string = exp__136._0
        var x140 *ref_exp_x = exp__136._1
        var e__140 *ref_exp_x = x140
        var x__139 string = x139
        var ty_x__141 Typ = newvar(st__134)
        var t327 Tuple2_string_Typ = Tuple2_string_Typ{
            _0: x__139,
            _1: ty_x__141,
        }
        var new_env__142 []Tuple2_string_Typ = vec_cons_env(t327, env__135)
        var t328 Exp = ref_get__Ref_Exp(e__140)
        var mtmp155 Result__Typ__string = typeof(st__134, new_env__142, t328)
        switch mtmp155 := mtmp155.(type) {
        case Result__Typ__string_Ok:
            var x156 Typ = mtmp155._0
            var ty_e__143 Typ = x156
            var t330 *ref_typ_x = ref__Ref_Typ(ty_x__141)
            var t331 *ref_typ_x = ref__Ref_Typ(ty_e__143)
            var t329 Typ = TArrow{
                _0: t330,
                _1: t331,
            }
            ret530 = Result__Typ__string_Ok{
                _0: t329,
            }
        case Result__Typ__string_Err:
            var x157 string = mtmp155._0
            var e__144 string = x157
            ret530 = Result__Typ__string_Err{
                _0: e__144,
            }
        }
    case Let:
        var x141 string = exp__136._0
        var x142 *ref_exp_x = exp__136._1
        var x143 *ref_exp_x = exp__136._2
        var e2__155 *ref_exp_x = x143
        var e__154 *ref_exp_x = x142
        var x__153 string = x141
        enter_level(st__134)
        var t332 Exp = ref_get__Ref_Exp(e__154)
        var ty_e_result__156 Result__Typ__string = typeof(st__134, env__135, t332)
        leave_level(st__134)
        switch ty_e_result__156 := ty_e_result__156.(type) {
        case Result__Typ__string_Ok:
            var x160 Typ = ty_e_result__156._0
            var ty_e__157 Typ = x160
            var gen_ty__158 Typ = gen(st__134, ty_e__157)
            var t333 Tuple2_string_Typ = Tuple2_string_Typ{
                _0: x__153,
                _1: gen_ty__158,
            }
            var new_env__159 []Tuple2_string_Typ = vec_cons_env(t333, env__135)
            var t334 Exp = ref_get__Ref_Exp(e2__155)
            ret530 = typeof(st__134, new_env__159, t334)
        case Result__Typ__string_Err:
            var x161 string = ty_e_result__156._0
            var e__160 string = x161
            ret530 = Result__Typ__string_Err{
                _0: e__160,
            }
        }
    }
    return ret530
}

func mk_id() Exp {
    var ret531 Exp
    var t336 Exp = Var{
        _0: "x",
    }
    var t335 *ref_exp_x = ref__Ref_Exp(t336)
    ret531 = Lam{
        _0: "x",
        _1: t335,
    }
    return ret531
}

func mk_c1() Exp {
    var ret532 Exp
    var t342 Exp = Var{
        _0: "x",
    }
    var t341 *ref_exp_x = ref__Ref_Exp(t342)
    var t344 Exp = Var{
        _0: "y",
    }
    var t343 *ref_exp_x = ref__Ref_Exp(t344)
    var t340 Exp = App{
        _0: t341,
        _1: t343,
    }
    var t339 *ref_exp_x = ref__Ref_Exp(t340)
    var t338 Exp = Lam{
        _0: "y",
        _1: t339,
    }
    var t337 *ref_exp_x = ref__Ref_Exp(t338)
    ret532 = Lam{
        _0: "x",
        _1: t337,
    }
    return ret532
}

func test1(st__161 *ref_state_x) struct{} {
    var ret533 struct{}
    reset_type_variables(st__161)
    var empty_env__162 []Tuple2_string_Typ = nil
    var t345 Exp = mk_id()
    var ty__163 Result__Typ__string = typeof(st__161, empty_env__162, t345)
    switch ty__163 := ty__163.(type) {
    case Result__Typ__string_Ok:
        ret533 = println__T_string("test1: OK")
    case Result__Typ__string_Err:
        var x164 string = ty__163._0
        var e__164 string = x164
        var t346 string = "test1: " + e__164
        ret533 = println__T_string(t346)
    }
    return ret533
}

func test2(st__165 *ref_state_x) struct{} {
    var ret534 struct{}
    reset_type_variables(st__165)
    var empty_env__166 []Tuple2_string_Typ = nil
    var t347 Exp = mk_c1()
    var ty__167 Result__Typ__string = typeof(st__165, empty_env__166, t347)
    switch ty__167 := ty__167.(type) {
    case Result__Typ__string_Ok:
        ret534 = println__T_string("test2: OK")
    case Result__Typ__string_Err:
        var x167 string = ty__167._0
        var e__168 string = x167
        var t348 string = "test2: " + e__168
        ret534 = println__T_string(t348)
    }
    return ret534
}

func test3(st__169 *ref_state_x) struct{} {
    var ret535 struct{}
    reset_type_variables(st__169)
    var empty_env__170 []Tuple2_string_Typ = nil
    var t350 Exp = mk_c1()
    var t349 *ref_exp_x = ref__Ref_Exp(t350)
    var t352 Exp = Var{
        _0: "x",
    }
    var t351 *ref_exp_x = ref__Ref_Exp(t352)
    var exp__171 Exp = Let{
        _0: "x",
        _1: t349,
        _2: t351,
    }
    var ty__172 Result__Typ__string = typeof(st__169, empty_env__170, exp__171)
    switch ty__172 := ty__172.(type) {
    case Result__Typ__string_Ok:
        ret535 = println__T_string("test3: OK")
    case Result__Typ__string_Err:
        var x170 string = ty__172._0
        var e__173 string = x170
        var t353 string = "test3: " + e__173
        ret535 = println__T_string(t353)
    }
    return ret535
}

func test4(st__174 *ref_state_x) struct{} {
    var ret536 struct{}
    reset_type_variables(st__174)
    var empty_env__175 []Tuple2_string_Typ = nil
    var t357 Exp = Var{
        _0: "z",
    }
    var t356 *ref_exp_x = ref__Ref_Exp(t357)
    var t355 Exp = Lam{
        _0: "z",
        _1: t356,
    }
    var t354 *ref_exp_x = ref__Ref_Exp(t355)
    var t359 Exp = Var{
        _0: "y",
    }
    var t358 *ref_exp_x = ref__Ref_Exp(t359)
    var exp__176 Exp = Let{
        _0: "y",
        _1: t354,
        _2: t358,
    }
    var ty__177 Result__Typ__string = typeof(st__174, empty_env__175, exp__176)
    switch ty__177 := ty__177.(type) {
    case Result__Typ__string_Ok:
        ret536 = println__T_string("test4: OK")
    case Result__Typ__string_Err:
        var x173 string = ty__177._0
        var e__178 string = x173
        var t360 string = "test4: " + e__178
        ret536 = println__T_string(t360)
    }
    return ret536
}

func test5(st__179 *ref_state_x) struct{} {
    var ret537 struct{}
    reset_type_variables(st__179)
    var empty_env__180 []Tuple2_string_Typ = nil
    var t366 Exp = Var{
        _0: "z",
    }
    var t365 *ref_exp_x = ref__Ref_Exp(t366)
    var t364 Exp = Lam{
        _0: "z",
        _1: t365,
    }
    var t363 *ref_exp_x = ref__Ref_Exp(t364)
    var t368 Exp = Var{
        _0: "y",
    }
    var t367 *ref_exp_x = ref__Ref_Exp(t368)
    var t362 Exp = Let{
        _0: "y",
        _1: t363,
        _2: t367,
    }
    var t361 *ref_exp_x = ref__Ref_Exp(t362)
    var exp__181 Exp = Lam{
        _0: "x",
        _1: t361,
    }
    var ty__182 Result__Typ__string = typeof(st__179, empty_env__180, exp__181)
    switch ty__182 := ty__182.(type) {
    case Result__Typ__string_Ok:
        ret537 = println__T_string("test5: OK")
    case Result__Typ__string_Err:
        var x176 string = ty__182._0
        var e__183 string = x176
        var t369 string = "test5: " + e__183
        ret537 = println__T_string(t369)
    }
    return ret537
}

func test6(st__184 *ref_state_x) struct{} {
    var ret538 struct{}
    reset_type_variables(st__184)
    var empty_env__185 []Tuple2_string_Typ = nil
    var t375 Exp = Var{
        _0: "z",
    }
    var t374 *ref_exp_x = ref__Ref_Exp(t375)
    var t373 Exp = Lam{
        _0: "z",
        _1: t374,
    }
    var t372 *ref_exp_x = ref__Ref_Exp(t373)
    var t379 Exp = Var{
        _0: "y",
    }
    var t378 *ref_exp_x = ref__Ref_Exp(t379)
    var t381 Exp = Var{
        _0: "x",
    }
    var t380 *ref_exp_x = ref__Ref_Exp(t381)
    var t377 Exp = App{
        _0: t378,
        _1: t380,
    }
    var t376 *ref_exp_x = ref__Ref_Exp(t377)
    var t371 Exp = Let{
        _0: "y",
        _1: t372,
        _2: t376,
    }
    var t370 *ref_exp_x = ref__Ref_Exp(t371)
    var exp__186 Exp = Lam{
        _0: "x",
        _1: t370,
    }
    var ty__187 Result__Typ__string = typeof(st__184, empty_env__185, exp__186)
    switch ty__187 := ty__187.(type) {
    case Result__Typ__string_Ok:
        ret538 = println__T_string("test6: OK")
    case Result__Typ__string_Err:
        var x179 string = ty__187._0
        var e__188 string = x179
        var t382 string = "test6: " + e__188
        ret538 = println__T_string(t382)
    }
    return ret538
}

func test7(st__189 *ref_state_x) struct{} {
    var ret539 struct{}
    reset_type_variables(st__189)
    var empty_env__190 []Tuple2_string_Typ = nil
    var t386 Exp = Var{
        _0: "x",
    }
    var t385 *ref_exp_x = ref__Ref_Exp(t386)
    var t388 Exp = Var{
        _0: "x",
    }
    var t387 *ref_exp_x = ref__Ref_Exp(t388)
    var t384 Exp = App{
        _0: t385,
        _1: t387,
    }
    var t383 *ref_exp_x = ref__Ref_Exp(t384)
    var exp__191 Exp = Lam{
        _0: "x",
        _1: t383,
    }
    var ty__192 Result__Typ__string = typeof(st__189, empty_env__190, exp__191)
    switch ty__192 := ty__192.(type) {
    case Result__Typ__string_Ok:
        ret539 = println__T_string("test7: FAIL - should have failed")
    case Result__Typ__string_Err:
        var x182 string = ty__192._0
        var e__193 string = x182
        ret539 = println__T_string(e__193)
    }
    return ret539
}

func test8(st__194 *ref_state_x) struct{} {
    var ret540 struct{}
    reset_type_variables(st__194)
    var empty_env__195 []Tuple2_string_Typ = nil
    var t390 Exp = Var{
        _0: "x",
    }
    var t389 *ref_exp_x = ref__Ref_Exp(t390)
    var t392 Exp = Var{
        _0: "x",
    }
    var t391 *ref_exp_x = ref__Ref_Exp(t392)
    var exp__196 Exp = Let{
        _0: "x",
        _1: t389,
        _2: t391,
    }
    var ty__197 Result__Typ__string = typeof(st__194, empty_env__195, exp__196)
    switch ty__197 := ty__197.(type) {
    case Result__Typ__string_Ok:
        ret540 = println__T_string("test8: FAIL - should have failed")
    case Result__Typ__string_Err:
        var x185 string = ty__197._0
        var e__198 string = x185
        ret540 = println__T_string(e__198)
    }
    return ret540
}

func test9(st__199 *ref_state_x) struct{} {
    var ret541 struct{}
    reset_type_variables(st__199)
    var empty_env__200 []Tuple2_string_Typ = nil
    var t396 Exp = Var{
        _0: "y",
    }
    var t395 *ref_exp_x = ref__Ref_Exp(t396)
    var t402 Exp = Var{
        _0: "y",
    }
    var t401 *ref_exp_x = ref__Ref_Exp(t402)
    var t404 Exp = Var{
        _0: "z",
    }
    var t403 *ref_exp_x = ref__Ref_Exp(t404)
    var t400 Exp = App{
        _0: t401,
        _1: t403,
    }
    var t399 *ref_exp_x = ref__Ref_Exp(t400)
    var t398 Exp = Lam{
        _0: "z",
        _1: t399,
    }
    var t397 *ref_exp_x = ref__Ref_Exp(t398)
    var t394 Exp = App{
        _0: t395,
        _1: t397,
    }
    var t393 *ref_exp_x = ref__Ref_Exp(t394)
    var exp__201 Exp = Lam{
        _0: "y",
        _1: t393,
    }
    var ty__202 Result__Typ__string = typeof(st__199, empty_env__200, exp__201)
    switch ty__202 := ty__202.(type) {
    case Result__Typ__string_Ok:
        ret541 = println__T_string("test9: FAIL - should have failed")
    case Result__Typ__string_Err:
        var x188 string = ty__202._0
        var e__203 string = x188
        ret541 = println__T_string(e__203)
    }
    return ret541
}

func test10(st__204 *ref_state_x) struct{} {
    var ret542 struct{}
    reset_type_variables(st__204)
    var empty_env__205 []Tuple2_string_Typ = nil
    var t405 Exp = mk_id()
    var id_exp__206 *ref_exp_x = ref__Ref_Exp(t405)
    var t409 Exp = Var{
        _0: "id",
    }
    var t408 *ref_exp_x = ref__Ref_Exp(t409)
    var t411 Exp = Var{
        _0: "id",
    }
    var t410 *ref_exp_x = ref__Ref_Exp(t411)
    var t407 Exp = App{
        _0: t408,
        _1: t410,
    }
    var t406 *ref_exp_x = ref__Ref_Exp(t407)
    var exp__207 Exp = Let{
        _0: "id",
        _1: id_exp__206,
        _2: t406,
    }
    var ty__208 Result__Typ__string = typeof(st__204, empty_env__205, exp__207)
    switch ty__208 := ty__208.(type) {
    case Result__Typ__string_Ok:
        ret542 = println__T_string("test10: OK")
    case Result__Typ__string_Err:
        var x191 string = ty__208._0
        var e__209 string = x191
        var t412 string = "test10: " + e__209
        ret542 = println__T_string(t412)
    }
    return ret542
}

func test11(st__210 *ref_state_x) struct{} {
    var ret543 struct{}
    reset_type_variables(st__210)
    var empty_env__211 []Tuple2_string_Typ = nil
    var t413 Exp = mk_id()
    var id_exp__212 *ref_exp_x = ref__Ref_Exp(t413)
    var t415 Exp = Var{
        _0: "x",
    }
    var t414 *ref_exp_x = ref__Ref_Exp(t415)
    var z_app__213 Exp = App{
        _0: t414,
        _1: id_exp__212,
    }
    var t416 *ref_exp_x = ref__Ref_Exp(z_app__213)
    var t418 Exp = Var{
        _0: "z",
    }
    var t417 *ref_exp_x = ref__Ref_Exp(t418)
    var let_z__214 Exp = Let{
        _0: "z",
        _1: t416,
        _2: t417,
    }
    var t419 *ref_exp_x = ref__Ref_Exp(let_z__214)
    var t421 Exp = Var{
        _0: "y",
    }
    var t420 *ref_exp_x = ref__Ref_Exp(t421)
    var let_y__215 Exp = Let{
        _0: "y",
        _1: t419,
        _2: t420,
    }
    var t423 Exp = mk_c1()
    var t422 *ref_exp_x = ref__Ref_Exp(t423)
    var t424 *ref_exp_x = ref__Ref_Exp(let_y__215)
    var exp__216 Exp = Let{
        _0: "x",
        _1: t422,
        _2: t424,
    }
    var ty__217 Result__Typ__string = typeof(st__210, empty_env__211, exp__216)
    switch ty__217 := ty__217.(type) {
    case Result__Typ__string_Ok:
        ret543 = println__T_string("test11: OK")
    case Result__Typ__string_Err:
        var x194 string = ty__217._0
        var e__218 string = x194
        var t425 string = "test11: " + e__218
        ret543 = println__T_string(t425)
    }
    return ret543
}

func test12(st__219 *ref_state_x) struct{} {
    var ret544 struct{}
    reset_type_variables(st__219)
    var empty_env__220 []Tuple2_string_Typ = nil
    var t429 Exp = Var{
        _0: "y",
    }
    var t428 *ref_exp_x = ref__Ref_Exp(t429)
    var t431 Exp = Var{
        _0: "x",
    }
    var t430 *ref_exp_x = ref__Ref_Exp(t431)
    var t427 Exp = App{
        _0: t428,
        _1: t430,
    }
    var t426 *ref_exp_x = ref__Ref_Exp(t427)
    var inner_lam__221 Exp = Lam{
        _0: "x",
        _1: t426,
    }
    var t435 Exp = Var{
        _0: "x",
    }
    var t434 *ref_exp_x = ref__Ref_Exp(t435)
    var t437 Exp = Var{
        _0: "y",
    }
    var t436 *ref_exp_x = ref__Ref_Exp(t437)
    var t433 Exp = App{
        _0: t434,
        _1: t436,
    }
    var t432 *ref_exp_x = ref__Ref_Exp(t433)
    var t438 *ref_exp_x = ref__Ref_Exp(inner_lam__221)
    var let_x__222 Exp = Let{
        _0: "x",
        _1: t432,
        _2: t438,
    }
    var t441 *ref_exp_x = ref__Ref_Exp(let_x__222)
    var t440 Exp = Lam{
        _0: "y",
        _1: t441,
    }
    var t439 *ref_exp_x = ref__Ref_Exp(t440)
    var exp__223 Exp = Lam{
        _0: "x",
        _1: t439,
    }
    var ty__224 Result__Typ__string = typeof(st__219, empty_env__220, exp__223)
    switch ty__224 := ty__224.(type) {
    case Result__Typ__string_Ok:
        ret544 = println__T_string("test12: OK")
    case Result__Typ__string_Err:
        var x197 string = ty__224._0
        var e__225 string = x197
        var t442 string = "test12: " + e__225
        ret544 = println__T_string(t442)
    }
    return ret544
}

func test13(st__226 *ref_state_x) struct{} {
    var ret545 struct{}
    reset_type_variables(st__226)
    var empty_env__227 []Tuple2_string_Typ = nil
    var t446 Exp = Var{
        _0: "x",
    }
    var t445 *ref_exp_x = ref__Ref_Exp(t446)
    var t448 Exp = Var{
        _0: "y",
    }
    var t447 *ref_exp_x = ref__Ref_Exp(t448)
    var t444 Exp = Let{
        _0: "y",
        _1: t445,
        _2: t447,
    }
    var t443 *ref_exp_x = ref__Ref_Exp(t444)
    var exp__228 Exp = Lam{
        _0: "x",
        _1: t443,
    }
    var ty__229 Result__Typ__string = typeof(st__226, empty_env__227, exp__228)
    switch ty__229 := ty__229.(type) {
    case Result__Typ__string_Ok:
        ret545 = println__T_string("test13: OK")
    case Result__Typ__string_Err:
        var x200 string = ty__229._0
        var e__230 string = x200
        var t449 string = "test13: " + e__230
        ret545 = println__T_string(t449)
    }
    return ret545
}

func test14(st__231 *ref_state_x) struct{} {
    var ret546 struct{}
    reset_type_variables(st__231)
    var empty_env__232 []Tuple2_string_Typ = nil
    var t455 Exp = Var{
        _0: "x",
    }
    var t454 *ref_exp_x = ref__Ref_Exp(t455)
    var t453 Exp = Lam{
        _0: "z",
        _1: t454,
    }
    var t452 *ref_exp_x = ref__Ref_Exp(t453)
    var t457 Exp = Var{
        _0: "y",
    }
    var t456 *ref_exp_x = ref__Ref_Exp(t457)
    var t451 Exp = Let{
        _0: "y",
        _1: t452,
        _2: t456,
    }
    var t450 *ref_exp_x = ref__Ref_Exp(t451)
    var exp__233 Exp = Lam{
        _0: "x",
        _1: t450,
    }
    var ty__234 Result__Typ__string = typeof(st__231, empty_env__232, exp__233)
    switch ty__234 := ty__234.(type) {
    case Result__Typ__string_Ok:
        ret546 = println__T_string("test14: OK")
    case Result__Typ__string_Err:
        var x203 string = ty__234._0
        var e__235 string = x203
        var t458 string = "test14: " + e__235
        ret546 = println__T_string(t458)
    }
    return ret546
}

func test15(st__236 *ref_state_x) struct{} {
    var ret547 struct{}
    reset_type_variables(st__236)
    var empty_env__237 []Tuple2_string_Typ = nil
    var t466 Exp = Var{
        _0: "x",
    }
    var t465 *ref_exp_x = ref__Ref_Exp(t466)
    var t468 Exp = Var{
        _0: "z",
    }
    var t467 *ref_exp_x = ref__Ref_Exp(t468)
    var t464 Exp = App{
        _0: t465,
        _1: t467,
    }
    var t463 *ref_exp_x = ref__Ref_Exp(t464)
    var t462 Exp = Lam{
        _0: "z",
        _1: t463,
    }
    var t461 *ref_exp_x = ref__Ref_Exp(t462)
    var t470 Exp = Var{
        _0: "y",
    }
    var t469 *ref_exp_x = ref__Ref_Exp(t470)
    var t460 Exp = Let{
        _0: "y",
        _1: t461,
        _2: t469,
    }
    var t459 *ref_exp_x = ref__Ref_Exp(t460)
    var exp__238 Exp = Lam{
        _0: "x",
        _1: t459,
    }
    var ty__239 Result__Typ__string = typeof(st__236, empty_env__237, exp__238)
    switch ty__239 := ty__239.(type) {
    case Result__Typ__string_Ok:
        ret547 = println__T_string("test15: OK")
    case Result__Typ__string_Err:
        var x206 string = ty__239._0
        var e__240 string = x206
        var t471 string = "test15: " + e__240
        ret547 = println__T_string(t471)
    }
    return ret547
}

func test16(st__241 *ref_state_x) struct{} {
    var ret548 struct{}
    reset_type_variables(st__241)
    var empty_env__242 []Tuple2_string_Typ = nil
    var t473 Exp = Var{
        _0: "x",
    }
    var t472 *ref_exp_x = ref__Ref_Exp(t473)
    var t475 Exp = Var{
        _0: "y",
    }
    var t474 *ref_exp_x = ref__Ref_Exp(t475)
    var inner_app__243 Exp = App{
        _0: t472,
        _1: t474,
    }
    var t479 Exp = Var{
        _0: "x",
    }
    var t478 *ref_exp_x = ref__Ref_Exp(t479)
    var t481 Exp = Var{
        _0: "y",
    }
    var t480 *ref_exp_x = ref__Ref_Exp(t481)
    var t477 Exp = App{
        _0: t478,
        _1: t480,
    }
    var t476 *ref_exp_x = ref__Ref_Exp(t477)
    var t482 *ref_exp_x = ref__Ref_Exp(inner_app__243)
    var let_x__244 Exp = Let{
        _0: "x",
        _1: t476,
        _2: t482,
    }
    var t485 *ref_exp_x = ref__Ref_Exp(let_x__244)
    var t484 Exp = Lam{
        _0: "y",
        _1: t485,
    }
    var t483 *ref_exp_x = ref__Ref_Exp(t484)
    var exp__245 Exp = Lam{
        _0: "x",
        _1: t483,
    }
    var ty__246 Result__Typ__string = typeof(st__241, empty_env__242, exp__245)
    switch ty__246 := ty__246.(type) {
    case Result__Typ__string_Ok:
        ret548 = println__T_string("test16: OK")
    case Result__Typ__string_Err:
        var x209 string = ty__246._0
        var e__247 string = x209
        var t486 string = "test16: " + e__247
        ret548 = println__T_string(t486)
    }
    return ret548
}

func test17(st__248 *ref_state_x) struct{} {
    var ret549 struct{}
    reset_type_variables(st__248)
    var empty_env__249 []Tuple2_string_Typ = nil
    var t490 Exp = Var{
        _0: "x",
    }
    var t489 *ref_exp_x = ref__Ref_Exp(t490)
    var t494 Exp = Var{
        _0: "y",
    }
    var t493 *ref_exp_x = ref__Ref_Exp(t494)
    var t496 Exp = Var{
        _0: "y",
    }
    var t495 *ref_exp_x = ref__Ref_Exp(t496)
    var t492 Exp = App{
        _0: t493,
        _1: t495,
    }
    var t491 *ref_exp_x = ref__Ref_Exp(t492)
    var t488 Exp = Let{
        _0: "y",
        _1: t489,
        _2: t491,
    }
    var t487 *ref_exp_x = ref__Ref_Exp(t488)
    var exp__250 Exp = Lam{
        _0: "x",
        _1: t487,
    }
    var ty__251 Result__Typ__string = typeof(st__248, empty_env__249, exp__250)
    switch ty__251 := ty__251.(type) {
    case Result__Typ__string_Ok:
        ret549 = println__T_string("test17: FAIL - should have failed")
    case Result__Typ__string_Err:
        var x212 string = ty__251._0
        var e__252 string = x212
        ret549 = println__T_string(e__252)
    }
    return ret549
}

func test18(st__253 *ref_state_x) struct{} {
    var ret550 struct{}
    reset_gensym(st__253)
    var empty_env__254 []Tuple2_string_Typ = nil
    var t497 Exp = mk_id()
    var id_exp__255 *ref_exp_x = ref__Ref_Exp(t497)
    var t499 Exp = Var{
        _0: "x",
    }
    var t498 *ref_exp_x = ref__Ref_Exp(t499)
    var z_app__256 Exp = App{
        _0: t498,
        _1: id_exp__255,
    }
    var t500 *ref_exp_x = ref__Ref_Exp(z_app__256)
    var t502 Exp = Var{
        _0: "z",
    }
    var t501 *ref_exp_x = ref__Ref_Exp(t502)
    var let_z__257 Exp = Let{
        _0: "z",
        _1: t500,
        _2: t501,
    }
    var t503 *ref_exp_x = ref__Ref_Exp(let_z__257)
    var t505 Exp = Var{
        _0: "y",
    }
    var t504 *ref_exp_x = ref__Ref_Exp(t505)
    var let_y__258 Exp = Let{
        _0: "y",
        _1: t503,
        _2: t504,
    }
    var t506 *ref_exp_x = ref__Ref_Exp(let_y__258)
    var exp__259 Exp = Lam{
        _0: "x",
        _1: t506,
    }
    var ty__260 Result__Typ__string = typeof(st__253, empty_env__254, exp__259)
    switch ty__260 := ty__260.(type) {
    case Result__Typ__string_Ok:
        ret550 = println__T_string("test18: OK")
    case Result__Typ__string_Err:
        var x215 string = ty__260._0
        var e__261 string = x215
        var t507 string = "test18: " + e__261
        ret550 = println__T_string(t507)
    }
    return ret550
}

func main0() struct{} {
    var ret551 struct{}
    var t508 State = state_new()
    var st__262 *ref_state_x = ref__Ref_State(t508)
    test1(st__262)
    test2(st__262)
    test3(st__262)
    test4(st__262)
    test5(st__262)
    test6(st__262)
    test7(st__262)
    test8(st__262)
    test9(st__262)
    test10(st__262)
    test11(st__262)
    test12(st__262)
    test13(st__262)
    test14(st__262)
    test15(st__262)
    test16(st__262)
    test17(st__262)
    test18(st__262)
    ret551 = println__T_string("\\nAll Done\\n")
    return ret551
}

func println__T_string(value__1 string) struct{} {
    var ret552 struct{}
    ret552 = string_println(value__1)
    return ret552
}

func main() {
    main0()
}
