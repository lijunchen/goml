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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Token struct {
    items []Token
}

func vec_new__Vec_5Token() *_goml_vec_Token {
    return &_goml_vec_Token{
        items: nil,
    }
}

func vec_with_capacity__Vec_5Token(capacity int) *_goml_vec_Token {
    return &_goml_vec_Token{
        items: make([]Token, 0, capacity),
    }
}

func vec_push__Vec_5Token(vec *_goml_vec_Token, elem Token) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int {
    return int(len(vec.items))
}

type _goml_vec_Binding struct {
    items []Binding
}

func vec_new__Vec_7Binding() *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: nil,
    }
}

func vec_with_capacity__Vec_7Binding(capacity int) *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: make([]Binding, 0, capacity),
    }
}

func vec_push__Vec_7Binding(vec *_goml_vec_Binding, elem Binding) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int {
    return int(len(vec.items))
}

type _goml_vec_SExpr struct {
    items []SExpr
}

func vec_new__Vec_5SExpr() *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: nil,
    }
}

func vec_with_capacity__Vec_5SExpr(capacity int) *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: make([]SExpr, 0, capacity),
    }
}

func vec_push__Vec_5SExpr(vec *_goml_vec_SExpr, elem SExpr) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int {
    return int(len(vec.items))
}

type _goml_vec_Value struct {
    items []Value
}

func vec_new__Vec_5Value() *_goml_vec_Value {
    return &_goml_vec_Value{
        items: nil,
    }
}

func vec_with_capacity__Vec_5Value(capacity int) *_goml_vec_Value {
    return &_goml_vec_Value{
        items: make([]Value, 0, capacity),
    }
}

func vec_push__Vec_5Value(vec *_goml_vec_Value, elem Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: make([]string, 0, capacity),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Token_x struct {
    value *_goml_vec_Token
}

func ref__Ref_10Vec_5Token(value *_goml_vec_Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) *_goml_vec_Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value *_goml_vec_Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Value_x struct {
    value Value
}

func ref__Ref_5Value(value Value) *ref_Value_x {
    return &ref_Value_x{
        value: value,
    }
}

func ref_get__Ref_5Value(reference *ref_Value_x) Value {
    return reference.value
}

func ref_set__Ref_5Value(reference *ref_Value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5SExpr_x struct {
    value *_goml_vec_SExpr
}

func ref__Ref_10Vec_5SExpr(value *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value *_goml_vec_SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value *_goml_vec_Binding
}

func ref__Ref_12Vec_7Binding(value *_goml_vec_Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) *_goml_vec_Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value *_goml_vec_Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value *_goml_vec_string
}

func ref__Ref_11Vec_6string(value *_goml_vec_string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) *_goml_vec_string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value *_goml_vec_string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value *_goml_vec_Value
}

func ref__Ref_10Vec_5Value(value *_goml_vec_Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) *_goml_vec_Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value *_goml_vec_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_3int struct {
    _0 Token
    _1 int
}

type Tuple2_10Vec_5SExpr_3int struct {
    _0 *_goml_vec_SExpr
    _1 int
}

type Tuple2_5SExpr_3int struct {
    _0 SExpr
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Binding struct {
    name string
    value Value
}

type Lambda struct {
    params *_goml_vec_string
    body SExpr
    env *_goml_vec_Binding
    global *ref_Vec_7Binding_x
}

type Ordering int32

type Token interface {
    isToken()
}

type LParen struct {}

func (_ LParen) isToken() {}

type RParen struct {}

func (_ RParen) isToken() {}

type Token_Sym struct {
    _0 string
}

func (_ Token_Sym) isToken() {}

type Token_Int struct {
    _0 int32
}

func (_ Token_Int) isToken() {}

type Token_Bool struct {
    _0 bool
}

func (_ Token_Bool) isToken() {}

type Value interface {
    isValue()
}

type Value_Int struct {
    _0 int32
}

func (_ Value_Int) isValue() {}

type Value_Bool struct {
    _0 bool
}

func (_ Value_Bool) isValue() {}

type Func struct {
    _0 Lambda
}

func (_ Func) isValue() {}

type Nil struct {}

func (_ Nil) isValue() {}

type SExpr interface {
    isSExpr()
}

type SExpr_Int struct {
    _0 int32
}

func (_ SExpr_Int) isSExpr() {}

type SExpr_Bool struct {
    _0 bool
}

func (_ SExpr_Bool) isSExpr() {}

type SExpr_Sym struct {
    _0 string
}

func (_ SExpr_Sym) isSExpr() {}

type List struct {
    _0 *_goml_vec_SExpr
}

func (_ List) isSExpr() {}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func is_int_text(text__2 string) bool {
    var len__3 int
    var inline1499 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1499
    var t561 bool = len__3 == 0
    if t561 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1496 int = 0
        var inline1497 *ref_int_x = ref__Ref_3int(inline1496)
        i__4 = inline1497
        var saw_digit__5 *ref_bool_x
        var inline1493 bool = false
        var inline1494 *ref_bool_x = ref__Ref_4bool(inline1493)
        saw_digit__5 = inline1494
        var ok__6 *ref_bool_x
        var inline1490 bool = true
        var inline1491 *ref_bool_x = ref__Ref_4bool(inline1490)
        ok__6 = inline1491
        var started__7 *ref_bool_x
        var inline1487 bool = false
        var inline1488 *ref_bool_x = ref__Ref_4bool(inline1487)
        started__7 = inline1488
        Loop_loop567:
        for {
            var t586 bool
            var inline1481 bool = ref_get__Ref_4bool(ok__6)
            t586 = inline1481
            var jp569 bool
            if t586 {
                var t587 int
                var inline1450 int = ref_get__Ref_3int(i__4)
                t587 = inline1450
                var t588 bool = t587 < len__3
                jp569 = t588
            } else {
                jp569 = false
            }
            if jp569 {
                var t570 int
                var inline1479 int = ref_get__Ref_3int(i__4)
                t570 = inline1479
                var ch__8 rune
                var inline1477 rune = string_get(text__2, t570)
                ch__8 = inline1477
                var t583 bool
                var inline1475 bool = ref_get__Ref_4bool(started__7)
                t583 = inline1475
                var t584 bool = !t583
                var jp573 bool
                if t584 {
                    var t585 bool = ch__8 == 45
                    jp573 = t585
                } else {
                    jp573 = false
                }
                if jp573 {
                    var inline1456 bool = true
                    ref_set__Ref_4bool(started__7, inline1456)
                    var t574 int
                    var inline1454 int = ref_get__Ref_3int(i__4)
                    t574 = inline1454
                    var t575 int = t574 + 1
                    ref_set__Ref_3int(i__4, t575)
                    continue
                } else {
                    var t578 bool
                    var inline1472 bool = ch__8 >= 48
                    if inline1472 {
                        var inline1473 bool = ch__8 <= 57
                        t578 = inline1473
                    } else {
                        t578 = false
                    }
                    if t578 {
                        var inline1466 bool = true
                        ref_set__Ref_4bool(started__7, inline1466)
                        var inline1463 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1463)
                        var t579 int
                        var inline1461 int = ref_get__Ref_3int(i__4)
                        t579 = inline1461
                        var t580 int = t579 + 1
                        ref_set__Ref_3int(i__4, t580)
                        continue
                    } else {
                        var inline1469 bool = false
                        ref_set__Ref_4bool(ok__6, inline1469)
                        continue
                    }
                }
            } else {
                break Loop_loop567
            }
        }
        var t565 bool
        var inline1485 bool = ref_get__Ref_4bool(ok__6)
        t565 = inline1485
        if t565 {
            var inline1483 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1483
        } else {
            return false
        }
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x
    var inline1540 bool = false
    var inline1541 *ref_bool_x = ref__Ref_4bool(inline1540)
    started__13 = inline1541
    var acc__14 *ref_int32_x
    var inline1537 int32 = 0
    var inline1538 *ref_int32_x = ref__Ref_5int32(inline1537)
    acc__14 = inline1538
    Loop_loop598:
    for {
        var t599 int
        var inline1529 int = ref_get__Ref_3int(i__11)
        t599 = inline1529
        var t600 bool = t599 < len__10
        if t600 {
            var t601 int
            var inline1527 int = ref_get__Ref_3int(i__11)
            t601 = inline1527
            var ch__15 rune
            var inline1525 rune = string_get(text__9, t601)
            ch__15 = inline1525
            var t614 bool
            var inline1523 bool = ref_get__Ref_4bool(started__13)
            t614 = inline1523
            var t615 bool = !t614
            var jp604 bool
            if t615 {
                var t616 bool = ch__15 == 45
                jp604 = t616
            } else {
                jp604 = false
            }
            if jp604 {
                var inline1508 bool = true
                ref_set__Ref_4bool(started__13, inline1508)
                var inline1505 bool = true
                ref_set__Ref_4bool(negative__12, inline1505)
                var t605 int
                var inline1503 int = ref_get__Ref_3int(i__11)
                t605 = inline1503
                var t606 int = t605 + 1
                ref_set__Ref_3int(i__11, t606)
                continue
            } else {
                var inline1520 bool = true
                ref_set__Ref_4bool(started__13, inline1520)
                var d__16 int32
                switch ch__15 {
                case 48:
                    d__16 = 0
                case 49:
                    d__16 = 1
                case 50:
                    d__16 = 2
                case 51:
                    d__16 = 3
                case 52:
                    d__16 = 4
                case 53:
                    d__16 = 5
                case 54:
                    d__16 = 6
                case 55:
                    d__16 = 7
                case 56:
                    d__16 = 8
                case 57:
                    d__16 = 9
                default:
                    d__16 = 0
                }
                var t608 int32
                var inline1517 int32 = ref_get__Ref_5int32(acc__14)
                t608 = inline1517
                var t609 int32 = t608 * 10
                var t610 int32 = t609 + d__16
                ref_set__Ref_5int32(acc__14, t610)
                var t611 int
                var inline1513 int = ref_get__Ref_3int(i__11)
                t611 = inline1513
                var t612 int = t611 + 1
                ref_set__Ref_3int(i__11, t612)
                continue
            }
        } else {
            break Loop_loop598
        }
    }
    var t594 bool
    var inline1535 bool = ref_get__Ref_4bool(negative__12)
    t594 = inline1535
    if t594 {
        var t595 int32
        var inline1531 int32 = ref_get__Ref_5int32(acc__14)
        t595 = inline1531
        var t596 int32 = 0 - t595
        return t596
    } else {
        var inline1533 int32 = ref_get__Ref_5int32(acc__14)
        return inline1533
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1582 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1582
    var text__21 *ref_string_x
    var inline1579 string = ""
    var inline1580 *ref_string_x = ref__Ref_6string(inline1579)
    text__21 = inline1580
    var i__22 *ref_int_x
    var inline1577 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1577
    var done__23 *ref_bool_x
    var inline1574 bool = false
    var inline1575 *ref_bool_x = ref__Ref_4bool(inline1574)
    done__23 = inline1575
    Loop_loop641:
    for {
        var t654 bool
        var inline1568 bool = ref_get__Ref_4bool(done__23)
        t654 = inline1568
        var t655 bool = !t654
        var jp643 bool
        if t655 {
            var t656 int
            var inline1543 int = ref_get__Ref_3int(i__22)
            t656 = inline1543
            var t657 bool = t656 < len__20
            jp643 = t657
        } else {
            jp643 = false
        }
        if jp643 {
            var t644 int
            var inline1566 int = ref_get__Ref_3int(i__22)
            t644 = inline1566
            var ch__24 rune
            var inline1564 rune = string_get(source__18, t644)
            ch__24 = inline1564
            var t646 bool
            var inline1558 bool = ch__24 == 40
            var inline1560 bool
            if inline1558 {
                inline1560 = true
            } else {
                var inline1562 bool = ch__24 == 41
                inline1560 = inline1562
            }
            if inline1560 {
                t646 = true
                if t646 {
                    var inline1545 bool = true
                    ref_set__Ref_4bool(done__23, inline1545)
                    continue
                } else {
                    var t648 string
                    var inline1556 string = ref_get__Ref_6string(text__21)
                    t648 = inline1556
                    var t649 string
                    var inline1554 string = char_to_string(ch__24)
                    t649 = inline1554
                    var t650 string = t648 + t649
                    ref_set__Ref_6string(text__21, t650)
                    var t651 int
                    var inline1550 int = ref_get__Ref_3int(i__22)
                    t651 = inline1550
                    var t652 int = t651 + 1
                    ref_set__Ref_3int(i__22, t652)
                    continue
                }
            } else {
                var inline1561 bool = ch__24 == 32
                t646 = inline1561
                if t646 {
                    var inline1545 bool = true
                    ref_set__Ref_4bool(done__23, inline1545)
                    continue
                } else {
                    var t648 string
                    var inline1556 string = ref_get__Ref_6string(text__21)
                    t648 = inline1556
                    var t649 string
                    var inline1554 string = char_to_string(ch__24)
                    t649 = inline1554
                    var t650 string = t648 + t649
                    ref_set__Ref_6string(text__21, t650)
                    var t651 int
                    var inline1550 int = ref_get__Ref_3int(i__22)
                    t651 = inline1550
                    var t652 int = t651 + 1
                    ref_set__Ref_3int(i__22, t652)
                    continue
                }
            }
        } else {
            break Loop_loop641
        }
    }
    var atom__25 string
    var inline1572 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1572
    var jp630 Token
    switch atom__25 {
    case "true":
        var t633 Token = Token_Bool{
            _0: true,
        }
        jp630 = t633
    case "false":
        var t634 Token = Token_Bool{
            _0: false,
        }
        jp630 = t634
    default:
        var t637 bool = is_int_text(atom__25)
        if t637 {
            var t638 int32 = parse_int32(atom__25)
            var t639 Token = Token_Int{
                _0: t638,
            }
            jp630 = t639
        } else {
            var t640 Token = Token_Sym{
                _0: atom__25,
            }
            jp630 = t640
        }
    }
    var t631 int
    var inline1570 int = ref_get__Ref_3int(i__22)
    t631 = inline1570
    var t632 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp630,
        _1: t631,
    }
    return t632
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline1627 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline1627
    var toks0__29 *_goml_vec_Token
    var inline1625 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1625
    var toks__30 *ref_Vec_5Token_x
    var inline1623 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1623
    var i__31 *ref_int_x
    var inline1620 int = 0
    var inline1621 *ref_int_x = ref__Ref_3int(inline1620)
    i__31 = inline1621
    Loop_loop662:
    for {
        var t663 int
        var inline1616 int = ref_get__Ref_3int(i__31)
        t663 = inline1616
        var t664 bool = t663 < len__28
        if t664 {
            var t665 int
            var inline1614 int = ref_get__Ref_3int(i__31)
            t665 = inline1614
            var ch__32 rune
            var inline1612 rune = string_get(source__27, t665)
            ch__32 = inline1612
            var t667 bool = ch__32 == 40
            if t667 {
                var t668 *_goml_vec_Token
                var inline1590 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t668 = inline1590
                var t669 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t668, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t669)
                var t670 int
                var inline1586 int = ref_get__Ref_3int(i__31)
                t670 = inline1586
                var t671 int = t670 + 1
                ref_set__Ref_3int(i__31, t671)
                continue
            } else {
                var t674 bool = ch__32 == 41
                if t674 {
                    var t675 *_goml_vec_Token
                    var inline1598 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t675 = inline1598
                    var t676 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t675, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t676)
                    var t677 int
                    var inline1594 int = ref_get__Ref_3int(i__31)
                    t677 = inline1594
                    var t678 int = t677 + 1
                    ref_set__Ref_3int(i__31, t678)
                    continue
                } else {
                    var t681 bool = ch__32 == 32
                    if t681 {
                        var t682 int
                        var inline1602 int = ref_get__Ref_3int(i__31)
                        t682 = inline1602
                        var t683 int = t682 + 1
                        ref_set__Ref_3int(i__31, t683)
                        continue
                    } else {
                        var t685 int
                        var inline1610 int = ref_get__Ref_3int(i__31)
                        t685 = inline1610
                        var mtmp424 Tuple2_5Token_3int = lex_atom(source__27, t685)
                        var x425 Token = mtmp424._0
                        var x426 int = mtmp424._1
                        var t686 *_goml_vec_Token
                        var inline1608 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t686 = inline1608
                        var t687 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t686, x425)
                        ref_set__Ref_10Vec_5Token(toks__30, t687)
                        ref_set__Ref_3int(i__31, x426)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop662
        }
    }
    var inline1618 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1618
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t691 int
    var inline1653 int = vec_len__Vec_7Binding(env__35)
    t691 = inline1653
    var t692 int = t691 - 1
    var i__37 *ref_int_x
    var inline1651 *ref_int_x = ref__Ref_3int(t692)
    i__37 = inline1651
    var result__38 *ref_Value_x
    var inline1649 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1649
    var done__39 *ref_bool_x
    var inline1646 bool = false
    var inline1647 *ref_bool_x = ref__Ref_4bool(inline1646)
    done__39 = inline1647
    Loop_loop695:
    for {
        var t707 bool
        var inline1642 bool = ref_get__Ref_4bool(done__39)
        t707 = inline1642
        var t708 bool = !t707
        var jp697 bool
        if t708 {
            var t709 int
            var inline1629 int = ref_get__Ref_3int(i__37)
            t709 = inline1629
            var t710 bool = t709 >= 0
            jp697 = t710
        } else {
            jp697 = false
        }
        if jp697 {
            var t698 int
            var inline1640 int = ref_get__Ref_3int(i__37)
            t698 = inline1640
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t698)
            var t700 string = binding__40.name
            var t701 bool = t700 == name__36
            if t701 {
                var t702 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t702)
                var inline1631 bool = true
                ref_set__Ref_4bool(done__39, inline1631)
                continue
            } else {
                var t704 int
                var inline1638 int = ref_get__Ref_3int(i__37)
                t704 = inline1638
                var t705 int = t704 - 1
                ref_set__Ref_3int(i__37, t705)
                continue
            }
        } else {
            break Loop_loop695
        }
    }
    var inline1644 Value = ref_get__Ref_5Value(result__38)
    return inline1644
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1689 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1689
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1687 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1687
    var i__49 *ref_int_x
    var inline1685 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1685
    var done__50 *ref_bool_x
    var inline1682 bool = false
    var inline1683 *ref_bool_x = ref__Ref_4bool(inline1682)
    done__50 = inline1683
    Loop_loop722:
    for {
        var t734 bool
        var inline1676 bool = ref_get__Ref_4bool(done__50)
        t734 = inline1676
        var t735 bool = !t734
        var jp724 bool
        if t735 {
            var t736 int
            var inline1657 int = ref_get__Ref_3int(i__49)
            t736 = inline1657
            var t737 int
            var inline1655 int = vec_len__Vec_5Token(tokens__45)
            t737 = inline1655
            var t738 bool = t736 < t737
            jp724 = t738
        } else {
            jp724 = false
        }
        if jp724 {
            var t725 int
            var inline1674 int = ref_get__Ref_3int(i__49)
            t725 = inline1674
            var mtmp435 Token = vec_get__Vec_5Token(tokens__45, t725)
            switch mtmp435.(type) {
            case RParen:
                var inline1663 bool = true
                ref_set__Ref_4bool(done__50, inline1663)
                var t727 int
                var inline1661 int = ref_get__Ref_3int(i__49)
                t727 = inline1661
                var t728 int = t727 + 1
                ref_set__Ref_3int(i__49, t728)
                continue
            default:
                var t730 int
                var inline1672 int = ref_get__Ref_3int(i__49)
                t730 = inline1672
                var mtmp440 Tuple2_5SExpr_3int = parse_expr(tokens__45, t730)
                var x441 SExpr = mtmp440._0
                var x442 int = mtmp440._1
                var t731 *_goml_vec_SExpr
                var inline1670 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t731 = inline1670
                var t732 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t731, x441)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t732)
                ref_set__Ref_3int(i__49, x442)
                continue
            }
        } else {
            break Loop_loop722
        }
    }
    var t719 *_goml_vec_SExpr
    var inline1680 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t719 = inline1680
    var t720 int
    var inline1678 int = ref_get__Ref_3int(i__49)
    t720 = inline1678
    var t721 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t719,
        _1: t720,
    }
    return t721
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp445 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp445.(type) {
    case LParen:
        var t743 int = start__54 + 1
        var mtmp449 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t743)
        var x450 *_goml_vec_SExpr = mtmp449._0
        var x451 int = mtmp449._1
        var t744 SExpr = List{
            _0: x450,
        }
        var t745 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t744,
            _1: x451,
        }
        return t745
    case RParen:
        var t746 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t747 int = start__54 + 1
        var t748 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t746,
            _1: t747,
        }
        return t748
    case Token_Sym:
        var x446 string = mtmp445.(Token_Sym)._0
        var t749 SExpr = SExpr_Sym{
            _0: x446,
        }
        var t750 int = start__54 + 1
        var t751 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t749,
            _1: t750,
        }
        return t751
    case Token_Int:
        var x447 int32 = mtmp445.(Token_Int)._0
        var t752 SExpr = SExpr_Int{
            _0: x447,
        }
        var t753 int = start__54 + 1
        var t754 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t752,
            _1: t753,
        }
        return t754
    case Token_Bool:
        var x448 bool = mtmp445.(Token_Bool)._0
        var t755 SExpr = SExpr_Bool{
            _0: x448,
        }
        var t756 int = start__54 + 1
        var t757 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t755,
            _1: t756,
        }
        return t757
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1709 int = 0
    var inline1710 *ref_int_x = ref__Ref_3int(inline1709)
    i__61 = inline1710
    var acc__62 *_goml_vec_SExpr
    var inline1707 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1707
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1705 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1705
    Loop_loop762:
    for {
        var t763 int
        var inline1701 int = ref_get__Ref_3int(i__61)
        t763 = inline1701
        var t764 int
        var inline1699 int = vec_len__Vec_5Token(tokens__60)
        t764 = inline1699
        var t765 bool = t763 < t764
        if t765 {
            var t766 int
            var inline1697 int = ref_get__Ref_3int(i__61)
            t766 = inline1697
            var mtmp452 Tuple2_5SExpr_3int = parse_expr(tokens__60, t766)
            var x453 SExpr = mtmp452._0
            var x454 int = mtmp452._1
            var t767 *_goml_vec_SExpr
            var inline1695 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t767 = inline1695
            var t768 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t767, x453)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t768)
            ref_set__Ref_3int(i__61, x454)
            continue
        } else {
            break Loop_loop762
        }
    }
    var inline1703 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1703
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x463 int32 = expr__72.(SExpr_Int)._0
        var t785 Value = Value_Int{
            _0: x463,
        }
        return t785
    case SExpr_Bool:
        var x464 bool = expr__72.(SExpr_Bool)._0
        var t786 Value = Value_Bool{
            _0: x464,
        }
        return t786
    case SExpr_Sym:
        var x465 string = expr__72.(SExpr_Sym)._0
        var t787 *_goml_vec_Binding
        var inline1720 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t787 = inline1720
        var inline1716 Value = env_lookup(local__73, x465)
        switch inline1716.(type) {
        case Nil:
            var inline1717 Value = env_lookup(t787, x465)
            return inline1717
        default:
            return inline1716
        }
    case List:
        var x466 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1722 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x466)
        var inline1723 bool = inline1722 == 0
        if inline1723 {
            return Nil{}
        } else {
            var inline1724 SExpr = vec_get__Vec_5SExpr(x466, 0)
            switch inline1724.(type) {
            case SExpr_Sym:
                var inline1725 string = inline1724.(SExpr_Sym)._0
                var inline1727 Value = eval_list_sym(inline1725, x466, local__73, global__74)
                return inline1727
            default:
                var inline1728 Value = eval(inline1724, local__73, global__74)
                var inline1729 *_goml_vec_Value = eval_args(x466, 1, local__73, global__74)
                var inline1730 Value = apply(inline1728, inline1729, global__74)
                return inline1730
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t804 Value = eval_begin(items__87, 1, local__88, global__89)
        return t804
    case "define":
        var t807 int
        var inline1742 int = vec_len__Vec_5SExpr(items__87)
        t807 = inline1742
        var t808 bool = t807 == 3
        if t808 {
            var mtmp471 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp471.(type) {
            case SExpr_Sym:
                var x474 string = mtmp471.(SExpr_Sym)._0
                var t811 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t811, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1740 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1740
                var t812 Binding = Binding{
                    name: x474,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t812)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t815 int
        var inline1750 int = vec_len__Vec_5SExpr(items__87)
        t815 = inline1750
        var t816 bool = t815 == 4
        if t816 {
            var t817 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t817, local__88, global__89)
            var t820 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1744 int32 = cond__94.(Value_Int)._0
                var inline1746 bool = inline1744 != 0
                t820 = inline1746
            case Value_Bool:
                var inline1747 bool = cond__94.(Value_Bool)._0
                t820 = inline1747
            case Func:
                t820 = true
            case Nil:
                t820 = false
            default:
                panic("non-exhaustive match")
            }
            if t820 {
                var t821 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t822 Value = eval(t821, local__88, global__89)
                return t822
            } else {
                var t823 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t824 Value = eval(t823, local__88, global__89)
                return t824
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t827 int
        var inline1752 int = vec_len__Vec_5SExpr(items__87)
        t827 = inline1752
        var t828 bool = t827 == 3
        if t828 {
            var mtmp477 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp477.(type) {
            case List:
                var x481 *_goml_vec_SExpr = mtmp477.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x481)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t831 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t832 Value = Func{
                    _0: t831,
                }
                return t832
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t833 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t834 Value = apply_builtin("+", t833)
        return t834
    case "-":
        var t835 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t836 Value = apply_builtin("-", t835)
        return t836
    case "*":
        var t837 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t838 Value = apply_builtin("*", t837)
        return t838
    case "/":
        var t839 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t840 Value = apply_builtin("/", t839)
        return t840
    case "=":
        var t841 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t842 Value = apply_builtin("=", t841)
        return t842
    default:
        var t843 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t843, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1754 Lambda = f__98.(Func)._0
            var inline1756 Value = apply_lambda(inline1754, args__99)
            return inline1756
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1774 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1774
    var last__105 *ref_Value_x
    var inline1772 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1772
    Loop_loop849:
    for {
        var t850 int
        var inline1768 int = ref_get__Ref_3int(i__104)
        t850 = inline1768
        var t851 int
        var inline1766 int = vec_len__Vec_5SExpr(items__100)
        t851 = inline1766
        var t852 bool = t850 < t851
        if t852 {
            var t853 int
            var inline1764 int = ref_get__Ref_3int(i__104)
            t853 = inline1764
            var t854 SExpr = vec_get__Vec_5SExpr(items__100, t853)
            var v__106 Value = eval(t854, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t855 int
            var inline1760 int = ref_get__Ref_3int(i__104)
            t855 = inline1760
            var t856 int = t855 + 1
            ref_set__Ref_3int(i__104, t856)
            continue
        } else {
            break Loop_loop849
        }
    }
    var inline1770 Value = ref_get__Ref_5Value(last__105)
    return inline1770
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1800 int = 0
    var inline1801 *ref_int_x = ref__Ref_3int(inline1800)
    i__108 = inline1801
    var acc__109 *_goml_vec_string
    var inline1798 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1798
    var params__110 *ref_Vec_6string_x
    var inline1796 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1796
    Loop_loop862:
    for {
        var t863 int
        var inline1792 int = ref_get__Ref_3int(i__108)
        t863 = inline1792
        var t864 int
        var inline1790 int = vec_len__Vec_5SExpr(items__107)
        t864 = inline1790
        var t865 bool = t863 < t864
        if t865 {
            var t866 int
            var inline1788 int = ref_get__Ref_3int(i__108)
            t866 = inline1788
            var mtmp484 SExpr = vec_get__Vec_5SExpr(items__107, t866)
            switch mtmp484.(type) {
            case SExpr_Sym:
                var x487 string = mtmp484.(SExpr_Sym)._0
                var t868 *_goml_vec_string
                var inline1782 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t868 = inline1782
                var t869 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t868, x487)
                ref_set__Ref_11Vec_6string(params__110, t869)
                var t870 int
                var inline1778 int = ref_get__Ref_3int(i__108)
                t870 = inline1778
                var t871 int = t870 + 1
                ref_set__Ref_3int(i__108, t871)
                continue
            default:
                var t873 int
                var inline1786 int = ref_get__Ref_3int(i__108)
                t873 = inline1786
                var t874 int = t873 + 1
                ref_set__Ref_3int(i__108, t874)
                continue
            }
        } else {
            break Loop_loop862
        }
    }
    var inline1794 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1794
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1823 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1823
    var acc__117 *_goml_vec_Value
    var inline1821 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1821
    var args__118 *ref_Vec_5Value_x
    var inline1819 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1819
    Loop_loop880:
    for {
        var t881 int
        var inline1815 int = ref_get__Ref_3int(i__116)
        t881 = inline1815
        var t882 int
        var inline1813 int = vec_len__Vec_5SExpr(items__112)
        t882 = inline1813
        var t883 bool = t881 < t882
        if t883 {
            var t884 int
            var inline1811 int = ref_get__Ref_3int(i__116)
            t884 = inline1811
            var t885 SExpr = vec_get__Vec_5SExpr(items__112, t884)
            var v__119 Value = eval(t885, local__114, global__115)
            var t886 *_goml_vec_Value
            var inline1809 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t886 = inline1809
            var t887 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t886, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t887)
            var t888 int
            var inline1805 int = ref_get__Ref_3int(i__116)
            t888 = inline1805
            var t889 int = t888 + 1
            ref_set__Ref_3int(i__116, t889)
            continue
        } else {
            break Loop_loop880
        }
    }
    var inline1817 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1817
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t897 int
        var inline1825 int = vec_len__Vec_5Value(args__121)
        t897 = inline1825
        var t898 bool = t897 == 2
        if t898 {
            var t899 Value = vec_get__Vec_5Value(args__121, 0)
            var t900 Value = vec_get__Vec_5Value(args__121, 1)
            switch t900.(type) {
            case Value_Int:
                var x496 int32 = t900.(Value_Int)._0
                switch t899.(type) {
                case Value_Int:
                    var x499 int32 = t899.(Value_Int)._0
                    var t905 bool = x499 == x496
                    var t906 Value = Value_Bool{
                        _0: t905,
                    }
                    return t906
                default:
                    var t907 Value = Value_Bool{
                        _0: false,
                    }
                    return t907
                }
            case Value_Bool:
                var x497 bool = t900.(Value_Bool)._0
                switch t899.(type) {
                case Value_Bool:
                    var x503 bool = t899.(Value_Bool)._0
                    var t910 bool = x503 == x497
                    var t911 Value = Value_Bool{
                        _0: t910,
                    }
                    return t911
                default:
                    var t912 Value = Value_Bool{
                        _0: false,
                    }
                    return t912
                }
            default:
                var t913 Value = Value_Bool{
                    _0: false,
                }
                return t913
            }
        } else {
            var t914 Value = Value_Bool{
                _0: false,
            }
            return t914
        }
    case "+":
        var i__126 *ref_int_x
        var inline1850 int = 0
        var inline1851 *ref_int_x = ref__Ref_3int(inline1850)
        i__126 = inline1851
        var acc__127 *ref_int32_x
        var inline1847 int32 = 0
        var inline1848 *ref_int32_x = ref__Ref_5int32(inline1847)
        acc__127 = inline1848
        Loop_loop918:
        for {
            var t919 int
            var inline1843 int = ref_get__Ref_3int(i__126)
            t919 = inline1843
            var t920 int
            var inline1841 int = vec_len__Vec_5Value(args__121)
            t920 = inline1841
            var t921 bool = t919 < t920
            if t921 {
                var t922 int
                var inline1839 int = ref_get__Ref_3int(i__126)
                t922 = inline1839
                var mtmp505 Value = vec_get__Vec_5Value(args__121, t922)
                switch mtmp505.(type) {
                case Value_Int:
                    var x506 int32 = mtmp505.(Value_Int)._0
                    var t924 int32
                    var inline1833 int32 = ref_get__Ref_5int32(acc__127)
                    t924 = inline1833
                    var t925 int32 = t924 + x506
                    ref_set__Ref_5int32(acc__127, t925)
                    var t926 int
                    var inline1829 int = ref_get__Ref_3int(i__126)
                    t926 = inline1829
                    var t927 int = t926 + 1
                    ref_set__Ref_3int(i__126, t927)
                    continue
                default:
                    var t929 int
                    var inline1837 int = ref_get__Ref_3int(i__126)
                    t929 = inline1837
                    var t930 int = t929 + 1
                    ref_set__Ref_3int(i__126, t930)
                    continue
                }
            } else {
                break Loop_loop918
            }
        }
        var t916 int32
        var inline1845 int32 = ref_get__Ref_5int32(acc__127)
        t916 = inline1845
        var t917 Value = Value_Int{
            _0: t916,
        }
        return t917
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
        var acc__130 *ref_int32_x
        var inline1873 int32 = 1
        var inline1874 *ref_int32_x = ref__Ref_5int32(inline1873)
        acc__130 = inline1874
        Loop_loop935:
        for {
            var t936 int
            var inline1869 int = ref_get__Ref_3int(i__129)
            t936 = inline1869
            var t937 int
            var inline1867 int = vec_len__Vec_5Value(args__121)
            t937 = inline1867
            var t938 bool = t936 < t937
            if t938 {
                var t939 int
                var inline1865 int = ref_get__Ref_3int(i__129)
                t939 = inline1865
                var mtmp511 Value = vec_get__Vec_5Value(args__121, t939)
                switch mtmp511.(type) {
                case Value_Int:
                    var x512 int32 = mtmp511.(Value_Int)._0
                    var t941 int32
                    var inline1859 int32 = ref_get__Ref_5int32(acc__130)
                    t941 = inline1859
                    var t942 int32 = t941 * x512
                    ref_set__Ref_5int32(acc__130, t942)
                    var t943 int
                    var inline1855 int = ref_get__Ref_3int(i__129)
                    t943 = inline1855
                    var t944 int = t943 + 1
                    ref_set__Ref_3int(i__129, t944)
                    continue
                default:
                    var t946 int
                    var inline1863 int = ref_get__Ref_3int(i__129)
                    t946 = inline1863
                    var t947 int = t946 + 1
                    ref_set__Ref_3int(i__129, t947)
                    continue
                }
            } else {
                break Loop_loop935
            }
        }
        var t933 int32
        var inline1871 int32 = ref_get__Ref_5int32(acc__130)
        t933 = inline1871
        var t934 Value = Value_Int{
            _0: t933,
        }
        return t934
    case "-":
        var mtmp517 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp517 {
        case 1:
            var mtmp518 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp518.(type) {
            case Value_Int:
                var x519 int32 = mtmp518.(Value_Int)._0
                var t953 int32 = 0 - x519
                var t954 Value = Value_Int{
                    _0: t953,
                }
                return t954
            default:
                return Nil{}
            }
        case 2:
            var t955 Value = vec_get__Vec_5Value(args__121, 0)
            var t956 Value = vec_get__Vec_5Value(args__121, 1)
            switch t956.(type) {
            case Value_Int:
                var x525 int32 = t956.(Value_Int)._0
                switch t955.(type) {
                case Value_Int:
                    var x528 int32 = t955.(Value_Int)._0
                    var t961 int32 = x528 - x525
                    var t962 Value = Value_Int{
                        _0: t961,
                    }
                    return t962
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        default:
            return Nil{}
        }
    case "/":
        var t965 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t966 bool = t965 == 2
        if t966 {
            var t967 Value = vec_get__Vec_5Value(args__121, 0)
            var t968 Value = vec_get__Vec_5Value(args__121, 1)
            switch t968.(type) {
            case Value_Int:
                var x534 int32 = t968.(Value_Int)._0
                switch t967.(type) {
                case Value_Int:
                    var x537 int32 = t967.(Value_Int)._0
                    var t973 int32 = x537 / x534
                    var t974 Value = Value_Int{
                        _0: t973,
                    }
                    return t974
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    default:
        return Nil{}
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    switch func__137.(type) {
    case Func:
        var x542 Lambda = func__137.(Func)._0
        var t979 Value = apply_lambda(x542, args__138)
        return t979
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t982 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1901 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t982)
    env__143 = inline1901
    var i__144 *ref_int_x
    var inline1898 int = 0
    var inline1899 *ref_int_x = ref__Ref_3int(inline1898)
    i__144 = inline1899
    Loop_loop988:
    for {
        var t999 int
        var inline1894 int = ref_get__Ref_3int(i__144)
        t999 = inline1894
        var t1000 *_goml_vec_string = lambda__141.params
        var t1001 int
        var inline1892 int = vec_len__Vec_6string(t1000)
        t1001 = inline1892
        var t1002 bool = t999 < t1001
        var jp990 bool
        if t1002 {
            var t1003 int
            var inline1878 int = ref_get__Ref_3int(i__144)
            t1003 = inline1878
            var t1004 int
            var inline1876 int = vec_len__Vec_5Value(args__142)
            t1004 = inline1876
            var t1005 bool = t1003 < t1004
            jp990 = t1005
        } else {
            jp990 = false
        }
        if jp990 {
            var t991 *_goml_vec_string = lambda__141.params
            var t992 int
            var inline1890 int = ref_get__Ref_3int(i__144)
            t992 = inline1890
            var name__145 string = vec_get__Vec_6string(t991, t992)
            var t993 int
            var inline1888 int = ref_get__Ref_3int(i__144)
            t993 = inline1888
            var value__146 Value = vec_get__Vec_5Value(args__142, t993)
            var t994 *_goml_vec_Binding
            var inline1886 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t994 = inline1886
            var t995 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t994, t995)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t996 int
            var inline1882 int = ref_get__Ref_3int(i__144)
            t996 = inline1882
            var t997 int = t996 + 1
            ref_set__Ref_3int(i__144, t997)
            continue
        } else {
            break Loop_loop988
        }
    }
    var t984 SExpr = lambda__141.body
    var t985 *_goml_vec_Binding
    var inline1896 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t985 = inline1896
    var t986 *ref_Vec_7Binding_x = lambda__141.global
    var t987 Value = eval(t984, t985, t986)
    return t987
}

func main0() struct{} {
    var t1007 *_goml_vec_Binding
    var inline1929 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1007 = inline1929
    var global__148 *ref_Vec_7Binding_x
    var inline1927 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t1007)
    global__148 = inline1927
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t1008 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t1008)
    var t1009 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t1010 *_goml_vec_Binding
    var inline1925 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1010 = inline1925
    var result__151 Value = eval(t1009, t1010, global__148)
    var t1011 string
    switch result__151.(type) {
    case Value_Int:
        var inline1918 int32 = result__151.(Value_Int)._0
        var inline1920 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1918)
        t1011 = inline1920
    case Value_Bool:
        var inline1921 bool = result__151.(Value_Bool)._0
        var inline1923 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1921)
        t1011 = inline1923
    case Func:
        t1011 = "<lambda>"
    case Nil:
        t1011 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1011)
    _goml_runtime_core_string_println(inline1915)
    var t1012 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t1012)
    var t1013 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t1014 *_goml_vec_Binding
    var inline1913 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1014 = inline1913
    var result2__153 Value = eval(t1013, t1014, global__148)
    var t1015 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1906 int32 = result2__153.(Value_Int)._0
        var inline1908 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline1906)
        t1015 = inline1908
    case Value_Bool:
        var inline1909 bool = result2__153.(Value_Bool)._0
        var inline1911 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1909)
        t1015 = inline1911
    case Func:
        t1015 = "<lambda>"
    case Nil:
        t1015 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1903 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1015)
    _goml_runtime_core_string_println(inline1903)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t1018 int = _goml_runtime_core_string_len(self__35)
    return t1018
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t1021 *ref_int_x = ref__Ref_3int(value__431)
    return t1021
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__431 bool) *ref_bool_x {
    var t1024 *ref_bool_x = ref__Ref_4bool(value__431)
    return t1024
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__260 *_goml_vec_Token, elem__261 Token) *_goml_vec_Token {
    var t1068 int
    var inline1951 int = vec_len__Vec_5Token(self__260)
    t1068 = inline1951
    var t1069 int = t1068 + 1
    var result__262 *_goml_vec_Token
    var inline1949 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t1069)
    result__262 = inline1949
    var index__263 int = 0
    Loop_loop1071:
    for {
        var t1072 int
        var inline1945 int = vec_len__Vec_5Token(self__260)
        t1072 = inline1945
        var t1073 bool = index__263 < t1072
        if t1073 {
            var t1074 Token = vec_get__Vec_5Token(self__260, index__263)
            vec_push__Vec_5Token(result__262, t1074)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1075 int = compound_old190 + compound_value191
            index__263 = t1075
            continue
        } else {
            break Loop_loop1071
        }
    }
    vec_push__Vec_5Token(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__260 *_goml_vec_SExpr, elem__261 SExpr) *_goml_vec_SExpr {
    var t1104 int
    var inline1961 int = vec_len__Vec_5SExpr(self__260)
    t1104 = inline1961
    var t1105 int = t1104 + 1
    var result__262 *_goml_vec_SExpr
    var inline1959 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t1105)
    result__262 = inline1959
    var index__263 int = 0
    Loop_loop1107:
    for {
        var t1108 int
        var inline1955 int = vec_len__Vec_5SExpr(self__260)
        t1108 = inline1955
        var t1109 bool = index__263 < t1108
        if t1109 {
            var t1110 SExpr = vec_get__Vec_5SExpr(self__260, index__263)
            vec_push__Vec_5SExpr(result__262, t1110)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1111 int = compound_old190 + compound_value191
            index__263 = t1111
            continue
        } else {
            break Loop_loop1107
        }
    }
    vec_push__Vec_5SExpr(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t1117 string = _goml_runtime_core_int32_to_string(self__33)
    return t1117
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1120 string = _goml_runtime_core_bool_to_string(self__148)
    return t1120
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__273 *_goml_vec_SExpr) int {
    var t1126 int = vec_len__Vec_5SExpr(self__273)
    return t1126
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__260 *_goml_vec_Binding, elem__261 Binding) *_goml_vec_Binding {
    var t1129 int
    var inline1971 int = vec_len__Vec_7Binding(self__260)
    t1129 = inline1971
    var t1130 int = t1129 + 1
    var result__262 *_goml_vec_Binding
    var inline1969 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t1130)
    result__262 = inline1969
    var index__263 int = 0
    Loop_loop1132:
    for {
        var t1133 int
        var inline1965 int = vec_len__Vec_7Binding(self__260)
        t1133 = inline1965
        var t1134 bool = index__263 < t1133
        if t1134 {
            var t1135 Binding = vec_get__Vec_7Binding(self__260, index__263)
            vec_push__Vec_7Binding(result__262, t1135)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1136 int = compound_old190 + compound_value191
            index__263 = t1136
            continue
        } else {
            break Loop_loop1132
        }
    }
    vec_push__Vec_7Binding(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__260 *_goml_vec_string, elem__261 string) *_goml_vec_string {
    var t1151 int
    var inline1981 int = vec_len__Vec_6string(self__260)
    t1151 = inline1981
    var t1152 int = t1151 + 1
    var result__262 *_goml_vec_string
    var inline1979 *_goml_vec_string = vec_with_capacity__Vec_6string(t1152)
    result__262 = inline1979
    var index__263 int = 0
    Loop_loop1154:
    for {
        var t1155 int
        var inline1975 int = vec_len__Vec_6string(self__260)
        t1155 = inline1975
        var t1156 bool = index__263 < t1155
        if t1156 {
            var t1157 string = vec_get__Vec_6string(self__260, index__263)
            vec_push__Vec_6string(result__262, t1157)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1158 int = compound_old190 + compound_value191
            index__263 = t1158
            continue
        } else {
            break Loop_loop1154
        }
    }
    vec_push__Vec_6string(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__260 *_goml_vec_Value, elem__261 Value) *_goml_vec_Value {
    var t1173 int
    var inline1991 int = vec_len__Vec_5Value(self__260)
    t1173 = inline1991
    var t1174 int = t1173 + 1
    var result__262 *_goml_vec_Value
    var inline1989 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t1174)
    result__262 = inline1989
    var index__263 int = 0
    Loop_loop1176:
    for {
        var t1177 int
        var inline1985 int = vec_len__Vec_5Value(self__260)
        t1177 = inline1985
        var t1178 bool = index__263 < t1177
        if t1178 {
            var t1179 Value = vec_get__Vec_5Value(self__260, index__263)
            vec_push__Vec_5Value(result__262, t1179)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1180 int = compound_old190 + compound_value191
            index__263 = t1180
            continue
        } else {
            break Loop_loop1176
        }
    }
    vec_push__Vec_5Value(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__273 *_goml_vec_Value) int {
    var t1186 int = vec_len__Vec_5Value(self__273)
    return t1186
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t1203 rune = _goml_runtime_core_string_get("", -1)
        return t1203
    }
}

func char_to_string(value__29 rune) string {
    var t1208 uint32 = uint32(rune(value__29))
    var t1209 bool
    var inline1994 bool = t1208 <= 1114111
    if inline1994 {
        var inline1995 bool = t1208 >= 55296
        var inline1997 bool
        if inline1995 {
            var inline1999 bool = t1208 <= 57343
            inline1997 = inline1999
        } else {
            inline1997 = false
        }
        var inline1998 bool = !inline1997
        t1209 = inline1998
    } else {
        t1209 = false
    }
    if t1209 {
        var t1210 string = _goml_runtime_core_char_to_string(value__29)
        return t1210
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1356 bool = index__6 < 0
    var jp1354 bool
    if t1356 {
        jp1354 = true
    } else {
        var t1357 bool = index__6 >= length__7
        jp1354 = t1357
    }
    if jp1354 {
        var inline2001 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2001
    } else {
        var t1241 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1241))
        var t1244 bool = first__8 < 128
        if t1244 {
            var inline2003 int = 1
            var inline2004 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline2004._tag {
            case 0:
                var inline2005 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2005
            case 1:
                var inline2006 rune = inline2004._v1_0
                var inline2008 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2006,
                    _2: inline2003,
                }
                return inline2008
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1248 bool = first__8 < 194
            if t1248 {
                var inline2010 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2010
            } else {
                var t1252 bool = first__8 < 224
                if t1252 {
                    var t1265 int = length__7 - index__6
                    var t1266 bool = t1265 < 2
                    if t1266 {
                        var inline2012 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2012
                    } else {
                        var t1254 int = index__6 + 1
                        var t1255 uint8
                        var inline2026 uint8 = _goml_runtime_core_string_byte_get(value__5, t1254)
                        t1255 = inline2026
                        var second__9 uint32 = uint32(uint8(t1255))
                        var t1258 bool
                        var inline2023 bool = second__9 < 128
                        if inline2023 {
                            t1258 = true
                        } else {
                            var inline2024 bool = second__9 > 191
                            t1258 = inline2024
                        }
                        if t1258 {
                            var inline2014 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2014
                        } else {
                            var t1260_rhs uint32 = 31
                            var t1260 uint32 = first__8 & t1260_rhs
                            var t1261_rhs int = 6
                            var t1261 uint32 = t1260 << t1261_rhs
                            var t1262_rhs uint32 = 63
                            var t1262 uint32 = second__9 & t1262_rhs
                            var t1263 uint32 = t1261 | t1262
                            var inline2016 int = 2
                            var inline2017 Option__char = __goml_builtin_char_from_uint32(t1263)
                            switch inline2017._tag {
                            case 0:
                                var inline2018 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2018
                            case 1:
                                var inline2019 rune = inline2017._v1_0
                                var inline2021 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2019,
                                    _2: inline2016,
                                }
                                return inline2021
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1270 bool = first__8 < 240
                    if t1270 {
                        var t1303 int = length__7 - index__6
                        var t1304 bool = t1303 < 3
                        if t1304 {
                            var inline2028 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2028
                        } else {
                            var t1272 int = index__6 + 1
                            var t1273 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1272)
                            var second__10 uint32 = uint32(uint8(t1273))
                            var t1274 int = index__6 + 2
                            var t1275 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1274)
                            var third__11 uint32 = uint32(uint8(t1275))
                            var t1301 bool = utf8_invalid_continuation(second__10)
                            var jp1296 bool
                            if t1301 {
                                jp1296 = true
                            } else {
                                var inline2030 bool = third__11 < 128
                                if inline2030 {
                                    jp1296 = true
                                } else {
                                    var inline2031 bool = third__11 > 191
                                    jp1296 = inline2031
                                }
                            }
                            var jp1290 bool
                            if jp1296 {
                                jp1290 = true
                            } else {
                                var t1299 bool = first__8 == 224
                                if t1299 {
                                    var t1300 bool = second__10 < 160
                                    jp1290 = t1300
                                } else {
                                    jp1290 = false
                                }
                            }
                            var jp1279 bool
                            if jp1290 {
                                jp1279 = true
                            } else {
                                var t1293 bool = first__8 == 237
                                if t1293 {
                                    var t1294 bool = second__10 >= 160
                                    jp1279 = t1294
                                } else {
                                    jp1279 = false
                                }
                            }
                            if jp1279 {
                                var inline2033 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2033
                            } else {
                                var t1281_rhs uint32 = 15
                                var t1281 uint32 = first__8 & t1281_rhs
                                var t1282_rhs int = 12
                                var t1282 uint32 = t1281 << t1282_rhs
                                var t1283_rhs uint32 = 63
                                var t1283 uint32 = second__10 & t1283_rhs
                                var t1284_rhs int = 6
                                var t1284 uint32 = t1283 << t1284_rhs
                                var t1285 uint32 = t1282 | t1284
                                var t1286_rhs uint32 = 63
                                var t1286 uint32 = third__11 & t1286_rhs
                                var t1287 uint32 = t1285 | t1286
                                var inline2035 int = 3
                                var inline2036 Option__char = __goml_builtin_char_from_uint32(t1287)
                                switch inline2036._tag {
                                case 0:
                                    var inline2037 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2037
                                case 1:
                                    var inline2038 rune = inline2036._v1_0
                                    var inline2040 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2038,
                                        _2: inline2035,
                                    }
                                    return inline2040
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1308 bool = first__8 < 245
                        if t1308 {
                            var t1349 int = length__7 - index__6
                            var t1350 bool = t1349 < 4
                            if t1350 {
                                var t1351 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1351
                            } else {
                                var t1310 int = index__6 + 1
                                var t1311 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1310)
                                var second__12 uint32 = uint32(uint8(t1311))
                                var t1312 int = index__6 + 2
                                var t1313 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1312)
                                var third__13 uint32 = uint32(uint8(t1313))
                                var t1314 int = index__6 + 3
                                var t1315 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1314)
                                var fourth__14 uint32 = uint32(uint8(t1315))
                                var t1347 bool = utf8_invalid_continuation(second__12)
                                var jp1345 bool
                                if t1347 {
                                    jp1345 = true
                                } else {
                                    var t1348 bool = utf8_invalid_continuation(third__13)
                                    jp1345 = t1348
                                }
                                var jp1339 bool
                                if jp1345 {
                                    jp1339 = true
                                } else {
                                    var t1346 bool = utf8_invalid_continuation(fourth__14)
                                    jp1339 = t1346
                                }
                                var jp1333 bool
                                if jp1339 {
                                    jp1333 = true
                                } else {
                                    var t1342 bool = first__8 == 240
                                    if t1342 {
                                        var t1343 bool = second__12 < 144
                                        jp1333 = t1343
                                    } else {
                                        jp1333 = false
                                    }
                                }
                                var jp1319 bool
                                if jp1333 {
                                    jp1319 = true
                                } else {
                                    var t1336 bool = first__8 == 244
                                    if t1336 {
                                        var t1337 bool = second__12 > 143
                                        jp1319 = t1337
                                    } else {
                                        jp1319 = false
                                    }
                                }
                                if jp1319 {
                                    var t1320 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1320
                                } else {
                                    var t1321_rhs uint32 = 7
                                    var t1321 uint32 = first__8 & t1321_rhs
                                    var t1322_rhs int = 18
                                    var t1322 uint32 = t1321 << t1322_rhs
                                    var t1323_rhs uint32 = 63
                                    var t1323 uint32 = second__12 & t1323_rhs
                                    var t1324_rhs int = 12
                                    var t1324 uint32 = t1323 << t1324_rhs
                                    var t1325 uint32 = t1322 | t1324
                                    var t1326_rhs uint32 = 63
                                    var t1326 uint32 = third__13 & t1326_rhs
                                    var t1327_rhs int = 6
                                    var t1327 uint32 = t1326 << t1327_rhs
                                    var t1328 uint32 = t1325 | t1327
                                    var t1329_rhs uint32 = 63
                                    var t1329 uint32 = fourth__14 & t1329_rhs
                                    var t1330 uint32 = t1328 | t1329
                                    var t1331 Tuple3_4bool_4char_3int = utf8_valid_decode(t1330, 4)
                                    return t1331
                                }
                            }
                        } else {
                            var t1352 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1352
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1362 bool = value__4 <= 1114111
    if t1362 {
        var t1366 bool = value__4 >= 55296
        var jp1364 bool
        if t1366 {
            var t1367 bool = value__4 <= 57343
            jp1364 = t1367
        } else {
            jp1364 = false
        }
        var t1365 bool = !jp1364
        return t1365
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1370 int = _goml_runtime_core_string_len(self__36)
    return t1370
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1373 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1373
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1376 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1376
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field2069 rune
    var inline2044 bool = utf8_valid_scalar(value__0)
    if inline2044 {
        var inline2045 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2046 rune = inline2045._1
        commute_field2069 = inline2046
        var t1382 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2069,
            _2: width__1,
        }
        return t1382
    } else {
        var inline2042 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2042
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1387 bool = value__3 < 128
    if t1387 {
        return true
    } else {
        var t1388 bool = value__3 > 191
        return t1388
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1393 bool
    var inline2050 bool = value__30 <= 1114111
    if inline2050 {
        var inline2051 bool = value__30 >= 55296
        var inline2053 bool
        if inline2051 {
            var inline2055 bool = value__30 <= 57343
            inline2053 = inline2055
        } else {
            inline2053 = false
        }
        var inline2054 bool = !inline2053
        t1393 = inline2054
    } else {
        t1393 = false
    }
    if t1393 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1394 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1394
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
