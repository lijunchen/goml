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
    var inline1496 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1496
    var t558 bool = len__3 == 0
    if t558 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1493 int = 0
        var inline1494 *ref_int_x = ref__Ref_3int(inline1493)
        i__4 = inline1494
        var saw_digit__5 *ref_bool_x
        var inline1490 bool = false
        var inline1491 *ref_bool_x = ref__Ref_4bool(inline1490)
        saw_digit__5 = inline1491
        var ok__6 *ref_bool_x
        var inline1487 bool = true
        var inline1488 *ref_bool_x = ref__Ref_4bool(inline1487)
        ok__6 = inline1488
        var started__7 *ref_bool_x
        var inline1484 bool = false
        var inline1485 *ref_bool_x = ref__Ref_4bool(inline1484)
        started__7 = inline1485
        Loop_loop564:
        for {
            var t583 bool
            var inline1478 bool = ref_get__Ref_4bool(ok__6)
            t583 = inline1478
            var jp566 bool
            if t583 {
                var t584 int
                var inline1447 int = ref_get__Ref_3int(i__4)
                t584 = inline1447
                var t585 bool = t584 < len__3
                jp566 = t585
            } else {
                jp566 = false
            }
            if jp566 {
                var t567 int
                var inline1476 int = ref_get__Ref_3int(i__4)
                t567 = inline1476
                var ch__8 rune
                var inline1474 rune = string_get(text__2, t567)
                ch__8 = inline1474
                var t580 bool
                var inline1472 bool = ref_get__Ref_4bool(started__7)
                t580 = inline1472
                var t581 bool = !t580
                var jp570 bool
                if t581 {
                    var t582 bool = ch__8 == 45
                    jp570 = t582
                } else {
                    jp570 = false
                }
                if jp570 {
                    var inline1453 bool = true
                    ref_set__Ref_4bool(started__7, inline1453)
                    var t571 int
                    var inline1451 int = ref_get__Ref_3int(i__4)
                    t571 = inline1451
                    var t572 int = t571 + 1
                    ref_set__Ref_3int(i__4, t572)
                    continue
                } else {
                    var t575 bool
                    var inline1469 bool = ch__8 >= 48
                    if inline1469 {
                        var inline1470 bool = ch__8 <= 57
                        t575 = inline1470
                    } else {
                        t575 = false
                    }
                    if t575 {
                        var inline1463 bool = true
                        ref_set__Ref_4bool(started__7, inline1463)
                        var inline1460 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1460)
                        var t576 int
                        var inline1458 int = ref_get__Ref_3int(i__4)
                        t576 = inline1458
                        var t577 int = t576 + 1
                        ref_set__Ref_3int(i__4, t577)
                        continue
                    } else {
                        var inline1466 bool = false
                        ref_set__Ref_4bool(ok__6, inline1466)
                        continue
                    }
                }
            } else {
                break Loop_loop564
            }
        }
        var t562 bool
        var inline1482 bool = ref_get__Ref_4bool(ok__6)
        t562 = inline1482
        if t562 {
            var inline1480 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1480
        } else {
            return false
        }
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x
    var inline1537 bool = false
    var inline1538 *ref_bool_x = ref__Ref_4bool(inline1537)
    started__13 = inline1538
    var acc__14 *ref_int32_x
    var inline1534 int32 = 0
    var inline1535 *ref_int32_x = ref__Ref_5int32(inline1534)
    acc__14 = inline1535
    Loop_loop595:
    for {
        var t596 int
        var inline1526 int = ref_get__Ref_3int(i__11)
        t596 = inline1526
        var t597 bool = t596 < len__10
        if t597 {
            var t598 int
            var inline1524 int = ref_get__Ref_3int(i__11)
            t598 = inline1524
            var ch__15 rune
            var inline1522 rune = string_get(text__9, t598)
            ch__15 = inline1522
            var t611 bool
            var inline1520 bool = ref_get__Ref_4bool(started__13)
            t611 = inline1520
            var t612 bool = !t611
            var jp601 bool
            if t612 {
                var t613 bool = ch__15 == 45
                jp601 = t613
            } else {
                jp601 = false
            }
            if jp601 {
                var inline1505 bool = true
                ref_set__Ref_4bool(started__13, inline1505)
                var inline1502 bool = true
                ref_set__Ref_4bool(negative__12, inline1502)
                var t602 int
                var inline1500 int = ref_get__Ref_3int(i__11)
                t602 = inline1500
                var t603 int = t602 + 1
                ref_set__Ref_3int(i__11, t603)
                continue
            } else {
                var inline1517 bool = true
                ref_set__Ref_4bool(started__13, inline1517)
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
                var t605 int32
                var inline1514 int32 = ref_get__Ref_5int32(acc__14)
                t605 = inline1514
                var t606 int32 = t605 * 10
                var t607 int32 = t606 + d__16
                ref_set__Ref_5int32(acc__14, t607)
                var t608 int
                var inline1510 int = ref_get__Ref_3int(i__11)
                t608 = inline1510
                var t609 int = t608 + 1
                ref_set__Ref_3int(i__11, t609)
                continue
            }
        } else {
            break Loop_loop595
        }
    }
    var t591 bool
    var inline1532 bool = ref_get__Ref_4bool(negative__12)
    t591 = inline1532
    if t591 {
        var t592 int32
        var inline1528 int32 = ref_get__Ref_5int32(acc__14)
        t592 = inline1528
        var t593 int32 = 0 - t592
        return t593
    } else {
        var inline1530 int32 = ref_get__Ref_5int32(acc__14)
        return inline1530
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1579 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1579
    var text__21 *ref_string_x
    var inline1576 string = ""
    var inline1577 *ref_string_x = ref__Ref_6string(inline1576)
    text__21 = inline1577
    var i__22 *ref_int_x
    var inline1574 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1574
    var done__23 *ref_bool_x
    var inline1571 bool = false
    var inline1572 *ref_bool_x = ref__Ref_4bool(inline1571)
    done__23 = inline1572
    Loop_loop638:
    for {
        var t651 bool
        var inline1565 bool = ref_get__Ref_4bool(done__23)
        t651 = inline1565
        var t652 bool = !t651
        var jp640 bool
        if t652 {
            var t653 int
            var inline1540 int = ref_get__Ref_3int(i__22)
            t653 = inline1540
            var t654 bool = t653 < len__20
            jp640 = t654
        } else {
            jp640 = false
        }
        if jp640 {
            var t641 int
            var inline1563 int = ref_get__Ref_3int(i__22)
            t641 = inline1563
            var ch__24 rune
            var inline1561 rune = string_get(source__18, t641)
            ch__24 = inline1561
            var t643 bool
            var inline1555 bool = ch__24 == 40
            var inline1557 bool
            if inline1555 {
                inline1557 = true
            } else {
                var inline1559 bool = ch__24 == 41
                inline1557 = inline1559
            }
            if inline1557 {
                t643 = true
                if t643 {
                    var inline1542 bool = true
                    ref_set__Ref_4bool(done__23, inline1542)
                    continue
                } else {
                    var t645 string
                    var inline1553 string = ref_get__Ref_6string(text__21)
                    t645 = inline1553
                    var t646 string
                    var inline1551 string = char_to_string(ch__24)
                    t646 = inline1551
                    var t647 string = t645 + t646
                    ref_set__Ref_6string(text__21, t647)
                    var t648 int
                    var inline1547 int = ref_get__Ref_3int(i__22)
                    t648 = inline1547
                    var t649 int = t648 + 1
                    ref_set__Ref_3int(i__22, t649)
                    continue
                }
            } else {
                var inline1558 bool = ch__24 == 32
                t643 = inline1558
                if t643 {
                    var inline1542 bool = true
                    ref_set__Ref_4bool(done__23, inline1542)
                    continue
                } else {
                    var t645 string
                    var inline1553 string = ref_get__Ref_6string(text__21)
                    t645 = inline1553
                    var t646 string
                    var inline1551 string = char_to_string(ch__24)
                    t646 = inline1551
                    var t647 string = t645 + t646
                    ref_set__Ref_6string(text__21, t647)
                    var t648 int
                    var inline1547 int = ref_get__Ref_3int(i__22)
                    t648 = inline1547
                    var t649 int = t648 + 1
                    ref_set__Ref_3int(i__22, t649)
                    continue
                }
            }
        } else {
            break Loop_loop638
        }
    }
    var atom__25 string
    var inline1569 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1569
    var jp627 Token
    switch atom__25 {
    case "true":
        var t630 Token = Token_Bool{
            _0: true,
        }
        jp627 = t630
    case "false":
        var t631 Token = Token_Bool{
            _0: false,
        }
        jp627 = t631
    default:
        var t634 bool = is_int_text(atom__25)
        if t634 {
            var t635 int32 = parse_int32(atom__25)
            var t636 Token = Token_Int{
                _0: t635,
            }
            jp627 = t636
        } else {
            var t637 Token = Token_Sym{
                _0: atom__25,
            }
            jp627 = t637
        }
    }
    var t628 int
    var inline1567 int = ref_get__Ref_3int(i__22)
    t628 = inline1567
    var t629 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp627,
        _1: t628,
    }
    return t629
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline1624 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline1624
    var toks0__29 *_goml_vec_Token
    var inline1622 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1622
    var toks__30 *ref_Vec_5Token_x
    var inline1620 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1620
    var i__31 *ref_int_x
    var inline1617 int = 0
    var inline1618 *ref_int_x = ref__Ref_3int(inline1617)
    i__31 = inline1618
    Loop_loop659:
    for {
        var t660 int
        var inline1613 int = ref_get__Ref_3int(i__31)
        t660 = inline1613
        var t661 bool = t660 < len__28
        if t661 {
            var t662 int
            var inline1611 int = ref_get__Ref_3int(i__31)
            t662 = inline1611
            var ch__32 rune
            var inline1609 rune = string_get(source__27, t662)
            ch__32 = inline1609
            var t664 bool = ch__32 == 40
            if t664 {
                var t665 *_goml_vec_Token
                var inline1587 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t665 = inline1587
                var t666 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t665, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t666)
                var t667 int
                var inline1583 int = ref_get__Ref_3int(i__31)
                t667 = inline1583
                var t668 int = t667 + 1
                ref_set__Ref_3int(i__31, t668)
                continue
            } else {
                var t671 bool = ch__32 == 41
                if t671 {
                    var t672 *_goml_vec_Token
                    var inline1595 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t672 = inline1595
                    var t673 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t672, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t673)
                    var t674 int
                    var inline1591 int = ref_get__Ref_3int(i__31)
                    t674 = inline1591
                    var t675 int = t674 + 1
                    ref_set__Ref_3int(i__31, t675)
                    continue
                } else {
                    var t678 bool = ch__32 == 32
                    if t678 {
                        var t679 int
                        var inline1599 int = ref_get__Ref_3int(i__31)
                        t679 = inline1599
                        var t680 int = t679 + 1
                        ref_set__Ref_3int(i__31, t680)
                        continue
                    } else {
                        var t682 int
                        var inline1607 int = ref_get__Ref_3int(i__31)
                        t682 = inline1607
                        var mtmp421 Tuple2_5Token_3int = lex_atom(source__27, t682)
                        var x422 Token = mtmp421._0
                        var x423 int = mtmp421._1
                        var t683 *_goml_vec_Token
                        var inline1605 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t683 = inline1605
                        var t684 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t683, x422)
                        ref_set__Ref_10Vec_5Token(toks__30, t684)
                        ref_set__Ref_3int(i__31, x423)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop659
        }
    }
    var inline1615 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1615
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t688 int
    var inline1650 int = vec_len__Vec_7Binding(env__35)
    t688 = inline1650
    var t689 int = t688 - 1
    var i__37 *ref_int_x
    var inline1648 *ref_int_x = ref__Ref_3int(t689)
    i__37 = inline1648
    var result__38 *ref_Value_x
    var inline1646 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1646
    var done__39 *ref_bool_x
    var inline1643 bool = false
    var inline1644 *ref_bool_x = ref__Ref_4bool(inline1643)
    done__39 = inline1644
    Loop_loop692:
    for {
        var t704 bool
        var inline1639 bool = ref_get__Ref_4bool(done__39)
        t704 = inline1639
        var t705 bool = !t704
        var jp694 bool
        if t705 {
            var t706 int
            var inline1626 int = ref_get__Ref_3int(i__37)
            t706 = inline1626
            var t707 bool = t706 >= 0
            jp694 = t707
        } else {
            jp694 = false
        }
        if jp694 {
            var t695 int
            var inline1637 int = ref_get__Ref_3int(i__37)
            t695 = inline1637
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t695)
            var t697 string = binding__40.name
            var t698 bool = t697 == name__36
            if t698 {
                var t699 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t699)
                var inline1628 bool = true
                ref_set__Ref_4bool(done__39, inline1628)
                continue
            } else {
                var t701 int
                var inline1635 int = ref_get__Ref_3int(i__37)
                t701 = inline1635
                var t702 int = t701 - 1
                ref_set__Ref_3int(i__37, t702)
                continue
            }
        } else {
            break Loop_loop692
        }
    }
    var inline1641 Value = ref_get__Ref_5Value(result__38)
    return inline1641
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1686 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1686
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1684 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1684
    var i__49 *ref_int_x
    var inline1682 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1682
    var done__50 *ref_bool_x
    var inline1679 bool = false
    var inline1680 *ref_bool_x = ref__Ref_4bool(inline1679)
    done__50 = inline1680
    Loop_loop719:
    for {
        var t731 bool
        var inline1673 bool = ref_get__Ref_4bool(done__50)
        t731 = inline1673
        var t732 bool = !t731
        var jp721 bool
        if t732 {
            var t733 int
            var inline1654 int = ref_get__Ref_3int(i__49)
            t733 = inline1654
            var t734 int
            var inline1652 int = vec_len__Vec_5Token(tokens__45)
            t734 = inline1652
            var t735 bool = t733 < t734
            jp721 = t735
        } else {
            jp721 = false
        }
        if jp721 {
            var t722 int
            var inline1671 int = ref_get__Ref_3int(i__49)
            t722 = inline1671
            var mtmp432 Token = vec_get__Vec_5Token(tokens__45, t722)
            switch mtmp432.(type) {
            case RParen:
                var inline1660 bool = true
                ref_set__Ref_4bool(done__50, inline1660)
                var t724 int
                var inline1658 int = ref_get__Ref_3int(i__49)
                t724 = inline1658
                var t725 int = t724 + 1
                ref_set__Ref_3int(i__49, t725)
                continue
            default:
                var t727 int
                var inline1669 int = ref_get__Ref_3int(i__49)
                t727 = inline1669
                var mtmp437 Tuple2_5SExpr_3int = parse_expr(tokens__45, t727)
                var x438 SExpr = mtmp437._0
                var x439 int = mtmp437._1
                var t728 *_goml_vec_SExpr
                var inline1667 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t728 = inline1667
                var t729 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t728, x438)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t729)
                ref_set__Ref_3int(i__49, x439)
                continue
            }
        } else {
            break Loop_loop719
        }
    }
    var t716 *_goml_vec_SExpr
    var inline1677 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t716 = inline1677
    var t717 int
    var inline1675 int = ref_get__Ref_3int(i__49)
    t717 = inline1675
    var t718 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t716,
        _1: t717,
    }
    return t718
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp442 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp442.(type) {
    case LParen:
        var t740 int = start__54 + 1
        var mtmp446 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t740)
        var x447 *_goml_vec_SExpr = mtmp446._0
        var x448 int = mtmp446._1
        var t741 SExpr = List{
            _0: x447,
        }
        var t742 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t741,
            _1: x448,
        }
        return t742
    case RParen:
        var t743 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t744 int = start__54 + 1
        var t745 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t743,
            _1: t744,
        }
        return t745
    case Token_Sym:
        var x443 string = mtmp442.(Token_Sym)._0
        var t746 SExpr = SExpr_Sym{
            _0: x443,
        }
        var t747 int = start__54 + 1
        var t748 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t746,
            _1: t747,
        }
        return t748
    case Token_Int:
        var x444 int32 = mtmp442.(Token_Int)._0
        var t749 SExpr = SExpr_Int{
            _0: x444,
        }
        var t750 int = start__54 + 1
        var t751 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t749,
            _1: t750,
        }
        return t751
    case Token_Bool:
        var x445 bool = mtmp442.(Token_Bool)._0
        var t752 SExpr = SExpr_Bool{
            _0: x445,
        }
        var t753 int = start__54 + 1
        var t754 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t752,
            _1: t753,
        }
        return t754
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1706 int = 0
    var inline1707 *ref_int_x = ref__Ref_3int(inline1706)
    i__61 = inline1707
    var acc__62 *_goml_vec_SExpr
    var inline1704 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1704
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1702 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1702
    Loop_loop759:
    for {
        var t760 int
        var inline1698 int = ref_get__Ref_3int(i__61)
        t760 = inline1698
        var t761 int
        var inline1696 int = vec_len__Vec_5Token(tokens__60)
        t761 = inline1696
        var t762 bool = t760 < t761
        if t762 {
            var t763 int
            var inline1694 int = ref_get__Ref_3int(i__61)
            t763 = inline1694
            var mtmp449 Tuple2_5SExpr_3int = parse_expr(tokens__60, t763)
            var x450 SExpr = mtmp449._0
            var x451 int = mtmp449._1
            var t764 *_goml_vec_SExpr
            var inline1692 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t764 = inline1692
            var t765 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t764, x450)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t765)
            ref_set__Ref_3int(i__61, x451)
            continue
        } else {
            break Loop_loop759
        }
    }
    var inline1700 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1700
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x460 int32 = expr__72.(SExpr_Int)._0
        var t782 Value = Value_Int{
            _0: x460,
        }
        return t782
    case SExpr_Bool:
        var x461 bool = expr__72.(SExpr_Bool)._0
        var t783 Value = Value_Bool{
            _0: x461,
        }
        return t783
    case SExpr_Sym:
        var x462 string = expr__72.(SExpr_Sym)._0
        var t784 *_goml_vec_Binding
        var inline1717 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t784 = inline1717
        var inline1713 Value = env_lookup(local__73, x462)
        switch inline1713.(type) {
        case Nil:
            var inline1714 Value = env_lookup(t784, x462)
            return inline1714
        default:
            return inline1713
        }
    case List:
        var x463 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1719 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x463)
        var inline1720 bool = inline1719 == 0
        if inline1720 {
            return Nil{}
        } else {
            var inline1721 SExpr = vec_get__Vec_5SExpr(x463, 0)
            switch inline1721.(type) {
            case SExpr_Sym:
                var inline1722 string = inline1721.(SExpr_Sym)._0
                var inline1724 Value = eval_list_sym(inline1722, x463, local__73, global__74)
                return inline1724
            default:
                var inline1725 Value = eval(inline1721, local__73, global__74)
                var inline1726 *_goml_vec_Value = eval_args(x463, 1, local__73, global__74)
                var inline1727 Value = apply(inline1725, inline1726, global__74)
                return inline1727
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t801 Value = eval_begin(items__87, 1, local__88, global__89)
        return t801
    case "define":
        var t804 int
        var inline1739 int = vec_len__Vec_5SExpr(items__87)
        t804 = inline1739
        var t805 bool = t804 == 3
        if t805 {
            var mtmp468 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp468.(type) {
            case SExpr_Sym:
                var x471 string = mtmp468.(SExpr_Sym)._0
                var t808 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t808, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1737 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1737
                var t809 Binding = Binding{
                    name: x471,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t809)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t812 int
        var inline1747 int = vec_len__Vec_5SExpr(items__87)
        t812 = inline1747
        var t813 bool = t812 == 4
        if t813 {
            var t814 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t814, local__88, global__89)
            var t817 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1741 int32 = cond__94.(Value_Int)._0
                var inline1743 bool = inline1741 != 0
                t817 = inline1743
            case Value_Bool:
                var inline1744 bool = cond__94.(Value_Bool)._0
                t817 = inline1744
            case Func:
                t817 = true
            case Nil:
                t817 = false
            default:
                panic("non-exhaustive match")
            }
            if t817 {
                var t818 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t819 Value = eval(t818, local__88, global__89)
                return t819
            } else {
                var t820 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t821 Value = eval(t820, local__88, global__89)
                return t821
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t824 int
        var inline1749 int = vec_len__Vec_5SExpr(items__87)
        t824 = inline1749
        var t825 bool = t824 == 3
        if t825 {
            var mtmp474 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp474.(type) {
            case List:
                var x478 *_goml_vec_SExpr = mtmp474.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x478)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t828 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t829 Value = Func{
                    _0: t828,
                }
                return t829
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t830 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t831 Value = apply_builtin("+", t830)
        return t831
    case "-":
        var t832 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t833 Value = apply_builtin("-", t832)
        return t833
    case "*":
        var t834 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t835 Value = apply_builtin("*", t834)
        return t835
    case "/":
        var t836 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t837 Value = apply_builtin("/", t836)
        return t837
    case "=":
        var t838 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t839 Value = apply_builtin("=", t838)
        return t839
    default:
        var t840 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t840, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1751 Lambda = f__98.(Func)._0
            var inline1753 Value = apply_lambda(inline1751, args__99)
            return inline1753
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1771 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1771
    var last__105 *ref_Value_x
    var inline1769 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1769
    Loop_loop846:
    for {
        var t847 int
        var inline1765 int = ref_get__Ref_3int(i__104)
        t847 = inline1765
        var t848 int
        var inline1763 int = vec_len__Vec_5SExpr(items__100)
        t848 = inline1763
        var t849 bool = t847 < t848
        if t849 {
            var t850 int
            var inline1761 int = ref_get__Ref_3int(i__104)
            t850 = inline1761
            var t851 SExpr = vec_get__Vec_5SExpr(items__100, t850)
            var v__106 Value = eval(t851, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t852 int
            var inline1757 int = ref_get__Ref_3int(i__104)
            t852 = inline1757
            var t853 int = t852 + 1
            ref_set__Ref_3int(i__104, t853)
            continue
        } else {
            break Loop_loop846
        }
    }
    var inline1767 Value = ref_get__Ref_5Value(last__105)
    return inline1767
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1797 int = 0
    var inline1798 *ref_int_x = ref__Ref_3int(inline1797)
    i__108 = inline1798
    var acc__109 *_goml_vec_string
    var inline1795 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1795
    var params__110 *ref_Vec_6string_x
    var inline1793 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1793
    Loop_loop859:
    for {
        var t860 int
        var inline1789 int = ref_get__Ref_3int(i__108)
        t860 = inline1789
        var t861 int
        var inline1787 int = vec_len__Vec_5SExpr(items__107)
        t861 = inline1787
        var t862 bool = t860 < t861
        if t862 {
            var t863 int
            var inline1785 int = ref_get__Ref_3int(i__108)
            t863 = inline1785
            var mtmp481 SExpr = vec_get__Vec_5SExpr(items__107, t863)
            switch mtmp481.(type) {
            case SExpr_Sym:
                var x484 string = mtmp481.(SExpr_Sym)._0
                var t865 *_goml_vec_string
                var inline1779 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t865 = inline1779
                var t866 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t865, x484)
                ref_set__Ref_11Vec_6string(params__110, t866)
                var t867 int
                var inline1775 int = ref_get__Ref_3int(i__108)
                t867 = inline1775
                var t868 int = t867 + 1
                ref_set__Ref_3int(i__108, t868)
                continue
            default:
                var t870 int
                var inline1783 int = ref_get__Ref_3int(i__108)
                t870 = inline1783
                var t871 int = t870 + 1
                ref_set__Ref_3int(i__108, t871)
                continue
            }
        } else {
            break Loop_loop859
        }
    }
    var inline1791 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1791
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1820 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1820
    var acc__117 *_goml_vec_Value
    var inline1818 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1818
    var args__118 *ref_Vec_5Value_x
    var inline1816 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1816
    Loop_loop877:
    for {
        var t878 int
        var inline1812 int = ref_get__Ref_3int(i__116)
        t878 = inline1812
        var t879 int
        var inline1810 int = vec_len__Vec_5SExpr(items__112)
        t879 = inline1810
        var t880 bool = t878 < t879
        if t880 {
            var t881 int
            var inline1808 int = ref_get__Ref_3int(i__116)
            t881 = inline1808
            var t882 SExpr = vec_get__Vec_5SExpr(items__112, t881)
            var v__119 Value = eval(t882, local__114, global__115)
            var t883 *_goml_vec_Value
            var inline1806 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t883 = inline1806
            var t884 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t883, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t884)
            var t885 int
            var inline1802 int = ref_get__Ref_3int(i__116)
            t885 = inline1802
            var t886 int = t885 + 1
            ref_set__Ref_3int(i__116, t886)
            continue
        } else {
            break Loop_loop877
        }
    }
    var inline1814 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1814
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t894 int
        var inline1822 int = vec_len__Vec_5Value(args__121)
        t894 = inline1822
        var t895 bool = t894 == 2
        if t895 {
            var t896 Value = vec_get__Vec_5Value(args__121, 0)
            var t897 Value = vec_get__Vec_5Value(args__121, 1)
            switch t897.(type) {
            case Value_Int:
                var x493 int32 = t897.(Value_Int)._0
                switch t896.(type) {
                case Value_Int:
                    var x496 int32 = t896.(Value_Int)._0
                    var t902 bool = x496 == x493
                    var t903 Value = Value_Bool{
                        _0: t902,
                    }
                    return t903
                default:
                    var t904 Value = Value_Bool{
                        _0: false,
                    }
                    return t904
                }
            case Value_Bool:
                var x494 bool = t897.(Value_Bool)._0
                switch t896.(type) {
                case Value_Bool:
                    var x500 bool = t896.(Value_Bool)._0
                    var t907 bool = x500 == x494
                    var t908 Value = Value_Bool{
                        _0: t907,
                    }
                    return t908
                default:
                    var t909 Value = Value_Bool{
                        _0: false,
                    }
                    return t909
                }
            default:
                var t910 Value = Value_Bool{
                    _0: false,
                }
                return t910
            }
        } else {
            var t911 Value = Value_Bool{
                _0: false,
            }
            return t911
        }
    case "+":
        var i__126 *ref_int_x
        var inline1847 int = 0
        var inline1848 *ref_int_x = ref__Ref_3int(inline1847)
        i__126 = inline1848
        var acc__127 *ref_int32_x
        var inline1844 int32 = 0
        var inline1845 *ref_int32_x = ref__Ref_5int32(inline1844)
        acc__127 = inline1845
        Loop_loop915:
        for {
            var t916 int
            var inline1840 int = ref_get__Ref_3int(i__126)
            t916 = inline1840
            var t917 int
            var inline1838 int = vec_len__Vec_5Value(args__121)
            t917 = inline1838
            var t918 bool = t916 < t917
            if t918 {
                var t919 int
                var inline1836 int = ref_get__Ref_3int(i__126)
                t919 = inline1836
                var mtmp502 Value = vec_get__Vec_5Value(args__121, t919)
                switch mtmp502.(type) {
                case Value_Int:
                    var x503 int32 = mtmp502.(Value_Int)._0
                    var t921 int32
                    var inline1830 int32 = ref_get__Ref_5int32(acc__127)
                    t921 = inline1830
                    var t922 int32 = t921 + x503
                    ref_set__Ref_5int32(acc__127, t922)
                    var t923 int
                    var inline1826 int = ref_get__Ref_3int(i__126)
                    t923 = inline1826
                    var t924 int = t923 + 1
                    ref_set__Ref_3int(i__126, t924)
                    continue
                default:
                    var t926 int
                    var inline1834 int = ref_get__Ref_3int(i__126)
                    t926 = inline1834
                    var t927 int = t926 + 1
                    ref_set__Ref_3int(i__126, t927)
                    continue
                }
            } else {
                break Loop_loop915
            }
        }
        var t913 int32
        var inline1842 int32 = ref_get__Ref_5int32(acc__127)
        t913 = inline1842
        var t914 Value = Value_Int{
            _0: t913,
        }
        return t914
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x
        var inline1870 int32 = 1
        var inline1871 *ref_int32_x = ref__Ref_5int32(inline1870)
        acc__130 = inline1871
        Loop_loop932:
        for {
            var t933 int
            var inline1866 int = ref_get__Ref_3int(i__129)
            t933 = inline1866
            var t934 int
            var inline1864 int = vec_len__Vec_5Value(args__121)
            t934 = inline1864
            var t935 bool = t933 < t934
            if t935 {
                var t936 int
                var inline1862 int = ref_get__Ref_3int(i__129)
                t936 = inline1862
                var mtmp508 Value = vec_get__Vec_5Value(args__121, t936)
                switch mtmp508.(type) {
                case Value_Int:
                    var x509 int32 = mtmp508.(Value_Int)._0
                    var t938 int32
                    var inline1856 int32 = ref_get__Ref_5int32(acc__130)
                    t938 = inline1856
                    var t939 int32 = t938 * x509
                    ref_set__Ref_5int32(acc__130, t939)
                    var t940 int
                    var inline1852 int = ref_get__Ref_3int(i__129)
                    t940 = inline1852
                    var t941 int = t940 + 1
                    ref_set__Ref_3int(i__129, t941)
                    continue
                default:
                    var t943 int
                    var inline1860 int = ref_get__Ref_3int(i__129)
                    t943 = inline1860
                    var t944 int = t943 + 1
                    ref_set__Ref_3int(i__129, t944)
                    continue
                }
            } else {
                break Loop_loop932
            }
        }
        var t930 int32
        var inline1868 int32 = ref_get__Ref_5int32(acc__130)
        t930 = inline1868
        var t931 Value = Value_Int{
            _0: t930,
        }
        return t931
    case "-":
        var mtmp514 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp514 {
        case 1:
            var mtmp515 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp515.(type) {
            case Value_Int:
                var x516 int32 = mtmp515.(Value_Int)._0
                var t950 int32 = 0 - x516
                var t951 Value = Value_Int{
                    _0: t950,
                }
                return t951
            default:
                return Nil{}
            }
        case 2:
            var t952 Value = vec_get__Vec_5Value(args__121, 0)
            var t953 Value = vec_get__Vec_5Value(args__121, 1)
            switch t953.(type) {
            case Value_Int:
                var x522 int32 = t953.(Value_Int)._0
                switch t952.(type) {
                case Value_Int:
                    var x525 int32 = t952.(Value_Int)._0
                    var t958 int32 = x525 - x522
                    var t959 Value = Value_Int{
                        _0: t958,
                    }
                    return t959
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
        var t962 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t963 bool = t962 == 2
        if t963 {
            var t964 Value = vec_get__Vec_5Value(args__121, 0)
            var t965 Value = vec_get__Vec_5Value(args__121, 1)
            switch t965.(type) {
            case Value_Int:
                var x531 int32 = t965.(Value_Int)._0
                switch t964.(type) {
                case Value_Int:
                    var x534 int32 = t964.(Value_Int)._0
                    var t970 int32 = x534 / x531
                    var t971 Value = Value_Int{
                        _0: t970,
                    }
                    return t971
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
        var x539 Lambda = func__137.(Func)._0
        var t976 Value = apply_lambda(x539, args__138)
        return t976
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t979 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1898 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t979)
    env__143 = inline1898
    var i__144 *ref_int_x
    var inline1895 int = 0
    var inline1896 *ref_int_x = ref__Ref_3int(inline1895)
    i__144 = inline1896
    Loop_loop985:
    for {
        var t996 int
        var inline1891 int = ref_get__Ref_3int(i__144)
        t996 = inline1891
        var t997 *_goml_vec_string = lambda__141.params
        var t998 int
        var inline1889 int = vec_len__Vec_6string(t997)
        t998 = inline1889
        var t999 bool = t996 < t998
        var jp987 bool
        if t999 {
            var t1000 int
            var inline1875 int = ref_get__Ref_3int(i__144)
            t1000 = inline1875
            var t1001 int
            var inline1873 int = vec_len__Vec_5Value(args__142)
            t1001 = inline1873
            var t1002 bool = t1000 < t1001
            jp987 = t1002
        } else {
            jp987 = false
        }
        if jp987 {
            var t988 *_goml_vec_string = lambda__141.params
            var t989 int
            var inline1887 int = ref_get__Ref_3int(i__144)
            t989 = inline1887
            var name__145 string = vec_get__Vec_6string(t988, t989)
            var t990 int
            var inline1885 int = ref_get__Ref_3int(i__144)
            t990 = inline1885
            var value__146 Value = vec_get__Vec_5Value(args__142, t990)
            var t991 *_goml_vec_Binding
            var inline1883 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t991 = inline1883
            var t992 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t991, t992)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t993 int
            var inline1879 int = ref_get__Ref_3int(i__144)
            t993 = inline1879
            var t994 int = t993 + 1
            ref_set__Ref_3int(i__144, t994)
            continue
        } else {
            break Loop_loop985
        }
    }
    var t981 SExpr = lambda__141.body
    var t982 *_goml_vec_Binding
    var inline1893 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t982 = inline1893
    var t983 *ref_Vec_7Binding_x = lambda__141.global
    var t984 Value = eval(t981, t982, t983)
    return t984
}

func main0() struct{} {
    var t1004 *_goml_vec_Binding
    var inline1926 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1004 = inline1926
    var global__148 *ref_Vec_7Binding_x
    var inline1924 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t1004)
    global__148 = inline1924
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t1005 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t1005)
    var t1006 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t1007 *_goml_vec_Binding
    var inline1922 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1007 = inline1922
    var result__151 Value = eval(t1006, t1007, global__148)
    var t1008 string
    switch result__151.(type) {
    case Value_Int:
        var inline1915 int32 = result__151.(Value_Int)._0
        var inline1917 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1915)
        t1008 = inline1917
    case Value_Bool:
        var inline1918 bool = result__151.(Value_Bool)._0
        var inline1920 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1918)
        t1008 = inline1920
    case Func:
        t1008 = "<lambda>"
    case Nil:
        t1008 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1912 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1008)
    _goml_runtime_core_string_println(inline1912)
    var t1009 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t1009)
    var t1010 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t1011 *_goml_vec_Binding
    var inline1910 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t1011 = inline1910
    var result2__153 Value = eval(t1010, t1011, global__148)
    var t1012 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1903 int32 = result2__153.(Value_Int)._0
        var inline1905 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1903)
        t1012 = inline1905
    case Value_Bool:
        var inline1906 bool = result2__153.(Value_Bool)._0
        var inline1908 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1906)
        t1012 = inline1908
    case Func:
        t1012 = "<lambda>"
    case Nil:
        t1012 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1900 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1012)
    _goml_runtime_core_string_println(inline1900)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t1015 int = _goml_runtime_core_string_len(self__35)
    return t1015
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t1018 *ref_int_x = ref__Ref_3int(value__431)
    return t1018
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__431 bool) *ref_bool_x {
    var t1021 *ref_bool_x = ref__Ref_4bool(value__431)
    return t1021
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__260 *_goml_vec_Token, elem__261 Token) *_goml_vec_Token {
    var t1065 int
    var inline1948 int = vec_len__Vec_5Token(self__260)
    t1065 = inline1948
    var t1066 int = t1065 + 1
    var result__262 *_goml_vec_Token
    var inline1946 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t1066)
    result__262 = inline1946
    var index__263 int = 0
    Loop_loop1068:
    for {
        var t1069 int
        var inline1942 int = vec_len__Vec_5Token(self__260)
        t1069 = inline1942
        var t1070 bool = index__263 < t1069
        if t1070 {
            var t1071 Token = vec_get__Vec_5Token(self__260, index__263)
            vec_push__Vec_5Token(result__262, t1071)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1072 int = compound_old190 + compound_value191
            index__263 = t1072
            continue
        } else {
            break Loop_loop1068
        }
    }
    vec_push__Vec_5Token(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__260 *_goml_vec_SExpr, elem__261 SExpr) *_goml_vec_SExpr {
    var t1101 int
    var inline1958 int = vec_len__Vec_5SExpr(self__260)
    t1101 = inline1958
    var t1102 int = t1101 + 1
    var result__262 *_goml_vec_SExpr
    var inline1956 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t1102)
    result__262 = inline1956
    var index__263 int = 0
    Loop_loop1104:
    for {
        var t1105 int
        var inline1952 int = vec_len__Vec_5SExpr(self__260)
        t1105 = inline1952
        var t1106 bool = index__263 < t1105
        if t1106 {
            var t1107 SExpr = vec_get__Vec_5SExpr(self__260, index__263)
            vec_push__Vec_5SExpr(result__262, t1107)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1108 int = compound_old190 + compound_value191
            index__263 = t1108
            continue
        } else {
            break Loop_loop1104
        }
    }
    vec_push__Vec_5SExpr(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t1114 string = _goml_runtime_core_int32_to_string(self__33)
    return t1114
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1117 string = _goml_runtime_core_bool_to_string(self__148)
    return t1117
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__273 *_goml_vec_SExpr) int {
    var t1123 int = vec_len__Vec_5SExpr(self__273)
    return t1123
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__260 *_goml_vec_Binding, elem__261 Binding) *_goml_vec_Binding {
    var t1126 int
    var inline1968 int = vec_len__Vec_7Binding(self__260)
    t1126 = inline1968
    var t1127 int = t1126 + 1
    var result__262 *_goml_vec_Binding
    var inline1966 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t1127)
    result__262 = inline1966
    var index__263 int = 0
    Loop_loop1129:
    for {
        var t1130 int
        var inline1962 int = vec_len__Vec_7Binding(self__260)
        t1130 = inline1962
        var t1131 bool = index__263 < t1130
        if t1131 {
            var t1132 Binding = vec_get__Vec_7Binding(self__260, index__263)
            vec_push__Vec_7Binding(result__262, t1132)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1133 int = compound_old190 + compound_value191
            index__263 = t1133
            continue
        } else {
            break Loop_loop1129
        }
    }
    vec_push__Vec_7Binding(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__260 *_goml_vec_string, elem__261 string) *_goml_vec_string {
    var t1148 int
    var inline1978 int = vec_len__Vec_6string(self__260)
    t1148 = inline1978
    var t1149 int = t1148 + 1
    var result__262 *_goml_vec_string
    var inline1976 *_goml_vec_string = vec_with_capacity__Vec_6string(t1149)
    result__262 = inline1976
    var index__263 int = 0
    Loop_loop1151:
    for {
        var t1152 int
        var inline1972 int = vec_len__Vec_6string(self__260)
        t1152 = inline1972
        var t1153 bool = index__263 < t1152
        if t1153 {
            var t1154 string = vec_get__Vec_6string(self__260, index__263)
            vec_push__Vec_6string(result__262, t1154)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1155 int = compound_old190 + compound_value191
            index__263 = t1155
            continue
        } else {
            break Loop_loop1151
        }
    }
    vec_push__Vec_6string(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__260 *_goml_vec_Value, elem__261 Value) *_goml_vec_Value {
    var t1170 int
    var inline1988 int = vec_len__Vec_5Value(self__260)
    t1170 = inline1988
    var t1171 int = t1170 + 1
    var result__262 *_goml_vec_Value
    var inline1986 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t1171)
    result__262 = inline1986
    var index__263 int = 0
    Loop_loop1173:
    for {
        var t1174 int
        var inline1982 int = vec_len__Vec_5Value(self__260)
        t1174 = inline1982
        var t1175 bool = index__263 < t1174
        if t1175 {
            var t1176 Value = vec_get__Vec_5Value(self__260, index__263)
            vec_push__Vec_5Value(result__262, t1176)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1177 int = compound_old190 + compound_value191
            index__263 = t1177
            continue
        } else {
            break Loop_loop1173
        }
    }
    vec_push__Vec_5Value(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__273 *_goml_vec_Value) int {
    var t1183 int = vec_len__Vec_5Value(self__273)
    return t1183
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t1200 rune = _goml_runtime_core_string_get("", -1)
        return t1200
    }
}

func char_to_string(value__29 rune) string {
    var t1205 uint32 = uint32(rune(value__29))
    var t1206 bool
    var inline1991 bool = t1205 <= 1114111
    if inline1991 {
        var inline1992 bool = t1205 >= 55296
        var inline1994 bool
        if inline1992 {
            var inline1996 bool = t1205 <= 57343
            inline1994 = inline1996
        } else {
            inline1994 = false
        }
        var inline1995 bool = !inline1994
        t1206 = inline1995
    } else {
        t1206 = false
    }
    if t1206 {
        var t1207 string = _goml_runtime_core_char_to_string(value__29)
        return t1207
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
    var t1353 bool = index__6 < 0
    var jp1351 bool
    if t1353 {
        jp1351 = true
    } else {
        var t1354 bool = index__6 >= length__7
        jp1351 = t1354
    }
    if jp1351 {
        var inline1998 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1998
    } else {
        var t1238 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1238))
        var t1241 bool = first__8 < 128
        if t1241 {
            var inline2000 int = 1
            var inline2001 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline2001._tag {
            case 0:
                var inline2002 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2002
            case 1:
                var inline2003 rune = inline2001._v1_0
                var inline2005 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2003,
                    _2: inline2000,
                }
                return inline2005
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1245 bool = first__8 < 194
            if t1245 {
                var inline2007 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2007
            } else {
                var t1249 bool = first__8 < 224
                if t1249 {
                    var t1262 int = length__7 - index__6
                    var t1263 bool = t1262 < 2
                    if t1263 {
                        var inline2009 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2009
                    } else {
                        var t1251 int = index__6 + 1
                        var t1252 uint8
                        var inline2023 uint8 = _goml_runtime_core_string_byte_get(value__5, t1251)
                        t1252 = inline2023
                        var second__9 uint32 = uint32(uint8(t1252))
                        var t1255 bool
                        var inline2020 bool = second__9 < 128
                        if inline2020 {
                            t1255 = true
                        } else {
                            var inline2021 bool = second__9 > 191
                            t1255 = inline2021
                        }
                        if t1255 {
                            var inline2011 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2011
                        } else {
                            var t1257_rhs uint32 = 31
                            var t1257 uint32 = first__8 & t1257_rhs
                            var t1258_rhs int = 6
                            var t1258 uint32 = t1257 << t1258_rhs
                            var t1259_rhs uint32 = 63
                            var t1259 uint32 = second__9 & t1259_rhs
                            var t1260 uint32 = t1258 | t1259
                            var inline2013 int = 2
                            var inline2014 Option__char = __goml_builtin_char_from_uint32(t1260)
                            switch inline2014._tag {
                            case 0:
                                var inline2015 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2015
                            case 1:
                                var inline2016 rune = inline2014._v1_0
                                var inline2018 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2016,
                                    _2: inline2013,
                                }
                                return inline2018
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1267 bool = first__8 < 240
                    if t1267 {
                        var t1300 int = length__7 - index__6
                        var t1301 bool = t1300 < 3
                        if t1301 {
                            var inline2025 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2025
                        } else {
                            var t1269 int = index__6 + 1
                            var t1270 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1269)
                            var second__10 uint32 = uint32(uint8(t1270))
                            var t1271 int = index__6 + 2
                            var t1272 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1271)
                            var third__11 uint32 = uint32(uint8(t1272))
                            var t1298 bool = utf8_invalid_continuation(second__10)
                            var jp1293 bool
                            if t1298 {
                                jp1293 = true
                            } else {
                                var inline2027 bool = third__11 < 128
                                if inline2027 {
                                    jp1293 = true
                                } else {
                                    var inline2028 bool = third__11 > 191
                                    jp1293 = inline2028
                                }
                            }
                            var jp1287 bool
                            if jp1293 {
                                jp1287 = true
                            } else {
                                var t1296 bool = first__8 == 224
                                if t1296 {
                                    var t1297 bool = second__10 < 160
                                    jp1287 = t1297
                                } else {
                                    jp1287 = false
                                }
                            }
                            var jp1276 bool
                            if jp1287 {
                                jp1276 = true
                            } else {
                                var t1290 bool = first__8 == 237
                                if t1290 {
                                    var t1291 bool = second__10 >= 160
                                    jp1276 = t1291
                                } else {
                                    jp1276 = false
                                }
                            }
                            if jp1276 {
                                var inline2030 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2030
                            } else {
                                var t1278_rhs uint32 = 15
                                var t1278 uint32 = first__8 & t1278_rhs
                                var t1279_rhs int = 12
                                var t1279 uint32 = t1278 << t1279_rhs
                                var t1280_rhs uint32 = 63
                                var t1280 uint32 = second__10 & t1280_rhs
                                var t1281_rhs int = 6
                                var t1281 uint32 = t1280 << t1281_rhs
                                var t1282 uint32 = t1279 | t1281
                                var t1283_rhs uint32 = 63
                                var t1283 uint32 = third__11 & t1283_rhs
                                var t1284 uint32 = t1282 | t1283
                                var inline2032 int = 3
                                var inline2033 Option__char = __goml_builtin_char_from_uint32(t1284)
                                switch inline2033._tag {
                                case 0:
                                    var inline2034 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2034
                                case 1:
                                    var inline2035 rune = inline2033._v1_0
                                    var inline2037 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2035,
                                        _2: inline2032,
                                    }
                                    return inline2037
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1305 bool = first__8 < 245
                        if t1305 {
                            var t1346 int = length__7 - index__6
                            var t1347 bool = t1346 < 4
                            if t1347 {
                                var t1348 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1348
                            } else {
                                var t1307 int = index__6 + 1
                                var t1308 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1307)
                                var second__12 uint32 = uint32(uint8(t1308))
                                var t1309 int = index__6 + 2
                                var t1310 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1309)
                                var third__13 uint32 = uint32(uint8(t1310))
                                var t1311 int = index__6 + 3
                                var t1312 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1311)
                                var fourth__14 uint32 = uint32(uint8(t1312))
                                var t1344 bool = utf8_invalid_continuation(second__12)
                                var jp1342 bool
                                if t1344 {
                                    jp1342 = true
                                } else {
                                    var t1345 bool = utf8_invalid_continuation(third__13)
                                    jp1342 = t1345
                                }
                                var jp1336 bool
                                if jp1342 {
                                    jp1336 = true
                                } else {
                                    var t1343 bool = utf8_invalid_continuation(fourth__14)
                                    jp1336 = t1343
                                }
                                var jp1330 bool
                                if jp1336 {
                                    jp1330 = true
                                } else {
                                    var t1339 bool = first__8 == 240
                                    if t1339 {
                                        var t1340 bool = second__12 < 144
                                        jp1330 = t1340
                                    } else {
                                        jp1330 = false
                                    }
                                }
                                var jp1316 bool
                                if jp1330 {
                                    jp1316 = true
                                } else {
                                    var t1333 bool = first__8 == 244
                                    if t1333 {
                                        var t1334 bool = second__12 > 143
                                        jp1316 = t1334
                                    } else {
                                        jp1316 = false
                                    }
                                }
                                if jp1316 {
                                    var t1317 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1317
                                } else {
                                    var t1318_rhs uint32 = 7
                                    var t1318 uint32 = first__8 & t1318_rhs
                                    var t1319_rhs int = 18
                                    var t1319 uint32 = t1318 << t1319_rhs
                                    var t1320_rhs uint32 = 63
                                    var t1320 uint32 = second__12 & t1320_rhs
                                    var t1321_rhs int = 12
                                    var t1321 uint32 = t1320 << t1321_rhs
                                    var t1322 uint32 = t1319 | t1321
                                    var t1323_rhs uint32 = 63
                                    var t1323 uint32 = third__13 & t1323_rhs
                                    var t1324_rhs int = 6
                                    var t1324 uint32 = t1323 << t1324_rhs
                                    var t1325 uint32 = t1322 | t1324
                                    var t1326_rhs uint32 = 63
                                    var t1326 uint32 = fourth__14 & t1326_rhs
                                    var t1327 uint32 = t1325 | t1326
                                    var t1328 Tuple3_4bool_4char_3int = utf8_valid_decode(t1327, 4)
                                    return t1328
                                }
                            }
                        } else {
                            var t1349 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1349
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1359 bool = value__4 <= 1114111
    if t1359 {
        var t1363 bool = value__4 >= 55296
        var jp1361 bool
        if t1363 {
            var t1364 bool = value__4 <= 57343
            jp1361 = t1364
        } else {
            jp1361 = false
        }
        var t1362 bool = !jp1361
        return t1362
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1367 int = _goml_runtime_core_string_len(self__36)
    return t1367
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1370 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1370
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1373 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1373
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field2066 rune
    var inline2041 bool = utf8_valid_scalar(value__0)
    if inline2041 {
        var inline2042 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2043 rune = inline2042._1
        commute_field2066 = inline2043
        var t1379 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2066,
            _2: width__1,
        }
        return t1379
    } else {
        var inline2039 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2039
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1384 bool = value__3 < 128
    if t1384 {
        return true
    } else {
        var t1385 bool = value__3 > 191
        return t1385
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1390 bool
    var inline2047 bool = value__30 <= 1114111
    if inline2047 {
        var inline2048 bool = value__30 >= 55296
        var inline2050 bool
        if inline2048 {
            var inline2052 bool = value__30 <= 57343
            inline2050 = inline2052
        } else {
            inline2050 = false
        }
        var inline2051 bool = !inline2050
        t1390 = inline2051
    } else {
        t1390 = false
    }
    if t1390 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1391 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1391
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
